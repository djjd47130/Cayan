unit Cayan.Genius.Impl;

interface

uses
  System.Classes, System.SysUtils, System.Variants,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,
  IdExceptionCore
  {$IFDEF IOS}
  , IdSSLOpenSSLHeaders_Static
  {$ENDIF}
  , Cayan.Common, Cayan.MWv4.Impl, Cayan.MWv4.Intf, Cayan.Genius.Intf
  , Xml.XmlIntf, Xml.XmlDoc, Xml.XmlDom, Xml.OmniXmlDom
  {$IFDEF MSWINDOWS}
  , Winapi.ActiveX
  {$ENDIF}
  ;

const

  //Endpoints
  TRANSPORT_PROD_URL =    'https://transport.merchantware.net/%s/%s';
  TRANSPORT_TEST_URL =    'https://transport.merchantware.net/%s/%s';

  URL_STAGE_TRANS =       'https://transport.merchantware.net/v4/transportService.asmx';
  URL_STAGE_TRANS_TEST =  'https://simulator-transport.merchantware.net/v4/transportService.asmx';
  URL_RETR_TRANS =        'https://genius.merchantware.net/v1/Reporting.asmx';
  URL_CREDIT_TRANS =      'https://ps1.merchantware.net/Merchantware/ws/RetailTransaction/v4/Credit.asmx';
  URL_GIFT_TRANS =        'https://ps1.merchantware.net/Merchantware/ws/ExtensionServices/v4/Giftcard.asmx';
  URL_RETR_TRANS_ACT =    'http://schemas.merchantwarehouse.com/genius/10/Reporting';

  //Test Credentials
  //NOTE: Does not save any type of transaction history, so cannot test
  //  requests such as Void or Close Batch, etc.
  TEST_MERCHANT_NAME = 'PSTest';
  TEST_MERCHANT_SITE_ID = '22222222';
  TEST_MERCHANT_KEY = '22222-22222-22222-22222-22222';

  //Emulator Genius CED
  //NOTE: Only handles specific transaction requests - has no knowledge of
  //  most CED functions, such as status, signature, cancel, etc.
  //NOTE: As of 4/26/2017, it does not appear this emulator is available anymore.
  TEST_HOST = '205.219.72.106';
  TEST_PORT = 8080;

  //How to format a URL for a Genius CED
  //SAMPLE: https://0.0.0.0:8080/v1/pos?TransportKey=000-000&Format=XML
  URL_DEVICE =          '%s://%s:%d/%s/pos?%s';

  //Maximum number of characters per line item description in line item display
  LID_MAX_DESCR = 35;

type
  TCayanEndpoints = class(TPersistent)
  public
    constructor Create;
    destructor Destroy; override;
  public
    class function GetTransportUrl(const TestMode: Boolean = False): String;
    class function GetStagingUrl(const TestMode: Boolean = False): String;
    class function GetReportingUrl(const TestMode: Boolean = False): String;
  published

  end;

type
  TGeniusTransactionThread = class;
  TGeniusStatusThread = class;
  TGenius = class;
  TGeniusStageRequest = class;
  TGeniusStageResponse = class;
  TGeniusStageMessage = class;
  TGeniusTransactionResponse = class;
  TGeniusTransactionResponseEmv = class;
  TGeniusTransactionDiscount = class;
  TGeniusSignatureResponse = class;
  TGeniusLineItems = class;
  TGeniusLineItem = class;
  TGeniusStartOrderResponse = class;
  TGeniusCancelTransactionResponse = class;
  TGeniusPaymentDetails = class;
  TGeniusPaymentDetail = class;

  TGeniusRequestThreadEvent = procedure(Sender: TObject; const AURL: String;
    const Response: String) of object;

  ///  <summary>
  ///    Used for sending requests to Genius CED without
  ///    waiting for a response - instead, the thread triggers an event
  ///    on response.
  ///  <summary>
  TGeniusRequestThread = class(TThread)
  private
    FOwner: IGenius;
    FUrl: String;
    FResponseText: String;
    FResponseEvent: TGeniusRequestThreadEvent;
    procedure DoOnResponse;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: IGenius; const AURL: String;
      const AResponseEvent: TGeniusRequestThreadEvent); reintroduce; virtual;
    destructor Destroy; override;
  end;




  //Initiating transaction needs to wait indefinitely for a response from
  //the device, during which time Merchant POS must continue working.
  //This processing is done within a thread to allow merchant to
  //cancel transaction if they need to, which is a requirement.
  TGeniusTransactionThread = class(TThread)
  private
    FOwner: TGenius;
    FTransportKey: String;
    FResponseText: String;
    FResponse: IGeniusTransactionResponse;
    FUrl: String;
    procedure DoOnResponse;
    procedure ParseXML;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TGenius; const TransportKey: String);
    destructor Destroy; override;
  end;

  ///  <summary>
  ///    Constantly monitors the current status of CED
  ///  </summary>
  TGeniusStatusThread = class(TThread)
  private
    FOwner: TGenius;
    FResponse: IGeniusStatusResponse;
    FActive: Boolean;
    FStatusCheckProc: TGeniusStatusCheckProc;
    FOnResponse: TGeniusStatusResponseEvent;
    procedure DoOnResponse;
    procedure SetActive(const Value: Boolean);
    procedure FreeResponse;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TGenius);
    destructor Destroy; override;
    property Active: Boolean read FActive write SetActive;
    property OnResponse: TGeniusStatusResponseEvent read FOnResponse write FOnResponse;
  end;

  ///  <summary>
  ///    Core implementation of Genius API and represents a single CED
  ///  </summary>
  TGenius = class(TInterfacedObject, IGenius)
  private
    FWebCayan: TIdHTTP;
    FSSLCayan: TIdSSLIOHandlerSocketOpenSSL;
    FWebDevice: TIdHTTP;
    FSSLDevice: TIdSSLIOHandlerSocketOpenSSL;
    FOwner: TMerchantWare;
    FDeviceTimeout: Integer;
    FCancelled: Boolean;
    FInTransaction: Boolean;
    FInSignature: Boolean;
    FInAgreement: Boolean;
    FInCustomerInput: Boolean;
    FDeviceAddress: String;
    FDevicePort: Integer;
    FDeviceProtocol: TGeniusProtocol;
    FDeviceVersion: TGeniusDeviceVersion;
    FLineItems: TGeniusLineItems;
    FTransactionResponse: TGeniusTransactionEvent;
    FDba: String;
    FSoftwareName: String;
    FSoftwareVersion: String;
    FStatusThread: TGeniusStatusThread;
    FOnStatus: TGeniusStatusResponseEvent;
    procedure TransThreadResponse(Response: IGeniusTransactionResponse);
    function GetCredentials(const CredMerch: Boolean = True; const Cap: Boolean = False): String;
    function DeviceUrl(const Params: String;
      const Ver: TGeniusDeviceVersion = gdVer1): String;
    function TransportUrl(const Ver, Req: String): String;
    function SendDeviceRequest(const Url: String; const Timeout: Integer = 0): String;
    function SendTransportRequest(const SvcUrl: String; const Ver: String; const ActionUrl: String;
      const Action: String; const AXML: String; const CredMerch: Boolean = True; const Cap: Boolean = False): IXMLDocument;
    function SendDeviceRequestXML(const Url: String; const Timeout: Integer = 0): IXMLDocument;
    procedure StatusResponse(Sender: IGenius;
      const Status: IGeniusStatusResponse);
  public
    constructor Create(AOwner: TMerchantWare);
    destructor Destroy; override;
    procedure ParseApprovalStatus(Value: String;
      var Stat: TGeniusTransStatusSet; var Code: Integer; var Msg: String);
  public
    function GetMerchantName: String;
    function GetMerchantKey: String;
    function GetMerchantSiteId: String;
    function GetTestMode: Boolean;
    procedure SetTestMode(const Value: Boolean);
    function GetDeviceAddress: String;
    procedure SetDeviceAddress(const Value: String);
    function GetDevicePort: Integer;
    procedure SetDevicePort(const Value: Integer);
    function GetLineItems: IGeniusLineItems;
    function GetDeviceProtocol: TGeniusProtocol;
    procedure SetDeviceProtocol(const Value: TGeniusProtocol);
    function GetDeviceVersion: TGeniusDeviceVersion;
    procedure SetDeviceVersion(const Value: TGeniusDeviceVersion);
    function GetTransactionResponse: TGeniusTransactionEvent;
    procedure SetTransactionResponse(const Value: TGeniusTransactionEvent);
    function GetDba: String;
    procedure SetDba(const Value: String);
    function GetSoftwareName: String;
    procedure SetSoftwareName(const Value: String);
    function GetSoftwareVersion: String;
    procedure SetSoftwareVersion(const Value: String);
    function GetDeviceTimeout: Integer;
    procedure SetDeviceTimeout(const Value: Integer);
    function GetOnStatus: TGeniusStatusResponseEvent;
    procedure SetOnStatus(const Value: TGeniusStatusResponseEvent);
    function GetMonitoring: Boolean;
    procedure SetMonitoring(const Value: Boolean);

    function CreateStageRequest: IGeniusStageRequest;

    function StageTransaction(const ARequest: IGeniusStageRequest): IGeniusStageResponse;
    function InitiateTransaction(const TransportKey: String): Boolean;
    function CaptureSignature(const RequestId, Title: String): IGeniusSignatureResponse;
    function CancelTransaction: IGeniusCancelTransactionResponse;
    function StatusCheck(const Timeout: Integer = 0): IGeniusStatusResponse;
    function InitiateKeyedEntry: TGeniusKeyedSaleStatus;
    function DetailsByTransportKey(const TransportKey: String): IGeniusPaymentDetails;
    function IsInTransaction: Boolean;
    function IsInSignature: Boolean;
    function IsInAgreement: Boolean;
    function IsInCustomerInput: Boolean;
    function GetAgreement(const RequestID: String; const Title: String;
      const AgreementText: String; const AcceptLabel: String;
      const DeclineLabel: String): IGeniusAgreementResponse;

    property MerchantName: String read GetMerchantName;
    property MerchantSiteId: String read GetMerchantSiteId;
    property MerchantKey: String read GetMerchantKey;
    property TestMode: Boolean read GetTestMode write SetTestMode;
    property DeviceAddress: String read GetDeviceAddress write SetDeviceAddress;
    property DevicePort: Integer read GetDevicePort write SetDevicePort;
    property DeviceProtocol: TGeniusProtocol read GetDeviceProtocol write SetDeviceProtocol;
    property DeviceVersion: TGeniusDeviceVersion read GetDeviceVersion write SetDeviceVersion;
    property LineItems: IGeniusLineItems read GetLineItems;
    property TransactionResponse: TGeniusTransactionEvent read GetTransactionResponse write SetTransactionResponse;
    property Dba: String read GetDba write SetDba;
    property SoftwareName: String read GetSoftwareName write SetSoftwareName;
    property SoftwareVersion: String read GetSoftwareVersion write SetSoftwareVersion;
    property DeviceTimeout: Integer read GetDeviceTimeout write SetDeviceTimeout;
    property Monitoring: Boolean read GetMonitoring write SetMonitoring;

    property OnStatus: TGeniusStatusResponseEvent read GetOnStatus write SetOnStatus;
  end;

  TGeniusStageRequest = class(TInterfacedObject, IGeniusStageRequest)
  private
    FTransactionType: TGeniusTransactionType;
    FAmount: Currency;
    FCardHolder: String;
    FClerkId: String;
    FOrderNumber: String;
    FDba: String;
    FSoftwareName: String;
    FSoftwareVersion: String;
    FTransactionId: String;
    FForceDuplicate: Boolean;
    FCustomerCode: String;
    FPoNumber: String;
    FTaxAmount: Currency;
    FTerminalId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAmount: Currency;
    function GetClerkId: String;
    function GetCustomerCode: String;
    function GetDba: String;
    function GetForceDuplicate: Boolean;
    function GetOrderNumber: String;
    function GetPoNumber: String;
    function GetSoftwareName: String;
    function GetSoftwareVersion: String;
    function GetTaxAmount: Currency;
    function GetTransactionId: String;
    function GetTransactionType: TGeniusTransactionType;
    procedure SetAmount(const Value: Currency);
    procedure SetClerkId(const Value: String);
    procedure SetCustomerCode(const Value: String);
    procedure SetDba(const Value: String);
    procedure SetForceDuplicate(const Value: Boolean);
    procedure SetOrderNumber(const Value: String);
    procedure SetPoNumber(const Value: String);
    procedure SetSoftwareName(const Value: String);
    procedure SetSoftwareVersion(const Value: String);
    procedure SetTaxAmount(const Value: Currency);
    procedure SetTransactionId(const Value: String);
    procedure SetTransactionType(const Value: TGeniusTransactionType);
    function GetCardHolder: String;
    procedure SetCardHolder(const Value: String);
    function GetTerminalId: Integer;
    procedure SetTerminalId(const Value: Integer);

    property TransactionType: TGeniusTransactionType read GetTransactionType write SetTransactionType;
    property Amount: Currency read GetAmount write SetAmount;
    property ClerkId: String read GetClerkId write SetClerkId;
    property OrderNumber: String read GetOrderNumber write SetOrderNumber;
    property Dba: String read GetDba write SetDba;
    property SoftwareName: String read GetSoftwareName write SetSoftwareName;
    property SoftwareVersion: String read GetSoftwareVersion write SetSoftwareVersion;
    property CardHolder: String read GetCardHolder write SetCardHolder;
    property TransactionId: String read GetTransactionId write SetTransactionId;
    property ForceDuplicate: Boolean read GetForceDuplicate write SetForceDuplicate;
    //Required for Level 2 transactions (CustomerCode, PoNumber, TaxAmount
    property CustomerCode: String read GetCustomerCode write SetCustomerCode;
    property PoNumber: String read GetPoNumber write SetPoNumber;
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;
    property TerminalId: Integer read GetTerminalId write SetTerminalId;
  end;

  TGeniusStageResponse = class(TInterfacedObject, IGeniusStageResponse)
  private
    FTransportKey: String;
    FValidationKey: String;
    FMessages: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: IGeniusStageMessage;
    procedure Delete(const Index: Integer);
    procedure Clear;
  public
    function GetTransportKey: String;
    procedure SetTransportKey(const Value: String);
    function GetValidationKey: String;
    procedure SetValidationKey(const Value: String);
    function MessageCount: Integer;
    function GetMessage(const Index: Integer): IGeniusStageMessage;

    property TransportKey: String read GetTransportKey write SetTransportKey;
    property ValidationKey: String read GetValidationKey write SetValidationKey;
    property Messages[const Index: Integer]: IGeniusStageMessage read GetMessage;
  end;

  TGeniusStageMessage = class(TInterfacedObject, IGeniusStageMessage)
  private
    FField: String;
    FInformation: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetField: String;
    procedure SetField(const Value: String);
    function GetInformation: String;
    procedure SetInformation(const Value: String);

    property Field: String read GetField write SetField;
    property Information: String read GetInformation write SetInformation;
  end;

  TGeniusTransactionResponse = class(TInterfacedObject, IGeniusTransactionResponse)
  private
    FTransportKey: String;
    FStatusStr: String;
    FStatus: TGeniusTransStatus;
    FAmountApproved: Currency;
    FAuthorizationCode: String;
    FCardHolder: String;
    FAccountNumber: String;
    FPaymentType: TGeniusPaymentType;
    FEntryMode: TGeniusEntryMode;
    FErrorMessage: String;
    FToken: String;
    FTransactionDate: TDateTime;
    FTransactionType: TGeniusTransactionType;
    FResponseType: TGeniusResponseType;
    FValidationKey: String;
    FSignatureData: String;
    FUserTip: Currency;
    FCashback: Currency;
    FDonation: Currency;
    FSurcharge: Currency;
    FDiscount: Currency;
    FDiscountsApplied: TInterfaceList;
    FKeyedExpiration: TExpirationDate;
    FKeyedAvsStreetZipCode: String;
    FKeyedAvsResponse: Char;
    FKeyedCvResponse: Char;
    FEmvResponse: IGeniusTransactionResponseEmv;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAccountNumber: String;
    function GetAmountApproved: Currency;
    function GetAuthorizationCode: String;
    function GetCardHolder: String;
    function GetCashback: Currency;
    function GetDiscount: Currency;
    function GetDonation: Currency;
    function GetEntryMode: TGeniusEntryMode;
    function GetErrorMessage: String;
    function GetKeyedAvsResponse: Char;
    function GetKeyedAvsStreetZipCode: String;
    function GetKeyedCvResponse: Char;
    function GetKeyedExpiration: TExpirationDate;
    function GetPaymentType: TGeniusPaymentType;
    function GetResponseType: TGeniusResponseType;
    function GetSignatureData: String;
    function GetStatus: TGeniusTransStatus;
    function GetSurcharge: Currency;
    function GetToken: String;
    function GetTransactionDate: TDateTime;
    function GetTransactionType: TGeniusTransactionType;
    function GetUserTip: Currency;
    function GetValidationKey: String;
    procedure SetAccountNumber(const Value: String);
    procedure SetAmountApproved(const Value: Currency);
    procedure SetAuthorizationCode(const Value: String);
    procedure SetCardHolder(const Value: String);
    procedure SetCashback(const Value: Currency);
    procedure SetDiscount(const Value: Currency);
    procedure SetDonation(const Value: Currency);
    procedure SetEntryMode(const Value: TGeniusEntryMode);
    procedure SetErrorMessage(const Value: String);
    procedure SetKeyedAvsResponse(const Value: Char);
    procedure SetKeyedAvsStreetZipCode(const Value: String);
    procedure SetKeyedCvResponse(const Value: Char);
    procedure SetKeyedExpiration(const Value: TExpirationDate);
    procedure SetPaymentType(const Value: TGeniusPaymentType);
    procedure SetResponseType(const Value: TGeniusResponseType);
    procedure SetSignatureData(const Value: String);
    procedure SetStatus(const Value: TGeniusTransStatus);
    procedure SetSurcharge(const Value: Currency);
    procedure SetToken(const Value: String);
    procedure SetTransactionDate(const Value: TDateTime);
    procedure SetTransactionType(const Value: TGeniusTransactionType);
    procedure SetUserTip(const Value: Currency);
    procedure SetValidationKey(const Value: String);
    function GetTransportKey: String;
    procedure SetTransportKey(const Value: String);
    function GetEmvResponse: IGeniusTransactionResponseEmv;
    procedure SetEmvResponse(const Value: IGeniusTransactionResponseEmv);
    function GetStatusStr: String;
    procedure SetStatusStr(const Value: String);

    function AddDiscountApplied: IGeniusTransactionDiscount;
    procedure ClearDiscountsApplied;
    procedure DeleteDiscountApplied(const Index: Integer);
    function GetDiscountsApplied(
      const Index: Integer): IGeniusTransactionDiscount;
    function GetDiscountsAppliedCount: Integer;

    property TransportKey: String read GetTransportKey write SetTransportKey;
    property StatusStr: String read GetStatusStr write SetStatusStr;
    property Status: TGeniusTransStatus read GetStatus write SetStatus;
    property AmountApproved: Currency read GetAmountApproved write SetAmountApproved;
    property AuthorizationCode: String read GetAuthorizationCode write SetAuthorizationCode;
    property CardHolder: String read GetCardHolder write SetCardHolder;
    property AccountNumber: String read GetAccountNumber write SetAccountNumber;
    property PaymentType: TGeniusPaymentType read GetPaymentType write SetPaymentType;
    property EntryMode: TGeniusEntryMode read GetEntryMode write SetEntryMode;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property Token: String read GetToken write SetToken;
    property TransactionDate: TDateTime read GetTransactionDate write SetTransactionDate;
    property TransactionType: TGeniusTransactionType read GetTransactionType write SetTransactionType;
    property ResponseType: TGeniusResponseType read GetResponseType write SetResponseType;
    property ValidationKey: String read GetValidationKey write SetValidationKey;
    property SignatureData: String read GetSignatureData write SetSignatureData;
    property UserTip: Currency read GetUserTip write SetUserTip;
    property Cashback: Currency read GetCashback write SetCashback;
    property Donation: Currency read GetDonation write SetDonation;
    property Surcharge: Currency read GetSurcharge write SetSurcharge;
    property Discount: Currency read GetDiscount write SetDiscount;
    property DiscountsAppliedCount: Integer read GetDiscountsAppliedCount;
    property DiscountsApplied[const Index: Integer]: IGeniusTransactionDiscount read GetDiscountsApplied;
    property KeyedExpirationMonth: TExpirationDate read GetKeyedExpiration write SetKeyedExpiration;
    property KeyedAvsStreetZipCode: String read GetKeyedAvsStreetZipCode write SetKeyedAvsStreetZipCode;
    property KeyedAvsResponse: Char read GetKeyedAvsResponse write SetKeyedAvsResponse;
    property KeyedCvResponse: Char read GetKeyedCvResponse write SetKeyedCvResponse;
    property EmvResponse: IGeniusTransactionResponseEmv read GetEmvResponse write SetEmvResponse;
  end;

  TGeniusTransactionResponseEmv = class(TInterfacedObject, IGeniusTransactionResponseEmv)
  private
    FAid: String;
    FApplicationLabel: String;
    FApplicationExpiryDate: TDateTime;
    FApplicationEffectiveDate: TDateTime;
    FApplicationInterchangeProfile: String;
    FApplicationVersionNumber: Integer;
    FApplicationTransactionCounter: Integer;
    FMaskedPan: String;
    FPanSequenceNumber: Integer;
    FCardExpiryDate: TExpirationDate;
    FCryptogramType: TGeniusCryptogramType;
    FCryptogram: String;
    FCvmResults: String;
    FIssuerApplicationData: String;
    FTerminalVerificationResults: String;
    FUnpredictableNumber: Integer;
    FAmountAuthorized: Currency;
    FAmountOther: Currency;
    FPosEntryMode: TGeniusEntryMode;
    FTerminalType: Integer;
    FIfdSerialNumber: String;
    FTerminalCountryCode: String;
    FTransactionType: TGeniusEmvTransactionType;
    FTransactionCurrencyCode: String;
    FCryptogramInformationData: String;
    FPINStatement: String;
    FCvmMethod: TGeniusCvmMethod;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAid: String;
    function GetAmountAuthorized: Currency;
    function GetAmountOther: Currency;
    function GetApplicationEffectiveDate: TDateTime;
    function GetApplicationExpiryDate: TDateTime;
    function GetApplicationInterchangeProfile: String;
    function GetApplicationLabel: String;
    function GetApplicationTransactionCounter: Integer;
    function GetApplicationVersionNumber: Integer;
    function GetCardExpiryDate: TExpirationDate;
    function GetCryptogram: String;
    function GetCryptogramInformationData: String;
    function GetCryptogramType: TGeniusCryptogramType;
    function GetCvmMethod: TGeniusCvmMethod;
    function GetCvmResults: String;
    function GetIfdSerialNumber: String;
    function GetIssuerApplicationData: String;
    function GetMaskedPan: String;
    function GetPanSequenceNumber: Integer;
    function GetPINStatement: String;
    function GetPosEntryMode: TGeniusEntryMode;
    function GetTerminalCountryCode: String;
    function GetTerminalType: Integer;
    function GetTerminalVerificationResults: String;
    function GetTransactionCurrencyCode: String;
    function GetTransactionType: TGeniusEmvTransactionType;
    function GetUnpredictableNumber: Integer;
    procedure SetAid(const Value: String);
    procedure SetAmountAuthorized(const Value: Currency);
    procedure SetAmountOther(const Value: Currency);
    procedure SetApplicationEffectiveDate(const Value: TDateTime);
    procedure SetApplicationExpiryDate(const Value: TDateTime);
    procedure SetApplicationInterchangeProfile(const Value: String);
    procedure SetApplicationLabel(const Value: String);
    procedure SetApplicationTransactionCounter(const Value: Integer);
    procedure SetApplicationVersionNumber(const Value: Integer);
    procedure SetCardExpiryDate(const Value: TExpirationDate);
    procedure SetCryptogram(const Value: String);
    procedure SetCryptogramInformationData(const Value: String);
    procedure SetCryptogramType(const Value: TGeniusCryptogramType);
    procedure SetCvmMethod(const Value: TGeniusCvmMethod);
    procedure SetCvmResults(const Value: String);
    procedure SetIfdSerialNumber(const Value: String);
    procedure SetIssuerApplicationData(const Value: String);
    procedure SetMaskedPan(const Value: String);
    procedure SetPanSequenceNumber(const Value: Integer);
    procedure SetPINStatement(const Value: String);
    procedure SetPosEntryMode(const Value: TGeniusEntryMode);
    procedure SetTerminalCountryCode(const Value: String);
    procedure SetTerminalType(const Value: Integer);
    procedure SetTerminalVerificationResults(const Value: String);
    procedure SetTransactionCurrencyCode(const Value: String);
    procedure SetTransactionType(const Value: TGeniusEmvTransactionType);
    procedure SetUnpredictableNumber(const Value: Integer);

    property Aid: String read GetAid write SetAid;
    property ApplicationLabel: String read GetApplicationLabel write SetApplicationLabel;
    property ApplicationExpiryDate: TDateTime read GetApplicationExpiryDate write SetApplicationExpiryDate;
    property ApplicationEffectiveDate: TDateTime read GetApplicationEffectiveDate write SetApplicationEffectiveDate;
    property ApplicationInterchangeProfile: String read GetApplicationInterchangeProfile write SetApplicationInterchangeProfile;
    property ApplicationVersionNumber: Integer read GetApplicationVersionNumber write SetApplicationVersionNumber;
    property ApplicationTransactionCounter: Integer read GetApplicationTransactionCounter write SetApplicationTransactionCounter;
    property MaskedPan: String read GetMaskedPan write SetMaskedPan;
    property PanSequenceNumber: Integer read GetPanSequenceNumber write SetPanSequenceNumber;
    property CardExpiryDate: TExpirationDate read GetCardExpiryDate write SetCardExpiryDate;
    property CryptogramType: TGeniusCryptogramType read GetCryptogramType write SetCryptogramType;
    property Cryptogram: String read GetCryptogram write SetCryptogram;
    property CvmResults: String read GetCvmResults write SetCvmResults;
    property IssuerApplicationData: String read GetIssuerApplicationData write SetIssuerApplicationData;
    property TerminalVerificationResults: String read GetTerminalVerificationResults write SetTerminalVerificationResults;
    property UnpredictableNumber: Integer read GetUnpredictableNumber write SetUnpredictableNumber;
    property AmountAuthorized: Currency read GetAmountAuthorized write SetAmountAuthorized;
    property AmountOther: Currency read GetAmountOther write SetAmountOther;
    property PosEntryMode: TGeniusEntryMode read GetPosEntryMode write SetPosEntryMode;
    property TerminalType: Integer read GetTerminalType write SetTerminalType;
    property IfdSerialNumber: String read GetIfdSerialNumber write SetIfdSerialNumber;
    property TerminalCountryCode: String read GetTerminalCountryCode write SetTerminalCountryCode;
    property TransactionType: TGeniusEmvTransactionType read GetTransactionType write SetTransactionType;
    property TransactionCurrencyCode: String read GetTransactionCurrencyCode write SetTransactionCurrencyCode;
    property CryptogramInformationData: String read GetCryptogramInformationData write SetCryptogramInformationData;
    property PINStatement: String read GetPINStatement write SetPINStatement;
    property CvmMethod: TGeniusCvmMethod read GetCvmMethod write SetCvmMethod;
  end;

  TGeniusTransactionDiscount = class(TInterfacedObject, IGeniusTransactionDiscount)
  private
    FOwner: TGeniusTransactionResponse;
    FAmount: Currency;
    FDiscountType: String;
    FMessage: String;
  public
    constructor Create(AOwner: TGeniusTransactionResponse);
    destructor Destroy; override;
  public
    function GetAmount: Currency;
    function GetDiscountType: String;
    function GetMessage: String;
    procedure SetAmount(const Value: Currency);
    procedure SetDiscountType(const Value: String);
    procedure SetMessage(const Value: String);

    function Owner: IGeniusTransactionResponse;

    property Amount: Currency read GetAmount write SetAmount;
    property DiscountType: String read GetDiscountType write SetDiscountType;
    property Message: String read GetMessage write SetMessage;
  end;

  TGeniusSignatureResponse = class(TInterfacedObject, IGeniusSignatureResponse)
  private
    FStatus: TGeniusSignatureStatus;
    FData: String;
    FRequestID: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetData: String;
    procedure SetData(const Value: String);
    function GetRequestID: String;
    procedure SetRequestID(const Value: String);
    function GetStatus: TGeniusSignatureStatus;
    procedure SetStatus(const Value: TGeniusSignatureStatus);

    property Status: TGeniusSignatureStatus read GetStatus write SetStatus;
    property Data: String read GetData write SetData;
    property RequestID: String read GetRequestID write SetRequestID;
  end;

  TGeniusLineItems = class(TInterfacedObject, IGeniusLineItems)
  private
    FOwner: TGenius;
    FItems: TInterfaceList;
    FInProgress: Boolean;
    FOrder: String;
    FOrderTotal: Currency;
    FOrderTax: Currency;
    FDisplayCustomSubtotal: String;
    FAutoTotal: Boolean;
  public
    constructor Create(AOwner: TGenius);
    destructor Destroy; override;
  public
    function GetOrder: String;
    procedure SetOrder(const Value: String);
    function GetDisplayCustomSubtotal: String;
    procedure SetDisplayCustomSubtotal(const Value: String);
    function GetAutoTotal: Boolean;
    procedure SetAutoTotal(const Value: Boolean);

    function GetItem(const Index: Integer): IGeniusLineItem;

    function Count: Integer;
    function InProgress: Boolean;
    function StartOrder(const Order: String): IGeniusStartOrderResponse;
    function EndOrder(const ExternalPaymentType: TGeniusExternalPaymentType): IGeniusStartOrderResponse;
    function AddItem(
      const ItemTypeValue, Upc, Description: String;
      const Amount, TaxAmount: Currency;
      const Quantity: Integer = 1;
      const ItemType: TGeniusLineItemType = glSku;
      const Category: TGeniusLineItemCategory = glNone;
      const DisplayOverride: String = ''): IGeniusLineItem;
    function UpdateItem(
      const TargetItemId, ItemTypeValue, Upc, Description: String;
      const Amount, TaxAmount: Currency;
      const Quantity: Integer = 1;
      const ItemType: TGeniusLineItemType = glSku;
      const Category: TGeniusLineItemCategory = glNone;
      const DisplayOverride: String = ''): IGeniusLineItem;
    function DiscountItem(
      const TargetItem: IGeniusLineItem;
      const ItemTypeValue, Upc, Description: String;
      const Amount, TaxAmount: Currency;
      const Quantity: Integer = 1;
      const ItemType: TGeniusLineItemType = glSku;
      const Category: TGeniusLineItemCategory = glNone;
      const DisplayOverride: String = ''): IGeniusLineItemDiscount;
    procedure DeleteItem(const Index: Integer); overload;
    procedure DeleteItem(const ItemID: String); overload;
    procedure DeleteDiscount(const ADiscount: IGeniusLineItemDiscount);
    procedure ClearItems;
    function UpdateTotal(const OrderTotal, OrderTax: Currency): Boolean;
    function OrderTotal: Currency;
    function OrderTax: Currency;

    property Items[const Index: Integer]: IGeniusLineItem read GetItem; default;

    property AutoTotal: Boolean read GetAutoTotal write SetAutoTotal;
    property Order: String read GetOrder write SetOrder;
    property DisplayCustomSubtotal: String read GetDisplayCustomSubtotal write SetDisplayCustomSubtotal;
  end;

  TGeniusLineItem = class(TInterfacedObject, IGeniusLineItem)
  private
    FOwner: TGeniusLineItems;
    FDiscounts: TInterfaceList;
    FStatus: TGeniusLineItemStatus;
    FResponseMessage: String;
    FOrder: String;
    FItemID: String;
    FItemType: TGeniusLineItemType;
    FItemTypeValue: String;
    FUPC: String;
    FQuantity: Integer;
    FDescription: String;
    FAmount: Currency;
    FTaxAmount: Currency;
    FCategory: TGeniusLineItemCategory;
    FDisplayOverride: String;
  public
    constructor Create(AOwner: TGeniusLineItems);
    destructor Destroy; override;
  public
    function GetAmount: Currency;
    function GetCategory: TGeniusLineItemCategory;
    function GetDescription: String;
    function GetDisplayOverride: String;
    function GetItemID: String;
    function GetItemType: TGeniusLineItemType;
    function GetItemTypeValue: String;
    function GetOrder: String;
    function GetQuantity: Integer;
    function GetTaxAmount: Currency;
    function GetUPC: String;
    procedure SetAmount(const Value: Currency);
    procedure SetCategory(const Value: TGeniusLineItemCategory);
    procedure SetDescription(const Value: String);
    procedure SetDisplayOverride(const Value: String);
    procedure SetItemID(const Value: String);
    procedure SetItemType(const Value: TGeniusLineItemType);
    procedure SetItemTypeValue(const Value: String);
    procedure SetOrder(const Value: String);
    procedure SetQuantity(const Value: Integer);
    procedure SetTaxAmount(const Value: Currency);
    procedure SetUPC(const Value: String);
    function GetResponseMessage: String;
    function GetStatus: TGeniusLineItemStatus;
    procedure SetResponseMessage(const Value: String);
    procedure SetStatus(const Value: TGeniusLineItemStatus);
    function GetDiscount(const Index: Integer): IGeniusLineItemDiscount;

    function Owner: IGeniusLineItems;

    property Status: TGeniusLineItemStatus read GetStatus write SetStatus;
    property ResponseMessage: String read GetResponseMessage write SetResponseMessage;

    function DiscountCount: Integer;
    property Discounts[const Index: Integer]: IGeniusLineItemDiscount read GetDiscount;
    function AddDiscount: IGeniusLineItemDiscount;
    procedure DeleteDiscount(const Index: Integer);
    procedure ClearDiscounts;

    property Order: String read GetOrder write SetOrder;
    property ItemID: String read GetItemID write SetItemID;
    property ItemType: TGeniusLineItemType read GetItemType write SetItemType;
    property ItemTypeValue: String read GetItemTypeValue write SetItemTypeValue;
    property UPC: String read GetUPC write SetUPC;
    property Quantity: Integer read GetQuantity write SetQuantity;
    property Description: String read GetDescription write SetDescription;
    property Amount: Currency read GetAmount write SetAmount;
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;
    property Category: TGeniusLineItemCategory read GetCategory write SetCategory;
    property DisplayOverride: String read GetDisplayOverride write SetDisplayOverride;
  end;

  TGeniusLineItemDiscount = class(TInterfacedObject, IGeniusLineItemDiscount)
  private
    FOwner: TGeniusLineItem;
    FStatus: TGeniusLineItemStatus;
    FResponseMessage: String;
    FOrder: String;
    FItemID: String;
    FTargetItemID: String;
    FItemType: TGeniusLineItemType;
    FItemTypeValue: String;
    FUPC: String;
    FQuantity: Integer;
    FDescription: String;
    FAmount: Currency;
    FTaxAmount: Currency;
    FOrderTotal: Currency;
    FOrderTax: Currency;
    FCategory: TGeniusLineItemCategory;
    FDisplayOverride: String;
    FDisplayCustomSubTotal: String;
  public
    constructor Create(AOwner: TGeniusLineItem);
    destructor Destroy; override;
  public
    function GetAmount: Currency;
    function GetCategory: TGeniusLineItemCategory;
    function GetDescription: String;
    function GetDisplayCustomSubTotal: String;
    function GetDisplayOverride: String;
    function GetItemID: String;
    function GetItemType: TGeniusLineItemType;
    function GetItemTypeValue: String;
    function GetOrder: String;
    function GetOrderTax: Currency;
    function GetOrderTotal: Currency;
    function GetQuantity: Integer;
    function GetTaxAmount: Currency;
    function GetUPC: String;
    procedure SetAmount(const Value: Currency);
    procedure SetCategory(const Value: TGeniusLineItemCategory);
    procedure SetDescription(const Value: String);
    procedure SetDisplayCustomSubTotal(const Value: String);
    procedure SetDisplayOverride(const Value: String);
    procedure SetItemID(const Value: String);
    procedure SetItemType(const Value: TGeniusLineItemType);
    procedure SetItemTypeValue(const Value: String);
    procedure SetOrder(const Value: String);
    procedure SetOrderTax(const Value: Currency);
    procedure SetOrderTotal(const Value: Currency);
    procedure SetQuantity(const Value: Integer);
    procedure SetTaxAmount(const Value: Currency);
    procedure SetUPC(const Value: String);
    function GetResponseMessage: String;
    function GetStatus: TGeniusLineItemStatus;
    procedure SetResponseMessage(const Value: String);
    procedure SetStatus(const Value: TGeniusLineItemStatus);
    function GetTargetItemID: String;
    procedure SetTargetItemID(const Value: String);

    function Owner: IGeniusLineItem;

    property Status: TGeniusLineItemStatus read GetStatus write SetStatus;
    property ResponseMessage: String read GetResponseMessage write SetResponseMessage;

    property Order: String read GetOrder write SetOrder;
    property ItemID: String read GetItemID write SetItemID;
    property TargetItemID: String read GetTargetItemID write SetTargetItemID;
    property ItemType: TGeniusLineItemType read GetItemType write SetItemType;
    property ItemTypeValue: String read GetItemTypeValue write SetItemTypeValue;
    property UPC: String read GetUPC write SetUPC;
    property Quantity: Integer read GetQuantity write SetQuantity;
    property Description: String read GetDescription write SetDescription;
    property Amount: Currency read GetAmount write SetAmount;
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;
    property OrderTotal: Currency read GetOrderTotal write SetOrderTotal;
    property OrderTax: Currency read GetOrderTax write SetOrderTax;
    property Category: TGeniusLineItemCategory read GetCategory write SetCategory;
    property DisplayOverride: String read GetDisplayOverride write SetDisplayOverride;
    property DisplayCustomSubTotal: String read GetDisplayCustomSubTotal write SetDisplayCustomSubTotal;
  end;

  TGeniusStartOrderResponse = class(TInterfacedObject, IGeniusStartOrderResponse)
  private
    FStatus: TGeniusLineItemStatus;
    FResponseMessage: String;
    //FAdditionalParameters: ???; //TODO
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetResponseMessage: String;
    procedure SetResponseMessage(const Value: String);
    function GetStatus: TGeniusLineItemStatus;
    procedure SetStatus(const Value: TGeniusLineItemStatus);

    property Status: TGeniusLineItemStatus read GetStatus write SetStatus;
    property ResponseMessage: String read GetResponseMessage write SetResponseMessage;
  end;

  TGeniusCancelTransactionResponse = class(TInterfacedObject, IGeniusCancelTransactionResponse)
  private
    FStatus: TGeniusCancelTransactionStatus;
    FResponseMessage: String;
    //FAdditionalParameters: ???; //TODO
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetResponseMessage: String;
    procedure SetResponseMessage(const Value: String);
    function GetStatus: TGeniusCancelTransactionStatus;
    procedure SetStatus(const Value: TGeniusCancelTransactionStatus);

    property Status: TGeniusCancelTransactionStatus read GetStatus write SetStatus;
    property ResponseMessage: String read GetResponseMessage write SetResponseMessage;
  end;

  TGeniusPaymentDetails = class(TInterfacedObject, IGeniusPaymentDetails)
  private
    FOwner: TGenius;
    FStatus: TGeniusTransStatus;
    FErrorMessage: String;
    FTotalAmountApproved: Currency;
    FRequestedAmount: Currency;
    FResponseType: TGeniusResponseType;
    FPaymentDetails: TInterfaceList;
  public
    constructor Create(AOwner: TGenius);
    destructor Destroy; override;
    function Add: IGeniusPaymentDetail;
    procedure Delete(const Index: Integer);
    procedure Clear;
  public
    function GetStatus: TGeniusTransStatus;
    procedure SetStatus(const Value: TGeniusTransStatus);
    function GetErrorMessage: String;
    procedure SetErrorMessage(const Value: String);
    function GetTotalAmountApproved: Currency;
    procedure SetTotalAmountApproved(const Value: Currency);
    function GetRequestedAmount: Currency;
    procedure SetRequestedAmount(const Value: Currency);
    function GetResponseType: TGeniusResponseType;
    procedure SetResponseType(const Value: TGeniusResponseType);
    function GetPaymentDetails(const Index: Integer): IGeniusPaymentDetail;

    function PaymentCount: Integer;

    property Status: TGeniusTransStatus read GetStatus write SetStatus;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property TotalAmountApproved: Currency read GetTotalAmountApproved write SetTotalAmountApproved;
    property RequestedAmount: Currency read GetRequestedAmount write SetRequestedAmount;
    property ResponseType: TGeniusResponseType read GetResponseType write SetResponseType;
    property PaymentDetails[const Index: Integer]: IGeniusPaymentDetail read GetPaymentDetails;
  end;

  TGeniusPaymentDetail = class(TInterfacedObject, IGeniusPaymentDetail)
  private
    FOwner: TGeniusPaymentDetails;
    FPaymentType: TGeniusPaymentType;
    FStatus: TGeniusTransStatus;
    FErrorMessage: String;
    FTransactionType: TGeniusTransactionType;
    FToken: String;
    FAuthorizationCode: String;
    FCustomer: String;
    FEmail: String;
    FPhoneNumber: String;
    FAccountNumber: String;
    FExpirationDate: TExpirationDate;
    FEntryMode: TGeniusEntryMode;
    FTransactionDate: TDateTime;
    FAmountApproved: Currency;
    FAmountCharged: Currency;
    FTaxAmount: Currency;
    FTipAmount: Currency;
    FUserTipAmount: Currency;
    FDiscountAmount: Currency;
    FVoucherAmount: Currency;
    FSignatureType: TGeniusSignatureType;
    FSignature: String;
    FGiftBalance: Currency;
    FLoyaltyVisits: Integer;
    FLoyaltyLastVisit: TDateTime;
    FLoyaltyLifetimeSpend: Currency;
    FLoyaltyBalance: Currency;
  public
    constructor Create(AOwner: TGeniusPaymentDetails);
    destructor Destroy; override;
  public
    function GetAmountApproved: Currency;
    function GetAmountCharged: Currency;
    function GetAuthorizationCode: String;
    function GetCustomer: String;
    function GetDiscountAmount: Currency;
    function GetEmail: String;
    function GetEntryMode: TGeniusEntryMode;
    function GetErrorMessage: String;
    function GetExpirationDate: TExpirationDate;
    function GetGiftBalance: Currency;
    function GetLoyaltyBalance: Currency;
    function GetLoyaltyLastVisit: TDateTime;
    function GetLoyaltyLifetimeSpend: Currency;
    function GetLoyaltyVisits: Integer;
    function GetPaymentType: TGeniusPaymentType;
    function GetPhoneNumber: String;
    function GetSignature: String;
    function GetSignatureType: TGeniusSignatureType;
    function GetStatus: TGeniusTransStatus;
    function GetTaxAmount: Currency;
    function GetTipAmount: Currency;
    function GetToken: String;
    function GetTransactionDate: TDateTime;
    function GetTransactionType: TGeniusTransactionType;
    function GetUserTipAmount: Currency;
    function GetVoucherAmount: Currency;
    procedure SetAmountApproved(const Value: Currency);
    procedure SetAmountCharged(const Value: Currency);
    procedure SetAuthorizationCode(const Value: String);
    procedure SetCustomer(const Value: String);
    procedure SetDiscountAmount(const Value: Currency);
    procedure SetEmail(const Value: String);
    procedure SetEntryMode(const Value: TGeniusEntryMode);
    procedure SetErrorMessage(const Value: String);
    procedure SetExpirationDate(const Value: TExpirationDate);
    procedure SetGiftBalance(const Value: Currency);
    procedure SetLoyaltyBalance(const Value: Currency);
    procedure SetLoyaltyLastVisit(const Value: TDateTime);
    procedure SetLoyaltyLifetimeSpend(const Value: Currency);
    procedure SetLoyaltyVisits(const Value: Integer);
    procedure SetPaymentType(const Value: TGeniusPaymentType);
    procedure SetPhoneNumber(const Value: String);
    procedure SetSignature(const Value: String);
    procedure SetSignatureType(const Value: TGeniusSignatureType);
    procedure SetStatus(const Value: TGeniusTransStatus);
    procedure SetTaxAmount(const Value: Currency);
    procedure SetTipAmount(const Value: Currency);
    procedure SetToken(const Value: String);
    procedure SetTransactionDate(const Value: TDateTime);
    procedure SetTransactionType(const Value: TGeniusTransactionType);
    procedure SetUserTipAmount(const Value: Currency);
    procedure SetVoucherAmount(const Value: Currency);
    function GetAccountNumber: String;
    procedure SetAccountNumber(const Value: String);

    function Owner: IGeniusPaymentDetails;

    property PaymentType: TGeniusPaymentType read GetPaymentType write SetPaymentType;
    property Status: TGeniusTransStatus read GetStatus write SetStatus;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property TransactionType: TGeniusTransactionType read GetTransactionType write SetTransactionType;
    property Token: String read GetToken write SetToken;
    property AuthorizationCode: String read GetAuthorizationCode write SetAuthorizationCode;
    property Customer: String read GetCustomer write SetCustomer;
    property Email: String read GetEmail write SetEmail;
    property PhoneNumber: String read GetPhoneNumber write SetPhoneNumber;
    property AccountNumber: String read GetAccountNumber write SetAccountNumber;
    property ExpirationDate: TExpirationDate read GetExpirationDate write SetExpirationDate;
    property EntryMode: TGeniusEntryMode read GetEntryMode write SetEntryMode;
    property TransactionDate: TDateTime read GetTransactionDate write SetTransactionDate;
    property AmountApproved: Currency read GetAmountApproved write SetAmountApproved;
    property AmountCharged: Currency read GetAmountCharged write SetAmountCharged;
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;
    property TipAmount: Currency read GetTipAmount write SetTipAmount;
    property UserTipAmount: Currency read GetUserTipAmount write SetUserTipAmount;
    property DiscountAmount: Currency read GetDiscountAmount write SetDiscountAmount;
    property VoucherAmount: Currency read GetVoucherAmount write SetVoucherAmount;
    property SignatureType: TGeniusSignatureType read GetSignatureType write SetSignatureType;
    property Signature: String read GetSignature write SetSignature;
    property GiftBalance: Currency read GetGiftBalance write SetGiftBalance;
    property LoyaltyVisits: Integer read GetLoyaltyVisits write SetLoyaltyVisits;
    property LoyaltyLastVisit: TDateTime read GetLoyaltyLastVisit write SetLoyaltyLastVisit;
    property LoyaltyLifetimeSpend: Currency read GetLoyaltyLifetimeSpend write SetLoyaltyLifetimeSpend;
    property LoyaltyBalance: Currency read GetLoyaltyBalance write SetLoyaltyBalance;
  end;

  TGeniusStatusResponse = class(TInterfacedObject, IGeniusStatusResponse)
  private
    FStatus: TGeniusCedStatus;
    FCurrentScreen: TGeniusCedScreen;
    FResponseMessage: String;
    FSerialNumber: String;
    FApplicationVersion: String;
    FOSVersion: String;
    FPaymentDataCaptured: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetApplicationVersion: String;
    function GetCurrentScreen: TGeniusCedScreen;
    function GetOSVersion: String;
    function GetPaymentDataCaptured: Boolean;
    function GetResponseMessage: String;
    function GetSerialNumber: String;
    function GetStatus: TGeniusCedStatus;
    procedure SetApplicationVersion(const Value: String);
    procedure SetCurrentScreen(const Value: TGeniusCedScreen);
    procedure SetOSVersion(const Value: String);
    procedure SetPaymentDataCaptured(const Value: Boolean);
    procedure SetResponseMessage(const Value: String);
    procedure SetSerialNumber(const Value: String);
    procedure SetStatus(const Value: TGeniusCedStatus);

    property Status: TGeniusCedStatus read GetStatus write SetStatus;
    property CurrentScreen: TGeniusCedScreen read GetCurrentScreen write SetCurrentScreen;
    property ResponseMessage: String read GetResponseMessage write SetResponseMessage;
    property SerialNumber: String read GetSerialNumber write SetSerialNumber;
    property ApplicationVersion: String read GetApplicationVersion write SetApplicationVersion;
    property OSVersion: String read GetOSVersion write SetOSVersion;
    property PaymentDataCaptured: Boolean read GetPaymentDataCaptured write SetPaymentDataCaptured;
  end;

  TGeniusAgreementResponse = class(TInterfacedObject, IGeniusAgreementResponse)
  private
    FRequestID: String;
    FStatus: TGeniusAgreementResponseStatus;
    FErrorMessage: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetRequestID: String;
    procedure SetRequestID(const Value: String);
    function GetStatus: TGeniusAgreementResponseStatus;
    procedure SetStatus(const Value: TGeniusAgreementResponseStatus);
    function GetErrorMessage: String;
    procedure SetErrorMessage(const Value: String);

    property RequestID: String read GetRequestID write SetRequestID;
    property Status: TGeniusAgreementResponseStatus read GetStatus write SetStatus;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
  end;

procedure ParseAgreementResponse(const XML: IXMLDocument; var Response: IGeniusAgreementResponse);

implementation

uses
  StrUtils;


{ TCayanEndpoints }

constructor TCayanEndpoints.Create;
begin

end;

destructor TCayanEndpoints.Destroy;
begin

  inherited;
end;

class function TCayanEndpoints.GetReportingUrl(const TestMode: Boolean): String;
begin
  if TestMode then begin
    Result:= '';
  end else begin
    Result:= '';
  end;
end;

class function TCayanEndpoints.GetStagingUrl(const TestMode: Boolean): String;
begin
  if TestMode then begin
    Result:= '';
  end else begin
    Result:= '';
  end;
end;

class function TCayanEndpoints.GetTransportUrl(const TestMode: Boolean): String;
begin
  if TestMode then begin
    Result:= '';
  end else begin
    Result:= '';
  end;
end;

{$REGION 'TGeniusTransactionThread Object'}

function TrimStr(const Str: String; const Len: Integer): String;
begin
  Result:= Copy(Str, 1, Len);
end;

procedure ParseAgreementResponse(const XML: IXMLDocument; var Response: IGeniusAgreementResponse);
var
  Node: IXmlNode;
begin
  Node:= GetNodePath(XML, '/AgreementTextResponse');
  if Assigned(Node) then begin
    Response.RequestID:= GetNodeValue(Node, 'RequestID');
    Response.Status:= GeniusStrToAgreementResponseStatus(GetNodeValue(Node, 'Status'));
    Response.ErrorMessage:= GetNodeValue(Node, 'ErrorMessage');
  end else begin
    Response.Status:= TGeniusAgreementResponseStatus.arsError;
    Response.ErrorMessage:= 'Failed to get agreement from CED';
  end;
end;

{ TGeniusRequestThread }

constructor TGeniusRequestThread.Create(AOwner: IGenius; const AURL: String;
  const AResponseEvent: TGeniusRequestThreadEvent);
begin
  inherited Create(True);
  FreeOnTerminate:= True;
  FOwner:= AOwner;
  FUrl:= AURL;
  FResponseEvent:= AResponseEvent;
end;

destructor TGeniusRequestThread.Destroy;
begin

  inherited;
end;

procedure TGeniusRequestThread.Execute;
var
  Web: TIdHTTP;
  OpenSSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  if Terminated then Exit;
  try
    Web:= TIdHTTP.Create(nil);
    OpenSSL:= TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      try
        Web.ReadTimeout:= 0;
        Web.IOHandler:= OpenSSL;
        Web.HandleRedirects:= True;
        FResponseText:= Web.Get(FUrl);
      except
        on E: Exception do begin
          FResponseText:= 'ERROR: ' + E.Message;
        end;
      end;
    finally
      FreeAndNil(OpenSSL);
      FreeAndNil(Web);
    end;
  finally
    Synchronize(DoOnResponse);
  end;
  Terminate;
end;

procedure TGeniusRequestThread.DoOnResponse;
begin
  if Assigned(FResponseEvent) then
    FResponseEvent(Self, FURL, FResponseText);
end;

{ TGeniusTransactionThread }

constructor TGeniusTransactionThread.Create(AOwner: TGenius;
  const TransportKey: String);
var
  Pars: TParamList;
begin
  inherited Create(True);
  FreeOnTerminate:= True;
  try
    FOwner:= AOwner;
    FResponse:= TGeniusTransactionResponse.Create;
    FResponse._AddRef;
    FTransportKey:= TransportKey;
    FResponse.TransportKey:= FTransportKey;
    Pars:= TParamList.Create;
    try
      Pars['TransportKey']:= FTransportKey;
      Pars['Format']:= 'XML';
      FUrl:= FOwner.DeviceUrl(Pars.ParamStr, TGeniusDeviceVersion.gdVer2);
    finally
      FreeAndNil(Pars);
    end;
  finally
    //Resume;
    //TODO
  end;
end;

destructor TGeniusTransactionThread.Destroy;
begin
  FResponse._Release;
  FResponse:= nil;
  FOwner:= nil;
  inherited;
end;

procedure TGeniusTransactionThread.ParseXML;
var
  Xml: IXMLDocument;
  Node: IXmlNode;
  N2: IXmlNode;
  D: IGeniusTransactionDiscount;
  EMV: IGeniusTransactionResponseEmv;
  X: Integer;
  LoadedXML: Boolean;
begin
  //Presumably received response back from CED
  if Terminated then Exit;
  FResponse.ClearDiscountsApplied;
  FResponse.EmvResponse:= nil;

  Xml:= TXmlDocument.Create(nil);
  try
    try
      Xml.LoadFromXML(FResponseText);
      LoadedXML:= True;
    except
      on E: Exception do begin
        LoadedXML:= False;
      end;
    end;
    if LoadedXML then begin

      Node := GetNodePath(Xml, '//TransactionResult');
      if (Assigned(Node)) then begin
        FResponse.StatusStr:= GetNodeValue(Node, 'Status');
        FResponse.Status:= GeniusStrToTransStatus(GetNodeValue(Node, 'Status'));
        FResponse.AmountApproved:= StrToCurrDef(GetNodeValue(Node, 'AmountApproved'), 0);
        FResponse.AuthorizationCode:= GetNodeValue(Node, 'AuthorizationCode');
        FResponse.CardHolder:= GetNodeValue(Node, 'CardHolder');
        FResponse.AccountNumber:= GetNodeValue(Node, 'AccountNumber');
        FResponse.PaymentType:= GeniusStrToPaymentType(GetNodeValue(Node, 'PaymentType'));
        FResponse.EntryMode:= GeniusStrToEntryMode(GetNodeValue(Node, 'EntryMode'));
        FResponse.ErrorMessage:= GetNodeValue(Node, 'ErrorMessage');
        FResponse.Token:= GetNodeValue(Node, 'Token');
        FResponse.TransactionDate:= StrToDateTimeDef(GetNodeValue(Node, 'TransactionDate'), 0);
        FResponse.TransactionType:= GeniusStrToTransactionType(GetNodeValue(Node, 'TransactionType'));
        FResponse.ResponseType:= GeniusStrToResponseType(GetNodeValue(Node, 'ResponseType'));
        FResponse.ValidationKey:= GetNodeValue(Node, 'ValidationKey');

        Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters');
        if Assigned(Node) then begin
          FResponse.SignatureData:= GetNodeValue(Node, 'SignatureData');

          Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/AmountDetails');
          if Assigned(Node) then begin
            FResponse.UserTip:=   StrToCurrDef(GetNodeValue(Node, 'UserTip'), 0);
            FResponse.Cashback:=  StrToCurrDef(GetNodeValue(Node, 'Cashback'), 0);
            FResponse.Donation:=  StrToCurrDef(GetNodeValue(Node, 'Donation'), 0);
            FResponse.Surcharge:= StrToCurrDef(GetNodeValue(Node, 'Surcharge'), 0);

            Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/AmountDetails/Discount');
            if Assigned(Node) then begin
              FResponse.Discount:= StrToCurrDef(GetNodeValue(Node, 'Total'), 0);

              Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/AmountDetails/Discount/DiscountsApplied');
              if Assigned(Node) then begin
                for X := 0 to Node.ChildNodes.Count-1 do begin
                  N2:= Node.ChildNodes.Nodes[X];
                  D:= FResponse.AddDiscountApplied;
                  D.Amount:=        StrToCurrDef(GetNodeValue(N2, 'Amount'), 0);
                  D.DiscountType:=  GetNodeValue(N2, 'Type');
                  D.Message:=       GetNodeValue(N2, 'Message');
                end;
              end else begin
                //No discount details - okay to be missing
              end;

            end else begin
              FResponse.Discount:= 0;
            end;
          end else begin
            FResponse.UserTip:= 0;
            FResponse.Cashback:= 0;
            FResponse.Donation:= 0;
          end;

          Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/KeyedDetails');
          if Assigned(Node) then begin
            FResponse.KeyedExpiration:= GetNodeValue(Node, 'ExpirationDate'); //TODO
            FResponse.KeyedAvsStreetZipCode:= GetNodeValue(Node, 'AvsStreetZipCode');
            FResponse.KeyedAvsResponse:= GetNodeValue(Node, 'AvsResponse')[1];  //TODO
            FResponse.KeyedCvResponse:= GetNodeValue(Node, 'CvResponse')[1];  //TODO
          end else begin
            if FResponse.EntryMode = TGeniusEntryMode.geManual then begin
              //MISSING KEYED INFO!
            end else begin
              FResponse.KeyedExpiration:= '';
              FResponse.KeyedAvsStreetZipCode:= '';
              FResponse.KeyedAvsResponse:= #0;
              FResponse.KeyedCvResponse:= #0;
            end;
          end;

          //EMV Chip Card Support
          Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/EMV');
          if Assigned(Node) then begin
            FResponse.EmvResponse:= TGeniusTransactionResponseEmv.Create;
            Emv:= FResponse.EmvResponse;

            Emv.CvmResults:= GetNodeValue(Node, 'CvmResults');
            Emv.IssuerApplicationData:= GetNodeValue(Node, 'IssuerApplicationData');
            Emv.TerminalVerificationResults:= GetNodeValue(Node, 'TerminalVerificationResults');
            Emv.UnpredictableNumber:= StrToIntDef(GetNodeValue(Node, 'UnpredictableNumber'), 0);
            Emv.PosEntryMode:= GeniusStrToEntryMode(GetNodeValue(Node, 'PosEntryMode'));
            Emv.CryptogramInformationData:= GetNodeValue(Node, 'CryptogramInformationData');
            Emv.PINStatement:= GetNodeValue(Node, 'PINStatement');
            Emv.CvmMethod:= GeniusStrToCvmMethod(GetNodeValue(Node, 'CvmMethod'));

            Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/EMV/ApplicationInformation');
            Emv.Aid:= GetNodeValue(Node, 'Aid');
            Emv.ApplicationLabel:= GetNodeValue(Node, 'ApplicationLabel');
            Emv.ApplicationExpiryDate:= StrToDateTimeDef(GetNodeValue(Node, 'ApplicationExpiryDate'), 0);
            Emv.ApplicationEffectiveDate:= StrToDateTimeDef(GetNodeValue(Node, 'ApplicationEffectiveDate'), 0);
            Emv.ApplicationInterchangeProfile:= GetNodeValue(Node, 'ApplicationInterchangeProfile');
            Emv.ApplicationVersionNumber:= StrToIntDef(GetNodeValue(Node, 'ApplicationVersionNumber'), 0);
            Emv.ApplicationTransactionCounter:= StrToIntDef(GetNodeValue(Node, 'ApplicationTransactionCounter'), 0);

            Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/EMV/CardInformation');
            Emv.MaskedPan:= GetNodeValue(Node, 'MaskedPan');
            Emv.PanSequenceNumber:= StrToIntDef(GetNodeValue(Node, 'PanSequenceNumber'), 0);
            Emv.CardExpiryDate:= GetNodeValue(Node, 'CardExpiryDate');

            Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/EMV/ApplicationCryptogram');
            Emv.CryptogramType:= GeniusStrToCryptogramType(GetNodeValue(Node, 'CryptogramType'));
            Emv.Cryptogram:= GetNodeValue(Node, 'Cryptogram');

            Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/EMV/Amount');
            Emv.AmountAuthorized:= StrToCurrDef(GetNodeValue(Node, 'AmountAuthorized'), 0);
            Emv.AmountOther:= StrToCurrDef(GetNodeValue(Node, 'AmountOther'), 0);

            Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/EMV/TerminalInformation');
            Emv.TerminalType:= StrToIntDef(GetNodeValue(Node, 'TerminalType'), 0);
            Emv.IfdSerialNumber:= GetNodeValue(Node, 'IfdSerialNumber');
            Emv.TerminalCountryCode:= GetNodeValue(Node, 'TerminalCountryCode');

            Node:= GetNodePath(Xml, '//TransactionResult/AdditionalParameters/EMV/TransactionInformation');
            Emv.TransactionType:= GeniusStrToEmvTransactionType(GetNodeValue(Node, 'TransactionType'));
            Emv.TransactionCurrencyCode:= GetNodeValue(Node, 'TransactionCurrencyCode');

          end else begin
            //EMV information not included
          end;

        end else begin
          FResponse.Status:= TGeniusTransStatus.gsError;
          FResponse.ErrorMessage:= 'Failed to locate CED additional parameters in XML';
        end;

      end else begin
        FResponse.Status:= TGeniusTransStatus.gsError;
        FResponse.ErrorMessage:= 'Failed to locate CED response in XML';
      end;
    end else begin
      FResponse.Status:= TGeniusTransStatus.gsError;
      FResponse.ErrorMessage:= 'Failed to parse CED response XML';
      //TODO: check for reason why it failed to parse XML
      //NOTE: Emulator responds with HTML page upon error

    end;
  finally
    Node:= nil;
    Xml:= nil;
  end;
end;

procedure TGeniusTransactionThread.DoOnResponse;
begin
  FOwner.TransThreadResponse(FResponse);
end;

procedure TGeniusTransactionThread.Execute;
var
  Web: TIdHTTP;
  OpenSSL: TIdSSLIOHandlerSocketOpenSSL;
begin
(*
  Send transaction info to device and wait for response once transaction completes
  V1: http://[CED-IP-Address]:8080/v1/pos?TransportKey=xxxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxxxx&Format=XML
  V2: http://[CED-IP-Address]:8080/v2/pos?TransportKey=xxxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxxxx&Format=XML

  Use GET to send the TransportKey to the Customer Engagement Device.
  The device will prompt the customer to select the payment type and complete the transaction.
  The Point of Sale will keep the port open to the device and wait for the GET request to complete.
  The GET response will contain an XML, JSON or JSONP packet.
*)

  if Terminated then Exit;
  try
    Web:= TIdHTTP.Create(nil);
    OpenSSL:= TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      try
        //Prepare request
        Web.ReadTimeout:= 0;  //TODO: Make sure thread waits indefinitely for response
        Web.IOHandler:= OpenSSL;
        Web.HandleRedirects:= True;

        FResponseText:= Web.Get(FUrl);

        //At this point, thread will freeze and wait for response from device
        //NOTE: This part could wait for minutes before response

        ParseXML;

      except
        on E: Exception do begin
          FResponse.Status:= TGeniusTransstatus.gsError;
          FResponse.ErrorMessage:= E.Message;
        end;
      end;

    finally
      FreeAndNil(OpenSSL);
      FreeAndNil(Web);
    end;
  finally

    Synchronize(DoOnResponse);

    //TODO: Fix to synchronize response - currently not working since in DLL
    //PROBLEM: Synchronize doesn't work on a thread which runs inside a DLL.
    //Synchronize depends on the main thread and "Application" instance, which
    //in a DLL, "Application" is a spearate instance which does not work.

  end;
  Terminate;
end;

{$ENDREGION}

{$REGION 'TGeniusStatusThread Object'}

{ TGeniusStatusThread }

constructor TGeniusStatusThread.Create(AOwner: TGenius);
begin
  inherited Create(True);
  try
    FOwner:= AOwner;
    FStatusCheckProc:= FOwner.StatusCheck;
  finally
    //Resume;
    //TODO
  end;
end;

destructor TGeniusStatusThread.Destroy;
begin
  FreeResponse;
  FOwner:= nil;
  inherited;
end;

procedure TGeniusStatusThread.DoOnResponse;
begin
  if Assigned(FOnResponse) then
    FOnResponse(FOwner, FResponse);
end;

procedure TGeniusStatusThread.FreeResponse;
begin
  if Assigned(FResponse) then begin
    FResponse._Release;
    FResponse:= nil;
  end;
end;

procedure TGeniusStatusThread.Execute;
begin
  {$IFDEF MSWINDOWS}
  CoInitialize(nil);
  {$ENDIF}
  try
    while not Terminated do begin
      //Only perform check if active and can report response
      if (FActive) and (Assigned(FOnResponse)) and
        (FOwner.Monitoring) and (not FOwner.TestMode) then
      begin
        while (FActive) and (not Terminated) do begin

          FreeResponse;

          try
            if Terminated or (not FActive) then Break;
            FResponse:= FStatusCheckProc(700);
            FResponse._AddRef;
          except
            on E: Exception do begin
              FResponse:= TGeniusStatusResponse.Create;
              FResponse._AddRef;
              FResponse.Status:= TGeniusCedStatus.csOffline;
              FResponse.CurrentScreen:= TGeniusCedScreen.csNoScreen;
              FResponse.ResponseMessage:= E.Message;
            end;
          end;
          Synchronize(DoOnResponse);
          //Sleep for 3 second3, 200 ms intervals

          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;

          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;

          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;
          Sleep(200);
          if Terminated or (not FActive) then Break;


        end;
        Sleep(200);
        if Terminated then Break;
      end;
      Sleep(10);
    end;

    FreeResponse;

  finally
    {$IFDEF MSWINDOWS}
    CoUninitialize;
    {$ENDIF}
  end;
end;

procedure TGeniusStatusThread.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

{$ENDREGION}

{$REGION 'TGenius Object'}

{ TGenius }

constructor TGenius.Create(AOwner: TMerchantWare);
begin
  FOwner:= AOwner;
  FWebCayan:= TIdHTTP.Create(nil);
  FSSLCayan:= TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FWebCayan.IOHandler:= FSSLCayan;
  FWebCayan.HandleRedirects:= True;

  FWebDevice:= TIdHTTP.Create(nil);
  FSSLDevice:= TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FWebDevice.IOHandler:= FSSLDevice;
  FWebDevice.HandleRedirects:= True;

  FDeviceTimeout:= 900; //msec
  FLineItems:= TGeniusLineItems.Create(Self);
  IGeniusLineItems(FLineItems)._AddRef;
  FDeviceVersion:= TGeniusDeviceVersion.gdVer1;
  FStatusThread:= TGeniusStatusThread.Create(Self);
  FStatusThread.OnResponse:= StatusResponse;
  FStatusThread.Start;
end;

destructor TGenius.Destroy;
begin
  FStatusThread.Active:= False;
  FStatusThread.Terminate;
  FStatusThread.WaitFor; //TODO...?
  FreeAndNil(FStatusThread);
  IGeniusLineItems(FLineItems)._Release;
  FLineItems:= nil;
  FreeAndNil(FWebDevice);
  FreeAndNil(FSSLDevice);
  FreeAndNil(FWebCayan);
  FreeAndNil(FSSLCayan);
  inherited;
end;

procedure TGenius.StatusResponse(Sender: IGenius;
  const Status: IGeniusStatusResponse);
begin
  //TODO: Trigger status response event to owner
  if Assigned(FOnStatus) then
    FOnStatus(Self, Status);
end;

function TGenius.GetDba: String;
begin
  Result:= FDba;
end;

function TGenius.GetDeviceAddress: String;
begin
  Result:= FDeviceAddress;
end;

function TGenius.GetDevicePort: Integer;
begin
  Result:= FDevicePort;
end;

function TGenius.GetDeviceProtocol: TGeniusProtocol;
begin
  Result:= FDeviceProtocol;
end;

function TGenius.GetDeviceTimeout: Integer;
begin
  Result:= FDeviceTimeout;
end;

function TGenius.GetDeviceVersion: TGeniusDeviceVersion;
begin
  Result:= FDeviceVersion;
end;

function TGenius.GetLineItems: IGeniusLineItems;
begin
  Result:= FLineItems;
end;

function TGenius.GetMerchantKey: String;
begin
  Result:= FOwner.Key;
end;

function TGenius.GetMerchantName: String;
begin
  Result:= FOwner.Name;
end;

function TGenius.GetMerchantSiteId: String;
begin
  Result:= FOwner.SiteId;
end;

function TGenius.GetMonitoring: Boolean;
begin
  Result:= FStatusThread.Active;
end;

function TGenius.GetOnStatus: TGeniusStatusResponseEvent;
begin
  Result:= FOnStatus;
end;

function TGenius.GetSoftwareName: String;
begin
  Result:= FSoftwareName;
end;

function TGenius.GetSoftwareVersion: String;
begin
  Result:= FSoftwareVersion;
end;

function TGenius.GetTestMode: Boolean;
begin
  Result:= FOwner.TestMode;
end;

function TGenius.GetTransactionResponse: TGeniusTransactionEvent;
begin
  Result:= FTransactionResponse;
end;

procedure TGenius.SetDba(const Value: String);
begin
  FDba:= Value;
end;

procedure TGenius.SetDeviceAddress(const Value: String);
begin
  FDeviceAddress:= Value;
end;

procedure TGenius.SetDevicePort(const Value: Integer);
begin
  FDevicePort:= Value;
end;

procedure TGenius.SetDeviceProtocol(const Value: TGeniusProtocol);
begin
  FDeviceProtocol:= Value;
end;

procedure TGenius.SetDeviceTimeout(const Value: Integer);
begin
  FDeviceTimeout:= Value;
end;

procedure TGenius.SetDeviceVersion(const Value: TGeniusDeviceVersion);
begin
  FDeviceVersion:= Value;
end;

procedure TGenius.SetMonitoring(const Value: Boolean);
begin
  FStatusThread.Active:= Value;
end;

procedure TGenius.SetOnStatus(const Value: TGeniusStatusResponseEvent);
begin
  FOnStatus:= Value;
end;

procedure TGenius.SetSoftwareName(const Value: String);
begin
  FSoftwarename:= Value;
end;

procedure TGenius.SetSoftwareVersion(const Value: String);
begin
  FSoftwareVersion:= Value;
end;

procedure TGenius.SetTestMode(const Value: Boolean);
begin
  FOwner.TestMode:= Value;
end;

procedure TGenius.SetTransactionResponse(const Value: TGeniusTransactionEvent);
begin
  FTransactionResponse:= Value;
end;

function TGenius.CreateStageRequest: IGeniusStageRequest;
begin
  Result:= TGeniusStageRequest.Create;
  Result.Dba:= Dba;
  Result.SoftwareName:= SoftwareName;
  Result.SoftwareVersion:= SoftwareVersion;
end;

function TGenius.GetAgreement(const RequestID, Title, AgreementText,
  AcceptLabel, DeclineLabel: String): IGeniusAgreementResponse;
var
  Pars: TParamList;
  XML: IXMLDocument;
  Res: TGeniusAgreementResponse;
begin
(*
  Action=GetAgreement Sends request to prompt customer to accept or decline agreement.

  http://[CED-IP-Address]:8080/v2/pos?Action=GetAgreement&RequestID=xxx&Title=xxx
    &AgreementText=xxx&AcceptLabel=xxx&DeclineLabel=xxx&Format=xxx
*)
  Res:= TGeniusAgreementResponse.Create;
  Pars:= TParamList.Create;
  try
    Pars['Action']:= 'GetAgreement';
    Pars['RequestID']:= RequestID;
    Pars['Title']:= Title;
    Pars['AgreementText']:= AgreementText;
    Pars['AcceptLabel']:= AcceptLabel;
    Pars['DeclineLabel']:= DeclineLabel;
    Pars['Format']:= 'XML';
    XML:= SendDeviceRequestXML(DeviceUrl(Pars.ParamStr), 0);
    Result:= Res;
    Result._AddRef;
    try
      ParseAgreementResponse(XML, Result);
    finally
      Result._Release;
    end;
  finally
    FreeAndNil(Pars);
  end;
end;

function TGenius.GetCredentials(const CredMerch: Boolean = True; const Cap: Boolean = False): String;
var
  Pre, SID: String;
  procedure A(const S: String);
  begin
    Result:= Result + S + sLineBreak;
  end;
begin
  Pre:= IfThen(CredMerch, 'merchant', '');
  SID:= IfThen(Cap, 'SiteID', 'SiteId');
  if not TestMode then begin
    A(XmlVal(Pre+'Name', MerchantName));
    A(XmlVal(Pre+SID,    MerchantSiteId));
    A(XmlVal(Pre+'Key',  MerchantKey));
  end else begin
    A(XmlVal(Pre+'Name', TEST_MERCHANT_NAME));
    A(XmlVal(Pre+SID,    TEST_MERCHANT_SITE_ID));
    A(XmlVal(Pre+'Key',  TEST_MERCHANT_KEY));
  end;
end;

function TGenius.SendDeviceRequest(const Url: String; const Timeout: Integer = 0): String;
begin
  Result:= '';

  if not Assigned(FWebDevice) then Exit;

  if Timeout = 0 then
    FWebDevice.ConnectTimeout:= FDeviceTimeout
  else
    FWebDevice.ConnectTimeout:= Timeout;
  //Everything is controlled within the URL for device requests
  try
    Result:= FWebDevice.Get(Url);
  except
    on E: EIdConnectTimeout do begin
      //do nothing
    end;
    on E: Exception do begin

    end;
  end;
end;

function TGenius.SendDeviceRequestXML(const Url: String; const Timeout: Integer = 0): IXMLDocument;
var
  Res: String;
begin
  Res:= SendDeviceRequest(Url, Timeout);
  Result:= TXMLDocument.Create(nil);
  Result.LoadFromXML(Res);
end;

function TGenius.SendTransportRequest(const SvcUrl: String; const Ver: String; const ActionUrl: String;
  const Action: String; const AXML: String; const CredMerch: Boolean = True; const Cap: Boolean = False): IXMLDocument;
const
  XML_URL = '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">';
var
  Url: String;
  Str: TStringStream;
  Xml: String;
  procedure A(const S: String);
  begin
    Str.WriteString(S);
  end;
begin
  Result:= TXMLDocument.Create(nil);
  Str:= TStringStream.Create;
  try
    A('<?xml version="1.0" encoding="utf-8"?>' + sLineBreak);
    A(XML_URL +             sLineBreak);
    A('<soap:Body>' +       sLineBreak);
    A('<'+Action+' xmlns="'+ActionUrl+'">' + sLineBreak);
    A(GetCredentials(CredMerch, Cap));
    A(AXML                  ); // + sLineBreak); //Already has a line break
    A('</'+Action+'>' +     sLineBreak);
    A('</soap:Body>' +      sLineBreak);
    A('</soap:Envelope>' +  sLineBreak);
    //TODO: Incorporate version number (as it's different per request)
    Url:= TransportUrl(Ver, Action);
    FWebCayan.Request.ContentType:= 'text/xml';
    FWebCayan.Request.CharSet:= 'utf-8';
    Str.Position:= 0;
    Xml:= FWebCayan.Post(SvcUrl, Str);
    Result.LoadFromXML(Xml);
  finally
    FreeAndNil(Str);
  end;
end;

function TGenius.DeviceUrl(const Params: String;
  const Ver: TGeniusDeviceVersion = gdVer1): String;
var
  H: String;
  P: Integer;
  PR: String;
  V: String;
begin
  if TestMode then begin
    H:= TEST_HOST;
    P:= TEST_PORT;
    PR:= 'http';
  end else begin
    H:= DeviceAddress;
    P:= DevicePort;
    case DeviceProtocol of
      prHTTP:   PR:= 'http';
      prHTTPS:  PR:= 'https';
    end;
  end;
  case DeviceVersion of
    gdVer1: begin
      case Ver of
        gdVer1: V:= 'v1';
        gdVer2: V:= 'v2';
      end;
    end;
    gdVer2: V:= 'v2';
  end;
  Result:= Format(URL_DEVICE, [PR, H, P, V, Params]);
  Result:= UriEncode(Result);
end;

function TGenius.TransportUrl(const Ver, Req: String): String;
var
  F: String;
begin
  if TestMode then
    F:= TRANSPORT_TEST_URL
  else
    F:= TRANSPORT_PROD_URL;
  Result:= Format(F, [Ver, Req]);
end;

function TGenius.StageTransaction(const ARequest: IGeniusStageRequest): IGeniusStageResponse;
var
  X: Integer;
  S: String;
  Res: TGeniusStageResponse;
  Msg: IGeniusStageMessage;
  XML: IXMLDocument;
  Node, Node2: IXmlNode;
  function StageTransport: String;
  begin
    if Self.TestMode then
      Result:= URL_STAGE_TRANS_TEST
    else
      Result:= URL_STAGE_TRANS;
  end;
  procedure A(const Str: String);
  begin
    S:= S + Str + sLineBreak;
  end;
begin
(*
  Prepare transaction for further processing
  This is the very first stage to all interaction with Genius / CED
  It prepares transaction in Cayan's servers to be used in future CED requests

  API Call: CreateTransaction
  SOAPAction: http://transport.merchantware.net/v4/CreateTransaction
*)

  Res:= TGeniusStageResponse.Create;
  Result:= Res;

  A('<request>');
  A(XmlVal('TransactionType', GeniusTransactionTypeToStr(ARequest.TransactionType)));
  A(XmlVal('Amount', ARequest.Amount));
  A(XmlVal('ClerkId', ARequest.ClerkId));
  A(XmlVal('OrderNumber', ARequest.OrderNumber));
  A(XmlVal('Dba', ARequest.Dba));
  A(XmlVal('SoftwareName', ARequest.SoftwareName));
  A(XmlVal('SoftwareVersion', ARequest.SoftwareVersion));
  A(XmlVal('CardHolder', ARequest.CardHolder));
  A(XmlVal('TransactionId', ARequest.TransactionId));
  A(XmlVal('ForceDuplicate', ARequest.ForceDuplicate));
  A(XmlVal('CustomerCode', ARequest.CustomerCode));
  A(XmlVal('PoNumber', ARequest.PoNumber));
  A(XmlVal('TaxAmount', ARequest.TaxAmount));
  A(XmlVal('TerminalId', ARequest.TerminalId));
  A('</request>');

  XML:= SendTransportRequest(StageTransport, 'v4', 'http://transport.merchantware.net/v4/', 'CreateTransaction', S, True);
  try

    Node:= GetNodePath(XML, '//CreateTransactionResponse/CreateTransactionResult');
    Result.TransportKey:= GetNodeValue(Node, 'TransportKey');
    Result.ValidationKey:= GetNodeValue(Node, 'ValidationKey');

    Node:= GetNodePath(XML, '//CreateTransactionResponse/CreateTransactionResult/Messages');
    if Assigned(Node) then begin
      for X := 0 to Node.ChildNodes.Count-1 do begin
        Node2:= Node.childNodes.Nodes[X];
        Msg:= Res.Add;
        Msg.Field:= GetNodeValue(Node2, 'Field');
        Msg.Information:= GetNodeValue(Node2, 'Information');
      end;
    end;

  finally
    XML:= nil;
  end;
end;

function TGenius.InitiateTransaction(const TransportKey: String): Boolean;
var
  T: TGeniusTransactionThread;
begin
  //Create self-contained thread which sends transaction request to device
  //  and triggers callback event handler when device responds
  //  Thread self-terminates when finished
  //IMPORTANT: Thread starts processing transaction immediately upon creation!!!
  //NOTE: Self (AOwner) parameter contains callback procedure to be used

  try
    FCancelled:= False;
    FInTransaction:= True;
    T:= TGeniusTransactionThread.Create(Self, TransportKey);
    T.Start;
    Result:= True;
  except
    on E: Exception do begin
      Result:= False;
    end;
  end;
end;

function TGenius.IsInAgreement: Boolean;
begin
  Result:= FInAgreement;
end;

function TGenius.IsInCustomerInput: Boolean;
begin
  Result:= FInCustomerInput;
end;

function TGenius.IsInSignature: Boolean;
begin
  Result:= FInSignature;
end;

function TGenius.IsInTransaction: Boolean;
begin
  Result:= FInTransaction;
end;

procedure TGenius.ParseApprovalStatus(Value: String; var Stat: TGeniusTransStatusSet;
  var Code: Integer; var Msg: String);
var
  T: String;
begin
  //DECLINED;1023;invalid account num
  Value:= Value + ';';
  T:= Copy(Value, 1, Pos(';', Value)-1);
  Delete(Value, 1, Pos(';', Value));
  Stat:= [];
  if ContainsText(T, 'APPROVED') then
    Stat:= Stat + [gsApproved];
  if ContainsText(T, 'DECLINED') then
    Stat:= Stat + [gsDeclined];
  if ContainsText(T, 'ERROR') then
    Stat:= Stat + [gsError];
  //if ContainsText(T, 'REFERRAL') then
    //Stat:= Stat + [asReferral];
  if ContainsText(T, 'DUPLICATE') then
    Stat:= Stat + [gsDuplicate];
  T:= Copy(Value, 1, Pos(';', Value)-1);
  Delete(Value, 1, Pos(';', Value));
  Code:= StrToIntDef(T, 0);
  Msg:= Copy(Value, 1, Length(Value)-1);;
end;

procedure TGenius.TransThreadResponse(Response: IGeniusTransactionResponse);
var
  VR: IMWCreditResponse4;
begin
  //Received response from transaction thread
  //WARNING: This is run in the context of the thread. FTransactionResponse
  //  is also triggered within the same thread context
  //TODO: Change so this procedure is the core of further cancellation logic
  //NOTE: If response status indicates transaction already completed, must void instead

  if Assigned(FTransactionResponse) then begin
    try
      FInTransaction:= False;
      if FCancelled then begin
        if Response.Status = gsApproved then begin
          //Void transaction
          Response.ErrorMessage:= 'POS Cancelled Transaction';
          try                                       //Register, TransId
            VR:= FOwner.Credit.Void(Response.Token, '0', ''); //TODO: Use real values
            if asApproved in VR.ApprovalStatus then begin
              //Successfully voided transaction
              Response.ErrorMessage:= 'POS Cancelled and Voided Transaction';
            end else
            if asDeclined in VR.ApprovalStatus then begin
              //Voiding transaction was declined
              Response.ErrorMessage:= 'POS Cancelled Transaction but Void Declined: ' + VR.ErrorMessage;
            end else begin
              //Error voiding transaction
              Response.ErrorMessage:= 'POS Cancelled Transaction but Void Failed: ' + VR.ErrorMessage;
            end;
          except
            on E: Exception do begin
              //Failed to void
              Response.ErrorMessage:= 'POS Cancelled Transaction but Void Failed: ' + E.Message;
            end;
          end;
        end;
        Response.Status:= TGeniusTransStatus.gsPosCancelled;
      end
    finally
      //Trigger event - this must be called and only once
      FTransactionResponse(Response);
    end;
  end else begin
    raise Exception.Create('No transaction response event handler assigned.');
  end;
end;

function TGenius.CancelTransaction: IGeniusCancelTransactionResponse;
var
  Pars: TParamList;
  XML: IXMLDocument;
  Node: IXmlNode;
begin
  //Cancels a transaction currently in progress on CED
  //  Typically after InitiateTransaction before response
  //  InitiateTransaction runs indefinitely in a thread, and this
  //  triggers the device to respond with POS Cancelled.
  //NOTE: Cancellation will still wait for response from CED thread

  Result:= TGeniusCancelTransactionResponse.Create;
  Result.Status:= ctError;
  Result.ResponseMessage:= 'Request not sent to device';

  FInTransaction:= False;
  FInSignature:= False;
  FInAgreement:= False;
  FInCustomerInput:= False;
  FLineItems.FInProgress:= False;
  FCancelled:= True;

  Pars:= TParamList.Create;
  try
    Pars['Action']:= 'Cancel';
    Pars['Format']:= 'XML';

    XML:= SendDeviceRequestXML(DeviceUrl(Pars.ParamStr));
    try
      Node := GetNodePath(XML, '/CancelResult');
      if (Assigned(Node)) then begin
        Result.Status:= GeniusStrToCancelTransactionStatus(GetNodeValue(Node, 'Status'));
        Result.ResponseMessage:= GetNodeValue(Node, 'ResponseMessage');
        //Result.AdditionalParameters... //TODO
      end else begin
        //Failed to locate response node

      end;
    finally
      XML:= nil;
    end;
  finally
    FreeAndNil(Pars);
  end;
end;

function TGenius.InitiateKeyedEntry(): TGeniusKeyedSaleStatus;
var
  Pars: TParamList;
  XML: IXMLDocument;
  Node: IXmlNode;
begin
(*
  Action=InitiateKeyedEntry Sends a request to the CED to process manual
    entry of PAN, Expiration Date, CVV and Zip. This function can only
    be used while the payment selection is active for an existing transaction.
    Using this function without an existing sale on the device will result
    in a failed response. If you intend to use this feature you must
    contact the Certification team.

  //NOTE: CAN ONLY BE USED DURING CED TRANSACTION

  //NOTE: PaymentType only supports GIFT

  http://[CED-IP-Address]:8080/v1/pos?Action=InitiateKeyedEntry&PaymentType=GIFT&Format=XML
*)
  if not FInTransaction then
    raise Exception.Create('Cannot initiate keyed sale without current transaction');
  FInTransaction:= False;
  Pars:= TParamList.Create;
  try
    Pars['Action']:= 'InitiateKeyedEntry';
    Pars['Format']:= 'XML';

    try
      XML:= SendDeviceRequestXML(DeviceUrl(Pars.ParamStr));
      Node:= GetNodePath(XML, '/InitiateKeyedEntryResult');
      Result:= GeniusStrToKeyedSaleStatus(GetNodeValue(Node, 'Status'));
    except
      on E: Exception do begin
        Result:= TGeniusKeyedSaleStatus.gkFailure;
      end;
    end;
  finally
    FreeAndNil(Pars);
  end;
end;

function TGenius.StatusCheck(const Timeout: Integer = 0): IGeniusStatusResponse;
var
  Pars: TParamList;
  XML: IXMLDocument;
  Node: IXmlNode;
begin
(*
  Action=Status There are two versions of the Status request available.
  Version 1 sends a request to the CED to check which screen the device is on.
  Version 2 is analogous with verison 1 but also includes any AdditionalParameters fields.

  http://[CED-IP-Address]:8080/v2/pos?Action=Status&Format=XML
  https://[CED-IP-Address]:8443/v2/pos?Action=Status&Format=XML
*)
  Result:= TGeniusStatusResponse.Create;

  if TestMode then begin
    Result.Status:= TGeniusCedStatus.csOnline;
    Result.CurrentScreen:= TGeniusCedScreen.csIdle;
    Exit;
  end;

  Pars:= TParamList.Create;
  try
    Pars['Action']:= 'Status';
    Pars['Format']:= 'XML';

    XML:= SendDeviceRequestXML(DeviceUrl(Pars.ParamStr), Timeout);
    Node:= GetNodePath(XML, '/StatusResult');
    if Assigned(Node) then begin
      //TODO
      Result.Status:= GeniusStrToCedStatus(GetNodeValue(Node, 'Status'));
      Result.CurrentScreen:= GeniusStrToCedScreen(GetNodeValue(Node, 'CurrentScreen'));
      Result.ResponseMessage:= GetNodeValue(Node, 'ResponseMessage');
      Result.SerialNumber:= GetNodeValue(Node, 'SerialNumber');
      Result.ApplicationVersion:= GetNodeValue(Node, 'ApplicationVersion');
      Result.OSVersion:= GetNodeValue(Node, 'OSVersion');
      Result.PaymentDataCaptured:= SameText(GetNodeValue(Node, 'PaymentDataCaptured'), 'true');
    end else begin
      //Failed to get result
      Result.Status:= TGeniusCedStatus.csOffline;
      Result.ResponseMessage:= 'Failed to get status from CED';
      Result.PaymentDataCaptured:= False;
    end;
  finally
    FreeAndNil(Pars);
  end;
end;

function TGenius.CaptureSignature(const RequestId, Title: String): IGeniusSignatureResponse;
var
  Pars: TParamList;
  XML: IXMLDocument;
  Node: IXmlNode;
begin
  //The GetSignature request will allow the Point of Sale capture a customer's signature on demand.
  //This captures signature outside of standard transactions - typically if payment was made with cash or check
  FInSignature:= True;
  try
    Result:= TGeniusSignatureResponse.Create;
    Pars:= TParamList.Create;
    try
      Pars['Action']:= 'GetSignature';
      Pars['RequestID']:= RequestId;
      Pars['Title']:= Title;
      Pars['Format']:= 'XML';

      XML:= SendDeviceRequestXML(DeviceUrl(Pars.ParamStr));
      Node:= GetNodePath(XML, '/GetSignatureResult');
      if Assigned(Node) then begin
        Result.Status:= GeniusStrToSignatureStatus(GetNodeValue(Node, 'Status'));
        Result.Data:= GetNodeValue(Node, 'SignatureData');
        Result.RequestID:= GetNodeValue(Node, 'RequestID');
      end else begin
        Result.Status:= TGeniusSignatureStatus.ssTimeout;
        Result.Data:= '';
        Result.RequestID:= RequestId;
      end;
    finally
      FreeAndNil(Pars);
    end;
  finally
    FInSignature:= False;
  end;
end;

// -------------------------- WEB SERVER REQUESTS ------------------------------

function TGenius.DetailsByTransportKey(const TransportKey: String): IGeniusPaymentDetails;
var
  Res: TGeniusPaymentDetails;
  PD: IGeniusPaymentDetail;
  S: String;
  XML: IXMLDocument;
  Node, Node2, Node3: IXmlNode;
  X, Y: Integer;
begin
(*
  The DetailsByTransportKey web service method allows the Point of Sale developers
  to send in the original TransportKey and request additional payment information
  at a later time. The additional payment information is for reference purposes
  only and the Point of Sale developers are not required to retrieve this information.

  SOAPAction: http://schemas.merchantwarehouse.com/genius/10/Reporting/DetailsByTransportKey
  Endpoint: https://genius.merchantware.net/v1/Reporting.asmx
*)

  Res:= TGeniusPaymentDetails.Create(Self);
  Result:= Res;

  S:= XmlVal('TransportKey', TransportKey);

  XML:= SendTransportRequest(URL_RETR_TRANS, '10', URL_RETR_TRANS_ACT, 'DetailsByTransportKey', S, False, True);

  Node:= GetSoapNodePath(XML, '/DetailsByTransportKeyResponse/DetailsByTransportKeyResult');
  Result.Status:= GeniusStrToTransStatus(GetNodeValue(Node, 'Status'));
  Result.ErrorMessage:= GetNodeValue(Node, 'ErrorMessage');
  Result.TotalAmountApproved:= StrToCurrDef(GetNodeValue(Node, 'TotalAmountApproved'), 0);
  Result.RequestedAmount:= StrToCurrDef(GetNodeValue(Node, 'RequestedAmount'), 0);
  Result.ResponseType:= GeniusStrToResponseType(GetNodeValue(Node, 'ResponseType'));

  //TODO: Check for errors first

  Node:= GetSoapNodePath(XML, '/DetailsByTransportKeyResponse/DetailsByTransportKeyResult/PaymentDetails');
  if Assigned(Node) then begin
    for X := 0 to Node.ChildNodes.Count-1 do begin
      if Node.childNodes.Nodes[X].NodeName = 'PaymentDetail' then begin
        Node2:= Node.childNodes.Nodes[X];
        PD:= Res.Add;
        PD.PaymentType:= GeniusStrToPaymentType(GetNodeValue(Node2, 'PaymentType'));
        PD.Status:= GeniusStrToTransStatus(GetNodeValue(Node2, 'Status'));
        PD.ErrorMessage:= GetNodeValue(Node2, 'ErrorMessage');
        PD.TransactionType:= GeniusStrToTransactionType(GetNodeValue(Node2, 'TransactionType'));
        PD.Token:= GetNodeValue(Node2, 'Token');
        PD.AuthorizationCode:= GetNodeValue(Node2, 'AuthorizationCode');
        PD.Customer:= GetNodeValue(Node2, 'Customer');
        PD.Email:= GetNodeValue(Node2, 'Email');
        PD.PhoneNumber:= GetNodeValue(Node2, 'PhoneNumber');
        PD.AccountNumber:= GetNodeValue(Node2, 'AccountNumber');
        PD.ExpirationDate:= GetNodeValue(Node2, 'ExpirationDate');
        PD.EntryMode:= GeniusStrToEntryMode(GetNodeValue(Node2, 'EntryMode'));
        PD.TransactionDate:= StrToDateTimeDef(GetNodeValue(Node2, 'TransactionDate'), 0);

        for Y := 0 to Node2.childNodes.Count-1 do begin
          Node3:= Node2.childNodes.Nodes[Y];
          if Node3.nodeName = 'AmountDetail' then begin
            PD.AmountApproved:= StrToCurrDef(GetNodeValue(Node3, 'AmountApproved'), 0);
            PD.AmountCharged:= StrToCurrDef(GetNodeValue(Node3, 'AmountCharged'), 0);
            PD.TaxAmount:= StrToCurrDef(GetNodeValue(Node3, 'TaxAmount'), 0);
            PD.TipAmount:=  StrToCurrDef(GetNodeValue(Node3, 'TipAmount'), 0);
            PD.UserTipAmount:= StrToCurrDef(GetNodeValue(Node3, 'UserTipAmount'), 0);
            PD.DiscountAmount:= StrToCurrDef(GetNodeValue(Node3, 'DiscountAmount'), 0);
            PD.VoucherAmount:= StrToCurrDef(GetNodeValue(Node3, 'VoucherAmount'), 0);
          end else
          if Node3.nodeName = 'SignatureDetail' then begin
            PD.SignatureType:= TGeniusSignatureType.gsVector; //TODO
            PD.Signature:= GetNodeValue(Node3, 'Signature');
          end else
          if Node3.nodeName = 'GiftDetail' then begin
            PD.GiftBalance:= StrToCurrDef(GetNodeValue(Node3, 'Balance'), 0);
          end else
          if Node3.nodeName = 'LoyaltyDetail' then begin
            PD.LoyaltyVisits:= StrToIntDef(GetNodeValue(Node3, 'Visits'), 0);
            PD.LoyaltyLastVisit:= StrToDateTimeDef(GetNodeValue(Node3, 'LastVisit'), 0);
            PD.LoyaltyLifetimeSpend:= StrToCurrDef(GetNodeValue(Node3, 'LifetimeSpend'), 0);
            PD.LoyaltyBalance:= StrToCurrDef(GetNodeValue(Node3, 'Balance'), 0);
          end else begin
            //Not a recognized node name

          end;
        end;

      end else begin
        //Child node is NOT PaymentDetail - unrecognized

      end;
    end;
  end else begin
    //Node not assigned
    //Result.Status:= TGeniusTransStatus.gsError;
    //Result.ErrorMessage:= 'PaymentDetails node does not exist';
  end;
end;

{$ENDREGION}

{$REGION 'TGeniusStageRequest Object'}

{ TGeniusStageRequest }

constructor TGeniusStageRequest.Create;
begin
  inherited;

end;

destructor TGeniusStageRequest.Destroy;
begin

  inherited;
end;

function TGeniusStageRequest.GetAmount: Currency;
begin
  Result:= FAmount;
end;

function TGeniusStageRequest.GetCardHolder: String;
begin
  Result:= FCardHolder;
end;

function TGeniusStageRequest.GetClerkId: String;
begin
  Result:= FClerkId;
end;

function TGeniusStageRequest.GetCustomerCode: String;
begin
  Result:= FCustomerCode;
end;

function TGeniusStageRequest.GetDba: String;
begin
  Result:= FDba;
end;

function TGeniusStageRequest.GetForceDuplicate: Boolean;
begin
  Result:= FForceDuplicate;
end;

function TGeniusStageRequest.GetOrderNumber: String;
begin
  Result:= FOrderNumber;
end;

function TGeniusStageRequest.GetPoNumber: String;
begin
  Result:= FPoNumber;
end;

function TGeniusStageRequest.GetSoftwareName: String;
begin
  Result:= FSoftwareName;
end;

function TGeniusStageRequest.GetSoftwareVersion: String;
begin
  Result:= FSoftwareVersion;
end;

function TGeniusStageRequest.GetTaxAmount: Currency;
begin
  Result:= FTaxAmount;
end;

function TGeniusStageRequest.GetTerminalId: Integer;
begin
  Result:= FTerminalId;
end;

function TGeniusStageRequest.GetTransactionId: String;
begin
  Result:= FTransactionId;
end;

function TGeniusStageRequest.GetTransactionType: TGeniusTransactionType;
begin
  Result:= FTransactionType;
end;

procedure TGeniusStageRequest.SetAmount(const Value: Currency);
begin
  FAmount:= Value;
end;

procedure TGeniusStageRequest.SetCardHolder(const Value: String);
begin
  FCardHolder:= Value;
end;

procedure TGeniusStageRequest.SetClerkId(const Value: String);
begin
  FClerkId:= Value;
end;

procedure TGeniusStageRequest.SetCustomerCode(const Value: String);
begin
  FCustomerCode:= Value;
end;

procedure TGeniusStageRequest.SetDba(const Value: String);
begin
  FDba:= value;
end;

procedure TGeniusStageRequest.SetForceDuplicate(const Value: Boolean);
begin
  FForceDuplicate:= Value;
end;

procedure TGeniusStageRequest.SetOrderNumber(const Value: String);
begin
  FOrderNumber:= Value;
end;

procedure TGeniusStageRequest.SetPoNumber(const Value: String);
begin
  FPoNumber:= Value;
end;

procedure TGeniusStageRequest.SetSoftwareName(const Value: String);
begin
  FSoftwareName:= Value;
end;

procedure TGeniusStageRequest.SetSoftwareVersion(const Value: String);
begin
  FSoftwareVersion:= Value;
end;

procedure TGeniusStageRequest.SetTaxAmount(const Value: Currency);
begin
  FTaxAmount:= Value;
end;

procedure TGeniusStageRequest.SetTerminalId(const Value: Integer);
begin
  FTerminalId:= Value;
end;

procedure TGeniusStageRequest.SetTransactionId(const Value: String);
begin
  FTransactionId:= Value;
end;

procedure TGeniusStageRequest.SetTransactionType(
  const Value: TGeniusTransactionType);
begin
  FTransactionType:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusStageResponse Object'}

{ TGeniusStageResponse }

constructor TGeniusStageResponse.Create;
begin
  inherited;
  FMessages:= TInterfaceList.Create;
end;

destructor TGeniusStageResponse.Destroy;
begin
  Clear;
  FreeAndNil(FMessages);
  inherited;
end;

function TGeniusStageResponse.Add: IGeniusStageMessage;
begin
  Result:= TGeniusStageMessage.Create;
  FMessages.Add(Result);
  Result._AddRef;
end;

procedure TGeniusStageResponse.Delete(const Index: Integer);
begin
  IGeniusStageMessage(FMessages[Index])._Release;
  FMessages.Delete(Index);
end;

procedure TGeniusStageResponse.Clear;
begin
  while MessageCount > 0 do
    Delete(0);
end;

function TGeniusStageResponse.GetMessage(const Index: Integer): IGeniusStageMessage;
begin
  Result:= IGeniusStageMessage(FMessages[Index]);
end;

function TGeniusStageResponse.GetTransportKey: String;
begin
  Result:= FTransportKey;
end;

function TGeniusStageResponse.GetValidationKey: String;
begin
  Result:= FValidationKey;
end;

function TGeniusStageResponse.MessageCount: Integer;
begin
  Result:= FMessages.Count;
end;

procedure TGeniusStageResponse.SetTransportKey(const Value: String);
begin
  FTransportKey:= Value;
end;

procedure TGeniusStageResponse.SetValidationKey(const Value: String);
begin
  FValidationKey:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusStageMessage Object}

{ TGeniusStageMessage }

constructor TGeniusStageMessage.Create;
begin

end;

destructor TGeniusStageMessage.Destroy;
begin

  inherited;
end;

function TGeniusStageMessage.GetField: String;
begin
  Result:= FField;
end;

function TGeniusStageMessage.GetInformation: String;
begin
  Result:= FInformation;
end;

procedure TGeniusStageMessage.SetField(const Value: String);
begin
  FField:= Value;
end;

procedure TGeniusStageMessage.SetInformation(const Value: String);
begin
  FInformation:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusTransactionResponse Object'}

{ TGeniusTransactionResponse }

constructor TGeniusTransactionResponse.Create;
begin
  FDiscountsApplied:= TInterfaceList.Create;
end;

destructor TGeniusTransactionResponse.Destroy;
begin
  ClearDiscountsApplied;
  FreeAndNil(FDiscountsApplied);
  inherited;
end;

function TGeniusTransactionResponse.AddDiscountApplied: IGeniusTransactionDiscount;
begin
  Result:= TGeniusTransactionDiscount.Create(Self);
  FDiscountsApplied.Add(Result);
  Result._AddRef;
end;

procedure TGeniusTransactionResponse.DeleteDiscountApplied(const Index: Integer);
begin
  IGeniusTransactionDiscount(FDiscountsApplied[Index])._Release;
  FDiscountsApplied.Delete(Index);
end;

procedure TGeniusTransactionResponse.ClearDiscountsApplied;
begin
  while DiscountsAppliedCount > 0 do
    DeleteDiscountApplied(0);
end;

function TGeniusTransactionResponse.GetDiscountsAppliedCount: Integer;
begin
  Result:= FDiscountsApplied.Count;
end;

function TGeniusTransactionResponse.GetDiscountsApplied(
  const Index: Integer): IGeniusTransactionDiscount;
begin
  Result:= IGeniusTransactionDiscount(FDiscountsApplied[Index]);
end;

function TGeniusTransactionResponse.GetAccountNumber: String;
begin
  Result:= FAccountNumber;
end;

function TGeniusTransactionResponse.GetAmountApproved: Currency;
begin
  Result:= FAmountApproved;
end;

function TGeniusTransactionResponse.GetAuthorizationCode: String;
begin
  Result:= FAuthorizationCode;
end;

function TGeniusTransactionResponse.GetCardHolder: String;
begin
  Result:= FCardHolder;
end;

function TGeniusTransactionResponse.GetCashback: Currency;
begin
  Result:= FCashback;
end;

function TGeniusTransactionResponse.GetDiscount: Currency;
begin
  Result:= FDiscount;
end;

function TGeniusTransactionResponse.GetDonation: Currency;
begin
  Result:= FDonation;
end;

function TGeniusTransactionResponse.GetEmvResponse: IGeniusTransactionResponseEmv;
begin
  Result:= FEmvResponse;
end;

function TGeniusTransactionResponse.GetEntryMode: TGeniusEntryMode;
begin
  Result:= FEntryMode;
end;

function TGeniusTransactionResponse.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TGeniusTransactionResponse.GetKeyedAvsResponse: Char;
begin
  Result:= FKeyedAvsResponse;
end;

function TGeniusTransactionResponse.GetKeyedAvsStreetZipCode: String;
begin
  Result:= FKeyedAvsStreetZipCode;
end;

function TGeniusTransactionResponse.GetKeyedCvResponse: Char;
begin
  Result:= FKeyedCvResponse;
end;

function TGeniusTransactionResponse.GetKeyedExpiration: TExpirationDate;
begin
  Result:= FKeyedExpiration;
end;

function TGeniusTransactionResponse.GetPaymentType: TGeniusPaymentType;
begin
  Result:= FPaymentType;
end;

function TGeniusTransactionResponse.GetResponseType: TGeniusResponseType;
begin
  Result:= FResponseType;
end;

function TGeniusTransactionResponse.GetSignatureData: String;
begin
  Result:= FSignatureData;
end;

function TGeniusTransactionResponse.GetStatus: TGeniusTransStatus;
begin
  Result:= FStatus;
end;

function TGeniusTransactionResponse.GetStatusStr: String;
begin
  Result:= FStatusStr;
end;

function TGeniusTransactionResponse.GetSurcharge: Currency;
begin
  Result:= FSurcharge;
end;

function TGeniusTransactionResponse.GetToken: String;
begin
  Result:= FToken;
end;

function TGeniusTransactionResponse.GetTransactionDate: TDateTime;
begin
  Result:= FTransactionDate;
end;

function TGeniusTransactionResponse.GetTransactionType: TGeniusTransactionType;
begin
  Result:= FTransactionType;
end;

function TGeniusTransactionResponse.GetTransportKey: String;
begin
  Result:= FTransportKey;
end;

function TGeniusTransactionResponse.GetUserTip: Currency;
begin
  Result:= FUserTip;
end;

function TGeniusTransactionResponse.GetValidationKey: String;
begin
  Result:= FValidationKey;
end;

procedure TGeniusTransactionResponse.SetAccountNumber(const Value: String);
begin
  FAccountNumber:= Value;
end;

procedure TGeniusTransactionResponse.SetAmountApproved(const Value: Currency);
begin
  FAmountApproved:= Value;
end;

procedure TGeniusTransactionResponse.SetAuthorizationCode(
  const Value: String);
begin
  FAuthorizationCode:= Value;
end;

procedure TGeniusTransactionResponse.SetCardHolder(const Value: String);
begin
  FCardHolder:= Value;
end;

procedure TGeniusTransactionResponse.SetCashback(const Value: Currency);
begin
  FCashback:= Value;
end;

procedure TGeniusTransactionResponse.SetDiscount(const Value: Currency);
begin
  FDiscount:= Value;
end;

procedure TGeniusTransactionResponse.SetDonation(const Value: Currency);
begin
  FDonation:= Value;
end;

procedure TGeniusTransactionResponse.SetEmvResponse(
  const Value: IGeniusTransactionResponseEmv);
begin
  FEmvResponse:= Value;
end;

procedure TGeniusTransactionResponse.SetEntryMode(
  const Value: TGeniusEntryMode);
begin
  FEntryMode:= Value;
end;

procedure TGeniusTransactionResponse.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TGeniusTransactionResponse.SetKeyedAvsResponse(const Value: Char);
begin
  FKeyedAvsResponse:= Value;
end;

procedure TGeniusTransactionResponse.SetKeyedAvsStreetZipCode(const Value: String);
begin
  FKeyedAvsStreetZipCode:= Value;
end;

procedure TGeniusTransactionResponse.SetKeyedCvResponse(const Value: Char);
begin
  FKeyedCvResponse:= Value;
end;

procedure TGeniusTransactionResponse.SetKeyedExpiration(const Value: TExpirationDate);
begin
  FKeyedExpiration:= Value;
end;

procedure TGeniusTransactionResponse.SetPaymentType(const Value: TGeniusPaymentType);
begin
  FPaymentType:= Value;
end;

procedure TGeniusTransactionResponse.SetResponseType(const Value: TGeniusResponseType);
begin
  FResponseType:= Value;
end;

procedure TGeniusTransactionResponse.SetSignatureData(const Value: String);
begin
  FSignatureData:= Value;
end;

procedure TGeniusTransactionResponse.SetStatus(const Value: TGeniusTransStatus);
begin
  FStatus:= Value;
end;

procedure TGeniusTransactionResponse.SetStatusStr(const Value: String);
begin
  FStatusStr:= Value;
end;

procedure TGeniusTransactionResponse.SetSurcharge(const Value: Currency);
begin
  FSurcharge:= Value;
end;

procedure TGeniusTransactionResponse.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TGeniusTransactionResponse.SetTransactionDate(const Value: TDateTime);
begin
  FTransactionDate:= Value;
end;

procedure TGeniusTransactionResponse.SetTransactionType(
  const Value: TGeniusTransactionType);
begin
  FTransactionType:= Value;
end;

procedure TGeniusTransactionResponse.SetTransportKey(const Value: String);
begin
  FTransportKey:= Value;
end;

procedure TGeniusTransactionResponse.SetUserTip(const Value: Currency);
begin
  FUserTip:= Value;
end;

procedure TGeniusTransactionResponse.SetValidationKey(const Value: String);
begin
  FValidationKey:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusTransactionResponseEmv'}

{ TGeniusTransactionResponseEmv }

constructor TGeniusTransactionResponseEmv.Create;
begin

end;

destructor TGeniusTransactionResponseEmv.Destroy;
begin
  inherited;
end;

function TGeniusTransactionResponseEmv.GetAid: String;
begin
  Result:= FAid;
end;

function TGeniusTransactionResponseEmv.GetAmountAuthorized: Currency;
begin
  Result:= FAmountAuthorized;
end;

function TGeniusTransactionResponseEmv.GetAmountOther: Currency;
begin
  Result:= FAmountOther;
end;

function TGeniusTransactionResponseEmv.GetApplicationEffectiveDate: TDateTime;
begin
  Result:= FApplicationEffectiveDate;
end;

function TGeniusTransactionResponseEmv.GetApplicationExpiryDate: TDateTime;
begin
  Result:= FApplicationExpiryDate;
end;

function TGeniusTransactionResponseEmv.GetApplicationInterchangeProfile: String;
begin
  Result:= FApplicationInterchangeProfile;
end;

function TGeniusTransactionResponseEmv.GetApplicationLabel: String;
begin
  Result:= FApplicationLabel;
end;

function TGeniusTransactionResponseEmv.GetApplicationTransactionCounter: Integer;
begin
  Result:= FApplicationTransactionCounter;
end;

function TGeniusTransactionResponseEmv.GetApplicationVersionNumber: Integer;
begin
  Result:= FApplicationVersionNumber;
end;

function TGeniusTransactionResponseEmv.GetCardExpiryDate: TExpirationDate;
begin
  Result:= FCardExpiryDate;
end;

function TGeniusTransactionResponseEmv.GetCryptogram: String;
begin
  Result:= FCryptogram;
end;

function TGeniusTransactionResponseEmv.GetCryptogramInformationData: String;
begin
  Result:= FCryptogramInformationData;
end;

function TGeniusTransactionResponseEmv.GetCryptogramType: TGeniusCryptogramType;
begin
  Result:= FCryptogramType;
end;

function TGeniusTransactionResponseEmv.GetCvmMethod: TGeniusCvmMethod;
begin
  Result:= FCvmMethod;
end;

function TGeniusTransactionResponseEmv.GetCvmResults: String;
begin
  Result:= FCvmResults;
end;

function TGeniusTransactionResponseEmv.GetIfdSerialNumber: String;
begin
  Result:= FIfdSerialNumber;
end;

function TGeniusTransactionResponseEmv.GetIssuerApplicationData: String;
begin
  Result:= FIssuerApplicationData;
end;

function TGeniusTransactionResponseEmv.GetMaskedPan: String;
begin
  Result:= FMaskedPan;
end;

function TGeniusTransactionResponseEmv.GetPanSequenceNumber: Integer;
begin
  Result:= FPanSequenceNumber;
end;

function TGeniusTransactionResponseEmv.GetPINStatement: String;
begin
  Result:= FPINStatement;
end;

function TGeniusTransactionResponseEmv.GetPosEntryMode: TGeniusEntryMode;
begin
  Result:= FPosEntryMode;
end;

function TGeniusTransactionResponseEmv.GetTerminalCountryCode: String;
begin
  Result:= FTerminalCountryCode;
end;

function TGeniusTransactionResponseEmv.GetTerminalType: Integer;
begin
  Result:= FTerminalType;
end;

function TGeniusTransactionResponseEmv.GetTerminalVerificationResults: String;
begin
  Result:= FTerminalVerificationResults;
end;

function TGeniusTransactionResponseEmv.GetTransactionCurrencyCode: String;
begin
  Result:= FTransactionCurrencyCode;
end;

function TGeniusTransactionResponseEmv.GetTransactionType: TGeniusEmvTransactionType;
begin
  Result:= FTransactionType;
end;

function TGeniusTransactionResponseEmv.GetUnpredictableNumber: Integer;
begin
  Result:= FUnpredictableNumber;
end;

procedure TGeniusTransactionResponseEmv.SetAid(const Value: String);
begin
  FAid:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetAmountAuthorized(const Value: Currency);
begin
  FAmountAuthorized:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetAmountOther(const Value: Currency);
begin
  FAmountOther:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetApplicationEffectiveDate(const Value: TDateTime);
begin
  FApplicationEffectiveDate:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetApplicationExpiryDate(const Value: TDateTime);
begin
  FApplicationExpiryDate:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetApplicationInterchangeProfile(const Value: String);
begin
  FApplicationInterchangeProfile:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetApplicationLabel(const Value: String);
begin
  FApplicationLabel:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetApplicationTransactionCounter(const Value: Integer);
begin
  FApplicationTransactionCounter:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetApplicationVersionNumber(const Value: Integer);
begin
  FApplicationVersionNumber:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetCardExpiryDate(const Value: TExpirationDate);
begin
  FCardExpiryDate:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetCryptogram(const Value: String);
begin
  FCryptogram:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetCryptogramInformationData(const Value: String);
begin
  FCryptogramInformationData:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetCryptogramType(const Value: TGeniusCryptogramType);
begin
  FCryptogramType:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetCvmMethod(const Value: TGeniusCvmMethod);
begin
  FCvmMethod:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetCvmResults(const Value: String);
begin
  FCvmResults:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetIfdSerialNumber(const Value: String);
begin
  FIfdSerialNumber:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetIssuerApplicationData(const Value: String);
begin
  FIssuerApplicationData:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetMaskedPan(const Value: String);
begin
  FMaskedPan:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetPanSequenceNumber(const Value: Integer);
begin
  FPanSequenceNumber:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetPINStatement(const Value: String);
begin
  FPINStatement:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetPosEntryMode(const Value: TGeniusEntryMode);
begin
  FPosEntryMode:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetTerminalCountryCode(const Value: String);
begin
  FTerminalCountryCode:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetTerminalType(const Value: Integer);
begin
  FTerminalType:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetTerminalVerificationResults(const Value: String);
begin
  FTerminalVerificationResults:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetTransactionCurrencyCode(const Value: String);
begin
  FTransactionCurrencyCode:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetTransactionType(const Value: TGeniusEmvTransactionType);
begin
  FTransactionType:= Value;
end;

procedure TGeniusTransactionResponseEmv.SetUnpredictableNumber(const Value: Integer);
begin
  FUnpredictableNumber:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusTransactionDiscount Object'}

{ TGeniusTransactionDiscount }

constructor TGeniusTransactionDiscount.Create(
  AOwner: TGeniusTransactionResponse);
begin
  FOwner:= AOwner;

end;

destructor TGeniusTransactionDiscount.Destroy;
begin

  inherited;
end;

function TGeniusTransactionDiscount.GetAmount: Currency;
begin
  Result:= FAmount;
end;

function TGeniusTransactionDiscount.GetDiscountType: String;
begin
  Result:= FDiscountType;
end;

function TGeniusTransactionDiscount.GetMessage: String;
begin
  Result:= FMessage;
end;

function TGeniusTransactionDiscount.Owner: IGeniusTransactionResponse;
begin
  Result:= FOwner;
end;

procedure TGeniusTransactionDiscount.SetAmount(const Value: Currency);
begin
  FAmount:= Value;
end;

procedure TGeniusTransactionDiscount.SetDiscountType(const Value: String);
begin
  FDiscountType:= Value;
end;

procedure TGeniusTransactionDiscount.SetMessage(const Value: String);
begin
  FMessage:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusSignatureResponse Object'}

{ TGeniusSignatureResponse }

constructor TGeniusSignatureResponse.Create;
begin

end;

destructor TGeniusSignatureResponse.Destroy;
begin

  inherited;
end;

function TGeniusSignatureResponse.GetData: String;
begin
  Result:= FData;
end;

function TGeniusSignatureResponse.GetRequestID: String;
begin
  Result:= FRequestID;
end;

function TGeniusSignatureResponse.GetStatus: TGeniusSignatureStatus;
begin
  Result:= FStatus;
end;

procedure TGeniusSignatureResponse.SetData(const Value: String);
begin
  FData:= Value;
end;

procedure TGeniusSignatureResponse.SetRequestID(const Value: String);
begin
  FRequestID:= Value;
end;

procedure TGeniusSignatureResponse.SetStatus(const Value: TGeniusSignatureStatus);
begin
  FStatus:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusLineItems Object'}

{ TGeniusLineItems }

constructor TGeniusLineItems.Create(AOwner: TGenius);
begin
  FOwner:= AOwner;
  FItems:= TInterfaceList.Create;
  FInProgress:= False;
end;

destructor TGeniusLineItems.Destroy;
begin
  if FInProgress then
    FOwner.CancelTransaction;
  ClearItems;
  FreeAndNil(FItems);
  inherited;
end;

function TGeniusLineItems.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TGeniusLineItems.ClearItems;
var
  X: Integer;
begin
  for X := FItems.Count-1 downto 0 do begin
    DeleteItem(X);
  end;
end;

function TGeniusLineItems.GetAutoTotal: Boolean;
begin
  Result:= FAutoTotal;
end;

function TGeniusLineItems.GetDisplayCustomSubtotal: String;
begin
  Result:= FDisplayCustomSubtotal;
end;

function TGeniusLineItems.GetItem(const Index: Integer): IGeniusLineItem;
begin
  Result:= IGeniusLineItem(FItems[Index]);
end;

function TGeniusLineItems.GetOrder: String;
begin
  Result:= FOrder;
end;

function TGeniusLineItems.InProgress: Boolean;
begin
  Result:= FInProgress;
end;

function TGeniusLineItems.OrderTax: Currency;
var
  X: Integer;
  I: IGeniusLineItem;
begin
  if FAutoTotal then begin
    Result:= 0;
    for X := 0 to Count-1 do begin
      I:= Items[X];
      Result:= Result + (I.TaxAmount * I.Quantity);
    end;
  end else begin
    Result:= FOrderTax;
  end;
end;

function TGeniusLineItems.OrderTotal: Currency;
var
  X: Integer;
  I: IGeniusLineItem;
begin
  if FAutoTotal then begin
    Result:= 0;
    for X := 0 to Count-1 do begin
      I:= Items[X];
      Result:= Result + (I.Amount * I.Quantity);
    end;
  end else begin
    Result:= FOrderTotal;
  end;
end;

procedure TGeniusLineItems.SetAutoTotal(const Value: Boolean);
begin
  FAutoTotal:= Value;
end;

procedure TGeniusLineItems.SetDisplayCustomSubtotal(const Value: String);
begin
  FDisplayCustomSubtotal:= Value;
end;

procedure TGeniusLineItems.SetOrder(const Value: String);
begin
  FOrder:= Value;
end;

function TGeniusLineItems.StartOrder(const Order: String): IGeniusStartOrderResponse;
var
  Url: String;
  Pars: TParamList;
  Xml: IXMLDocument;
  Node: IXmlNode;
begin
  Result:= TGeniusStartOrderResponse.Create;
  FInProgress:= True;
  FOrder:= Order;
  Pars:= TParamList.Create;
  FOrderTotal:= 0;
  FOrderTax:= 0;
  try
    Pars['Action']:= 'StartOrder';
    Pars['Order']:= Order;
    Pars['Format']:= 'XML';
    Url:= FOwner.DeviceUrl(Pars.ParamStr);
    try
      XML:= FOwner.SendDeviceRequestXML(Url);
      Node:= GetNodePath(XML, '/OrderResult');
      if Assigned(Node) then begin
        Result.Status:= GeniusStrToLineItemStatus(GetNodeValue(Node, 'Status'));
        Result.ResponseMessage:= GetNodeValue(Node, 'ResponseMessage');
      end else begin
        Result.Status:= TGeniusLineItemStatus.soError;
        Result.ResponseMessage:= 'Error: Node empty';
      end;
    except
      on E: Exception do begin
        Result.Status:= soError;
        Result.ResponseMessage:= 'Error: ' + E.Message;
      end;
    end;
  finally
    FreeAndNil(Pars);
  end;
end;

function TGeniusLineItems.EndOrder(const ExternalPaymentType: TGeniusExternalPaymentType): IGeniusStartOrderResponse;
var
  Url: String;
  Pars: TParamList;
  Xml: IXMLDocument;
  Node: IXmlNode;
begin
  Result:= TGeniusStartOrderResponse.Create;
  FInProgress:= False;
  Pars:= TParamList.Create;
  try
    Pars['Action']:= 'EndOrder';
    Pars['Order']:= FOrder;
    Pars['ExternalPaymentType']:= GeniusExternalPaymentTypeToStr(ExternalPaymentType);
    Pars['Format']:= 'XML';
    Url:= FOwner.DeviceUrl(Pars.ParamStr);
    try
      XML:= FOwner.SendDeviceRequestXML(Url);
      Node:= GetNodePath(XML, '/GetSignatureResult');
      if Assigned(Node) then begin
        Result.Status:= GeniusStrToLineItemStatus(GetNodeValue(Node, 'Status'));
        Result.ResponseMessage:= GetNodeValue(Node, 'ResponseMessage');
        //Result.AdditionalParameter ... Empty
      end else begin
        Result.Status:= TGeniusLineItemStatus.soError;
        Result.ResponseMessage:= 'Error: Node empty';
      end;
    except
      on E: Exception do begin
        Result.Status:= soError;
        Result.ResponseMessage:= 'Error: ' + E.Message;
      end;
    end;
  finally
    FreeAndNil(Pars);
  end;
end;

function TGeniusLineItems.AddItem(const ItemTypeValue, Upc, Description: String;
  const Amount, TaxAmount: Currency;
  const Quantity: Integer = 1;
  const ItemType: TGeniusLineItemType = glSku;
  const Category: TGeniusLineItemCategory = glNone;
  const DisplayOverride: String = ''): IGeniusLineItem;
var
  Url: String;
  Pars: TParamList;
  XML: IXMLDocument;
  Node: IXmlNode;
begin
  //http://[CED-IP-Address]:8080/v1/pos?Action=AddItem&Order=1000&Type=Sku&TypeValue=xxx&UPC=xxx&
  //Quantity=1&Description=Something&Amount=1.00&TaxAmount=0.06&OrderTotal=1.00&OrderTax=0.06&Category=None&Format=XML

  Result:= TGeniusLineItem.Create(Self);
  Result.ItemID:= '';
  Result.ItemTypeValue:= ItemTypeValue;
  Result.UPC:= Upc;
  Result.Description:= Description;
  Result.Amount:= Amount;
  Result.TaxAmount:= TaxAmount;
  Result.Quantity:= Quantity;
  Result.ItemType:= ItemType;
  Result.Category:= Category;
  Result.DisplayOverride:= DisplayOverride;

  Pars:= TParamList.Create;
  try
    try
      Pars['Action']:= 'AddItem';
      Pars['Format']:= 'XML';
      Pars['Order']:= Order;
      Pars['Type']:= GeniusLineItemTypeToStr(ItemType);
      Pars['TypeValue']:= ItemTypeValue;
      if UPC <> '' then
        Pars['UPC']:= UPC;
      Pars['Quantity']:= IntToStr(Quantity);
      Pars['Description']:= TrimStr(Description, LID_MAX_DESCR);
      Pars['Amount']:= CurrToStr(Amount);
      Pars['TaxAmount']:= CurrToStr(TaxAmount);
      Pars['Category']:= GeniusLineItemCategoryToStr(Category);
      if DisplayOverride <> '' then
        Pars['DisplayOverride']:= DisplayOverride;

      if FDisplayCustomSubTotal <> '' then
        Pars['DisplayCustomSubTotal']:= FDisplayCustomSubTotal;
      Pars['OrderTotal']:= CurrToStr(OrderTotal + ((Amount + TaxAmount) * Quantity));
      Pars['OrderTax']:= CurrToStr(OrderTax + (TaxAmount * Quantity));

      Url:= FOwner.DeviceUrl(Pars.ParamStr);
      XML:= FOwner.SendDeviceRequestXML(Url);
      Node:= GetNodePath(XML, '/OrderResult');
      if Assigned(Node) then begin
        Result.Status:= GeniusStrToLineItemStatus(GetNodeValue(Node, 'Status'));
        Result.ResponseMessage:= GetNodeValue(Node, 'ResponseMessage');
        case Result.Status of
          soSuccess: begin
            FOrderTotal:= OrderTotal + ((Amount + TaxAmount) * Quantity);
            FOrderTax:= OrderTax + (TaxAmount * Quantity);
            Result.ItemID:= GetNodeValue(Node, 'ItemID');
            FItems.Add(Result);
            Result._AddRef;
          end;
          soDenied: begin
            //raise Exception.Create('Adding line item denied: ' + Result.ResponseMessage);
          end;
          soError: begin
            //raise Exception.Create('Adding line item failed: ' + Result.ResponseMessage);
          end;
        end;
      end else begin
        Result.Status:= TGeniusLineItemStatus.soError;
        Result.ResponseMessage:= 'Error in AddItem: OrderResult Node empty';
      end;
    except
      on E: Exception do begin
        Result.Status:= soError;
        Result.ResponseMessage:= 'Exception in AddItem: ' + E.Message;
      end;
    end;
  finally
    FreeAndNil(Pars);
  end;
end;

function TGeniusLineItems.UpdateItem(const TargetItemId, ItemTypeValue, Upc,
  Description: String; const Amount, TaxAmount: Currency;
  const Quantity: Integer; const ItemType: TGeniusLineItemType;
  const Category: TGeniusLineItemCategory; const DisplayOverride: String): IGeniusLineItem;
var
  Url: String;
  Pars: TParamList;
  XML: IXMLDocument;
  Node: IXmlNode;
  X: Integer;
begin
  //http://[CED-IP-Address]:8080/v1/pos?Action=UpdateItem&Order=xxx&
  //TargetItemID=xxx&OrderTotal=xxx&OrderTax=xxx&Format=xxx

  for X := 0 to FOwner.FLineItems.Count-1 do begin
    if IGeniusLineItem(FItems[X]).ItemID = TargetItemID then begin
      Result:= IGeniusLineItem(FItems[X]);
      Break;
    end;
  end;
  if Assigned(Result) then begin
    //Remove old item amounts from totals
    FOrderTotal:= FOrderTotal - ((Result.Amount + Result.TaxAmount) * Result.Quantity);
    FOrderTax:= FOrderTax - (Result.TaxAmount * Result.Quantity);

    Result.ItemTypeValue:= ItemTypeValue;
    Result.UPC:= Upc;
    Result.Description:= Description;
    Result.Amount:= Amount;
    Result.TaxAmount:= TaxAmount;
    Result.Quantity:= Quantity;
    Result.ItemType:= ItemType;
    Result.Category:= Category;
    Result.DisplayOverride:= DisplayOverride;
    Pars:= TParamList.Create;
    try
      Pars['Action']:= 'UpdateItem';
      Pars['Order']:= Order;
      Pars['TargetItemID']:= TargetItemId;
      Pars['Type']:= GeniusLineItemTypeToStr(ItemType);
      Pars['TypeValue']:= ItemTypeValue;
      if UPC <> '' then
        Pars['UPC']:= UPC;
      Pars['Quantity']:= IntToStr(Quantity);
      Pars['Description']:= TrimStr(Description, LID_MAX_DESCR);
      Pars['Amount']:= CurrToStr(Amount);  // CurrToStr(Amount + TaxAmount); //Hide Tax Per Line
      Pars['TaxAmount']:= CurrToStr(TaxAmount);

      //Add new item amounts to totals
      FOrderTotal:= FOrderTotal + ((Amount + TaxAmount) * Quantity);
      FOrderTax:= FOrderTax + (TaxAmount * Quantity);

      Pars['OrderTotal']:= CurrToStr(FOrderTotal);
      Pars['OrderTax']:= CurrToStr(FOrderTax);
      Pars['Category']:= GeniusLineItemCategoryToStr(Category);
      if DisplayOverride <> '' then
        Pars['DisplayOverride']:= DisplayOverride;
      if FDisplayCustomSubTotal <> '' then
        Pars['DisplayCustomSubTotal']:= FDisplayCustomSubTotal;
      Pars['Format']:= 'XML';

      Url:= FOwner.DeviceUrl(Pars.ParamStr);
      try
        XML:= FOwner.SendDeviceRequestXML(Url);
        Node:= GetNodePath(XML, '/OrderResult');
        if Assigned(Node) then begin
          Result.Status:= GeniusStrToLineItemStatus(GetNodeValue(Node, 'Status'));
          Result.ResponseMessage:= GetNodeValue(Node, 'ResponseMessage');
          if Result.Status = TGeniusLineItemStatus.soSuccess then begin
            FItems.Add(Result);
            Result._AddRef;
          end;
        end else begin
          Result.Status:= TGeniusLineItemStatus.soError;
          Result.ResponseMessage:= 'Error: Node empty';
        end;
      except
        on E: Exception do begin
          Result.Status:= soError;
          Result.ResponseMessage:= 'Error: ' + E.Message;
        end;
      end;
    finally
      FreeAndNil(Pars);
    end;
  end else begin
    raise Exception.Create('Item ID "'+TargetItemID+'" not found');
  end;
end;

function TGeniusLineItems.UpdateTotal(const OrderTotal, OrderTax: Currency): Boolean;
var
  Url: String;
  Pars: TParamList;
  XML: IXMLDocument;
  Node: IXmlNode;
begin
  //http://[CED-IP-Address]:8080/v1/pos?Action=UpdateTotal&Order=xxx&
  //  OrderTotal=xxx&OrderTax=xxx&DisplayCustomSubTotal=xxx&Format=xxx

  Result:= False;
  FInProgress:= False;
  Pars:= TParamList.Create;
  try
    Pars['Action']:= 'UpdateTotal';
    Pars['Order']:= FOrder;
    Pars['OrderTotal']:= CurrToStr(OrderTotal);
    Pars['OrderTax']:= CurrToStr(OrderTax);
    Pars['Format']:= 'XML';
    Url:= FOwner.DeviceUrl(Pars.ParamStr);
    try
      XML:= FOwner.SendDeviceRequestXML(Url);
      Node:= GetNodePath(XML, '/OrderResult');
      if Assigned(Node) then begin
        Result:= GeniusStrToLineItemStatus(GetNodeValue(Node, 'Status')) = TGeniusLineItemStatus.soSuccess;
        if not Result then begin
          raise Exception.Create(GetNodeValue(Node, 'ResponseMessage'));
        end;
      end else begin
        Result:= False;
      end;
    except
      on E: Exception do begin
        Result:= False;
      end;
    end;
  finally
    FreeAndNil(Pars);
  end;
end;

function TGeniusLineItems.DiscountItem(const TargetItem: IGeniusLineItem;
  const ItemTypeValue, Upc, Description: String; const Amount,
  TaxAmount: Currency; const Quantity: Integer;
  const ItemType: TGeniusLineItemType; const Category: TGeniusLineItemCategory;
  const DisplayOverride: String): IGeniusLineItemDiscount;
var
  Url: String;
  Pars: TParamList;
  XML: IXMLDocument;
  Node: IXmlNode;
begin
  //http://[CED-IP-Address]:8080/v1/pos?Action=DiscountItem&Order=xxx&Type=Sku&
  //TypeValue=xxx&UPC=xxx&Quantity=xxx&Description=xxx&Amount=xxx&TaxAmount=xxx&OrderTotal=xxx&OrderTax=xxx&Category=xxx&Format=xxx

  if Assigned(TargetItem) then begin
    Result.ItemTypeValue:= ItemTypeValue;
    Result.UPC:= Upc;
    Result.Description:= Description;
    Result.Amount:= Amount;
    Result.TaxAmount:= TaxAmount;
    Result.Quantity:= Quantity;
    Result.ItemType:= ItemType;
    Result.Category:= Category;
    Result.DisplayOverride:= DisplayOverride;
    Pars:= TParamList.Create;
    try
      Pars['Action']:= 'DiscountItem';
      Pars['Order']:= Order;
      Pars['TargetItemID']:= TargetItem.ItemID;
      Pars['Type']:= GeniusLineItemTypeToStr(ItemType);
      Pars['TypeValue']:= ItemTypeValue;
      if UPC <> '' then
        Pars['UPC']:= UPC;
      Pars['Quantity']:= IntToStr(Quantity);
      Pars['Description']:= TrimStr(Description, LID_MAX_DESCR);
      Pars['Amount']:= CurrToStr(Amount);
      Pars['TaxAmount']:= CurrToStr(TaxAmount);

      //Subtract discount amount from total
      FOrderTotal:= FOrderTotal - ((Amount + TaxAmount) * Quantity);
      FOrderTax:= FOrderTax - (TaxAmount * Quantity);

      Pars['OrderTotal']:= CurrToStr(FOrderTotal);
      Pars['OrderTax']:= CurrToStr(FOrderTax);
      Pars['Category']:= GeniusLineItemCategoryToStr(Category);
      if DisplayOverride <> '' then
        Pars['DisplayOverride']:= DisplayOverride;
      if FDisplayCustomSubTotal <> '' then
        Pars['DisplayCustomSubTotal']:= FDisplayCustomSubTotal;
      Pars['Format']:= 'XML';

      Url:= FOwner.DeviceUrl(Pars.ParamStr);
      try
        XML:= FOwner.SendDeviceRequestXML(Url);
        Node:= GetNodePath(XML, '/OrderResult');
        if Assigned(Node) then begin
          Result.Status:= GeniusStrToLineItemStatus(GetNodeValue(Node, 'Status'));
          Result.ResponseMessage:= GetNodeValue(Node, 'ResponseMessage');
          if Result.Status = TGeniusLineItemStatus.soSuccess then begin
            FItems.Add(Result);
            Result._AddRef;
          end;
        end else begin
          Result.Status:= TGeniusLineItemStatus.soError;
          Result.ResponseMessage:= 'Error: Node empty';
        end;
      except
        on E: Exception do begin
          Result.Status:= soError;
          Result.ResponseMessage:= 'Error: ' + E.Message;
        end;
      end;
    finally
      FreeAndNil(Pars);
    end;
  end else begin
    raise Exception.Create('Target item not assigned for discount');
  end;
end;

procedure TGeniusLineItems.DeleteItem(const ItemID: String);
var
  X: Integer;
  LI: IGeniusLineItem;
begin
  for X := 0 to FItems.Count-1 do begin
    LI:= IGeniusLineItem(FItems[X]);
    if LI.ItemID = ItemID then begin
      DeleteItem(X);
      Break;
    end;
  end;
end;

procedure TGeniusLineItems.DeleteItem(const Index: Integer);
var
  I: IGeniusLineItem;
  Url: String;
  Pars: TParamList;
  Res: String;
begin
  Pars:= TParamList.Create;
  try
    I:= IGeniusLineItem(Self.FItems[Index]);
    FOrderTotal:= FOrderTotal - ((I.Amount + I.TaxAmount) * I.Quantity);
    FOrderTax:= FOrderTax - (I.TaxAmount * I.Quantity);

    Pars['Action']:= 'DeleteItem';
    Pars['Order']:= FOrder;
    Pars['TargetItemID']:= I.ItemID;
    Pars['OrderTotal']:= CurrToStr(FOrderTotal);
    Pars['OrderTax']:= CurrToStr(FOrderTax);
    if FDisplayCustomSubTotal <> '' then
      Pars['DisplayCustomSubTotal']:= FDisplayCustomSubTotal;
    Pars['Format']:= 'XML';

    Url:= FOwner.DeviceUrl(Pars.ParamStr);
    try
      Res:= FOwner.SendDeviceRequest(Url);

      //TODO: Read Response

    except
      on E: Exception do begin

      end;
    end;
    I._Release;
    FItems.Delete(Index);
  finally
    FreeAndNil(Pars);
  end;
end;

procedure TGeniusLineItems.DeleteDiscount(const ADiscount: IGeniusLineItemDiscount);
var
  Url: String;
  Pars: TParamList;
  Res: String;
begin
  Pars:= TParamList.Create;
  try
    FOrderTotal:= FOrderTotal + ((ADiscount.Amount + ADiscount.TaxAmount) * ADiscount.Quantity);
    FOrderTax:= FOrderTax + (ADiscount.TaxAmount * ADiscount.Quantity);

    Pars['Action']:= 'DeleteItem';
    Pars['Order']:= FOrder;
    Pars['TargetItemID']:= ADiscount.ItemID;
    Pars['OrderTotal']:= CurrToStr(FOrderTotal);
    Pars['OrderTax']:= CurrToStr(FOrderTax);
    if FDisplayCustomSubTotal <> '' then
      Pars['DisplayCustomSubTotal']:= FDisplayCustomSubTotal;
    Pars['Format']:= 'XML';

    Url:= FOwner.DeviceUrl(Pars.ParamStr);
    try
      Res:= FOwner.SendDeviceRequest(Url);

      //TODO: Read Response

    except
      on E: Exception do begin

      end;
    end;
  finally
    FreeAndNil(Pars);
  end;
end;

{$ENDREGION}

{$REGION 'TGeniusLineItem Object'}

{ TGeniusLineItem }

constructor TGeniusLineItem.Create(AOwner: TGeniusLineItems);
begin
  FOwner:= AOwner;
  FDiscounts:= TInterfaceList.Create;
end;

destructor TGeniusLineItem.Destroy;
begin
  ClearDiscounts;
  FreeAndNil(FDiscounts);
  inherited;
end;

function TGeniusLineItem.AddDiscount: IGeniusLineItemDiscount;
begin
  Result:= TGeniusLineItemDiscount.Create(Self);
  Result._AddRef;
end;

procedure TGeniusLineItem.DeleteDiscount(const Index: Integer);
var
  D: IGeniusLineItemDiscount;
begin
  D:= IGeniusLineItemDiscount(FDiscounts[Index]);
  FOwner.DeleteDiscount(D);
  D._Release;
  FDiscounts.Delete(Index);
end;

procedure TGeniusLineItem.ClearDiscounts;
begin
  while DiscountCount > 0 do
    DeleteDiscount(0);
end;

function TGeniusLineItem.DiscountCount: Integer;
begin
  Result:= FDiscounts.Count;
end;

function TGeniusLineItem.Owner: IGeniusLineItems;
begin
  Result:= FOwner;
end;

function TGeniusLineItem.GetAmount: Currency;
begin
  Result:= FAmount;
end;

function TGeniusLineItem.GetCategory: TGeniusLineItemCategory;
begin
  Result:= FCategory;
end;

function TGeniusLineItem.GetDescription: String;
begin
  Result:= FDescription;
end;

function TGeniusLineItem.GetDiscount(
  const Index: Integer): IGeniusLineItemDiscount;
begin
  Result:= IGeniusLineItemDiscount(FDiscounts[Index]);
end;

function TGeniusLineItem.GetDisplayOverride: String;
begin
  Result:= FDisplayOverride;
end;

function TGeniusLineItem.GetItemID: String;
begin
  Result:= FItemID;
end;

function TGeniusLineItem.GetItemType: TGeniusLineItemType;
begin
  Result:= FItemType;
end;

function TGeniusLineItem.GetItemTypeValue: String;
begin
  Result:= FItemTypeValue;
end;

function TGeniusLineItem.GetOrder: String;
begin
  Result:= FOrder;
end;

function TGeniusLineItem.GetQuantity: Integer;
begin
  Result:= FQuantity;
end;

function TGeniusLineItem.GetResponseMessage: String;
begin
  Result:= FResponseMessage;
end;

function TGeniusLineItem.GetStatus: TGeniusLineItemStatus;
begin
  Result:= FStatus;
end;

function TGeniusLineItem.GetTaxAmount: Currency;
begin
  Result:= FTaxAmount;
end;

function TGeniusLineItem.GetUPC: String;
begin
  Result:= FUPC;
end;

procedure TGeniusLineItem.SetAmount(const Value: Currency);
begin
  FAmount:= Value;
end;

procedure TGeniusLineItem.SetCategory(const Value: TGeniusLineItemCategory);
begin
  FCategory:= Value;
end;

procedure TGeniusLineItem.SetDescription(const Value: String);
begin
  FDescription:= Value;
end;

procedure TGeniusLineItem.SetDisplayOverride(const Value: String);
begin
  FDisplayOverride:= Value;
end;

procedure TGeniusLineItem.SetItemID(const Value: String);
begin
  FItemID:= Value;
end;

procedure TGeniusLineItem.SetItemType(const Value: TGeniusLineItemType);
begin
  FItemType:= Value;
end;

procedure TGeniusLineItem.SetItemTypeValue(const Value: String);
begin
  FItemTypeValue:= Value;
end;

procedure TGeniusLineItem.SetOrder(const Value: String);
begin
  FOrder:= Value;
end;

procedure TGeniusLineItem.SetQuantity(const Value: Integer);
begin
  FQuantity:= Value;
end;

procedure TGeniusLineItem.SetResponseMessage(const Value: String);
begin
  FResponseMessage:= Value;
end;

procedure TGeniusLineItem.SetStatus(const Value: TGeniusLineItemStatus);
begin
  FStatus:= Value;
end;

procedure TGeniusLineItem.SetTaxAmount(const Value: Currency);
begin
  FTaxAmount:= Value;
end;

procedure TGeniusLineItem.SetUPC(const Value: String);
begin
  FUPC:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusLineItemDiscount Object'}

{ TGeniusLineItemDiscount }

constructor TGeniusLineItemDiscount.Create(AOwner: TGeniusLineItem);
begin
  FOwner:= AOwner;
end;

destructor TGeniusLineItemDiscount.Destroy;
begin

  inherited;
end;

function TGeniusLineItemDiscount.Owner: IGeniusLineItem;
begin
  Result:= IGeniusLineItem(FOwner);
end;

function TGeniusLineItemDiscount.GetAmount: Currency;
begin
  Result:= FAmount;
end;

function TGeniusLineItemDiscount.GetCategory: TGeniusLineItemCategory;
begin
  Result:= FCategory;
end;

function TGeniusLineItemDiscount.GetDescription: String;
begin
  Result:= FDescription;
end;

function TGeniusLineItemDiscount.GetDisplayCustomSubTotal: String;
begin
  Result:= FDisplayCustomSubTotal;
end;

function TGeniusLineItemDiscount.GetDisplayOverride: String;
begin
  Result:= FDisplayOverride;
end;

function TGeniusLineItemDiscount.GetItemID: String;
begin
  Result:= FItemID;
end;

function TGeniusLineItemDiscount.GetItemType: TGeniusLineItemType;
begin
  Result:= FItemType;
end;

function TGeniusLineItemDiscount.GetItemTypeValue: String;
begin
  Result:= FItemTypeValue;
end;

function TGeniusLineItemDiscount.GetOrder: String;
begin
  Result:= FOrder;
end;

function TGeniusLineItemDiscount.GetOrderTax: Currency;
begin
  Result:= FOrderTax;
end;

function TGeniusLineItemDiscount.GetOrderTotal: Currency;
begin
  Result:= FOrderTotal;
end;

function TGeniusLineItemDiscount.GetQuantity: Integer;
begin
  Result:= FQuantity;
end;

function TGeniusLineItemDiscount.GetResponseMessage: String;
begin
  Result:= FResponseMessage;
end;

function TGeniusLineItemDiscount.GetStatus: TGeniusLineItemStatus;
begin
  Result:= FStatus;
end;

function TGeniusLineItemDiscount.GetTargetItemID: String;
begin
  Result:= FTargetItemID;
end;

function TGeniusLineItemDiscount.GetTaxAmount: Currency;
begin
  Result:= FTaxAmount;
end;

function TGeniusLineItemDiscount.GetUPC: String;
begin
  Result:= FUPC;
end;

procedure TGeniusLineItemDiscount.SetAmount(const Value: Currency);
begin
  FAmount:= Value;
end;

procedure TGeniusLineItemDiscount.SetCategory(const Value: TGeniusLineItemCategory);
begin
  FCategory:= Value;
end;

procedure TGeniusLineItemDiscount.SetDescription(const Value: String);
begin
  FDescription:= Value;
end;

procedure TGeniusLineItemDiscount.SetDisplayCustomSubTotal(const Value: String);
begin
  FDisplayCustomSubTotal:= Value;
end;

procedure TGeniusLineItemDiscount.SetDisplayOverride(const Value: String);
begin
  FDisplayOverride:= Value;
end;

procedure TGeniusLineItemDiscount.SetItemID(const Value: String);
begin
  FItemID:= Value;
end;

procedure TGeniusLineItemDiscount.SetItemType(const Value: TGeniusLineItemType);
begin
  FItemType:= Value;
end;

procedure TGeniusLineItemDiscount.SetItemTypeValue(const Value: String);
begin
  FItemTypeValue:= Value;
end;

procedure TGeniusLineItemDiscount.SetOrder(const Value: String);
begin
  FOrder:= Value;
end;

procedure TGeniusLineItemDiscount.SetOrderTax(const Value: Currency);
begin
  FOrderTax:= Value;
end;

procedure TGeniusLineItemDiscount.SetOrderTotal(const Value: Currency);
begin
  FOrderTotal:= Value;
end;

procedure TGeniusLineItemDiscount.SetQuantity(const Value: Integer);
begin
  FQuantity:= Value;
end;

procedure TGeniusLineItemDiscount.SetResponseMessage(const Value: String);
begin
  FResponseMessage:= Value;
end;

procedure TGeniusLineItemDiscount.SetStatus(const Value: TGeniusLineItemStatus);
begin
  FStatus:= Value;
end;

procedure TGeniusLineItemDiscount.SetTargetItemID(const Value: String);
begin
  FTargetItemID:= Value;
end;

procedure TGeniusLineItemDiscount.SetTaxAmount(const Value: Currency);
begin
  FTaxAmount:= Value;
end;

procedure TGeniusLineItemDiscount.SetUPC(const Value: String);
begin
  FUPC:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusStartOrderResponse Object'}

{ TGeniusStartOrderResponse }

constructor TGeniusStartOrderResponse.Create;
begin

end;

destructor TGeniusStartOrderResponse.Destroy;
begin

  inherited;
end;

function TGeniusStartOrderResponse.GetResponseMessage: String;
begin
  Result:= FResponseMessage;
end;

function TGeniusStartOrderResponse.GetStatus: TGeniusLineItemStatus;
begin
  Result:= FStatus;
end;

procedure TGeniusStartOrderResponse.SetResponseMessage(const Value: String);
begin
  FResponseMessage:= Value;
end;

procedure TGeniusStartOrderResponse.SetStatus(
  const Value: TGeniusLineItemStatus);
begin
  FStatus:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusCancelTransactionResponse Object'}

{ TGeniusCancelTransactionResponse }

constructor TGeniusCancelTransactionResponse.Create;
begin

end;

destructor TGeniusCancelTransactionResponse.Destroy;
begin

  inherited;
end;

function TGeniusCancelTransactionResponse.GetResponseMessage: String;
begin
  Result:= FResponseMessage;
end;

function TGeniusCancelTransactionResponse.GetStatus: TGeniusCancelTransactionStatus;
begin
  Result:= FStatus;
end;

procedure TGeniusCancelTransactionResponse.SetResponseMessage(
  const Value: String);
begin
  FResponseMessage:= Value;
end;

procedure TGeniusCancelTransactionResponse.SetStatus(
  const Value: TGeniusCancelTransactionStatus);
begin
  FStatus:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusPaymentDetails Object'}

{ TGeniusPaymentDetails }

constructor TGeniusPaymentDetails.Create(AOwner: TGenius);
var
  Det: IGeniusPaymentDetail;
begin
  FOwner:= AOwner;
  FPaymentDetails:= TInterfaceList.Create;
  if FOwner.TestMode then begin
    Det:= TGeniusPaymentDetail.Create(Self);


    Self.FPaymentDetails.Add(Det);
  end;
end;

destructor TGeniusPaymentDetails.Destroy;
begin
  Clear;
  FreeAndNil(FPaymentDetails);
  inherited;
end;

function TGeniusPaymentDetails.Add: IGeniusPaymentDetail;
begin
  Result:= TGeniusPaymentDetail.Create(Self);
  FPaymentDetails.Add(Result);
  Result._AddRef;
end;

procedure TGeniusPaymentDetails.Delete(const Index: Integer);
begin
  IGeniusPaymentDetail(FPaymentDetails[Index])._Release;
  FPaymentDetails.Delete(Index);
end;

procedure TGeniusPaymentDetails.Clear;
begin
  while PaymentCount > 0 do
    Delete(0);
end;

function TGeniusPaymentDetails.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TGeniusPaymentDetails.GetRequestedAmount: Currency;
begin
  Result:= FRequestedAmount;
end;

function TGeniusPaymentDetails.GetResponseType: TGeniusResponseType;
begin
  Result:= FResponseType;
end;

function TGeniusPaymentDetails.GetStatus: TGeniusTransStatus;
begin
  Result:= FStatus;
end;

function TGeniusPaymentDetails.GetTotalAmountApproved: Currency;
begin
  Result:= FTotalAmountApproved;
end;

procedure TGeniusPaymentDetails.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TGeniusPaymentDetails.SetRequestedAmount(const Value: Currency);
begin
  FRequestedAmount:= Value;
end;

procedure TGeniusPaymentDetails.SetResponseType(const Value: TGeniusResponseType);
begin
  FResponseType:= Value;
end;

procedure TGeniusPaymentDetails.SetStatus(const Value: TGeniusTransStatus);
begin
  FStatus:= Value;
end;

procedure TGeniusPaymentDetails.SetTotalAmountApproved(const Value: Currency);
begin
  FTotalAmountApproved:= Value;
end;

function TGeniusPaymentDetails.PaymentCount: Integer;
begin
  Result:= FPaymentDetails.Count;
end;

function TGeniusPaymentDetails.GetPaymentDetails(const Index: Integer): IGeniusPaymentDetail;
begin
  Result:= IGeniusPaymentDetail(FPaymentDetails[Index]);
end;

{$ENDREGION}

{$REGION 'TGeniusPaymentDetail Object'}

{ TGeniusPaymentDetail }

constructor TGeniusPaymentDetail.Create(AOwner: TGeniusPaymentDetails);
begin
  FOwner:= AOwner;
end;

destructor TGeniusPaymentDetail.Destroy;
begin

  inherited;
end;

function TGeniusPaymentDetail.Owner: IGeniusPaymentDetails;
begin
  Result:= FOwner;
end;

function TGeniusPaymentDetail.GetAccountNumber: String;
begin
  Result:= FAccountNumber;
end;

function TGeniusPaymentDetail.GetAmountApproved: Currency;
begin
  Result:= FAmountApproved;
end;

function TGeniusPaymentDetail.GetAmountCharged: Currency;
begin
  Result:= FAmountCharged;
end;

function TGeniusPaymentDetail.GetAuthorizationCode: String;
begin
  Result:= FAuthorizationCode;
end;

function TGeniusPaymentDetail.GetCustomer: String;
begin
  Result:= FCustomer;
end;

function TGeniusPaymentDetail.GetDiscountAmount: Currency;
begin
  Result:= FDiscountAmount;
end;

function TGeniusPaymentDetail.GetEmail: String;
begin
  Result:= FEmail;
end;

function TGeniusPaymentDetail.GetEntryMode: TGeniusEntryMode;
begin
  Result:= FEntryMode;
end;

function TGeniusPaymentDetail.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TGeniusPaymentDetail.GetExpirationDate: TExpirationDate;
begin
  Result:= FExpirationDate;
end;

function TGeniusPaymentDetail.GetGiftBalance: Currency;
begin
  Result:= FGiftBalance;
end;

function TGeniusPaymentDetail.GetLoyaltyBalance: Currency;
begin
  Result:= FLoyaltyBalance;
end;

function TGeniusPaymentDetail.GetLoyaltyLastVisit: TDateTime;
begin
  Result:= FLoyaltyLastVisit;
end;

function TGeniusPaymentDetail.GetLoyaltyLifetimeSpend: Currency;
begin
  Result:= FLoyaltyLifetimeSpend;
end;

function TGeniusPaymentDetail.GetLoyaltyVisits: Integer;
begin
  Result:= FLoyaltyVisits;
end;

function TGeniusPaymentDetail.GetPaymentType: TGeniusPaymentType;
begin
  Result:= FPaymentType;
end;

function TGeniusPaymentDetail.GetPhoneNumber: String;
begin
  Result:= FPhoneNumber;
end;

function TGeniusPaymentDetail.GetSignature: String;
begin
  Result:= FSIgnature;
end;

function TGeniusPaymentDetail.GetSignatureType: TGeniusSignatureType;
begin
  Result:= FSignatureType;
end;

function TGeniusPaymentDetail.GetStatus: TGeniusTransStatus;
begin
  Result:= FStatus;
end;

function TGeniusPaymentDetail.GetTaxAmount: Currency;
begin
  Result:= FTaxAmount;
end;

function TGeniusPaymentDetail.GetTipAmount: Currency;
begin
  Result:= FTipAmount;
end;

function TGeniusPaymentDetail.GetToken: String;
begin
  Result:= FToken;
end;

function TGeniusPaymentDetail.GetTransactionDate: TDateTime;
begin
  Result:= FTransactionDate;
end;

function TGeniusPaymentDetail.GetTransactionType: TGeniusTransactionType;
begin
  Result:= FTransactionType;
end;

function TGeniusPaymentDetail.GetUserTipAmount: Currency;
begin
  Result:= FUserTipAmount;
end;

function TGeniusPaymentDetail.GetVoucherAmount: Currency;
begin
  Result:= FVoucherAmount;
end;

procedure TGeniusPaymentDetail.SetAccountNumber(const Value: String);
begin
  FAccountNumber:= Value;
end;

procedure TGeniusPaymentDetail.SetAmountApproved(const Value: Currency);
begin
  FAmountApproved:= Value;
end;

procedure TGeniusPaymentDetail.SetAmountCharged(const Value: Currency);
begin
  FAmountCharged:= Value;
end;

procedure TGeniusPaymentDetail.SetAuthorizationCode(const Value: String);
begin
  FAuthorizationCode:= Value;
end;

procedure TGeniusPaymentDetail.SetCustomer(const Value: String);
begin
  FCustomer:= Value;
end;

procedure TGeniusPaymentDetail.SetDiscountAmount(const Value: Currency);
begin
  FDiscountAmount:= Value;
end;

procedure TGeniusPaymentDetail.SetEmail(const Value: String);
begin
  FEmail:= Value;
end;

procedure TGeniusPaymentDetail.SetEntryMode(const Value: TGeniusEntryMode);
begin
  FEntryMode:= Value;
end;

procedure TGeniusPaymentDetail.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TGeniusPaymentDetail.SetExpirationDate(const Value: TExpirationDate);
begin
  FExpirationDate:= Value;
end;

procedure TGeniusPaymentDetail.SetGiftBalance(const Value: Currency);
begin
  FGiftBalance:= Value;
end;

procedure TGeniusPaymentDetail.SetLoyaltyBalance(const Value: Currency);
begin
  FLoyaltyBalance:= Value;
end;

procedure TGeniusPaymentDetail.SetLoyaltyLastVisit(const Value: TDateTime);
begin
  FLoyaltyLastVisit:= Value;
end;

procedure TGeniusPaymentDetail.SetLoyaltyLifetimeSpend(const Value: Currency);
begin
  FLoyaltyLifetimeSpend:= Value;
end;

procedure TGeniusPaymentDetail.SetLoyaltyVisits(const Value: Integer);
begin
  FLoyaltyVisits:= Value;
end;

procedure TGeniusPaymentDetail.SetPaymentType(const Value: TGeniusPaymentType);
begin
  FPaymentType:= Value;
end;

procedure TGeniusPaymentDetail.SetPhoneNumber(const Value: String);
begin
  FPhoneNumber:= Value;
end;

procedure TGeniusPaymentDetail.SetSignature(const Value: String);
begin
  FSignature:= Value;
end;

procedure TGeniusPaymentDetail.SetSignatureType(const Value: TGeniusSignatureType);
begin
  FSignatureType:= Value;
end;

procedure TGeniusPaymentDetail.SetStatus(const Value: TGeniusTransStatus);
begin
  FStatus:= Value;
end;

procedure TGeniusPaymentDetail.SetTaxAmount(const Value: Currency);
begin
  FTaxAmount:= Value;
end;

procedure TGeniusPaymentDetail.SetTipAmount(const Value: Currency);
begin
  FTipAmount:= Value;
end;

procedure TGeniusPaymentDetail.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TGeniusPaymentDetail.SetTransactionDate(const Value: TDateTime);
begin
  FTransactionDate:= Value;
end;

procedure TGeniusPaymentDetail.SetTransactionType(const Value: TGeniusTransactionType);
begin
  FTransactionType:= Value;
end;

procedure TGeniusPaymentDetail.SetUserTipAmount(const Value: Currency);
begin
  FUserTipAmount:= Value;
end;

procedure TGeniusPaymentDetail.SetVoucherAmount(const Value: Currency);
begin
  FVoucherAmount:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusStatusResponse Object'}

{ TGeniusStatusResponse }

constructor TGeniusStatusResponse.Create;
begin

end;

destructor TGeniusStatusResponse.Destroy;
begin

  inherited;
end;

function TGeniusStatusResponse.GetApplicationVersion: String;
begin
  Result:= FApplicationVersion;
end;

function TGeniusStatusResponse.GetCurrentScreen: TGeniusCedScreen;
begin
  Result:= FCurrentScreen;
end;

function TGeniusStatusResponse.GetOSVersion: String;
begin
  Result:= FOSVersion;
end;

function TGeniusStatusResponse.GetPaymentDataCaptured: Boolean;
begin
  Result:= FPaymentDataCaptured;
end;

function TGeniusStatusResponse.GetResponseMessage: String;
begin
  Result:= FResponseMessage;
end;

function TGeniusStatusResponse.GetSerialNumber: String;
begin
  Result:= FSerialNumber;
end;

function TGeniusStatusResponse.GetStatus: TGeniusCedStatus;
begin
  Result:= FStatus;
end;

procedure TGeniusStatusResponse.SetApplicationVersion(const Value: String);
begin
  FApplicationVersion:= Value;
end;

procedure TGeniusStatusResponse.SetCurrentScreen(const Value: TGeniusCedScreen);
begin
  FCurrentScreen:= Value;
end;

procedure TGeniusStatusResponse.SetOSVersion(const Value: String);
begin
  FOSVersion:= Value;
end;

procedure TGeniusStatusResponse.SetPaymentDataCaptured(const Value: Boolean);
begin
  FPaymentDataCaptured:= Value;
end;

procedure TGeniusStatusResponse.SetResponseMessage(const Value: String);
begin
  FResponseMessage:= Value;
end;

procedure TGeniusStatusResponse.SetSerialNumber(const Value: String);
begin
  FSerialNumber:= Value;
end;

procedure TGeniusStatusResponse.SetStatus(const Value: TGeniusCedStatus);
begin
  FStatus:= Value;
end;

{$ENDREGION}

{$REGION 'TGeniusPostAuthResponse Object'}

{$ENDREGION}

{ TGeniusAgreementResponse }

constructor TGeniusAgreementResponse.Create;
begin

end;

destructor TGeniusAgreementResponse.Destroy;
begin

  inherited;
end;

function TGeniusAgreementResponse.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TGeniusAgreementResponse.GetRequestID: String;
begin
  Result:= FRequestID;
end;

function TGeniusAgreementResponse.GetStatus: TGeniusAgreementResponseStatus;
begin
  Result:= FStatus;
end;

procedure TGeniusAgreementResponse.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TGeniusAgreementResponse.SetRequestID(const Value: String);
begin
  FRequestID:= Value;
end;

procedure TGeniusAgreementResponse.SetStatus(
  const Value: TGeniusAgreementResponseStatus);
begin
  FStatus:= Value;
end;

initialization
  DefaultDOMVendor:= sOmniXmlVendor;

end.
