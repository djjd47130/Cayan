unit Cayan.MWv4.Impl;

interface

uses
  System.Classes, System.SysUtils, System.Variants,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL
  {$IFDEF IOS}
  , IdSSLOpenSSLHeaders_Static
  {$ENDIF}
  , Xml.XmlIntf
  , Xml.XmlDoc
  , Xml.XmlDom
  , Xml.OmniXmlDom
  , Cayan.Common
  , Cayan.MWv4.Intf
  ;

const
  EP_PROD_CREDIT_TRANS =   'https://ps1.merchantware.net/Merchantware/ws/RetailTransaction/v4/Credit.asmx';
  EP_PROD_VAULT_TRANS =   'https://ps1.merchantware.net/Merchantware/ws/RetailTransaction/v4/Credit.asmx';
  EP_PROD_EBT_TRANS =      'https://ps1.merchantware.net/Merchantware/ws/RetailTransaction/v4/Ebt.asmx';
  EP_PROD_GIFT_TRANS =     'https://ps1.merchantware.net/Merchantware/ws/ExtensionServices/v4/Giftcard.asmx';
  EP_PROD_LEVELUP_TRANS =  'https://ps1.merchantware.net/Merchantware/ws/RetailTransaction/v4/LevelUp.asmx';
  EP_PROD_CHECK_TRANS =    'https://ps1.merchantware.net/Merchantware/ws/RetailTransaction/v4/Check.asmx';
  EP_PROD_REPORTING =      'https://ps1.merchantware.net/Merchantware/ws/TransactionHistory/v4/Reporting.asmx';

  EP_TEST_CREDIT_TRANS =   'https://staging.merchantware.net/Merchantware/ws/RetailTransaction/v4/Credit.asmx';
  EP_TEST_VAULT_TRANS =   'https://staging.merchantware.net/Merchantware/ws/RetailTransaction/v4/Credit.asmx';
  EP_TEST_EBT_TRANS =      'https://staging.merchantware.net/Merchantware/ws/RetailTransaction/v4/Ebt.asmx';
  EP_TEST_GIFT_TRANS =     'https://staging.merchantware.net/Merchantware/ws/ExtensionServices/v4/Giftcard.asmx';
  EP_TEST_CHECK_TRANS =    'https://staging.merchantware.net/Merchantware/ws/ExtensionServices/v4/Check.asmx';
  EP_TEST_REPORTING =      'https://staging.merchantware.net/Merchantware/ws/TransactionHistory/v4/Reporting.asmx';

type
  TMerchantWare = class;
  TMWTransactionBase = class;
  TMWCreditTransactions = class;
  TMWVaultTransactions = class;
  TMWEbtTransactions = class;
  TMWGiftCardTransactions = class;
  TMWLevelUpTransactions = class;
  TMWCheckTransactions = class;
  TMWReportTransactions = class;
  //MerchantWare 4 Credit Data Structures
  TMWBatchResponse4 = class;
  TMWCreditLevel2Response4 = class;
  TMWCreditResponse4 = class;
  TMWEmvAuthorizeResponse4 = class;
  TMWEmvCompleteResponse4 = class;
  TMWSignatureResponse4 = class;
  TMWVaultBoardingResponse = class;
  TMWVaultPaymentInfoResponse = class;
  //MerchantWare 4 Gift Card Data Structures
  TMWGiftResponse4 = class;
  TMWGiftLoyaltyResponse4 = class;
  //MerchantWare 4 Report Data Structures
  TMWDetailedTransactionReference4 = class;
  TMWEmv = class;
  TMWEmvApplicationInformation = class;
  TMWEmvCardInformation = class;
  TMWEmvApplicationCryptogram = class;
  TMWEmvAmount = class;
  TMWEmvTerminalInformation = class;
  TMWEmvTransactionInformation = class;
  TMWHealthCareAmountDetails = class;
  TMWSupportedActions = class;
  TMWTransactionReference4List = class;
  TMWTransactionReference4 = class;
  TMWTransactionSummary4List = class;
  TMWTransactionSummary4 = class;

  TMerchantWare = class(TInterfacedObject, IMerchantWare)
  private
    FName: String;
    FKey: String;
    FSiteId: String;
    FTestMode: Boolean;

    FCredit: IMWCreditTransactions;
    FVault: IMWVaultTransactions;
    FGiftCard: IMWGiftCardTransactions;
    FEbt: IMWEbtTransactions;
    FReport: IMWReportTransactions;
    FLevelUp: IMWLevelUpTransactions;
    FCheck: IMWCheckTransactions;

    function GetCredentials: String;
    function SendTransportRequest(const Endpoint: String; const ActionUrl: String;
      const Action: String; const AXML: String): IXMLDocument;

  public
    constructor Create;
    destructor Destroy; override;

    //Credit Functions
    function DoRequestBatchResponse4(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWBatchResponse4;
    function DoRequestCreditLevel2Response4(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWCreditLevel2Response4;
    function DoRequestCreditResponse4(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWCreditResponse4;
    function DoRequestEmvAuthorizeResponse4(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWEmvAuthorizeResponse4;
    function DoRequestEmvCompleteResponse4(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWEmvCompleteResponse4;
    function DoRequestSignatureResponse4(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWSignatureResponse4;
    function DoRequestVaultBoardingResponse(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWVaultBoardingResponse;
    function DoRequestVaultPaymentInfoResponse(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWVaultPaymentInfoResponse;

    //Gift Card Functions
    function DoRequestGiftResponse4(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWGiftResponse4;
    function DoRequestGiftLoyaltyResponse4(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWGiftLoyaltyResponse4;

    //Report Functions
    function DoRequestDetailedTransactionReference4(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWDetailedTransactionReference4;
    function DoRequestTransactionReference4List(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWTransactionReference4List;
    function DoRequestTransactionSummary4List(const AXML: String;
      const ASvcPath: String; const AEndpointPath: String;
      const AReqName: String): IMWTransactionSummary4List;

  public
    function GetTestMode: Boolean;
    function GetKey: String;
    function GetName: String;
    function GetSiteId: String;
    procedure SetTestMode(const Value: Boolean);
    procedure SetKey(const Value: String);
    procedure SetName(const Value: String);
    procedure SetSiteId(const Value: String);

    function GetCheck: IMWCheckTransactions;
    function GetVault: IMWVaultTransactions;
    function GetCredit: IMWCreditTransactions;
    function GetEbt: IMWEbtTransactions;
    function GetGiftCard: IMWGiftCardTransactions;
    function GetLevelUp: IMWLevelUpTransactions;
    function GetReport: IMWReportTransactions;

    property Credit: IMWCreditTransactions read GetCredit;
    property Vault: IMWVaultTransactions read GetVault;
    property Ebt: IMWEbtTransactions read GetEbt;
    property GiftCard: IMWGiftCardTransactions read GetGiftCard;
    property LevelUp: IMWLevelUpTransactions read GetLevelUp;
    property Check: IMWCheckTransactions read GetCheck;
    property Report: IMWReportTransactions read GetReport;

    property TestMode: Boolean read GetTestMode write SetTestMode;
    property Name: String read GetName write SetName;
    property SiteId: String read GetSiteId write SetSiteId;
    property Key: String read GetKey write SetKey;
  end;

  TMWTransactionBase = class(TInterfacedObject, IMWTransactionBase)
  private
    FOwner: TMerchantWare;
  public
    constructor Create(AOwner: TMerchantWare); virtual;
    destructor Destroy; override;
    function GetEndpoint: String; virtual; abstract;
    function GetService: String; virtual; abstract;
  public
    function GetOwner: IMerchantWare;

    property Owner: IMerchantWare read GetOwner;
  end;

  TMWCreditTransactions = class(TMWTransactionBase, IMWCreditTransactions)
  public
    constructor Create(AOwner: TMerchantWare); override;
    destructor Destroy; override;
    function GetEndpoint: String; override;
    function GetService: String; override;
  public
    function ApplyTip(const Token: String;
      const TipAmount: Currency): IMWCreditResponse4;
    function CaptureSignatureTiff(const Token: String;
      const ImageData: TStream): IMWSignatureResponse4;
    function CaptureSignatureVector(const Token: String;
      const ImageData: String): IMWSignatureResponse4;
    function DebitSale(const InvoiceNumber: String; const Amount: Currency;
      const TrackData: String; const PinBlock: String;
      const PinKsn: String; const SurchargeAmount: Currency;
      const CashbackAmount: Currency; const ForceDuplicate: Boolean;
      const RegisterNumber: String): IMWCreditResponse4;
    //function EmvAuthorize(): IMWEmvAuthorizeResponse4;
    //function EmvComplete(): IMWEmvCompleteResponse4;
    function ForceSale(const InvoiceNumber: String;
      const AuthorizationCode: String; const Amount: Currency;
      const CardNumber: TCardNumber; const ExpirationDate: TExpirationDate;
      const Cardholder: String; const RegisterNumber: String;
      const MerchantTransactionId: String): IMWCreditResponse4;
    function Level2Sale(const InvoiceNumber: String; const Amount: Currency;
      const TrackData: String; const CustomerCode: String;
      const PoNumber: String; const TaxAmount: Currency;
      const ForceDuplicate: Boolean; const RegisterNumber: String;
      const MerchantTransactionId: String;
      const EntryMode: TMWEntryMode): IMWCreditResponse4;
    function Level2SaleKeyed(const InvoiceNumber: String;
      const Amount: Currency; const CardNumber: TCardNumber;
      const ExpirationDate: TExpirationDate; const CardHolder: String;
      const BillStreet: String; const BillZip: String;
      const SecCode: String; const CustomerCode: String;
      const PoNumber: String; const TaxAmount: Currency;
      const ForceDuplicate: Boolean; const RegisterNumber: String;
      const MerchantTransactionId: String): IMWCreditResponse4;
    function Level2SaleVault(const InvoiceNumber: String; const Amount: Currency;
      const VaultToken: String; const CustomerCode: String;
      const PoNumber: String; const TaxAmount: Currency;
      const ForceDuplicate: Boolean; const RegisterNumber: String;
      const MerchantTransactionId: String;
      const EntryMode: TMWEntryMode): IMWCreditResponse4;
    function PostAuthorization(const InvoiceNumber: String;
      const Token: String; const Amount: Currency; const RegisterNumber: String;
      const MerchantTransactionId: String): IMWCreditResponse4;
    function PreAuthorization(const InvoiceNumber: String; const Amount: Currency;
      const TrackData: String; const RegisterNumber: String;
      const MerchantTransactionId: String;
      const EntryMode: TMWEntryMode): IMWCreditResponse4;
    function PreAuthorizationKeyed(const InvoiceNumber: String; const Amount: Currency;
      const CardNumber: TCardNumber; const ExpirationDate: TExpirationDate;
      const CardHolder: String; const BillStreet: String;
      const BillZip: String; const SecCode: String;
      const RegisterNumber: String;
      const MerchantTransactionId: String): IMWCreditResponse4;
    function PreAuthorizationVault(const InvoiceNumber: String; const Amount: Currency;
      const VaultToken: String; const RegisterNumber: String;
      const MerchantTransactionID: String): IMWCreditResponse4;
    function Refund(const InvoiceNumber: String; const Token: String;
      const OverrideAmount: Currency; const RegisterNumber: String;
      const MerchantTransactionId: String): IMWCreditResponse4;
    function RepeatSale(const InvoiceNumber: String;
      const Token: String; const OverrideAmount: Currency;
      const ExpirationDate: TExpirationDate; const BillStreet: String;
      const BillZip: String; const RegisterNumber: String;
      const MerchantTransactionId: String): IMWCreditResponse4;
    function Sale(const InvoiceNumber: String; const Amount: Currency;
      const TrackData: String; const ForceDuplicate: Boolean;
      const RegisterNumber: String; const MerchantTransactionId: String;
      const EntryMode: TMWEntryMode): IMWCreditResponse4;
    function SaleKeyed(const InvoiceNumber: String; const Amount: Currency;
      const CardNumber: TCardNumber; const ExpirationDate: TExpirationDate;
      const CardHolder: String; const BillStreet: String;
      const BillZip: String; const SecCode: String;
      const ForceDuplicate: Boolean; const RegisterNumber: String;
      const MerchantTransactionId: String): IMWCreditResponse4;
    function SaleVault(const InvoiceNumber: String; const Amount: Currency;
      const VaultToken: String; const ForceDuplicate: Boolean;
      const RegisterNumber: String;
      const MerchantTransactionID: String): IMWCreditResponse4;
    function SettleBatch: IMWBatchResponse4;
    function Void(const Token: String; const RegisterNumber: String;
      const MerchantTransactionId: String): IMWCreditResponse4;
    function VoidPreAuthorization(const Token: String;
      const RegisterNumber, MerchantTransactionId: String): IMWCreditResponse4;
  end;

  TMWVaultTransactions = class(TMWTransactionBase, IMWVaultTransactions)
  public
    constructor Create(AOwner: TMerchantWare); override;
    destructor Destroy; override;
    function GetEndpoint: String; override;
    function GetService: String; override;
  public
    function VaultBoardCredit(const TrackData: String;
      const BillStreet: String; const BillZip: String;
      const MerchantDefinedToken: String = ''): IMWVaultBoardingResponse;
    function VaultBoardCreditByReference(const ReferenceNumber: String;
      const MerchantDefinedToken: String = ''): IMWVaultBoardingResponse;
    function VaultBoardCreditKeyed(const CardNumber: TCardNumber;
      const Expiration: TExpirationDate; const CardHolder: String;
      const BillStreet: String; const BillZip: String;
      const MerchantDefinedToken: String = ''): IMWVaultBoardingResponse;
    function VaultDeleteToken(const VaultToken: String): IMWVaultBoardingResponse;
    function VaultFindPaymentInfo(const VaultToken: String): IMWVaultPaymentInfoResponse;
  end;

  TMWEbtTransactions = class(TMWTransactionBase, IMWEbtTransactions)
  public
    constructor Create(AOwner: TMerchantWare); override;
    destructor Destroy; override;
    function GetEndpoint: String; override;
    function GetService: String; override;
  public

  end;

  TMWGiftCardTransactions = class(TMWTransactionBase, IMWGiftCardTransactions)
  public
    constructor Create(AOwner: TMerchantWare); override;
    destructor Destroy; override;
    function GetEndpoint: String; override;
    function GetService: String; override;
  public

  end;

  TMWLevelUpTransactions = class(TMWTransactionBase, IMWLevelUpTransactions)
  public
    constructor Create(AOwner: TMerchantWare); override;
    destructor Destroy; override;
    function GetEndpoint: String; override;
    function GetService: String; override;
  public

  end;

  TMWCheckTransactions = class(TMWTransactionBase, IMWCheckTransactions)
  public
    constructor Create(AOwner: TMerchantWare); override;
    destructor Destroy; override;
    function GetEndpoint: String; override;
    function GetService: String; override;
  public

  end;

  TMWReportTransactions = class(TMWTransactionBase, IMWReportTransactions)
  public
    constructor Create(AOwner: TMerchantWare); override;
    destructor Destroy; override;
    function GetEndpoint: String; override;
    function GetService: String; override;
  public
    function CurrentBatchSummary(const CardholderFilter: String;
      const CardType: TMWCardType): IMWTransactionSummary4List;
    function CurrentBatchTransactions: IMWTransactionReference4List;
    function SummaryByDate(const StartDate, EndDate: TDate;
      const CardholderFilter: String;
      const CardType: TMWCardType): IMWTransactionSummary4List;
    function TransactionsByDate(const StartDate, EndDate: TDate;
      const InvoiceNumber: String;
      const RegisterNumber: String;
      const AuthorizationCode: String): IMWTransactionReference4List;
    function TransactionsByReference(const Token: String): IMWTransactionReference4List;
    function TransactionsByTransactionId(
      const MerchantTransactionId: String): IMWTransactionReference4List;
    function DetailedTransactionByReference(const Token: String): IMWDetailedTransactionReference4;
    function DetailedTransactionByTransactionId(
      const MerchantTransactionId: String): IMWDetailedTransactionReference4;
  end;

  //MerchantWare 4 Credit Data Structures

  TMWBatchResponse4 = class(TInterfacedObject, IMWBatchResponse4)
  private
    FAuthorizationCode: String;
    FBatchAmount: Currency;
    FBatchStatus: TMWBatchStatusSet;
    FErrorMessage: String;
    FExtraData: String;
    FToken: String;
    FTransactionCount: Integer;
    FTRansactionDate: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAuthorizationCode: String;
    function GetBatchAmount: Currency;
    function GetBatchStatus: TMWBatchStatusSet;
    function GetErrorMessage: String;
    function GetExtraData: String;
    function GetToken: String;
    function GetTransactionCount: Integer;
    function GetTransactionDate: TDateTime;
    procedure SetAuthorizationCode(const Value: String);
    procedure SetBatchAmount(const Value: Currency);
    procedure SetBatchStatus(const Value: TMWBatchStatusSet);
    procedure SetErrorMessage(const Value: String);
    procedure SetExtraData(const Value: String);
    procedure SetToken(const Value: String);
    procedure SetTransactionCount(const Value: Integer);
    procedure SetTransactionDate(const Value: TDateTime);

    property AuthorizationCode: String read GetAuthorizationCode write SetAuthorizationCode;
    property BatchAmount: Currency read GetBatchAmount write SetBatchAmount;
    property BatchStatus: TMWBatchStatusSet read GetBatchStatus write SetBatchStatus;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property ExtraData: String read GetExtraData write SetExtraData;
    property Token: String read GetToken write SetToken;
    property TransactionCount: Integer read GetTransactionCount write SetTransactionCount;
    property TransactionDate: TDateTime read GetTransactionDate write SetTransactionDate;
  end;

  TMWCreditLevel2Response4 = class(TInterfacedObject, IMWCreditLevel2Response4)
  private
    FAmount: Currency;
    FApprovalStatus: TMWApprovalStatusSet;
    FApprovalCode: Integer;
    FApprovalMessage: String;
    FAuthorizationCode: String;
    FAvsResponse: String;
    FCardholder: String;
    FCardNumber: TCardNumber;
    FCardType: TMWCardType;
    FCustomerCode: String;
    FCvResponse: String;
    FEntryMode: TMWPosEntryType;
    FErrorMessage: String;
    FExtraData: String;
    FInvoiceNumber: String;
    FPurchaseOrderNumber: String;
    FTaxAmount: Currency;
    FToken: String;
    FTransactionDate: TDateTime;
    FTransactionType: TMWTransactionType;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAmount: Currency;
    function GetApprovalCode: Integer;
    function GetApprovalMessage: String;
    function GetApprovalStatus: TMWApprovalStatusSet;
    function GetAuthorizationCode: String;
    function GetAvsResponse: String;
    function GetCardholder: String;
    function GetCardNumber: TCardNumber;
    function GetCardType: TMWCardType;
    function GetCustomerCode: String;
    function GetCvResponse: String;
    function GetEntryMode: TMWPosEntryType;
    function GetErrorMessage: String;
    function GetExtraData: String;
    function GetInvoiceNumber: String;
    function GetPurchaseOrderNumber: String;
    function GetTaxAmount: Currency;
    function GetToken: String;
    function GetTransactionDate: TDateTime;
    function GetTransactionType: TMWTransactionType;
    procedure SetAmount(const Value: Currency);
    procedure SetApprovalCode(const Value: Integer);
    procedure SetApprovalMessage(const Value: String);
    procedure SetApprovalStatus(const Value: TMWApprovalStatusSet);
    procedure SetAuthorizationCode(const Value: String);
    procedure SetAvsResponse(const Value: String);
    procedure SetCardholder(const Value: String);
    procedure SetCardNumber(const Value: TCardNumber);
    procedure SetCardType(const Value: TMWCardType);
    procedure SetCustomerCode(const Value: String);
    procedure SetCvResponse(const Value: String);
    procedure SetEntryMode(const Value: TMWPosEntryType);
    procedure SetErrorMessage(const Value: String);
    procedure SetExtraData(const Value: String);
    procedure SetInvoiceNumber(const Value: String);
    procedure SetPurchaseOrderNumber(const Value: String);
    procedure SetTaxAmount(const Value: Currency);
    procedure SetToken(const Value: String);
    procedure SetTransactionDate(const Value: TDateTime);
    procedure SetTransactionType(const Value: TMWTransactionType);

    property Amount: Currency read GetAmount write SetAmount;
    property ApprovalStatus: TMWApprovalStatusSet read GetApprovalStatus write SetApprovalStatus;
    property ApprovalCode: Integer read GetApprovalCode write SetApprovalCode;
    property ApprovalMessage: String read GetApprovalMessage write SetApprovalMessage;
    property AuthorizationCode: String read GetAuthorizationCode write SetAuthorizationCode;
    property AvsResponse: String read GetAvsResponse write SetAvsResponse;
    property Cardholder: String read GetCardholder write SetCardholder;
    property CardNumber: TCardNumber read GetCardNumber write SetCardNumber;
    property CardType: TMWCardType read GetCardType write SetCardType;
    property CustomerCode: String read GetCustomerCode write SetCustomerCode;
    property CvResponse: String read GetCvResponse write SetCvResponse;
    property EntryMode: TMWPosEntryType read GetEntryMode write SetEntryMode;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property ExtraData: String read GetExtraData write SetExtraData;
    property InvoiceNumber: String read GetInvoiceNumber write SetInvoiceNumber;
    property PurchaseOrderNumber: String read GetPurchaseOrderNumber write SetPurchaseOrderNumber;
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;
    property Token: String read GetToken write SetToken;
    property TransactionDate: TDateTime read GetTransactionDate write SetTransactionDate;
    property TransactionType: TMWTransactionType read GetTransactionType write SetTransactionType;
  end;

  TMWCreditResponse4 = class(TInterfacedObject, IMWCreditResponse4)
  private
    FAmount: Currency;
    FApprovalStatus: TMWApprovalStatusSet;
    FApprovalCode: Integer;
    FApprovalMessage: String;
    FAuthorizationCode: String;
    FAvsResponse: String;
    FCardholder: String;
    FCardNumber: String;
    FCardType: TMWCardType;
    FCvResponse: String;
    FEntryMode: TMWPosEntryType;
    FErrorMessage: String;
    FExtraData: String;
    FInvoiceNumber: String;
    FToken: String;
    FTransactionDate: TDateTime;
    FTransactionType: TMWTransactionType;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAmount: Currency;
    function GetApprovalCode: Integer;
    function GetApprovalMessage: String;
    function GetApprovalStatus: TMWApprovalStatusSet;
    function GetAuthorizationCode: String;
    function GetAvsResponse: String;
    function GetCardholder: String;
    function GetCardNumber: String;
    function GetCardType: TMWCardType;
    function GetCvResponse: String;
    function GetEntryMode: TMWPosEntryType;
    function GetErrorMessage: String;
    function GetExtraData: String;
    function GetInvoiceNumber: String;
    function GetToken: String;
    function GetTransactionDate: TDateTime;
    function GetTransactionType: TMWTransactionType;
    procedure SetAmount(const Value: Currency);
    procedure SetApprovalCode(const Value: Integer);
    procedure SetApprovalMessage(const Value: String);
    procedure SetApprovalStatus(const Value: TMWApprovalStatusSet);
    procedure SetAuthorizationCode(const Value: String);
    procedure SetAvsResponse(const Value: String);
    procedure SetCardholder(const Value: String);
    procedure SetCardNumber(const Value: String);
    procedure SetCardType(const Value: TMWCardType);
    procedure SetCvResponse(const Value: String);
    procedure SetEntryMode(const Value: TMWPosEntryType);
    procedure SetErrorMessage(const Value: String);
    procedure SetExtraData(const Value: String);
    procedure SetInvoiceNumber(const Value: String);
    procedure SetToken(const Value: String);
    procedure SetTransactionDate(const Value: TDateTime);
    procedure SetTransactionType(const Value: TMWTransactionType);

    property Amount: Currency read GetAmount write SetAmount;
    property ApprovalStatus: TMWApprovalStatusSet read GetApprovalStatus write SetApprovalStatus;
    property ApprovalCode: Integer read GetApprovalCode write SetApprovalCode;
    property ApprovalMessage: String read GetApprovalMessage write SetApprovalMessage;
    property AuthorizationCode: String read GetAuthorizationCode write SetAuthorizationCode;
    property AvsResponse: String read GetAvsResponse write SetAvsResponse;
    property Cardholder: String read GetCardholder write SetCardholder;
    property CardNumber: String read GetCardNumber write SetCardNumber;
    property CardType: TMWCardType read GetCardType write SetCardType;
    property CvResponse: String read GetCvResponse write SetCvResponse;
    property EntryMode: TMWPosEntryType read GetEntryMode write SetEntryMode;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property ExtraData: String read GetExtraData write SetExtraData;
    property InvoiceNumber: String read GetInvoiceNumber write SetInvoiceNumber;
    property Token: String read GetToken write SetToken;
    property TransactionDate: TDateTime read GetTransactionDate write SetTransactionDate;
    property TransactionType: TMWTransactionType read GetTransactionType write SetTransactionType;
  end;

  TMWEmvAuthorizeResponse4 = class(TInterfacedObject, IMWEmvAuthorizeResponse4)
  private
    FApprovalStatus: TMWEmvApprovalStatusSet;
    FApprovalCode: Integer;
    FApprovalMessage: String;
    FErrorMessage: String;
    FToken: String;
    FTransactionDate: TDateTime;
    FTlvData: String;
    FCashbackAmount: Currency;
    FDonationAmount: Currency;
    FUserTipAmount: Currency;
    FSurchargeAmount: Currency;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetApprovalCode: Integer;
    function GetApprovalMessage: String;
    function GetApprovalStatus: TMWEmvApprovalStatusSet;
    function GetCashbackAmount: Currency;
    function GetDonationAmount: Currency;
    function GetErrorMessage: String;
    function GetSurchargeAmount: Currency;
    function GetTlvData: String;
    function GetToken: String;
    function GetTransactionDate: TDateTime;
    function GetUserTipAmount: Currency;
    procedure SetApprovalCode(const Value: Integer);
    procedure SetApprovalMessage(const Value: String);
    procedure SetApprovalStatus(const Value: TMWEmvApprovalStatusSet);
    procedure SetCashbackAmount(const Value: Currency);
    procedure SetDonationAmount(const Value: Currency);
    procedure SetErrorMessage(const Value: String);
    procedure SetSurchargeAmount(const Value: Currency);
    procedure SetTlvData(const Value: String);
    procedure SetToken(const Value: String);
    procedure SetTransactionDate(const Value: TDateTime);
    procedure SetUserTipAmount(const Value: Currency);

    property ApprovalStatus: TMWEmvApprovalStatusSet read GetApprovalStatus write SetApprovalStatus;
    property ApprovalCode: Integer read GetApprovalCode write SetApprovalCode;
    property ApprovalMessage: String read GetApprovalMessage write SetApprovalMessage;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property Token: String read GetToken write SetToken;
    property TransactionDate: TDateTime read GetTransactionDate write SetTransactionDate;
    property TlvData: String read GetTlvData write SetTlvData;
    property CashbackAmount: Currency read GetCashbackAmount write SetCashbackAmount;
    property DonationAmount: Currency read GetDonationAmount write SetDonationAmount;
    property UserTipAmount: Currency read GetUserTipAmount write SetUserTipAmount;
    property SurchargeAmount: Currency read GetSurchargeAmount write SetSurchargeAmount;
  end;

  TMWEmvCompleteResponse4 = class(TInterfacedObject, IMWEmvCompleteResponse4)
  private
    FApprovalStatus: TMWEmvApprovalStatusSet;
    FApprovalCode: Integer;
    FApprovalMessage: String;
    FErrorMessage: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetApprovalCode: Integer;
    function GetApprovalMessage: String;
    function GetApprovalStatus: TMWEmvApprovalStatusSet;
    function GetErrorMessage: String;
    procedure SetApprovalCode(const Value: Integer);
    procedure SetApprovalMessage(const Value: String);
    procedure SetApprovalStatus(const Value: TMWEmvApprovalStatusSet);
    procedure SetErrorMessage(const Value: String);

    property ApprovalStatus: TMWEmvApprovalStatusSet read GetApprovalStatus write SetApprovalStatus;
    property ApprovalCode: Integer read GetApprovalCode write SetApprovalCode;
    property ApprovalMessage: String read GetApprovalMessage write SetApprovalMessage;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
  end;

  TMWSignatureResponse4 = class(TInterfacedObject, IMWSignatureResponse4)
  private
    FErrorMessage: String;
    FToken: String;
    FSignatureType: TMWSignatureType;
    FUploadDate: TDateTime;
    FUploadStatus: TMWSignatureStatus;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetErrorMessage: String;
    function GetSignatureType: TMWSignatureType;
    function GetToken: String;
    function GetUploadDate: TDateTime;
    function GetUploadStatus: TMWSignatureStatus;
    procedure SetErrorMessage(const Value: String);
    procedure SetSignatureType(const Value: TMWSignatureType);
    procedure SetToken(const Value: String);
    procedure SetUploadDate(const Value: TDateTime);
    procedure SetUploadStatus(const Value: TMWSignatureStatus);

    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property Token: String read GetToken write SetToken;
    property SignatureType: TMWSignatureType read GetSignatureType write SetSignatureType;
    property TransactionDate: TDateTime read GetUploadDate write SetUploadDate;
    property UploadStatus: TMWSignatureStatus read GetUploadStatus write SetUploadStatus;
  end;

  TMWVaultBoardingResponse = class(TInterfacedObject, IMWVaultBoardingResponse)
  private
    FVaultToken: String;
    FErrorCode: Integer;
    FErrorMessage: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetErrorCode: Integer;
    function GetErrorMessage: String;
    function GetVaultToken: String;
    procedure SetErrorCode(const Value: Integer);
    procedure SetErrorMessage(const Value: String);
    procedure SetVaultToken(const Value: String);

    property VaultToken: String read GetVaultToken write SetVaultToken;
    property ErrorCode: Integer read GetErrorCode write SetErrorCode; //???
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
  end;

  TMWVaultPaymentInfoResponse = class(TInterfacedObject, IMWVaultPaymentInfoResponse)
  private
    FCardholder: String;
    FCardNumber: String;
    FCardType: TMWCardType;
    FErrorCode: Integer;
    FErrorMessage: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetCardholder: String;
    function GetCardNumber: String;
    function GetCardType: TMWCardType;
    function GetErrorCode: Integer;
    function GetErrorMessage: String;
    procedure SetCardholder(const Value: String);
    procedure SetCardNumber(const Value: String);
    procedure SetCardType(const Value: TMWCardType);
    procedure SetErrorCode(const Value: Integer);
    procedure SetErrorMessage(const Value: String);

    property Cardholder: String read GetCardholder write SetCardholder;
    property CardNumber: String read GetCardNumber write SetCardNumber;
    property CardType: TMWCardType read GetCardType write SetCardType;
    property ErrorCode: Integer read GetErrorCode write SetErrorCode;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
  end;

  //MerchantWare 4 Gift Card Data Structures

  TMWGiftResponse4 = class(TInterfacedObject, IMWGiftResponse4)
  private
    FAmount: Currency;
    FApprovalStatus: TMWApprovalStatusSet;
    FApprovalCode: Integer;
    FApprovalMessage: String;
    FCardBalance: Currency;
    FCardholder: String;
    FCardNumber: String;
    FEntryMode: TMWPosEntryType;
    FErrorMessage: String;
    FExpirationDate: TExpirationDate;
    FExtraData: String;
    FMiscMessage: String;
    FResponseMessage: String;
    FToken: String;
    FTransactionDate: TDateTime;
    FTransactionID: String;
    FTransactionType: TMWTransactionType;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAmount: Currency;
    function GetApprovalCode: Integer;
    function GetApprovalMessage: String;
    function GetApprovalStatus: TMWApprovalStatusSet;
    function GetCardBalance: Currency;
    function GetCardholder: String;
    function GetCardNumber: String;
    function GetEntryMode: TMWPosEntryType;
    function GetErrorMessage: String;
    function GetExpirationDate: TExpirationDate;
    function GetExtraData: String;
    function GetMiscMessage: String;
    function GetResponseMessage: String;
    function GetToken: String;
    function GetTransactionDate: TDateTime;
    function GetTransactionID: String;
    function GetTransactionType: TMWTransactionType;
    procedure SetAmount(const Value: Currency);
    procedure SetApprovalCode(const Value: Integer);
    procedure SetApprovalMessage(const Value: String);
    procedure SetApprovalStatus(const Value: TMWApprovalStatusSet);
    procedure SetCardBalance(const Value: Currency);
    procedure SetCardholder(const Value: String);
    procedure SetCardNumber(const Value: String);
    procedure SetEntryMode(const Value: TMWPosEntryType);
    procedure SetErrorMessage(const Value: String);
    procedure SetExpirationDate(const Value: TExpirationDate);
    procedure SetExtraData(const Value: String);
    procedure SetMiscMessage(const Value: String);
    procedure SetResponseMessage(const Value: String);
    procedure SetToken(const Value: String);
    procedure SetTransactionDate(const Value: TDateTime);
    procedure SetTransactionID(const Value: String);
    procedure SetTransactionType(const Value: TMWTransactionType);

    property Amount: Currency read GetAmount write SetAmount;
    property ApprovalStatus: TMWApprovalStatusSet read GetApprovalStatus write SetApprovalStatus;
    property ApprovalCode: Integer read GetApprovalCode write SetApprovalCode;
    property ApprovalMessage: String read GetApprovalMessage write SetApprovalMessage;
    property CardBalance: Currency read GetCardBalance write SetCardBalance;
    property Cardholder: String read GetCardholder write SetCardholder;
    property CardNumber: String read GetCardNumber write SetCardNumber;
    property EntryMode: TMWPosEntryType read GetEntryMode write SetEntryMode;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property ExpirationDate: TExpirationDate read GetExpirationDate write SetExpirationDate;
    property ExtraData: String read GetExtraData write SetExtraData;
    property MiscMessage: String read GetMiscMessage write SetMiscMessage;
    property ResponseMessage: String read GetResponseMessage write SetResponseMessage;
    property Token: String read GetToken write SetToken;
    property TransactionDate: TDateTime read GetTransactionDate write SetTransactionDate;
    property TransactionID: String read GetTransactionID write SetTransactionID;
    property TransactionType: TMWTransactionType read GetTransactionType write SetTransactionType;
  end;

  TMWGiftLoyaltyResponse4 = class(TInterfacedObject, IMWGiftLoyaltyResponse4)
  private
    FApprovalStatus: TMWApprovalStatusSet;
    FApprovalCode: Integer;
    FApprovalMessage: String;
    FCardBalance: Currency;
    FCardholder: String;
    FCardNumber: String;
    FEntryMode: TMWPosEntryType;
    FErrorMessage: String;
    FExpirationDate: TExpirationDate;
    FExtraData: String;
    FLifePointsBalance: Integer;
    FMiscMessage: String;
    FPointsBalance: Integer;
    FResponseMessage: String;
    FToken: String;
    FTransactionID: String;
    FTransactionDate: TDateTime;
    FTransactionType: TMWTransactionType;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetApprovalStatus: TMWApprovalStatusSet;
    function GetCardBalance: Currency;
    function GetCardholder: String;
    function GetCardNumber: String;
    function GetEntryMode: TMWPosEntryType;
    function GetErrorMessage: String;
    function GetExpirationDate: TExpirationDate;
    function GetExtraData: String;
    function GetLifePointsBalance: Integer;
    function GetMiscMessage: String;
    function GetPointsBalance: Integer;
    function GetResponseMessage: String;
    function GetToken: String;
    function GetTransactionDate: TDateTime;
    function GetTransactionID: String;
    function GetTransactionType: TMWTransactionType;
    procedure SetApprovalStatus(const Value: TMWApprovalStatusSet);
    procedure SetCardBalance(const Value: Currency);
    procedure SetCardholder(const Value: String);
    procedure SetCardNumber(const Value: String);
    procedure SetEntryMode(const Value: TMWPosEntryType);
    procedure SetErrorMessage(const Value: String);
    procedure SetExpirationDate(const Value: TExpirationDate);
    procedure SetExtraData(const Value: String);
    procedure SetLifePointsBalance(const Value: Integer);
    procedure SetMiscMessage(const Value: String);
    procedure SetPointsBalance(const Value: Integer);
    procedure SetResponseMessage(const Value: String);
    procedure SetToken(const Value: String);
    procedure SetTransactionDate(const Value: TDateTime);
    procedure SetTransactionID(const Value: String);
    procedure SetTransactionType(const Value: TMWTransactionType);
    function GetApprovalCode: Integer;
    function GetApprovalMessage: String;
    procedure SetApprovalCode(const Value: Integer);
    procedure SetApprovalMessage(const Value: String);

    property ApprovalStatus: TMWApprovalStatusSet read GetApprovalStatus write SetApprovalStatus;
    property ApprovalCode: Integer read GetApprovalCode write SetApprovalCode;
    property ApprovalMessage: String read GetApprovalMessage write SetApprovalMessage;
    property CardBalance: Currency read GetCardBalance write SetCardBalance;
    property Cardholder: String read GetCardholder write SetCardholder;
    property CardNumber: String read GetCardNumber write SetCardNumber;
    property EntryMode: TMWPosEntryType read GetEntryMode write SetEntryMode;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property ExpirationDate: TExpirationDate read GetExpirationDate write SetExpirationDate;
    property ExtraData: String read GetExtraData write SetExtraData;
    property LifePointsBalance: Integer read GetLifePointsBalance write SetLifePointsBalance;
    property MiscMessage: String read GetMiscMessage write SetMiscMessage;
    property PointsBalance: Integer read GetPointsBalance write SetPointsBalance;
    property ResponseMessage: String read GetResponseMessage write SetResponseMessage;
    property Token: String read GetToken write SetToken;
    property TransactionID: String read GetTransactionID write SetTransactionID;
    property TransactionDate: TDateTime read GetTransactionDate write SetTransactionDate;
    property TransactionType: TMWTransactionType read GetTransactionType write SetTransactionType;
  end;

  //MerchantWare 4 Report Data Structures

  TMWDetailedTransactionReference4 = class(TInterfacedObject, IMWDetailedTransactionReference4)
  private
    FApprovalCode: String;
    FApprovalCode2: Integer;
    FApprovalMessage: String;
    FApprovalStatus: TMWApprovalStatusSet;
    FAuthorizationAmount: Currency;
    FAvsResponse: String;
    FBatchNumber: String;
    FCardholder: String;
    FCashBackAmount: Currency;
    FCardNumber: String;
    FCardType: TMWCardType;
    FConvenienceAmount: Currency;
    FCustomerId: String;
    FCvResponse: String;
    FDiscountAmount: Currency;
    FEntryMode: TMWPosEntryType;
    FErrorMessage: String;
    FExpirationDate: TExpirationDate;
    FHostReference: String;
    FInvoiceNumber: String;
    FIsCardNumberKeyed: Boolean;
    FIsTransactionAdjusted: Boolean;
    FIsTransactionCaptured: Boolean;
    FIsTransactionVoided: Boolean;
    FIsTransactionSettled: Boolean;
    FMerchantTransactionId: String;
    FOriginalToken: String;
    FPostalCode: String;
    FPurchaseOrderNumber: String;
    FRegisterNumber: String;
    FSupportedActions: IMWSupportedActions;
    FSurchargeAmount: Currency;
    FTaxAmount: Currency;
    FTipAmount: Currency;
    FToken: String;
    FTotalAmount: Currency;
    FTransactionDate: TDateTime;
    FTransactionType: TMWTransactionType;
    FUserName: String;
    FHealthCareAmountDetails: IMWHealthCareAmountDetails;
    FEmv: IMWEmv;
    FEmvUsed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetApprovalCode: String;
    function GetApprovalCode2: Integer;
    function GetApprovalMessage: String;
    function GetApprovalStatus: TMWApprovalStatusSet;
    function GetAuthorizationAmount: Currency;
    function GetAvsResponse: String;
    function GetBatchNumber: String;
    function GetCardholder: String;
    function GetCardNumber: String;
    function GetCardType: TMWCardType;
    function GetCashBackAmount: Currency;
    function GetConvenienceAmount: Currency;
    function GetCustomerId: String;
    function GetCvResponse: String;
    function GetDiscountAmount: Currency;
    function GetEmv: IMWEmv;
    function GetEntryMode: TMWPosEntryType;
    function GetErrorMessage: String;
    function GetExpirationDate: TExpirationDate;
    function GetHealthCareAmountDetails: IMWHealthCareAmountDetails;
    function GetHostReference: String;
    function GetInvoiceNumber: String;
    function GetIsCardNumberKeyed: Boolean;
    function GetIsTransactionAdjusted: Boolean;
    function GetIsTransactionCaptured: Boolean;
    function GetIsTransactionSettled: Boolean;
    function GetIsTransactionVoided: Boolean;
    function GetMerchantTransactionId: String;
    function GetOriginalToken: String;
    function GetPostalCode: String;
    function GetPurchaseOrderNumber: String;
    function GetRegisterNumber: String;
    function GetSupportedActions: IMWSupportedActions;
    function GetSurchargeAmount: Currency;
    function GetTaxAmount: Currency;
    function GetTipAmount: Currency;
    function GetToken: String;
    function GetTotalAmount: Currency;
    function GetTransactionDate: TDateTime;
    function GetTransactionType: TMWTransactionType;
    function GetUserName: String;
    procedure SetApprovalCode(const Value: String);
    procedure SetApprovalCode2(const Value: Integer);
    procedure SetApprovalMessage(const Value: String);
    procedure SetApprovalStatus(const Value: TMWApprovalStatusSet);
    procedure SetAuthorizationAmount(const Value: Currency);
    procedure SetAvsResponse(const Value: String);
    procedure SetBatchNumber(const Value: String);
    procedure SetCardholder(const Value: String);
    procedure SetCardNumber(const Value: String);
    procedure SetCardType(const Value: TMWCardType);
    procedure SetCashBackAmount(const Value: Currency);
    procedure SetConvenienceAmount(const Value: Currency);
    procedure SetCustomerId(const Value: String);
    procedure SetCvResponse(const Value: String);
    procedure SetDiscountAmount(const Value: Currency);
    procedure SetEntryMode(const Value: TMWPosEntryType);
    procedure SetErrorMessage(const Value: String);
    procedure SetExpirationDate(const Value: TExpirationDate);
    procedure SetHostReference(const Value: String);
    procedure SetInvoiceNumber(const Value: String);
    procedure SetIsCardNumberKeyed(const Value: Boolean);
    procedure SetIsTransactionAdjusted(const Value: Boolean);
    procedure SetIsTransactionCaptured(const Value: Boolean);
    procedure SetIsTransactionSettled(const Value: Boolean);
    procedure SetIsTransactionVoided(const Value: Boolean);
    procedure SetMerchantTransactionId(const Value: String);
    procedure SetOriginalToken(const Value: String);
    procedure SetPostalCode(const Value: String);
    procedure SetPurchaseOrderNumber(const Value: String);
    procedure SetRegisterNumber(const Value: String);
    procedure SetSurchargeAmount(const Value: Currency);
    procedure SetTaxAmount(const Value: Currency);
    procedure SetTipAmount(const Value: Currency);
    procedure SetToken(const Value: String);
    procedure SetTotalAmount(const Value: Currency);
    procedure SetTransactionDate(const Value: TDateTime);
    procedure SetTransactionType(const Value: TMWTransactionType);
    procedure SetUserName(const Value: String);
    function GetEmvUsed: Boolean;
    procedure SetEmvUsed(const Value: Boolean);

    property ApprovalCode: String read GetApprovalCode write SetApprovalCode;
    property ApprovalCode2: Integer read GetApprovalCode2 write SetApprovalCode2;
    property ApprovalMessage: String read GetApprovalMessage write SetApprovalMessage;
    property ApprovalStatus: TMWApprovalStatusSet read GetApprovalStatus write SetApprovalStatus;
    property AuthorizationAmount: Currency read GetAuthorizationAmount write SetAuthorizationAmount;
    property AvsResponse: String read GetAvsResponse write SetAvsResponse;
    property BatchNumber: String read GetBatchNumber write SetBatchNumber;
    property Cardholder: String read GetCardholder write SetCardholder;
    property CashBackAmount: Currency read GetCashBackAmount write SetCashBackAmount;
    property CardNumber: String read GetCardNumber write SetCardNumber;
    property CardType: TMWCardType read GetCardType write SetCardType;
    property ConvenienceAmount: Currency read GetConvenienceAmount write SetConvenienceAmount;
    property CustomerId: String read GetCustomerId write SetCustomerId;
    property CvResponse: String read GetCvResponse write SetCvResponse;
    property DiscountAmount: Currency read GetDiscountAmount write SetDiscountAmount;
    property EntryMode: TMWPosEntryType read GetEntryMode write SetEntryMode;
    property ErrorMessage: String read GetErrorMessage write SetErrorMessage;
    property ExpirationDate: TExpirationDate read GetExpirationDate write SetExpirationDate;
    property HostReference: String read GetHostReference write SetHostReference;
    property InvoiceNumber: String read GetInvoiceNumber write SetInvoiceNumber;
    property IsCardNumberKeyed: Boolean read GetIsCardNumberKeyed write SetIsCardNumberKeyed;
    property IsTransactionAdjusted: Boolean read GetIsTransactionAdjusted write SetIsTransactionAdjusted;
    property IsTransactionCaptured: Boolean read GetIsTransactionCaptured write SetIsTransactionCaptured;
    property IsTransactionVoided: Boolean read GetIsTransactionVoided write SetIsTransactionVoided;
    property IsTransactionSettled: Boolean read GetIsTransactionSettled write SetIsTransactionSettled;
    property MerchantTransactionId: String read GetMerchantTransactionId write SetMerchantTransactionId;
    property OriginalToken: String read GetOriginalToken write SetOriginalToken;
    property PostalCode: String read GetPostalCode write SetPostalCode;
    property PurchaseOrderNumber: String read GetPurchaseOrderNumber write SetPurchaseOrderNumber;
    property RegisterNumber: String read GetRegisterNumber write SetRegisterNumber;
    property SupportedActions: IMWSupportedActions read GetSupportedActions;
    property SurchargeAmount: Currency read GetSurchargeAmount write SetSurchargeAmount;
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;
    property TipAmount: Currency read GetTipAmount write SetTipAmount;
    property Token: String read GetToken write SetToken;
    property TotalAmount: Currency read GetTotalAmount write SetTotalAmount;
    property TransactionDate: TDateTime read GetTransactionDate write SetTransactionDate;
    property TransactionType: TMWTransactionType read GetTransactionType write SetTransactionType;
    property UserName: String read GetUserName write SetUserName;
    property HealthCareAmountDetails: IMWHealthCareAmountDetails read GetHealthCareAmountDetails;
    property Emv: IMWEmv read GetEmv;
    property EmvUsed: Boolean read GetEmvUsed write SetEmvUsed;
  end;

  TMWEmv = class(TInterfacedObject, IMWEmv)
  private
    FApplicationInformation: IMWEmvApplicationInformation;
    FCardInformation: IMWEmvCardInformation;
    FApplicationCryptogram: IMWEmvApplicationCryptogram;
    FCVMResults: String;
    FIssuerApplicationData: String;
    FTerminalVerificationResults: String;
    FUnpredictableNumber: String;
    FAmount: IMWEmvAmount;
    FPosEntryMode: TMWPosEntryType;
    FTerminalInformation: IMWEmvTerminalInformation;
    FTransactionInformation: IMWEmvTransactionInformation;
    FCryptogramInformationData: String;
    FPINStatement: String;
    FCvmMethod: String;
    FIssuerActionCodeDefault: String;
    FIssuerActionCodeDenial: String;
    FIssuerActionCodeOnline: String;
    FAuthorizationResponseCode: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAmount: IMWEmvAmount;
    function GetApplicationCryptogram: IMWEmvApplicationCryptogram;
    function GetApplicationInformation: IMWEmvApplicationInformation;
    function GetAuthorizationResponseCode: String;
    function GetCardInformation: IMWEmvCardInformation;
    function GetCryptogramInformationData: String;
    function GetCvmMethod: String;
    function GetCVMResults: String;
    function GetIssuerActionCodeDefault: String;
    function GetIssuerActionCodeDenial: String;
    function GetIssuerActionCodeOnline: String;
    function GetIssuerApplicationData: String;
    function GetPINStatement: String;
    function GetPosEntryMode: TMWPosEntryType;
    function GetTerminalInformation: IMWEmvTerminalInformation;
    function GetTerminalVerificationResults: String;
    function GetTransactionInformation: IMWEmvTransactionInformation;
    function GetUnpredictableNumber: String;
    procedure SetAuthorizationResponseCode(const Value: String);
    procedure SetCryptogramInformationData(const Value: String);
    procedure SetCvmMethod(const Value: String);
    procedure SetCVMResults(const Value: String);
    procedure SetIssuerActionCodeDefault(const Value: String);
    procedure SetIssuerActionCodeDenial(const Value: String);
    procedure SetIssuerActionCodeOnline(const Value: String);
    procedure SetIssuerApplicationData(const Value: String);
    procedure SetPINStatement(const Value: String);
    procedure SetPosEntryMode(const Value: TMWPosEntryType);
    procedure SetTerminalVerificationResults(const Value: String);
    procedure SetUnpredictableNumber(const Value: String);

    property ApplicationInformation: IMWEmvApplicationInformation read GetApplicationInformation;
    property CardInformation: IMWEmvCardInformation read GetCardInformation;
    property ApplicationCryptogram: IMWEmvApplicationCryptogram read GetApplicationCryptogram;
    property CVMResults: String read GetCVMResults write SetCVMResults;
    property IssuerApplicationData: String read GetIssuerApplicationData write SetIssuerApplicationData;
    property TerminalVerificationResults: String read GetTerminalVerificationResults write SetTerminalVerificationResults;
    property UnpredictableNumber: String read GetUnpredictableNumber write SetUnpredictableNumber;
    property Amount: IMWEmvAmount read GetAmount;
    property PosEntryMode: TMWPosEntryType read GetPosEntryMode write SetPosEntryMode;
    property TerminalInformation: IMWEmvTerminalInformation read GetTerminalInformation;
    property TransactionInformation: IMWEmvTransactionInformation read GetTransactionInformation;
    property CryptogramInformationData: String read GetCryptogramInformationData write SetCryptogramInformationData;
    property PINStatement: String read GetPINStatement write SetPINStatement;
    property CvmMethod: String read GetCvmMethod write SetCvmMethod;
    property IssuerActionCodeDefault: String read GetIssuerActionCodeDefault write SetIssuerActionCodeDefault;
    property IssuerActionCodeDenial: String read GetIssuerActionCodeDenial write SetIssuerActionCodeDenial;
    property IssuerActionCodeOnline: String read GetIssuerActionCodeOnline write SetIssuerActionCodeOnline;
    property AuthorizationResponseCode: String read GetAuthorizationResponseCode write SetAuthorizationResponseCode;
  end;

  TMWEmvApplicationInformation = class(TInterfacedObject, IMWEmvApplicationInformation)
  private
    FAid: String;
    FApplicationLabel: String;
    FApplicationExpiryDate: TDateTime;
    FApplicationEffectiveDate: TDateTime;
    FApplicationInterchangeProfile: String;
    FApplicationVersionNumber: String;
    FApplicationTransactionCounter: Integer;
    FApplicationUsageControl: String;
    FApplicationPreferredName: String;
    FApplicationDisplayName: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAid: String;
    function GetApplicationDisplayName: String;
    function GetApplicationEffectiveDate: TDateTime;
    function GetApplicationExpiryDate: TDateTime;
    function GetApplicationInterchangeProfile: String;
    function GetApplicationLabel: String;
    function GetApplicationPreferredName: String;
    function GetApplicationTransactionCounter: Integer;
    function GetApplicationUsageControl: String;
    function GetApplicationVersionNumber: String;
    procedure SetAid(const Value: String);
    procedure SetApplicationDisplayName(const Value: String);
    procedure SetApplicationEffectiveDate(const Value: TDateTime);
    procedure SetApplicationExpiryDate(const Value: TDateTime);
    procedure SetApplicationInterchangeProfile(const Value: String);
    procedure SetApplicationLabel(const Value: String);
    procedure SetApplicationPreferredName(const Value: String);
    procedure SetApplicationTransactionCounter(const Value: Integer);
    procedure SetApplicationUsageControl(const Value: String);
    procedure SetApplicationVersionNumber(const Value: String);

    property Aid: String read GetAid write SetAid;
    property ApplicationLabel: String read GetApplicationLabel write SetApplicationLabel;
    property ApplicationExpiryDate: TDateTime read GetApplicationExpiryDate write SetApplicationExpiryDate;
    property ApplicationEffectiveDate: TDateTime read GetApplicationEffectiveDate write SetApplicationEffectiveDate;
    property ApplicationInterchangeProfile: String read GetApplicationInterchangeProfile write SetApplicationInterchangeProfile;
    property ApplicationVersionNumber: String read GetApplicationVersionNumber write SetApplicationVersionNumber;
    property ApplicationTransactionCounter: Integer read GetApplicationTransactionCounter write SetApplicationTransactionCounter;
    property ApplicationUsageControl: String read GetApplicationUsageControl write SetApplicationUsageControl;
    property ApplicationPreferredName: String read GetApplicationPreferredName write SetApplicationPreferredName;
    property ApplicationDisplayName: String read GetApplicationDisplayName write SetApplicationDisplayName;
  end;

  TMWEmvCardInformation = class(TInterfacedObject, IMWEmvCardInformation)
  private
    FMaskedPan: String;
    FPanSequenceNumber: String;
    FCardExpiryDate: TExpirationDate;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetCardExpiryDate: TExpirationDate;
    function GetMaskedPan: String;
    function GetPanSequenceNumber: String;
    procedure SetCardExpiryDate(const Value: TExpirationDate);
    procedure SetMaskedPan(const Value: String);
    procedure SetPanSequenceNumber(const Value: String);

    property MaskedPan: String read GetMaskedPan write SetMaskedPan;
    property PanSequenceNumber: String read GetPanSequenceNumber write SetPanSequenceNumber;
    property CardExpiryDate: TExpirationDate read GetCardExpiryDate write SetCardExpiryDate;
  end;

  TMWEmvApplicationCryptogram = class(TInterfacedObject, IMWEmvApplicationCryptogram)
  private
    FCryptogramType: String;
    FCryptogram: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetCryptogram: String;
    function GetCryptogramType: String;
    procedure SetCryptogram(const Value: String);
    procedure SetCryptogramType(const Value: String);

    property CryptogramType: String read GetCryptogramType write SetCryptogramType;
    property Cryptogram: String read GetCryptogram write SetCryptogram;
  end;

  TMWEmvAmount = class(TInterfacedObject, IMWEmvAmount)
  private
    FAmountAuthorized: Currency;
    FAmountOther: Currency;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAmountAuthorized: Currency;
    function GetAmountOther: Currency;
    procedure SetAmountAuthorized(const Value: Currency);
    procedure SetAmountOther(const Value: Currency);

    property AmountAuthorized: Currency read GetAmountAuthorized write SetAmountAuthorized;
    property AmountOther: Currency read GetAmountOther write SetAmountOther;
  end;

  TMWEmvTerminalInformation = class(TInterfacedObject, IMWEmvTerminalInformation)
  private
    FTerminalType: Integer;
    FIfdSerialNumber: String;
    FTerminalCountryCode: String;
    FTerminalID: String;
    FTerminalActionCodeDefault: String;
    FTerminalActionCodeDenial: String;
    FTerminalActionCodeOnline: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetIfdSerialNumber: String;
    function GetTerminalActionCodeDefault: String;
    function GetTerminalActionCodeDenial: String;
    function GetTerminalActionCodeOnline: String;
    function GetTerminalCountryCode: String;
    function GetTerminalID: String;
    function GetTerminalType: Integer;
    procedure SetIfdSerialNumber(const Value: String);
    procedure SetTerminalActionCodeDefault(const Value: String);
    procedure SetTerminalActionCodeDenial(const Value: String);
    procedure SetTerminalActionCodeOnline(const Value: String);
    procedure SetTerminalCountryCode(const Value: String);
    procedure SetTerminalID(const Value: String);
    procedure SetTerminalType(const Value: Integer);

    property TerminalType: Integer read GetTerminalType write SetTerminalType;
    property IfdSerialNumber: String read GetIfdSerialNumber write SetIfdSerialNumber;
    property TerminalCountryCode: String read GetTerminalCountryCode write SetTerminalCountryCode;
    property TerminalID: String read GetTerminalID write SetTerminalID;
    property TerminalActionCodeDefault: String read GetTerminalActionCodeDefault write SetTerminalActionCodeDefault;
    property TerminalActionCodeDenial: String read GetTerminalActionCodeDenial write SetTerminalActionCodeDenial;
    property TerminalActionCodeOnline: String read GetTerminalActionCodeOnline write SetTerminalActionCodeOnline;
  end;

  TMWEmvTransactionInformation = class(TInterfacedObject, IMWEmvTransactionInformation)
  private
    FTransactionType: TMWTransactionType;
    FTransactionCurrencyCode: Integer;
    FTransactionStatusInformation: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetTransactionCurrencyCode: Integer;
    function GetTransactionStatusInformation: String;
    function GetTransactionType: TMWTransactionType;
    procedure SetTransactionCurrencyCode(const Value: Integer);
    procedure SetTransactionStatusInformation(const Value: String);
    procedure SetTransactionType(const Value: TMWTransactionType);

    property TransactionType: TMWTransactionType read GetTransactionType write SetTransactionType;
    property TransactionCurrencyCode: Integer read GetTransactionCurrencyCode write SetTransactionCurrencyCode;
    property TransactionStatusInformation: String read GetTransactionStatusInformation write SetTransactionStatusInformation;
  end;

  TMWHealthCareAmountDetails = class(TInterfacedObject, IMWHealthCareAmountDetails)
  private
    FHealthCareTotalAmount: Currency;
    FClinicalAmount: Currency;
    FCopayAmount: Currency;
    FDentalAmount: Currency;
    FPrescriptionAmount: Currency;
    FVisionAmount: Currency;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetClinicalAmount: Currency;
    function GetCopayAmount: Currency;
    function GetDentalAmount: Currency;
    function GetHealthCareTotalAmount: Currency;
    function GetPrescriptionAmount: Currency;
    function GetVisionAmount: Currency;
    procedure SetClinicalAmount(const Value: Currency);
    procedure SetCopayAmount(const Value: Currency);
    procedure SetDentalAmount(const Value: Currency);
    procedure SetHealthCareTotalAmount(const Value: Currency);
    procedure SetPrescriptionAmount(const Value: Currency);
    procedure SetVisionAmount(const Value: Currency);

    property HealthCareTotalAmount: Currency read GetHealthCareTotalAmount write SetHealthCareTotalAmount;
    property ClinicalAmount: Currency read GetClinicalAmount write SetClinicalAmount;
    property CopayAmount: Currency read GetCopayAmount write SetCopayAmount;
    property DentalAmount: Currency read GetDentalAmount write SetDentalAmount;
    property PrescriptionAmount: Currency read GetPrescriptionAmount write SetPrescriptionAmount;
    property VisionAmount: Currency read GetVisionAmount write SetVisionAmount;
  end;

  TMWSupportedActions = class(TInterfacedObject, IMWSupportedActions)
  private
    FAdjustmentToken: String;
    FCaptureToken: String;
    FEmailReceiptToken: String;
    FRefundMaxAmount: Currency;
    FRefundToken: String;
    FRepeatSaleToken: String;
    FVoidToken: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAdjustmentToken: String;
    function GetCaptureToken: String;
    function GetEmailReceiptToken: String;
    function GetRefundMaxAmount: Currency;
    function GetRepeatSaleToken: String;
    function GetVoidToken: String;
    procedure SetAdjustmentToken(const Value: String);
    procedure SetCaptureToken(const Value: String);
    procedure SetEmailReceiptToken(const Value: String);
    procedure SetRefundMaxAmount(const Value: Currency);
    procedure SetRepeatSaleToken(const Value: String);
    procedure SetVoidToken(const Value: String);
    function GetRefundToken: String;
    procedure SetRefundToken(const Value: String);

    property AdjustmentToken: String read GetAdjustmentToken write SetAdjustmentToken;
    property CaptureToken: String read GetCaptureToken write SetCaptureToken;
    property EmailReceiptToken: String read GetEmailReceiptToken write SetEmailReceiptToken;
    property RefundMaxAmount: Currency read GetRefundMaxAmount write SetRefundMaxAmount;
    property RefundToken: String read GetRefundToken write SetRefundToken;
    property RepeatSaleToken: String read GetRepeatSaleToken write SetRepeatSaleToken;
    property VoidToken: String read GetVoidToken write SetVoidToken;
  end;

  TMWTransactionReference4List = class(TInterfacedObject, IMWTransactionReference4List)
  private
    FItems: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetItem(Index: Integer): IMWTransactionReference4;

    function Count: Integer;
    function Add: IMWTransactionReference4;
    procedure Delete(const Index: Integer);
    procedure Clear;
    property Items[Index: Integer]: IMWTransactionReference4 read GetItem; default;
  end;

  TMWTransactionReference4 = class(TInterfacedObject, IMWTransactionReference4)
  private
    FToken: String;
    FInvoiceNumber: String;
    FTransactionDate: TDateTime;
    FApprovalStatus: TMWApprovalStatusSet;
    FApprovalCode: Integer;
    FApprovalMessage: String;
    FCardholder: String;
    FAmount: Currency;
    FTransactionType: TMWTransactionType;
    FCardNumber: String;
    FCardType: TMWCardType;
    FAuthorizationCode: String;
    FMerchantTransactionId: String;
    FBatchNumber: String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAmount: Currency;
    function GetApprovalCode: Integer;
    function GetApprovalMessage: String;
    function GetApprovalStatus: TMWApprovalStatusSet;
    function GetAuthorizationCode: String;
    function GetBatchNumber: String;
    function GetCardholder: String;
    function GetCardNumber: String;
    function GetCardType: TMWCardType;
    function GetInvoiceNumber: String;
    function GetMerchantTransactionId: String;
    function GetToken: String;
    function GetTransactionDate: TDateTime;
    function GetTransactionType: TMWTransactionType;
    procedure SetAmount(const Value: Currency);
    procedure SetApprovalCode(const Value: Integer);
    procedure SetApprovalMessage(const Value: String);
    procedure SetApprovalStatus(const Value: TMWApprovalStatusSet);
    procedure SetAuthorizationCode(const Value: String);
    procedure SetBatchNumber(const Value: String);
    procedure SetCardholder(const Value: String);
    procedure SetCardNumber(const Value: String);
    procedure SetCardType(const Value: TMWCardType);
    procedure SetInvoiceNumber(const Value: String);
    procedure SetMerchantTransactionId(const Value: String);
    procedure SetToken(const Value: String);
    procedure SetTransactionDate(const Value: TDateTime);
    procedure SetTransactionType(const Value: TMWTransactionType);

    property Token: String read GetToken write SetToken;
    property InvoiceNumber: String read GetInvoiceNumber write SetInvoiceNumber;
    property TransactionDate: TDateTime read GetTransactionDate write SetTransactionDate;
    property ApprovalStatus: TMWApprovalStatusSet read GetApprovalStatus write SetApprovalStatus;
    property ApprovalCode: Integer read GetApprovalCode write SetApprovalCode;
    property ApprovalMessage: String read GetApprovalMessage write SetApprovalMessage;
    property Cardholder: String read GetCardholder write SetCardholder;
    property Amount: Currency read GetAmount write SetAmount;
    property TransactionType: TMWTransactionType read GetTransactionType write SetTransactionType;
    property CardNumber: String read GetCardNumber write SetCardNumber;
    property CardType: TMWCardType read GetCardType write SetCardType;
    property AuthorizationCode: String read GetAuthorizationCode write SetAuthorizationCode;
    property MerchantTransactionId: String read GetMerchantTransactionId write SetMerchantTransactionId;
    property BatchNumber: String read GetBatchNumber write SetBatchNumber;
  end;

  TMWTransactionSummary4List = class(TInterfacedObject, IMWTransactionSummary4List)
  private
    FItems: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetItem(Index: Integer): IMWTransactionSummary4;

    function Count: Integer;
    function Add: IMWTransactionSummary4;
    procedure Delete(const Index: Integer);
    procedure Clear;
    property Items[Index: Integer]: IMWTransactionSummary4 read GetItem; default;
  end;

  TMWTransactionSummary4 = class(TInterfacedObject, IMWTransactionSummary4)
  private
    FCardholder: String;
    FSaleAmount: Currency;
    FSaleCount: Integer;
    FRefundAmount: Currency;
    FRefundCount: Integer;
    FNetAmount: Currency;
    FTotalTransactions: Integer;
    FCardType: TMWCardType;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetCardholder: String;
    function GetCardType: TMWCardType;
    function GetNetAmount: Currency;
    function GetRefundAmount: Currency;
    function GetRefundCount: Integer;
    function GetSaleAmount: Currency;
    function GetSaleCount: Integer;
    function GetTotalTransactions: Integer;
    procedure SetCardholder(const Value: String);
    procedure SetCardType(const Value: TMWCardType);
    procedure SetNetAmount(const Value: Currency);
    procedure SetRefundAmount(const Value: Currency);
    procedure SetRefundCount(const Value: Integer);
    procedure SetSaleAmount(const Value: Currency);
    procedure SetSaleCount(const Value: Integer);
    procedure SetTotalTransactions(const Value: Integer);

    property Cardholder: String read GetCardholder write SetCardholder;
    property SaleAmount: Currency read GetSaleAmount write SetSaleAmount;
    property SaleCount: Integer read GetSaleCount write SetSaleCount;
    property RefundAmount: Currency read GetRefundAmount write SetRefundAmount;
    property RefundCount: Integer read GetRefundCount write SetRefundCount;
    property NetAmount: Currency read GetNetAmount write SetNetAmount;
    property TotalTransactions: Integer read GetTotalTransactions write SetTotalTransactions;
    property CardType: TMWCardType read GetCardType write SetCardType;
  end;



  TMWTransactionList = class(TInterfacedObject, IMWTransactionList)
  private
    FOwner: TMerchantWare;
    FItems: TInterfaceList;
    //FErrorCode: Integer;
    FErrorMessage: String;
    function GetErrorMessage: String;
    function Add: IMWTransactionItem; overload;
    procedure Delete(const Index: Integer);
    procedure Clear;
  public
    constructor Create(AOwner: TMerchantWare);
    destructor Destroy; override;
  public
    function GetItem(Index: Integer): IMWTransactionItem;

    function Search(const DateFrom, DateTo: TDateTime;
      const InvoiceNum, RegisterNum, AuthCode: String): Boolean;
    function Count: Integer;
    function Add(const Token: String): IMWTransactionItem; overload;

    property Items[Index: Integer]: IMWTransactionItem read GetItem; default;
    property ErrorMessage: String read GetErrorMessage;
  end;

  TMWTransactionItem = class(TInterfacedObject, IMWTransactionItem)
  private
    FOwner: TMWTransactionList;
  public
    constructor Create(AOwner: TMWTransactionList);
    destructor Destroy; override;
  public

  end;

implementation

uses
  StrUtils, System.IOUtils;

procedure ParseApprovalStatus(Val: String; var Stat: TMWApprovalStatusSet;
  var Code: Integer; var Msg: String);
var
  T: String;
begin
  //TMWApprovalStatus = (asApproved, asDeclined, asDuplicate, asReferral, asFailed);
  //DECLINED;1023;invalid account num
  Val:= Val + ';';
  T:= Copy(Val, 1, Pos(';', Val)-1);
  Delete(Val, 1, Pos(';', Val));
  Stat:= [];
  if ContainsText(T, 'APPROVED') then
    Stat:= Stat + [asApproved];
  if ContainsText(T, 'DECLINED') then
    Stat:= Stat + [asDeclined];
  if ContainsText(T, 'REFERRAL') then
    Stat:= Stat + [asReferral];
  if ContainsText(T, 'DUPLICATE') then
    Stat:= Stat + [asDuplicate];
  if ContainsText(T, 'FAILED') then
    Stat:= Stat + [asFailed];
  T:= Copy(Val, 1, Pos(';', Val)-1);
  Delete(Val, 1, Pos(';', Val));
  Code:= StrToIntDef(T, 0);
  Msg:= Copy(Val, 1, Length(Val)-1);;
end;

function StrToCurrDef(Value: String; const Def: Currency): Currency;
begin
  Value:= StringReplace(Value, ',', '', [rfReplaceAll]);
  Result:= System.SysUtils.StrToCurrDef(Value, Def);
end;

{ TMerchantWare }

constructor TMerchantWare.Create;
begin
  FCredit:= TMWCreditTransactions.Create(Self);
  FCredit._AddRef;
  FVault:= TMWVaultTransactions.Create(Self);
  FVault._AddRef;
  FEbt:= TMWEbtTransactions.Create(Self);
  FEbt._AddRef;
  FGiftCard:= TMWGiftCardTransactions.Create(Self);
  FGiftCard._AddRef;
  FLevelUp:= TMWLevelUpTransactions.Create(Self);
  FLevelUp._AddRef;
  FCheck:= TMWCheckTransactions.Create(Self);
  FCheck._AddRef;
  FReport:= TMWReportTransactions.Create(Self);
  FReport._AddRef;
end;

destructor TMerchantWare.Destroy;
begin
  FReport._Release;
  FCheck._Release;
  FLevelUp._Release;
  FGiftCard._Release;
  FEbt._Release;
  FVault._Release;
  FCredit._Release;
  inherited;
end;

function TMerchantWare.GetTestMode: Boolean;
begin
  Result:= FTestMode;
end;

function TMerchantWare.GetVault: IMWVaultTransactions;
begin
  Result:= FVault;
end;

function TMerchantWare.GetName: String;
begin
  Result:= FName;
end;

function TMerchantWare.GetSiteId: String;
begin
  Result:= FSiteId;
end;

function TMerchantWare.GetKey: String;
begin
  Result:= FKey;
end;

procedure TMerchantWare.SetName(const Value: String);
begin
  FName:= Value;
end;

procedure TMerchantWare.SetSiteId(const Value: String);
begin
  FSiteId:= Value;
end;

procedure TMerchantWare.SetKey(const Value: String);
begin
  FKey:= Value;
end;

procedure TMerchantWare.SetTestMode(const Value: Boolean);
begin
  FTestMode:= Value;
end;

function TMerchantWare.GetCredit: IMWCreditTransactions;
begin
  Result:= FCredit;
end;

function TMerchantWare.GetEbt: IMWEbtTransactions;
begin
  Result:= FEbt;
end;

function TMerchantWare.GetGiftCard: IMWGiftCardTransactions;
begin
  Result:= FGiftCard;
end;

function TMerchantWare.GetLevelUp: IMWLevelUpTransactions;
begin
  Result:= FLevelUp;
end;

function TMerchantWare.GetCheck: IMWCheckTransactions;
begin
  Result:= FCheck;
end;

function TMerchantWare.GetReport: IMWReportTransactions;
begin
  Result:= FReport;
end;

function TMerchantWare.GetCredentials: String;
  procedure A(const S: String);
  begin
    Result:= Result + S + sLineBreak;
  end;
begin
  A(XmlVal('merchantName', FName));
  A(XmlVal('merchantSiteId', FSiteId));
  A(XmlVal('merchantKey', FKey));
end;

function TMerchantWare.SendTransportRequest(const Endpoint, ActionUrl, Action,
  AXML: String): IXMLDocument;
const
  XML_URL = '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">';
var
  Str: TStringStream;
  Http: TIdHTTP;
  OpenSSL: TIdSSLIOHandlerSocketOpenSSL;
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
    A(GetCredentials);
    A(AXML                  );
    A('</'+Action+'>' +     sLineBreak);
    A('</soap:Body>' +      sLineBreak);
    A('</soap:Envelope>' +  sLineBreak);

    Http:= TIdHTTP.Create(nil);
    OpenSSL:= TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      Http.IOHandler:= OpenSSL;
      Http.HandleRedirects:= True;
      Http.Request.ContentType:= 'text/xml';
      Http.Request.CharSet:= 'utf-8';
      Str.Position:= 0;

      Xml:= Http.Post(Endpoint, Str);
      Result.LoadFromXML(Xml);
    finally
      FreeAndNil(OpenSSL);
      FreeAndNil(Http);
    end;

  finally
    FreeAndNil(Str);
  end;
end;

//Credit Functions

function TMerchantWare.DoRequestBatchResponse4(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWBatchResponse4;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
begin
  Result:= TMWBatchResponse4.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      Result.BatchStatus:= [bsFailed];
      //Result.ApprovalCode:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      Result.BatchStatus:= [bsFailed];
      //Result.ApprovalCode:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    Result.AuthorizationCode:=  GetNodeValue(ResponseNode, 'AuthorizationCode');
    Result.BatchAmount:=        StrToCurrDef(GetNodeValue(ResponseNode, 'BatchAmount'), 0);
    Result.BatchStatus:=        MWStrToBatchStatusSet(GetNodeValue(ResponseNode, 'BatchStatus'));
    Result.ErrorMessage:=       GetNodeValue(ResponseNode, 'ErrorMessage');
    Result.ExtraData:=          GetNodeValue(ResponseNode, 'ExtraData');
    Result.Token:=              GetNodeValue(ResponseNode, 'Token');
    Result.TransactionCount:=   StrToIntDef(GetNodeValue(ResponseNode, 'TransactionCount'), 0);
    Result.TransactionDate:=    StrToDateTimeDef(GetNodeValue(ResponseNode, 'TransactionDate'), 0);
  except
    on E: Exception do begin
      Result.BatchStatus:= [bsFailed];
      //Result.ApprovalCode:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

function TMerchantWare.DoRequestCreditLevel2Response4(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWCreditLevel2Response4;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
  Stat: TMWApprovalStatusSet;
  Code: Integer;
  Msg: String;
begin
  Result:= TMWCreditLevel2Response4.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    ParseApprovalStatus(GetNodeValue(ResponseNode, 'ApprovalStatus'), Stat, Code, Msg);
    Result.Amount:=               StrToCurrDef(GetNodeValue(ResponseNode, 'Amount'), 0);
    Result.ApprovalStatus:=       Stat;
    Result.ApprovalCode:=         Code;
    Result.ApprovalMessage:=      Msg;
    Result.AuthorizationCode:=    GetNodeValue(ResponseNode, 'AuthorizationCode');
    Result.AvsResponse:=          GetNodeValue(ResponseNode, 'AvsResponse');
    Result.Cardholder:=           GetNodeValue(ResponseNode, 'Cardholder');
    Result.CardNumber:=           GetNodeValue(ResponseNode, 'CardNumber');
    Result.CardType:=             MWStrToCardType(GetNodeValue(ResponseNode, 'CardType'));
    Result.CustomerCode:=         GetNodeValue(ResponseNode, 'CustomerCode');
    Result.CvResponse:=           GetNodeValue(ResponseNode, 'CvResponse');
    Result.EntryMode:=            MWStrToPosEntryType(GetNodeValue(ResponseNode, 'EntryMode'));
    Result.ErrorMessage:=         GetNodeValue(ResponseNode, 'ErrorMessage');
    Result.ExtraData:=            GetNodeValue(ResponseNode, 'ExtraData');
    Result.InvoiceNumber:=        GetNodeValue(ResponseNode, 'InvoiceNumber');
    Result.PurchaseOrderNumber:=  GetNodeValue(ResponseNode, 'PurchaseOrderNumber');
    Result.TaxAmount:=            StrToCurrDef(GetNodeValue(ResponseNode, 'TaxAmount'), 0);
    Result.Token:=                GetNodeValue(ResponseNode, 'Token');
    Result.TransactionDate:=      StrToDateTimeDef(GetNodeValue(ResponseNode, 'TransactionDate'), 0);
    Result.TransactionType:=      MWStrToTransactionType(GetNodeValue(ResponseNode, 'TransactionType'));
  except
    on E: Exception do begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

function TMerchantWare.DoRequestCreditResponse4(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWCreditResponse4;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
  Stat: TMWApprovalStatusSet;
  Code: Integer;
  Msg: String;
begin
  Result:= TMWCreditResponse4.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    //ResponseNode:= GetSoapNodeRoot(Response, AReqName);
    if (not Assigned(ResponseNode)) then begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    ParseApprovalStatus(GetNodeValue(ResponseNode, 'ApprovalStatus'), Stat, Code, Msg);
    Result.Amount:=               StrToCurrDef(GetNodeValue(ResponseNode, 'Amount'), 0);
    Result.ApprovalStatus:=       Stat;
    Result.ApprovalCode:=         Code;
    Result.ApprovalMessage:=      Msg;
    Result.AuthorizationCode:=    GetNodeValue(ResponseNode, 'AuthorizationCode');
    Result.AvsResponse:=          GetNodeValue(ResponseNode, 'AvsResponse');
    Result.Cardholder:=           GetNodeValue(ResponseNode, 'Cardholder');
    Result.CardNumber:=           GetNodeValue(ResponseNode, 'CardNumber');
    Result.CardType:=             MWStrToCardType(GetNodeValue(ResponseNode, 'CardType'));
    Result.CvResponse:=           GetNodeValue(ResponseNode, 'CvResponse');
    Result.EntryMode:=            MWStrToPosEntryType(GetNodeValue(ResponseNode, 'EntryMode'));
    Result.ErrorMessage:=         GetNodeValue(ResponseNode, 'ErrorMessage');
    Result.ExtraData:=            GetNodeValue(ResponseNode, 'ExtraData');
    Result.InvoiceNumber:=        GetNodeValue(ResponseNode, 'InvoiceNumber');
    Result.Token:=                GetNodeValue(ResponseNode, 'Token');
    Result.TransactionDate:=      StrToDateTimeDef(GetNodeValue(ResponseNode, 'TransactionDate'), 0);
    Result.TransactionType:=      MWStrToTransactionType(GetNodeValue(ResponseNode, 'TransactionType'));
  except
    on E: Exception do begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

function TMerchantWare.DoRequestEmvAuthorizeResponse4(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWEmvAuthorizeResponse4;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
begin
  Result:= TMWEmvAuthorizeResponse4.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      Result.ApprovalStatus:= [easFailed];
      //Result.ApprovalCode:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      Result.ApprovalStatus:= [easFailed];
      //Result.ApprovalCode:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    Result.ApprovalStatus:=   MWStrToEmvApprovalStatusSet(GetNodeValue(ResponseNode, 'ApprovalStatus'));
    Result.ErrorMessage:=     GetNodeValue(ResponseNode, 'ErrorMessage');
    Result.Token:=            GetNodeValue(ResponseNode, 'Token');
    Result.TransactionDate:=  StrToDateTimeDef(GetNodeValue(ResponseNode, 'TransactionDate'), 0);
    Result.TlvData:=          GetNodeValue(ResponseNode, 'TlvData');
    Result.CashbackAmount:=   StrToCurrDef(GetNodeValue(ResponseNode, 'CashbackAmount'), 0);
    Result.DonationAmount:=   StrToCurrDef(GetNodeValue(ResponseNode, 'DonationAmount'), 0);
    Result.UserTipAmount:=    StrToCurrDef(GetNodeValue(ResponseNode, 'UserTipAmount'), 0);
    Result.SurchargeAmount:=  StrToCurrDef(GetNodeValue(ResponseNode, 'SurchargeAmount'), 0);
  except
    on E: Exception do begin
      Result.ApprovalStatus:= [easFailed];
      //Result.ApprovalCode:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

function TMerchantWare.DoRequestEmvCompleteResponse4(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWEmvCompleteResponse4;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
begin
  Result:= TMWEmvCompleteResponse4.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      Result.ApprovalStatus:= [easFailed];
      //Result.ApprovalCode:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      Result.ApprovalStatus:= [easFailed];
      //Result.ApprovalCode:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    Result.ApprovalStatus:=   MWStrToEmvApprovalStatusSet(GetNodeValue(ResponseNode, 'ApprovalStatus'));
    Result.ErrorMessage:=     GetNodeValue(ResponseNode, 'ErrorMessage');
  except
    on E: Exception do begin
      Result.ApprovalStatus:= [easFailed];
      //Result.ApprovalCode:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

function TMerchantWare.DoRequestSignatureResponse4(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWSignatureResponse4;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
begin
  Result:= TMWSignatureResponse4.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      //Result.ApprovalStatus:= [asFailed];
      //Result.ApprovalCode:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      //Result.ApprovalStatus:= [asFailed];
      //Result.ApprovalCode:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    Result.ErrorMessage:=       GetNodeValue(ResponseNode, 'ErrorMessage');
    Result.Token:=              GetNodeValue(ResponseNode, 'Token');
    Result.SignatureType:=      MWStrToSignatureType(GetNodeValue(ResponseNode, 'SignatureType'));
    Result.TransactionDate:=         StrToDateTimeDef(GetNodeValue(ResponseNode, 'TransactionDate'), 0);
    Result.UploadStatus:=       MWStrToSignatureStatus(GetNodeValue(ResponseNode, 'UploadStatus'));
  except
    on E: Exception do begin
      //Result.ApprovalStatus:= [asFailed];
      //Result.ApprovalCode:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

function TMerchantWare.DoRequestVaultBoardingResponse(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWVaultBoardingResponse;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
begin
  Result:= TMWVaultBoardingResponse.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      //Result.ApprovalStatus:= [asFailed];
      Result.ErrorCode:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      //Result.ApprovalStatus:= [asFailed];
      Result.ErrorCode:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    Result.VaultToken:=       GetNodeValue(ResponseNode, 'VaultToken');
    Result.ErrorCode:=        StrToIntDef(GetNodeValue(ResponseNode, 'ErrorCode'), 0);
    Result.ErrorMessage:=     GetNodeValue(ResponseNode, 'ErrorMessage');
  except
    on E: Exception do begin
      //Result.ApprovalStatus:= [asFailed];
      Result.ErrorCode:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

function TMerchantWare.DoRequestVaultPaymentInfoResponse(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWVaultPaymentInfoResponse;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
begin
  Result:= TMWVaultPaymentInfoResponse.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      //Result.ApprovalStatus:= [asFailed];
      Result.ErrorCode:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      //Result.ApprovalStatus:= [asFailed];
      Result.ErrorCode:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    Result.Cardholder:=     GetNodeValue(ResponseNode, 'Cardholder');
    Result.CardNumber:=     GetNodeValue(ResponseNode, 'CardNumber');
    Result.CardType:=       MWStrToCardType(GetNodeValue(ResponseNode, 'CardType'));
    Result.ErrorCode:=      StrToIntDef(GetNodeValue(ResponseNode, 'ErrorCode'), 0);
    Result.ErrorMessage:=   GetNodeValue(ResponseNode, 'ErrorMessage');
  except
    on E: Exception do begin
      //Result.ApprovalStatus:= [asFailed];
      Result.ErrorCode:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

//Gift Card Functions

function TMerchantWare.DoRequestGiftResponse4(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWGiftResponse4;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
  Stat: TMWApprovalStatusSet;
  Code: Integer;
  Msg: String;
begin
  Result:= TMWGiftResponse4.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    ParseApprovalStatus(GetNodeValue(ResponseNode, 'ApprovalStatus'), Stat, Code, Msg);
    Result.Amount:=           StrToCurrDef(GetNodeValue(ResponseNode, 'Amount'), 0);
    Result.ApprovalStatus:=   Stat;
    Result.ApprovalCode:=     Code;
    Result.ApprovalMessage:=  Msg;
    Result.CardBalance:=      StrToCurrDef(GetNodeValue(ResponseNode, 'CardBalance'), 0);
    Result.Cardholder:=       GetNodeValue(ResponseNode, 'Cardholder');
    Result.CardNumber:=       GetNodeValue(ResponseNode, 'CardNumber');
    Result.EntryMode:=        MWStrToPosEntryType(GetNodeValue(ResponseNode, 'EntryMode'));
    Result.ErrorMessage:=     GetNodeValue(ResponseNode, 'ErrorMessage');
    Result.ExpirationDate:=   GetNodeValue(ResponseNode, 'ExpirationDate');
    Result.ExtraData:=        GetNodeValue(ResponseNode, 'ExtraData');
    Result.MiscMessage:=      GetNodeValue(ResponseNode, 'MiscMessage');
    Result.ResponseMessage:=  GetNodeValue(ResponseNode, 'ResponseMessage');
    Result.Token:=            GetNodeValue(ResponseNode, 'Token');
    Result.TransactionDate:=  StrToDateTimeDef(GetNodeValue(ResponseNode, 'TransactionDate'), 0);
    Result.TransactionID:=    GetNodeValue(ResponseNode, 'TransactionID');
    Result.TransactionType:=  MWStrToTransactionType(GetNodeValue(ResponseNode, 'TransactionType'));
  except
    on E: Exception do begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

function TMerchantWare.DoRequestGiftLoyaltyResponse4(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWGiftLoyaltyResponse4;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
  Stat: TMWApprovalStatusSet;
  Code: Integer;
  Msg: String;
begin
  Result:= TMWGiftLoyaltyResponse4.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    ParseApprovalStatus(GetNodeValue(ResponseNode, 'ApprovalStatus'), Stat, Code, Msg);
    Result.ApprovalStatus:=     Stat;
    Result.ApprovalCode:=       Code;
    Result.ApprovalMessage:=    Msg;
    Result.CardBalance:=        StrToCurrDef(GetNodeValue(ResponseNode, 'CardBalance'), 0);
    Result.Cardholder:=         GetNodeValue(ResponseNode, 'Cardholder');
    Result.CardNumber:=         GetNodeValue(ResponseNode, 'CardNumber');
    Result.EntryMode:=          MWStrToPosEntryType(GetNodeValue(ResponseNode, 'EntryMode'));
    Result.ErrorMessage:=       GetNodeValue(ResponseNode, 'ErrorMessage');
    Result.ExpirationDate:=     GetNodeValue(ResponseNode, 'ExpirationDate');
    Result.ExtraData:=          GetNodeValue(ResponseNode, 'ExtraData');
    Result.LifePointsBalance:=  StrToIntDef(GetNodeValue(ResponseNode, 'LifePointsBalance'), 0);
    Result.MiscMessage:=        GetNodeValue(ResponseNode, 'MiscMessage');
    Result.PointsBalance:=      StrToIntDef(GetNodeValue(ResponseNode, 'PointsBalance'), 0);
    Result.ResponseMessage:=    GetNodeValue(ResponseNode, 'ResponseMessage');
    Result.Token:=              GetNodeValue(ResponseNode, 'Token');
    Result.TransactionID:=      GetNodeValue(ResponseNode, 'TransactionID');
    Result.TransactionDate:=    StrToDateTimeDef(GetNodeValue(ResponseNode, 'TransactionDate'), 0);
    Result.TransactionType:=    MWStrToTransactionType(GetNodeValue(ResponseNode, 'TransactionType'));
  except
    on E: Exception do begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

//Report Functions

function TMerchantWare.DoRequestDetailedTransactionReference4(const AXML,
  ASvcPath, AEndpointPath,
  AReqName: String): IMWDetailedTransactionReference4;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
  N2, N3: IXMLNode;
  X, Y: Integer;
  Stat: TMWApprovalStatusSet;
  Code: Integer;
  Msg: String;
begin
  //This one will be HUGE
  Result:= TMWDetailedTransactionReference4.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode2:= 99600;
      Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode2:= 99500;
      Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    ParseApprovalStatus(GetNodeValue(ResponseNode, 'ApprovalStatus'), Stat, Code, Msg);
    Result.ApprovalCode:=           GetNodeValue(ResponseNode, 'ApprovalCode');
    Result.ApprovalStatus:=         Stat;
    Result.ApprovalCode2:=          Code;
    Result.ApprovalMessage:=        Msg;
    Result.AuthorizationAmount:=    StrToCurrDef(GetNodeValue(ResponseNode, 'AuthorizationAmount'), 0);
    Result.AvsResponse:=            GetNodeValue(ResponseNode, 'AvsResponse');
    Result.BatchNumber:=            GetNodeValue(ResponseNode, 'BatchNumber');
    Result.Cardholder:=             GetNodeValue(ResponseNode, 'Cardholder');
    Result.CashBackAmount:=         StrToCurrDef(GetNodeValue(ResponseNode, 'CashBackAmount'), 0);
    Result.CardNumber:=             GetNodeValue(ResponseNode, 'CardNumber');
    Result.CardType:=               MWStrToCardType(GetNodeValue(ResponseNode, 'CardType'));
    Result.ConvenienceAmount:=      StrToCurrDef(GetNodeValue(ResponseNode, 'ConvenienceAmount'), 0);
    Result.CustomerId:=             GetNodeValue(ResponseNode, 'CustomerId');
    Result.CvResponse:=             GetNodeValue(ResponseNode, 'CvResponse');
    Result.DiscountAmount:=         StrToCurrDef(GetNodeValue(ResponseNode, 'DiscountAmount'), 0);
    Result.EntryMode:=              MWStrToPosEntryType(GetNodeValue(ResponseNode, 'EntryMode'));
    Result.ErrorMessage:=           GetNodeValue(ResponseNode, 'ErrorMessage');
    Result.ExpirationDate:=         GetNodeValue(ResponseNode, 'ExpirationDate');
    Result.HostReference:=          GetNodeValue(ResponseNode, 'HostReference');
    Result.InvoiceNumber:=          GetNodeValue(ResponseNode, 'InvoiceNumber');
    Result.IsCardNumberKeyed:=      GetNodeValue(ResponseNode, 'IsCardNumberKeyed') = 'true';
    Result.IsTransactionAdjusted:=  GetNodeValue(ResponseNode, 'IsTransactionAdjusted') = 'true';
    Result.IsTransactionCaptured:=  GetNodeValue(ResponseNode, 'IsTransactionCaptured') = 'true';
    Result.IsTransactionVoided:=    GetNodeValue(ResponseNode, 'IsTransactionVoided') = 'true';
    Result.IsTransactionSettled:=   GetNodeValue(ResponseNode, 'IsTransactionSettled') = 'true';
    Result.MerchantTransactionId:=  GetNodeValue(ResponseNode, 'MerchantTransactionId');
    Result.OriginalToken:=          GetNodeValue(ResponseNode, 'OriginalToken');
    Result.PostalCode:=             GetNodeValue(ResponseNode, 'PostalCode');
    Result.PurchaseOrderNumber:=    GetNodeValue(ResponseNode, 'PurchaseOrderNumber');
    Result.RegisterNumber:=         GetNodeValue(ResponseNode, 'RegisterNumber');
    Result.SurchargeAmount:=        StrToCurrDef(GetNodeValue(ResponseNode, 'SurchargeAmount'), 0);
    Result.TaxAmount:=              StrToCurrDef(GetNodeValue(ResponseNode, 'TaxAmount'), 0);
    Result.TipAmount:=              StrToCurrDef(GetNodeValue(ResponseNode, 'TipAmount'), 0);
    Result.Token:=                  GetNodeValue(ResponseNode, 'Token');
    Result.TotalAmount:=            StrToCurrDef(GetNodeValue(ResponseNode, 'TotalAmount'), 0);
    Result.TransactionDate:=        StrToDateTimeDef(GetNodeValue(ResponseNode, 'TransactionDate'), 0);
    Result.TransactionType:=        MWStrToTransactionType(GetNodeValue(ResponseNode, 'TransactionType'));
    Result.UserName:=               GetNodeValue(ResponseNode, 'UserName');

    for X := 0 to ResponseNode.ChildNodes.Count-1 do begin
      N2:= ResponseNode.ChildNodes.Nodes[X];
      if N2.NodeName = 'SupportedActions' then begin
        Result.SupportedActions.AdjustmentToken:=   GetNodeValue(N2, 'AdjustmentToken');
        Result.SupportedActions.CaptureToken:=      GetNodeValue(N2, 'CaptureToken');
        Result.SupportedActions.EmailReceiptToken:= GetNodeValue(N2, 'EmailReceiptToken');
        Result.SupportedActions.RefundMaxAmount:=   StrToCurrDef(GetNodeValue(N2, 'RefundMaxAmount'), 0);
        Result.SupportedActions.RefundToken:=       GetNodeValue(N2, 'RefundToken');
        Result.SupportedActions.RepeatSaleToken:=   GetNodeValue(N2, 'RepeatSaleToken');
        Result.SupportedActions.VoidToken:=         GetNodeValue(N2, 'VoidToken');
      end else
      if N2.NodeName = 'HealthCareAmountDetails' then begin
        Result.HealthCareAmountDetails.HealthCareTotalAmount:=  StrToCurrDef(GetNodeValue(N2, 'HealthCareTotalAmount'), 0);
        Result.HealthCareAmountDetails.ClinicalAmount:=         StrToCurrDef(GetNodeValue(N2, 'ClinicalAmount'), 0);
        Result.HealthCareAmountDetails.CopayAmount:=            StrToCurrDef(GetNodeValue(N2, 'CopayAmount'), 0);
        Result.HealthCareAmountDetails.DentalAmount:=           StrToCurrDef(GetNodeValue(N2, 'DentalAmount'), 0);
        Result.HealthCareAmountDetails.PrescriptionAmount:=     StrToCurrDef(GetNodeValue(N2, 'PrescriptionAmount'), 0);
        Result.HealthCareAmountDetails.VisionAmount:=           StrToCurrDef(GetNodeValue(N2, 'VisionAmount'), 0);
      end else
      if N2.NodeName = 'Emv' then begin
        if N2.ChildNodes.Count > 0 then begin
          Result.EmvUsed:= True;
          Result.Emv.CVMResults:=                   GetNodeValue(N2, 'CVMResults');
          Result.Emv.IssuerApplicationData:=        GetNodeValue(N2, 'IssuerApplicationData');
          Result.Emv.TerminalVerificationResults:=  GetNodeValue(N2, 'TerminalVerificationResults');
          Result.Emv.UnpredictableNumber:=          GetNodeValue(N2, 'UnpredictableNumber');
          Result.Emv.PosEntryMode:=                 MWStrToPosEntryType(GetNodeValue(N2, 'PosEntryMode'));
          Result.Emv.CryptogramInformationData:=    GetNodeValue(N2, 'CryptogramInformationData');
          Result.Emv.PINStatement:=                 GetNodeValue(N2, 'PINStatement');
          Result.Emv.CvmMethod:=                    GetNodeValue(N2, 'CvmMethod');
          Result.Emv.IssuerApplicationData:=        GetNodeValue(N2, 'IssuerApplicationData');
          Result.Emv.IssuerActionCodeDefault:=      GetNodeValue(N2, 'IssuerActionCodeDefault');
          Result.Emv.IssuerActionCodeDenial:=       GetNodeValue(N2, 'IssuerActionCodeDenial');
          Result.Emv.IssuerActionCodeOnline:=       GetNodeValue(N2, 'IssuerActionCodeOnline');
          Result.Emv.AuthorizationResponseCode:=    GetNodeValue(N2, 'AuthorizationResponseCode');
          for Y := 0 to N2.ChildNodes.Count-1 do begin
            N3:= N2.ChildNodes.Nodes[Y];
            if N3.NodeName = 'ApplicationInformation' then begin
              Result.Emv.ApplicationInformation.Aid:=                           GetNodeValue(N3, 'Aid');
              Result.Emv.ApplicationInformation.ApplicationLabel:=              GetNodeValue(N3, 'ApplicationLabel');
              Result.Emv.ApplicationInformation.ApplicationExpiryDate:=         StrToDateTimeDef(GetNodeValue(N3, 'ApplicationExpiryDate'), 0);
              Result.Emv.ApplicationInformation.ApplicationEffectiveDate:=      StrToDateTimeDef(GetNodeValue(N3, 'ApplicationEffectiveDate'), 0);
              Result.Emv.ApplicationInformation.ApplicationInterchangeProfile:= GetNodeValue(N3, 'ApplicationInterchangeProfile');
              Result.Emv.ApplicationInformation.ApplicationVersionNumber:=      GetNodeValue(N3, 'ApplicationVersionNumber');
              Result.Emv.ApplicationInformation.ApplicationTransactionCounter:= StrToIntDef(GetNodeValue(N3, 'ApplicationTransactionCounter'), 0);
              Result.Emv.ApplicationInformation.ApplicationUsageControl:=       GetNodeValue(N3, 'ApplicationUsageControl');
              Result.Emv.ApplicationInformation.ApplicationPreferredName:=      GetNodeValue(N3, 'ApplicationPreferredName');
              Result.Emv.ApplicationInformation.ApplicationDisplayName:=        GetNodeValue(N3, 'ApplicationDisplayName');
            end else
            if N3.NodeName = 'CardInformation' then begin
              Result.Emv.CardInformation.MaskedPan:=          GetNodeValue(N3, 'MaskedPan');
              Result.Emv.CardInformation.PanSequenceNumber:=  GetNodeValue(N3, 'PanSequenceNumber');
              Result.Emv.CardInformation.CardExpiryDate:=     GetNodeValue(N3, 'CardExpiryDate');
            end else
            if N3.NodeName = 'ApplicationCryptogram' then begin
              Result.Emv.ApplicationCryptogram.CryptogramType:= GetNodeValue(N3, 'CryptogramType');
              Result.Emv.ApplicationCryptogram.Cryptogram:=     GetNodeValue(N3, 'Cryptogram');
            end else
            if N3.NodeName = 'Amount' then begin
              Result.Emv.Amount.AmountAuthorized:=  StrToCurrDef(GetNodeValue(N3, 'AmountAuthorized'), 0);
              Result.Emv.Amount.AmountOther:=       StrToCurrDef(GetNodeValue(N3, 'AmountOther'), 0);
            end else
            if N3.NodeName = 'TerminalInformation' then begin
              Result.Emv.TerminalInformation.TerminalType:=               StrToIntDef(GetNodeValue(N3, 'TerminalType'), 0);
              Result.Emv.TerminalInformation.IfdSerialNumber:=            GetNodeValue(N3, 'IfdSerialNumber');
              Result.Emv.TerminalInformation.TerminalCountryCode:=        GetNodeValue(N3, 'TerminalCountryCode');
              Result.Emv.TerminalInformation.TerminalID:=                 GetNodeValue(N3, 'TerminalID');
              Result.Emv.TerminalInformation.TerminalActionCodeDefault:=  GetNodeValue(N3, 'TerminalActionCodeDefault');
              Result.Emv.TerminalInformation.TerminalActionCodeDenial:=   GetNodeValue(N3, 'TerminalActionCodeDenial');
              Result.Emv.TerminalInformation.TerminalActionCodeOnline:=   GetNodeValue(N3, 'TerminalActionCodeOnline');
            end else
            if N3.NodeName = 'TransactionInformation' then begin
              Result.Emv.TransactionInformation.TransactionType:=               MWStrToTransactionType(GetNodeValue(N3, 'TransactionType'));
              Result.Emv.TransactionInformation.TransactionCurrencyCode:=       StrToIntDef(GetNodeValue(N3, 'TransactionCurrencyCode'), 0);
              Result.Emv.TransactionInformation.TransactionStatusInformation:=  GetNodeValue(N3, 'TransactionStatusInformation');
            end else begin
              //Unrecognized emv node name
            end;
          end;
        end else begin
          //No Emv Information Available
          Result.EmvUsed:= False;
        end;
      end else begin
        //Unrecognized node name
      end;
    end;
  except
    on E: Exception do begin
      Result.ApprovalStatus:= [asFailed];
      Result.ApprovalCode2:= 99700;
      Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

function TMerchantWare.DoRequestTransactionReference4List(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWTransactionReference4List;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
  X: Integer;
  N: IXMLNode;
  T: IMWTransactionReference4;
  Stat: TMWApprovalStatusSet;
  Code: Integer;
  Msg: String;
begin
  //This one will be a list
  Result:= TMWTransactionReference4List.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      //Result.ApprovalStatus:= [asFailed];
      //Result.ErrorCode:= 99600;
      //Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      //Result.ApprovalStatus:= [asFailed];
      //Result.ErrorCode:= 99500;
      //Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    for X := 0 to ResponseNode.ChildNodes.Count-1 do begin
      N:= ResponseNode.ChildNodes.Nodes[X];
      T:= Result.Add;
      ParseApprovalStatus(GetNodeValue(N, 'ApprovalStatus'), Stat, Code, Msg);
      T.Token:=                 GetNodeValue(N, 'Token');
      T.InvoiceNumber:=         GetNodeValue(N, 'InvoiceNumber');
      T.TransactionDate:=       StrToDateTimeDef(GetNodeValue(N, 'TransactionDate'), 0);
      T.ApprovalStatus:=        Stat;
      T.ApprovalCode:=          Code;
      T.ApprovalMessage:=       Msg;
      T.Cardholder:=            GetNodeValue(N, 'Cardholder');
      T.Amount:=                StrToCurrDef(GetNodeValue(N, 'Amount'), 0);
      T.TransactionType:=       MWStrToTransactionType(GetNodeValue(N, 'TransactionType'));
      T.CardNumber:=            GetNodeValue(N, 'CardNumber');
      T.CardType:=              MWStrToCardType(GetNodeValue(N, 'CardType'));
      T.AuthorizationCode:=     GetNodeValue(N, 'AuthorizationCode');
      T.MerchantTransactionId:= GetNodeValue(N, 'MerchantTransactionId');
      T.BatchNumber:=           GetNodeValue(N, 'BatchNumber');
    end;
  except
    on E: Exception do begin
      //Result.ApprovalStatus:= [asFailed];
      //Result.ErrorCode:= 99700;
      //Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

function TMerchantWare.DoRequestTransactionSummary4List(const AXML, ASvcPath,
  AEndpointPath, AReqName: String): IMWTransactionSummary4List;
var
  Response: IXMLDocument;
  ResponseNode : IXMLNode;
  X: Integer;
  N: IXMLNode;
  T: IMWTransactionSummary4;
begin
  //This one will be a list
  Result:= TMWTransactionSummary4List.Create;
  try
    Response:= SendTransportRequest(AEndpointPath, ASvcPath, AReqName, AXML);

    if (not Assigned(Response)) then begin
      //Result.ApprovalStatus:= [asFailed];
      //Result.ErrorCode:= 99600;
      //Result.ErrorMessage:= 'Failed to load Response XML';
      Exit;
    end;

    ResponseNode := GetNodePath(Response, '//'+AReqName+'Response/'+AReqName+'Result');
    if (not Assigned(ResponseNode)) then begin
      //Result.ApprovalStatus:= [asFailed];
      //Result.ErrorCode:= 99500;
      //Result.ErrorMessage:= 'Failed to locate Response XML Node';
      Exit;
    end;

    //Read Responce into result
    for X := 0 to ResponseNode.ChildNodes.Count-1 do begin
      N:= ResponseNode.ChildNodes.Nodes[X];
      T:= Result.Add;
      T.Cardholder:=            GetNodeValue(N, 'Cardholder');
      T.SaleAmount:=            StrToCurrDef(GetNodeValue(N, 'SaleAmount'), 0);
      T.SaleCount:=             StrToIntDef(GetNodeValue(N, 'SaleCount'), 0);
      T.RefundAmount:=          StrToCurrDef(GetNodeValue(N, 'RefundAmount'), 0);
      T.RefundCount:=           StrToIntDef(GetNodeValue(N, 'RefundCount'), 0);
      T.NetAmount:=             StrToCurrDef(GetNodeValue(N, 'NetAmount'), 0);
      T.TotalTransactions:=     StrToIntDef(GetNodeValue(N, 'TotalTransactions'), 0);
      T.CardType:=              MWStrToCardType(GetNodeValue(N, 'CardType'));
    end;
  except
    on E: Exception do begin
      //Result.ApprovalStatus:= [asFailed];
      //Result.ErrorCode:= 99700;
      //Result.ErrorMessage:= 'General Failure: '+E.Message;
    end;
  end;
end;

{ TMWTransactionBase }

constructor TMWTransactionBase.Create(AOwner: TMerchantWare);
begin
  FOwner:= AOwner;
end;

destructor TMWTransactionBase.Destroy;
begin

  inherited;
end;

function TMWTransactionBase.GetOwner: IMerchantWare;
begin
  Result:= IMerchantWare(FOwner);
end;

{ TMWCreditTransactions }

constructor TMWCreditTransactions.Create(AOwner: TMerchantWare);
begin
  inherited;
end;

destructor TMWCreditTransactions.Destroy;
begin

  inherited;
end;

function TMWCreditTransactions.GetEndpoint: String;
begin
  if FOwner.TestMode then
    Result:= EP_TEST_CREDIT_TRANS
  else
    Result:= EP_PROD_CREDIT_TRANS;
end;

function TMWCreditTransactions.GetService: String;
begin
  Result:= 'http://schemas.merchantwarehouse.com/merchantware/40/Credit/';
end;

// ------------------------- Credit Transactions ------------------------------

function TMWCreditTransactions.ApplyTip(const Token: String; const TipAmount: Currency): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_ApplyTip.aspx

  XML:=       XmlVal('token', Token, True);
  XML:= XML + XmlVal('tipAmount', TipAmount);

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'ApplyTip');
end;

function TMWCreditTransactions.CaptureSignatureTiff(const Token: String; const ImageData: TStream): IMWSignatureResponse4;
var
  XML: String;
  Str: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_CaptureSignatureTiff.aspx

  //TODO: Convert stream into Base64 string (Str)

  XML:=       XmlVal('token', Token, True);
  XML:= XML + XmlVal('imageData', Str);

  Result:= FOwner.DoRequestSignatureResponse4(XML,GetService, GetEndpoint, 'CaptureSignatureTiff');
end;

function TMWCreditTransactions.CaptureSignatureVector(const Token, ImageData: String): IMWSignatureResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_CaptureSignatureVector.aspx

  XML:=       XmlVal('token', Token, True);
  XML:= XML + XmlVal('imageData', ImageData);

  Result:= FOwner.DoRequestSignatureResponse4(XML,GetService, GetEndpoint, 'CaptureSignatureVector');
end;

function TMWCreditTransactions.DebitSale(const InvoiceNumber: String;
  const Amount: Currency; const TrackData, PinBlock, PinKsn: String;
  const SurchargeAmount, CashbackAmount: Currency; const ForceDuplicate: Boolean;
  const RegisterNumber: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_DebitSale.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('trackData', TrackData, True);
  XML:= XML + XmlVal('pinBlock', PinBlock, True);
  XML:= XML + XmlVal('pinKsn', PinKsn, True);
  XML:= XML + XmlVal('surchargeAmount', SurchargeAmount, True);
  XML:= XML + XmlVal('cashbackAmount', CashbackAmount, True);
  XML:= XML + XmlVal('forceDuplicate', ForceDuplicate, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber);

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'DebitSale');
end;

function TMWCreditTransactions.ForceSale(const InvoiceNumber,
  AuthorizationCode: String; const Amount: Currency;
  const CardNumber: TCardNumber; const ExpirationDate: TExpirationDate;
  const Cardholder, RegisterNumber, MerchantTransactionId: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_ForceSale.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('authorizationCode', AuthorizationCode, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('cardNumber', CardNumber, True);
  XML:= XML + XmlVal('expirationDate', ExpirationDate.GetStr(False, False), True);
  XML:= XML + XmlVal('cardHolder', Cardholder, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'ForceSale');
end;

function TMWCreditTransactions.Level2Sale(const InvoiceNumber: String;
  const Amount: Currency; const TrackData, CustomerCode, PoNumber: String;
  const TaxAmount: Currency; const ForceDuplicate: Boolean;
  const RegisterNumber, MerchantTransactionId: String;
  const EntryMode: TMWEntryMode): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_Level2Sale.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('trackData', TrackData, True);
  XML:= XML + XmlVal('customerCode', CustomerCode, True);
  XML:= XML + XmlVal('poNumber', PONumber, True);
  XML:= XML + XmlVal('taxAmount', TaxAmount, True);
  XML:= XML + XmlVal('forceDuplicate', ForceDuplicate, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId, True);
  XML:= XML + XmlVal('entryMode', MWEntryModeToStr(EntryMode));

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'Level2Sale');
end;

function TMWCreditTransactions.Level2SaleKeyed(const InvoiceNumber: String;
  const Amount: Currency; const CardNumber: TCardNumber;
  const ExpirationDate: TExpirationDate; const CardHolder, BillStreet, BillZip,
  SecCode, CustomerCode, PoNumber: String; const TaxAmount: Currency;
  const ForceDuplicate: Boolean; const RegisterNumber,
  MerchantTransactionId: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_Level2SaleKeyed.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('cardNumber', CardNumber, True);
  XML:= XML + XmlVal('expirationDate', ExpirationDate.GetStr(False, False), True);
  XML:= XML + XmlVal('cardHolder', CardHolder, True);
  XML:= XML + XmlVal('avsStreetAddress', BillStreet, True);
  XML:= XML + XmlVal('avsStreetZipCode', BillZip, True);
  XML:= XML + XmlVal('cardSecurityCode', SecCode, True);
  XML:= XML + XmlVal('customerCode', CustomerCode, True);
  XML:= XML + XmlVal('poNumber', PONumber, True);
  XML:= XML + XmlVal('taxAmount', TaxAmount, True);
  XML:= XML + XmlVal('forceDuplicate', ForceDuplicate, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'Level2SaleKeyed');
end;

function TMWCreditTransactions.Level2SaleVault(const InvoiceNumber: String;
  const Amount: Currency; const VaultToken, CustomerCode, PoNumber: String;
  const TaxAmount: Currency; const ForceDuplicate: Boolean;
  const RegisterNumber, MerchantTransactionId: String;
  const EntryMode: TMWEntryMode): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_Level2SaleVault.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('vaultToken', VaultToken, True);
  XML:= XML + XmlVal('customerCode', CustomerCode, True);
  XML:= XML + XmlVal('poNumber', PONumber, True);
  XML:= XML + XmlVal('taxAmount', TaxAmount, True);
  XML:= XML + XmlVal('forceDuplicate', ForceDuplicate, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId, True);
  XML:= XML + XmlVal('entryMode', MWEntryModeToStr(EntryMode));

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'Level2SaleVault');
end;

function TMWCreditTransactions.PostAuthorization(const InvoiceNumber,
  Token: String; const Amount: Currency; const RegisterNumber,
  MerchantTransactionId: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_PostAuthorization.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('token', Token, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestCreditResponse4(XML, GetService, GetEndpoint, 'PostAuthorization');
end;

function TMWCreditTransactions.PreAuthorization(const InvoiceNumber: String;
  const Amount: Currency; const TrackData, RegisterNumber,
  MerchantTransactionId: String;
  const EntryMode: TMWEntryMode): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_PreAuthorization.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('trackData', TrackData, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId, True);
  XML:= XML + XmlVal('entryMode', MWEntryModeToStr(EntryMode));

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'PreAuthorization');
end;

function TMWCreditTransactions.PreAuthorizationKeyed(
  const InvoiceNumber: String; const Amount: Currency;
  const CardNumber: TCardNumber; const ExpirationDate: TExpirationDate;
  const CardHolder, BillStreet, BillZip, SecCode, RegisterNumber,
  MerchantTransactionId: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_PreAuthorizationKeyed.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('cardNumber', CardNumber, True);
  XML:= XML + XmlVal('expirationDate', ExpirationDate.GetStr(False, False), True);
  XML:= XML + XmlVal('cardholder', CardHolder, True);
  XML:= XML + XmlVal('avsStreetAddress', BillStreet, True);
  XML:= XML + XmlVal('avsStreetZipCode', BillZip, True);
  XML:= XML + XmlVal('cardSecurityCode', SecCode, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'PreAuthorizationKeyed');
end;

function TMWCreditTransactions.PreAuthorizationVault(
  const InvoiceNumber: String; const Amount: Currency; const VaultToken,
  RegisterNumber, MerchantTransactionID: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_PreAuthorizationVault.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('vaultToken', VaultToken, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId, True);

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'PreAuthorizationVault');
end;

function TMWCreditTransactions.Refund(const InvoiceNumber, Token: String;
  const OverrideAmount: Currency; const RegisterNumber,
  MerchantTransactionId: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_Refund.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('token', Token, True);
  XML:= XML + XmlVal('overrideAmount', OverrideAmount, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'Refund');
end;

function TMWCreditTransactions.RepeatSale(const InvoiceNumber,
  Token: String; const OverrideAmount: Currency;
  const ExpirationDate: TExpirationDate; const BillStreet, BillZip,
  RegisterNumber, MerchantTransactionId: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_RepeatSale.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('token', Token, True);
  XML:= XML + XmlVal('overrideAmount', OverrideAmount, True);
  XML:= XML + XmlVal('expirationDate', ExpirationDate.GetStr(False, False), True);
  XML:= XML + XmlVal('avsStreetAddress', BillStreet, True);
  XML:= XML + XmlVal('avsStreetZipCode', BillZip, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'RepeatSale');
end;

function TMWCreditTransactions.Sale(const InvoiceNumber: String;
  const Amount: Currency; const TrackData: String;
  const ForceDuplicate: Boolean; const RegisterNumber,
  MerchantTransactionId: String;
  const EntryMode: TMWEntryMode): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_Sale.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('trackData', TrackData, True);
  XML:= XML + XmlVal('forceDuplicate', ForceDuplicate, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId, True);
  XML:= XML + XmlVal('entryMode', MWEntryModeToStr(EntryMode));

  Result:= FOwner.DoRequestCreditResponse4(XML,GetService, GetEndpoint, 'Sale');
end;

function TMWCreditTransactions.SaleKeyed(const InvoiceNumber: String;
  const Amount: Currency; const CardNumber: TCardNumber;
  const ExpirationDate: TExpirationDate; const CardHolder, BillStreet, BillZip,
  SecCode: String; const ForceDuplicate: Boolean; const RegisterNumber,
  MerchantTransactionId: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_SaleKeyed.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('cardNumber', CardNumber, True);
  XML:= XML + XmlVal('expirationDate', ExpirationDate.GetStr(False, False), True);
  XML:= XML + XmlVal('cardholder', CardHolder, True);
  XML:= XML + XmlVal('avsStreetAddress', BillStreet, True);
  XML:= XML + XmlVal('avsStreetZipCode', BillZip, True);
  XML:= XML + XmlVal('cardSecurityCode', SecCode, True);
  XML:= XML + XmlVal('forceDuplicate', ForceDuplicate, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestCreditResponse4(XML, GetService, GetEndpoint, 'SaleKeyed');
end;

function TMWCreditTransactions.SaleVault(const InvoiceNumber: String;
  const Amount: Currency; const VaultToken: String;
  const ForceDuplicate: Boolean; const RegisterNumber,
  MerchantTransactionID: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_SaleVault.aspx

  XML:=       XmlVal('invoiceNumber', InvoiceNumber, True);
  XML:= XML + XmlVal('amount', Amount, True);
  XML:= XML + XmlVal('vaultToken', VaultToken, True);
  XML:= XML + XmlVal('forceDuplicate', ForceDuplicate, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestCreditResponse4(XML, GetService, GetEndpoint, 'SaleVault');
end;

function TMWCreditTransactions.SettleBatch: IMWBatchResponse4;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_SettleBatch.aspx

  Result:= FOwner.DoRequestBatchResponse4('', GetService, GetEndpoint, 'SettleBatch');
end;

function TMWCreditTransactions.Void(const Token, RegisterNumber,
  MerchantTransactionId: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_Void.aspx

  XML:=       XmlVal('token', Token, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestCreditResponse4(XML, GetService, GetEndpoint, 'Void');
end;

function TMWCreditTransactions.VoidPreAuthorization(const Token,
  RegisterNumber, MerchantTransactionId: String): IMWCreditResponse4;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_VoidPreAuthorization.aspx

  XML:=       XmlVal('token', Token, True);
  XML:= XML + XmlVal('registerNumber', RegisterNumber, True);
  XML:= XML + XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestCreditResponse4(XML, GetService, GetEndpoint, 'VoidPreAuthorization');
end;

{ TMWVaultTransactions }

constructor TMWVaultTransactions.Create(AOwner: TMerchantWare);
begin
  inherited;

end;

destructor TMWVaultTransactions.Destroy;
begin

  inherited;
end;

function TMWVaultTransactions.GetEndpoint: String;
begin
  if FOwner.TestMode then
    Result:= EP_TEST_VAULT_TRANS
  else
    Result:= EP_PROD_VAULT_TRANS;
end;

function TMWVaultTransactions.GetService: String;
begin
  Result:= 'http://schemas.merchantwarehouse.com/merchantware/40/Credit/';
end;

//

function TMWVaultTransactions.VaultBoardCredit(const TrackData, BillStreet,
  BillZip: String; const MerchantDefinedToken: String = ''): IMWVaultBoardingResponse;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_VaultBoardCredit.aspx

  XML:=       XmlVal('trackData', TrackData, True);
  XML:= XML + XmlVal('avsStreetAddress', BillStreet, True);
  XML:= XML + XmlVal('avsStreetZipCode', BillZip);
  if MerchantDefinedToken <> '' then
    XML:= XML + XmlVal('merchantDefinedToken', MerchantDefinedToken, True);

  Result:= FOwner.DoRequestVaultBoardingResponse(XML, GetService, GetEndpoint, 'VaultBoardCredit');
end;

function TMWVaultTransactions.VaultBoardCreditByReference(const ReferenceNumber: String;
  const MerchantDefinedToken: String = ''): IMWVaultBoardingResponse;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_VaultBoardCreditByReference.aspx

  XML:=       XmlVal('referenceNumber', ReferenceNumber, True);
  if MerchantDefinedToken <> '' then
    XML:= XML + XmlVal('merchantDefinedToken', MerchantDefinedToken, True);

  Result:= FOwner.DoRequestVaultBoardingResponse(XML, GetService, GetEndpoint, 'VaultBoardCreditByReference');
end;

function TMWVaultTransactions.VaultBoardCreditKeyed(
  const CardNumber: TCardNumber; const Expiration: TExpirationDate;
  const CardHolder, BillStreet, BillZip: String;
  const MerchantDefinedToken: String = ''): IMWVaultBoardingResponse;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_VaultBoardCreditKeyed.aspx

  XML:=       XmlVal('cardNumber', CardNumber, True);
  XML:= XML + XmlVal('expirationDate', Expiration.GetStr(False, False), True);
  XML:= XML + XmlVal('cardholder', CardHolder, True);
  XML:= XML + XmlVal('avsStreetAddress', BillStreet, True);
  XML:= XML + XmlVal('avsStreetZipCode', BillZip);
  if MerchantDefinedToken <> '' then
    XML:= XML + XmlVal('merchantDefinedToken', MerchantDefinedToken, True);

  Result:= FOwner.DoRequestVaultBoardingResponse(XML, GetService, GetEndpoint, 'VaultBoardCreditKeyed');
end;

function TMWVaultTransactions.VaultDeleteToken(
  const VaultToken: String): IMWVaultBoardingResponse;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_VaultDeleteToken.aspx

  XML:=       XmlVal('vaultToken', VaultToken, True);

  Result:= FOwner.DoRequestVaultBoardingResponse(XML,GetService, GetEndpoint, 'VaultDeleteToken');
end;

function TMWVaultTransactions.VaultFindPaymentInfo(
  const VaultToken: String): IMWVaultPaymentInfoResponse;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/credit_VaultFindPaymentInfo.aspx

  XML:=       XmlVal('vaultToken', VaultToken, True);

  Result:= FOwner.DoRequestVaultPaymentInfoResponse(XML,GetService, GetEndpoint, 'VaultFindPaymentInfo');
end;

{ TMWEbtTransactions }

constructor TMWEbtTransactions.Create(AOwner: TMerchantWare);
begin
  inherited;

end;

destructor TMWEbtTransactions.Destroy;
begin

  inherited;
end;

function TMWEbtTransactions.GetEndpoint: String;
begin
  if FOwner.TestMode then
    Result:= EP_TEST_EBT_TRANS
  else
    Result:= EP_PROD_EBT_TRANS;
end;

function TMWEbtTransactions.GetService: String;
begin
  Result:= 'http://schemas.merchantwarehouse.com/merchantware/40/Ebt/';
end;

{ TMWGiftCardTransactions }

constructor TMWGiftCardTransactions.Create(AOwner: TMerchantWare);
begin
  inherited;

end;

destructor TMWGiftCardTransactions.Destroy;
begin

  inherited;
end;

function TMWGiftCardTransactions.GetEndpoint: String;
begin
  if FOwner.TestMode then
    Result:= EP_TEST_GIFT_TRANS
  else
    Result:= EP_PROD_GIFT_TRANS;
end;

function TMWGiftCardTransactions.GetService: String;
begin
  Result:= 'http://schemas.merchantwarehouse.com/merchantware/40/Giftcard/';
end;

{ TMWLevelUpTransactions }

constructor TMWLevelUpTransactions.Create(AOwner: TMerchantWare);
begin
  inherited;

end;

destructor TMWLevelUpTransactions.Destroy;
begin

  inherited;
end;

function TMWLevelUpTransactions.GetEndpoint: String;
begin
  if FOwner.TestMode then
    Result:= '' //NONE
  else
    Result:= EP_PROD_LEVELUP_TRANS;
end;

function TMWLevelUpTransactions.GetService: String;
begin
  Result:= 'http://schemas.merchantwarehouse.com/merchantware/40/LevelUp/';
end;

{ TMWCheckTransactions }

constructor TMWCheckTransactions.Create(AOwner: TMerchantWare);
begin
  inherited;

end;

destructor TMWCheckTransactions.Destroy;
begin

  inherited;
end;

function TMWCheckTransactions.GetEndpoint: String;
begin
  if FOwner.TestMode then
    Result:= EP_TEST_CHECK_TRANS
  else
    Result:= EP_PROD_CHECK_TRANS;
end;

function TMWCheckTransactions.GetService: String;
begin
  Result:= 'http://schemas.merchantwarehouse.com/merchantware/40/Check/';
end;

{ TMWReportTransactions }

constructor TMWReportTransactions.Create(AOwner: TMerchantWare);
begin
  inherited;

end;

destructor TMWReportTransactions.Destroy;
begin

  inherited;
end;

function TMWReportTransactions.GetEndpoint: String;
begin
  if FOwner.TestMode then
    Result:= EP_TEST_REPORTING
  else
    Result:= EP_PROD_REPORTING;
end;

function TMWReportTransactions.GetService: String;
begin
  Result:= 'http://schemas.merchantwarehouse.com/merchantware/40/Reports/';
end;

function TMWReportTransactions.CurrentBatchSummary(
  const CardholderFilter: String;
  const CardType: TMWCardType): IMWTransactionSummary4List;
var
  XML: String;
begin
  //https://ps1.merchantware.net/Merchantware/documentation40/standard/report_CurrentBatchSummary.aspx

  XML:= '';
  //if CardholderFilter <> '' then
    XML:= XML + XmlVal('cardholderFilter', CardholderFilter, True);
  //if CardType <> TMWCardType.ctUnknown then
    XML:= XML + XmlVal('cardType', MWCardTypeToStr(CardType), True);

  Result:= FOwner.DoRequestTransactionSummary4List(XML,GetService, GetEndpoint, 'CurrentBatchSummary');
end;

function TMWReportTransactions.CurrentBatchTransactions: IMWTransactionReference4List;
begin
  //

  Result:= FOwner.DoRequestTransactionReference4List('',GetService, GetEndpoint, 'CurrentBatchTransactions');
end;

function TMWReportTransactions.DetailedTransactionByReference(
  const Token: String): IMWDetailedTransactionReference4;
var
  XML: String;
begin
  //

  XML:= XmlVal('token', Token);

  Result:= FOwner.DoRequestDetailedTransactionReference4(XML,GetService, GetEndpoint, 'DetailedTransactionByReference');
end;

function TMWReportTransactions.DetailedTransactionByTransactionId(
  const MerchantTransactionId: String): IMWDetailedTransactionReference4;
var
  XML: String;
begin
  //

  XML:= XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestDetailedTransactionReference4(XML,GetService, GetEndpoint, 'DetailedTransactionByTransactionId');
end;

function TMWReportTransactions.SummaryByDate(const StartDate,
  EndDate: TDate; const CardholderFilter: String;
  const CardType: TMWCardType): IMWTransactionSummary4List;
var
  XML: String;
begin
  //

  XML:= XmlVal('startDate', StartDate);
  XML:= XML + XmlVal('endDate', EndDate);
  XML:= XML + XmlVal('cardholderFilter', CardholderFilter);
  XML:= XML + XmlVal('cardType', MWCardTypeToStr(CardType));

  Result:= FOwner.DoRequestTransactionSummary4List(XML,GetService, GetEndpoint, 'SummaryByDate');
end;

function TMWReportTransactions.TransactionsByDate(const StartDate,
  EndDate: TDate; const InvoiceNumber, RegisterNumber,
  AuthorizationCode: String): IMWTransactionReference4List;
var
  XML: String;
begin
  //

  XML:= XmlVal('startDate', StartDate);
  XML:= XML + XmlVal('endDate', EndDate);
  XML:= XML + XmlVal('invoiceNumber', InvoiceNumber);
  XML:= XML + XmlVal('registerNumber', RegisterNumber);
  XML:= XML + XmlVal('authorizationCode', AuthorizationCode);

  Result:= FOwner.DoRequestTransactionReference4List(XML,GetService, GetEndpoint, 'TransactionsByDate');
end;

function TMWReportTransactions.TransactionsByReference(
  const Token: String): IMWTransactionReference4List;
var
  XML: String;
begin
  //

  XML:= XmlVal('token', Token);

  Result:= FOwner.DoRequestTransactionReference4List(XML,GetService, GetEndpoint, 'TransactionsByReference');
end;

function TMWReportTransactions.TransactionsByTransactionId(
  const MerchantTransactionId: String): IMWTransactionReference4List;
var
  XML: String;
begin
  //

  XML:= XmlVal('merchantTransactionId', MerchantTransactionId);

  Result:= FOwner.DoRequestTransactionReference4List(XML,GetService, GetEndpoint, 'TransactionsByTransactionId');
end;

{ TMWBatchResponse4 }

constructor TMWBatchResponse4.Create;
begin

end;

destructor TMWBatchResponse4.Destroy;
begin

  inherited;
end;

function TMWBatchResponse4.GetAuthorizationCode: String;
begin
  Result:= FAuthorizationCode;
end;

function TMWBatchResponse4.GetBatchAmount: Currency;
begin
  Result:= FBatchAmount;
end;

function TMWBatchResponse4.GetBatchStatus: TMWBatchStatusSet;
begin
  Result:= FBatchStatus;
end;

function TMWBatchResponse4.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TMWBatchResponse4.GetExtraData: String;
begin
  Result:= FExtraData;
end;

function TMWBatchResponse4.GetToken: String;
begin
  Result:= FToken;
end;

function TMWBatchResponse4.GetTransactionCount: Integer;
begin
  Result:= FTransactionCount;
end;

function TMWBatchResponse4.GetTransactionDate: TDateTime;
begin
  Result:= FTransactionDate;
end;

procedure TMWBatchResponse4.SetAuthorizationCode(const Value: String);
begin
  FAuthorizationCode:= Value;
end;

procedure TMWBatchResponse4.SetBatchAmount(const Value: Currency);
begin
  FBatchAmount:= Value;
end;

procedure TMWBatchResponse4.SetBatchStatus(const Value: TMWBatchStatusSet);
begin
  FBatchStatus:= Value;
end;

procedure TMWBatchResponse4.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TMWBatchResponse4.SetExtraData(const Value: String);
begin
  FExtraData:= Value;
end;

procedure TMWBatchResponse4.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TMWBatchResponse4.SetTransactionCount(const Value: Integer);
begin
  FTransactionCount:= Value;
end;

procedure TMWBatchResponse4.SetTransactionDate(const Value: TDateTime);
begin
  FTransactionDate:= Value;
end;

{ TMWCreditLevel2Response4 }

constructor TMWCreditLevel2Response4.Create;
begin

end;

destructor TMWCreditLevel2Response4.Destroy;
begin

  inherited;
end;

function TMWCreditLevel2Response4.GetAmount: Currency;
begin
  Result:= FAmount;
end;

function TMWCreditLevel2Response4.GetApprovalCode: Integer;
begin
  Result:= FApprovalCode;
end;

function TMWCreditLevel2Response4.GetApprovalMessage: String;
begin
  Result:= FApprovalMessage;
end;

function TMWCreditLevel2Response4.GetApprovalStatus: TMWApprovalStatusSet;
begin
  Result:= FApprovalStatus;
end;

function TMWCreditLevel2Response4.GetAuthorizationCode: String;
begin
  Result:= FAuthorizationCode;
end;

function TMWCreditLevel2Response4.GetAvsResponse: String;
begin
  Result:= FAvsResponse;
end;

function TMWCreditLevel2Response4.GetCardholder: String;
begin
  Result:= FCardholder;
end;

function TMWCreditLevel2Response4.GetCardNumber: TCardNumber;
begin
  Result:= FCardNumber;
end;

function TMWCreditLevel2Response4.GetCardType: TMWCardType;
begin
  Result:= FCardType;
end;

function TMWCreditLevel2Response4.GetCustomerCode: String;
begin
  Result:= FCustomerCode;
end;

function TMWCreditLevel2Response4.GetCvResponse: String;
begin
  Result:= FCvResponse;
end;

function TMWCreditLevel2Response4.GetEntryMode: TMWPosEntryType;
begin
  Result:= FEntryMode;
end;

function TMWCreditLevel2Response4.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TMWCreditLevel2Response4.GetExtraData: String;
begin
  Result:= FExtraData;
end;

function TMWCreditLevel2Response4.GetInvoiceNumber: String;
begin
  Result:= FInvoiceNumber;
end;

function TMWCreditLevel2Response4.GetPurchaseOrderNumber: String;
begin
  Result:= FPurchaseOrderNumber;
end;

function TMWCreditLevel2Response4.GetTaxAmount: Currency;
begin
  Result:= FTaxAmount;
end;

function TMWCreditLevel2Response4.GetToken: String;
begin
  Result:= FToken;
end;

function TMWCreditLevel2Response4.GetTransactionDate: TDateTime;
begin
  Result:= FTransactionDate;
end;

function TMWCreditLevel2Response4.GetTransactionType: TMWTransactionType;
begin
  Result:= FTransactionType;
end;

procedure TMWCreditLevel2Response4.SetAmount(const Value: Currency);
begin
  FAmount:= Value;
end;

procedure TMWCreditLevel2Response4.SetApprovalCode(const Value: Integer);
begin
  FApprovalCode:= Value;
end;

procedure TMWCreditLevel2Response4.SetApprovalMessage(const Value: String);
begin
  FApprovalMessage:= Value;
end;

procedure TMWCreditLevel2Response4.SetApprovalStatus(
  const Value: TMWApprovalStatusSet);
begin
  FApprovalStatus:= Value;
end;

procedure TMWCreditLevel2Response4.SetAuthorizationCode(
  const Value: String);
begin
  FAuthorizationCode:= Value;
end;

procedure TMWCreditLevel2Response4.SetAvsResponse(const Value: String);
begin
  FAVsResponse:= Value;
end;

procedure TMWCreditLevel2Response4.SetCardholder(const Value: String);
begin
  FCardholder:= Value;
end;

procedure TMWCreditLevel2Response4.SetCardNumber(const Value: TCardNumber);
begin
  FCardNumber:= Value;
end;

procedure TMWCreditLevel2Response4.SetCardType(const Value: TMWCardType);
begin
  FCardType:= Value;
end;

procedure TMWCreditLevel2Response4.SetCustomerCode(const Value: String);
begin
  FCustomerCode:= Value;
end;

procedure TMWCreditLevel2Response4.SetCvResponse(const Value: String);
begin
  FCvResponse:= Value;
end;

procedure TMWCreditLevel2Response4.SetEntryMode(const Value: TMWPosEntryType);
begin
  FEntryMode:= Value;
end;

procedure TMWCreditLevel2Response4.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TMWCreditLevel2Response4.SetExtraData(const Value: String);
begin
  FExtraData:= Value;
end;

procedure TMWCreditLevel2Response4.SetInvoiceNumber(const Value: String);
begin
  FInvoiceNumber:= Value;
end;

procedure TMWCreditLevel2Response4.SetPurchaseOrderNumber(
  const Value: String);
begin
  FPurchaseOrderNumber:= Value;
end;

procedure TMWCreditLevel2Response4.SetTaxAmount(const Value: Currency);
begin
  FTaxAmount:= Value;
end;

procedure TMWCreditLevel2Response4.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TMWCreditLevel2Response4.SetTransactionDate(const Value: TDateTime);
begin
  FTransactionDate:= Value;
end;

procedure TMWCreditLevel2Response4.SetTransactionType(
  const Value: TMWTransactionType);
begin
  FTransactionType:= Value;
end;

{ TMWCreditResponse4 }

constructor TMWCreditResponse4.Create;
begin

end;

destructor TMWCreditResponse4.Destroy;
begin

  inherited;
end;

function TMWCreditResponse4.GetAmount: Currency;
begin
  Result:= FAmount;
end;

function TMWCreditResponse4.GetApprovalCode: Integer;
begin
  Result:= FApprovalCode;
end;

function TMWCreditResponse4.GetApprovalMessage: String;
begin
  Result:= FApprovalMessage;
end;

function TMWCreditResponse4.GetApprovalStatus: TMWApprovalStatusSet;
begin
  Result:= FApprovalStatus;
end;

function TMWCreditResponse4.GetAuthorizationCode: String;
begin
  Result:= FAuthorizationCode;
end;

function TMWCreditResponse4.GetAvsResponse: String;
begin
  Result:= FAvsResponse;
end;

function TMWCreditResponse4.GetCardholder: String;
begin
  Result:= FCardholder;
end;

function TMWCreditResponse4.GetCardNumber: String;
begin
  Result:= FCardNumber;
end;

function TMWCreditResponse4.GetCardType: TMWCardType;
begin
  Result:= FCardType;
end;

function TMWCreditResponse4.GetCvResponse: String;
begin
  Result:= FCvResponse;
end;

function TMWCreditResponse4.GetEntryMode: TMWPosEntryType;
begin
  Result:= FEntryMode;
end;

function TMWCreditResponse4.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TMWCreditResponse4.GetExtraData: String;
begin
  Result:= FExtraData;
end;

function TMWCreditResponse4.GetInvoiceNumber: String;
begin
  Result:= FInvoiceNumber;
end;

function TMWCreditResponse4.GetToken: String;
begin
  Result:= FToken;
end;

function TMWCreditResponse4.GetTransactionDate: TDateTime;
begin
  Result:= FTransactionDate;
end;

function TMWCreditResponse4.GetTransactionType: TMWTransactionType;
begin
  Result:= FTransactionType;
end;

procedure TMWCreditResponse4.SetAmount(const Value: Currency);
begin
  FAmount:= Value;
end;

procedure TMWCreditResponse4.SetApprovalCode(const Value: Integer);
begin
  FApprovalCode:= Value;
end;

procedure TMWCreditResponse4.SetApprovalMessage(const Value: String);
begin
  FApprovalMessage:= Value;
end;

procedure TMWCreditResponse4.SetApprovalStatus(
  const Value: TMWApprovalStatusSet);
begin
  FApprovalStatus:= Value;
end;

procedure TMWCreditResponse4.SetAuthorizationCode(const Value: String);
begin
  FAuthorizationCode:= Value;
end;

procedure TMWCreditResponse4.SetAvsResponse(const Value: String);
begin
  FAvsResponse:= Value;
end;

procedure TMWCreditResponse4.SetCardholder(const Value: String);
begin
  FCardholder:= Value;
end;

procedure TMWCreditResponse4.SetCardNumber(const Value: String);
begin
  FCardNumber:= Value;
end;

procedure TMWCreditResponse4.SetCardType(const Value: TMWCardType);
begin
  FCardType:= Value;
end;

procedure TMWCreditResponse4.SetCvResponse(const Value: String);
begin
  FCvResponse:= Value;
end;

procedure TMWCreditResponse4.SetEntryMode(const Value: TMWPosEntryType);
begin
  FEntryMode:= Value;
end;

procedure TMWCreditResponse4.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TMWCreditResponse4.SetExtraData(const Value: String);
begin
  FExtraData:= Value;
end;

procedure TMWCreditResponse4.SetInvoiceNumber(const Value: String);
begin
  FInvoiceNumber:= Value;
end;

procedure TMWCreditResponse4.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TMWCreditResponse4.SetTransactionDate(const Value: TDateTime);
begin
  FTransactionDate:= Value;
end;

procedure TMWCreditResponse4.SetTransactionType(
  const Value: TMWTransactionType);
begin
  FTransactionType:= Value;
end;

{ TMWEmvAuthorizeResponse4 }

constructor TMWEmvAuthorizeResponse4.Create;
begin

end;

destructor TMWEmvAuthorizeResponse4.Destroy;
begin

  inherited;
end;

function TMWEmvAuthorizeResponse4.GetApprovalCode: Integer;
begin
  Result:= FApprovalCode;
end;

function TMWEmvAuthorizeResponse4.GetApprovalMessage: String;
begin
  Result:= FApprovalMessage;
end;

function TMWEmvAuthorizeResponse4.GetApprovalStatus: TMWEmvApprovalStatusSet;
begin
  Result:= FApprovalStatus;
end;

function TMWEmvAuthorizeResponse4.GetCashbackAmount: Currency;
begin
  Result:= FCashbackAmount;
end;

function TMWEmvAuthorizeResponse4.GetDonationAmount: Currency;
begin
  Result:= FDonationAmount;
end;

function TMWEmvAuthorizeResponse4.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TMWEmvAuthorizeResponse4.GetSurchargeAmount: Currency;
begin
  Result:= FSurchargeAmount;
end;

function TMWEmvAuthorizeResponse4.GetTlvData: String;
begin
  Result:= FTlvData;
end;

function TMWEmvAuthorizeResponse4.GetToken: String;
begin
  Result:= FToken;
end;

function TMWEmvAuthorizeResponse4.GetTransactionDate: TDateTime;
begin
  Result:= FTransactionDate;
end;

function TMWEmvAuthorizeResponse4.GetUserTipAmount: Currency;
begin
  Result:= FUserTipAmount;
end;

procedure TMWEmvAuthorizeResponse4.SetApprovalCode(const Value: Integer);
begin
  FApprovalCode:= Value;
end;

procedure TMWEmvAuthorizeResponse4.SetApprovalMessage(const Value: String);
begin
  FApprovalMessage:= Value;
end;

procedure TMWEmvAuthorizeResponse4.SetApprovalStatus(
  const Value: TMWEmvApprovalStatusSet);
begin
  FApprovalStatus:= Value;
end;

procedure TMWEmvAuthorizeResponse4.SetCashbackAmount(const Value: Currency);
begin
  FCashbackAmount:= Value;
end;

procedure TMWEmvAuthorizeResponse4.SetDonationAmount(const Value: Currency);
begin
  FDonationAmount:= Value;
end;

procedure TMWEmvAuthorizeResponse4.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TMWEmvAuthorizeResponse4.SetSurchargeAmount(const Value: Currency);
begin
  FSurchargeAmount:= Value;
end;

procedure TMWEmvAuthorizeResponse4.SetTlvData(const Value: String);
begin
  FTlvData:= Value;
end;

procedure TMWEmvAuthorizeResponse4.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TMWEmvAuthorizeResponse4.SetTransactionDate(const Value: TDateTime);
begin
  FTransactionDate:= Value;
end;

procedure TMWEmvAuthorizeResponse4.SetUserTipAmount(const Value: Currency);
begin
  FUserTipAmount:= Value;
end;

{ TMWEmvCompleteResponse4 }

constructor TMWEmvCompleteResponse4.Create;
begin

end;

destructor TMWEmvCompleteResponse4.Destroy;
begin

  inherited;
end;

function TMWEmvCompleteResponse4.GetApprovalCode: Integer;
begin
  Result:= FApprovalCode;
end;

function TMWEmvCompleteResponse4.GetApprovalMessage: String;
begin
  Result:= FApprovalMessage;
end;

function TMWEmvCompleteResponse4.GetApprovalStatus: TMWEmvApprovalStatusSet;
begin
  Result:= FApprovalStatus;
end;

function TMWEmvCompleteResponse4.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

procedure TMWEmvCompleteResponse4.SetApprovalCode(const Value: Integer);
begin
  FApprovalCode:= Value;
end;

procedure TMWEmvCompleteResponse4.SetApprovalMessage(const Value: String);
begin
  FApprovalMessage:= Value;
end;

procedure TMWEmvCompleteResponse4.SetApprovalStatus(
  const Value: TMWEmvApprovalStatusSet);
begin
  FApprovalStatus:= Value;
end;

procedure TMWEmvCompleteResponse4.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

{ TMWSignatureResponse4 }

constructor TMWSignatureResponse4.Create;
begin

end;

destructor TMWSignatureResponse4.Destroy;
begin

  inherited;
end;

function TMWSignatureResponse4.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TMWSignatureResponse4.GetSignatureType: TMWSignatureType;
begin
  Result:= FSignatureType;
end;

function TMWSignatureResponse4.GetToken: String;
begin
  Result:= FToken;
end;

function TMWSignatureResponse4.GetUploadDate: TDateTime;
begin
  Result:= FUploadDate;
end;

function TMWSignatureResponse4.GetUploadStatus: TMWSignatureStatus;
begin
  Result:= FUploadStatus;
end;

procedure TMWSignatureResponse4.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TMWSignatureResponse4.SetSignatureType(const Value: TMWSignatureType);
begin
  FSignatureType:= Value;
end;

procedure TMWSignatureResponse4.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TMWSignatureResponse4.SetUploadDate(const Value: TDateTime);
begin
  FUploadDate:= VAlue;
end;

procedure TMWSignatureResponse4.SetUploadStatus(
  const Value: TMWSignatureStatus);
begin
  FUploadStatus:= Value;
end;

{ TMWVaultBoardingResponse }

constructor TMWVaultBoardingResponse.Create;
begin

end;

destructor TMWVaultBoardingResponse.Destroy;
begin

  inherited;
end;

function TMWVaultBoardingResponse.GetErrorCode: Integer;
begin
  Result:= FErrorCode;
end;

function TMWVaultBoardingResponse.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TMWVaultBoardingResponse.GetVaultToken: String;
begin
  Result:= FVaultToken;
end;

procedure TMWVaultBoardingResponse.SetErrorCode(const Value: Integer);
begin
  FErrorCode:= Value;
end;

procedure TMWVaultBoardingResponse.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TMWVaultBoardingResponse.SetVaultToken(const Value: String);
begin
  FVaultToken:= Value;
end;

{ TMWVaultPaymentInfoResponse }

constructor TMWVaultPaymentInfoResponse.Create;
begin

end;

destructor TMWVaultPaymentInfoResponse.Destroy;
begin

  inherited;
end;

function TMWVaultPaymentInfoResponse.GetCardholder: String;
begin
  Result:= FCardholder;
end;

function TMWVaultPaymentInfoResponse.GetCardNumber: String;
begin
  Result:= FCardNumber;
end;

function TMWVaultPaymentInfoResponse.GetCardType: TMWCardType;
begin
  Result:= FCardType;
end;

function TMWVaultPaymentInfoResponse.GetErrorCode: Integer;
begin
  Result:= FErrorCode;
end;

function TMWVaultPaymentInfoResponse.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

procedure TMWVaultPaymentInfoResponse.SetCardholder(const Value: String);
begin
  FCardholder:= Value;
end;

procedure TMWVaultPaymentInfoResponse.SetCardNumber(const Value: String);
begin
  FCardNumber:= Value;
end;

procedure TMWVaultPaymentInfoResponse.SetCardType(const Value: TMWCardType);
begin
  FCardType:= Value;
end;

procedure TMWVaultPaymentInfoResponse.SetErrorCode(const Value: Integer);
begin
  FErrorCode:= Value;
end;

procedure TMWVaultPaymentInfoResponse.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

{ TMWGiftResponse4 }

constructor TMWGiftResponse4.Create;
begin

end;

destructor TMWGiftResponse4.Destroy;
begin

  inherited;
end;

function TMWGiftResponse4.GetAmount: Currency;
begin
  Result:= FAmount;
end;

function TMWGiftResponse4.GetApprovalCode: Integer;
begin
  Result:= FApprovalCode;
end;

function TMWGiftResponse4.GetApprovalMessage: String;
begin
  Result:= FApprovalMessage;
end;

function TMWGiftResponse4.GetApprovalStatus: TMWApprovalStatusSet;
begin
  Result:= FApprovalStatus;
end;

function TMWGiftResponse4.GetCardBalance: Currency;
begin
  Result:= FCardBalance;
end;

function TMWGiftResponse4.GetCardholder: String;
begin
  Result:= FCardholder;
end;

function TMWGiftResponse4.GetCardNumber: String;
begin
  Result:= FCardNumber;
end;

function TMWGiftResponse4.GetEntryMode: TMWPosEntryType;
begin
  Result:= FEntryMode;
end;

function TMWGiftResponse4.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TMWGiftResponse4.GetExpirationDate: TExpirationDate;
begin
  Result:= FExpirationDate;
end;

function TMWGiftResponse4.GetExtraData: String;
begin
  Result:= FExtraData;
end;

function TMWGiftResponse4.GetMiscMessage: String;
begin
  Result:= FMiscMessage;
end;

function TMWGiftResponse4.GetResponseMessage: String;
begin
  Result:= FResponseMessage;
end;

function TMWGiftResponse4.GetToken: String;
begin
  Result:= FToken;
end;

function TMWGiftResponse4.GetTransactionDate: TDateTime;
begin
  Result:= FTransactionDate;
end;

function TMWGiftResponse4.GetTransactionID: String;
begin
  Result:= FTransactionID;
end;

function TMWGiftResponse4.GetTransactionType: TMWTransactionType;
begin
  Result:= FTransactionType;
end;

procedure TMWGiftResponse4.SetAmount(const Value: Currency);
begin
  FAmount:= Value;
end;

procedure TMWGiftResponse4.SetApprovalCode(const Value: Integer);
begin
  FApprovalCode:= Value;
end;

procedure TMWGiftResponse4.SetApprovalMessage(const Value: String);
begin
  FApprovalMessage:= Value;
end;

procedure TMWGiftResponse4.SetApprovalStatus(const Value: TMWApprovalStatusSet);
begin
  FApprovalStatus:= Value;
end;

procedure TMWGiftResponse4.SetCardBalance(const Value: Currency);
begin
  FCardBalance:= Value;
end;

procedure TMWGiftResponse4.SetCardholder(const Value: String);
begin
  FCardholder:= Value;
end;

procedure TMWGiftResponse4.SetCardNumber(const Value: String);
begin
  FCardNumber:= Value;
end;

procedure TMWGiftResponse4.SetEntryMode(const Value: TMWPosEntryType);
begin
  FEntryMode:= Value;
end;

procedure TMWGiftResponse4.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TMWGiftResponse4.SetExpirationDate(const Value: TExpirationDate);
begin
  FExpirationDate:= Value;
end;

procedure TMWGiftResponse4.SetExtraData(const Value: String);
begin
  FExtraData:= Value;
end;

procedure TMWGiftResponse4.SetMiscMessage(const Value: String);
begin
  FMiscMessage:= Value;
end;

procedure TMWGiftResponse4.SetResponseMessage(const Value: String);
begin
  FResponseMessage:= Value;
end;

procedure TMWGiftResponse4.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TMWGiftResponse4.SetTransactionDate(const Value: TDateTime);
begin
  FTransactionDate:= Value;
end;

procedure TMWGiftResponse4.SetTransactionID(const Value: String);
begin
  FTransactionID:= Value;
end;

procedure TMWGiftResponse4.SetTransactionType(const Value: TMWTransactionType);
begin
  FTransactionType:= Value;
end;

{ TMWGiftLoyaltyResponse4 }

constructor TMWGiftLoyaltyResponse4.Create;
begin

end;

destructor TMWGiftLoyaltyResponse4.Destroy;
begin

  inherited;
end;

function TMWGiftLoyaltyResponse4.GetApprovalCode: Integer;
begin
  Result:= FApprovalCode;
end;

function TMWGiftLoyaltyResponse4.GetApprovalMessage: String;
begin
  Result:= FApprovalMessage;
end;

function TMWGiftLoyaltyResponse4.GetApprovalStatus: TMWApprovalStatusSet;
begin
  Result:= FApprovalStatus;
end;

function TMWGiftLoyaltyResponse4.GetCardBalance: Currency;
begin
  Result:= FCardBalance;
end;

function TMWGiftLoyaltyResponse4.GetCardholder: String;
begin
  Result:= FCardholder;
end;

function TMWGiftLoyaltyResponse4.GetCardNumber: String;
begin
  Result:= FCardNumber;
end;

function TMWGiftLoyaltyResponse4.GetEntryMode: TMWPosEntryType;
begin
  Result:= FEntryMode;
end;

function TMWGiftLoyaltyResponse4.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TMWGiftLoyaltyResponse4.GetExpirationDate: TExpirationDate;
begin
  Result:= FExpirationDate;
end;

function TMWGiftLoyaltyResponse4.GetExtraData: String;
begin
  Result:= FExtraData;
end;

function TMWGiftLoyaltyResponse4.GetLifePointsBalance: Integer;
begin
  Result:= FLifePointsBalance;
end;

function TMWGiftLoyaltyResponse4.GetMiscMessage: String;
begin
  Result:= FMiscMessage;
end;

function TMWGiftLoyaltyResponse4.GetPointsBalance: Integer;
begin
  Result:= FPointsBalance;
end;

function TMWGiftLoyaltyResponse4.GetResponseMessage: String;
begin
  Result:= FResponseMessage;
end;

function TMWGiftLoyaltyResponse4.GetToken: String;
begin
  Result:= FToken;
end;

function TMWGiftLoyaltyResponse4.GetTransactionDate: TDateTime;
begin
  Result:= FTransactionDate;
end;

function TMWGiftLoyaltyResponse4.GetTransactionID: String;
begin
  Result:= FTransactionID;
end;

function TMWGiftLoyaltyResponse4.GetTransactionType: TMWTransactionType;
begin
  Result:= FTransactionType;
end;

procedure TMWGiftLoyaltyResponse4.SetApprovalCode(const Value: Integer);
begin
  FApprovalCode:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetApprovalMessage(const Value: String);
begin
  FApprovalMessage:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetApprovalStatus(const Value: TMWApprovalStatusSet);
begin
  FApprovalStatus:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetCardBalance(const Value: Currency);
begin
  FCardBalance:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetCardholder(const Value: String);
begin
  FCardholder:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetCardNumber(const Value: String);
begin
  FCardNumber:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetEntryMode(const Value: TMWPosEntryType);
begin
  FEntryMode:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetExpirationDate(const Value: TExpirationDate);
begin
  FExpirationDate:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetExtraData(const Value: String);
begin
  FExtraData:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetLifePointsBalance(const Value: Integer);
begin
  FLifePointsBalance:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetMiscMessage(const Value: String);
begin
  FMiscMessage:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetPointsBalance(const Value: Integer);
begin
  FPointsBalance:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetResponseMessage(const Value: String);
begin
  FResponseMessage:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetTransactionDate(const Value: TDateTime);
begin
  FTransactionDate:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetTransactionID(const Value: String);
begin
  FTransactionID:= Value;
end;

procedure TMWGiftLoyaltyResponse4.SetTransactionType(const Value: TMWTransactionType);
begin
  FTransactionType:= Value;
end;

{ TMWDetailedTransactionReference4 }

constructor TMWDetailedTransactionReference4.Create;
begin
  FSupportedActions:= TMWSupportedActions.Create;
  FSupportedActions._AddRef;
  FEmv:= TMWEmv.Create;
  FEmv._AddRef;
  FHealthCareAmountDetails:= TMWHealthCareAmountDetails.Create;
  FHealthCareAmountDetails._AddRef;
end;

destructor TMWDetailedTransactionReference4.Destroy;
begin
  FHealthCareAmountDetails._Release;
  FEmv._Release;
  FSupportedActions._Release;
  inherited;
end;

function TMWDetailedTransactionReference4.GetApprovalCode: String;
begin
  Result:= FApprovalCode;
end;

function TMWDetailedTransactionReference4.GetApprovalCode2: Integer;
begin
  Result:= FApprovalCode2;
end;

function TMWDetailedTransactionReference4.GetApprovalMessage: String;
begin
  Result:= FApprovalMessage;
end;

function TMWDetailedTransactionReference4.GetApprovalStatus: TMWApprovalStatusSet;
begin
  Result:= FApprovalStatus;
end;

function TMWDetailedTransactionReference4.GetAuthorizationAmount: Currency;
begin
  Result:= FAuthorizationAmount;
end;

function TMWDetailedTransactionReference4.GetAvsResponse: String;
begin
  Result:= FAvsResponse;
end;

function TMWDetailedTransactionReference4.GetBatchNumber: String;
begin
  Result:= FBatchNumber;
end;

function TMWDetailedTransactionReference4.GetCardholder: String;
begin
  Result:= FCardholder;
end;

function TMWDetailedTransactionReference4.GetCardNumber: String;
begin
  Result:= FCardNumber;
end;

function TMWDetailedTransactionReference4.GetCardType: TMWCardType;
begin
  Result:= FCardType;
end;

function TMWDetailedTransactionReference4.GetCashBackAmount: Currency;
begin
  Result:= FCashBackAmount;
end;

function TMWDetailedTransactionReference4.GetConvenienceAmount: Currency;
begin
  Result:= FConvenienceAmount;
end;

function TMWDetailedTransactionReference4.GetCustomerId: String;
begin
  Result:= FCustomerId;
end;

function TMWDetailedTransactionReference4.GetCvResponse: String;
begin
  Result:= FCvResponse;
end;

function TMWDetailedTransactionReference4.GetDiscountAmount: Currency;
begin
  Result:= FDiscountAmount;
end;

function TMWDetailedTransactionReference4.GetEmv: IMWEmv;
begin
  Result:= FEmv;
end;

function TMWDetailedTransactionReference4.GetEmvUsed: Boolean;
begin
  Result:= FEmvUsed;
end;

function TMWDetailedTransactionReference4.GetEntryMode: TMWPosEntryType;
begin
  Result:= FEntryMode;
end;

function TMWDetailedTransactionReference4.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TMWDetailedTransactionReference4.GetExpirationDate: TExpirationDate;
begin
  Result:= FExpirationDate;
end;

function TMWDetailedTransactionReference4.GetHealthCareAmountDetails: IMWHealthCareAmountDetails;
begin
  Result:= FHealthCareAmountDetails;
end;

function TMWDetailedTransactionReference4.GetHostReference: String;
begin
  Result:= FHostReference;
end;

function TMWDetailedTransactionReference4.GetInvoiceNumber: String;
begin
  Result:= FInvoiceNumber;
end;

function TMWDetailedTransactionReference4.GetIsCardNumberKeyed: Boolean;
begin
  Result:= FIsCardNumberKeyed;
end;

function TMWDetailedTransactionReference4.GetIsTransactionAdjusted: Boolean;
begin
  Result:= FIsTransactionAdjusted;
end;

function TMWDetailedTransactionReference4.GetIsTransactionCaptured: Boolean;
begin
  Result:= FIsTransactionCaptured;
end;

function TMWDetailedTransactionReference4.GetIsTransactionSettled: Boolean;
begin
  Result:= FIsTransactionSettled;
end;

function TMWDetailedTransactionReference4.GetIsTransactionVoided: Boolean;
begin
  Result:= FIsTransactionVoided;
end;

function TMWDetailedTransactionReference4.GetMerchantTransactionId: String;
begin
  Result:= FMerchantTransactionId;
end;

function TMWDetailedTransactionReference4.GetOriginalToken: String;
begin
  Result:= FOriginalToken;
end;

function TMWDetailedTransactionReference4.GetPostalCode: String;
begin
  Result:= FPostalCode;
end;

function TMWDetailedTransactionReference4.GetPurchaseOrderNumber: String;
begin
  Result:= FPurchaseOrderNumber;
end;

function TMWDetailedTransactionReference4.GetRegisterNumber: String;
begin
  Result:= FRegisterNumber;
end;

function TMWDetailedTransactionReference4.GetSupportedActions: IMWSupportedActions;
begin
  Result:= FSupportedActions;
end;

function TMWDetailedTransactionReference4.GetSurchargeAmount: Currency;
begin
  Result:= FSurchargeAmount;
end;

function TMWDetailedTransactionReference4.GetTaxAmount: Currency;
begin
  Result:= FTaxAmount;
end;

function TMWDetailedTransactionReference4.GetTipAmount: Currency;
begin
  Result:= FTipAmount;
end;

function TMWDetailedTransactionReference4.GetToken: String;
begin
  Result:= FToken;
end;

function TMWDetailedTransactionReference4.GetTotalAmount: Currency;
begin
  Result:= FTotalAmount;
end;

function TMWDetailedTransactionReference4.GetTransactionDate: TDateTime;
begin
  Result:= FTransactionDate;
end;

function TMWDetailedTransactionReference4.GetTransactionType: TMWTransactionType;
begin
  Result:= FTransactionType;
end;

function TMWDetailedTransactionReference4.GetUserName: String;
begin
  Result:= FUserName;
end;

procedure TMWDetailedTransactionReference4.SetApprovalCode(const Value: String);
begin
  FApprovalCode:= Value;
end;

procedure TMWDetailedTransactionReference4.SetApprovalCode2(const Value: Integer);
begin
  FApprovalCode2:= Value;
end;

procedure TMWDetailedTransactionReference4.SetApprovalMessage(const Value: String);
begin
  FApprovalMessage:= Value;
end;

procedure TMWDetailedTransactionReference4.SetApprovalStatus(const Value: TMWApprovalStatusSet);
begin
  FApprovalStatus:= Value;
end;

procedure TMWDetailedTransactionReference4.SetAuthorizationAmount(const Value: Currency);
begin
  FAuthorizationAmount:= Value;
end;

procedure TMWDetailedTransactionReference4.SetAvsResponse(const Value: String);
begin
  FAvsResponse:= Value;
end;

procedure TMWDetailedTransactionReference4.SetBatchNumber(const Value: String);
begin
  FBatchNumber:= Value;
end;

procedure TMWDetailedTransactionReference4.SetCardholder(const Value: String);
begin
  FCardholder:= Value;
end;

procedure TMWDetailedTransactionReference4.SetCardNumber(const Value: String);
begin
  FCardNumber:= Value;
end;

procedure TMWDetailedTransactionReference4.SetCardType(const Value: TMWCardType);
begin
  FCardType:= Value;
end;

procedure TMWDetailedTransactionReference4.SetCashBackAmount(const Value: Currency);
begin
  FCashBackAmount:= Value;
end;

procedure TMWDetailedTransactionReference4.SetConvenienceAmount(const Value: Currency);
begin
  FConvenienceAmount:= Value;
end;

procedure TMWDetailedTransactionReference4.SetCustomerId(const Value: String);
begin
  FCustomerId:= Value;
end;

procedure TMWDetailedTransactionReference4.SetCvResponse(const Value: String);
begin
  FCvResponse:= Value;
end;

procedure TMWDetailedTransactionReference4.SetDiscountAmount(const Value: Currency);
begin
  FDiscountAmount:= Value;
end;

procedure TMWDetailedTransactionReference4.SetEmvUsed(const Value: Boolean);
begin
  FEmvUsed:= Value;
end;

procedure TMWDetailedTransactionReference4.SetEntryMode(const Value: TMWPosEntryType);
begin
  FEntryMode:= Value;
end;

procedure TMWDetailedTransactionReference4.SetErrorMessage(const Value: String);
begin
  FErrorMessage:= Value;
end;

procedure TMWDetailedTransactionReference4.SetExpirationDate(const Value: TExpirationDate);
begin
  FExpirationDate:= Value;
end;

procedure TMWDetailedTransactionReference4.SetHostReference(const Value: String);
begin
  FHostReference:= Value;
end;

procedure TMWDetailedTransactionReference4.SetInvoiceNumber(const Value: String);
begin
  FInvoiceNumber:= Value;
end;

procedure TMWDetailedTransactionReference4.SetIsCardNumberKeyed(const Value: Boolean);
begin
  FIsCardNumberKeyed:= Value;
end;

procedure TMWDetailedTransactionReference4.SetIsTransactionAdjusted(const Value: Boolean);
begin
  FIsTransactionAdjusted:= Value;
end;

procedure TMWDetailedTransactionReference4.SetIsTransactionCaptured(const Value: Boolean);
begin
  FIsTransactionCaptured:= Value;
end;

procedure TMWDetailedTransactionReference4.SetIsTransactionSettled(const Value: Boolean);
begin
  FIsTransactionSettled:= Value;
end;

procedure TMWDetailedTransactionReference4.SetIsTransactionVoided(const Value: Boolean);
begin
  FIsTransactionVoided:= Value;
end;

procedure TMWDetailedTransactionReference4.SetMerchantTransactionId(const Value: String);
begin
  FMerchantTransactionId:= Value;
end;

procedure TMWDetailedTransactionReference4.SetOriginalToken(const Value: String);
begin
  FOriginalToken:= Value;
end;

procedure TMWDetailedTransactionReference4.SetPostalCode(const Value: String);
begin
  FPostalCode:= Value;
end;

procedure TMWDetailedTransactionReference4.SetPurchaseOrderNumber(const Value: String);
begin
  FPurchaseOrderNumber:= Value;
end;

procedure TMWDetailedTransactionReference4.SetRegisterNumber(const Value: String);
begin
  FRegisterNumber:= Value;
end;

procedure TMWDetailedTransactionReference4.SetSurchargeAmount(const Value: Currency);
begin
  FSurchargeAmount:= Value;
end;

procedure TMWDetailedTransactionReference4.SetTaxAmount(const Value: Currency);
begin
  FTaxAmount:= Value;
end;

procedure TMWDetailedTransactionReference4.SetTipAmount(const Value: Currency);
begin
  FTipAmount:= Value;
end;

procedure TMWDetailedTransactionReference4.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TMWDetailedTransactionReference4.SetTotalAmount(const Value: Currency);
begin
  FTotalAmount:= Value;
end;

procedure TMWDetailedTransactionReference4.SetTransactionDate(const Value: TDateTime);
begin
  FTransactionDate:= Value;
end;

procedure TMWDetailedTransactionReference4.SetTransactionType(const Value: TMWTransactionType);
begin
  FTransactionType:= Value;
end;

procedure TMWDetailedTransactionReference4.SetUserName(const Value: String);
begin
  FUserName:= Value;
end;

{ TMWEmv }

constructor TMWEmv.Create;
begin
  FApplicationInformation:= TMWEmvApplicationInformation.Create;
  FApplicationInformation._AddRef;
  FCardInformation:= TMWEmvCardInformation.Create;
  FCardInformation._AddRef;
  FApplicationCryptogram:= TMWEmvApplicationCryptogram.Create;
  FApplicationCryptogram._AddRef;
  FAmount:= TMWEmvAmount.Create;
  FAmount._AddRef;
  FTerminalInformation:= TMWEmvTerminalInformation.Create;
  FTerminalInformation._AddRef;
  FTransactionInformation:= TMWEmvTransactionInformation.Create;
  FTransactionInformation._AddRef;
end;

destructor TMWEmv.Destroy;
begin
  FTransactionInformation._Release;
  FTerminalInformation._Release;
  FAmount._Release;
  FApplicationCryptogram._Release;
  FCardInformation._Release;
  FApplicationInformation._Release;
  inherited;
end;

function TMWEmv.GetAmount: IMWEmvAmount;
begin
  Result:= FAmount;
end;

function TMWEmv.GetApplicationCryptogram: IMWEmvApplicationCryptogram;
begin
  Result:= FApplicationCryptogram;
end;

function TMWEmv.GetApplicationInformation: IMWEmvApplicationInformation;
begin
  Result:= FApplicationInformation;
end;

function TMWEmv.GetAuthorizationResponseCode: String;
begin
  Result:= FAuthorizationResponseCode;
end;

function TMWEmv.GetCardInformation: IMWEmvCardInformation;
begin
  Result:= FCardInformation;
end;

function TMWEmv.GetCryptogramInformationData: String;
begin
  Result:= FCryptogramInformationData;
end;

function TMWEmv.GetCvmMethod: String;
begin
  Result:= FCvmMethod;
end;

function TMWEmv.GetCVMResults: String;
begin
  Result:= FCVMResults;
end;

function TMWEmv.GetIssuerActionCodeDefault: String;
begin
  Result:= FIssuerActionCodeDefault;
end;

function TMWEmv.GetIssuerActionCodeDenial: String;
begin
  Result:= FIssuerActionCodeDenial;
end;

function TMWEmv.GetIssuerActionCodeOnline: String;
begin
  Result:= FIssuerActionCodeOnline;
end;

function TMWEmv.GetIssuerApplicationData: String;
begin
  Result:= FIssuerApplicationData;
end;

function TMWEmv.GetPINStatement: String;
begin
  Result:= FPINStatement;
end;

function TMWEmv.GetPosEntryMode: TMWPosEntryType;
begin
  Result:= FPosEntryMode;
end;

function TMWEmv.GetTerminalInformation: IMWEmvTerminalInformation;
begin
  Result:= FTerminalInformation;
end;

function TMWEmv.GetTerminalVerificationResults: String;
begin
  Result:= FTerminalVerificationResults;
end;

function TMWEmv.GetTransactionInformation: IMWEmvTransactionInformation;
begin
  Result:= FTransactionInformation;
end;

function TMWEmv.GetUnpredictableNumber: String;
begin
  Result:= FUnpredictableNumber;
end;

procedure TMWEmv.SetAuthorizationResponseCode(const Value: String);
begin
  FAuthorizationResponseCode:= Value;
end;

procedure TMWEmv.SetCryptogramInformationData(const Value: String);
begin
  FCryptogramInformationData:= Value;
end;

procedure TMWEmv.SetCvmMethod(const Value: String);
begin
  FCvmMethod:= Value;
end;

procedure TMWEmv.SetCVMResults(const Value: String);
begin
  FCVMResults:= Value;
end;

procedure TMWEmv.SetIssuerActionCodeDefault(const Value: String);
begin
  FIssuerActionCodeDefault:= Value;
end;

procedure TMWEmv.SetIssuerActionCodeDenial(const Value: String);
begin
  FIssuerActionCodeDenial:= Value;
end;

procedure TMWEmv.SetIssuerActionCodeOnline(const Value: String);
begin
  FIssuerActionCodeOnline:= Value;
end;

procedure TMWEmv.SetIssuerApplicationData(const Value: String);
begin
  FIssuerApplicationData:= Value;
end;

procedure TMWEmv.SetPINStatement(const Value: String);
begin
  FPINStatement:= Value;
end;

procedure TMWEmv.SetPosEntryMode(const Value: TMWPosEntryType);
begin
  FPosEntryMode:= Value;
end;

procedure TMWEmv.SetTerminalVerificationResults(const Value: String);
begin
  FTerminalVerificationResults:= Value;
end;

procedure TMWEmv.SetUnpredictableNumber(const Value: String);
begin
  FUnpredictableNumber:= Value;
end;

{ TMWEmvApplicationInformation }

constructor TMWEmvApplicationInformation.Create;
begin

end;

destructor TMWEmvApplicationInformation.Destroy;
begin

  inherited;
end;

function TMWEmvApplicationInformation.GetAid: String;
begin
  Result:= FAid;
end;

function TMWEmvApplicationInformation.GetApplicationDisplayName: String;
begin
  Result:= FApplicationDisplayName;
end;

function TMWEmvApplicationInformation.GetApplicationEffectiveDate: TDateTime;
begin
  Result:= FApplicationEffectiveDate;
end;

function TMWEmvApplicationInformation.GetApplicationExpiryDate: TDateTime;
begin
  Result:= FApplicationExpiryDate;
end;

function TMWEmvApplicationInformation.GetApplicationInterchangeProfile: String;
begin
  Result:= FApplicationInterchangeProfile;
end;

function TMWEmvApplicationInformation.GetApplicationLabel: String;
begin
  Result:= FApplicationLabel;
end;

function TMWEmvApplicationInformation.GetApplicationPreferredName: String;
begin
  Result:= FApplicationPreferredName;
end;

function TMWEmvApplicationInformation.GetApplicationTransactionCounter: Integer;
begin
  Result:= FApplicationTransactionCounter;
end;

function TMWEmvApplicationInformation.GetApplicationUsageControl: String;
begin
  Result:= FApplicationUsageControl;
end;

function TMWEmvApplicationInformation.GetApplicationVersionNumber: String;
begin
  Result:= FApplicationVersionNumber;
end;

procedure TMWEmvApplicationInformation.SetAid(const Value: String);
begin
  FAid:= Value;
end;

procedure TMWEmvApplicationInformation.SetApplicationDisplayName(const Value: String);
begin
  FApplicationDisplayName:= Value;
end;

procedure TMWEmvApplicationInformation.SetApplicationEffectiveDate(const Value: TDateTime);
begin
  FApplicationEffectiveDate:= Value;
end;

procedure TMWEmvApplicationInformation.SetApplicationExpiryDate(const Value: TDateTime);
begin
  FApplicationExpiryDate:= Value;
end;

procedure TMWEmvApplicationInformation.SetApplicationInterchangeProfile(const Value: String);
begin
  FApplicationInterchangeProfile:= Value;
end;

procedure TMWEmvApplicationInformation.SetApplicationLabel(const Value: String);
begin
  FApplicationLabel:= Value;
end;

procedure TMWEmvApplicationInformation.SetApplicationPreferredName(const Value: String);
begin
  FApplicationPreferredName:= Value;
end;

procedure TMWEmvApplicationInformation.SetApplicationTransactionCounter(const Value: Integer);
begin
  FApplicationTransactionCounter:= Value;
end;

procedure TMWEmvApplicationInformation.SetApplicationUsageControl(const Value: String);
begin
  FApplicationUsageControl:= Value;
end;

procedure TMWEmvApplicationInformation.SetApplicationVersionNumber(const Value: String);
begin
  FApplicationVersionNumber:= Value;
end;

{ TMWEmvCardInformation }

constructor TMWEmvCardInformation.Create;
begin

end;

destructor TMWEmvCardInformation.Destroy;
begin

  inherited;
end;

function TMWEmvCardInformation.GetCardExpiryDate: TExpirationDate;
begin
  Result:= FCardExpiryDate;
end;

function TMWEmvCardInformation.GetMaskedPan: String;
begin
  Result:= FMaskedPan;
end;

function TMWEmvCardInformation.GetPanSequenceNumber: String;
begin
  Result:= FPanSequenceNumber;
end;

procedure TMWEmvCardInformation.SetCardExpiryDate(const Value: TExpirationDate);
begin
  FCardExpiryDate:= Value;
end;

procedure TMWEmvCardInformation.SetMaskedPan(const Value: String);
begin
  FMaskedPan:= Value;
end;

procedure TMWEmvCardInformation.SetPanSequenceNumber(const Value: String);
begin
  FPanSequenceNumber:= Value;
end;

{ TMWEmvApplicationCryptogram }

constructor TMWEmvApplicationCryptogram.Create;
begin

end;

destructor TMWEmvApplicationCryptogram.Destroy;
begin

  inherited;
end;

function TMWEmvApplicationCryptogram.GetCryptogram: String;
begin
  Result:= FCryptogram;
end;

function TMWEmvApplicationCryptogram.GetCryptogramType: String;
begin
  Result:= FCryptogramType;
end;

procedure TMWEmvApplicationCryptogram.SetCryptogram(const Value: String);
begin
  FCryptogram:= Value;
end;

procedure TMWEmvApplicationCryptogram.SetCryptogramType(const Value: String);
begin
  FCryptogramType:= Value;
end;

{ TMWEmvAmount }

constructor TMWEmvAmount.Create;
begin

end;

destructor TMWEmvAmount.Destroy;
begin

  inherited;
end;

function TMWEmvAmount.GetAmountAuthorized: Currency;
begin
  Result:= FAmountAuthorized;
end;

function TMWEmvAmount.GetAmountOther: Currency;
begin
  Result:= FAmountOther;
end;

procedure TMWEmvAmount.SetAmountAuthorized(const Value: Currency);
begin
  FAmountAuthorized:= Value;
end;

procedure TMWEmvAmount.SetAmountOther(const Value: Currency);
begin
  FAmountOther:= Value;
end;

{ TMWEmvTerminalInformation }

constructor TMWEmvTerminalInformation.Create;
begin

end;

destructor TMWEmvTerminalInformation.Destroy;
begin

  inherited;
end;

function TMWEmvTerminalInformation.GetIfdSerialNumber: String;
begin
  Result:= FIfdSerialNumber;
end;

function TMWEmvTerminalInformation.GetTerminalActionCodeDefault: String;
begin
  Result:= FTerminalActionCodeDefault;
end;

function TMWEmvTerminalInformation.GetTerminalActionCodeDenial: String;
begin
  Result:= FTerminalActionCodeDenial;
end;

function TMWEmvTerminalInformation.GetTerminalActionCodeOnline: String;
begin
  Result:= FTerminalActionCodeOnline;
end;

function TMWEmvTerminalInformation.GetTerminalCountryCode: String;
begin
  Result:= FTerminalCountryCode;
end;

function TMWEmvTerminalInformation.GetTerminalID: String;
begin
  Result:= FTerminalID;
end;

function TMWEmvTerminalInformation.GetTerminalType: Integer;
begin
  Result:= FTerminalType;
end;

procedure TMWEmvTerminalInformation.SetIfdSerialNumber(const Value: String);
begin
  FIfdSerialNumber:= Value;
end;

procedure TMWEmvTerminalInformation.SetTerminalActionCodeDefault(const Value: String);
begin
  FTerminalActionCodeDefault:= Value;
end;

procedure TMWEmvTerminalInformation.SetTerminalActionCodeDenial(const Value: String);
begin
  FTerminalActionCodeDenial:= Value;
end;

procedure TMWEmvTerminalInformation.SetTerminalActionCodeOnline(const Value: String);
begin
  FTerminalActionCodeOnline:= Value;
end;

procedure TMWEmvTerminalInformation.SetTerminalCountryCode(const Value: String);
begin
  FTerminalCountryCode:= Value;
end;

procedure TMWEmvTerminalInformation.SetTerminalID(const Value: String);
begin
  FTerminalID:= Value;
end;

procedure TMWEmvTerminalInformation.SetTerminalType(const Value: Integer);
begin
  FTerminalType:= Value;
end;

{ TMWEmvTransactionInformation }

constructor TMWEmvTransactionInformation.Create;
begin

end;

destructor TMWEmvTransactionInformation.Destroy;
begin

  inherited;
end;

function TMWEmvTransactionInformation.GetTransactionCurrencyCode: Integer;
begin
  Result:= FTransactionCurrencyCode;
end;

function TMWEmvTransactionInformation.GetTransactionStatusInformation: String;
begin
  Result:= FTransactionStatusInformation;
end;

function TMWEmvTransactionInformation.GetTransactionType: TMWTransactionType;
begin
  Result:= FTransactionType;
end;

procedure TMWEmvTransactionInformation.SetTransactionCurrencyCode(const Value: Integer);
begin
  FTransactionCurrencyCode:= Value;
end;

procedure TMWEmvTransactionInformation.SetTransactionStatusInformation(const Value: String);
begin
  FTransactionStatusInformation:= Value;
end;

procedure TMWEmvTransactionInformation.SetTransactionType(const Value: TMWTransactionType);
begin
  FTransactionType:= Value;
end;

{ TMWHealthCareAmountDetails }

constructor TMWHealthCareAmountDetails.Create;
begin

end;

destructor TMWHealthCareAmountDetails.Destroy;
begin

  inherited;
end;

function TMWHealthCareAmountDetails.GetClinicalAmount: Currency;
begin
  Result:= FClinicalAmount;
end;

function TMWHealthCareAmountDetails.GetCopayAmount: Currency;
begin
  Result:= FCopayAmount;
end;

function TMWHealthCareAmountDetails.GetDentalAmount: Currency;
begin
  Result:= FDentalAmount;
end;

function TMWHealthCareAmountDetails.GetHealthCareTotalAmount: Currency;
begin
  Result:= FHealthCareTotalAmount;
end;

function TMWHealthCareAmountDetails.GetPrescriptionAmount: Currency;
begin
  Result:= FPrescriptionAmount;
end;

function TMWHealthCareAmountDetails.GetVisionAmount: Currency;
begin
  Result:= FVisionAmount;
end;

procedure TMWHealthCareAmountDetails.SetClinicalAmount(const Value: Currency);
begin
  FClinicalAmount:= Value;
end;

procedure TMWHealthCareAmountDetails.SetCopayAmount(const Value: Currency);
begin
  FCopayAmount:= Value;
end;

procedure TMWHealthCareAmountDetails.SetDentalAmount(const Value: Currency);
begin
  FDentalAmount:= Value;
end;

procedure TMWHealthCareAmountDetails.SetHealthCareTotalAmount(const Value: Currency);
begin
  FHealthCareTotalAmount:= Value;
end;

procedure TMWHealthCareAmountDetails.SetPrescriptionAmount(const Value: Currency);
begin
  FPrescriptionAmount:= Value;
end;

procedure TMWHealthCareAmountDetails.SetVisionAmount(const Value: Currency);
begin
  FVisionAmount:= Value;
end;

{ TMWSupportedActions }

constructor TMWSupportedActions.Create;
begin

end;

destructor TMWSupportedActions.Destroy;
begin

  inherited;
end;

function TMWSupportedActions.GetAdjustmentToken: String;
begin
  Result:= FAdjustmentToken;
end;

function TMWSupportedActions.GetCaptureToken: String;
begin
  Result:= FCaptureToken;
end;

function TMWSupportedActions.GetEmailReceiptToken: String;
begin
  Result:= FEmailReceiptToken;
end;

function TMWSupportedActions.GetRefundMaxAmount: Currency;
begin
  Result:= FRefundMaxAmount;
end;

function TMWSupportedActions.GetRefundToken: String;
begin
  Result:= FRefundToken;
end;

function TMWSupportedActions.GetRepeatSaleToken: String;
begin
  Result:= FRepeatSaleToken;
end;

function TMWSupportedActions.GetVoidToken: String;
begin
  Result:= FVoidToken;
end;

procedure TMWSupportedActions.SetAdjustmentToken(const Value: String);
begin
  FAdjustmentToken:= Value;
end;

procedure TMWSupportedActions.SetCaptureToken(const Value: String);
begin
  FCaptureToken:= Value;
end;

procedure TMWSupportedActions.SetEmailReceiptToken(const Value: String);
begin
  FEmailReceiptToken:= Value;
end;

procedure TMWSupportedActions.SetRefundMaxAmount(const Value: Currency);
begin
  FRefundMaxAmount:= Value;
end;

procedure TMWSupportedActions.SetRefundToken(const Value: String);
begin
  FRefundToken:= Value;
end;

procedure TMWSupportedActions.SetRepeatSaleToken(const Value: String);
begin
  FRepeatSaleToken:= Value;
end;

procedure TMWSupportedActions.SetVoidToken(const Value: String);
begin
  FVoidToken:= Value;
end;

{ TMWTransactionReference4List }

constructor TMWTransactionReference4List.Create;
begin
  FItems:= TInterfaceList.Create;
end;

destructor TMWTransactionReference4List.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TMWTransactionReference4List.Add: IMWTransactionReference4;
begin
  Result:= TMWTransactionReference4.Create;
  Result._AddRef;
  FItems.Add(Result);
end;

procedure TMWTransactionReference4List.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TMWTransactionReference4List.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TMWTransactionReference4List.Delete(const Index: Integer);
begin
  IMWTransactionReference4(FItems[Index])._Release;
  FItems.Delete(Index);
end;

function TMWTransactionReference4List.GetItem(Index: Integer): IMWTransactionReference4;
begin
  Result:= IMWTransactionReference4(FItems[Index]);
end;

{ TMWTransactionReference4 }

constructor TMWTransactionReference4.Create;
begin

end;

destructor TMWTransactionReference4.Destroy;
begin

  inherited;
end;

function TMWTransactionReference4.GetAmount: Currency;
begin
  Result:= FAmount;
end;

function TMWTransactionReference4.GetApprovalCode: Integer;
begin
  Result:= FApprovalCode;
end;

function TMWTransactionReference4.GetApprovalMessage: String;
begin
  Result:= FApprovalMessage;
end;

function TMWTransactionReference4.GetApprovalStatus: TMWApprovalStatusSet;
begin
  Result:= FApprovalStatus;
end;

function TMWTransactionReference4.GetAuthorizationCode: String;
begin
  Result:= FAuthorizationCode;
end;

function TMWTransactionReference4.GetBatchNumber: String;
begin
  Result:= FBatchNumber;
end;

function TMWTransactionReference4.GetCardholder: String;
begin
  Result:= FCardholder;
end;

function TMWTransactionReference4.GetCardNumber: String;
begin
  Result:= FCardNumber;
end;

function TMWTransactionReference4.GetCardType: TMWCardType;
begin
  Result:= FCardType;
end;

function TMWTransactionReference4.GetInvoiceNumber: String;
begin
  Result:= FInvoiceNumber;
end;

function TMWTransactionReference4.GetMerchantTransactionId: String;
begin
  Result:= FMerchantTransactionId;
end;

function TMWTransactionReference4.GetToken: String;
begin
  Result:= FToken;
end;

function TMWTransactionReference4.GetTransactionDate: TDateTime;
begin
  Result:= FTransactionDate;
end;

function TMWTransactionReference4.GetTransactionType: TMWTransactionType;
begin
  Result:= FTransactionType;
end;

procedure TMWTransactionReference4.SetAmount(const Value: Currency);
begin
  FAmount:= Value;
end;

procedure TMWTransactionReference4.SetApprovalCode(const Value: Integer);
begin
  FApprovalCode:= Value;
end;

procedure TMWTransactionReference4.SetApprovalMessage(const Value: String);
begin
  FApprovalMessage:= Value;
end;

procedure TMWTransactionReference4.SetApprovalStatus(const Value: TMWApprovalStatusSet);
begin
  FApprovalStatus:= Value;
end;

procedure TMWTransactionReference4.SetAuthorizationCode(const Value: String);
begin
  FAuthorizationCode:= Value;
end;

procedure TMWTransactionReference4.SetBatchNumber(const Value: String);
begin
  FBatchNumber:= Value;
end;

procedure TMWTransactionReference4.SetCardholder(const Value: String);
begin
  FCardholder:= Value;
end;

procedure TMWTransactionReference4.SetCardNumber(const Value: String);
begin
  FCardNumber:= Value;
end;

procedure TMWTransactionReference4.SetCardType(const Value: TMWCardType);
begin
  FCardType:= Value;
end;

procedure TMWTransactionReference4.SetInvoiceNumber(const Value: String);
begin
  FInvoiceNumber:= Value;
end;

procedure TMWTransactionReference4.SetMerchantTransactionId(const Value: String);
begin
  FMerchantTransactionId:= Value;
end;

procedure TMWTransactionReference4.SetToken(const Value: String);
begin
  FToken:= Value;
end;

procedure TMWTransactionReference4.SetTransactionDate(const Value: TDateTime);
begin
  FTransactionDate:= Value;
end;

procedure TMWTransactionReference4.SetTransactionType(const Value: TMWTransactionType);
begin
  FTransactionType:= Value;
end;

{ TMWTransactionSummary4List }

constructor TMWTransactionSummary4List.Create;
begin
  FItems:= TInterfaceList.Create;
end;

destructor TMWTransactionSummary4List.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TMWTransactionSummary4List.Add: IMWTransactionSummary4;
begin
  Result:= TMWTransactionSummary4.Create;
  Result._AddRef;
  FItems.Add(Result);
end;

procedure TMWTransactionSummary4List.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TMWTransactionSummary4List.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TMWTransactionSummary4List.Delete(const Index: Integer);
begin
  IMWTransactionSummary4(FItems[Index])._Release;
  FItems.Delete(Index);
end;

function TMWTransactionSummary4List.GetItem(Index: Integer): IMWTransactionSummary4;
begin
  Result:= IMWTransactionSummary4(FItems[Index]);
end;

{ TMWTransactionSummary4 }

constructor TMWTransactionSummary4.Create;
begin

end;

destructor TMWTransactionSummary4.Destroy;
begin

  inherited;
end;

function TMWTransactionSummary4.GetCardholder: String;
begin
  Result:= FCardholder;
end;

function TMWTransactionSummary4.GetCardType: TMWCardType;
begin
  Result:= FCardType;
end;

function TMWTransactionSummary4.GetNetAmount: Currency;
begin
  Result:= FNetAmount;
end;

function TMWTransactionSummary4.GetRefundAmount: Currency;
begin
  Result:= FRefundAmount;
end;

function TMWTransactionSummary4.GetRefundCount: Integer;
begin
  Result:= FRefundCount;
end;

function TMWTransactionSummary4.GetSaleAmount: Currency;
begin
  Result:= FSaleAmount;
end;

function TMWTransactionSummary4.GetSaleCount: Integer;
begin
  Result:= FSaleCount;
end;

function TMWTransactionSummary4.GetTotalTransactions: Integer;
begin
  Result:= FTotalTransactions;
end;

procedure TMWTransactionSummary4.SetCardholder(const Value: String);
begin
  FCardholder:= Value;
end;

procedure TMWTransactionSummary4.SetCardType(const Value: TMWCardType);
begin
  FCardType:= Value;
end;

procedure TMWTransactionSummary4.SetNetAmount(const Value: Currency);
begin
  FNetAmount:= Value;
end;

procedure TMWTransactionSummary4.SetRefundAmount(const Value: Currency);
begin
  FRefundAmount:= Value;
end;

procedure TMWTransactionSummary4.SetRefundCount(const Value: Integer);
begin
  FRefundCount:= Value;
end;

procedure TMWTransactionSummary4.SetSaleAmount(const Value: Currency);
begin
  FSaleAmount:= Value;
end;

procedure TMWTransactionSummary4.SetSaleCount(const Value: Integer);
begin
  FSaleCount:= Value;
end;

procedure TMWTransactionSummary4.SetTotalTransactions(const Value: Integer);
begin
  FTotalTransactions:= Value;
end;

{ TMWTransactionList }

constructor TMWTransactionList.Create(AOwner: TMerchantWare);
begin
  FOwner:= AOwner;
  FItems:= TInterfaceList.Create;
end;

destructor TMWTransactionList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TMWTransactionList.Add: IMWTransactionItem;
begin
  Result:= TMWTransactionItem.Create(Self);
  Result._AddRef;
end;

function TMWTransactionList.Add(const Token: String): IMWTransactionItem;
begin
  Result:= Add;
  try
    //TODO: Lookup transaction by token

  except
    on E: Exception do begin

    end;
  end;
end;

procedure TMWTransactionList.Delete(const Index: Integer);
begin
  TMWTransactionItem(FItems[Index])._Release;
  FItems.Delete(Index);
end;

procedure TMWTransactionList.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TMWTransactionList.Count: Integer;
begin
  Result:= FItems.Count;
end;

function TMWTransactionList.GetErrorMessage: String;
begin
  Result:= FErrorMessage;
end;

function TMWTransactionList.GetItem(Index: Integer): IMWTransactionItem;
begin
  Result:= IMWTransactionItem(FItems[Index]);
end;

function TMWTransactionList.Search(const DateFrom, DateTo: TDateTime;
  const InvoiceNum, RegisterNum, AuthCode: String): Boolean;
var
  Res: IMWTransactionReference4List;
begin
  //TODO
  Result:= False;
  Clear;
  Res:= FOwner.Report.TransactionsByDate(DateFrom, DateTo, InvoiceNum,
    RegisterNum, AuthCode);
  if Res.Count > 0 then begin


  end;
end;

{ TMWTransactionItem }

constructor TMWTransactionItem.Create(AOwner: TMWTransactionList);
begin
  FOwner:= AOwner;

end;

destructor TMWTransactionItem.Destroy;
begin

  inherited;
end;

initialization
  DefaultDOMVendor:= sOmniXmlVendor;

end.
