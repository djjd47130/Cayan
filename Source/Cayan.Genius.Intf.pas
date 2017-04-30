{
*******************************************************************************

2015-5-22 - Jerry Dodge

Genius Credit Card Processing Interface Library

  - IGenius: encapsulates entire Cayan Genius solution
    - Create by incorporating "GeniusImpl.pas" and creating "TGenius"


*******************************************************************************
}
unit Cayan.Genius.Intf;

interface

uses
  System.Classes, System.SysUtils,
  Cayan.Common;

type
  //TODO: Separate this out for use in either Request or Response
  //Request should only allow Sale, Refund, Level 2, and Pre Auth
  //Post Auth and Unknown are only meant for a Response
  TGeniusTransactionType = (gtSale, gtRefund, gtLevel2Sale, gtPreAuth, gtPostAuth,
    gtForceSale, gtAddValue, gtBalanceInquiry, gtUnknown);

  TGeniusPaymentType = (gpVisa, gpMastercard, gpAmex, gpDiscover, gpDebit, gpGift,
    gpEbt, gpLevelUp, gpVoyager, gpWex, gpJcb, gpCup, gpUnknown);

  TGeniusTransStatus = (gsApproved, gsDeclined, gsError, gsUserCancelled, gsPosCancelled, gsDuplicate);
  TGeniusTransStatusSet = set of TGeniusTransStatus;

  TGeniusSignatureStatus = (ssSuccess, ssTimeout, ssPosCancelled);

  TGeniusLineItemStatus = (soSuccess, soDenied, soError);

  TGeniusCancelTransactionStatus = (ctCancelled, ctApprovedNoSignature, ctDenied, ctError);

  TGeniusKeyedSaleStatus = (gkSuccess, gkFailure, gkDeclined);

  TGeniusEntryMode = (geSwipe, geProximity, geBarcode, geManual);

  TGeniusResponseType = (grSingle, grMulti, grCompound);

  TGeniusProtocol = (prHTTP, prHTTPS);

  TGeniusTransportVersion = (gtVer1, gtVer2, gtVer3, gtVer4);

  TGeniusDeviceVersion = (gdVer1, gdVer2);

  TGeniusLineItemType = (glSku, glItem);

  TGeniusLineItemCategory = (glNone, glEbt, glFuel);

  TGeniusSignatureType = (gsVector, gsBitmap);

  TGeniusCedStatus = (csOffline, csOnline, csDownloadNeeded);

  TGeniusCryptogramType = (ctARQC, ctTC, ctAAC);

  TGeniusExternalPaymentType = (epCash, epCheck, epStoreCredit, epOther);

  TGeniusEmvTransactionType = (etCash, etGoods, etServices, etCashback,
    etInquiry, etPayment, etAdministrative);

  TGeniusCvmMethod = (cmSignature, cmOfflinePin, cmOnlinePin, cmNone, cmOther);

  TGeniusBatchStatus = (bsSuccess, bsFailure);

  TGeniusCedScreen = (csNoScreen = -1, csIdle = 0, csValidatingTransportKey = 1,
    csMainPayment = 2, csCustomPayment = 3, csLookupCardBin = 4,
    csWaitingForAmount = 5, csWaitingForPin = 6, csProcessing = 7,
    csWaitingForSignature = 8, csProcessingSignature = 9,
    csTransactionResult = 10, csCancelConfirm = 11, csRunAsCreditConfirm = 12,
    csSkuDisplay = 13, csCashbackSelection = 14, csCashbackCustom = 15,
    csTipSelection = 16, csTipCustom = 17, csDonationSelection = 18,
    csDonationCustom = 19, csConfirmation = 20, csErrorScreen = 24,
    csSkuAmountConfirm = 26, csPanEntry = 27, csExpirationEntry = 28,
    csCvvEntry = 29, csZipEntry = 30,
    csAgreement = 31, csAgreementSign = 32, csEMVAppSelect = 33,
    csCustomerInput = 35, csGiftCard = 36, csNetworkDetail = 38,
    csNetworkConfig = 39);

  TGeniusAgreementResponseStatus = (arsAccepted, arsDeclined, arsTimeout,
    arsPOSCancelled, arsError);

type
  IGenius = interface;
  IGeniusStageRequest = interface;
  IGeniusStageResponse = interface;
  IGeniusStageMessage = interface;
  IGeniusTransactionResponse = interface;
  IGeniusTransactionResponseEmv = interface;
  IGeniusTransactionDiscount = interface;
  IGeniusSignatureResponse = interface;
  IGeniusLineItems = interface;
  IGeniusLineItem = interface;
  IGeniusLineItemDiscount = interface;
  IGeniusStartOrderResponse = interface;
  IGeniusCancelTransactionResponse = interface;
  IGeniusPaymentDetails = interface;
  IGeniusPaymentDetail = interface;
  IGeniusStatusResponse = interface;
  IGeniusAgreementResponse = interface;


  TGeniusTransactionEvent = procedure(Response: IGeniusTransactionResponse) of object; // stdcall;

  TGeniusStatusResponseEvent = procedure(Sender: IGenius; const Status: IGeniusStatusResponse) of object;

  TGeniusStatusCheckProc = function(const Timeout: Integer = 0): IGeniusStatusResponse of object;

  ///	<summary>
  ///	  Provides integration with Cayan's Genius solution using a
  ///   CED (Customer Experience Device) for processing payments.
  ///	</summary>
  ///	<remarks>
  ///   This service works very different from other processing services because
  ///   all processing is done within the device itself, and host app only
  ///   receives information about success/failure/details rather than the host
  ///   forwarding transactions directly to a central server.
  ///	</remarks>
  IGenius = interface
    ['{A40ADB6D-402F-4144-98A9-A58E6106F45A}']
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
    property Dba: String read GetDba write SetDba;
    property SoftwareName: String read GetSoftwareName write SetSoftwareName;
    property SoftwareVersion: String read GetSoftwareVersion write SetSoftwareVersion;
    property DeviceTimeout: Integer read GetDeviceTimeout write SetDeviceTimeout;
    property Monitoring: Boolean read GetMonitoring write SetMonitoring;

    property TransactionResponse: TGeniusTransactionEvent read GetTransactionResponse write SetTransactionResponse;
    property OnStatus: TGeniusStatusResponseEvent read GetOnStatus write SetOnStatus;
  end;

  IGeniusSignaturePoint = interface
    ['{FE06A17F-0925-4773-9479-6B82118AC2A3}']
    function GetPosX: Integer;
    procedure SetPosX(const Value: Integer);
    function GetPosY: Integer;
    procedure SetPosY(const Value: Integer);

    property PosX: Integer read GetPosX write SetPosX;
    property PosY: Integer read GetPosY write SetPosY;
  end;

  IGeniusSignaturePoints = interface
    ['{D13D9369-58DC-4DE4-8968-F235327A0B43}']
    function GetItem(const Index: Integer): IGeniusSignaturePoint;
    procedure SetItem(const Index: Integer; const Item: IGeniusSignaturePoint);

    property Items[const Index: Integer]: IGeniusSignaturePoint
      read GetItem write SetItem; default;
  end;

  TGeniusSignatureRelated = (srTransaction, srInvoice, srAgreement, srOther);

  IGeniusSignature = interface
    ['{8B2C854A-0831-4E2D-8BF5-CAE952923BCE}']
    function GetRelated: TGeniusSignatureRelated;
    procedure SetRelated(const Value: TGeniusSignatureRelated);
    function GetPoints: IGeniusSignaturePoints;
    procedure SetPoints(const Value: IGeniusSignaturePoints);

    property Related: TGeniusSignatureRelated read GetRelated write SetRelated;
    property Points: IGeniusSignaturePoints read GetPoints write SetPoints;
  end;

  IGeniusStageRequest = interface
    ['{D5F46CA0-D2AE-49A0-87ED-95CED51788AE}']
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

    ///<summary>
    /// Type of transaction to be performed:
    ///</summary>
    property TransactionType: TGeniusTransactionType read GetTransactionType write SetTransactionType;

    ///<summary>
    /// Desired amount of transaction:
    ///</summary>
    property Amount: Currency read GetAmount write SetAmount;

    ///<summary>
    /// Unique ID of user, register, etc.
    ///</summary>
    property ClerkId: String read GetClerkId write SetClerkId;

    ///<summary>
    /// Invoice number
    ///</summary>
    property OrderNumber: String read GetOrderNumber write SetOrderNumber;

    ///<summary>
    /// Name of merchant as should appear on customer's card
    ///</summary>
    property Dba: String read GetDba write SetDba;

    ///<summary>
    /// Name of software processing payment
    ///</summary>
    property SoftwareName: String read GetSoftwareName write SetSoftwareName;

    ///<summary>
    /// Version of software processing payment
    ///</summary>
    property SoftwareVersion: String read GetSoftwareVersion write SetSoftwareVersion;

    ///<summary>
    /// Card holder name
    ///</summary>
    property CardHolder: String read GetCardHolder write SetCardHolder;

    ///<summary>
    /// The merchant-defined identifier for the transaction:
    ///</summary>
    property TransactionId: String read GetTransactionId write SetTransactionId;

    ///<summary>
    /// Override duplicate protection and allow the transaction to process normally
    ///</summary>
    property ForceDuplicate: Boolean read GetForceDuplicate write SetForceDuplicate;

    ///<summary>
    /// Required for level 2 transactions. The merchant-defined identifier for the customer involved in the transaction.
    ///</summary>
    property CustomerCode: String read GetCustomerCode write SetCustomerCode;

    ///<summary>
    /// Required for level 2 transactions. The customer-defined identifier declaring a purchase order for the transaction.
    ///</summary>
    property PoNumber: String read GetPoNumber write SetPoNumber;

    ///<summary>
    /// Required for level 2 transactions. The declared tax amount of the transaction.
    ///</summary>
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;

    ///<summary>
    /// Required for MasterCard - uniquely identifies a particular station number
    ///</summary>
    property TerminalId: Integer read GetTerminalId write SetTerminalId;

    //TODO: HealthCareAmountDetails


    //TODO: TipDetails


  end;

  IGeniusStageResponse = interface
    ['{90DACB78-EF52-4374-8CEE-B1E52DDF9E61}']
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

  IGeniusStageMessage = interface
    ['{52FC9A2F-FB89-413E-84E8-A7E83121B0BC}']
    function GetField: String;
    procedure SetField(const Value: String);
    function GetInformation: String;
    procedure SetInformation(const Value: String);

    property Field: String read GetField write SetField;
    property Information: String read GetInformation write SetInformation;
  end;

  IGeniusTransactionResponse = interface
    ['{66965194-1CAE-4378-9067-C00DF926B0F4}']
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
    property KeyedExpiration: TExpirationDate read GetKeyedExpiration write SetKeyedExpiration;
    property KeyedAvsStreetZipCode: String read GetKeyedAvsStreetZipCode write SetKeyedAvsStreetZipCode;
    property KeyedAvsResponse: Char read GetKeyedAvsResponse write SetKeyedAvsResponse;
    property KeyedCvResponse: Char read GetKeyedCvResponse write SetKeyedCvResponse;
    property EmvResponse: IGeniusTransactionResponseEmv read GetEmvResponse write SetEmvResponse;
  end;

  IGeniusTransactionResponseEmv = interface
    ['{6B584A76-8A12-4EEE-9FB6-7680FF9E698A}']
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

  IGeniusTransactionDiscount = interface
    ['{280251AC-033C-45F2-9565-F1A06A84D306}']
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

  IGeniusSignatureResponse = interface
    ['{01C01DE6-5178-46AA-90F7-DA7B86A8E794}']
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

  IGeniusLineItems = interface
    ['{64DFDE94-7AAC-4F92-A12C-296542AE9976}']
    function GetOrder: String;
    procedure SetOrder(const Value: String);
    function GetDisplayCustomSubtotal: String;
    procedure SetDisplayCustomSubtotal(const Value: String);
    function GetAutoTotal: Boolean;
    procedure SetAutoTotal(const Value: Boolean);

    function GetItem(const Index: Integer): IGeniusLineItem;

    //Number of line items
    function Count: Integer;
    //Whether line item display is in progress
    function InProgress: Boolean;
    //Begin line item display
    function StartOrder(const Order: String): IGeniusStartOrderResponse;
    //End line item display with another payment method
    function EndOrder(const ExternalPaymentType: TGeniusExternalPaymentType): IGeniusStartOrderResponse;
    //Add a new item
    function AddItem(const ItemTypeValue, Upc, Description: String;
      const Amount, TaxAmount: Currency;
      const Quantity: Integer = 1;
      const ItemType: TGeniusLineItemType = glSku;
      const Category: TGeniusLineItemCategory = glNone;
      const DisplayOverride: String = ''): IGeniusLineItem; overload;
    //Update an existing item
    function UpdateItem(const TargetItemId, ItemTypeValue, Upc, Description: String;
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
    //Remove an item by its internal index
    procedure DeleteItem(const Index: Integer); overload;
    procedure DeleteItem(const ItemID: String); overload;
    procedure DeleteDiscount(const ADiscount: IGeniusLineItemDiscount); deprecated;
    function UpdateTotal(const OrderTotal, OrderTax: Currency): Boolean;
    //Clear all items from line item display
    procedure ClearItems;
    function OrderTotal: Currency;
    function OrderTax: Currency;

    property Items[const Index: Integer]: IGeniusLineItem read GetItem; default;

    property AutoTotal: Boolean read GetAutoTotal write SetAutoTotal;
    property Order: String read GetOrder write SetOrder;
    property DisplayCustomSubtotal: String read GetDisplayCustomSubtotal write SetDisplayCustomSubtotal;

  end;

  IGeniusLineItem = interface
    ['{0DE9EB6E-43EE-46B7-93BB-133B732D814E}']
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

    property Order: String read GetOrder write SetOrder;  //1-8
    property ItemID: String read GetItemID write SetItemID; //1-8
    property ItemType: TGeniusLineItemType read GetItemType write SetItemType;  //1-8
    property ItemTypeValue: String read GetItemTypeValue write SetItemTypeValue;  //1-32
    property UPC: String read GetUPC write SetUPC;  //1-32
    property Quantity: Integer read GetQuantity write SetQuantity;  //1-3
    property Description: String read GetDescription write SetDescription;  //1-35
    property Amount: Currency read GetAmount write SetAmount; //1-9
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;  //1-9
    property Category: TGeniusLineItemCategory read GetCategory write SetCategory;  //1-8
    property DisplayOverride: String read GetDisplayOverride write SetDisplayOverride;  //0-35
  end;

  IGeniusLineItemDiscount = interface
    ['{0BC05265-933B-40B9-A943-F72876444853}']
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

    property Order: String read GetOrder write SetOrder;  //1-8
    property ItemID: String read GetItemID write SetItemID; //1-8
    property TargetItemID: String read GetTargetItemID write SetTargetItemID; //1-8
    property ItemType: TGeniusLineItemType read GetItemType write SetItemType;  //1-8
    property ItemTypeValue: String read GetItemTypeValue write SetItemTypeValue;  //1-32
    property UPC: String read GetUPC write SetUPC;  //1-32
    property Quantity: Integer read GetQuantity write SetQuantity;  //1-3
    property Description: String read GetDescription write SetDescription;  //1-35
    property Amount: Currency read GetAmount write SetAmount; //1-9
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;  //1-9
    property OrderTotal: Currency read GetOrderTotal write SetOrderTotal; //1-9
    property OrderTax: Currency read GetOrderTax write SetOrderTax; //1-9
    property Category: TGeniusLineItemCategory read GetCategory write SetCategory;  //1-8
    property DisplayOverride: String read GetDisplayOverride write SetDisplayOverride;  //0-35
  end;

  IGeniusStartOrderResponse = interface
    ['{FA3B8662-B5B1-4A54-B17C-76E4428206B0}']
    function GetResponseMessage: String;
    procedure SetResponseMessage(const Value: String);
    function GetStatus: TGeniusLineItemStatus;
    procedure SetStatus(const Value: TGeniusLineItemStatus);

    property Status: TGeniusLineItemStatus read GetStatus write SetStatus;
    property ResponseMessage: String read GetResponseMessage write SetResponseMessage;
  end;

  IGeniusCancelTransactionResponse = interface
    ['{7E01DB03-53E3-4A65-AB26-76CA29B69B61}']
    function GetResponseMessage: String;
    procedure SetResponseMessage(const Value: String);
    function GetStatus: TGeniusCancelTransactionStatus;
    procedure SetStatus(const Value: TGeniusCancelTransactionStatus);

    property Status: TGeniusCancelTransactionStatus read GetStatus write SetStatus;
    property ResponseMessage: String read GetResponseMessage write SetResponseMessage;
  end;

  IGeniusPaymentDetails = interface
    ['{BDDE212A-8A65-4945-98AB-60F765BC6464}']
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

  IGeniusPaymentDetail = interface
    ['{61F8037E-A206-4A23-84CF-6F5D1C19EDEB}']
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

  IGeniusStatusResponse = interface
    ['{729D5B96-6311-488D-826E-113BDCC22F5A}']
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

  IGeniusAgreementResponse = interface
    ['{7A30A50F-DECE-4629-B906-E57C70D02CAC}']
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

function GeniusTransactionTypeToStr(const Value: TGeniusTransactionType): String;
function GeniusStrToTransactionType(const Value: String): TGeniusTransactionType;
function GeniusPaymentTypeToStr(const Value: TGeniusPaymentType): String;
function GeniusStrToPaymentType(const Value: String): TGeniusPaymentType;
function GeniusTransStatusToStr(const Value: TGeniusTransStatus): String;
function GeniusStrToTransStatus(const Value: String): TGeniusTransStatus;
function GeniusEntryModeToStr(const Value: TGeniusentryMode): String;
function GeniusStrToEntryMode(const Value: String): TGeniusEntryMode;
function GeniusResponseTypeToStr(const Value: TGeniusResponseType): String;
function GeniusStrToResponseType(const Value: String): TGeniusResponseType;
function GeniusLineItemTypeToStr(const Value: TGeniusLineItemType): String;
function GeniusStrToLineItemType(const Value: String): TGeniusLineItemType;
function GeniusLineItemCategoryToStr(const Value: TGeniusLineItemCategory): String;
function GeniusStrToLineItemCategory(const Value: String): TGeniusLineItemCategory;
function GeniusCancelTransactionStatusToStr(const Value: TGeniusCancelTransactionStatus): String;
function GeniusStrToCancelTransactionStatus(const Value: String): TGeniusCancelTransactionStatus;
function GeniusKeyedSaleStatusToStr(const Value: TGeniusKeyedSaleStatus): String;
function GeniusStrToKeyedSaleStatus(const Value: String): TGeniusKeyedSaleStatus;
function GeniusSignatureStatusToStr(const Value: TGeniusSignatureStatus): String;
function GeniusStrToSignatureStatus(const Value: String): TGeniusSignatureStatus;
function GeniusCedStatusToStr(const Value: TGeniusCedStatus): String;
function GeniusStrToCedStatus(const Value: String): TGeniusCedStatus;
function GeniusCedScreenToStr(const Value: TGeniusCedScreen): String;
function GeniusCedScreenToInt(const Value: TGeniusCedScreen): Integer;
function GeniusStrToCedScreen(const Value: String): TGeniusCedScreen;
function GeniusSignatureTypeToStr(const Value: TGeniusSignatureType): String;
function GeniusStrToSignatureType(const Value: String): TGeniusSignatureType;
function GeniusStrToCvmMethod(const Value: String): TGeniusCvmMethod;
function GeniusCvmMethodToStr(const Value: TGeniusCvmMethod): String;
function GeniusStrToCryptogramType(const Value: String): TGeniusCryptogramType;
function GeniusCryptogramTypeToStr(const Value: TGeniusCryptogramType): string;
function GeniusStrToEmvTransactionType(const Value: String): TGeniusEmvTransactionType;
function GeniusEmvTransactionTypeToStr(const Value: TGeniusEmvTransactionType): String;
function GeniusStrToLineItemStatus(const Value: String): TGeniusLineItemStatus;
function GeniusLineItemStatusToStr(const Value: TGeniusLineItemStatus): String;
function GeniusStrToExternalPaymentType(const Value: String): TGeniusExternalPaymentType;
function GeniusExternalPaymentTypeToStr(const Value: TGeniusExternalPaymentType): String;
function GeniusStrToBatchStatus(const Value: String): TGeniusBatchStatus;
function GeniusBatchStatusToStr(const Value: TGeniusBatchStatus): String;
function GeniusStrToAgreementResponseStatus(const Value: String): TGeniusAgreementResponseStatus;
function GeniusAgreementResponseStatusToStr(const Value: TGeniusAgreementResponseStatus): String;

implementation

uses
  StrUtils, DateUtils;

function GeniusTransactionTypeToStr(const Value: TGeniusTransactionType): String;
begin
  case Value of
    gtSale:           Result:= 'SALE';
    gtRefund:         Result:= '_REFUND';
    gtLevel2Sale:     Result:= 'LEVEL2SALE';
    gtPreAuth:        Result:= 'PREAUTH';
    gtPostAuth:       Result:= 'POSTAUTH';
    gtForceSale:      Result:= 'FORCESALE';
    gtAddValue:       Result:= 'ADDVALUE';
    gtBalanceInquiry: Result:= 'BALANCEINQUIRY';
    gtUnknown:        Result:= 'UNKNOWN';
    else              Result:= 'SALE';
  end;
end;

function GeniusStrToTransactionType(const Value: String): TGeniusTransactionType;
begin
  if SameText(Value, 'SALE') then
    Result:= gtSale
  else if SameText(Value, '_REFUND') then
    Result:= gtRefund
  else if SameText(Value, 'LEVEL2SALE') then
    Result:= gtLevel2Sale
  else if SameText(Value, 'PREAUTH') then
    Result:= gtPreAuth
  else if SameText(Value, 'POSTAUTH') then
    Result:= gtPostAuth
  else if SameText(Value, 'FORCESALE') then
    Result:= gtForceSale
  else if SameText(Value, 'ADDVALUE') then
    Result:= gtAddValue
  else if SameText(Value, 'BALANCEINQUIRY') then
    Result:= gtBalanceInquiry
  else if SameText(Value, 'UNKNOWN') then
    Result:= gtUnknown
  else
    Result:= gtSale;
end;

function GeniusPaymentTypeToStr(const Value: TGeniusPaymentType): String;
begin
  case Value of
    gpVisa:       Result:= 'VISA';
    gpMastercard: Result:= 'MASTERCARD';
    gpAmex:       Result:= 'AMEX';
    gpDiscover:   Result:= 'DISCOVER';
    gpDebit:      Result:= 'DEBIT';
    gpGift:       Result:= 'GIFT';
    gpEbt:        Result:= 'EBT';
    gpLevelUp:    Result:= 'LEVELUP';
    gpVoyager:    Result:= 'VOYAGER';
    gpWex:        Result:= 'WEX';
    gpJcb:        Result:= 'JCB';
    gpCup:        Result:= 'CUP';
    gpUnknown:    Result:= 'UNKNOWN';
  end;
end;

function GeniusStrToPaymentType(const Value: String): TGeniusPaymentType;
begin
  if SameText(Value, 'VISA') then
    Result:= gpVisa
  else if SameText(Value, 'MASTERCARD') then
    Result:= gpMasterCard
  else if SameText(Value, 'AMEX') then
    Result:= gpAmex
  else if SameText(Value, 'DISCOVER') then
    Result:= gpDiscover
  else if SameText(Value, 'DEBIT') then
    Result:= gpDebit
  else if SameText(Value, 'GIFT') then
    Result:= gpGift
  else if SameText(Value, 'EBT') then
    Result:= gpEbt
  else if SameText(Value, 'LEVELUP') then
    Result:= gpLevelUp
  else if SameText(Value, 'VOYAGER') then
    Result:= gpVoyager
  else if SameText(Value, 'WEX') then
    Result:= gpWex
  else if SameText(Value, 'JCB') then
    Result:= gpJcb
  else if SameText(Value, 'CUP') then
    Result:= gpCup
  else if SameText(Value, 'UNKNOWN') then
    Result:= gpUnknown
  else
    Result:= gpUnknown;
end;

function GeniusTransStatusToStr(const Value: TGeniusTransStatus): String;
begin
  case Value of
    gsApproved:       Result:= 'Approved';
    gsDeclined:       Result:= 'Declined';
    gsError:          Result:= 'Error';
    gsUserCancelled:  Result:= 'UserCancelled';
    gsPosCancelled:   Result:= 'POSCancelled';
    gsDuplicate:      Result:= 'DECLINED_DUPLICATE';
    else              Result:= 'Error';
  end;
end;

function GeniusStrToTransStatus(const Value: String): TGeniusTransStatus;
begin
  if Pos('Fail', Value) > 0 then
    Result:= gsError
  else if SameText(Value, 'Approved') then
    Result:= gsApproved
  else if SameText(Value, 'Declined') then
    Result:= gsDeclined
  else if SameText(Value, 'Error') then
    Result:= gsError
  else if SameText(Value, 'UserCancelled') then
    Result:= gsUserCancelled
  else if SameText(Value, 'POSCancelled') then
    Result:= gsPosCancelled
  else if SameText(Value, 'DECLINED_DUPLICATE') then
    Result:= gsDuplicate
  else
    Result:= gsError;
end;

function GeniusEntryModeToStr(const Value: TGeniusentryMode): String;
begin
  case Value of
    geSwipe:      Result:= 'SWIPE';
    geProximity:  Result:= 'PROXIMITY';
    geBarcode:    Result:= 'BARCODE';
    geManual:     Result:= 'MANUAL';
    else          Result:= 'SWIPE';
  end;
end;

function GeniusStrToEntryMode(const Value: String): TGeniusEntryMode;
begin
  if SameText(Value, 'SWIPE') then
    Result:= geSwipe
  else if SameText(Value, 'PROXIMITY') then
    Result:= geProximity
  else if SameText(Value, 'BARCODE') then
    Result:= geBarcode
  else if SameText(Value, 'MANUAL') then
    Result:= geManual
  else
    Result:= geSwipe;
end;

function GeniusResponseTypeToStr(const Value: TGeniusResponseType): String;
begin
  case Value of
    grSingle:     Result:= 'SINGLE';
    grMulti:      Result:= 'MULTI';
    grCompound:   Result:= 'COMPOUND';
    else          Result:= 'SINGLE';
  end;
end;

function GeniusStrToResponseType(const Value: String): TGeniusResponseType;
begin
  if SameText(Value, 'SINGLE') then
    Result:= grSingle
  else if SameText(Value, 'MULTI') then
    Result:= grMulti
  else if SameText(Value, 'COMPOUND') then
    Result:= grCompound
  else
    Result:= grSingle;
end;

function GeniusLineItemTypeToStr(const Value: TGeniusLineItemType): String;
begin
  case Value of
    glSku:  Result:= 'Sku';
    glItem: Result:= 'AddItem';
    else    Result:= 'AddItem';
  end;
end;

function GeniusStrToLineItemType(const Value: String): TGeniusLineItemType;
begin
  if SameText(Value, 'Sku') then
    Result:= glSku
  else if SameText(Value, 'AddItem') then
    Result:= glItem
  else
    Result:= glItem;
end;

function GeniusLineItemCategoryToStr(const Value: TGeniusLineItemCategory): String;
begin
  case Value of
    glNone: Result:= 'None';
    glEbt:  Result:= 'Ebt';
    glFuel: Result:= 'Fuel';
    else    Result:= 'None';
  end;
end;

function GeniusStrToLineItemCategory(const Value: String): TGeniusLineItemCategory;
begin
  if SameText(Value, 'None') then
    Result:= glNone
  else if SameText(Value, 'Ebt') then
    Result:= glEbt
  else if SameText(Value, 'Fuel') then
    Result:= glFuel
  else
    Result:= glNone;
end;

function GeniusCancelTransactionStatusToStr(const Value: TGeniusCancelTransactionStatus): String;
begin
  case Value of
    ctCancelled:            Result:= 'Cancelled';
    ctApprovedNoSignature:  Result:= 'TransactionApproved_NoSignatureCollected';
    ctDenied:               Result:= 'Denied';
    ctError:                Result:= 'Error';
    else                    Result:= 'Error'
  end;
end;

function GeniusStrToCancelTransactionStatus(const Value: String): TGeniusCancelTransactionStatus;
begin
  if SameText(Value, 'Cancelled') then
    Result:= ctCancelled
  else if SameText(Value, 'TransactionApproved_NoSignatureCollected') then
    Result:= ctApprovedNoSignature
  else if SameText(Value, 'Denied') then
    Result:= ctDenied
  else if SameText(Value, 'Error') then
    Result:= ctError
  else
    Result:= ctError;
end;

function GeniusKeyedSaleStatusToStr(const Value: TGeniusKeyedSaleStatus): String;
begin
  case Value of
    gkSuccess:  Result:= 'Success';
    gkFailure:  Result:= 'Failure';
    gkDeclined: Result:= 'Declined';
    else        Result:= 'Failure';
  end;
end;

function GeniusStrToKeyedSaleStatus(const Value: String): TGeniusKeyedSaleStatus;
begin
  if SameText(Value, 'Success') then
    Result:= gkSuccess
  else if SameText(Value, 'Failure') then
    Result:= gkFailure
  else if SameText(Value, 'Declined') then
    Result:= gkDeclined
  else
    Result:= gkFailure;
end;

function GeniusSignatureStatusToStr(const Value: TGeniusSignatureStatus): String;
begin
  case Value of
    ssSuccess:      Result:= 'Success';
    ssTimeout:      Result:= 'Timeout';
    ssPosCancelled: Result:= 'POSCancelled';
    else            Result:= 'Timeout';
  end;
end;

function GeniusStrToSignatureStatus(const Value: String): TGeniusSignatureStatus;
begin
  if SameText(Value, 'Success') then
    Result:= ssSuccess
  else if SameText(Value, 'Timeout') then
    Result:= ssTimeout
  else if SameText(Value, 'POSCancelled') then
    Result:= ssPosCancelled
  else
    Result:= ssTimeout;
end;

function GeniusCedStatusToStr(const Value: TGeniusCedStatus): String;
begin
  //NOTE: Offline is not an official property
  case Value of
    csOffline:        Result:= 'Offline';
    csOnline:         Result:= 'Online';
    csDownloadNeeded: Result:= 'DOWNLOAD_NEEDED';
    else              Result:= 'Offline';
  end;
end;

function GeniusStrToCedStatus(const Value: String): TGeniusCedStatus;
begin
  //NOTE: Offline is not an official property
  if SameText(Value, 'Online') then
    Result:= csOnline
  else if SameText(Value, 'DOWNLOAD_NEEDED') then
    Result:= csDownloadNeeded
  else
    Result:= csOffline;
end;

function GeniusCedScreenToStr(const Value: TGeniusCedScreen): String;
begin
  case Value of
    csNoScreen:               Result:= 'No Screen Display';
    csIdle:                   Result:= 'Idle Screen';
    csValidatingTransportKey: Result:= 'Validating TransportKey';
    csMainPayment:            Result:= 'Main Payment Collection';
    csCustomPayment:          Result:= 'Custom Payment Collection';
    csLookupCardBin:          Result:= 'Looking Up Card BIN';
    csWaitingForAmount:       Result:= 'Waiting for Amount Confirmation';
    csWaitingForPin:          Result:= 'Waiting for PIN Entry';
    csProcessing:             Result:= 'Processing Payment';
    csWaitingForSignature:    Result:= 'Waiting for Signature';
    csProcessingSignature:    Result:= 'Processing Signature Capture';
    csTransactionResult:      Result:= 'Transaction Result';
    csCancelConfirm:          Result:= 'Cancel Confirmation';
    csRunAsCreditConfirm:     Result:= 'Run as Credit Confirmation';
    csSkuDisplay:             Result:= 'SKU Display';
    csCashbackSelection:      Result:= 'Cashback Selection';
    csCashbackCustom:         Result:= 'Custom Cashback Selection';
    csTipSelection:           Result:= 'Tip Selection';
    csTipCustom:              Result:= 'Custom Tip Selection';
    csDonationSelection:      Result:= 'Donation Selection';
    csDonationCustom:         Result:= 'Custom Donation Selection';
    csConfirmation:           Result:= 'Confirmation Screen';
    csErrorScreen:            Result:= 'Error Screen';
    csSkuAmountConfirm:       Result:= 'SKU Amount Confirmation';
    csPanEntry:               Result:= 'PAN Entry';
    csExpirationEntry:        Result:= 'Expiration Entry';
    csCvvEntry:               Result:= 'CVV Entry';
    csZipEntry:               Result:= 'Zip Code Entry';
    else                      Result:= 'No Screen Display';
  end;
end;

function GeniusSignatureTypeToStr(const Value: TGeniusSignatureType): String;
begin
  case Value of
    gsVector: Result:= 'Vector';
    gsBitmap: Result:= 'Bitmap';
    else Result:= 'Vector';
  end;
end;

function GeniusStrToSignatureType(const Value: String): TGeniusSignatureType;
begin
  if SameText(Value, 'Vector') then
    Result:= gsVector
  else if SameText(Value, 'Bitmap') then
    Result:= gsBitmap
  else
    Result:= gsVector;
end;

function GeniusCedScreenToInt(const Value: TGeniusCedScreen): Integer;
begin
  Result:= Integer(Value);
end;

function GeniusStrToCedScreen(const Value: String): TGeniusCedScreen;
var
  I: Integer;
begin
  I:= StrToIntDef(Value, 0);
  Result:= TGeniusCedScreen(I);
end;

function GeniusStrToCvmMethod(const Value: String): TGeniusCvmMethod;
begin
  if SameText(Value, 'SIGNATURE') then
    Result:= cmSignature
  else if SameText(Value, 'OFFLINE PIN') then
    Result:= cmOfflinePin
  else if SameText(Value, 'ONLINE PIN') then
    Result:= cmOnlinePin
  else if SameText(Value, 'NONE') then
    Result:= cmNone
  else if SameText(Value, 'OTHER') then
    Result:= cmOther
  else
    Result:= cmNone;
end;

function GeniusCvmMethodToStr(const Value: TGeniusCvmMethod): String;
begin
  case Value of
    cmSignature:  Result:= 'SIGNATURE';
    cmOfflinePin: Result:= 'OFFLINE PIN';
    cmOnlinePin:  Result:= 'ONLINE PIN';
    cmNone:       Result:= 'NONE';
    cmOther:      Result:= 'OTHER';
    else          Result:= 'NONE';
  end;
end;

function GeniusStrToCryptogramType(const Value: String): TGeniusCryptogramType;
begin
  if SameText(Value, 'ARQC') then
    Result:= ctARQC
  else if SameText(Value, 'TC') then
    Result:= ctTC
  else if SameText(Value, 'AAC') then
    Result:= ctAAC
  else
    Result:= ctARQC;
end;

function GeniusCryptogramTypeToStr(const Value: TGeniusCryptogramType): string;
begin
  case Value of
    ctARQC: Result:= 'ARQC';
    ctTC:   Result:= 'TC';
    ctAAC:  Result:= 'AAC';
    else    Result:= 'ARQC';
  end;
end;

function GeniusStrToEmvTransactionType(const Value: String): TGeniusEmvTransactionType;
begin
  if SameText(Value, 'CASH') then
    Result:= etCash
  else if SameText(Value, 'GOODS') then
    Result:= etGoods
  else if SameText(Value, 'SERVICES') then
    Result:= etServices
  else if SameText(Value, 'CASHBACK') then
    Result:= etCashback
  else if SameText(Value, 'INQUIRY') then
    Result:= etInquiry
  else if SameText(Value, 'PAYMENT') then
    Result:= etPayment
  else if SameText(Value, 'ADMINISTRATIVE') then
    Result:= etAdministrative
  else
    Result:= etCash;
end;

function GeniusEmvTransactionTypeToStr(const Value: TGeniusEmvTransactionType): String;
begin
  case Value of
    etCash:           Result:= 'CASH';
    etGoods:          Result:= 'GOODS';
    etServices:       Result:= 'SERVICES';
    etCashback:       Result:= 'CASHBACK';
    etInquiry:        Result:= 'INQUIRY';
    etPayment:        Result:= 'PAYMENT';
    etAdministrative: Result:= 'ADMINISTRATIVE';
    else              Result:= 'CASH';
  end;
end;

function GeniusStrToLineItemStatus(const Value: String): TGeniusLineItemStatus;
begin
  if SameText(Value, 'Success') then
    Result:= soSuccess
  else if SameText(Value, 'Denied') then
    Result:= soDenied
  else if SameText(Value, 'Error') then
    Result:= soError
  else
    Result:= soError;
end;

function GeniusLineItemStatusToStr(const Value: TGeniusLineItemStatus): String;
begin
  case Value of
    soSuccess:  Result:= 'Success';
    soDenied:   Result:= 'Denied';
    soError:    Result:= 'Error';
    else        Result:= 'Error';
  end;
end;

function GeniusStrToExternalPaymentType(const Value: String): TGeniusExternalPaymentType;
begin
  if SameText(Value, 'Cash') then
    Result:= epCash
  else if SameText(Value, 'Check') then
    Result:= epCheck
  else if SameText(Value, 'StoreCredit') then
    Result:= epStoreCredit
  else if SameText(Value, 'Other') then
    Result:= epOther
  else
    Result:= epOther;
end;

function GeniusExternalPaymentTypeToStr(const Value: TGeniusExternalPaymentType): String;
begin
  case Value of
    epCash:         Result:= 'Cash';
    epCheck:        Result:= 'Check';
    epStoreCredit:  Result:= 'StoreCredit';
    epOther:        Result:= 'Other';
    else            Result:= 'Other';
  end;
end;

function GeniusStrToBatchStatus(const Value: String): TGeniusBatchStatus;
begin
  if SameText(Value, 'SUCCESS') then
    Result:= bsSuccess
  else if SameText(Value, 'FAILURE') then
    Result:= bsFailure
  else
    Result:= bsFailure;
end;

function GeniusBatchStatusToStr(const Value: TGeniusBatchStatus): String;
begin
  case Value of
    bsSuccess: Result:= 'SUCCESS';
    bsFailure: Result:= 'FAILURE';
    else       Result:= 'FAILURE';
  end;
end;

function GeniusStrToAgreementResponseStatus(const Value: String): TGeniusAgreementResponseStatus;
begin
  if SameText(Value, 'Accepted') then
    Result:= arsAccepted
  else if SameText(Value, 'Declined') then
    Result:= arsDeclined
  else if SameText(Value, 'Timeout') then
    Result:= arsTimeout
  else if SameText(Value, 'POSCancelled') then
    Result:= arsPOSCancelled
  else if SameText(Value, 'Error') then
    Result:= arsError
  else
    Result:= arsError;
end;

function GeniusAgreementResponseStatusToStr(const Value: TGeniusAgreementResponseStatus): String;
begin
  case Value of
    arsAccepted:     Result:= 'Accepted';
    arsDeclined:     Result:= 'Declined';
    arsTimeout:      Result:= 'Timeout';
    arsPOSCancelled: Result:= 'POSCancelled';
    arsError:        Result:= 'Error';
    else begin
      Result:= 'Error'
    end;
  end;
end;

end.
