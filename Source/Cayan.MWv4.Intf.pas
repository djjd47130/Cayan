unit Cayan.MWv4.Intf;

interface

uses
  System.Classes, System.SysUtils,
  Cayan.Common;

type
  IMerchantWare = interface;
  //MerchantWare 4 Endpoints
  IMWTransactionBase = interface;
  IMWCreditTransactions = interface;
  IMWVaultTransactions = interface;
  IMWEbtTransactions = interface;
  IMWGiftCardTransactions = interface;
  IMWLevelUpTransactions = interface;
  IMWCheckTransactions = interface;
  IMWReportTransactions = interface;
  //MerchantWare 4 Credit Data Structures
  IMWBatchResponse4 = interface;
  IMWCreditLevel2Response4 = interface;
  IMWCreditResponse4 = interface;
  IMWEmvAuthorizeResponse4 = interface;
  IMWEmvCompleteResponse4 = interface;
  IMWSignatureResponse4 = interface;
  IMWVaultBoardingResponse = interface;
  IMWVaultPaymentInfoResponse = interface;
  //MerchantWare 4 Report Data Structures
  IMWDetailedTransactionReference4 = interface;
  IMWEmv = interface;
  IMWEmvApplicationInformation = interface;
  IMWEmvCardInformation = interface;
  IMWEmvApplicationCryptogram = interface;
  IMWEmvAmount = interface;
  IMWEmvTerminalInformation = interface;
  IMWEmvTransactionInformation = interface;
  IMWHealthCareAmountDetails = interface;
  IMWSupportedActions = interface;
  IMWTransactionReference4List = interface;
  IMWTransactionReference4 = interface;
  IMWTransactionSummary4List = interface;
  IMWTransactionSummary4 = interface;

  TMWApprovalStatus = (asApproved, asDeclined, asDuplicate, asReferral, asFailed);
  TMWApprovalStatusSet = set of TMWApprovalStatus;

  TMWBatchStatus = (bsAccepted, bsSuccess, bsFailed, bsDeclined, bsEmpty);
  TMWBatchStatusSet = set of TMWBatchStatus;

  TMWEmvApprovalStatus = (easUnknown, easApproved, easFailed, easDeclined,
    easDuplicate, easReferral);
  TMWEmvApprovalStatusSet = set of TMWEmvApprovalStatus;

  TMWCardType = (ctUnknown = 0, ctAmex = 1, ctDiscover = 2, ctMasterCard = 3,
    ctVisa = 4, ctDebit = 5, ctEbt = 6, ctEgc = 7, ctWex = 8, ctVoyager = 9,
    ctJcb = 10, ctCup = 11, ctLvlUp = 12);

  TMWEbtBasisType = (ebUnknown = 0, ebFoodStamp = 1, ebCashBenefit = 2);

  TMWEmvEntryType = (eeContact, eeContactless);

  TMWPosEntryType = (peUnknown = 0, peKeyed = 1, peSwipe = 2,
    peAuthorization = 3, peProximity = 4, peLvlUp = 5);

  TMWSignatureType = (stUnknown = 0, stTiffZip = 1, stVectorText = 2);

  TMWSignatureStatus = (ssAccepted, ssFailed);

  TMWTransactionType = (ttUnknown = 0, ttSale = 1, ttRefund = 2, ttVoid = 3,
    ttForce = 4, ttAuth = 5, ttCapture = 6, ttAdjust = 7, ttRepeatSale = 8,
    ttPostAuth = 9, ttLevelUpSale = 11, ttLevelUpCredit = 12);
                            {
  TMWGiftTransactionType = (gtUnknown = 0, gtRedemption = 1, gtAddValue = 2,
    gtRedeemPoints = 3, gtAddPoints = 4, gtBalanceInquiry = 5,
    gtActivationPurchase = 6, gtActivationReturn = 7, gtCredit = 8,
    gtBalanceTransfer = 9, gtAddTip = 10, gtVoid = 11, gtTotalsInquiry = 12,
    gtRedemptionForce = 19);        } //TODO

  TMWCheckVerificationMethod = (cvDriversLicense, cvKeyed, cvSwiped,
    cvKeyedWithDriversLicense, cvSwipedWithDriversLicense);

  TMWCheckType = (ctPersonal, ctCompany);

  TMWEntryMode = (emMagneticStripe, emProximity);

  IMerchantWare = interface
    ['{50DAD05C-067C-4477-A838-F07E14D1FAAF}']
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

  IMWTransactionBase = interface
    ['{7FA9AA86-E031-438E-A315-FABAA2771CBB}']
    function GetOwner: IMerchantWare;

    property Owner: IMerchantWare read GetOwner;
  end;

  IMWCreditTransactions = interface(IMWTransactionBase)
    ['{0EAEDAC3-4D40-4892-96BF-CE980DA0BBD4}']
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
      const RegisterNumber, MerchantTransactionId: String): IMWCreditResponse4; deprecated;
  end;

  IMWVaultTransactions = interface(IMWTransactionBase)
    ['{1E863720-D2E2-4244-A975-5FA7A0B2835C}']
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

  IMWEbtTransactions = interface(IMWTransactionBase)
    ['{F73F2A21-48BB-486A-A376-D85A4EFBD947}']

  end;

  IMWGiftCardTransactions = interface(IMWTransactionBase)
    ['{72488C80-B7C3-4094-837B-7E817558E9E4}']

  end;

  IMWLevelUpTransactions = interface(IMWTransactionBase)
    ['{8A704B6B-DEE3-4861-85AE-AAE6717E4242}']

  end;

  IMWCheckTransactions = interface(IMWTransactionBase)
    ['{7AA7A6F3-3F6E-4D09-A7BE-CEB0989F86CC}']

  end;

  IMWReportTransactions = interface(IMWTransactionBase)
    ['{77A9B94E-2543-402A-962E-D8D4FF906AC9}']
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

  IMWBatchResponse4 = interface
    ['{4522516C-8A3B-4234-8859-E229CB082252}']
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

  IMWCreditLevel2Response4 = interface
    ['{C07EF505-D535-46CA-BA6F-D306565E8342}']
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

  IMWCreditResponse4 = interface
    ['{32B68EB0-2A1F-4715-8C74-4CD9D8E64E59}']
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

  IMWEmvAuthorizeResponse4 = interface
    ['{EAD451B6-93D1-4404-9BB8-67D032EFBF9D}']
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

  IMWEmvCompleteResponse4 = interface
    ['{57A895DF-7164-408A-931B-BE914104264C}']
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

  IMWSignatureResponse4 = interface
    ['{C30E1F34-A573-42CD-A3D5-0DC64D4ADE20}']
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

  IMWVaultBoardingResponse = interface
    ['{E65F7C5B-FA09-4617-81E1-E2BC384212F5}']
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

  IMWVaultPaymentInfoResponse = interface
    ['{67668CE1-15EA-4B88-B8E3-E10167010A1D}']
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

  IMWGiftResponse4 = interface
    ['{0C45FD21-77A8-4EFD-AF3B-90FA8E4F841D}']
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

  IMWGiftLoyaltyResponse4 = interface
    ['{A71C73AB-965F-4B46-B611-BC0EBE49E8FD}']
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

  IMWDetailedTransactionReference4 = interface
    ['{6CA69D23-5A5B-4EBF-800A-CB00C7E6EDAD}']
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

  IMWEmv = interface
    ['{D83E533E-2FFC-4DBB-96BA-5F02287DA12B}']
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

  IMWEmvApplicationInformation = interface
    ['{E1B78D3A-E9AB-4B0A-9C19-DCDB33E2872A}']
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

  IMWEmvCardInformation = interface
    ['{4BEFE521-663B-4B5F-94E7-907C604F1188}']
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

  IMWEmvApplicationCryptogram = interface
    ['{FE21C0A0-33BC-4EC6-BE55-A1F7F3298750}']
    function GetCryptogram: String;
    function GetCryptogramType: String;
    procedure SetCryptogram(const Value: String);
    procedure SetCryptogramType(const Value: String);

    property CryptogramType: String read GetCryptogramType write SetCryptogramType;
    property Cryptogram: String read GetCryptogram write SetCryptogram;
  end;

  IMWEmvAmount = interface
    ['{0FE3F516-DBA7-48AA-9C38-B6E24989ADAB}']
    function GetAmountAuthorized: Currency;
    function GetAmountOther: Currency;
    procedure SetAmountAuthorized(const Value: Currency);
    procedure SetAmountOther(const Value: Currency);

    property AmountAuthorized: Currency read GetAmountAuthorized write SetAmountAuthorized;
    property AmountOther: Currency read GetAmountOther write SetAmountOther;
  end;

  IMWEmvTerminalInformation = interface
    ['{E94100C5-76F8-4A11-9ABB-07E2586AD0A4}']
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

  IMWEmvTransactionInformation = interface
    ['{6BB914E5-AAF1-43B4-A680-7B58915D9288}']
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

  IMWHealthCareAmountDetails = interface
    ['{895A9441-CBC9-4DD9-A5D1-FEA7402FFB73}']
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

  IMWSupportedActions = interface
    ['{D161F74E-367F-483F-BE49-AF10E782FB09}']
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

  IMWTransactionReference4List = interface
    ['{0C88074E-0D5B-4C8B-AAAB-4A6691AAB383}']
    function GetItem(Index: Integer): IMWTransactionReference4;

    function Count: Integer;
    function Add: IMWTransactionReference4;
    procedure Delete(const Index: Integer);
    procedure Clear;
    property Items[Index: Integer]: IMWTransactionReference4 read GetItem; default;
  end;

  IMWTransactionReference4 = interface
    ['{B3509C31-8FCD-4F1B-BA31-DB9BE30F56CF}']
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

  IMWTransactionSummary4List = interface
    ['{A942F247-0090-4A55-A801-4CAD6ADBE7BC}']
    function GetItem(Index: Integer): IMWTransactionSummary4;

    function Count: Integer;
    function Add: IMWTransactionSummary4;
    procedure Delete(const Index: Integer);
    procedure Clear;
    property Items[Index: Integer]: IMWTransactionSummary4 read GetItem; default;
  end;

  IMWTransactionSummary4 = interface
    ['{240A78FD-EF53-4CD6-A0B7-E7E8CF21A9EF}']
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

  //Generic Structures

  IMWTransactionList = interface
    ['{411B9FEC-B13B-4455-856F-BAFD172A7BCC}']

  end;

  IMWTransactionItem = interface
    ['{DD7BC5A9-D2B3-485D-93D4-F65B985C586D}']

  end;

function MWStrToEntryMode(const Value: String): TMWEntryMode;
function MWEntryModeToStr(const Value: TMWEntryMode): String;
function MWStrToApprovalStatusSet(const Value: String): TMWApprovalStatusSet;
function MWApprovalStatusSetToStr(const Value: TMWApprovalStatusSet): String;
function MWStrToBatchStatusSet(const Value: String): TMWBatchStatusSet;
function MWBatchStatusSetToStr(const Value: TMWBatchStatusSet): String;
function MWStrToCardType(const Value: String): TMWCardType;
function MWCardTypeToStr(const Value: TMWCardType): String;
function MWCardTypeCaption(const Value: TMWCardType): String;
function MWStrToPosEntryType(const Value: String): TMWPosEntryType;
function MWPosEntryTypeToStr(const Value: TMWPosEntryType): String;
function MWPosEntryTypeCaption(const Value: TMWPosEntryType): String;
function MWStrToTransactionType(const Value: String): TMWTransactionType;
function MWTransactionTypeToStr(const Value: TMWTransactionType): String;
function MWTransactionTypeCaption(const Value: TMWTransactionType): String;
function MWStrToEmvApprovalStatusSet(const Value: String): TMWEmvApprovalStatusSet;
function MWEmvApprovalStatusSetToStr(const Value: TMWEmvApprovalStatusSet): String;
function MWStrToSignatureType(const Value: String): TMWSignatureType;
function MWSignatureTypeToStr(const Value: TMWSignatureType): String;
function MWSignatureTypeCaption(const Value: TMWSignatureType): String;
function MWStrToSignatureStatus(const Value: String): TMWSignatureStatus;
function MWSignatureStatusToStr(const Value: TMWSignatureStatus): String;
function MWCvResponseCaption(const Value: String): String;
function MWAvsResponseCaption(const Value: String): String;

implementation

function MWStrToEntryMode(const Value: String): TMWEntryMode;
begin
  if SameText(Value, 'MAGNETICSTRIPE') then
    Result:= emMagneticStripe
  else if SameText(Value, '') then
    Result:= emProximity
  else
    Result:= emMagneticStripe;
end;

function MWEntryModeToStr(const Value: TMWEntryMode): String;
begin
  case Value of
    emMagneticStripe: Result:= 'MAGNETICSTRIPE';
    emProximity:      Result:= 'PROXIMITY';
    else              Result:= 'MAGNETICSTRIPE';
  end;
end;

function MWStrToApprovalStatusSet(const Value: String): TMWApprovalStatusSet;
  procedure A(N: String; V: TMWApprovalStatus);
  begin
    if Pos(N, Value) > 0 then
      Result:= Result + [V];
  end;
begin
  A('APPROVED', asApproved);
  A('FAILED', asFailed);
  A('DECLINED', asDeclined);
  A('DUPLICATE', asDuplicate);
end;

function MWApprovalStatusSetToStr(const Value: TMWApprovalStatusSet): String;
  procedure A(const S: String);
  begin
    if Length(Result) > 0 then Result:= Result + ',';
    Result:= Result + S;
  end;
begin
  Result:= '';
  if asApproved in Value then
    A('APPROVED');
  if asFailed in Value then
    A('FAILED');
  if asDeclined in Value then
    A('DECLINED');
  if asDuplicate in Value then
    A('DUPLICATE');
end;

function MWStrToBatchStatusSet(const Value: String): TMWBatchStatusSet;
  procedure A(N: String; V: TMWBatchStatus);
  begin
    if Pos(N, Value) > 0 then
      Result:= Result + [V];
  end;
begin
  A('ACCEPTED', bsAccepted);
  A('SUCCESS', bsSuccess);
  A('FAILED', bsFailed);
  A('DECLINED', bsDeclined);
  A('EMPTY', bsEmpty);
end;

function MWBatchStatusSetToStr(const Value: TMWBatchStatusSet): String;
  procedure A(const S: String);
  begin
    if Length(Result) > 0 then Result:= Result + ',';
    Result:= Result + S;
  end;
begin
  Result:= '';
  if bsAccepted in Value then
    A('ACCEPTED');
  if bsSuccess in Value then
    A('SUCCESS');
  if bsFailed in Value then
    A('FAILED');
  if bsDeclined in Value then
    A('DECLINED');
  if bsEmpty in Value then
    A('EMPTY');
end;

function MWStrToCardType(const Value: String): TMWCardType;
begin
  Result:= TMWCardType(StrToIntDef(Value, 0));
end;

function MWCardTypeToStr(const Value: TMWCardType): String;
begin
  Result:= IntToStr(Integer(Value));
end;

function MWCardTypeCaption(const Value: TMWCardType): String;
begin
  case Value of
    ctUnknown:    Result:= 'Unknown';
    ctAmex:       Result:= 'American Express';
    ctDiscover:   Result:= 'Discover';
    ctMasterCard: Result:= 'MasterCard';
    ctVisa:       Result:= 'Visa';
    ctDebit:      Result:= 'Debit';
    ctEbt:        Result:= 'Ebt';
    ctEgc:        Result:= 'Egc';
    ctWex:        Result:= 'Wex';
    ctVoyager:    Result:= 'Voyager';
    ctJcb:        Result:= 'Jcb';
    ctCup:        Result:= 'Cup';
    ctLvlUp:      Result:= 'LevelUp';
  end;
end;

function MWStrToPosEntryType(const Value: String): TMWPosEntryType;
begin
  Result:= TMWPosEntryType(StrToIntDef(Value, 0));
end;

function MWPosEntryTypeToStr(const Value: TMWPosEntryType): String;
begin
  Result:= IntToStr(Integer(Value));
end;

function MWPosEntryTypeCaption(const Value: TMWPosEntryType): String;
begin
  case Value of
    peUnknown:        Result:= 'Unknown';
    peKeyed:          Result:= 'Keyed';
    peSwipe:          Result:= 'Swiped';
    peAuthorization:  Result:= 'Authorization';
    peProximity:      Result:= 'Proximity';
    peLvlUp:          Result:= 'LevelUp';
  end;
end;

function MWStrToTransactionType(const Value: String): TMWTransactionType;
begin
  Result:= TMWTransactionType(StrToIntDef(Value, 0));
end;

function MWTransactionTypeToStr(const Value: TMWTransactionType): String;
begin
  Result:= IntToStr(Integer(Value));
end;

function MWTransactionTypeCaption(const Value: TMWTransactionType): String;
begin
  case Value of
    ttUnknown:        Result:= 'Unknown';
    ttSale:           Result:= 'Sale';
    ttRefund:         Result:= 'Refund';
    ttVoid:           Result:= 'Void';
    ttForce:          Result:= 'Force';
    ttAuth:           Result:= 'Pre-Auth';
    ttCapture:        Result:= 'Capture';
    ttAdjust:         Result:= 'Adjust';
    ttRepeatSale:     Result:= 'Repeat Sale';
    ttPostAuth:       Result:= 'Post-Auth';
    ttLevelUpSale:    Result:= 'LevelUp Sale';
    ttLevelUpCredit:  Result:= 'LevelUp Credit';
    else              Result:= 'Unknown';
  end;
end;

function MWStrToEmvApprovalStatusSet(const Value: String): TMWEmvApprovalStatusSet;
  procedure A(N: String; V: TMWEmvApprovalStatus);
  begin
    if Pos(N, Value) > 0 then
      Result:= Result + [V];
  end;
begin
  A('Unknown', easUnknown);
  A('Approved', easApproved);
  A('Failed', easFailed);
  A('Declined', easDeclined);
  A('DeclinedDuplicate', easDuplicate);
  A('Referral', easReferral);
end;

function MWEmvApprovalStatusSetToStr(const Value: TMWEmvApprovalStatusSet): String;
  procedure A(const S: String);
  begin
    if Length(Result) > 0 then Result:= Result + ',';
    Result:= Result + S;
  end;
begin
  Result:= '';
  if easUnknown in Value then
    A('Unknown');
  if easApproved in Value then
    A('Approved');
  if easFailed in Value then
    A('Failed');
  if easDeclined in Value then
    A('Declined');
  if easDuplicate in Value then
    A('DeclinedDuplicate');
  if easReferral in Value then
    A('Referral');
end;

function MWStrToSignatureType(const Value: String): TMWSignatureType;
begin
  Result:= TMWSignatureType(StrToIntDef(Value, 0));
end;

function MWSignatureTypeToStr(const Value: TMWSignatureType): String;
begin
  Result:= IntToStr(Integer(Value));
end;

function MWSignatureTypeCaption(const Value: TMWSignatureType): String;
begin
  case Value of
    stUnknown:    Result:= 'Unknown';
    stTiffZip:    Result:= 'Tiff Image';
    stVectorText: Result:= 'Vector Text';
  end;
end;

function MWStrToSignatureStatus(const Value: String): TMWSignatureStatus;
begin
  if SameText(Value, 'ACCEPTED') then
    Result:= ssAccepted
  else if SameText(Value, 'FAILED') then
    Result:= ssFailed
  else
    Result:= ssFailed;
end;

function MWSignatureStatusToStr(const Value: TMWSignatureStatus): String;
begin
  case Value of
    ssAccepted:   Result:= 'ACCEPTED';
    ssFailed:     Result:= 'FAILED';
    else          Result:= 'FAILED';
  end;
end;

function MWCvResponseCaption(const Value: String): String;
begin
  if Value = '' then
    Result:= '(No Cv Code Returned)'
  else if SameText(Value, 'D') then
    Result:= 'Transaction determined suspicious by issuing bank.'
  else if SameText(Value, 'I') then
    Result:= 'Card verification number failed processor''s data validation check.'
  else if SameText(Value, 'M') then
    Result:= 'Card verification number matched.'
  else if SameText(Value, 'N') then
    Result:= 'Card verification number not matched.'
  else if SameText(Value, 'P') then
    Result:= 'Card verification number not processed by processor for unspecified reason.'
  else if SameText(Value, 'S') then
    Result:= 'Card verification number is on the card but was not included in the request.'
  else if SameText(Value, 'U') then
    Result:= 'Card verification is not supported by the issuing bank.'
  else if SameText(Value, 'X') then
    Result:= 'Card verification is not supported by the card association.'
  else if SameText(Value, '1') then
    Result:= 'Card verification is not supported for this processor or card type.'
  else if SameText(Value, '2') then
    Result:= 'Unrecognized result code returned by processor for card verification response.'
  else if SameText(Value, '3') then
    Result:= 'No result code returned by processor.'
  else
    Result:= 'Unknown "'+Value+'"';
end;

function MWAvsResponseCaption(const Value: String): String;
begin
  if Value = '' then
    Result:= '(No Avs Code Returned)'
  else if SameText(Value, 'X') then
    Result:= 'Street address and 9-digit ZIP code both match'
  else if SameText(Value, 'Y') then
    Result:= 'Street address and 5-digit ZIP code both match'
  else if SameText(Value, 'A') then
    Result:= 'Street address matches, but both 5-digit and 9-digit ZIP Code do not match'
  else if SameText(Value, 'W') then
    Result:= 'Street address does not match, but 9-digit ZIP code matches'
  else if SameText(Value, 'Z') then
    Result:= 'Street address does not match, but 5-digit ZIP code matches'
  else if SameText(Value, 'N') then
    Result:= 'Street address, 5-digit ZIP code, and 9-digit ZIP code all do not match'
  else if SameText(Value, 'U') then
    Result:= 'Address information unavailable. Returned if non-US. AVS is not available or if the AVS in a U.S. bank is not functioning properly.'
  else if SameText(Value, 'R') then
    Result:= 'Retry - Issuer''s System Unavailable or Timed Out.'
  else if SameText(Value, 'E') then
    Result:= 'AVS data is invalid'
  else if SameText(Value, 'S') then
    Result:= 'U.S. issuing bank does not support AVS'
  else
    Result:= 'Unknown "'+Value+'"';
end;

end.
