unit Cayan.Genius.Emulator;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  IdContext, IdCustomHTTPServer, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdHTTPServer, IdTCPConnection, IdYarn,
  Cayan.Common,
  Cayan.Genius.Intf,
  Cayan.Genius.Impl,
  Cayan.MWv4.Intf,
  Cayan.MWv4.Impl,
  Xml.XmlDoc, Xml.XmlDom, Xml.XmlIntf;

type
  TCayanGeniusLineItem = class;
  TCayanGeniusEmulator = class;
  TCayanGeniusEmulatorThread = class;

  TEmulatorAction = (eaUnknown, eaStatus, eaCancel, eaStartOrder, eaStartTran,
    eaAddItem, eaDiscountItem, eaDeleteItem, eaDeleteAllItems, eaUpdateItem,
    eaUpdateTotal, eaOrderSummary, eaEndOrder, eaGetAgreement, eaGetSignature,
    eaGetCustomerInput);

  TCayanGeniusScreenEvent = procedure(Sender: TObject; Screen: TGeniusCedScreen) of object;

  TCayanGeniusLineItemEvent = procedure(Sender: TObject; Item: TCayanGeniusLineItem) of object;

  TCayanGeniusLineItem = class(TObject)
  private
    FQty: Integer;
    FItemType: TGeniusLineItemType;
    FTaxAmount: Currency;
    FAmount: Currency;
    FDescription: String;
    FCategory: TGeniusLineItemCategory;
    FTargetItemID: String;
    FDisplayOverride: String;
    FUPC: String;
    FItemTypeValue: String;
    procedure SetAmount(const Value: Currency);
    procedure SetCategory(const Value: TGeniusLineItemCategory);
    procedure SetDescription(const Value: String);
    procedure SetDisplayOverride(const Value: String);
    procedure SetItemType(const Value: TGeniusLineItemType);
    procedure SetItemTypeValue(const Value: String);
    procedure SetQty(const Value: Integer);
    procedure SetTargetItemID(const Value: String);
    procedure SetTaxAmount(const Value: Currency);
    procedure SetUPC(const Value: String);
  public
    procedure UpdateItem;
    property TargetItemID: String read FTargetItemID write SetTargetItemID;
    property ItemType: TGeniusLineItemType read FItemType write SetItemType;
    property ItemTypeValue: String read FItemTypeValue write SetItemTypeValue;
    property UPC: String read FUPC write SetUPC;
    property Qty: Integer read FQty write SetQty;
    property Description: String read FDescription write SetDescription;
    property Amount: Currency read FAmount write SetAmount;
    property TaxAmount: Currency read FTaxAmount write SetTaxAmount;
    property Category: TGeniusLineItemCategory read FCategory write SetCategory;
    property DisplayOverride: String read FDisplayOverride write SetDisplayOverride;
  end;

  TCayanGeniusPaymentSelected = (psNone, psSwipe, psChip, psContact, psScan,
    psCredit, psDebit, psGiftCard, psAndroid, psLevelUp);

  TCayanGeniusEmulatorAgreementEvent = procedure(Sender: TObject;
    const RequestID: String; const Title: String; const AgreementText: String;
    const AcceptLabel: String; const DeclineLabel: String;
    var Accepted: Boolean) of object;

  TCayanGeniusEmulatorSignatureEvent = procedure(Sender: TObject;
    const RequestID: String; const Title: String;
    const Signature: IGeniusSignature) of object;

  TCayanGeniusEmulator = class(TComponent)
  private
    FThread: TCayanGeniusEmulatorThread;
    FOnScreen: TCayanGeniusScreenEvent;
    FPort: Integer;
    FActive: Boolean;
    FOnItemDelete: TCayanGeniusLineItemEvent;
    FOnItemEdit: TCayanGeniusLineItemEvent;
    FOnItemAdd: TCayanGeniusLineItemEvent;

    FSelected: TCayanGeniusPaymentSelected;

    FOrderNum: String;
    FOrderTotal: Currency;
    FOrderTax: Currency;
    FOrderSubtotal: String;
    FMerchantSiteId: String;
    FMerchantKey: String;
    FMerchantName: String;
    FTimeout: Integer;
    FSerialNum: String;
    FOsVersion: String;
    FAppVersion: String;
    FOnAgreement: TCayanGeniusEmulatorAgreementEvent;
    FOnSignature: TCayanGeniusEmulatorSignatureEvent;

    procedure DestroyThread;
    procedure SetPort(const Value: Integer);
    procedure SetActive(const Value: Boolean);
    procedure ThreadScreen(Sender: TObject; Screen: TGeniusCedScreen);
    procedure ThreadItemAdd(Sender: TObject; Item: TCayanGeniusLineItem);
    procedure ThreadItemEdit(Sender: TObject; Item: TCayanGeniusLineItem);
    procedure ThreadItemDelete(Sender: TObject; Item: TCayanGeniusLineItem);
    procedure ThreadAgreement(Sender: TObject;
      const RequestID: String; const Title: String; const AgreementText: String;
      const AcceptLabel: String; const DeclineLabel: String;
      var Accepted: Boolean);
    procedure ThreadSignature(Sender: TObject;
      const RequestID: String; const Title: String;
      const Signature: IGeniusSignature);
    procedure ProcessSwiped;
    procedure ProcessInserted;
    procedure ProcessContacted;
    procedure ProcessScanned;
    procedure SetMerchantKey(const Value: String);
    procedure SetMerchantName(const Value: String);
    procedure SetMerchantSiteID(const Value: String);
    procedure SetTimeout(const Value: Integer);
    procedure SetAppVersion(const Value: String);
    procedure SetOsVersion(const Value: String);
    procedure SetSerialNum(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Genius: IGenius;
    function MerchantWare: IMerchantWare;

    function CanSwipeInScreen: Boolean;

    function OrderNum: String;
    function OrderTotal: Currency;
    function OrderTax: Currency;
    function OrderSubtotal: String;

    procedure CardSwiped();
    procedure CardInserted();
    procedure PhoneContacted();
    procedure PhoneScanned();

    property Selected: TCayanGeniusPaymentSelected read FSelected;

  published
    property Active: Boolean read FActive write SetActive;
    property Port: Integer read FPort write SetPort;
    property Timeout: Integer read FTimeout write SetTimeout;
    property AppVersion: String read FAppVersion write SetAppVersion;
    property OsVersion: String read FOsVersion write SetOsVersion;
    property SerialNum: String read FSerialNum write SetSerialNum;

    property MerchantName: String read FMerchantName write SetMerchantName;
    property MerchantSiteId: String read FMerchantSiteId write SetMerchantSiteID;
    property MerchantKey: String read FMerchantKey write SetMerchantKey;

    property OnScreen: TCayanGeniusScreenEvent read FOnScreen write FOnScreen;

    property OnItemAdd: TCayanGeniusLineItemEvent read FOnItemAdd write FOnItemAdd;
    property OnItemEdit: TCayanGeniusLineItemEvent read FOnItemEdit write FOnItemEdit;
    property OnItemDelete: TCayanGeniusLineItemEvent read FOnItemDelete write FOnItemDelete;

    /// <summary>
    ///  Triggered when the CED needs to prompt the user to accept an agreement.
    /// </summary>
    property OnAgreement: TCayanGeniusEmulatorAgreementEvent read FOnAgreement write FOnAgreement;

    /// <summary
    ///  Triggered when the CED needs to prompt the user to sign.
    /// </summary>
    property OnSignature: TCayanGeniusEmulatorSignatureEvent read FOnSignature write FOnSignature;
  end;

  TCayanGeniusEmulatorThread = class(TThread)
  private
    FOwner: TCayanGeniusEmulator;
    FSvr: TIdHTTPServer;
    FScreen: TGeniusCedScreen;
    FItem: TCayanGeniusLineItem;
    FInTransaction: Boolean;
    FInLineItems: Boolean;
    FInSignature: Boolean;
    FInAgreement: Boolean;
    FInCustomerInput: Boolean;
    FFinished: Boolean;

    FLineItems: TObjectList<TCayanGeniusLineItem>;

    FOnScreen: TCayanGeniusScreenEvent;
    FOnItemDelete: TCayanGeniusLineItemEvent;
    FOnItemEdit: TCayanGeniusLineItemEvent;
    FOnItemAdd: TCayanGeniusLineItemEvent;
    FOnAgreement: TCayanGeniusEmulatorAgreementEvent;
    FOnSignature: TCayanGeniusEmulatorSignatureEvent;
    procedure Init;
    procedure Uninit;
    procedure Process;
    procedure SvrCommand(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure SvrConnect(AContext: TIdContext);
    procedure ClearOrder;
  protected
    procedure Execute; override;
    procedure SYNC_OnScreen;
    procedure SYNC_OnItemAdd;
    procedure SYNC_OnItemEdit;
    procedure SYNC_OnItemDelete;
  public
    constructor Create(AOwner: TCayanGeniusEmulator); reintroduce;
    destructor Destroy; override;
    procedure ChangeScreen(const AScreen: TGeniusCedScreen);
    procedure UpdateLID;
    procedure ClearLineItems;
    function AddLineItem: TCayanGeniusLineItem;
    procedure DeleteLineItem(const Index: Integer);

    property OnScreen: TCayanGeniusScreenEvent read FOnScreen write FOnScreen;

    property OnItemAdd: TCayanGeniusLineItemEvent read FOnItemAdd write FOnItemAdd;
    property OnItemEdit: TCayanGeniusLineItemEvent read FOnItemEdit write FOnItemEdit;
    property OnItemDelete: TCayanGeniusLineItemEvent read FOnItemDelete write FOnItemDelete;

    /// <summary>
    ///  Triggered when the CED needs to prompt the user to accept an agreement.
    /// </summary>
    property OnAgreement: TCayanGeniusEmulatorAgreementEvent read FOnAgreement write FOnAgreement;

    /// <summary
    ///  Triggered when the CED needs to prompt the user to sign.
    /// </summary>
    property OnSignature: TCayanGeniusEmulatorSignatureEvent read FOnSignature write FOnSignature;

  end;

  TCayanGeniusEmulatorContext = class(TIdServerContext)
  private
    FDoc: TStringList;
    FVer: TGeniusDeviceVersion;
    FAction: TEmulatorAction;
    FOwner: TCayanGeniusEmulatorThread;
    FFormat: TDataFormat;
    FTransportKey: String;
    FDetails: IGeniusPaymentDetails;
    FMerchantWare: TMerchantWare;
    FGenius: TGenius;
    function GetDoc: TStrings;
    procedure SetDoc(const Value: TStrings);
    procedure SetAction(const Value: TEmulatorAction);
    procedure SetOwner(const Value: TCayanGeniusEmulatorThread);
    procedure SetFormat(const Value: TDataFormat);
    procedure HandleCommand(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure DoPosTransaction(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleStartOrder(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleStartTran(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleStatus(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleCancel(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleAddItem(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleDiscountItem(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleDeleteItem(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleDeleteAllItems(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleUpdateItem(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleUpdateTotal(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleOrderSummary(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleEndOrder(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleGetAgreement(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleGetSignature(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleGetCustomerInput(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    function TranStr: String;
  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn;
      AList: TIdContextThreadList = nil); override;
    destructor Destroy; override;
    property Doc: TStrings read GetDoc write SetDoc;
    property Action: TEmulatorAction read FAction write SetAction;
    property Format: TDataFormat read FFormat write SetFormat;
    property Owner: TCayanGeniusEmulatorThread read FOwner write SetOwner;
  end;

implementation

uses
  StrUtils;

function GeniusEmulatorActionToStr(const Value: TEmulatorAction): String;
begin
  case Value of
    eaUnknown: Result:= '';
    eaStatus: Result:= 'Status';
    eaCancel: Result:= 'Cancel';
    eaStartOrder: Result:= 'StartOrder';
    eaStartTran: Result:= '';
    eaAddItem: Result:= 'AddItem';
    eaDiscountItem: Result:= 'DiscountItem';
    eaDeleteItem: Result:= 'DeleteItem';
    eaDeleteAllItems: Result:= 'DeleteAllItems';
    eaUpdateItem: Result:= 'UpdateItem';
    eaUpdateTotal: Result:= 'UpdateTotal';
    eaOrderSummary: Result:= 'OrderSummary';
    eaEndOrder: Result:= 'EndOrder';
    eaGetAgreement: Result:= 'GetAgreement';
    eaGetSignature: Result:= 'GetSignature';
    eaGetCustomerInput: Result:= 'GetCustomerInput';
  end;
end;

function GeniusStrToEmulatorAction(const Value: String): TEmulatorAction;
begin
  if SameText(Value, '') then
    Result:= eaStartTran
  else if SameText(Value, 'Status') then
    Result:= eaStatus
  else if SameText(Value, 'Cancel') then
    Result:= eaCancel
  else if SameText(Value, 'StartOrder') then
    Result:= eaStartOrder
  else if SameText(Value, 'AddItem') then
    Result:= eaAddItem
  else if SameText(Value, 'DiscountItem') then
    Result:= eaDiscountItem
  else if SameText(Value, 'DeleteItem') then
    Result:= eaDeleteItem
  else if SameText(Value, 'DeleteAllItems') then
    Result:= eaDeleteAllItems
  else if SameText(Value, 'UpdateItem') then
    Result:= eaUpdateItem
  else if SameText(Value, 'UpdateTotal') then
    Result:= eaUpdateTotal
  else if SameText(Value, 'OrderSummary') then
    Result:= eaOrderSummary
  else if SameText(Value, 'EndOrder') then
    Result:= eaEndOrder
  else if SameText(Value, 'GetAgreement') then
    Result:= eaGetAgreement
  else if SameText(Value, 'GetSignature') then
    Result:= eaGetSignature
  else if SameText(Value, 'GetCustomerInput') then
    Result:= eaGetCustomerInput
  else
    Result:= eaUnknown;
end;

{ TCayanGeniusLineItem }

procedure TCayanGeniusLineItem.SetAmount(const Value: Currency);
begin
  FAmount := Value;
end;

procedure TCayanGeniusLineItem.SetCategory(
  const Value: TGeniusLineItemCategory);
begin
  FCategory := Value;
end;

procedure TCayanGeniusLineItem.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TCayanGeniusLineItem.SetDisplayOverride(const Value: String);
begin
  FDisplayOverride := Value;
end;

procedure TCayanGeniusLineItem.SetItemType(const Value: TGeniusLineItemType);
begin
  FItemType := Value;
end;

procedure TCayanGeniusLineItem.SetItemTypeValue(const Value: String);
begin
  FItemTypeValue := Value;
end;

procedure TCayanGeniusLineItem.SetQty(const Value: Integer);
begin
  FQty := Value;
end;

procedure TCayanGeniusLineItem.SetTargetItemID(const Value: String);
begin
  FTargetItemID := Value;
end;

procedure TCayanGeniusLineItem.SetTaxAmount(const Value: Currency);
begin
  FTaxAmount := Value;
end;

procedure TCayanGeniusLineItem.SetUPC(const Value: String);
begin
  FUPC := Value;
end;

procedure TCayanGeniusLineItem.UpdateItem;
begin

end;

{ TCayanGeniusEmulator }

constructor TCayanGeniusEmulator.Create(AOwner: TComponent);
begin
  inherited;
  FThread:= nil;

end;

destructor TCayanGeniusEmulator.Destroy;
begin
  DestroyThread;
  inherited;
end;

procedure TCayanGeniusEmulator.DestroyThread;
var
  T: TCayanGeniusEmulatorThread;
begin
  if Assigned(FThread) then begin
    T:= FThread;
    FThread:= nil;
    T.OnTerminate:= nil; //TODO
    T.FreeOnTerminate:= True;
    T.Terminate;
  end;
end;

function TCayanGeniusEmulator.Genius: IGenius;
begin

end;

function TCayanGeniusEmulator.MerchantWare: IMerchantWare;
begin

end;

function TCayanGeniusEmulator.OrderNum: String;
begin
  Result:= FOrderNum;
end;

function TCayanGeniusEmulator.OrderSubtotal: String;
begin
  Result:= FOrderSubtotal;
end;

function TCayanGeniusEmulator.OrderTax: Currency;
begin
  Result:= FOrderTax;
end;

function TCayanGeniusEmulator.OrderTotal: Currency;
begin
  Result:= FOrderTotal;
end;

procedure TCayanGeniusEmulator.SetActive(const Value: Boolean);
begin
  if Value then begin
    if not FActive then begin
      FActive:= True;
      Self.FThread:= TCayanGeniusEmulatorThread.Create(Self);
      Self.FThread.OnScreen:= ThreadScreen;
      Self.FThread.OnItemAdd:= ThreadItemAdd;
      Self.FThread.OnItemEdit:= ThreadItemEdit;
      Self.FThread.OnItemDelete:= ThreadItemDelete;
      Self.FThread.OnAgreement:= ThreadAgreement;
      Self.FThread.OnSignature:= ThreadSignature;

      Self.FThread.Start;
    end;
  end else begin
    if FActive then begin
      FActive:= False;
      Self.DestroyThread;
    end;
  end;
end;

procedure TCayanGeniusEmulator.SetAppVersion(const Value: String);
begin
  FAppVersion := Value;
end;

procedure TCayanGeniusEmulator.SetMerchantKey(const Value: String);
begin
  FMerchantKey := Value;
end;

procedure TCayanGeniusEmulator.SetMerchantName(const Value: String);
begin
  FMerchantName := Value;
end;

procedure TCayanGeniusEmulator.SetMerchantSiteID(const Value: String);
begin
  FMerchantSiteId := Value;
end;

procedure TCayanGeniusEmulator.SetOsVersion(const Value: String);
begin
  FOsVersion := Value;
end;

procedure TCayanGeniusEmulator.ThreadAgreement(Sender: TObject; const RequestID,
  Title, AgreementText, AcceptLabel, DeclineLabel: String;
  var Accepted: Boolean);
begin
  if Assigned(FOnAgreement) then
    FOnAgreement(Self, RequestID, Title, AgreementText, AcceptLabel, DeclineLabel, Accepted);
end;

procedure TCayanGeniusEmulator.ThreadItemAdd(Sender: TObject;
  Item: TCayanGeniusLineItem);
begin
  if Assigned(FOnItemAdd) then
    FOnItemAdd(Self, Item);
end;

procedure TCayanGeniusEmulator.ThreadItemDelete(Sender: TObject;
  Item: TCayanGeniusLineItem);
begin
  if Assigned(FOnItemDelete) then
    FOnItemDelete(Self, Item);
end;

procedure TCayanGeniusEmulator.ThreadItemEdit(Sender: TObject;
  Item: TCayanGeniusLineItem);
begin
  if Assigned(FOnItemEdit) then
    FOnItemEdit(Self, Item);
end;

procedure TCayanGeniusEmulator.ThreadScreen(Sender: TObject; Screen: TGeniusCedScreen);
begin
  if Assigned(Self.FOnScreen) then
    Self.FOnScreen(Self, Screen);
end;

procedure TCayanGeniusEmulator.ThreadSignature(Sender: TObject; const RequestID,
  Title: String; const Signature: IGeniusSignature);
begin
  if Assigned(FOnSignature) then
    FOnSignature(Self, RequestID, Title, Signature);
end;

procedure TCayanGeniusEmulator.SetPort(const Value: Integer);
begin
  if FActive then
    raise Exception.Create('Cannot change server port while active.');

  FPort := Value;
end;

procedure TCayanGeniusEmulator.SetSerialNum(const Value: String);
begin
  FSerialNum := Value;
end;

procedure TCayanGeniusEmulator.SetTimeout(const Value: Integer);
begin
  if FActive then
    raise Exception.Create('Cannot change timeout while active.');

  FTimeout := Value;
end;

function TCayanGeniusEmulator.CanSwipeInScreen: Boolean;
begin
  Result:= FActive;
  if Result then
    Result:= FThread.FScreen in [csMainPayment, csCustomPayment, csSkuDisplay];

{
csNoScreen = -1, csIdle = 0, csValidatingTransportKey = 1,
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
    csNetworkConfig = 39
}

end;

procedure TCayanGeniusEmulator.CardInserted;
begin
  if CanSwipeInScreen then begin
    if FSelected in [psNone, psChip] then begin
      FSelected:= psChip;
      if FThread.FInTransaction then begin
        ProcessInserted;
      end;
    end;
  end;
end;

procedure TCayanGeniusEmulator.ProcessInserted;
begin
  FSelected:= psChip;
  //TODO: Change to "Do Not Remove Card" screen...
  FThread.ChangeScreen(csProcessing);
  Sleep(3000);
  FThread.FFinished:= True;
  //TODO: Assign result accordingly...
  FThread.ChangeScreen(csTransactionResult);
  Sleep(5000);
  FThread.ChangeScreen(csIdle);
end;

procedure TCayanGeniusEmulator.CardSwiped;
begin
  if CanSwipeInScreen then begin
    if FSelected in [psNone, psSwipe] then begin
      FSelected:= psSwipe;
      if FThread.FInTransaction then begin
        ProcessSwiped;
      end;
    end;
  end;
end;

procedure TCayanGeniusEmulator.ProcessSwiped;
begin
  FSelected:= psSwipe;
  FThread.ChangeScreen(csProcessing);
  Sleep(3000);
  FThread.FFinished:= True;
  //TODO: Assign result accordingly...
  FThread.ChangeScreen(csTransactionResult);
  Sleep(5000);
  FThread.ChangeScreen(csIdle);
end;

procedure TCayanGeniusEmulator.PhoneContacted;
begin
  if CanSwipeInScreen then begin
    if FSelected in [psNone, psContact] then begin
      FSelected:= psContact;
      if FThread.FInTransaction then begin
        ProcessContacted;
      end;
    end;
  end;
end;

procedure TCayanGeniusEmulator.ProcessContacted;
begin
  FSelected:= psContact;
  FThread.ChangeScreen(csProcessing);
  Sleep(3000);
  FThread.FFinished:= True;
  //TODO: Assign result accordingly...
  FThread.ChangeScreen(csTransactionResult);
  Sleep(5000);
  FThread.ChangeScreen(csIdle);
end;

procedure TCayanGeniusEmulator.PhoneScanned;
begin
  if CanSwipeInScreen then begin
    if FSelected in [psNone, psScan] then begin
      FSelected:= psScan;
      if FThread.FInTransaction then begin
        ProcessScanned;
      end;
    end;
  end;
end;

procedure TCayanGeniusEmulator.ProcessScanned;
begin
  FSelected:= psScan;
  FThread.ChangeScreen(csProcessing);
  Sleep(3000);
  FThread.FFinished:= True;
  //TODO: Assign result accordingly...
  FThread.ChangeScreen(csTransactionResult);
  Sleep(5000);
  FThread.ChangeScreen(csIdle);
end;

{ TCayanGeniusEmulatorThread }

constructor TCayanGeniusEmulatorThread.Create(AOwner: TCayanGeniusEmulator);
begin
  inherited Create(True);
  FOwner:= AOwner;
  FLineItems:= TObjectList<TCayanGeniusLineItem>.Create(True);
end;

destructor TCayanGeniusEmulatorThread.Destroy;
begin
  Self.FSvr.Active:= False;
  FLineItems.Clear;
  FreeAndNil(FLineItems);
  inherited;
end;

procedure TCayanGeniusEmulatorThread.Init;
begin
  FSvr:= TIdHTTPServer.Create(nil);
  FSvr.DefaultPort:= FOwner.FPort;
  FSvr.ContextClass:= TCayanGeniusEmulatorContext;
  FSvr.KeepAlive:= True;
  FSvr.OnCommandGet:= SvrCommand;
  FSvr.OnCommandOther:= SvrCommand;
  FSvr.OnConnect:= SvrConnect;
  FSvr.Active:= True;

end;

procedure TCayanGeniusEmulatorThread.Uninit;
begin
  ClearLineItems;
  FSvr.Active:= False;
  FreeAndNil(FSvr);
end;

procedure TCayanGeniusEmulatorThread.ClearLineItems;
var
  X: Integer;
begin
  for X := FLineItems.Count-1 downto 0 do begin
    DeleteLineItem(X);
  end;
end;

procedure TCayanGeniusEmulatorThread.UpdateLID;
begin
  //TODO: Update Line Item Display...

end;

function TCayanGeniusEmulatorThread.AddLineItem: TCayanGeniusLineItem;
begin
  Result:= TCayanGeniusLineItem.Create;
  FLineItems.Add(Result);
  FItem:= Result;
  Synchronize(SYNC_OnItemAdd);
end;

procedure TCayanGeniusEmulatorThread.DeleteLineItem(const Index: Integer);
begin
  FItem:= FLineItems[Index];
  Synchronize(SYNC_OnItemDelete);
  FLineItems.Delete(Index);
end;

procedure TCayanGeniusEmulatorThread.ClearOrder;
begin
  FInTransaction:= False;
  FInLineItems:= False;
  FInSignature:= False;
  ClearLineItems;
  FOwner.FOrderNum:= '';
  FOwner.FOrderTotal:= 0;
  FOwner.FOrderTax:= 0;
  FOwner.FOrderSubtotal:= '';
  FOwner.FSelected:= psNone;
end;

procedure TCayanGeniusEmulatorThread.ChangeScreen(
  const AScreen: TGeniusCedScreen);
begin
  FScreen:= AScreen;
  case FScreen of
    csIdle: begin
      ClearOrder;
    end;
    csValidatingTransportKey: begin
      //Nothing to do...
    end;
    csMainPayment: begin
      FInTransaction:= True;
      case FOwner.FSelected of
        psNone: begin
          //Proceed with user selection of payment...

        end;
        psSwipe: begin
          FOwner.ProcessSwiped;
        end;
        psChip: begin
          FOwner.ProcessInserted;
        end;
        psContact: begin
          FOwner.ProcessContacted;
        end;
        psScan: begin
          FOwner.ProcessScanned;
        end;
        psCredit: begin
          FOwner.ProcessSwiped;
        end;
        psDebit: begin
          FOwner.ProcessSwiped;
        end;
        psGiftCard: begin
          FOwner.ProcessSwiped;
        end;
        psAndroid: begin
          FOwner.ProcessContacted;
        end;
        psLevelUp: begin
          FOwner.ProcessScanned;
        end;
      end;
    end;
    csCustomPayment: begin
      case FOwner.FSelected of
        psNone: begin
          Self.ChangeScreen(csMainPayment);
        end;
        else begin

        end;
      end;
    end;
    csLookupCardBin: begin

    end;
    csWaitingForAmount: begin
      //TODO: Prompt user for amount...

    end;
    csWaitingForPin: begin
      //TODO: Prompt user for PIN...

    end;
    csProcessing: begin

    end;
    csWaitingForSignature: begin
      //TODO: Prompt for signature...

    end;
    csProcessingSignature: begin

    end;
    csTransactionResult: begin

    end;
    csCancelConfirm: begin
      //TODO: Confirm cancellation...


    end;
    csRunAsCreditConfirm: begin

    end;
    csSkuDisplay: begin
      FInLineItems:= True;
      //TODO:

    end;
    csCashbackSelection: begin

    end;
    csCashbackCustom: begin

    end;
    csTipSelection: begin

    end;
    csTipCustom: begin

    end;
    csDonationSelection: begin

    end;
    csDonationCustom: begin

    end;
    csConfirmation: begin

    end;
    csErrorScreen: begin

    end;
    csSkuAmountConfirm: begin

    end;
    csPanEntry: begin

    end;
    csExpirationEntry: begin

    end;
    csCvvEntry: begin

    end;
    csZipEntry: begin

    end;
    csAgreement: begin

    end;
    csAgreementSign: begin

    end;
    csEMVAppSelect: begin

    end;
    csCustomerInput: begin

    end;
    csGiftCard: begin

    end;
    csNetworkDetail: begin

    end;
    csNetworkConfig: begin

    end;
  end;
  Synchronize(SYNC_OnScreen);
end;

procedure TCayanGeniusEmulatorThread.Process;
begin

end;

procedure TCayanGeniusEmulatorThread.SvrCommand(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  C: TCayanGeniusEmulatorContext;
begin
  C:= TCayanGeniusEmulatorContext(AContext);
  C.HandleCommand(ARequestInfo, AResponseInfo);
end;

procedure TCayanGeniusEmulatorThread.SvrConnect(AContext: TIdContext);
var
  C: TCayanGeniusEmulatorContext;
begin
  C:= TCayanGeniusEmulatorContext(AContext);
  C.Owner:= Self;
end;

procedure TCayanGeniusEmulatorThread.SYNC_OnItemAdd;
begin
  if Assigned(FOnItemAdd) then
    FOnItemAdd(Self, FItem);
end;

procedure TCayanGeniusEmulatorThread.SYNC_OnItemDelete;
begin
  if Assigned(FOnItemDelete) then
    FOnItemDelete(Self, FItem);
end;

procedure TCayanGeniusEmulatorThread.SYNC_OnItemEdit;
begin
  if Assigned(FOnItemEdit) then
    FOnItemEdit(Self, FItem);
end;

procedure TCayanGeniusEmulatorThread.SYNC_OnScreen;
begin
  if Assigned(FOnScreen) then
    FOnScreen(Self, FScreen);
end;

procedure TCayanGeniusEmulatorThread.Execute;
begin
  Init;
  try
    ChangeScreen(TGeniusCedScreen.csIdle);
    while not Terminated do begin
      try
        try
          Process;
        except
          on E: Exception do begin
            //TODO
          end;
        end;
      finally
        Sleep(1);
      end;
    end;
  finally
    Uninit;
  end;
end;

{ TCayanGeniusEmulatorContext }

constructor TCayanGeniusEmulatorContext.Create(AConnection: TIdTCPConnection;
  AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited;
  FDoc:= TStringList.Create;
  FMerchantWare:= TMerchantWare.Create;
  IMerchantWare(FMerchantWare)._AddRef;
  FGenius:= TGenius.Create(FMerchantWare);
  IGenius(FGenius)._AddRef;

end;

destructor TCayanGeniusEmulatorContext.Destroy;
begin
  FreeAndNil(FDoc);
  inherited;
end;

function TCayanGeniusEmulatorContext.GetDoc: TStrings;
begin
  Result:= TStrings(FDoc);
end;

procedure TCayanGeniusEmulatorContext.SetAction(const Value: TEmulatorAction);
begin
  FAction := Value;
end;

procedure TCayanGeniusEmulatorContext.SetDoc(const Value: TStrings);
begin
  FDoc.Assign(Value);
end;

procedure TCayanGeniusEmulatorContext.SetFormat(const Value: TDataFormat);
begin
  FFormat := Value;
end;

procedure TCayanGeniusEmulatorContext.SetOwner(const Value: TCayanGeniusEmulatorThread);
begin
  FOwner := Value;
  FMerchantWare.Name:= FOwner.FOwner.FMerchantName;
  FMerchantWare.SiteID:= FOwner.FOwner.FMerchantSiteID;
  FMerchantWare.Key:= FOwner.FOwner.FMerchantKey;
end;

procedure TCayanGeniusEmulatorContext.HandleCommand(ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  U: String;
  P: Integer;
begin
  FDoc.Clear;
  U:= ARequestInfo.Document;
  Delete(U, 1, 1);
  U:= U + '/';
  while Length(U) > 0 do begin
    P:= Pos('/', U);
    FDoc.Append(Copy(U, 1, P-1));
    Delete(U, 1, P);
  end;
  if FDoc[0] = 'v1' then begin
    FVer:= TGeniusDeviceVersion.gdVer1;
  end else
  if FDoc[0] = 'v2' then begin
    FVer:= TGeniusDeviceVersion.gdVer2;
  end else begin
    AResponseInfo.ResponseNo:= 400;
  end;

  case FDoc.Count of
    0: begin
      AResponseInfo.ResponseNo:= 400;
    end;
    1: begin
      AResponseInfo.ResponseNo:= 400;
    end;
    else begin
      case ARequestInfo.CommandType of
        hcGET: begin
          if FDoc[1] = 'pos' then begin
            DoPosTransaction(ARequestInfo, AResponseInfo);
          end else begin
            AResponseInfo.ResponseNo:= 400;
          end;
        end;
        else begin
          AResponseInfo.ResponseNo:= 400;
        end;
      end;
    end;
  end;
end;


procedure TCayanGeniusEmulatorContext.DoPosTransaction(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  F: String;
begin
  F:= ARequestInfo.Params.Values['Format'];
  if F = '' then
    Self.FFormat:= efXML
  else if F = 'XML' then
    Self.FFormat:= efXML
  else if F = 'JSON' then
    Self.FFormat:= efJSON
  else begin
    //Unrecognized format...
    AResponseInfo.ResponseNo:= 400;
  end;

  case FFormat of
    efXML: begin
      AResponseInfo.ContentType:= 'text/xml';
    end;
    efJSON: begin
      AResponseInfo.ContentType:= 'application/json';
    end;
  end;

  FAction:= GeniusStrToEmulatorAction(ARequestInfo.Params.Values['Action']);
  case FAction of
    eaStatus: HandleStatus(ARequestInfo, AResponseInfo);
    eaCancel: HandleCancel(ARequestInfo, AResponseInfo);
    eaStartOrder: HandleStartOrder(ARequestInfo, AResponseInfo);
    eaStartTran: HandleStartTran(ARequestInfo, AResponseInfo);
    eaAddItem: HandleAddItem(ARequestInfo, AResponseInfo);
    eaDiscountItem: HandleDiscountItem(ARequestInfo, AResponseInfo);
    eaDeleteItem: HandleDeleteItem(ARequestInfo, AResponseInfo);
    eaDeleteAllItems: HandleDeleteAllItems(ARequestInfo, AResponseInfo);
    eaUpdateItem: HandleUpdateItem(ARequestInfo, AResponseInfo);
    eaUpdateTotal: HandleUpdateTotal(ARequestInfo, AResponseInfo);
    eaOrderSummary: HandleOrderSummary(ARequestInfo, AResponseInfo);
    eaEndOrder: HandleEndOrder(ARequestInfo, AResponseInfo);
    eaGetAgreement: HandleGetAgreement(ARequestInfo, AResponseInfo);
    eaGetSignature: HandleGetSignature(ARequestInfo, AResponseInfo);
    eaGetCustomerInput: HandleGetCustomerInput(ARequestInfo, AResponseInfo);
    else begin
      AResponseInfo.ResponseNo:= 400;
    end;
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleStartTran(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  FDetails:= nil;

  //Validate transport key...
  FOwner.ChangeScreen(TGeniusCedScreen.csValidatingTransportKey);
  FTransportKey:= ARequestInfo.Params.Values['TransportKey'];
  FDetails:= FGenius.DetailsByTransportKey(FTransportKey);
  if FDetails.ErrorMessage <> '' then begin
    FOwner.ChangeScreen(TGeniusCedScreen.csMainPayment);
    FOwner.FFinished:= False;
    while not FOwner.FFinished do begin
      //TODO: Implement timeout...
      Sleep(50);
    end;

  end else begin

  end;
  AResponseInfo.ContentText:= TranStr;
end;

function TCayanGeniusEmulatorContext.TranStr: String;
var
  D: TCombinedData;
begin
  Result:= '';
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'TransactionResult';
    D.Start(FFormat);
    D.AddStr('Status', 'APPROVED');
    D.AddStr('AuthorizationCode', 'OK1234');
    D.AddStr('AmountApproved', CurrToStr(FOwner.FOwner.FOrderTotal));
    D.AddStr('Cardholder', 'TEST CARD/Genius');
    D.AddStr('AccountNumber', '************4321');
    D.AddStr('PaymentType', 'VISA');
    case FOwner.FOwner.FSelected of
      psSwipe: D.AddStr('EntryMode', 'SWIPE');
      psChip: D.AddStr('EntryMode', 'SWIPE');
      psContact: D.AddStr('EntryMode', 'PROXIMITY');
      psScan: D.AddStr('EntryMode', 'BARCODE');
      psCredit: D.AddStr('EntryMode', 'SWIPE');
      psDebit: D.AddStr('EntryMode', 'SWIPE');
      psGiftCard: D.AddStr('EntryMode', 'SWIPE');
      psAndroid: D.AddStr('EntryMode', 'PROXIMITY');
      psLevelUp: D.AddStr('EntryMode', 'BARCODE');
      else D.AddStr('EntryMode', 'UNKNOWN');
    end;
    D.AddStr('ErrorMessage', '');
    D.AddStr('Token', '601601601');
    D.AddStr('TransactionDate', FormatDateTime('m/d/yyyy h:nn:ss AMPM', Now));
    D.AddStr('TransactionType', 'SALE');
    D.AddStr('ResponseType', 'SINGLE');
    D.AddStr('ValidationKey', '');
    D.AddObject('AdditionalParameters');
      D.AddStr('SignatureData', '');
      D.AddObject('AmountDetails');
        D.AddStr('UserTip', '0.00');
        D.AddStr('Cashback', '0.00');
        D.AddStr('Donation', '0.00');
        D.AddStr('Surcharge', '0.00');
        D.AddObject('Discount');
          D.AddStr('Total', '0.00');
        D.DoneObject;
      D.DoneObject;
      D.AddObject('Loyalty');
        D.AddStr('AccountNumber', '');
        D.AddObject('Balances');
          D.AddStr('PointsBalance', '0');
          D.AddStr('AmountBalance', '0.00');
        D.DoneObject;
      D.DoneObject;
    D.DoneObject;
    Result:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;
                                                 

procedure TCayanGeniusEmulatorContext.HandleStatus(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
  Scr: Integer;
begin
  Scr:= Integer(FOwner.FScreen);
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'StatusResult';
    D.Start(FFormat);
    D.AddStr('Status', 'Online');
    D.AddStr('CurrentScreen', IntToStr(Scr));
    D.AddStr('ResponseMessage', '');
    D.AddStr('SerialNumber', FOwner.FOwner.FSerialNum);
    D.AddStr('ApplicationVersion', FOwner.FOwner.FAppVersion);
    D.AddStr('OSVersion', FOwner.FOwner.FOsVersion);
    D.AddObject('AdditionalParameters');
      D.AddStr('PaymentDataCaptured', IfThen(FOwner.FOwner.FSelected <> psNone, 'true', 'false'));
      D.AddStr('RemoveEMVCard', 'false'); //TODO: Return whether chip card needs to be removed
    D.DoneObject;
    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleCancel(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
begin
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'CancelResult';
    D.Start(FFormat);
    if FOwner.FInLineItems and FOwner.FInTransaction then begin
      //Currently in Payment Collection after Line Item Display...
      FOwner.FInTransaction:= False;
      FOwner.ChangeScreen(TGeniusCedScreen.csSkuDisplay);
    end else
    if FOwner.FInLineItems and FOwner.FInAgreement then begin

      FOwner.FInAgreement:= False;
      FOwner.ChangeScreen(TGeniusCedScreen.csSkuDisplay);

    end else
    if FOwner.FInLineItems and FOwner.FInCustomerInput then begin

      FOwner.FInCustomerInput:= False;
      FOwner.ChangeScreen(TGeniusCedScreen.csSkuDisplay);

    end else
    if FOwner.FInTransaction then begin
      //Currently in Payment Collection without Line Item Display...
      Fowner.FInTransaction:= False;
      FOwner.ChangeScreen(TGeniusCedScreen.csIdle);
    end else
    if FOwner.FInLineItems then begin
      //Currently in Line Item Display...
      FOwner.FInLineItems:= False;
      FOwner.ChangeScreen(TGeniusCedScreen.csIdle);
    end else
    if FOwner.FInSignature then begin
      //Currently waiting for signature input...
      FOwner.FInSignature:= False;
      FOwner.ChangeScreen(TGeniusCedScreen.csIdle);
    end else
    if FOwner.FInAgreement then begin

      FOwner.FInAgreement:= False;
      FOwner.ChangeScreen(TGeniusCedScreen.csIdle);

    end else
    if FOwner.FInCustomerInput then begin

      FOwner.FInCustomerInput:= False;
      FOwner.ChangeScreen(TGeniusCedScreen.csIdle);

    end else begin
      //Cannot cancel, not in transaction...

    end;

    FOwner.FFinished:= True;

    D.AddStr('Status', 'Cancelled');
    D.AddStr('ResponseMessage', '');
    D.AddObject('AdditionalParameters');

    D.DoneObject;

    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleStartOrder(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
begin
  FOwner.FOwner.FOrderNum:= ARequestInfo.Params.Values['Order'];
  FOwner.FOwner.FOrderTotal:= 0;
  FOwner.FOwner.FOrderTax:= 0;
  FOwner.FOwner.FOrderSubtotal:= '';

  FOwner.ChangeScreen(TGeniusCedScreen.csSkuDisplay);
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'OrderResult';
    D.Start(FFormat);

    D.AddStr('Status', 'Success');
    D.AddStr('ResponseMessage', '');
    D.AddObject('AdditionalParameters');

    D.DoneObject;

    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleAddItem(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  I: TCayanGeniusLineItem;
  D: TCombinedData;
  ItemID: String;
  X: Integer;
  Cont: Boolean;
  function ItemExists(const ID: String): Boolean;
  var
    X: Integer;
  begin
    Result:= False;
    for X := 0 to FOwner.FLineItems.Count-1 do begin
      I:= FOwner.FLineItems[X];
      if I.TargetItemID = ID then begin
        Result:= True;
        Break;
      end;
    end;
  end;
begin
  FOwner.ChangeScreen(TGeniusCedScreen.csSkuDisplay);
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'OrderResult';
    D.Start(FFormat);

    if SameText(FOwner.FOwner.FOrderNum, ARequestInfo.Params.Values['Order']) then begin
      Cont:= True;

      ItemID:= Trim(ARequestInfo.Params.Values['ItemID']);
      if ItemID = '' then begin
        X:= 1;
        while ItemExists(IntToStr(X)) do begin
          Inc(X);
        end;
        ItemID:= IntToStr(X);
      end else begin
        if ItemExists(ItemID) then begin
          Cont:= False;
          D.AddStr('Status', 'Denied');
          D.AddStr('ResponseMessage', 'Item ID already exists.');
        end;
      end;

      if Cont then begin

        I:= FOwner.AddLineItem;
        I.FTargetItemID:= ItemID;
        I.FItemType:= GeniusStrToLineItemType(ARequestInfo.Params.Values['Type']);
        I.FItemTypeValue:= ARequestInfo.Params.Values['TypeValue'];
        I.FUPC:= ARequestInfo.Params.Values['UPC'];
        I.FQty:= StrToIntDef(ARequestInfo.Params.Values['Quantity'], 1);
        I.FDescription:= ARequestInfo.Params.Values['Description'];
        I.FAmount:= StrToCurrDef(ARequestInfo.Params.Values['Amount'], 0);
        I.FTaxAmount:= StrToCurrDef(ARequestInfo.Params.Values['TaxAmount'], 0);
        I.FCategory:= GeniusStrToLineItemCategory(ARequestInfo.Params.Values['Category']);
        I.FDisplayOverride:= ARequestInfo.Params.Values['DisplayOverride'];

        FOwner.FOwner.FOrderTotal:= StrToCurrDef(ARequestInfo.Params.Values['OrderTotal'], 0);
        FOwner.FOwner.FOrderTax:= StrToCurrDef(ARequestInfo.Params.Values['OrderTax'], 0);
        FOwner.FOwner.FOrderSubtotal:= ARequestInfo.Params.Values['DisplayCustomSubTotal'];

        FOwner.FItem:= I;
        FOwner.Synchronize(FOwner.SYNC_OnItemEdit);

        D.AddStr('Status', 'Success');
        D.AddStr('ResponseMessage', '');
        D.AddStr('ItemID', ItemID);
      end;
    end else begin
      D.AddStr('Status', 'Denied');
      D.AddStr('ResponseMessage', 'Mis-matching order number');
    end;
    D.AddObject('AdditionalParameters');
    D.DoneObject;

    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleDiscountItem(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
begin
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'OrderResult';
    D.Start(FFormat);



    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleDeleteItem(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
begin
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'OrderResult';
    D.Start(FFormat);



    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleDeleteAllItems(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
begin
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'OrderResult';
    D.Start(FFormat);



    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleUpdateItem(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
begin
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'OrderResult';
    D.Start(FFormat);



    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleUpdateTotal(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
begin
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'OrderResult';
    D.Start(FFormat);



    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleOrderSummary(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
  O: String;
  X: Integer;
  I: IGeniusLineItem;
  TD: Integer;
  TA: Currency;
  TT: Currency;
begin
  O:= ARequestInfo.Params.Values['Order'];

  TD:= 0;
  TA:= 0;
  TT:= 0;

  D:= TCombinedData.Create;
  try
    D.RootElement:= 'OrderResult';
    D.Start(FFormat);
    if FOwner.FInLineItems then begin
      if O = FOwner.FOwner.FOrderNum then begin
        D.AddStr('Status', 'Open');
        D.AddStr('ResponseMessage', '');
        for X := 0 to FGenius.LineItems.Count-1 do begin
          I:= FGenius.LineItems[X];
          TD:= TD + I.DiscountCount;
          TA:= TA + ((I.Amount + I.TaxAmount) * I.Quantity);
          TT:= TT + (I.TaxAmount * I.Quantity);
        end;
      end else begin
        D.AddStr('Status', 'Failed'); //TODO
        D.AddStr('ResponseMessage', 'Mis-matching order number.');
      end;
    end else begin
      D.AddStr('Status', 'Failed'); //TODO
      D.AddStr('ResponseMessage', 'There is no order currently started.');
    end;
    D.AddStr('Items', IntToStr(FGenius.LineItems.Count));
    D.AddStr('Discounts', IntToStr(TD));
    D.AddStr('TotalAmount', FormatFloat('$#,###,##0.00', TA));
    D.AddStr('TaxAmount', FormatFloat('$#,###,##0.00', TT));
    D.AddObject('AdditionalParameters');

    D.DoneObject;

    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleEndOrder(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
  PT: TGeniusExternalPaymentType;
begin
  PT:= GeniusStrToExternalPaymentType(ARequestInfo.Params.Values['ExternalPaymentType']);

  case PT of
    epCash: ;
    epCheck: ;
    epStoreCredit: ;
    epOther: ;
  end;

  FOwner.ChangeScreen(TGeniusCedScreen.csIdle); //TODO: ?

  D:= TCombinedData.Create;
  try
    D.RootElement:= 'OrderResult';
    D.Start(FFormat);
    D.AddStr('Status', 'Success');
    D.AddStr('ResponseMessage', '');
    D.AddObject('AdditionalParameters');

    D.DoneObject;

    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleGetAgreement(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
  A: Boolean;
begin
  D:= TCombinedData.Create;
  Self.FOwner.FInAgreement:= True;
  try
    D.RootElement:= 'AgreementTextResponse';
    D.Start(FFormat);
    D.AddStr('RequestID', ARequestInfo.Params.Values['RequestID']);
    FOwner.ChangeScreen(TGeniusCedScreen.csAgreement);
    A:= False;
    if Assigned(FOwner.FOnAgreement) then begin
      FOwner.FOnAgreement(Self,
        ARequestInfo.Params.Values['RequestID'],
        ARequestInfo.Params.Values['Title'],
        ARequestInfo.Params.Values['AgreementText'],
        ARequestInfo.Params.Values['AcceptLabel'],
        ARequestInfo.Params.Values['DeclineLabel'],
        A);
      //Success, Timeout, POSCancelled
      //NO! It's either "Accepted" or "Declined" by default...
      D.AddStr('Status', IfThen(A, 'Accepted', 'Declined')); //TODO!!!
    end else begin
      D.AddStr('Status', 'Timeout'); //TODO!!!
    end;

    AResponseInfo.ContentText:= D.AsString;
  finally
    Self.FOwner.FInAgreement:= False;
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleGetCustomerInput(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
begin
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'GetCustomerInputResponse';
    D.Start(FFormat);


    //TODO: Show user prompt...

    //TODO: Wait for response...


    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

procedure TCayanGeniusEmulatorContext.HandleGetSignature(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  D: TCombinedData;
begin
  D:= TCombinedData.Create;
  try
    D.RootElement:= 'GetSignatureResult';
    D.Start(FFormat);


    //TODO: Show user prompt...

    //TODO: Wait for response...


    AResponseInfo.ContentText:= D.AsString;
  finally
    FreeAndNil(D);
  end;
end;

end.
