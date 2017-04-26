unit Cayan.Genius;

interface

uses
  System.Classes, System.SysUtils,
  Cayan,
  Cayan.Genius.Intf,
  Cayan.Genius.Impl,
  Cayan.MWv4.Intf,
  Cayan.MWv4.Impl,
  Cayan.Common;

type
  TCayanGeniusDevice = class;
  TCayanGenius = class;

  TCayanGeniusDevice = class(TPersistent)
  private
    FOwner: TCayanGenius;
    function GetDeviceAddress: String;
    function GetDevicePort: Integer;
    function GetDeviceProtocol: TGeniusProtocol;
    function GetDeviceTimeout: Integer;
    function GetDeviceVersion: TGeniusDeviceVersion;
    procedure SetDeviceAddress(const Value: String);
    procedure SetDevicePort(const Value: Integer);
    procedure SetDeviceProtocol(const Value: TGeniusProtocol);
    procedure SetDeviceTimeout(const Value: Integer);
    procedure SetDeviceVersion(const Value: TGeniusDeviceVersion);
    function GetMonitoring: Boolean;
    procedure SetMonitoring(const Value: Boolean);
  public
    constructor Create(AOwner: TCayanGenius);
    destructor Destroy; override;
  published
    property DeviceAddress: String read GetDeviceAddress write SetDeviceAddress;
    property DevicePort: Integer read GetDevicePort write SetDevicePort;
    property DeviceProtocol: TGeniusProtocol read GetDeviceProtocol write SetDeviceProtocol;
    property DeviceVersion: TGeniusDeviceVersion read GetDeviceVersion write SetDeviceVersion;
    property DeviceTimeout: Integer read GetDeviceTimeout write SetDeviceTimeout;
    property Monitoring: Boolean read GetMonitoring write SetMonitoring;
  end;

  TCayanGeniusTransactionEvent = procedure(const ATrans: TCayanGenius) of object;

  TCayanGeniusTransactionResultEvent = procedure(const ATrans: TCayanGenius;
      const AResult: IGeniusTransactionResponse) of object;

  ///  <summary>
  ///    Encapsulates the full transaction process using a Genius CED
  ///    payment terminal provided by Cayan.
  ///  </summary>
  TCayanGenius = class(TCayanBaseComponent)
  private
    FDevice: TCayanGeniusDevice;
    FMerchantWare: TMerchantWare;
    FGenius: TGenius;
    FOrder: IGeniusStartOrderResponse;
    FStaging: IGeniusStageResponse;
    FResult: IGeniusTransactionResponse;
    FTransactionID: String;
    FInvoiceNum: String;
    FForceDuplicate: Boolean;
    FTransactionType: TGeniusTransactionType;
    FPONumber: String;
    FAmount: Currency;
    FTaxAmount: Currency;
    FCustomerCode: String;
    FCardholder: String;
    FOnTransactionStart: TCayanGeniusTransactionEvent;
    FOnTransactionResult: TCayanGeniusTransactionResultEvent;
    FOnTransactionStaged: TCayanGeniusTransactionEvent;
    FOnDeviceStatus: TGeniusStatusResponseEvent;
    FOnCancel: TNotifyEvent;
    procedure SetAmount(const Value: Currency);
    procedure SetForceDuplicate(const Value: Boolean);
    procedure SetInvoiceNum(const Value: String);
    procedure SetPONumber(const Value: String);
    procedure SetTransactionID(const Value: String);
    procedure SetTransactionType(const Value: TGeniusTransactionType);
    function GetAmount: Currency;
    function GetTaxAmount: Currency;
    procedure SetTaxAmount(const Value: Currency);
    procedure DoStage;
    procedure SetCardholder(const Value: String);
    procedure SetCustomerCode(const Value: String);
    procedure ThreadTransactionResponse(Response: IGeniusTransactionResponse);
    procedure PrepareGenius;
    procedure FreeOrder;
    procedure SetDevice(const Value: TCayanGeniusDevice);
    procedure GeniusDeviceStatus(Sender: IGenius;
      const Status: IGeniusStatusResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Genius: IGenius;
    function MerchantWare: IMerchantWare;
    function StartNewOrder: Boolean;
    procedure StageTransaction;
    function Cancel: IGeniusCancelTransactionResponse;
  published
    property Device: TCayanGeniusDevice read FDevice write SetDevice;
    property TransactionType: TGeniusTransactionType read FTransactionType write SetTransactionType;
    property Amount: Currency read GetAmount write SetAmount;
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;
    property InvoiceNum: String read FInvoiceNum write SetInvoiceNum;
    property TransactionID: String read FTransactionID write SetTransactionID;
    property ForceDuplicate: Boolean read FForceDuplicate write SetForceDuplicate;
    property PONumber: String read FPONumber write SetPONumber;
    property Cardholder: String read FCardholder write SetCardholder;
    property CustomerCode: String read FCustomerCode write SetCustomerCode;

    property OnDeviceStatus: TGeniusStatusResponseEvent
      read FOnDeviceStatus write FOnDeviceStatus;
    property OnTransactionStart: TCayanGeniusTransactionEvent read FOnTransactionStart write FOnTransactionStart;
    property OnTransactionStaged: TCayanGeniusTransactionEvent read FOnTransactionStaged write FOnTransactionStaged;
    property OnTransactionResult: TCayanGeniusTransactionResultEvent read FOnTransactionResult write FOnTransactionResult;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

implementation

{ TCayanGeniusDevice }

constructor TCayanGeniusDevice.Create(AOwner: TCayanGenius);
begin
  FOwner:= AOwner;
end;

destructor TCayanGeniusDevice.Destroy;
begin

  inherited;
end;

function TCayanGeniusDevice.GetDeviceAddress: String;
begin
  Result:= FOwner.FGenius.DeviceAddress;
end;

function TCayanGeniusDevice.GetDevicePort: Integer;
begin
  Result:= FOwner.FGenius.DevicePort;
end;

function TCayanGeniusDevice.GetDeviceProtocol: TGeniusProtocol;
begin
  Result:= FOwner.FGenius.DeviceProtocol;
end;

function TCayanGeniusDevice.GetDeviceTimeout: Integer;
begin
  Result:= FOwner.FGenius.DeviceTimeout;
end;

function TCayanGeniusDevice.GetDeviceVersion: TGeniusDeviceVersion;
begin
  Result:= FOwner.FGenius.DeviceVersion;
end;

procedure TCayanGeniusDevice.SetDeviceAddress(const Value: String);
begin
  FOwner.FGenius.DeviceAddress:= Value;
end;

procedure TCayanGeniusDevice.SetDevicePort(const Value: Integer);
begin
  FOwner.FGenius.DevicePort:= Value;
end;

procedure TCayanGeniusDevice.SetDeviceProtocol(const Value: TGeniusProtocol);
begin
  FOwner.FGenius.DeviceProtocol:= Value;
end;

procedure TCayanGeniusDevice.SetDeviceTimeout(const Value: Integer);
begin
  FOwner.FGenius.DeviceTimeout:= Value;
end;

procedure TCayanGeniusDevice.SetDeviceVersion(const Value: TGeniusDeviceVersion);
begin
  FOwner.FGenius.DeviceVersion:= Value;
end;

function TCayanGeniusDevice.GetMonitoring: Boolean;
begin
  Result:= FOwner.FGenius.Monitoring;
end;

procedure TCayanGeniusDevice.SetMonitoring(const Value: Boolean);
begin
  FOwner.FGenius.Monitoring:= Value;
end;

{ TCayanGenius }

constructor TCayanGenius.Create(AOwner: TComponent);
begin
  inherited;
  FMerchantWare:= TMerchantWare.Create;
  IMerchantWare(FMerchantWare)._AddRef;
  FGenius:= TGenius.Create(FMerchantWare);
  IGenius(FGenius)._AddRef;
  FGenius.OnStatus:= GeniusDeviceStatus;
  FDevice:= TCayanGeniusDevice.Create(Self);
end;

destructor TCayanGenius.Destroy;
begin
  if Assigned(FOrder) then begin
    FOrder._Release;
    FOrder:= nil;
  end;
  if Assigned(FStaging) then begin
    FStaging._Release;
    FStaging:= nil;
  end;
  IGenius(FGenius)._Release;
  FGenius:= nil;
  IMerchantWare(FMerchantWare)._Release;
  FMerchantWare:= nil;
  FreeAndNil(FDevice);
  inherited;
end;

function TCayanGenius.Cancel: IGeniusCancelTransactionResponse;
begin
  if Assigned(Self.FOnCancel) then
    Self.FOnCancel(Self);
  Result:= FGenius.CancelTransaction;
end;

function TCayanGenius.Genius: IGenius;
begin
  Result:= FGenius;
end;

procedure TCayanGenius.GeniusDeviceStatus(Sender: IGenius;
  const Status: IGeniusStatusResponse);
begin
  if Assigned(Self.FOnDeviceStatus) then
    Self.FOnDeviceStatus(Sender, Status);
end;

function TCayanGenius.GetAmount: Currency;
begin
  Result:= FGenius.LineItems.OrderTotal;
end;

function TCayanGenius.GetTaxAmount: Currency;
begin
  Result:= FGenius.LineItems.OrderTax;
end;

function TCayanGenius.MerchantWare: IMerchantWare;
begin
  Result:= FMerchantWare;
end;

procedure TCayanGenius.SetAmount(const Value: Currency);
begin
  FAmount := Value;
end;

procedure TCayanGenius.SetCardholder(const Value: String);
begin
  FCardholder := Value;
end;

procedure TCayanGenius.SetCustomerCode(const Value: String);
begin
  FCustomerCode := Value;
end;

procedure TCayanGenius.SetDevice(const Value: TCayanGeniusDevice);
begin
  Self.FDevice.Assign(Value);
end;

procedure TCayanGenius.SetForceDuplicate(const Value: Boolean);
begin
  FForceDuplicate := Value;
end;

procedure TCayanGenius.SetInvoiceNum(const Value: String);
begin
  FInvoiceNum := Value;
end;

procedure TCayanGenius.SetPONumber(const Value: String);
begin
  FPONumber := Value;
end;

procedure TCayanGenius.SetTaxAmount(const Value: Currency);
begin
  FTaxAmount:= Value;
end;

procedure TCayanGenius.SetTransactionID(const Value: String);
begin
  FTransactionID := Value;
end;

procedure TCayanGenius.SetTransactionType(
  const Value: TGeniusTransactionType);
begin
  FTransactionType := Value;
end;

procedure TCayanGenius.PrepareGenius;
begin
  Self.FMerchantWare.Name:= Self.Cayan.MerchantName;
  Self.FMerchantWare.SiteId:= Self.Cayan.MerchantSiteId;
  Self.FMerchantWare.Key:= Self.Cayan.MerchantKey;
  Self.FMerchantWare.TestMode:= Self.Cayan.TestMode;
  FGenius.TransactionResponse:= Self.ThreadTransactionResponse;
end;

procedure TCayanGenius.DoStage;
var
  Req: IGeniusStageRequest;
  EM: String;
  X: Integer;
begin
  if Assigned(FStaging) then begin
    FStaging._Release;
    FStaging:= nil;
  end;

  if not Assigned(Cayan) then begin
    raise Exception.Create('Cayan component not assigned.');
  end;

  //If device is already in a transaction, cancel it first...
  if FGenius.IsInTransaction then begin
    Cancel;
  end;

  Req:= FGenius.CreateStageRequest;
  try
    Req.TransactionType:= Self.FTransactionType;
    Req.Amount:= Self.FAmount;
    Req.TaxAmount:= Self.FTaxAmount;
    Req.ClerkId:= Self.Cayan.ClerkID;
    Req.OrderNumber:= Self.InvoiceNum;
    Req.Dba:= Self.Cayan.Dba;
    Req.SoftwareName:= Self.Cayan.SoftwareName;
    Req.SoftwareVersion:= Self.Cayan.SoftwareVersion;
    Req.CardHolder:= Self.Cardholder;
    Req.TransactionId:= Self.TransactionID;
    Req.ForceDuplicate:= Self.ForceDuplicate;
    Req.CustomerCode:= Self.CustomerCode;
    Req.PoNumber:= Self.PONumber;
    //Req.TerminalId:= Self.Cayan.StationID; //TODO

    PrepareGenius;

    FStaging:= FGenius.StageTransaction(Req);
    FStaging._AddRef;
    if FStaging.MessageCount > 0 then begin
      EM:= '';
      for X := 0 to FStaging.MessageCount-1 do begin
        EM:= EM + FStaging.Messages[X].Field + ': ' + FStaging.Messages[X].Information + sLineBreak;
      end;
      raise Exception.Create('Cayan responded with the following messages: ' + sLineBreak + EM);
    end else begin
      if Assigned(FOnTransactionStaged) then
        FOnTransactionStaged(Self);
      //TODO: Implement initiating keyed entry instead of Genius CED...
      if FGenius.InitiateTransaction(FStaging.TransportKey) then begin
        if Assigned(FOnTransactionStart) then
          FOnTransactionStart(Self);
      end else begin
        raise Exception.Create('Failed to initiate transaction!');
      end;
    end;
  except
    on E: Exception do begin
      raise Exception.Create('Failed to stage transaction: ' + E.Message);
    end;
  end;
end;

procedure TCayanGenius.ThreadTransactionResponse(Response: IGeniusTransactionResponse);
begin
  Self.FResult:= Response;
  if Assigned(Self.FOnTransactionResult) then
    Self.FOnTransactionResult(Self, Response);
end;

procedure TCayanGenius.FreeOrder;
begin
  if Assigned(FOrder) then begin
    FOrder._Release;
    FOrder:= nil;
  end;
end;

function TCayanGenius.StartNewOrder: Boolean;
begin
  Result:= False;
  try
    FreeOrder;
    if FGenius.LineItems.InProgress then begin
      Cancel;
    end;
    FOrder:= FGenius.LineItems.StartOrder(InvoiceNum);
    FOrder._AddRef;
    case FOrder.Status of
      soSuccess: begin
        Result:= True;
      end;
      soDenied: begin
        raise Exception.Create('CED denied starting new order: ' + FOrder.ResponseMessage);
      end;
      soError: begin
        raise Exception.Create('Error occurred while starting new order: ' + FOrder.ResponseMessage);
      end;
    end;
  except
    on E: Exception do begin
      raise Exception.Create('Failed to start new order: ' + E.Message);
    end;
  end;
end;

procedure TCayanGenius.StageTransaction;
begin
  //TODO: Only proceed with staging if already started transaction...

  if Assigned(FOrder) then begin
    //TODO: Copy over details from Line Item Display...

  end else begin
    //TODO: Just prepare new transaction without items...

  end;

  DoStage;

end;

end.
