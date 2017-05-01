unit Cayan.Genius.Transactions;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Cayan,
  Cayan.Genius,
  Cayan.Genius.Intf;

type
  TCayanGeniusTransaction = class;

  TCayanGeniusTransactionEvent = procedure(const ATrans: TCayanGeniusTransaction) of object;

  TCayanGeniusTransactionResultEvent = procedure(const ATrans: TCayanGeniusTransaction;
      const AResult: IGeniusTransactionResponse) of object;

  TCayanGeniusTransaction = class(TComponent)
  private
    FGenius: TCayanGenius;
    FStaging: IGeniusStageResponse;
    FResult: IGeniusTransactionResponse;
    FTransactionID: String;
    FInvoiceNum: String;
    FTransactionType: TGeniusTransactionType;
    FPONumber: String;
    FCustomerCode: String;
    FCardholder: String;
    FAmount: Currency;
    FTaxAmount: Currency;
    FOnTransactionStart: TCayanGeniusTransactionEvent;
    FOnTransactionResult: TCayanGeniusTransactionResultEvent;
    FOnTransactionStaged: TCayanGeniusTransactionEvent;
    procedure SetGenius(const Value: TCayanGenius);
    procedure SetAmount(const Value: Currency);
    procedure SetCardholder(const Value: String);
    procedure SetCustomerCode(const Value: String);
    procedure SetInvoiceNum(const Value: String);
    procedure SetPONumber(const Value: String);
    procedure SetTaxAmount(const Value: Currency);
    procedure SetTransactionID(const Value: String);
    procedure SetTransactionType(const Value: TGeniusTransactionType);
    procedure PrepareGenius;
    procedure ThreadTransactionResponse(Response: IGeniusTransactionResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTransaction;
  published
    property Genius: TCayanGenius read FGenius write SetGenius;

    property TransactionType: TGeniusTransactionType read FTransactionType write SetTransactionType;
    property Amount: Currency read FAmount write SetAmount;
    property TaxAmount: Currency read FTaxAmount write SetTaxAmount;
    property InvoiceNum: String read FInvoiceNum write SetInvoiceNum;
    property TransactionID: String read FTransactionID write SetTransactionID;
    property PONumber: String read FPONumber write SetPONumber;
    property Cardholder: String read FCardholder write SetCardholder;
    property CustomerCode: String read FCustomerCode write SetCustomerCode;

    property OnTransactionStart: TCayanGeniusTransactionEvent read FOnTransactionStart write FOnTransactionStart;
    property OnTransactionStaged: TCayanGeniusTransactionEvent read FOnTransactionStaged write FOnTransactionStaged;
    property OnTransactionResult: TCayanGeniusTransactionResultEvent read FOnTransactionResult write FOnTransactionResult;
  end;

implementation

{ TCayanGeniusTransaction }

constructor TCayanGeniusTransaction.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TCayanGeniusTransaction.Destroy;
begin
  if Assigned(FStaging) then begin
    FStaging._Release;
    FStaging:= nil;
  end;
  inherited;
end;

procedure TCayanGeniusTransaction.SetAmount(const Value: Currency);
begin
  FAmount := Value;
end;

procedure TCayanGeniusTransaction.SetCardholder(const Value: String);
begin
  FCardholder := Value;
end;

procedure TCayanGeniusTransaction.SetCustomerCode(const Value: String);
begin
  FCustomerCode := Value;
end;

procedure TCayanGeniusTransaction.SetGenius(const Value: TCayanGenius);
begin
  FGenius := Value;
end;

procedure TCayanGeniusTransaction.SetInvoiceNum(const Value: String);
begin
  FInvoiceNum := Value;
end;

procedure TCayanGeniusTransaction.SetPONumber(const Value: String);
begin
  FPONumber := Value;
end;

procedure TCayanGeniusTransaction.SetTaxAmount(const Value: Currency);
begin
  FTaxAmount := Value;
end;

procedure TCayanGeniusTransaction.SetTransactionID(const Value: String);
begin
  FTransactionID := Value;
end;

procedure TCayanGeniusTransaction.SetTransactionType(
  const Value: TGeniusTransactionType);
begin
  FTransactionType := Value;
end;

procedure TCayanGeniusTransaction.PrepareGenius;
begin
  Genius.MerchantWare.Name:= Genius.Cayan.MerchantName;
  Genius.MerchantWare.SiteId:= Genius.Cayan.MerchantSiteId;
  Genius.MerchantWare.Key:= Genius.Cayan.MerchantKey;
  Genius.MerchantWare.TestMode:= Genius.Cayan.TestMode;
  Genius.Genius.TransactionResponse:= Self.ThreadTransactionResponse;
end;

procedure TCayanGeniusTransaction.ThreadTransactionResponse(Response: IGeniusTransactionResponse);
begin
  Self.FResult:= Response;
  if Assigned(Self.FOnTransactionResult) then
    Self.FOnTransactionResult(Self, Response);
end;

procedure TCayanGeniusTransaction.StartTransaction;
var
  Req: IGeniusStageRequest;
  EM: String;
  X: Integer;
begin
  if Assigned(FStaging) then begin
    FStaging._Release;
    FStaging:= nil;
  end;

  if not Assigned(FGenius) then begin
    raise Exception.Create('Cayan component not assigned.');
  end;

  //If device is already in a transaction, cancel it first...
  if Genius.Genius.IsInTransaction then begin
    Genius.Cancel;
  end;

  Req:= Genius.Genius.CreateStageRequest;
  try
    Req.TransactionType:= Self.FTransactionType;
    Req.Amount:= Self.FAmount;
    Req.TaxAmount:= Self.FTaxAmount;
    Req.ClerkId:= Genius.Cayan.ClerkID;
    Req.OrderNumber:= Self.InvoiceNum;
    Req.Dba:= Genius.Cayan.Dba;
    Req.SoftwareName:= Genius.Cayan.SoftwareName;
    Req.SoftwareVersion:= Genius.Cayan.SoftwareVersion;
    Req.CardHolder:= Self.Cardholder;
    Req.TransactionId:= Self.TransactionID;
    Req.ForceDuplicate:= False; // Genius.ForceDuplicate; //TODO
    Req.CustomerCode:= Self.CustomerCode;
    Req.PoNumber:= Self.PONumber;
    //Req.TerminalId:= Self.Cayan.StationID; //TODO

    PrepareGenius;

    FStaging:= FGenius.Genius.StageTransaction(Req);
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
      if Genius.Genius.InitiateTransaction(FStaging.TransportKey) then begin
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

end.
