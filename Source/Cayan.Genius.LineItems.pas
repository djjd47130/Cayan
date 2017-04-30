unit Cayan.Genius.LineItems;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Cayan,
  Cayan.Genius,
  Cayan.Genius.Intf,
  Cayan.Genius.Transactions;

type
  TCayanGeniusLineItem = class;
  TCayanGeniusLineItems = class;

  TCayanGeniusLineItemsEvent = procedure(Sender: TCayanGeniusLineItems) of object;

  TCayanGeniusLineItem = class(TObject)
  private
    FOwner: TCayanGeniusLineItems;
    FLineItem: IGeniusLineItem;
    function GetAmount: Currency;
    function GetCategory: TGeniusLineItemCategory;
    function GetDescription: String;
    function GetItemID: String;
    function GetItemType: TGeniusLineItemType;
    function GetItemTypeValue: String;
    function GetQuantity: Integer;
    function GetTaxAmount: Currency;
    function GetUPC: String;
    procedure SetAmount(const Value: Currency);
    procedure SetCategory(const Value: TGeniusLineItemCategory);
    procedure SetDescription(const Value: String);
    procedure SetItemID(const Value: String);
    procedure SetItemType(const Value: TGeniusLineItemType);
    procedure SetItemTypeValue(const Value: String);
    procedure SetQuantity(const Value: Integer);
    procedure SetTaxAmount(const Value: Currency);
    procedure SetUPC(const Value: String);
  public
    constructor Create(AOwner: TCayanGeniusLineItems);
    destructor Destroy; override;

    property ItemID: String read GetItemID write SetItemID; //1-8
    property ItemType: TGeniusLineItemType read GetItemType write SetItemType;  //1-8
    property ItemTypeValue: String read GetItemTypeValue write SetItemTypeValue;  //1-32
    property UPC: String read GetUPC write SetUPC;  //1-32
    property Quantity: Integer read GetQuantity write SetQuantity;  //1-3
    property Description: String read GetDescription write SetDescription;  //1-35
    property Amount: Currency read GetAmount write SetAmount; //1-9
    property TaxAmount: Currency read GetTaxAmount write SetTaxAmount;  //1-9
    property Category: TGeniusLineItemCategory read GetCategory write SetCategory;  //1-8
  end;

  TCayanGeniusLineItems = class(TComponent)
  private
    FItems: TList<TCayanGeniusLineItem>;
    FAutoTotal: Boolean;
    FOnChange: TCayanGeniusLineItemsEvent;
    FOrderTax: Currency;
    FOrderTotal: Currency;
    FDisplayCustomSubTotal: String;
    FTransaction: TCayanGeniusTransaction;
    procedure SetAutoTotal(const Value: Boolean);
    procedure SetOrderTax(const Value: Currency);
    procedure SetOrderTotal(const Value: Currency);
    function GetOrderTax: Currency;
    function GetOrderTotal: Currency;
    procedure SetDisplayCustomSubTotal(const Value: String);
    function GetItems(const Index: Integer): TCayanGeniusLineItem;
    procedure SetItems(const Index: Integer; const Value: TCayanGeniusLineItem);
    procedure SetTransaction(const Value: TCayanGeniusTransaction);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate;
    function InOrder: Boolean;
    function Count: Integer;
    function TotalQty: Integer;
    function Subtotal: Currency;
    procedure Clear(const IgnoreDevice: Boolean = False);
    function Add(const ItemType: TGeniusLineItemType;
      const TypeValue: String; const Amount: Currency; const Tax: Currency;
      const Qty: Integer; const Description: String;
      const Category: TGeniusLineItemCategory = glNone; const DisplayOverride: String = '';
      const UPC: String = ''): TCayanGeniusLineItem;
    procedure Delete(const Index: Integer; const IgnoreDevice: Boolean = False);
    procedure StartOrder;
    procedure UpdateTotal;

    property Items[const Index: Integer]: TCayanGeniusLineItem read GetItems write SetItems;
  published
    property Transaction: TCayanGeniusTransaction read FTransaction write SetTransaction;
    property AutoTotal: Boolean read FAutoTotal write SetAutoTotal;
    property DisplayCustomSubTotal: String read FDisplayCustomSubTotal write SetDisplayCustomSubTotal;
    property OrderTotal: Currency read GetOrderTotal write SetOrderTotal;
    property OrderTax: Currency read GetOrderTax write SetOrderTax;

    property OnChange: TCayanGeniusLineItemsEvent read FOnChange write FOnChange;
  end;

implementation

{ TCayanGeniusLineItem }

constructor TCayanGeniusLineItem.Create(AOwner: TCayanGeniusLineItems);
begin
  FLineItem:= nil;
end;

destructor TCayanGeniusLineItem.Destroy;
begin

  inherited;
end;

function TCayanGeniusLineItem.GetAmount: Currency;
begin
  Result:= FLineItem.Amount;
end;

function TCayanGeniusLineItem.GetCategory: TGeniusLineItemCategory;
begin
  Result:= FLineItem.Category;
end;

function TCayanGeniusLineItem.GetDescription: String;
begin
  Result:= FLineItem.Description;
end;

function TCayanGeniusLineItem.GetItemID: String;
begin
  Result:= FLineItem.ItemID;
end;

function TCayanGeniusLineItem.GetItemType: TGeniusLineItemType;
begin
  Result:= FLineItem.ItemType;
end;

function TCayanGeniusLineItem.GetItemTypeValue: String;
begin
  Result:= FLineItem.ItemTypeValue;
end;

function TCayanGeniusLineItem.GetQuantity: Integer;
begin
  Result:= FLineItem.Quantity;
end;

function TCayanGeniusLineItem.GetTaxAmount: Currency;
begin
  Result:= FLineItem.TaxAmount;
end;

function TCayanGeniusLineItem.GetUPC: String;
begin
  Result:= FLineItem.UPC;
end;

procedure TCayanGeniusLineItem.SetAmount(const Value: Currency);
begin
  FLineItem.Amount:= Value;
end;

procedure TCayanGeniusLineItem.SetCategory(
  const Value: TGeniusLineItemCategory);
begin
  FLineItem.Category:= Value;
end;

procedure TCayanGeniusLineItem.SetDescription(const Value: String);
begin
  FLineItem.Description:= Value;
end;

procedure TCayanGeniusLineItem.SetItemID(const Value: String);
begin
  FLineItem.ItemID:= Value;
end;

procedure TCayanGeniusLineItem.SetItemType(const Value: TGeniusLineItemType);
begin
  FLineItem.ItemType:= Value;
end;

procedure TCayanGeniusLineItem.SetItemTypeValue(const Value: String);
begin
  FLineItem.ItemTypeValue:= Value;
end;

procedure TCayanGeniusLineItem.SetQuantity(const Value: Integer);
begin
  FLineItem.Quantity:= Value;
end;

procedure TCayanGeniusLineItem.SetTaxAmount(const Value: Currency);
begin
  FLineItem.TaxAmount:= Value;
end;

procedure TCayanGeniusLineItem.SetUPC(const Value: String);
begin
  FLineItem.UPC:= Value;
end;

{ TCayanGeniusLineItems }

constructor TCayanGeniusLineItems.Create(AOwner: TComponent);
begin
  inherited;
  FItems:= TList<TCayanGeniusLineItem>.Create;
end;

destructor TCayanGeniusLineItems.Destroy;
begin
  Clear(True);
  FreeAndNil(FItems);
  inherited;
end;

function TCayanGeniusLineItems.InOrder: Boolean;
begin
  if not Assigned(FTransaction) then begin
    raise Exception.Create('Transaction property is not assigned.');
  end;

  if not Assigned(FTransaction.Genius) then begin
    raise Exception.Create('Transaction''s Genius property is not assigned.');
  end;

  Result:= FTransaction.Genius.Genius.LineItems.InProgress;
end;

procedure TCayanGeniusLineItems.Invalidate;
begin
  UpdateTotal;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TCayanGeniusLineItems.GetItems(
  const Index: Integer): TCayanGeniusLineItem;
begin
  Result:= FItems[Index];
end;

function TCayanGeniusLineItems.GetOrderTax: Currency;
var
  X: Integer;
begin
  if FAutoTotal then begin
    Result:= 0;
    for X := 0 to FItems.Count-1 do begin
      Result:= Result + (FItems[X].TaxAmount * FItems[X].Quantity);
    end;
  end else begin
    Result:= FOrderTax;
  end;
end;

function TCayanGeniusLineItems.GetOrderTotal: Currency;
var
  X: Integer;
begin
  if FAutoTotal then begin
    Result:= 0;
    for X := 0 to FItems.Count-1 do begin
      Result:= Result + (FItems[X].TaxAmount * FItems[X].Quantity);
      Result:= Result + (FItems[X].Amount * FItems[X].Quantity);
    end;
  end else begin
    Result:= FOrderTotal;
  end;
end;

function TCayanGeniusLineItems.TotalQty: Integer;
var
  X: Integer;
begin
  Result:= 0;
  for X := 0 to FItems.Count-1 do begin
    Result:= Result + FItems[X].Quantity;
  end;
end;

procedure TCayanGeniusLineItems.UpdateTotal;
begin
  if not (csDesigning in ComponentState) then begin
    if Assigned(FTransaction) then begin
      if Assigned(FTransaction.Genius) then begin
        FTransaction.Genius.Genius.LineItems.AutoTotal:= Self.FAutoTotal;
        FTransaction.Genius.Genius.LineItems.DisplayCustomSubtotal:= FDisplayCustomSubTotal;
        if Self.InOrder then begin
          //FGenius.Genius.LineItems.UpdateTotal(OrderTotal, OrderTax);
        end;
      end;
    end;
  end;
end;

function TCayanGeniusLineItems.Subtotal: Currency;
begin
  Result:= Self.OrderTotal - Self.OrderTax;
end;

function TCayanGeniusLineItems.Add(const ItemType: TGeniusLineItemType;
  const TypeValue: String; const Amount: Currency; const Tax: Currency;
  const Qty: Integer; const Description: String;
  const Category: TGeniusLineItemCategory = glNone; const DisplayOverride: String = '';
  const UPC: String = ''): TCayanGeniusLineItem;
begin
  if not (csDesigning in ComponentState) then begin
    if not Assigned(FTransaction) then begin
      raise Exception.Create('Transaction property is not assigned.');
    end;
    if not Assigned(FTransaction.Genius) then begin
      raise Exception.Create('Transaction''s Genius property is not assigned.');
    end;
    Result:= TCayanGeniusLineItem.Create(Self);
    FItems.Add(Result);
    FTransaction.Genius.Genius.LineItems.DisplayCustomSubtotal:= Self.FDisplayCustomSubTotal;
    Result.FLineItem:= FTransaction.Genius.Genius.LineItems.AddItem(TypeValue, Upc,
      Description, Amount, Tax, Qty, ItemType, Category, DisplayOverride);
    Invalidate;
  end;
end;

procedure TCayanGeniusLineItems.Delete(const Index: Integer; const IgnoreDevice: Boolean = False);
var
  I: TCayanGeniusLineItem;
begin
  if not Assigned(FTransaction) then begin
    raise Exception.Create('Transaction property is not assigned.');
  end;
  if not Assigned(FTransaction.Genius) then begin
    raise Exception.Create('Transaction''s Genius property is not assigned.');
  end;
  try
    if not IgnoreDevice then begin
      I:= FItems[Index];
      if Assigned(I.FLineItem) then begin
        FTransaction.Genius.Genius.LineItems.DeleteItem(I.ItemID);
      end;
    end;
  finally
    FItems.Delete(Index);
  end;
  Invalidate;
end;

procedure TCayanGeniusLineItems.Clear(const IgnoreDevice: Boolean = False);
begin
  try
    if not IgnoreDevice then begin
      if Assigned(FTransaction) then begin
        if Assigned(FTransaction.Genius) then begin
          FTransaction.Genius.Genius.LineItems.ClearItems;
        end;
      end;
    end;
  finally
    while Count > 0 do
      Delete(0, True);
  end;
  Invalidate;
end;

function TCayanGeniusLineItems.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TCayanGeniusLineItems.SetAutoTotal(const Value: Boolean);
begin
  FAutoTotal := Value;
  Invalidate;
end;

procedure TCayanGeniusLineItems.SetDisplayCustomSubTotal(const Value: String);
begin
  FDisplayCustomSubTotal := Value;
  Invalidate;
end;

procedure TCayanGeniusLineItems.SetItems(const Index: Integer;
  const Value: TCayanGeniusLineItem);
begin
  FItems[Index]:= Value;
  Invalidate;
end;

procedure TCayanGeniusLineItems.SetOrderTax(const Value: Currency);
begin
  FOrderTax := Value;
  Invalidate;
end;

procedure TCayanGeniusLineItems.SetOrderTotal(const Value: Currency);
begin
  FOrderTotal := Value;
  Invalidate;
end;

procedure TCayanGeniusLineItems.SetTransaction(
  const Value: TCayanGeniusTransaction);
begin
  FTransaction := Value;
  Invalidate;
end;

procedure TCayanGeniusLineItems.StartOrder;
var
  R: IGeniusStartOrderResponse;
begin
  if not Assigned(FTransaction) then begin
    raise Exception.Create('Transaction property is not assigned.');
  end;

  if not Assigned(FTransaction.Genius) then begin
    raise Exception.Create('Transaction''s Genius property is not assigned.');
  end;

  if FTransaction.InvoiceNum = '' then begin
    raise Exception.Create('Transaction.InvoiceNum property is empty.');
  end;

  if FTransaction.Genius.Genius.LineItems.InProgress then begin
    raise Exception.Create('Line Item Order already in progress.');
  end;

  Self.Clear(True);

  R:= FTransaction.Genius.Genius.LineItems.StartOrder(FTransaction.InvoiceNum);
  case R.Status of
    soSuccess: begin
      //Order was successfully started.

      Self.UpdateTotal;
    end;
    else begin
      raise Exception.Create('Error starting Line Item order: ' + R.ResponseMessage);
    end;
  end;
  Invalidate;
end;

end.
