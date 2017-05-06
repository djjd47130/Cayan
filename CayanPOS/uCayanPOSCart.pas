unit uCayanPOSCart;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ListView, FMX.Layouts,
  uDM,
  Cayan,
  Cayan.POS,
  Cayan.Genius,
  Cayan.Genius.Intf,
  Cayan.Genius.LineItems;

type
  TfrmCart = class;

  {
  TCartItemType = (citInvent, citCharge);

  TCartItem = class(TObject)
  private
    FOwner: TfrmCart;
    FLineItem: TCayanGeniusLineItem;
    FItemID: Integer;
    FPrice: Currency;
    FQty: Integer;
    FItemType: TCartItemType;
    FID: Integer;
    procedure SetID(const Value: Integer);
    procedure SetItemID(const Value: Integer);
    procedure SetItemType(const Value: TCartItemType);
    procedure SetPrice(const Value: Currency);
    procedure SetQty(const Value: Integer);
  public
    constructor Create(AOwner: TfrmCart);
    destructor Destroy; override;
    function SubTotal: Currency;
    function TotalTax: Currency;
    function GrandTotal: Currency;
    property ID: Integer read FID write SetID;
    property ItemID: Integer read FItemID write SetItemID;
    property ItemType: TCartItemType read FItemType write SetItemType;
    property Qty: Integer read FQty write SetQty;
    property Price: Currency read FPrice write SetPrice;
  end;
  }

  TfrmCart = class(TForm)
    ContentLayout: TLayout;
    ButtonsLayout: TGridPanelLayout;
    btnCartAdd: TButton;
    btnCartEdit: TButton;
    btnCartDelete: TButton;
    lstItems: TListView;
    ToolBar3: TToolBar;
    lblTitle: TLabel;
    btnNext: TSpeedButton;
    btnBack: TSpeedButton;
    TotalsLayout: TGridPanelLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblCartQty: TLabel;
    lblCartSubtotal: TLabel;
    lblCartTax: TLabel;
    lblCartTotal: TLabel;
    LID: TCayanGeniusLineItems;
    procedure btnCartAddClick(Sender: TObject);
    procedure btnCartDeleteClick(Sender: TObject);
  private
    //FItems: TObjectList<TCartItem>;
    FSetup: ICayanPOSSetup;
    procedure SetSetup(const Value: ICayanPOSSetup);
  public
    constructor Create(AContainer: TControl); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    //function AddItem(const AItemID: Integer): TCartItem;
    //procedure Delete(const Index: Integer);
    procedure UpdateTotals;
    property Setup: ICayanPOSSetup read FSetup write SetSetup;
  end;

var
  frmCart: TfrmCart;

implementation

{$R *.fmx}

uses
  uCayanPOSMain;

{ TCartItem }

{
constructor TCartItem.Create(AOwner: TfrmCart);
begin
  FOwner:= AOwner;
  FLineItem:= nil;

end;

destructor TCartItem.Destroy;
begin

  inherited;
end;

procedure TCartItem.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TCartItem.SetItemID(const Value: Integer);
begin
  FItemID := Value;
end;

procedure TCartItem.SetItemType(const Value: TCartItemType);
begin
  FItemType := Value;
end;

procedure TCartItem.SetPrice(const Value: Currency);
begin
  FPrice := Value;
end;

procedure TCartItem.SetQty(const Value: Integer);
begin
  FQty := Value;
end;

function TCartItem.SubTotal: Currency;
begin
  Result:= FPrice * FQty;
end;

function TCartItem.TotalTax: Currency;
begin
  Result:= SubTotal * FOwner.FSetup.TaxRate;
end;

function TCartItem.GrandTotal: Currency;
begin
  Result:= SubTotal + TotalTax;
end;
}

{ TfrmCart }

constructor TfrmCart.Create(AContainer: TControl);
begin
  if not Assigned(AContainer) then
    raise Exception.Create('Failed to create cart screen: Container must be assigned.');
  inherited Create(nil);
  ContentLayout.Parent:= AContainer;
  //FItems:= TObjectList<TCartItem>.Create(True);

  UpdateTotals;
end;

destructor TfrmCart.Destroy;
begin
  Clear;
  //FreeAndNil(FItems);
  inherited;
end;

procedure TfrmCart.SetSetup(const Value: ICayanPOSSetup);
begin
  FSetup := Value;
end;

{
function TfrmCart.AddItem(const AItemID: Integer): TCartItem;
begin
  Result:= TCartItem.Create(Self);
  Result.ItemType:= TCartItemType.citInvent;
  Result.ItemID:= AItemID;
  //TODO: Load item from database...

  UpdateTotals;
end;

procedure TfrmCart.Delete(const Index: Integer);
var
  I: TCartItem;
begin
  I:= FItems[Index];
  try
    //TODO: Remove from Line Item Display...
    I.FLineItem.Description; //TEMP

  finally
    FItems.Delete(Index);
  end;
  UpdateTotals;
end;
}

procedure TfrmCart.btnCartAddClick(Sender: TObject);
var
  I: TCayanGeniusLineItem;
  LI: TListViewItem;
  Price: Currency;
begin
  Price:= (Random(200) + 5);
  try
    I:= LID.Add(glSku, 'Inventory', Price, (Price * FSetup.TaxRate), 1, 'New Item'); //TODO
    LI:= lstItems.Items.Add;
    LI.Text:= IntToStr(I.Quantity) + ' ' + I.Description;
    LI.Detail:= FormatFloat('$#,###,##0.00', (I.Amount * I.Quantity));
    LI.Tag:= NativeInt(I);
    UpdateTotals;
  except
    on E: Exception do begin
      //TODO
    end;
  end;
end;

procedure TfrmCart.btnCartDeleteClick(Sender: TObject);
var
  I: TCayanGeniusLineItem;
  X: Integer;
begin
  if lstItems.ItemIndex >= 0 then begin
    I:= TCayanGeniusLineItem(lstItems.Items[lstItems.ItemIndex].Tag);
    for X := 0 to LID.Count - 1 do begin
      if LID.Items[X] = I then begin
        LID.Delete(X);
        Break;
      end;
    end;
    lstItems.Items.Delete(lstItems.ItemIndex);
  end;
  UpdateTotals;
end;

procedure TfrmCart.Clear;
begin
  lstItems.Items.Clear;
  LID.Clear;
  LID.Transaction.Genius.Cancel;
  UpdateTotals;
end;

procedure TfrmCart.UpdateTotals;
begin
  lblCartQty.Text:= IntToStr(LID.TotalQty);
  lblCartSubtotal.Text:= FormatFloat('$#,###,##0.00', LID.Subtotal);
  lblCartTax.Text:= FormatFloat('$#,###,##0.00', LID.OrderTax);
  lblCartTotal.Text:= FormatFloat('$#,###,##0.00', LID.OrderTotal);
end;

end.
