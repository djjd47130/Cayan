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
  Cayan.Genius.LineItems, FMX.TabControl, System.Actions, FMX.ActnList,
  FMX.Edit, FMX.ListBox, FMX.EditBox, FMX.NumberBox;

type
  TfrmCart = class;

  TfrmCart = class(TForm)
    ContentLayout: TLayout;
    LID: TCayanGeniusLineItems;
    Acts: TActionList;
    actItemsTab: TChangeTabAction;
    actLookupTab: TChangeTabAction;
    actDetailTab: TChangeTabAction;
    CartTabs: TTabControl;
    tabItems: TTabItem;
    ToolBar3: TToolBar;
    lblTitle: TLabel;
    btnNext: TSpeedButton;
    btnBack: TSpeedButton;
    lstItems: TListView;
    ButtonsLayout: TGridPanelLayout;
    btnCartAdd: TButton;
    btnCartEdit: TButton;
    btnCartDelete: TButton;
    TotalsLayout: TGridPanelLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblCartQty: TLabel;
    lblCartSubtotal: TLabel;
    lblCartTax: TLabel;
    lblCartTotal: TLabel;
    tabLookup: TTabItem;
    ToolBar1: TToolBar;
    Label3: TLabel;
    SpeedButton2: TSpeedButton;
    tabDetail: TTabItem;
    ToolBar2: TToolBar;
    Label6: TLabel;
    SpeedButton4: TSpeedButton;
    lstLookup: TListView;
    lstSetup: TListBox;
    ListBoxGroupHeader13: TListBoxGroupHeader;
    liID: TListBoxItem;
    txtID: TEdit;
    liSKU: TListBoxItem;
    txtSKU: TEdit;
    liUPC: TListBoxItem;
    txtUPC: TEdit;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    liQuantity: TListBoxItem;
    liPrice: TListBoxItem;
    txtPrice: TNumberBox;
    txtQty: TNumberBox;
    procedure btnCartDeleteClick(Sender: TObject);
    procedure lstLookupSearchChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure lstLookupItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure btnCartEditClick(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure lstItemsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure lstItemsClick(Sender: TObject);
  private
    FSetup: ICayanPOSSetup;
    FItems: ICayanPOSItems;
    procedure SetSetup(const Value: ICayanPOSSetup);
    procedure DisplayItems;
    function ItemByID(const ID: Integer): ICayanPOSItem;
  public
    constructor Create(AContainer: TControl); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure UpdateTotals;
    property Setup: ICayanPOSSetup read FSetup write SetSetup;
  end;

var
  frmCart: TfrmCart;

implementation

{$R *.fmx}

uses
  uCayanPOSMain,
  uDialog;

{ TfrmCart }

constructor TfrmCart.Create(AContainer: TControl);
begin
  if not Assigned(AContainer) then begin
    MsgPrompt('Failed to create cart screen: Container must be assigned.',
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK);
  end;
  inherited Create(nil);
  ContentLayout.Parent:= AContainer;
  CartTabs.TabPosition:= TTabPosition.None;
  CartTabs.ActiveTab:= Self.tabItems;

  UpdateTotals;
end;

destructor TfrmCart.Destroy;
begin
  Clear;
  if Assigned(FItems) then begin
    FItems._Release;
    FItems:= nil;
  end;
  FSetup:= nil;
  inherited;
end;

function TfrmCart.ItemByID(const ID: Integer): ICayanPOSItem;
var
  X: Integer;
begin
  Result:= nil;
  for X := 0 to FItems.Count-1 do begin
    if FItems[X].ID = ID then begin
      Result:= FItems[X];
      Break;
    end;
  end;
end;

procedure TfrmCart.lstItemsClick(Sender: TObject);
begin
  UpdateTotals;
end;

procedure TfrmCart.lstItemsItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  UpdateTotals;
end;

procedure TfrmCart.lstLookupItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  I: ICayanPOSItem;
  LineItem: TCayanGeniusLineItem;
  LI: TListViewItem;
begin
  I:= ItemByID(AItem.Tag);
  LineItem:= LID.Add(glSku, 'Invent', I.Price, (I.Price * FSetup.TaxRate), 1, I.ShortDescr); //TODO
  LI:= lstItems.Items.Add;
  LI.Text:= IntToStr(LineItem.Quantity) + ' ' + LineItem.Description;
  LI.Detail:= FormatFloat('$#,###,##0.00', LineItem.Amount);
  LI.Tag:= NativeInt(LineItem);
  lstItems.ScrollTo(lstItems.Items.Count-1);
  UpdateTotals;
  Self.actItemsTab.ExecuteTarget(Self);
end;

procedure TfrmCart.lstLookupSearchChange(Sender: TObject);
begin
  //TODO: Search for items...



end;

procedure TfrmCart.SetSetup(const Value: ICayanPOSSetup);
begin
  FSetup := Value;
end;

procedure TfrmCart.SpeedButton4Click(Sender: TObject);
begin
  //TODO: Update item...


  actItemsTab.ExecuteTarget(Self);
end;

procedure TfrmCart.btnCartDeleteClick(Sender: TObject);
var
  X: Integer;
  I: TCayanGeniusLineItem;
begin
  if lstItems.ItemIndex >= 0 then begin
    if MsgPrompt('Are you sure you wish to delete this item?',
      TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo) = mrYes then
    begin
      I:= TCayanGeniusLineItem(lstItems.Items[lstItems.ItemIndex].Tag);
      for X := 0 to LID.Count - 1 do begin
        if LID.Items[X] = I then begin
          LID.Delete(X);
          Break;
        end;
      end;
      lstItems.Items.Delete(lstItems.ItemIndex);
    end;
  end else begin
    MsgPrompt('No item is selected.',
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK);
  end;
  UpdateTotals;
end;

procedure TfrmCart.btnCartEditClick(Sender: TObject);
begin
  //TODO: Show details


  actDetailTab.ExecuteTarget(Self);
end;

procedure TfrmCart.Button1Click(Sender: TObject);
begin
  if Assigned(FItems) then begin
    FItems._Release;
    FItems:= nil;
  end;

  Self.actLookupTab.ExecuteTarget(Self);

  FItems:= DM.POS.GetInventory(''); //TODO
  FItems._AddRef;
  DisplayItems;
end;

procedure TfrmCart.DisplayItems;
var
  I: ICayanPOSItem;
  LI: TListViewItem;
  X: Integer;
begin
  Self.lstLookup.Items.Clear;

  for X := 0 to FItems.Count-1 do begin
    I:= FItems[X];
    LI:= Self.lstLookup.Items.Add;
    LI.Text:= I.ShortDescr;
    LI.Detail:= FormatFloat('$#,###,##0.00', I.Price);
    LI.Tag:= I.ID;

  end;

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
  btnCartDelete.Enabled:= (lstItems.ItemIndex >= 0);
end;

end.
