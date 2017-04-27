unit uCayanPOSMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Actions,


  Cayan.Common,
  Cayan.Genius.Intf,
  Cayan.MWv4.Intf,
  Cayan.Genius,
  Cayan.XSuperObject,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Gestures, FMX.ActnList,
  FMX.ListBox, FMX.Layouts, FMX.Edit,
  FMX.ListView, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, Cayan, FMX.EditBox, FMX.NumberBox;

type
  TfrmCayanPOSMain = class(TForm)
    MainTabs: TTabControl;
    tabCustomer: TTabItem;
    CustomerTabs: TTabControl;
    tabCustInfo: TTabItem;
    ToolBar1: TToolBar;
    lblTitle1: TLabel;
    btnCustNext: TSpeedButton;
    tabCart: TTabItem;
    ToolBar3: TToolBar;
    lblCartTitle: TLabel;
    tabPayment: TTabItem;
    ToolBar4: TToolBar;
    lblTitle4: TLabel;
    tabResult: TTabItem;
    ToolBar5: TToolBar;
    lblTitle5: TLabel;
    Acts: TActionList;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    lstCustomer: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    txtCustFirstName: TEdit;
    txtCustLastName: TEdit;
    txtCustCompanyName: TEdit;
    ListBoxItem4: TListBoxItem;
    txtCustMainPhone: TEdit;
    Genius: TCayanGenius;
    ListBoxItem5: TListBoxItem;
    txtCustCellPhone: TEdit;
    ListBoxItem6: TListBoxItem;
    txtCustEmail: TEdit;
    lstItems: TListView;
    lstPayments: TListView;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem7: TListBoxItem;
    txtCustBillAddr1: TEdit;
    ListBoxItem8: TListBoxItem;
    txtCustBillAddr2: TEdit;
    ListBoxItem9: TListBoxItem;
    txtCustBillCity: TEdit;
    ListBoxItem10: TListBoxItem;
    txtCustBillState: TEdit;
    ListBoxItem11: TListBoxItem;
    txtCustBillZip: TEdit;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    ListBoxItem12: TListBoxItem;
    txtCustShipAddr1: TEdit;
    ListBoxItem13: TListBoxItem;
    txtCustShipAddr2: TEdit;
    ListBoxItem14: TListBoxItem;
    txtCustShipCity: TEdit;
    ListBoxItem15: TListBoxItem;
    txtCustShipState: TEdit;
    ListBoxItem16: TListBoxItem;
    txtCustShipZip: TEdit;
    GridPanelLayout3: TGridPanelLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblCartQty: TLabel;
    lblCartSubtotal: TLabel;
    lblCartTax: TLabel;
    lblCartTotal: TLabel;
    lstPayDetail: TListBox;
    lhCardInfo: TListBoxGroupHeader;
    liCardNum: TListBoxItem;
    Edit17: TEdit;
    liCardExpiryMonth: TListBoxItem;
    ComboBox1: TComboBox;
    liCardExpiryYear: TListBoxItem;
    ComboBox2: TComboBox;
    liCardHolder: TListBoxItem;
    Edit20: TEdit;
    liCardCVCode: TListBoxItem;
    Edit21: TEdit;
    lhPayInfo: TListBoxGroupHeader;
    liPayAmount: TListBoxItem;
    txtPayAmount: TEdit;
    btnCartNext: TSpeedButton;
    GridPanelLayout2: TGridPanelLayout;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    actCustomerTab: TChangeTabAction;
    actCartTab: TChangeTabAction;
    actPaymentTab: TChangeTabAction;
    actResultTab: TChangeTabAction;
    actCustomerInfoTab: TChangeTabAction;
    btnCartBack: TSpeedButton;
    btnPaymentBack: TSpeedButton;
    GridPanelLayout5: TGridPanelLayout;
    btnCedStart: TButton;
    btnCedCancel: TButton;
    lhCheckInfo: TListBoxGroupHeader;
    liCheckNum: TListBoxItem;
    Edit19: TEdit;
    tabCustLookup: TTabItem;
    ToolBar6: TToolBar;
    Label3: TLabel;
    SpeedButton5: TSpeedButton;
    lstLookupCustomer: TListView;
    pPayType: TGridPanelLayout;
    btnPayGenius: TButton;
    btnPayCash: TButton;
    btnPayCheck: TButton;
    btnPayKeyed: TButton;
    btnPaySwipe: TButton;
    btnPayVault: TButton;
    GridPanelLayout7: TGridPanelLayout;
    btnCartAdd: TButton;
    btnCartEdit: TButton;
    btnCartDelete: TButton;
    pPayButtons: TGridPanelLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    tabLogin: TTabItem;
    actLoginTab: TChangeTabAction;
    ToolBar7: TToolBar;
    Label6: TLabel;
    btnLogin: TSpeedButton;
    lstLogin: TListBox;
    ListBoxGroupHeader5: TListBoxGroupHeader;
    ListBoxItem18: TListBoxItem;
    txtLoginUser: TEdit;
    ListBoxItem21: TListBoxItem;
    txtLoginPassword: TEdit;
    btnCustBack: TSpeedButton;
    btnResultBack: TSpeedButton;
    lstResult: TListBox;
    ListBoxGroupHeader6: TListBoxGroupHeader;
    ListBoxItem17: TListBoxItem;
    ListBoxItem19: TListBoxItem;
    ListBoxItem20: TListBoxItem;
    ListBoxItem22: TListBoxItem;
    ListBoxItem28: TListBoxItem;
    ListBoxItem29: TListBoxItem;
    ListBoxItem30: TListBoxItem;
    ListBoxItem31: TListBoxItem;
    ListBoxItem32: TListBoxItem;
    ListBoxItem33: TListBoxItem;
    ListBoxItem34: TListBoxItem;
    ListBoxItem35: TListBoxItem;
    ListBoxItem36: TListBoxItem;
    ListBoxItem37: TListBoxItem;
    ListBoxItem38: TListBoxItem;
    ListBoxGroupHeader7: TListBoxGroupHeader;
    ListBoxItem39: TListBoxItem;
    ListBoxItem40: TListBoxItem;
    ListBoxItem41: TListBoxItem;
    ListBoxItem42: TListBoxItem;
    ListBoxItem43: TListBoxItem;
    ListBoxGroupHeader8: TListBoxGroupHeader;
    ListBoxGroupHeader9: TListBoxGroupHeader;
    ListBoxGroupHeader10: TListBoxGroupHeader;
    ListBoxGroupHeader11: TListBoxGroupHeader;
    Button4: TButton;
    Button5: TButton;
    MainStyle: TStyleBook;
    Cayan: TCayan;
    actCustomerLookupTab: TChangeTabAction;
    tabSetup: TTabItem;
    actSetupTab: TChangeTabAction;
    ToolBar2: TToolBar;
    Label7: TLabel;
    SpeedButton1: TSpeedButton;
    lstSetup: TListBox;
    ListBoxGroupHeader12: TListBoxGroupHeader;
    ListBoxItem47: TListBoxItem;
    ListBoxItem51: TListBoxItem;
    ListBoxGroupHeader13: TListBoxGroupHeader;
    ListBoxItem53: TListBoxItem;
    ListBoxItem58: TListBoxItem;
    ListBoxItem23: TListBoxItem;
    ListBoxItem24: TListBoxItem;
    ListBoxItem25: TListBoxItem;
    ListBoxItem26: TListBoxItem;
    txtMerchantName: TEdit;
    txtMerchantSiteId: TEdit;
    txtMerchantKey: TEdit;
    txtCedAddress: TEdit;
    txtCedTimeout: TNumberBox;
    txtCedPort: TNumberBox;
    ListBoxGroupHeader14: TListBoxGroupHeader;
    ListBoxItem27: TListBoxItem;
    txtSetupUsername: TEdit;
    ListBoxItem44: TListBoxItem;
    txtSetupPassword: TEdit;
    ListBoxGroupHeader15: TListBoxGroupHeader;
    ListBoxItem45: TListBoxItem;
    swCustRequired: TSwitch;
    ListBoxItem46: TListBoxItem;
    swPhoneRequired: TSwitch;
    ListBoxItem48: TListBoxItem;
    swAddressRequired: TSwitch;
    ListBoxItem49: TListBoxItem;
    swEmailRequired: TSwitch;
    Button6: TButton;
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure GeniusDeviceStatus(Sender: IGenius;
      const Status: IGeniusStatusResponse);
    procedure cboPayMethodClick(Sender: TObject);
    procedure lstNewInvoiceItemClick(Sender: TObject);
    procedure btnCartNextClick(Sender: TObject);
    procedure btnCartBackClick(Sender: TObject);
    procedure btnPaymentBackClick(Sender: TObject);
    procedure btnCustNextClick(Sender: TObject);
    procedure MainTabsChange(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnCustBackClick(Sender: TObject);
    procedure txtLoginUserKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCedStartClick(Sender: TObject);
    procedure btnCedCancelClick(Sender: TObject);
    procedure btnCartAddClick(Sender: TObject);
    procedure TranTransactionStart(const ATrans: TCayanGenius);
    procedure TranTransactionStaged(const ATrans: TCayanGenius);
    procedure TranTransactionResult(const ATrans: TCayanGenius;
      const AResult: IGeniusTransactionResponse);
    procedure TranCancel(Sender: TObject);
    procedure btnResultBackClick(Sender: TObject);
    procedure btnCartDeleteClick(Sender: TObject);
    procedure lstItemsDeleteItem(Sender: TObject; AIndex: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    FUsername: String;
    FPassword: String;
    FTranStarted: Boolean;
    FProducts: TStringList;
    procedure HidePayInfo;
    procedure ShowCardInfo;
    procedure ShowCheckInfo;
    function ConfigFilename: String;
    procedure SetCedBusy(const Value: Boolean);
    procedure DisplayResult(const R: IGeniusTransactionResponse);
    procedure ClearCart;
    procedure PopulateProducts;
    function RandomProduct: String;
  public
    procedure UpdateCartTotals;
    procedure LoadFromConfig;
    procedure SaveToConfig;
  end;

var
  frmCayanPOSMain: TfrmCayanPOSMain;

implementation

{$R *.fmx}

uses
  System.IOUtils;

procedure ResizeListItems(AListBox: TListBox);
var
  X: Integer;
  I: TListBoxItem;
begin
  for X := 0 to AListBox.Count - 1 do begin
    I:= AListBox.ListItems[X];
    if I.Visible then begin
      if I is TListBoxGroupHeader then begin
        I.Height:= 30;
      end else begin
        I.Height:= 42;
      end;
    end else begin
      I.Height:= 0;
    end;
  end;
  AListBox.Repaint;
end;

{ TfrmCayanPOSMain }

procedure TfrmCayanPOSMain.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}

  FProducts:= TStringList.Create;

  lstCustomer.Align:= TAlignLayout.Client;
  lstItems.Align:= TAlignLayout.Client;
  lstPayments.Height:= 90;
  lstPayDetail.Align:= TAlignLayout.Client;
  lstLogin.Align:= TAlignLayout.Client;
  lstResult.Align:= TAlignLayout.Client;
  Self.MainTabs.TabPosition:= TTabPosition.None;
  Self.CustomerTabs.TabPosition:= TTabPosition.None;
  MainTabs.ActiveTab := tabLogin;
  CustomerTabs.ActiveTab:= tabCustInfo;
  Width:= 430;
  Height:= 530;

  LoadFromConfig;

  txtLoginUser.Text:= '';
  txtLoginPassword.Text:= '';
  txtLoginUser.SetFocus;

  Genius.Device.Monitoring:= True;

  Randomize;
  PopulateProducts;
end;

procedure TfrmCayanPOSMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProducts);
end;

procedure TfrmCayanPOSMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.SaveToConfig;
end;

procedure TfrmCayanPOSMain.PopulateProducts;
  procedure A(const S: String);
  begin
    FProducts.Append(S);
  end;
begin
  FProducts.Clear;
  A('Area Rug');
  A('Arm Chair');
  A('Dining Table');
  A('Dining Chairs');
  A('China Cabinet');
  A('Runner Rug');
  A('Small Rug');
  A('Rug Pad');
  A('Area Rug');
  A('Gallery Rug');
end;

function TfrmCayanPOSMain.RandomProduct: String;
var
  I: Integer;
begin
  I:= Random(FProducts.Count);
  Result:= FProducts[I];
end;

procedure TfrmCayanPOSMain.DisplayResult(const R: IGeniusTransactionResponse);
var
  X: Integer;
  I: TListBoxItem;
  H: TListBoxGroupHeader;
  D: IGeniusTransactionDiscount;
  procedure AH(const S: String);
  begin
    H:= TListBoxGroupHeader.Create(lstResult);
    H.Parent:= lstResult;
    H.Text:= S;
  end;
  procedure A(const N, V: String);
  begin
    I:= TListBoxItem.Create(lstResult);
    I.Parent:= lstResult;
    I.StyleLookup:= 'listboxitemrightdetail';
    I.Height:= 42;
    I.Text:= N;
    I.ItemData.Detail:= V;
  end;
begin
  lstResult.Items.Clear;

  AH('General Information');
  A('Status', GeniusTransStatusToStr(R.Status));
  A('Status Detail', R.StatusStr);
  A('Amount Approved', FormatFloat('$#,###,##0', R.AmountApproved));
  A('Authorization Code', R.AuthorizationCode);
  A('CardHolder', R.CardHolder);
  A('Account Number', R.AccountNumber);
  A('Payment Type', GeniusPaymentTypeToStr(R.PaymentType));
  A('Entry Mode', GeniusEntryModeToStr(R.EntryMode));
  A('Error Message', R.ErrorMessage);
  A('Token', R.Token);
  A('Transaction Date', FormatDateTime('m/d/yy h:nn AMPM', R.TransactionDate));
  A('Transaction Type', GeniusTransactionTypeToStr(R.TransactionType));
  A('Response Type', GeniusResponseTypeToStr(R.ResponseType));
  A('Validation Key', R.ValidationKey);
  A('Signature Data', R.SignatureData); //TODO

  AH('Amount Details');
  A('User Tip', FormatFloat('$#,###,##0', R.UserTip));
  A('Cashback', FormatFloat('$#,###,##0', R.Cashback));
  A('Donation', FormatFloat('$#,###,##0', R.Donation));
  A('Surcharge', FormatFloat('$#,###,##0', R.Surcharge));

  AH('Discounts');
  A('Total Discount', FormatFloat('$#,###,##0', R.Discount));
  for X := 0 to R.DiscountsAppliedCount-1 do begin
    D:= R.DiscountsApplied[X];
    A(D.DiscountType, FormatFloat('$#,###,##0', D.Amount));
  end;

  if R.EntryMode = TGeniusEntryMode.geManual then begin
    AH('Keyed Details');
    A('Expiration', R.KeyedExpiration);
    A('Zip Code', R.KeyedAvsStreetZipCode);
    A('Avs Response', R.KeyedAvsResponse);
    A('Cv Response', R.KeyedCvResponse);
  end;

  if Assigned(R.EmvResponse) then begin
    AH('EMV Transaction Info');
    A('Aid', R.EmvResponse.Aid);
    A('Application Label', R.EmvResponse.ApplicationLabel);
    A('PIN Statement', R.EmvResponse.PINStatement);
  end;
end;

procedure TfrmCayanPOSMain.btnBackClick(Sender: TObject);
begin
  actCustomerTab.ExecuteTarget(Self);
end;

procedure TfrmCayanPOSMain.btnCustNextClick(Sender: TObject);
begin
  if (txtCustFirstName.Text = '') and (txtCustLastName.Text = '') and
    (txtCustCompanyName.Text = '') then
  begin
    raise Exception.Create('Please enter customer name.');
  end;

  //TODO: Check if phone required...

  //TODO: Check if address required...

  //TODO: Check if email required...

  lblCartTitle.Text:= 'Cart - ' + txtCustFirstName.Text + ' ' + txtCustLastName.Text;
  ClearCart;
  if Genius.StartNewOrder then begin
    actCartTab.ExecuteTarget(Self);
  end else begin
    raise Exception.Create('Failed to start new order.');
  end;
end;

procedure TfrmCayanPOSMain.SetCedBusy(const Value: Boolean);
begin
  btnCedStart.Visible:= not Value;
  btnCedCancel.Visible:= Value;
  btnPaymentBack.Enabled:= not Value;
  lstPayDetail.Enabled:= not Value;
  pPayButtons.Enabled:= not Value;
  pPayType.Enabled:= not Value;
end;

procedure TfrmCayanPOSMain.btnCedStartClick(Sender: TObject);
begin
  //TODO: Start Transaction...

  SetCedBusy(True);

  Self.FTranStarted:= True;
  Genius.Amount:= StrToFloat(txtPayAmount.Text);
  Genius.Cardholder:= txtCustFirstName.Text + ' ' + txtCustLastName.Text;
  Genius.TransactionID:= Genius.InvoiceNum;


  Genius.StageTransaction;

end;

procedure TfrmCayanPOSMain.btnCartAddClick(Sender: TObject);
var
  LineItems: IGeniusLineItems;
  I: IGeniusLineItem;
  ItemNum: String;
  LI: TListViewItem;
  Price: Currency;
begin
  LineItems:= Genius.Genius.LineItems;
  ItemNum:= IntToStr(LineItems.Count + 1);
  Price:= (Random(200) + 5);
  LineItems.Order:= Genius.InvoiceNum;
  try
    I:= LineItems.AddItem(ItemNum, 'Inventory', '', RandomProduct,
      Price, (Price * 0.06));

    LI:= Self.lstItems.Items.Add;
    LI.Text:= IntToStr(I.Quantity) + ' ' + I.Description;
    LI.Detail:= FormatFloat('$#,###,##0.00', (I.Amount * I.Quantity));
    LI.Tag:= NativeInt(I);

    UpdateCartTotals;
  except
    on E: Exception do begin
      //TODO
    end;
  end;
end;

procedure TfrmCayanPOSMain.ClearCart;
begin
  lstItems.Items.Clear;
  Genius.Genius.LineItems.ClearItems;
  Genius.Cancel;
  Self.UpdateCartTotals;
end;

procedure TfrmCayanPOSMain.btnCartDeleteClick(Sender: TObject);
var
  Items: IGeniusLineItems;
  I: IGeniusLineItem;
  X: Integer;
begin
  if lstItems.ItemIndex >= 0 then begin
    I:= IGeniusLineItem(lstItems.Items[lstItems.ItemIndex].Tag);
    Items:= I.Owner;
    for X := 0 to Items.Count - 1 do begin
      if Items[X] = I then begin
        Items.DeleteItem(X);
        Break;
      end;
    end;
    I.Owner.DeleteItem(X);
    lstItems.Items.Delete(lstItems.ItemIndex);
  end;
end;

procedure TfrmCayanPOSMain.HidePayInfo;
begin
  lhCheckInfo.Visible:= False;
  liCheckNum.Visible:= False;
  lhCardInfo.Visible:= False;
  liCardNum.Visible:= False;
  liCardExpiryMonth.Visible:= False;
  liCardExpiryYear.Visible:= False;
  liCardHolder.Visible:= False;
  liCardCVCode.Visible:= False;

  ResizeListItems(lstPayDetail);
end;

procedure TfrmCayanPOSMain.ShowCardInfo;
begin
  lhCardInfo.Visible:= True;
  liCardNum.Visible:= True;
  liCardExpiryMonth.Visible:= True;
  liCardExpiryYear.Visible:= True;
  liCardHolder.Visible:= True;
  liCardCVCode.Visible:= True;

  ResizeListItems(lstPayDetail);
end;

procedure TfrmCayanPOSMain.ShowCheckInfo;
begin
  lhCheckInfo.Visible:= True;
  liCheckNum.Visible:= True;

  ResizeListItems(lstPayDetail);
end;

procedure TfrmCayanPOSMain.SpeedButton1Click(Sender: TObject);
begin

  //TODO: Save Settings...

  Genius.Device.Monitoring:= False;

  Cayan.MerchantName:= txtMerchantName.Text;
  Cayan.MerchantSiteId:= txtMerchantSiteId.Text;
  Cayan.MerchantKey:= txtMerchantKey.Text;
  Genius.Device.DeviceAddress:= txtCedAddress.Text;
  Genius.Device.DevicePort:= Trunc(txtCedPort.Value);
  Genius.Device.DeviceTimeout:= Trunc(txtCedTimeout.Value);

  Self.SaveToConfig;

  Genius.Device.Monitoring:= True;

  actCustomerTab.ExecuteTarget(Self);

end;

procedure TfrmCayanPOSMain.cboPayMethodClick(Sender: TObject);
var
  B: TButton;
begin
  btnPayGenius.StyleLookup:= 'toolbutton';
  btnPayCash.StyleLookup:= 'toolbutton';
  btnPayCheck.StyleLookup:= 'toolbutton';
  btnPayKeyed.StyleLookup:= 'toolbutton';
  btnPaySwipe.StyleLookup:= 'toolbutton';
  btnPayVault.StyleLookup:= 'toolbutton';

  HidePayInfo;
  B:= TButton(Sender);
  B.StyleLookup:= 'buttonstyle';
  case B.Tag of
    0: begin
      //Genius CED

    end;
    1: begin
      //Cash

    end;
    2: begin
      //Check
      ShowCheckInfo;
    end;
    3: begin
      //Keyed Card
      ShowCardInfo;
    end;
    4: begin
      //Swiped Card

    end;
    5: begin
      //Vault Card

    end;
  end;
  ResizeListItems(Self.lstPayDetail);
end;

procedure TfrmCayanPOSMain.LoadFromConfig;
var
  O: ISuperObject;
begin
  if not FileExists(ConfigFilename) then begin
    Cayan.MerchantName:= 'PSTEST';
    Cayan.MerchantSiteId:= '22222222';
    Cayan.MerchantKey:= '22222-22222-22222-22222-22222';
    Cayan.Dba:= 'My Company Name';
    Cayan.ClerkID:= 'admin';
    Cayan.StationID:= '1';
    Genius.ForceDuplicate:= False;
    Genius.Device.DeviceAddress:= 'LocalHost';
    Genius.Device.DevicePort:= 8989;
    Genius.Device.DeviceProtocol:= prHTTP;
    Genius.Device.DeviceTimeout:= 900;
    Genius.Device.DeviceVersion:= TGeniusDeviceVersion.gdVer1;
    FUsername:= 'admin';
    FPassword:= 'admin';
    SaveToConfig;
  end;
  O:= TSuperObject.ParseFile(ConfigFilename);
  if Assigned(O) then begin
    Cayan.MerchantName:= O.S['merchantName'];
    Cayan.MerchantSiteId:= O.S['merchantSiteId'];
    Cayan.MerchantKey:= O.S['merchantKey'];
    Cayan.Dba:= O.S['dba'];
    Cayan.ClerkID:= O.S['clerkId'];
    Cayan.StationID:= O.S['stationId'];
    Genius.ForceDuplicate:= O.B['forceDuplicates'];
    Genius.Device.DeviceAddress:= O.S['deviceAddress'];
    Genius.Device.DevicePort:= O.I['devicePort'];
    Genius.Device.DeviceProtocol:= TGeniusProtocol.prHTTP; // (O.I['deviceProtocol']);
    Genius.Device.DeviceTimeout:= O.I['deviceTimeout'];
    Genius.Device.DeviceVersion:= TGeniusDeviceVersion(O.I['deviceVersion']);
    FUsername:= O.S['username'];
    FPassword:= O.S['password'];
  end;
end;

procedure TfrmCayanPOSMain.SaveToConfig;
var
  O: ISuperObject;
  L: TStringList;
begin
  O:= SO;
  O.S['merchantName']:= Cayan.MerchantName;
  O.S['merchantSiteId']:= Cayan.MerchantSiteId;
  O.S['merchantKey']:= Cayan.MerchantKey;
  O.B['forceDuplicates']:= Genius.ForceDuplicate;
  O.S['deviceAddress']:= Genius.Device.DeviceAddress;
  O.I['devicePort']:= Genius.Device.DevicePort;
  O.I['deviceProtocol']:= Integer(Genius.Device.DeviceProtocol);
  O.I['deviceTimeout']:= Genius.Device.DeviceTimeout;
  O.I['deviceVersion']:= Integer(Genius.Device.DeviceVersion);
  O.S['dba']:= Cayan.Dba;
  O.S['clerkId']:= Cayan.ClerkID;
  O.S['stationId']:= Cayan.StationID;
  O.S['username']:= FUsername;
  O.S['password']:= FPassword;
  L:= TStringList.Create;
  try
    L.Text:= O.AsJSON(True);
    ForceDirectories(ExtractFilePath(ConfigFilename));
    L.SaveToFile(ConfigFilename);
  finally
    FreeAndNil(L);
  end;
end;

function TfrmCayanPOSMain.ConfigFilename: String;
begin
  Result:= TPath.GetHomePath;
  Result:= TPath.Combine(Result, 'Cayan');
  Result:= TPath.Combine(Result, 'POSConfig.json');
end;

procedure TfrmCayanPOSMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then begin
    if MainTabs.ActiveTab = tabCustomer then begin
      if CustomerTabs.ActiveTab = tabCustInfo then begin
        Key := 0;
        Close; //TODO: ???
      end else
      if CustomerTabs.ActiveTab = tabCustLookup then begin
        actCustomerInfoTab.ExecuteTarget(Self);
        Key := 0;
      end;
    end else
    if MainTabs.ActiveTab = tabCart then begin
      actCustomerTab.ExecuteTarget(Self);
      Key := 0;
    end else
    if MainTabs.ActiveTab = tabPayment then begin
      actCartTab.ExecuteTarget(Self);
      Key := 0;
    end else
    if MainTabs.ActiveTab = tabResult then begin

    end;

    if (MainTabs.ActiveTab = tabCustomer) and (CustomerTabs.ActiveTab = tabCustLookup) then begin
      CustomerTabs.Previous;
      Key := 0;
    end;
  end;
end;

procedure TfrmCayanPOSMain.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft: begin
      if MainTabs.ActiveTab <> MainTabs.Tabs[MainTabs.TabCount - 1] then
        MainTabs.ActiveTab := MainTabs.Tabs[MainTabs.TabIndex + 1];
      Handled := True;
    end;
    sgiRight: begin
      if MainTabs.ActiveTab <> MainTabs.Tabs[0] then
        MainTabs.ActiveTab := MainTabs.Tabs[MainTabs.TabIndex - 1];
      Handled := True;
    end;
  end;
end;

procedure TfrmCayanPOSMain.lstItemsDeleteItem(Sender: TObject; AIndex: Integer);
begin
  //TODO: Remove line item...

end;

procedure TfrmCayanPOSMain.lstNewInvoiceItemClick(Sender: TObject);
begin
  Cursor:= crHandPoint;
  try
    ClearCart;
    lblCartTitle.Text:= 'New Sale';
    Genius.TransactionType:= TGeniusTransactionType.gtSale;
    Genius.InvoiceNum:= '1234'; //TODO
    Genius.TransactionID:= '1234'; //TODO

    actCartTab.ExecuteTarget(Self);

    try
      if Genius.StartNewOrder then begin
        //Successfully started new order...

      end else begin
        //Failed to start new order...
        raise Exception.Create('Failed to start new order.');
      end;
    except
      on E: Exception do begin
        //Exception starting new order...
        raise Exception.Create('Failed to start new invoice: ' + E.Message);
      end;
    end;
  finally
    Cursor:= crDefault;
  end;
end;

procedure TfrmCayanPOSMain.MainTabsChange(Sender: TObject);
begin
  if MainTabs.ActiveTab = tabCustomer then begin

  end else
  if MainTabs.ActiveTab = tabCart then begin

  end else
  if MainTabs.ActiveTab = tabPayment then begin
    Self.HidePayInfo;
    //TODO
  end else
  if MainTabs.ActiveTab = tabResult then begin
  
  end;
end;

procedure TfrmCayanPOSMain.GeniusDeviceStatus(Sender: IGenius;
  const Status: IGeniusStatusResponse);
begin
  //CED status changed
  case Status.Status of
    csOffline: begin
      Caption:= 'Cayan POS - Device Offline';
    end;
    csOnline: begin
      Caption:= 'Cayan POS - Device Online';
    end;
    csDownloadNeeded: begin
      Caption:= 'Cayan POS - Device Download Needed';
    end;
  end;
end;

procedure TfrmCayanPOSMain.UpdateCartTotals;
var
  X: Integer;
  I: IGeniusLineItem;
  Q: Integer;
  S, T: Currency;
begin
  Q:= 0;
  S:= 0;
  T:= 0;
  for X := 0 to Genius.Genius.LineItems.Count-1 do begin
    I:= Genius.Genius.LineItems[X];
    Q:= Q + I.Quantity;
    S:= S + (I.Amount * I.Quantity);
    T:= T + (I.TaxAmount * I.Quantity);
  end;
  lblCartQty.Text:= IntToStr(Q);
  lblCartSubtotal.Text:= FormatFloat('$#,###,##0.00', S);
  lblCartTax.Text:= FormatFloat('$#,###,##0.00', T);
  lblCartTotal.Text:= FormatFloat('$#,###,##0.00', S + T);
end;

procedure TfrmCayanPOSMain.btnCartNextClick(Sender: TObject);
begin
  if Genius.Genius.LineItems.Count = 0 then begin
    raise Exception.Create('There are no items in the cart.');
  end;
  txtPayAmount.Text:= FormatFloat('#,###,##0.00', Genius.Amount);
  actPaymentTab.ExecuteTarget(Self);
end;

procedure TfrmCayanPOSMain.btnCartBackClick(Sender: TObject);
var
  R: IGeniusCancelTransactionResponse;
begin
  Cursor:= crHandPoint;
  try
    actCustomerTab.ExecuteTarget(Self);
    ClearCart;
    R:= Genius.Cancel;
    case R.Status of
      ctCancelled: begin
        //Successfully cancelled transaction...

      end;
      ctApprovedNoSignature: begin
        //
      end;
      ctDenied: begin

      end;
      ctError: begin

      end;
    end;
  finally
    Cursor:= crDefault;
  end;
end;

procedure TfrmCayanPOSMain.btnResultBackClick(Sender: TObject);
begin
  //TODO: Clear customer...

  ClearCart;

  Self.actCustomerTab.ExecuteTarget(Self);

end;

procedure TfrmCayanPOSMain.Button5Click(Sender: TObject);
begin

  //TODO: Load settings...

  txtMerchantName.Text:= Self.Cayan.MerchantName;
  txtMerchantSiteID.Text:= Self.Cayan.MerchantSiteId;
  txtMerchantKey.Text:= Self.Cayan.MerchantKey;
  txtCedAddress.Text:= Self.Genius.Device.DeviceAddress;
  txtCedPort.Value:= Self.Genius.Device.DevicePort;
  txtCedTimeout.Value:= Self.Genius.Device.DeviceTimeout;


  actSetupTab.ExecuteTarget(Self);

end;

procedure TfrmCayanPOSMain.btnPaymentBackClick(Sender: TObject);
begin
  //TODO: Check if any payments have already been collected.
  //If so, prompt user if they wish to void payments.

  actCartTab.ExecuteTarget(Self);
  if FTranStarted then begin
    FTranStarted:= False;
    Genius.Cancel;
  end;

end;

procedure TfrmCayanPOSMain.btnCedCancelClick(Sender: TObject);
begin
  //TODO: Cancel transaction...
  FTranStarted:= False;
  Genius.Cancel;

end;

procedure TfrmCayanPOSMain.btnLoginClick(Sender: TObject);
begin
  //TODO: Login...

  if SameText(FUsername, txtLoginUser.Text) then begin
    if FPassword = txtLoginPassword.Text then begin
      Self.Cayan.ClerkID:= txtLoginUser.Text;
      Self.actCustomerTab.ExecuteTarget(Self);
    end else begin
      MessageDlg('Login failed!', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    end;
  end else begin
    MessageDlg('Login failed!', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;

end;

procedure TfrmCayanPOSMain.btnCustBackClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you wish to log out?', TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
  begin
    txtLoginUser.Text:= '';
    txtLoginPassword.Text:= '';
    actLoginTab.ExecuteTarget(Self);
    txtLoginUser.SetFocus;
  end else begin

  end;
end;

procedure TfrmCayanPOSMain.TranCancel(Sender: TObject);
begin
  if Self.MainTabs.ActiveTab = Self.tabPayment then begin
    Self.SetCedBusy(False);
  end else
  if MainTabs.ActiveTab = Self.tabCart then begin
    btnCartBackClick(btnCartBack);
  end else begin
    //Cancelled at unexpected place...
  end;
end;

procedure TfrmCayanPOSMain.TranTransactionResult(
  const ATrans: TCayanGenius;
  const AResult: IGeniusTransactionResponse);
begin
  Self.SetCedBusy(False);

  DisplayResult(AResult);

  case AResult.Status of
    gsApproved: begin
      //Transaction was approved...
      //ShowMessage('Success!');
      Self.actResultTab.ExecuteTarget(Self);
    end;
    gsDeclined: begin
      //Transaction was declined...
      ShowMessage('Declined!');
    end;
    gsError: begin
      //An error occurred...
      ShowMessage('Error: ' + AResult.ErrorMessage);
    end;
    gsUserCancelled: begin
      //User (customer) cancelled transaction...
      ShowMessage('User Cancelled!');
    end;
    gsPosCancelled: begin
      //User (pos) cancelled transaction...
      //ShowMessage('POS Cancelled!');
    end;
    gsDuplicate: begin
      //Duplicate transaction detected...
      ShowMessage('Duplicate!');
    end;
  end;
end;

procedure TfrmCayanPOSMain.TranTransactionStaged(
  const ATrans: TCayanGenius);
begin
  Self.SetCedBusy(True);
end;

procedure TfrmCayanPOSMain.TranTransactionStart(
  const ATrans: TCayanGenius);
begin
  Self.SetCedBusy(True);
end;

procedure TfrmCayanPOSMain.txtLoginUserKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then begin
    btnLoginClick(btnLogin);
  end;
end;

end.

