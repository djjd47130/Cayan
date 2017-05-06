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

  FMX.DialogService,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Gestures, FMX.ActnList,
  FMX.ListBox, FMX.Layouts, FMX.Edit,
  FMX.ListView, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, Cayan, FMX.EditBox, FMX.NumberBox,
  Cayan.Genius.LineItems, Cayan.POS, Cayan.Genius.Transactions, FMX.ScrollBox,
  FMX.Memo,
  uCayanPOSCart;

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
    tabPayment: TTabItem;
    ToolBar4: TToolBar;
    lblPaymentTitle: TLabel;
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
    lstPayDetail: TListBox;
    liCardNum: TListBoxItem;
    txtKeyCardNum: TEdit;
    liCardExpiryMonth: TListBoxItem;
    txtKeyExpiryMonth: TComboBox;
    liCardExpiryYear: TListBoxItem;
    txtKeyExpiryYear: TComboBox;
    liCardHolder: TListBoxItem;
    txtKeyCardholder: TEdit;
    liCardCVCode: TListBoxItem;
    txtKeySecCode: TEdit;
    lhPayInfo: TListBoxGroupHeader;
    liPayAmount: TListBoxItem;
    txtPayAmount: TEdit;
    GridPanelLayout2: TGridPanelLayout;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    actCustomerTab: TChangeTabAction;
    actCartTab: TChangeTabAction;
    actPaymentTab: TChangeTabAction;
    actResultTab: TChangeTabAction;
    actCustomerInfoTab: TChangeTabAction;
    btnPaymentBack: TSpeedButton;
    GridPanelLayout5: TGridPanelLayout;
    btnCedStart: TButton;
    btnCedCancel: TButton;
    liCheckNum: TListBoxItem;
    txtPayCheckNum: TEdit;
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
    actCustomerLookupTab: TChangeTabAction;
    tabSetup: TTabItem;
    actSetupTab: TChangeTabAction;
    ToolBar2: TToolBar;
    Label7: TLabel;
    SpeedButton1: TSpeedButton;
    lstSetup: TListBox;
    ListBoxGroupHeader13: TListBoxGroupHeader;
    ListBoxItem53: TListBoxItem;
    ListBoxItem23: TListBoxItem;
    ListBoxItem25: TListBoxItem;
    txtCedAddress: TEdit;
    txtCedTimeout: TNumberBox;
    txtCedPort: TNumberBox;
    Button6: TButton;
    ListBoxGroupHeader16: TListBoxGroupHeader;
    lManageServer: TListBoxItem;
    lServerHost: TListBoxItem;
    txtServerHost: TEdit;
    lServerPort: TListBoxItem;
    lServerKey: TListBoxItem;
    txtServerKey: TEdit;
    lServerStation: TListBoxItem;
    txtServerPort: TNumberBox;
    txtServerStation: TNumberBox;
    Tran: TCayanGeniusTransaction;
    btnCedKeyed: TButton;
    lblTerminalStatus: TLabel;
    liSaveToVault: TListBoxItem;
    swSaveToVault: TSwitch;
    txtSaveVaultName: TEdit;
    liVaultCards: TListBoxItem;
    cboVaultCards: TComboBox;
    liCustID: TListBoxItem;
    txtCustID: TEdit;
    tabMenu: TTabItem;
    lstMenu: TListBox;
    ListBoxGroupHeader14: TListBoxGroupHeader;
    ListBoxItem26: TListBoxItem;
    ListBoxItem27: TListBoxItem;
    ToolBar8: TToolBar;
    Label8: TLabel;
    ListBoxItem44: TListBoxItem;
    liCardAddress: TListBoxItem;
    txtKeyAddress: TEdit;
    liCardZipCode: TListBoxItem;
    txtKeyZipCode: TEdit;
    liTranType: TListBoxItem;
    liSwipe: TListBoxItem;
    txtSwipe: TMemo;
    ListBoxItem45: TListBoxItem;
    tabGift: TTabItem;
    Panel1: TPanel;
    Panel2: TPanel;
    ListBoxItem24: TListBoxItem;
    ListBoxItem46: TListBoxItem;
    TabControl1: TTabControl;
    TabItem2: TTabItem;
    ToolBar3: TToolBar;
    Label1: TLabel;
    SpeedButton3: TSpeedButton;
    ListBox1: TListBox;
    ListBoxGroupHeader12: TListBoxGroupHeader;
    ListBoxItem47: TListBoxItem;
    Edit1: TEdit;
    ListBoxItem48: TListBoxItem;
    Edit2: TEdit;
    ListBoxGroupHeader17: TListBoxGroupHeader;
    ListBoxItem55: TListBoxItem;
    GridPanelLayout1: TGridPanelLayout;
    Button11: TButton;
    Button12: TButton;
    TabItem3: TTabItem;
    ToolBar9: TToolBar;
    Label2: TLabel;
    SpeedButton4: TSpeedButton;
    TabItem4: TTabItem;
    actMenuTab: TChangeTabAction;
    actGiftTab: TChangeTabAction;
    ToolBar10: TToolBar;
    Label4: TLabel;
    SpeedButton2: TSpeedButton;
    NumberBox1: TNumberBox;
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
    procedure TranTransactionStart(const ATrans: TCayanGeniusTransaction);
    procedure TranTransactionStaged(const ATrans: TCayanGeniusTransaction);
    procedure TranTransactionResult(const ATrans: TCayanGeniusTransaction;
      const AStaging: IGeniusStageResponse;
      const AResult: IGeniusTransactionResponse);
    procedure TranCancel(Sender: TObject);
    procedure btnResultBackClick(Sender: TObject);
    procedure lstItemsDeleteItem(Sender: TObject; AIndex: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure LIDChange(Sender: TCayanGeniusLineItems);
    procedure lManageServerClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure lstLookupCustomerItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure btnCedKeyedClick(Sender: TObject);
    procedure swSaveToVaultSwitch(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FTranStarted: Boolean;
    FProducts: TStringList;
    FCustomers: ICayanPOSCustomers;
    FCustomer: ICayanPOSCustomer;
    FCards: ICayanPOSCards;
    FSetup: ICayanPOSSetup;

    FCart: TfrmCart;
    procedure HidePayInfo;
    procedure ShowCardInfo;
    procedure ShowCheckInfo;
    function ConfigFilename: String;
    procedure SetCedBusy(const Value: Boolean);
    procedure DisplayResultGenius(const R: IGeniusTransactionResponse);
    procedure DisplayResultMWCredit(const R: IMWCreditResponse4);
    procedure ClearCart;
    procedure PopulateProducts;
    function RandomProduct: String;
    procedure LoadCustomers;
    procedure ClearCustomers;
    function CustomerByID(const ID: Integer): ICayanPOSCustomer;
    procedure ClearCustomer;
    procedure ShowVaultInfo;
    procedure ProcessPaymentKeyed;
    function KeyedExpiry: TExpirationDate;
    procedure ProcessPaymentVault;
    procedure ProcessPaymentGenius;
    function VaultSelectedToken: String;
    procedure SaveToVault(const Token: String);
    procedure ProcessPaymentSwiped;
    procedure FinishCreditSale(Res: IMWCreditResponse4);
    procedure ClearPaymentInfo;
    procedure CreateSubForms;
  public
    procedure UpdateCartTotals;
    procedure LoadFromConfig;
    procedure SaveToConfig;
    property Setup: ICayanPOSSetup read FSetup;
  end;

var
  frmCayanPOSMain: TfrmCayanPOSMain;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  uDM;

{ TfrmCayanPOSMain }

procedure TfrmCayanPOSMain.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}

  CreateSubForms;

  MainTabs.TabPosition:= TTabPosition.None;
  CustomerTabs.TabPosition:= TTabPosition.None;
  MainTabs.ActiveTab := tabLogin;
  CustomerTabs.ActiveTab:= tabCustInfo;

  //TODO: IMPLEMENT SPLIT PAYMENTS!
  {$IFNDEF SPLIT_PAY}
  pPayButtons.Visible:= False;
  lstPayments.Visible:= False;
  {$ELSE}
  lstPayments.Height:= 90;
  {$ENDIF}

  Width:= 470;
  Height:= 570;

  LoadFromConfig;

  txtLoginUser.Text:= '';
  txtLoginPassword.Text:= '';
  txtLoginUser.SetFocus;

  //TODO: Implement real inventory. This is a fake demo.
  {$IFNDEF INVENT}
  Randomize;
  PopulateProducts;
  {$ENDIF}
end;

procedure TfrmCayanPOSMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCart);
  FreeAndNil(FProducts);
  FCustomers:= nil;
  FCustomer:= nil;
  FCards:= nil;
end;

procedure TfrmCayanPOSMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Genius.Device.Monitoring:= False;
  Genius.Cancel;
  Genius.Cancel;
  Self.SaveToConfig;
end;

procedure TfrmCayanPOSMain.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  R: Boolean;
begin
  R:= False;
  CanClose:= False;
  TDialogService.PreferredMode:= TDialogService.TPreferredMode.Platform;
  TDialogService.MessageDialog('Are you sure you wish to exit?',
    TMsgDlgType.mtConfirmation,
    FMX.Dialogs.mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then begin
        R:= True;
      end;
    end);
  CanClose:= R;
end;

procedure TfrmCayanPOSMain.CreateSubForms;
begin
  FProducts:= TStringList.Create;
  FCart:= TfrmCart.Create(tabCart);
  FCart.btnBack.OnClick:= Self.btnCartBackClick;
  FCart.btnNext.OnClick:= Self.btnCartNextClick;
  FCart.LID.Transaction:= Self.Tran;
  FCart.Setup:= FSetup;
end;

procedure TfrmCayanPOSMain.LoadFromConfig;
var
  O: ISuperObject;
begin
  if not FileExists(ConfigFilename) then begin
    DM.Cayan.StationID:= '1';
    Genius.Device.DeviceAddress:= 'LocalHost';
    Genius.Device.DevicePort:= 8989;
    Genius.Device.DeviceProtocol:= prHTTP;
    Genius.Device.DeviceTimeout:= 900;
    Genius.Device.DeviceVersion:= TGeniusDeviceVersion.gdVer1;
    Self.txtServerHost.Text:= 'LocalHost';
    Self.txtServerPort.Value:= 8787;
    Self.txtServerKey.Text:= '';
    //Self.txtServerStation.Text:= Self.Cayan.StationID;
    SaveToConfig;
  end;
  O:= TSuperObject.ParseFile(ConfigFilename);
  if Assigned(O) then begin
    DM.Cayan.Dba:= O.S['dba'];
    DM.Cayan.StationID:= O.S['stationId'];
    Genius.Device.DeviceAddress:= O.S['deviceAddress'];
    Genius.Device.DevicePort:= O.I['devicePort'];
    Genius.Device.DeviceProtocol:= TGeniusProtocol.prHTTP; // (O.I['deviceProtocol']);
    Genius.Device.DeviceTimeout:= O.I['deviceTimeout'];
    Genius.Device.DeviceVersion:= TGeniusDeviceVersion(O.I['deviceVersion']);
    Self.txtServerHost.Text:= O.S['serverAddr'];
    Self.txtServerPort.Value:= O.I['serverPort'];;
    Self.txtServerKey.Text:= O.S['serverKey'];
    DM.POS.Host:= O.S['serverAddr'];
    DM.POS.Port:= O.I['serverPort'];
    //Self.txtServerStation.Text:= Self.Cayan.StationID;
  end;
end;

procedure TfrmCayanPOSMain.SaveToConfig;
var
  O: ISuperObject;
  L: TStringList;
begin
  O:= SO;
  O.S['deviceAddress']:= Genius.Device.DeviceAddress;
  O.I['devicePort']:= Genius.Device.DevicePort;
  O.I['deviceProtocol']:= Integer(Genius.Device.DeviceProtocol);
  O.I['deviceTimeout']:= Genius.Device.DeviceTimeout;
  O.I['deviceVersion']:= Integer(Genius.Device.DeviceVersion);
  O.S['stationId']:= DM.Cayan.StationID;
  O.S['serverAddr']:= Self.txtServerHost.Text;
  O.I['serverPort']:= Trunc(Self.txtServerPort.Value);
  O.S['serverKey']:= Self.txtServerKey.Text;
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

procedure TfrmCayanPOSMain.DisplayResultGenius(const R: IGeniusTransactionResponse);
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
  A('Transaction Type', GeniusTransactionTypeToStr(R.TransactionType));
  if R.TransactionDate <> 0 then
    A('Transaction Date', FormatDateTime('m/d/yy h:nn AMPM', R.TransactionDate));
  A('Status', GeniusTransStatusToStr(R.Status));
  if R.ErrorMessage <> '' then
    A('Error Message', R.ErrorMessage);
  A('Amount Approved', FormatFloat('$#,###,##0', R.AmountApproved));
  if R.AuthorizationCode <> '' then
    A('Authorization Code', R.AuthorizationCode);
  if R.CardHolder <> '' then
    A('CardHolder', R.CardHolder);
  if R.AccountNumber <> '' then
    A('Account Number', R.AccountNumber);
  A('Payment Type', GeniusPaymentTypeToStr(R.PaymentType));
  A('Entry Mode', GeniusEntryModeToStr(R.EntryMode));

  AH('Additional Information');
  if R.Token <> '' then
    A('Token', R.Token);
  A('Response Type', GeniusResponseTypeToStr(R.ResponseType));
  A('Validation Key', R.ValidationKey);
  if R.SignatureData <> '' then
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
    //A('Avs Response', R.KeyedAvsResponse);
    //A('Cv Response', R.KeyedCvResponse);
  end;

  if Assigned(R.EmvResponse) then begin
    AH('EMV Transaction Info');
    A('Aid', R.EmvResponse.Aid);
    A('Application Label', R.EmvResponse.ApplicationLabel);
    A('PIN Statement', R.EmvResponse.PINStatement);
  end;
end;

procedure TfrmCayanPOSMain.DisplayResultMWCredit(const R: IMWCreditResponse4);
var
  I: TListBoxItem;
  H: TListBoxGroupHeader;
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
  A('Transaction Type', MWTransactionTypeCaption(R.TransactionType));
  if R.TransactionDate <> 0 then
    A('Transaction Date', FormatDateTime('m/d/yy h:nn ampm', R.TransactionDate));
  A('Status', MWApprovalStatusSetToStr(R.ApprovalStatus));
  if R.ErrorMessage <> '' then
    A('Error Message', R.ErrorMessage);
  A('Amount Approved', FormatFloat('$#,###,##0.00', R.Amount));
  if R.AuthorizationCode <> '' then
    A('Authorization Code', R.AuthorizationCode);
  if R.Cardholder <> '' then
    A('Cardholder', R.Cardholder);
  if R.CardNumber <> '' then
    A('Account Number', R.CardNumber);
  A('Payment Type', MWCardTypeCaption(R.CardType));
  A('Entry Mode', MWPosEntryTypeCaption(R.EntryMode));

  AH('Additional Information');
  if R.Token <> '' then
    A('Token', R.Token);
  if R.ApprovalMessage <> '' then
    A('Approval Msg', R.ApprovalMessage);
  if R.InvoiceNumber <> '' then
    A('Invoice Num', R.InvoiceNumber);
  if R.ApprovalCode <> 0 then
    A('Approval Code', IntToStr(R.ApprovalCode));
  if R.ExtraData <> '' then
    A('Extra Data', R.ExtraData);
end;

procedure TfrmCayanPOSMain.btnBackClick(Sender: TObject);
begin
  actCustomerTab.ExecuteTarget(Self);
end;

procedure TfrmCayanPOSMain.btnCustNextClick(Sender: TObject);
begin
  //TODO: Check if customer is required...
  if (txtCustFirstName.Text = '') and (txtCustLastName.Text = '') and
    (txtCustCompanyName.Text = '') then
  begin
    raise Exception.Create('Please enter customer name.');
  end;

  //TODO: Check if phone required...

  //TODO: Check if address required...

  //TODO: Check if email required...

  FCart.lblTitle.Text:= 'Cart - ' + txtCustFirstName.Text + ' ' + txtCustLastName.Text;
  ClearCart;
  FCart.LID.DisplayCustomSubTotal:= txtCustFirstName.Text + ' ' + txtCustLastName.Text;
  actCartTab.ExecuteTarget(Self);
  FCart.LID.StartOrder;
end;

procedure TfrmCayanPOSMain.SetCedBusy(const Value: Boolean);
begin
  //TODO: Dim window...
  btnCedStart.Visible:= not Value;
  btnCedCancel.Visible:= Value;
  if not Value then //TODO: Why did I do this again?
    btnCedKeyed.Visible:= False;
  btnPaymentBack.Enabled:= not Value;
  lstPayDetail.Enabled:= not Value;
  pPayButtons.Enabled:= not Value;
  pPayType.Enabled:= not Value;
end;

procedure TfrmCayanPOSMain.TranCancel(Sender: TObject);
begin
  if Self.MainTabs.ActiveTab = Self.tabPayment then begin
    Self.SetCedBusy(False);
  end else
  if MainTabs.ActiveTab = Self.tabCart then begin
    btnCartBackClick(nil);
  end else begin
    //Cancelled at unexpected place...
  end;
end;

procedure TfrmCayanPOSMain.TranTransactionResult(
  const ATrans: TCayanGeniusTransaction;
  const AStaging: IGeniusStageResponse;
  const AResult: IGeniusTransactionResponse);
begin
  SetCedBusy(False);
  FTranStarted:= False;
  DisplayResultGenius(AResult);
  case AResult.Status of
    gsApproved: begin
      actResultTab.ExecuteTarget(Self);
      SaveToVault(AResult.Token);
    end;
    gsDeclined: begin
      ShowMessage('Declined!');
    end;
    gsError: begin
      ShowMessage('Error: ' + AResult.ErrorMessage);
    end;
    gsUserCancelled: begin
      ShowMessage('User Cancelled!');
    end;
    gsPosCancelled: begin
    end;
    gsDuplicate: begin
      ShowMessage('Duplicate!');
    end;
  end;
end;

procedure TfrmCayanPOSMain.TranTransactionStaged(
  const ATrans: TCayanGeniusTransaction);
begin
  Self.SetCedBusy(True);
end;

procedure TfrmCayanPOSMain.TranTransactionStart(
  const ATrans: TCayanGeniusTransaction);
begin
  Self.SetCedBusy(True);
end;

function TfrmCayanPOSMain.KeyedExpiry: TExpirationDate;
var
  Y: String;
begin
  Result.Month:= txtKeyExpiryMonth.ItemIndex + 1;
  Y:= txtKeyExpiryYear.Items[txtKeyExpiryYear.ItemIndex];
  Y:= Copy(Y, 3, 2);
  Result.Year:= StrToIntDef(Y, 0);
end;

procedure TfrmCayanPOSMain.SaveToVault(const Token: String);
var
  V: IMWVaultBoardingResponse;
  VI: IMWVaultPaymentInfoResponse;
begin
  if swSaveToVault.IsChecked and liSaveToVault.Visible then begin
    V:= Genius.MerchantWare.Credit.VaultBoardCreditByReference(Token);
    if V.ErrorMessage <> '' then begin
      ShowMessage('Error saving card info: ' + V.ErrorMessage);
    end else begin
      VI:= Genius.MerchantWare.Credit.VaultFindPaymentInfo(V.VaultToken);
      if VI.ErrorMessage <> '' then begin
        ShowMessage('Error obtaining payment info: ' + VI.ErrorMessage);
      end else begin
        //TODO: Submit to POS web server...

        ShowMessage('Card info successfully saved: ' + V.VaultToken);
      end;
    end;
  end;
end;

procedure TfrmCayanPOSMain.FinishCreditSale(Res: IMWCreditResponse4);
begin
  if asApproved in Res.ApprovalStatus then begin
    if FCart.LID.InOrder then begin
      FCart.LID.EndOrder(epOther);
    end;
    DisplayResultMWCredit(Res);
    SaveToVault(Res.Token);
    actResultTab.ExecuteTarget(Self);
  end else begin
    ShowMessage('Declined: ' + MWApprovalStatusSetToStr(Res.ApprovalStatus) + ' - ' + Res.ErrorMessage);
  end;
end;

procedure TfrmCayanPOSMain.ProcessPaymentSwiped;
var
  Res: IMWCreditResponse4;
begin
  Res:= Genius.MerchantWare.Credit.Sale(Tran.InvoiceNum, Tran.Amount,
    txtSwipe.Text, Genius.ForceDuplicate, DM.Cayan.StationID,
    Tran.TransactionID, TMWEntryMode.emMagneticStripe);
  FinishCreditSale(Res);
end;

procedure TfrmCayanPOSMain.ProcessPaymentKeyed;
var
  Res: IMWCreditResponse4;
begin
  Res:= Genius.MerchantWare.Credit.SaleKeyed(Tran.InvoiceNum, Tran.Amount,
    txtKeyCardNum.Text, KeyedExpiry, txtKeyCardholder.Text,
    txtKeyAddress.Text, txtKeyZipCode.Text, txtKeySecCode.Text,
    Genius.ForceDuplicate, DM.Cayan.StationID, Tran.TransactionID);
  FinishCreditSale(Res);
end;

procedure TfrmCayanPOSMain.ProcessPaymentVault;
var
  Res: IMWCreditResponse4;
begin
  Res:= Genius.MerchantWare.Credit.SaleVault(Tran.InvoiceNum, Tran.Amount,
    VaultSelectedToken, False, DM.Cayan.StationID, Tran.TransactionID);
  FinishCreditSale(Res);
end;

procedure TfrmCayanPOSMain.ProcessPaymentGenius;
begin
  SetCedBusy(True);
  btnCedKeyed.Visible:= True;
  FTranStarted:= True;
  Tran.Amount:= StrToFloat(txtPayAmount.Text);
  Tran.Cardholder:= txtCustFirstName.Text + ' ' + txtCustLastName.Text;
  Tran.TransactionID:= Tran.InvoiceNum;
  Tran.StartTransaction;
end;

function TfrmCayanPOSMain.VaultSelectedToken: String;
var
  ID: Integer;
  X: Integer;
begin
  Result:= '';
  if cboVaultCards.ItemIndex >= 0 then begin
    ID:= Integer(cboVaultCards.Items.Objects[cboVaultCards.ItemIndex]);
    for X := 0 to FCards.Count-1 do begin
      if FCards[X].ID = ID then begin
        Result:= FCards[X].Token;
        Break;
      end;
    end;
  end;
end;

procedure TfrmCayanPOSMain.btnCedStartClick(Sender: TObject);
begin
  Genius.PrepareGenius;
  case btnCedStart.Tag of
    0: begin
      ProcessPaymentGenius;
    end;
    1: begin
      //Cash
      if FCart.LID.InOrder then begin
        FCart.LID.EndOrder(epCash);
      end;
    end;
    2: begin
      //Check
      if FCart.LID.InOrder then begin
        FCart.LID.EndOrder(epCheck);
      end;
    end;
    3: begin
      ProcessPaymentKeyed;
    end;
    4: begin
      ProcessPaymentSwiped;
    end;
    5: begin
      ProcessPaymentVault;
    end;
  end;
end;

procedure TfrmCayanPOSMain.ClearCart;
begin
  FCart.Clear;
end;

procedure TfrmCayanPOSMain.HidePayInfo;
begin
  ClearPaymentInfo;
  liCheckNum.Visible:= False;
  liCardNum.Visible:= False;
  liCardExpiryMonth.Visible:= False;
  liCardExpiryYear.Visible:= False;
  liCardHolder.Visible:= False;
  liCardCVCode.Visible:= False;
  liSaveToVault.Visible:= False;
  liVaultCards.Visible:= False;
  liSaveToVault.Visible:= False;
  liCardAddress.Visible:= False;
  liCardZipCode.Visible:= False;
  liSwipe.Visible:= False;
end;

procedure TfrmCayanPOSMain.ShowCardInfo;
begin
  liCardNum.Visible:= True;
  liCardExpiryMonth.Visible:= True;
  liCardExpiryYear.Visible:= True;
  liCardHolder.Visible:= True;
  liCardCVCode.Visible:= True;
  liSaveToVault.Visible:= True;
  liCardAddress.Visible:= True;
  liCardZipCode.Visible:= True;
end;

procedure TfrmCayanPOSMain.ShowCheckInfo;
begin
  liCheckNum.Visible:= True;
end;

procedure TfrmCayanPOSMain.ShowVaultInfo;
var
  X: Integer;
  C: ICayanPOSCard;
begin
  liVaultCards.Visible:= True;
  cboVaultCards.Items.Clear;
  if Assigned(FCustomer) then begin
    FCards:= DM.POS.GetVaultCards(FCustomer.ID);
    for X := 0 to FCards.Count-1 do begin
      C:= FCards[X];
      cboVaultCards.Items.AddObject(C.Caption, Pointer(C.ID));
    end;
  end else begin
    FCards:= nil;
  end;
end;

procedure TfrmCayanPOSMain.SpeedButton1Click(Sender: TObject);
begin
  Genius.Device.Monitoring:= False;
  Genius.Device.DeviceAddress:= txtCedAddress.Text;
  Genius.Device.DevicePort:= Trunc(txtCedPort.Value);
  Genius.Device.DeviceTimeout:= Trunc(txtCedTimeout.Value);
  Self.SaveToConfig;
  Genius.Device.Monitoring:= True;
  actCustomerTab.ExecuteTarget(Self);
end;

procedure TfrmCayanPOSMain.swSaveToVaultSwitch(Sender: TObject);
begin
  txtSaveVaultName.Visible:= swSaveToVault.IsChecked;
  txtSaveVaultName.Text:= '';
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
  btnCedStart.Tag:= B.Tag;
  case B.Tag of
    0: begin
      //Genius CED
      liSaveToVault.Visible:= True;
      btnCedStart.Text:= 'Start Transaction';
    end;
    1: begin
      //Cash
      btnCedStart.Text:= 'Save Transaction';
    end;
    2: begin
      //Check
      ShowCheckInfo;
      btnCedStart.Text:= 'Save Transaction';
    end;
    3: begin
      //Keyed Card
      ShowCardInfo;
      btnCedStart.Text:= 'Start Transaction';
    end;
    4: begin
      //Swiped Card
      liSaveToVault.Visible:= True;
      liSwipe.Visible:= True;
      liSwipe.Height:= 100;
      btnCedStart.Text:= 'Start Transaction';
    end;
    5: begin
      //Vault Card
      ShowVaultInfo;
      btnCedStart.Text:= 'Start Transaction';
    end;
  end;
end;

procedure TfrmCayanPOSMain.LIDChange(Sender: TCayanGeniusLineItems);
begin
  Self.UpdateCartTotals;
end;

procedure TfrmCayanPOSMain.lManageServerClick(Sender: TObject);
begin
  case lManageServer.Tag of
    0: begin
      lManageServer.Tag:= 1;
      lManageServer.Text:= 'Click to Collapse...';
      lServerHost.Visible:= True;
      lServerPort.Visible:= True;
      lServerKey.Visible:= True;
      lServerStation.Visible:= True;
    end;
    1: begin
      lManageServer.Tag:= 0;
      lManageServer.Text:= 'Click to Manage...';
      lServerHost.Visible:= False;
      lServerPort.Visible:= False;
      lServerKey.Visible:= False;
      lServerStation.Visible:= False;
    end;
  end;

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

procedure TfrmCayanPOSMain.lstLookupCustomerItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  C: ICayanPOSCustomer;
begin
  C:= CustomerByID(AItem.Tag);
  FCustomer:= C;
  Self.ClearCustomer;
  Self.actCustomerInfoTab.ExecuteTarget(Self);
  txtCustID.Text:= IntToStr(C.ID);
  txtCustID.Enabled:= False;
  Self.txtCustFirstName.Text:= C.FirstName;
  Self.txtCustLastName.Text:= C.LastName;
  Self.txtCustCompanyName.Text:= C.CompanyName;
  Self.txtCustMainPhone.Text:= C.MainPhone;
  Self.txtCustCellPhone.Text:= C.CellPhone;
  Self.txtCustEmail.Text:= C.Email;
  Self.txtCustBillAddr1.Text:= C.BillAddr1;
  txtCustBillAddr2.Text:= C.BillAddr2;
  txtCustBillCity.Text:= C.BillCity;
  txtCustBillState.Text:= C.BillState;
  txtCustBillZip.Text:= C.BillZip;
  txtCustShipAddr1.Text:= C.ShipAddr1;
  txtCustShipAddr2.Text:= C.ShipAddr2;
  txtCustShipCity.Text:= C.ShipCity;
  txtCustShipState.Text:= C.ShipState;
  txtCustShipZip.Text:= C.ShipZip;
end;

procedure TfrmCayanPOSMain.ClearCustomer;
begin
  txtCustID.Text:= '';
  txtCustID.Enabled:= True;
  Self.txtCustFirstName.Text:= '';
  Self.txtCustLastName.Text:= '';
  Self.txtCustCompanyName.Text:= '';
  Self.txtCustMainPhone.Text:= '';
  Self.txtCustCellPhone.Text:= '';
  Self.txtCustEmail.Text:= '';
  Self.txtCustBillAddr1.Text:= '';
  txtCustBillAddr2.Text:= '';
  txtCustBillCity.Text:= '';
  txtCustBillState.Text:= '';
  txtCustBillZip.Text:= '';
  txtCustShipAddr1.Text:= '';
  txtCustShipAddr2.Text:= '';
  txtCustShipCity.Text:= '';
  txtCustShipState.Text:= '';
  txtCustShipZip.Text:= '';
end;

procedure TfrmCayanPOSMain.lstNewInvoiceItemClick(Sender: TObject);
begin
  Cursor:= crHandPoint;
  try
    ClearCart;
    FCart.lblTitle.Text:= 'New Sale';
    Tran.TransactionType:= TGeniusTransactionType.gtSale;
    Tran.InvoiceNum:= '1234'; //TODO: Generate real invoice number
    Tran.TransactionID:= '1234'; //TODO: Generate real payment number

    actCartTab.ExecuteTarget(Self);

    try
      FCart.LID.StartOrder;
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
    //TODO: Clear payment info
  end else
  if MainTabs.ActiveTab = tabResult then begin
  
  end;
end;

procedure TfrmCayanPOSMain.GeniusDeviceStatus(Sender: IGenius;
  const Status: IGeniusStatusResponse);
var
  C: TAlphaColor;
begin
  //CED status changed
  C:= $FFDB5B5B; //Red
  if Genius.Device.Monitoring then begin
    case Status.Status of
      csOffline: begin
        lblTerminalStatus.Text:= 'Device Offline';
      end;
      csOnline: begin
        lblTerminalStatus.Text:= 'Device: ' + GeniusCedScreenToStr(Status.CurrentScreen);
        if Status.CurrentScreen in [csIdle] then begin
          C:= $FF50B7D8; //Blue
        end;
      end;
      csDownloadNeeded: begin
        lblTerminalStatus.Text:= 'Device Download Needed';
      end;
    end;
  end;
  Self.lblTerminalStatus.TextSettings.FontColor:= C;
end;

procedure TfrmCayanPOSMain.UpdateCartTotals;
begin
  if Application.Terminated then Exit;
  FCart.UpdateTotals;
end;

procedure TfrmCayanPOSMain.btnCartNextClick(Sender: TObject);
var
  S: String;
begin
  if FCart.LID.Count = 0 then begin
    raise Exception.Create('There are no items in the cart.');
  end;
  ClearPaymentInfo;
  DM.Cayan.Dba:= FSetup.Dba;
  Genius.ForceDuplicate:= FSetup.ForceDuplicates;
  Tran.Amount:= FCart.LID.OrderTotal;
  Tran.TaxAmount:= FCart.LID.OrderTax;
  txtPayAmount.Text:= FormatFloat('0.00', Tran.Amount);
  case Tran.TransactionType of
    gtSale: S:= 'Collect';
    gtRefund: S:= 'Refund';
    gtLevel2Sale: S:= 'Collect';
    gtPreAuth: S:= 'Pre Auth';
    gtPostAuth: S:= 'Post Auth';
    gtForceSale: S:= 'Force';
    gtAddValue: S:= 'Add Gift';
    gtBalanceInquiry: S:= '';
    gtUnknown: S:= '';
  end;
  lblPaymentTitle.Text:= S + ' ' + FormatFloat('$#,###,##0.00', Tran.Amount);
  cboPayMethodClick(btnPayGenius);
  actPaymentTab.ExecuteTarget(Self);

  //TODO: Figure out how to not need this ugly work-around!!!!!!!
  cboPayMethodClick(btnPayVault);
  cboPayMethodClick(btnPaySwipe);
  cboPayMethodClick(btnPayKeyed);
  cboPayMethodClick(btnPayCheck);
  cboPayMethodClick(btnPayGenius);

end;

procedure TfrmCayanPOSMain.ClearPaymentInfo;
begin
  liTranType.ItemData.Detail:= GeniusTransactionTypeToStr(Tran.TransactionType);
  //txtPayAmount.Text:= '';
  swSaveToVault.IsChecked:= False;
  txtSaveVaultName.Text:= '';
  txtKeyCardNum.Text:= '';
  txtKeyExpiryMonth.ItemIndex:= -1;
  txtKeyExpiryYear.ItemIndex:= -1;
  txtKeyCardholder.Text:= '';
  txtKeySecCode.Text:= '';
  txtKeyAddress.Text:= '';
  txtKeyZipCode.Text:= '';
  txtPayCheckNum.Text:= '';
  cboVaultCards.Items.Clear;
  txtSwipe.Lines.Clear;

end;

procedure TfrmCayanPOSMain.btnCartBackClick(Sender: TObject);
var
  R: IGeniusCancelTransactionResponse;
begin
  TDialogService.PreferredMode:= TDialogService.TPreferredMode.Platform;
  TDialogService.MessageDialog('Are you sure you wish to cancel invoice?',
    TMsgDlgType.mtConfirmation,
    FMX.Dialogs.mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then begin
        Cursor:= crHandPoint;
        try
          actCustomerTab.ExecuteTarget(Self);
          ClearCart;
          R:= Genius.Cancel;
        finally
          Cursor:= crDefault;
        end;
      end;
    end);
end;

procedure TfrmCayanPOSMain.btnResultBackClick(Sender: TObject);
begin
  ClearCustomer;
  ClearCart;
  actCustomerTab.ExecuteTarget(Self);
end;

procedure TfrmCayanPOSMain.Button10Click(Sender: TObject);
begin
  TDialogService.PreferredMode:= TDialogService.TPreferredMode.Platform;
  TDialogService.MessageDialog('Are you sure you wish to clear the customer?',
    TMsgDlgType.mtConfirmation,
    FMX.Dialogs.mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then begin
        ClearCustomer;
      end;
    end);
end;

procedure TfrmCayanPOSMain.Button5Click(Sender: TObject);
begin
  txtCedAddress.Text:= Self.Genius.Device.DeviceAddress;
  txtCedPort.Value:= Self.Genius.Device.DevicePort;
  txtCedTimeout.Value:= Self.Genius.Device.DeviceTimeout;
  actSetupTab.ExecuteTarget(Self);
end;

procedure TfrmCayanPOSMain.Button6Click(Sender: TObject);
begin
  txtCustShipAddr1.Text:= txtCustBillAddr1.Text;
  txtCustShipAddr2.Text:= txtCustBillAddr2.Text;
  txtCustShipCity.Text:= txtCustBillCity.Text;
  txtCustShipState.Text:= txtCustBillState.Text;
  txtCustShipZip.Text:= txtCustBillZip.Text;
end;

procedure TfrmCayanPOSMain.Button8Click(Sender: TObject);
begin
  ClearCustomers;
  actCustomerLookupTab.ExecuteTarget(Self);
  FCustomers:= DM.POS.GetCustomers('');
  LoadCustomers;
end;

procedure TfrmCayanPOSMain.ClearCustomers;
begin
  Self.lstLookupCustomer.Items.Clear;
  FCustomers:= nil;
  LoadCustomers;
end;

function TfrmCayanPOSMain.CustomerByID(const ID: Integer): ICayanPOSCustomer;
var
  X: Integer;
begin
  Result:= nil;
  for X := 0 to FCustomers.Count-1 do begin
    if FCustomers[X].ID = ID then begin
      Result:= FCustomers[X];
      Break;
    end;
  end;
end;

procedure TfrmCayanPOSMain.LoadCustomers;
var
  C: ICayanPOSCustomer;
  X: Integer;
  I: TListViewItem;
begin
  if Assigned(FCustomers) then begin
    for X := 0 to FCustomers.Count-1 do begin
      C:= FCustomers[X];
      I:= Self.lstLookupCustomer.Items.Add;
      I.Text:= C.FirstName + ' ' + C.LastName;
      I.Detail:= C.CompanyName;
      I.Tag:= C.ID;
      //TODO
    end;
  end;
end;

procedure TfrmCayanPOSMain.btnPaymentBackClick(Sender: TObject);
begin
  //TODO: Check if any payments have already been collected.
  //If so, prompt user if they wish to void payments.

  actCartTab.ExecuteTarget(Self);

end;

procedure TfrmCayanPOSMain.btnCedCancelClick(Sender: TObject);
begin
  //TODO: Cancel transaction...
  FTranStarted:= False;
  Genius.Cancel;

end;

procedure TfrmCayanPOSMain.btnLoginClick(Sender: TObject);
begin
  Self.SaveToConfig;
  if DM.POS.UserLogin(txtLoginUser.Text, txtLoginPassword.Text) then begin
    FSetup:= DM.POS.GetSetup;
    FCart.Setup:= FSetup;
    DM.Cayan.MerchantName:= FSetup.Merch_Name;
    DM.Cayan.MerchantSiteId:= FSetup.Merch_SiteId;
    DM.Cayan.MerchantKey:= FSetup.Merch_Key;
    Genius.ForceDuplicate:= FSetup.ForceDuplicates;
    DM.Cayan.Dba:= FSetup.Dba;
    DM.Cayan.ClerkID:= txtLoginUser.Text;
    Genius.Device.Monitoring:= True;
    Self.actCustomerTab.ExecuteTarget(Self);
  end else begin
    TDialogService.PreferredMode:= TDialogService.TPreferredMode.Platform;
    TDialogService.MessageDialog('Login failed!',
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbNo, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrYes then begin
          ClearCustomer;
        end;
      end);
  end;
end;

procedure TfrmCayanPOSMain.btnCustBackClick(Sender: TObject);
begin
  TDialogService.PreferredMode:= TDialogService.TPreferredMode.Platform;
  TDialogService.MessageDialog('Are you sure you wish to log out?',
    TMsgDlgType.mtConfirmation,
    FMX.Dialogs.mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then begin
        Genius.Device.Monitoring:= False;
        Self.lblTerminalStatus.Text:= '';
        txtLoginUser.Text:= '';
        txtLoginPassword.Text:= '';
        actLoginTab.ExecuteTarget(Self);
        txtLoginUser.SetFocus;
        Self.ClearCustomer;
        Self.ClearCustomers;
      end;
    end);
end;

procedure TfrmCayanPOSMain.btnCedKeyedClick(Sender: TObject);
var
  R: TGeniusKeyedSaleStatus;
begin
  btnCedKeyed.Visible:= False;
  R:= Tran.Genius.Genius.InitiateKeyedEntry;
  if R <> gkSuccess then begin
    btnCedKeyed.Visible:= True;
    //TODO: Raise error...
  end;
end;

procedure TfrmCayanPOSMain.txtLoginUserKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then begin
    btnLoginClick(btnLogin);
  end;
end;

end.

