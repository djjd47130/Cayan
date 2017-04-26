unit uEmulatorMain;

{$DEFINE STYLE_BLACK}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Actions,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.ActnList, FMX.Edit, FMX.ListBox,
  Xml.XmlDoc, Xml.XmlDom, Xml.XmlIntf,
  Cayan.XSuperObject, Cayan.Genius.Intf, Cayan.Genius.Emulator,
  FMX.ScrollBox,
  FMX.Memo, System.ImageList, FMX.ImgList, FMX.Objects;

const
  CED_MIN_WIDTH = 520;
  CED_MIN_HEIGHT = 565;

type
  TfrmEmulatorMain = class;

  TGeniusTransactionResult = (trError, trApproved, trDeclined, trDuplicate);

  TfrmEmulatorMain = class(TForm)
    Acts: TActionList;
    actIdleTab: TChangeTabAction;
    actWaitingTab: TChangeTabAction;
    actMainPayTab: TChangeTabAction;
    actSkuDisplayTab: TChangeTabAction;
    Emulator: TCayanGeniusEmulator;
    actResultTab: TChangeTabAction;
    Panel3: TPanel;
    Panel4: TPanel;
    btnKeyOne: TButton;
    btnKeyTwo: TButton;
    btnKeyThree: TButton;
    btnKeyFour: TButton;
    btnKeyFive: TButton;
    btnKeySix: TButton;
    btnKeySeven: TButton;
    btnKeyEight: TButton;
    btnKeyNine: TButton;
    btnKeyCancel: TButton;
    btnKeyZero: TButton;
    btnKeyAccept: TButton;
    btnKeyBack: TButton;
    actImageTab: TChangeTabAction;
    btnSimSwipe: TButton;
    btnSimChip: TButton;
    btnSimProximity: TButton;
    btnSimScan: TButton;
    pMain: TPanel;
    pScreen: TPanel;
    MainTabs: TTabControl;
    tabIdle: TTabItem;
    imgIdle: TImageControl;
    tabSkuDisplay: TTabItem;
    pSkuMain: TGridPanelLayout;
    imgSku: TImageControl;
    pSku: TPanel;
    lblOrderTotal: TLabel;
    lblOrderTotalLabel: TLabel;
    lstSku: TListView;
    tabMainPay: TTabItem;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lblMainPayTotalHead: TLabel;
    lblMainPayTotal: TLabel;
    GridPanelLayout1: TGridPanelLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    tabWaiting: TTabItem;
    pWaiting: TGridPanelLayout;
    Panel2: TPanel;
    Label3: TLabel;
    imgWaiting: TImageControl;
    tabResult: TTabItem;
    pResult: TGridPanelLayout;
    Panel6: TPanel;
    lblResult: TLabel;
    imgResult: TImageControl;
    tabImage: TTabItem;
    imgImage: TImageControl;
    tabAdmin: TTabItem;
    lstAdmin: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    GridPanelLayout2: TGridPanelLayout;
    Button8: TButton;
    Button10: TButton;
    Button7: TButton;
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    Button6: TButton;
    tabAgreement: TTabItem;
    actAgreementTab: TChangeTabAction;
    txtAgree: TMemo;
    GridPanelLayout3: TGridPanelLayout;
    btnAgreementDecline: TButton;
    btnAgreementAccept: TButton;
    Images: TImageList;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EmulatorScreen(Sender: TObject; Screen: TGeniusCedScreen);
    procedure EmulatorItemAdd(Sender: TObject; Item: TCayanGeniusLineItem);
    procedure EmulatorItemDelete(Sender: TObject; Item: TCayanGeniusLineItem);
    procedure EmulatorItemEdit(Sender: TObject; Item: TCayanGeniusLineItem);
    procedure btnSimSwipeClick(Sender: TObject);
    procedure btnSimChipClick(Sender: TObject);
    procedure btnSimProximityClick(Sender: TObject);
    procedure btnSimScanClick(Sender: TObject);
    procedure KeypadButtonClick(Sender: TObject);
    procedure EmulatorAgreement(Sender: TObject; const RequestID, Title,
      AgreementText, AcceptLabel, DeclineLabel: string; var Accepted: Boolean);
    procedure btnAgreementDeclineClick(Sender: TObject);
    procedure btnAgreementAcceptClick(Sender: TObject);
  private
    FScreen: TGeniusCEDScreen;
    FConfig: ISuperObject;
    FInAgreement: Boolean;
    FAgreementResult: Boolean;
    procedure UpdateLID;
    function ConfigFilename: String;
    procedure LoadFromConfig;
    procedure SaveToConfig;
    procedure ShowImage(const Filename, Text: String);
  public
    procedure ShowResult(const Value: TGeniusTransactionResult; const Msg: String);
  end;

var
  frmEmulatorMain: TfrmEmulatorMain;

implementation

{$R *.fmx}

uses
  uDM,
  System.IOUtils;

{ TfrmEmulatorMain }

procedure TfrmEmulatorMain.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}

  pMain.Align:= TAlignLayout.Client;
  MainTabs.Align:= TAlignLayout.Client;
  imgIdle.Align:= TAlignLayout.Client;
  pWaiting.Align:= TAlignLayout.Client;
  pSkuMain.Align:= TAlignLayout.Client;
  pSku.Align:= TAlignLayout.Client;
  lstSku.Align:= TAlignLayout.Client;
  imgSku.Align:= TAlignLayout.Client;
  lblMainPayTotalHead.Align:= TAlignLayout.Client;
  pResult.Align:= TAlignLayout.Client;
  imgResult.Align:= TAlignLayout.Client;
  imgWaiting.Align:= TAlignLayout.Client;
  txtAgree.Align:= TAlignLayout.Client;

  MainTabs.TabPosition:= TTabPosition.None;
  MainTabs.ActiveTab:= tabIdle;
  Self.ToolBarLabel.Text:= 'Idle';

  Width:= CED_MIN_WIDTH;
  Height:= CED_MIN_HEIGHT;

  Self.LoadFromConfig;

  Emulator.Active:= True;

end;

procedure TfrmEmulatorMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.Emulator.Active:= False;
end;

procedure TfrmEmulatorMain.FormResize(Sender: TObject);
begin
  if ClientWidth < CED_MIN_WIDTH then
     ClientWidth := CED_MIN_WIDTH;
  if ClientHeight < CED_MIN_HEIGHT then
     ClientHeight := CED_MIN_HEIGHT;
end;

procedure TfrmEmulatorMain.KeypadButtonClick(Sender: TObject);
var
  B: TButton;
begin
  B:= TButton(Sender);
  case B.Tag of
    0: begin
      //TODO: Detect pressing "0" 3 times in a row while in idle screen,
      //  and then open admin menu.
    end;
    1: begin

    end;
    2: begin

    end;
    3: begin

    end;
    4: begin

    end;
    5: begin

    end;
    6: begin

    end;
    7: begin

    end;
    8: begin

    end;
    9: begin

    end;
    10: begin
      //OK

    end;
    11: begin
      //Back

    end;
    12: begin
      //Cancel

    end;
  end;

end;

procedure TfrmEmulatorMain.LoadFromConfig;
begin
  if not FileExists(ConfigFilename) then begin
    Emulator.Port:= 8080;
    Emulator.Timeout:= 5; //TODO: Minutes?
    SaveToConfig;
  end;
  FConfig:= TSuperObject.ParseFile(ConfigFilename);
  if Assigned(FConfig) then begin
    Emulator.MerchantName:= FConfig.S['merchantName'];
    Emulator.MerchantSiteId:= FConfig.S['merchantSiteId'];
    Emulator.MerchantKey:= FConfig.S['merchantKey'];
    Emulator.Port:= FConfig.I['devicePort'];
    Emulator.Timeout:= FConfig.I['deviceTimeout'];
    Emulator.AppVersion:= FConfig.S['ApplicationVersion'];
    Emulator.OsVersion:= FConfig.S['OSVersion'];
    Emulator.SerialNum:= FConfig.S['SerialNumber'];

  end else begin
    //TODO
  end;
end;

procedure TfrmEmulatorMain.SaveToConfig;
var
  L: TStringList;
begin
  FConfig.S['merchantName']:= Emulator.MerchantName;
  FConfig.S['merchantSiteId']:= Emulator.MerchantSiteId;
  FConfig.S['merchantKey']:= Emulator.MerchantKey;
  FConfig.I['devicePort']:= Emulator.Port;
  FConfig.I['deviceTimeout']:= Emulator.Timeout;

  L:= TStringList.Create;
  try
    L.Text:= FConfig.AsJSON(True);
    ForceDirectories(ExtractFilePath(ConfigFilename));
    L.SaveToFile(ConfigFilename);
  finally
    FreeAndNil(L);
  end;
end;

function TfrmEmulatorMain.ConfigFilename: String;
begin
  Result:= TPath.GetHomePath;
  Result:= TPath.Combine(Result, 'Cayan');
  Result:= TPath.Combine(Result, 'CEDConfig.json');
end;

procedure TfrmEmulatorMain.UpdateLID;
begin
  lblOrderTotal.Text:= FormatFloat('$#,###,##0.00', Emulator.OrderTotal);
  //TODO: Implement more labels such as tax...

end;

procedure TfrmEmulatorMain.btnSimSwipeClick(Sender: TObject);
begin
  //Simulate user swiping card...
  Self.ToolBarLabel.Text:= 'Please Wait For Cashier...';
  Emulator.CardSwiped;
end;

procedure TfrmEmulatorMain.btnAgreementAcceptClick(Sender: TObject);
begin
  //TODO: User accepted agreement...
  Self.FAgreementResult:= True;
  Self.FInAgreement:= False;
  Self.ToolBarLabel.Text:= 'Idle';
  Self.actIdleTab.ExecuteTarget(Self);
end;

procedure TfrmEmulatorMain.btnAgreementDeclineClick(Sender: TObject);
begin
  //TODO: User declined agreement...
  Self.FAgreementResult:= False;
  Self.FInAgreement:= False;
  Self.ToolBarLabel.Text:= 'Idle';
  Self.actIdleTab.ExecuteTarget(Self);
end;

procedure TfrmEmulatorMain.btnSimChipClick(Sender: TObject);
begin
  //Simulate user inserting chip card...
  Self.ToolBarLabel.Text:= 'Please Wait For Cashier...';
  Emulator.CardInserted;
end;

procedure TfrmEmulatorMain.btnSimProximityClick(Sender: TObject);
begin
  //Simulate user touching phone...
  Self.ToolBarLabel.Text:= 'Please Wait For Cashier...';
  Emulator.PhoneContacted;
end;

procedure TfrmEmulatorMain.btnSimScanClick(Sender: TObject);
begin
  //Simulate user scanning phone...
  Self.ToolBarLabel.Text:= 'Please Wait For Cashier...';
  Emulator.PhoneScanned;
end;

procedure TfrmEmulatorMain.ShowResult(const Value: TGeniusTransactionResult; const Msg: String);
var
  P: String;
  FN: String;
begin
  P:= TPath.Combine(ExtractFilePath(ParamStr(0)), 'Img');

  case Value of
    trError: FN:= 'ResultX.png';
    trApproved: FN:= 'ResultCheck.png';
    trDeclined: FN:= 'ResultX.png';
    trDuplicate: FN:= 'ResultX.png';
    else FN:= 'ResultX.png';
  end;
  FN:= TPath.Combine(P, FN);
  imgResult.Bitmap.LoadFromFile(FN);

  lblResult.Text:= Msg;
  ToolBarLabel.Text:= 'Transaction Result';
  actResultTab.ExecuteTarget(Self);

  imgResult.Repaint;
  Application.ProcessMessages;

  tabResult.Repaint;
  Application.ProcessMessages;
end;

procedure TfrmEmulatorMain.EmulatorAgreement(Sender: TObject; const RequestID,
  Title, AgreementText, AcceptLabel, DeclineLabel: string;
  var Accepted: Boolean);
begin
  Self.ToolBarLabel.Text := Title;
  Self.txtAgree.Text:= AgreementText;
  Self.btnAgreementAccept.Text:= 'Accept';
  Self.btnAgreementDecline.Text:= 'Decline';
  Self.actAgreementTab.ExecuteTarget(Self);
  Self.FInAgreement:= True;
  while FInAgreement do begin
    Sleep(10);
    if not Emulator.Genius.IsInAgreement then Break;
    Application.ProcessMessages;
  end;
  Accepted:= Self.FAgreementResult;
end;

procedure TfrmEmulatorMain.EmulatorItemAdd(Sender: TObject;
  Item: TCayanGeniusLineItem);
var
  I: TListViewItem;
begin
  I:= lstSku.Items.Add;
  I.Text:= IntToStr(Item.Qty) + ' ' + Item.Description;
  I.Detail:= FormatFloat('$#,###,##0.00', Item.Amount - Item.TaxAmount);
  I.Tag:= NativeInt(Item);
  UpdateLID;
end;

procedure TfrmEmulatorMain.EmulatorItemEdit(Sender: TObject;
  Item: TCayanGeniusLineItem);
var
  I: TListViewItem;
  X: Integer;
begin
  for X := 0 to lstSku.Items.Count-1 do begin
    I:= lstSku.Items[X];
    if I.Tag = NativeInt(Item) then begin
      I.Text:= IntToStr(Item.Qty) + ' ' + Item.Description;
      I.Detail:= FormatFloat('$#,###,##0.00', Item.Amount - Item.TaxAmount);
      Break;
    end;
  end;
  UpdateLID;
end;

procedure TfrmEmulatorMain.EmulatorItemDelete(Sender: TObject;
  Item: TCayanGeniusLineItem);
var
  I: TListViewItem;
  X: Integer;
begin
  for X := 0 to lstSku.Items.Count-1 do begin
    I:= lstSku.Items[X];
    if I.Tag = NativeInt(Item) then begin
      lstSku.Items.Delete(X);
      Break;
    end;
  end;
  UpdateLID;
end;

procedure TfrmEmulatorMain.ShowImage(const Filename, Text: String);
var
  P: String;
begin
  P:= TPath.Combine(ExtractFilePath(ParamStr(0)), 'Img');
  P:= TPath.Combine(P, Filename);
  imgImage.Bitmap.LoadFromFile(P);
  ToolBarLabel.Text:= Text;
  actImageTab.ExecuteTarget(Self);
end;

procedure TfrmEmulatorMain.EmulatorScreen(Sender: TObject;
  Screen: TGeniusCedScreen);
  procedure SetAct(AAction: TChangeTabAction; ATitle: String);
  begin
    Self.ToolBarLabel.Text:= ATitle;
    AAction.ExecuteTarget(Self);
  end;
begin
  FScreen:= Screen;
  case Screen of
    csIdle: begin
      SetAct(actIdleTab, 'Idle');
    end;
    csValidatingTransportKey: begin
      SetAct(actWaitingTab, 'Validating Transport Key...');
    end;
    csMainPayment: begin
      lblMainPayTotal.Text:= FormatFloat('$#,###,##0.00', Self.Emulator.OrderTotal);
      if Emulator.OrderSubtotal <> '' then
        lblMainPayTotalHead.Text:= Self.Emulator.OrderSubtotal;
      SetAct(actMainPayTab, 'Enter Payment');
    end;
    csCustomPayment: begin
      case Self.Emulator.Selected of
        psNone: SetAct(actMainPayTab, 'Enter Payment');
        psSwipe: ShowImage('PaySwipe.png', 'Please Swipe Card');
        psChip: ShowImage('PaySwipe.png', 'Please Insert Card');
        psContact: ShowImage('PayTap.png', 'Please Tap Phone');
        psScan: ShowImage('PayScan.png', 'Please Scan Phone');
        psCredit: SetAct(actMainPayTab, 'Enter Payment');
        psDebit: SetAct(actMainPayTab, 'Enter Payment');
        psGiftCard: SetAct(actMainPayTab, 'Enter Payment');
        psAndroid: ShowImage('PayTap.png', 'Please Tap Phone');
        psLevelUp: ShowImage('PayScan.png', 'Please Scan Phone');
        else begin

        end;
      end;
    end;
    csLookupCardBin: begin
      SetAct(actWaitingTab, 'Looking Up Card Bin...');
    end;
    csWaitingForAmount: begin
      //TODO: Prompt user for amount...

    end;
    csWaitingForPin: begin
      //TODO: Prompt user for PIN...

    end;
    csProcessing: begin
      SetAct(actWaitingTab, 'Processing Payment...');
    end;
    csWaitingForSignature: begin
      //TODO: Prompt for signature...

    end;
    csProcessingSignature: begin
      SetAct(actWaitingTab, 'Processing Signature...');
    end;
    csTransactionResult: begin
      ShowResult(TGeniusTransactionResult.trApproved, 'Approved');
    end;
    csCancelConfirm: begin
      //TODO: Confirm cancellation...

    end;
    csRunAsCreditConfirm: begin

    end;
    csSkuDisplay: begin
      UpdateLID;
      SetAct(actSkuDisplayTab, 'Insert / Swipe / Tap / Scan');
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
  Application.ProcessMessages;
  Sleep(1);
  Application.ProcessMessages;
  Sleep(1);
  Application.ProcessMessages;
  Sleep(1);
end;

end.
