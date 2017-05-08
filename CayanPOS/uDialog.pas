unit uDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, System.ImageList, FMX.ImgList;

type
  TDialogForm = class(TForm)
    DialogLayout: TLayout;
    DimPanel: TPanel;
    DialogPanel: TPanel;
    ButtonPanel: TPanel;
    btnYes: TButton;
    btnNo: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnAbort: TButton;
    btnRetry: TButton;
    btnIgnore: TButton;
    btnAll: TButton;
    btnNoToAll: TButton;
    btnYesToAll: TButton;
    btnHelp: TButton;
    btnClose: TButton;
    DialogLabel: TLabel;
    imgError: TImageControl;
    imgInfo: TImageControl;
    imgConfirm: TImageControl;
    imgWarn: TImageControl;
    procedure FormCreate(Sender: TObject);
    procedure DialogButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCloseDialogProc: TInputCloseDialogProc;
    FDone: Boolean;
    procedure ShowButtons(const AButtons: TMsgDlgButtons);
    procedure ShowIcon(const ADialogType: TMsgDlgType);
    procedure SetDefaultButton(const ABtn: TMsgDlgBtn);
  public

  end;

var
  DialogForm: TDialogForm;

procedure SetDialogDefaultParent(AValue: TFmxObject);

function MsgPrompt(const AMessage: string;
  const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
  const ADefaultButton: TMsgDlgBtn): TModalResult;

procedure MessageDlg(const AMessage: string;
  const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
  const ADefaultButton: TMsgDlgBtn; const ACloseDialogProc: TInputCloseDialogProc);

implementation

{$R *.fmx}

uses
  uDM;

var
  _DefaultParent: TFmxObject;

procedure SetDialogDefaultParent(AValue: TFmxObject);
begin
  _DefaultParent:= AValue;
end;

function MsgPrompt(const AMessage: string;
  const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
  const ADefaultButton: TMsgDlgBtn): TModalResult;
var
  R: TModalResult;
begin
  MessageDlg(AMessage,
    ADialogType,
    AButtons,
    ADefaultButton,
    procedure(const AResult: TModalResult)
    begin
      R:= AResult;
    end);
  Result:= R;
end;

procedure MessageDlg(const AMessage: string;
  const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
  const ADefaultButton: TMsgDlgBtn; const ACloseDialogProc: TInputCloseDialogProc);
var
  F: TDialogForm;
begin
  F:= TDialogForm.Create(nil);
  try
    F.FCloseDialogProc:= ACloseDialogProc;
    F.DialogLabel.Text:= AMessage;
    F.ShowButtons(AButtons);
    F.ShowIcon(ADialogType);
    F.DialogLayout.Parent:= _DefaultParent;
    //Application.ProcessMessages;
    //F.DialogLayout.SetFocus;
    F.SetDefaultButton(ADefaultButton);
    while not F.FDone do begin
      Application.ProcessMessages;
      Sleep(50);
    end;
  finally
    F.Close;
  end;
end;

{ TDialogForm }

procedure TDialogForm.FormCreate(Sender: TObject);
begin
  DialogLayout.Align:= TAlignLayout.Client;
  DimPanel.Align:= TAlignLayout.Client;
  DialogLabel.Text:= '';
end;

procedure TDialogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;

procedure TDialogForm.DialogButtonClick(Sender: TObject);
var
  B: TButton;
  R: TModalResult;
begin
  DialogLayout.Visible:= False;
  B:= TButton(Sender);
  case B.Tag of
    0: R:= mrYes;
    1: R:= mrNo;
    2: R:= mrOK;
    3: R:= mrCancel;
    4: R:= mrAbort;
    5: R:= mrRetry;
    6: R:= mrIgnore;
    7: R:= mrAll;
    8: R:= mrNoToAll;
    9: R:= mrYesToAll;
    10: R:= mrHelp;
    11: R:= mrClose;
    else R:= mrOK;
  end;
  FCloseDialogProc(R);
  FDone:= True;
end;

procedure TDialogForm.ShowIcon(const ADialogType: TMsgDlgType);
begin
  case ADialogType of
    TMsgDlgType.mtWarning:      imgWarn.Visible:= True;
    TMsgDlgType.mtError:        imgError.Visible:= True;
    TMsgDlgType.mtInformation:  imgInfo.Visible:= True;
    TMsgDlgType.mtConfirmation: imgConfirm.Visible:= True;
    TMsgDlgType.mtCustom:       ; //TODO
  end;
end;

procedure TDialogForm.SetDefaultButton(const ABtn: TMsgDlgBtn);
var
  B: TButton;
begin
  B:= nil;
  case ABtn of
    TMsgDlgBtn.mbYes: B:= btnYes;
    TMsgDlgBtn.mbNo: B:= btnNo;
    TMsgDlgBtn.mbOK: B:= btnOK;
    TMsgDlgBtn.mbCancel: B:= btnCancel;
    TMsgDlgBtn.mbAbort: B:= btnAbort;
    TMsgDlgBtn.mbRetry: B:= btnRetry;
    TMsgDlgBtn.mbIgnore: B:= btnIgnore;
    TMsgDlgBtn.mbAll: B:= btnAll;
    TMsgDlgBtn.mbNoToAll: B:= btnNoToAll;
    TMsgDlgBtn.mbYesToAll: B:= btnYesToAll;
    TMsgDlgBtn.mbHelp: B:= btnHelp;
    TMsgDlgBtn.mbClose: B:= btnClose;
  end;
  if Assigned(B) then
    if B.Visible then
      if B.CanFocus then
        B.SetFocus;
end;

procedure TDialogForm.ShowButtons(const AButtons: TMsgDlgButtons);
  procedure ShowBtn(ABtn: TButton);
  begin
    ABtn.Visible:= True;
    ABtn.Align:= TAlignLayout.Left;
    ABtn.Align:= TAlignLayout.Right; //Forces it to left side of prior buttons
    ABtn.Width:= 120;
  end;
begin
  if TMsgDlgBtn.mbYes in AButtons then begin
    btnYes.Visible:= True;
  end;
  if TMsgDlgBtn.mbNo in AButtons then begin
    btnNo.Visible:= True;
  end;
  if TMsgDlgBtn.mbOK in AButtons then begin
    btnOK.Visible:= True;
  end;
  if TMsgDlgBtn.mbCancel in AButtons then begin
    btnCancel.Visible:= True;
  end;
  if TMsgDlgBtn.mbAbort in AButtons then begin
    btnAbort.Visible:= True;
  end;
  if TMsgDlgBtn.mbRetry in AButtons then begin
    btnRetry.Visible:= True;
  end;
  if TMsgDlgBtn.mbIgnore in AButtons then begin
    btnIgnore.Visible:= True;
  end;
  if TMsgDlgBtn.mbAll in AButtons then begin
    btnAll.Visible:= True;
  end;
  if TMsgDlgBtn.mbNoToAll in AButtons then begin
    btnNoToAll.Visible:= True;
  end;
  if TMsgDlgBtn.mbYesToAll in AButtons then begin
    btnYesToAll.Visible:= True;
  end;
  if TMsgDlgBtn.mbHelp in AButtons then begin
    btnHelp.Visible:= True;
  end;
  if TMsgDlgBtn.mbClose in AButtons then begin
    btnClose.Visible:= True;
  end;
end;

end.
