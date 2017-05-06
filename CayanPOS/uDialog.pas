unit uDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts;

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
    procedure FormCreate(Sender: TObject);
    procedure DialogButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCloseDialogProc: TInputCloseDialogProc;
    procedure ShowButtons(const AButtons: TMsgDlgButtons);
  public

  end;

var
  DialogForm: TDialogForm;

procedure MessageDlg(AParent: TFmxObject; const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn;
  const ACloseDialogProc: TInputCloseDialogProc);

implementation

{$R *.fmx}

procedure MessageDlg(AParent: TFmxObject; const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn;
  const ACloseDialogProc: TInputCloseDialogProc);
var
  F: TDialogForm;
begin
  //TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);

  F:= TDialogForm.Create(nil);

  F.FCloseDialogProc:= ACloseDialogProc;
  F.DialogLabel.Text:= AMessage;
  F.ShowButtons(AButtons);
  //TODO: Dialog type...

  F.DialogLayout.Parent:= AParent;

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
  R:= TModalResult(B.Tag);
  Self.FCloseDialogProc(R);
  Close; //TODO: ???
end;

procedure TDialogForm.ShowButtons(const AButtons: TMsgDlgButtons);
  procedure ShowBtn(ABtn: TButton);
  begin
    ABtn.Visible:= True;
    ABtn.Align:= TAlignLayout.Left;
    ABtn.Align:= TAlignLayout.Right; //Forces it to left side of prior buttons
    ABtn.Width:= 80;
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
