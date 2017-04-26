unit uTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Cayan.Genius, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, IdURI,
  Cayan.XSuperObject,
  Cayan.Genius.Intf, Cayan.Genius.Agreement, Cayan;

type
  TForm1 = class(TForm)
    btnSendCommand: TBitBtn;
    txtDeclineText: TEdit;
    txtAcceptText: TEdit;
    txtAgreementText: TMemo;
    txtTitle: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BitBtn2: TBitBtn;
    Genius: TCayanGenius;
    lblResult: TLabel;
    Agree: TCayanGeniusAgreement;
    Cayan: TCayan;
    procedure btnSendCommandClick(Sender: TObject);
    procedure AgreeAgreement(Sender: TObject;
      const Response: IGeniusAgreementResponse);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}

  lblResult.Caption:= '';

end;

procedure TForm1.AgreeAgreement(Sender: TObject;
  const Response: IGeniusAgreementResponse);
begin
  case Response.Status of
    arsAccepted: begin
      lblResult.Caption:= 'Accepted';
      lblResult.Font.Color:= clGreen;
    end;
    arsDeclined: begin
      lblResult.Caption:= 'Declined';
      lblResult.Font.Color:= clMaroon;
    end;
    arsTimeout: begin
      lblResult.Caption:= 'Timeout';
      lblResult.Font.Color:= clRed;
    end;
    arsPOSCancelled: begin
      lblResult.Caption:= 'POS Cancelled';
      lblResult.Font.Color:= clRed;
    end;
    arsError: begin
      lblResult.Caption:= 'Error: ' + Response.ErrorMessage;
      lblResult.Font.Color:= clRed;
    end;
  end;

  lblResult.Visible:= True;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  Agree.CancelRequest;
end;

procedure TForm1.btnSendCommandClick(Sender: TObject);
begin
  lblResult.Caption:= 'Please Wait...';
  lblResult.Font.Color:= clNavy;
  Agree.Title:= txtTitle.Text;
  Agree.AgreementText.Assign(txtAgreementText.Lines);
  Agree.AcceptLabel:= txtAcceptText.Text;
  Agree.DeclineLabel:= txtDeclineText.Text;
  Application.ProcessMessages;
  Agree.SendRequest;
end;

end.
