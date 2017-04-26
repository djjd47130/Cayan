program CayanPOS;

uses
  System.StartUpCopy,
  FMX.Forms,
  uCayanPOSMain in 'uCayanPOSMain.pas' {frmCayanPOSMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCayanPOSMain, frmCayanPOSMain);
  Application.Run;
end.
