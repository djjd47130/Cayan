program CayanPOS;

uses
  System.StartUpCopy,
  FMX.Forms,
  uCayanPOSMain in 'uCayanPOSMain.pas' {frmCayanPOSMain},
  uDM in 'uDM.pas' {DM: TDataModule},
  uCayanPOSCart in 'uCayanPOSCart.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TfrmCayanPOSMain, frmCayanPOSMain);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
