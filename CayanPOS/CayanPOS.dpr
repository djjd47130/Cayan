program CayanPOS;

uses
  System.StartUpCopy,
  FMX.Forms,
  uCayanPOSMain in 'uCayanPOSMain.pas' {frmCayanPOSMain},
  uDM in 'uDM.pas' {DM: TDataModule},
  uCayanPOSCart in 'uCayanPOSCart.pas' {frmCart};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TfrmCayanPOSMain, frmCayanPOSMain);
  Application.Run;
end.
