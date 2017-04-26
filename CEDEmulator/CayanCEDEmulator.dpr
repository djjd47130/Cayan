program CayanCEDEmulator;

uses
  System.StartUpCopy,
  FMX.Forms,
  uVeriFoneMX915 in 'uVeriFoneMX915.pas' {frmVeriFoneMX915},
  uDM in 'uDM.pas' {DataModule1: TDataModule},
  uGeniusEmulatorMain in 'Emulators\Genius 5-0-10-0\uGeniusEmulatorMain.pas' {frmGeniusEmulatorMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TfrmVeriFoneMX915, frmVeriFoneMX915);
  Application.CreateForm(TfrmGeniusEmulatorMain, frmGeniusEmulatorMain);
  Application.Run;
end.
