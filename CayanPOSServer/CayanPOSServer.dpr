program CayanPOSServer;

uses
  Vcl.SvcMgr,
  AppInit in 'AppInit.pas',
  uCayanPOSServerMain in 'uCayanPOSServerMain.pas' {CayanPOSSvr: TService},
  uCayanPOSServerTest in 'uCayanPOSServerTest.pas' {CayanPOSSvrTest};

{$R *.RES}

begin
  RunApp;
end.
