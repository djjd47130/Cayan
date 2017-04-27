unit AppInit;

interface

uses
  Vcl.SvcMgr,
  Vcl.Forms,
  uCayanPOSServerMain,
  uCayanPOSServerTest;

procedure RunApp;

implementation

var
  TestApp: Boolean;

procedure RunApp;
begin
  {$IFDEF DEBUG}
  TestApp:= True;
  {$ELSE}
  TestApp:= False;
  {$ENDIF}

  if TestApp then begin
    Vcl.Forms.Application.Initialize;
    Vcl.Forms.Application.MainFormOnTaskbar := True;
    Vcl.Forms.Application.CreateForm(TCayanPOSSvrTest, CayanPOSSvrTest);
    Vcl.Forms.Application.Run;
  end else begin
    if not Vcl.SvcMgr.Application.DelayInitialize or Vcl.SvcMgr.Application.Installing then
      Vcl.SvcMgr.Application.Initialize;
    Vcl.SvcMgr.Application.CreateForm(TCayanPOSSvr, CayanPOSSvr);
    Vcl.SvcMgr.Application.Run;
  end;
end;

end.
