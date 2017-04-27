unit uCayanPOSServerMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.SvcMgr,
  Cayan.POS.Server,
  Cayan.XSuperObject;

type
  TCayanPOSSvr = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FServer: TCayanPOSServer;
    FConfig: ISuperObject;
    function ConfigFilename: String;
    procedure LoadFromConfig;
    procedure SaveToConfig;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  CayanPOSSvr: TCayanPOSSvr;

implementation

{$R *.dfm}

uses
  System.IOUtils;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  CayanPOSSvr.Controller(CtrlCode);
end;

function TCayanPOSSvr.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TCayanPOSSvr.ServiceStart(Sender: TService; var Started: Boolean);
begin
  FServer:= TCayanPOSServer.Create(nil);
  LoadFromConfig;
  FServer.Active:= True;
end;

procedure TCayanPOSSvr.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FServer.Active:= False;
  //SaveToConfig;
  FreeAndNil(FServer);
end;

procedure TCayanPOSSvr.LoadFromConfig;
begin
  if not FileExists(ConfigFilename) then begin
    FServer.Port:= 8787;
    FServer.ConnStr:= ''; //TODO: ???
    SaveToConfig;
  end;
  FConfig:= TSuperObject.ParseFile(ConfigFilename);
  if Assigned(FConfig) then begin
    FServer.Port:= FConfig.I['port'];
    FServer.ConnStr:= FConfig.S['conn'];
  end;
end;

procedure TCayanPOSSvr.SaveToConfig;
var
  L: TStringList;
begin
  FConfig.I['port']:= FServer.Port;
  FConfig.S['conn']:= FServer.ConnStr;
  L:= TStringList.Create;
  try
    L.Text:= FConfig.AsJSON(True);
    ForceDirectories(ExtractFilePath(ConfigFilename));
    L.SaveToFile(ConfigFilename);
  finally
    FreeAndNil(L);
  end;
end;

function TCayanPOSSvr.ConfigFilename: String;
begin
  Result:= TPath.GetPublicPath;
  Result:= TPath.Combine(Result, 'Cayan');
  Result:= TPath.Combine(Result, 'POSServerConfig.json');
end;

end.
