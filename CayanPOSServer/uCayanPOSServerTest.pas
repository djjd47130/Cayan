unit uCayanPOSServerTest;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Cayan.POS.Server,
  Cayan.XSuperObject;

type
  TCayanPOSSvrTest = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FServer: TCayanPOSServer;
    FConfig: ISuperObject;
    function ConfigFilename: String;
    procedure LoadFromConfig;
    procedure SaveToConfig;
  public
    { Public declarations }
  end;

var
  CayanPOSSvrTest: TCayanPOSSvrTest;

implementation

{$R *.dfm}

uses
  System.IOUtils;

procedure TCayanPOSSvrTest.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FServer.Active:= False;
  //SaveToConfig;
  FreeAndNil(FServer);
end;

procedure TCayanPOSSvrTest.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}
  FServer:= TCayanPOSServer.Create(nil);
  LoadFromConfig;
  FServer.Active:= True;
end;

procedure TCayanPOSSvrTest.LoadFromConfig;
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

procedure TCayanPOSSvrTest.SaveToConfig;
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

function TCayanPOSSvrTest.ConfigFilename: String;
begin
  Result:= TPath.GetPublicPath;
  Result:= TPath.Combine(Result, 'Cayan');
  Result:= TPath.Combine(Result, 'POSServerConfig.json');
end;

end.
