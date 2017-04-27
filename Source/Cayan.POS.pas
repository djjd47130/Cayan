unit Cayan.POS;

interface

uses
  System.Classes, System.SysUtils,
  IdHTTP,
  Cayan.XSuperObject;

type
  TCayanGeniusPOS = class(TComponent)
  private
    FWeb: TIdHTTP;
    FPort: Integer;
    FHost: String;
    FToken: String;
    FUsername: String;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetUrl(const Action: String; Params: TStrings): String;
    function GetJSON(const Action: String; Params: TStrings): ISuperObject;
    function PostJSON(const Action: String; Params: TStrings; Obj: ISuperObject): ISuperObject;

    procedure UserLogin(const Username, Password: String);
  published
    property Host: String read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
  end;

implementation

{ TCayanGeniusPOS }

constructor TCayanGeniusPOS.Create(AOwner: TComponent);
begin
  inherited;
  FHost:= 'LocalHost';
  FPort:= 8787;
end;

destructor TCayanGeniusPOS.Destroy;
begin

  inherited;
end;

function TCayanGeniusPOS.GetUrl(const Action: String; Params: TStrings): String;
var
  X: Integer;
begin
  Result:= 'http://'+FHost+':'+IntToStr(FPort)+'/v1/'+Action+'?';
  if Assigned(Params) then begin
    for X := 0 to Params.Count-1 do begin
      if X > 0 then Result:= Result + '&';
      Result:= Result + Params.Names[X]+'='+Params.Values[Params.Names[X]];
    end;
  end;
end;

function TCayanGeniusPOS.GetJSON(const Action: String;
  Params: TStrings): ISuperObject;
var
  U: String;
  S: String;
begin
  Result:= SO;
  U:= GetUrl(Action, Params);
  S:= FWeb.Get(U);
  Result:= SO(S);
end;

function TCayanGeniusPOS.PostJSON(const Action: String; Params: TStrings;
  Obj: ISuperObject): ISuperObject;
var
  U: String;
  S: String;
  Str: TMemoryStream;
begin
  Result:= SO;
  Str:= TMemoryStream.Create;
  if Assigned(Obj) then begin
    Obj.SaveTo(Str);
    Str.Position:= 0;
  end;
  U:= GetUrl(Action, Params);
  S:= FWeb.Post(U, Str);
  Result:= SO(S);
end;

procedure TCayanGeniusPOS.SetHost(const Value: String);
begin
  FHost := Value;
end;

procedure TCayanGeniusPOS.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TCayanGeniusPOS.UserLogin(const Username, Password: String);
var
  P: TStringList;
  R: ISuperObject;
begin
  P:= TStringList.Create;
  try
    P.Values['user']:= Username;
    P.Values['pass']:= Password;
    P.Values['Format']:= 'JSON';
    R:= PostJson('UserLogin', P, nil);

  finally
    P.Free;
  end;
end;

end.
