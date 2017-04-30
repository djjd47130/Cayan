unit Cayan.POS;

interface

uses
  System.Classes, System.SysUtils,
  IdHTTP,
  Cayan.XSuperObject;

type


  ICayanPOSCustomer = interface
    ['{FEEF804E-2412-45CC-9C21-7D92872DC713}']
    procedure SetCompanyName(const Value: String);
    procedure SetFirstName(const Value: String);
    procedure SetID(const Value: Integer);
    procedure SetLastName(const Value: String);
    function GetCompanyName: String;
    function GetFirstName: String;
    function GetID: Integer;
    function GetLastName: String;

    property ID: Integer read GetID write SetID;
    property FirstName: String read GetFirstName write SetFirstName;
    property LastName: String read GetLastName write SetLastName;
    property CompanyName: String read GetCompanyName write SetCompanyName;
  end;

  ICayanPOSCustomers = interface
    ['{E941BF0B-9D24-4C4D-8E8D-D946D9FB7684}']
    function GetItem(Index: Integer): ICayanPOSCustomer;

    function Add: ICayanPOSCustomer;
    procedure Delete(const Index: Integer);
    procedure Clear;
    function Count: Integer;

    property Items[Index: Integer]: ICayanPOSCustomer read GetItem; default;
  end;





  TCayanPOSCustomer = class(TInterfacedObject, ICayanPOSCustomer)
  private
    FID: Integer;
    FFirstName: String;
    FLastName: String;
    FCompanyName: String;
    procedure SetCompanyName(const Value: String);
    procedure SetFirstName(const Value: String);
    procedure SetID(const Value: Integer);
    procedure SetLastName(const Value: String);
    function GetCompanyName: String;
    function GetFirstName: String;
    function GetID: Integer;
    function GetLastName: String;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Integer read GetID write SetID;
    property FirstName: String read GetFirstName write SetFirstName;
    property LastName: String read GetLastName write SetLastName;
    property CompanyName: String read GetCompanyName write SetCompanyName;
  end;

  TCayanPOSCustomers = class(TInterfacedObject, ICayanPOSCustomers)
  private
    FItems: TInterfaceList;
    function GetItem(Index: Integer): ICayanPOSCustomer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add: ICayanPOSCustomer;
    procedure Delete(const Index: Integer);
    procedure Clear;
    function Count: Integer;

    property Items[Index: Integer]: ICayanPOSCustomer read GetItem; default;
  end;





  TCayanPOS = class(TComponent)
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

    function UserLogin(const Username, Password: String): Boolean;
    function GetCustomers(const Q: String): ICayanPOSCustomers;
  published
    property Host: String read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
  end;

implementation

{ TCayanPOSCustomer }

constructor TCayanPOSCustomer.Create;
begin
  FID:= 0;
  FFirstName:= '';
  FLastName:= '';
  FCompanyName:= '';
end;

destructor TCayanPOSCustomer.Destroy;
begin

  inherited;
end;

function TCayanPOSCustomer.GetCompanyName: String;
begin
  Result:= FCompanyName;
end;

function TCayanPOSCustomer.GetFirstName: String;
begin
  Result:= FFirstName;
end;

function TCayanPOSCustomer.GetID: Integer;
begin
  Result:= FID;
end;

function TCayanPOSCustomer.GetLastName: String;
begin
  Result:= FLastName;
end;

procedure TCayanPOSCustomer.SetCompanyName(const Value: String);
begin
  FCompanyName := Value;
end;

procedure TCayanPOSCustomer.SetFirstName(const Value: String);
begin
  FFirstName := Value;
end;

procedure TCayanPOSCustomer.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TCayanPOSCustomer.SetLastName(const Value: String);
begin
  FLastName := Value;
end;

{ TCayanPOSCustomers }

constructor TCayanPOSCustomers.Create;
begin
  FItems:= TInterfaceList.Create;
end;

destructor TCayanPOSCustomers.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TCayanPOSCustomers.Add: ICayanPOSCustomer;
begin
  Result:= TCayanPOSCustomer.Create;
  Result._AddRef;
  FItems.Add(Result);
end;

procedure TCayanPOSCustomers.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TCayanPOSCustomers.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TCayanPOSCustomers.Delete(const Index: Integer);
begin
  ICayanPOSCustomer(FItems[Index])._Release;
  FItems.Delete(Index);
end;

function TCayanPOSCustomers.GetItem(Index: Integer): ICayanPOSCustomer;
begin
  Result:= ICayanPOSCustomer(FItems[Index]);
end;

{ TCayanPOS }

constructor TCayanPOS.Create(AOwner: TComponent);
begin
  inherited;
  FHost:= 'LocalHost';
  FPort:= 8787;
  FWeb:= TIdHTTP.Create(nil);
  FWeb.Request.CustomHeaders.AddValue('Connection', 'Keep-Alive');
end;

destructor TCayanPOS.Destroy;
begin
  FreeAndNil(FWeb);
  inherited;
end;

function TCayanPOS.GetUrl(const Action: String; Params: TStrings): String;
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

function TCayanPOS.GetJSON(const Action: String;
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

function TCayanPOS.PostJSON(const Action: String; Params: TStrings;
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

procedure TCayanPOS.SetHost(const Value: String);
begin
  FHost := Value;
end;

procedure TCayanPOS.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

function TCayanPOS.UserLogin(const Username, Password: String): Boolean;
var
  P: TStringList;
  R: ISuperObject;
begin
  Result:= False;
  P:= TStringList.Create;
  try
    P.Values['user']:= Username;
    P.Values['pass']:= Password;
    P.Values['Format']:= 'JSON';
    R:= PostJson('UserLogin', P, nil);
    if R.S['Status'] = 'Success' then begin
      Result:= True;
      FToken:= R.S['Token'];
      FUsername:= R.S['user'];
    end;
  finally
    P.Free;
  end;
end;

function TCayanPOS.GetCustomers(const Q: String): ICayanPOSCustomers;
var
  P: TStringList;
  R, O: ISuperObject;
  Res: TCayanPOSCustomers;
  C: ICayanPOSCustomer;
  X: Integer;
begin
  Result:= nil;
  P:= TStringList.Create;
  try
    P.Values['q']:= Q;
    R:= GetJson('Customers', P);
    Res:= TCayanPOSCustomers.Create;
    try
      for X := 0 to R.AsArray.Length-1 do begin
        O:= R.AsArray.O[X];
        C:= Res.Add;
        C.ID:= O.I['ID'];
        C.FirstName:= O.S['FirstName'];
        C.LastName:= O.S['LastName'];
        C.CompanyName:= O.S['CompanyName'];
      end;
    finally
      Result:= Res;
    end;
  finally
    P.Free;
  end;
end;

end.
