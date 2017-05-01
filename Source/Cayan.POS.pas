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
    procedure SetBillAddr1(const Value: String);
    procedure SetBillAddr2(const Value: String);
    procedure SetBillCity(const Value: String);
    procedure SetBillState(const Value: String);
    procedure SetBillZip(const Value: String);
    procedure SetCellPhone(const Value: String);
    procedure SetEmail(const Value: String);
    procedure SetMainPhone(const Value: String);
    procedure SetShipAddr1(const Value: String);
    procedure SetShipAddr2(const Value: String);
    procedure SetShipCity(const Value: String);
    procedure SetShipState(const Value: String);
    procedure SetShipZip(const Value: String);
    function GetBillAddr1: String;
    function GetBillAddr2: String;
    function GetBillCity: String;
    function GetBillState: String;
    function GetBillZip: String;
    function GetCellPhone: String;
    function GetEmail: String;
    function GetMainPhone: String;
    function GetShipAddr1: String;
    function GetShipAddr2: String;
    function GetShipCity: String;
    function GetShipState: String;
    function GetShipZip: String;

    property ID: Integer read GetID write SetID;
    property FirstName: String read GetFirstName write SetFirstName;
    property LastName: String read GetLastName write SetLastName;
    property CompanyName: String read GetCompanyName write SetCompanyName;
    property MainPhone: String read GetMainPhone write SetMainPhone;
    property CellPhone: String read GetCellPhone write SetCellPhone;
    property Email: String read GetEmail write SetEmail;
    property BillAddr1: String read GetBillAddr1 write SetBillAddr1;
    property BillAddr2: String read GetBillAddr2 write SetBillAddr2;
    property BillCity: String read GetBillCity write SetBillCity;
    property BillState: String read GetBillState write SetBillState;
    property BillZip: String read GetBillZip write SetBillZip;
    property ShipAddr1: String read GetShipAddr1 write SetShipAddr1;
    property ShipAddr2: String read GetShipAddr2 write SetShipAddr2;
    property ShipCity: String read GetShipCity write SetShipCity;
    property ShipState: String read GetShipState write SetShipState;
    property ShipZip: String read GetShipZip write SetShipZip;
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
    FMainPhone: String;
    FBillAddr2: String;
    FEmail: String;
    FShipAddr2: String;
    FBillAddr1: String;
    FShipAddr1: String;
    FBillCity: String;
    FShipCity: String;
    FCellPhone: String;
    FBillZip: String;
    FBillState: String;
    FShipZip: String;
    FShipState: String;
    procedure SetCompanyName(const Value: String);
    procedure SetFirstName(const Value: String);
    procedure SetID(const Value: Integer);
    procedure SetLastName(const Value: String);
    function GetCompanyName: String;
    function GetFirstName: String;
    function GetID: Integer;
    function GetLastName: String;
    procedure SetBillAddr1(const Value: String);
    procedure SetBillAddr2(const Value: String);
    procedure SetBillCity(const Value: String);
    procedure SetBillState(const Value: String);
    procedure SetBillZip(const Value: String);
    procedure SetCellPhone(const Value: String);
    procedure SetEmail(const Value: String);
    procedure SetMainPhone(const Value: String);
    procedure SetShipAddr1(const Value: String);
    procedure SetShipAddr2(const Value: String);
    procedure SetShipCity(const Value: String);
    procedure SetShipState(const Value: String);
    procedure SetShipZip(const Value: String);
    function GetBillAddr1: String;
    function GetBillAddr2: String;
    function GetBillCity: String;
    function GetBillState: String;
    function GetBillZip: String;
    function GetCellPhone: String;
    function GetEmail: String;
    function GetMainPhone: String;
    function GetShipAddr1: String;
    function GetShipAddr2: String;
    function GetShipCity: String;
    function GetShipState: String;
    function GetShipZip: String;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Integer read GetID write SetID;
    property FirstName: String read GetFirstName write SetFirstName;
    property LastName: String read GetLastName write SetLastName;
    property CompanyName: String read GetCompanyName write SetCompanyName;
    property MainPhone: String read GetMainPhone write SetMainPhone;
    property CellPhone: String read GetCellPhone write SetCellPhone;
    property Email: String read GetEmail write SetEmail;
    property BillAddr1: String read GetBillAddr1 write SetBillAddr1;
    property BillAddr2: String read GetBillAddr2 write SetBillAddr2;
    property BillCity: String read GetBillCity write SetBillCity;
    property BillState: String read GetBillState write SetBillState;
    property BillZip: String read GetBillZip write SetBillZip;
    property ShipAddr1: String read GetShipAddr1 write SetShipAddr1;
    property ShipAddr2: String read GetShipAddr2 write SetShipAddr2;
    property ShipCity: String read GetShipCity write SetShipCity;
    property ShipState: String read GetShipState write SetShipState;
    property ShipZip: String read GetShipZip write SetShipZip;
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

function TCayanPOSCustomer.GetBillAddr1: String;
begin
  Result:= FBillAddr1;
end;

function TCayanPOSCustomer.GetBillAddr2: String;
begin
  Result:= FBillAddr2;
end;

function TCayanPOSCustomer.GetBillCity: String;
begin
  Result:= FBillCity;
end;

function TCayanPOSCustomer.GetBillState: String;
begin
  Result:= FBillState;
end;

function TCayanPOSCustomer.GetBillZip: String;
begin
  Result:= FBillZip;
end;

function TCayanPOSCustomer.GetCellPhone: String;
begin
  Result:= FCellPhone;
end;

function TCayanPOSCustomer.GetCompanyName: String;
begin
  Result:= FCompanyName;
end;

function TCayanPOSCustomer.GetEmail: String;
begin
  Result:= FEmail;
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

function TCayanPOSCustomer.GetMainPhone: String;
begin
  Result:= FMainPhone;
end;

function TCayanPOSCustomer.GetShipAddr1: String;
begin
  Result:= FShipAddr1;
end;

function TCayanPOSCustomer.GetShipAddr2: String;
begin
  Result:= FShipAddr2;
end;

function TCayanPOSCustomer.GetShipCity: String;
begin
  Result:= FShipCity;
end;

function TCayanPOSCustomer.GetShipState: String;
begin
  Result:= FShipState;
end;

function TCayanPOSCustomer.GetShipZip: String;
begin
  Result:= FShipZip;
end;

procedure TCayanPOSCustomer.SetBillAddr1(const Value: String);
begin
  FBillAddr1 := Value;
end;

procedure TCayanPOSCustomer.SetBillAddr2(const Value: String);
begin
  FBillAddr2 := Value;
end;

procedure TCayanPOSCustomer.SetBillCity(const Value: String);
begin
  FBillCity := Value;
end;

procedure TCayanPOSCustomer.SetBillState(const Value: String);
begin
  FBillState := Value;
end;

procedure TCayanPOSCustomer.SetBillZip(const Value: String);
begin
  FBillZip := Value;
end;

procedure TCayanPOSCustomer.SetCellPhone(const Value: String);
begin
  FCellPhone := Value;
end;

procedure TCayanPOSCustomer.SetCompanyName(const Value: String);
begin
  FCompanyName := Value;
end;

procedure TCayanPOSCustomer.SetEmail(const Value: String);
begin
  FEmail := Value;
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

procedure TCayanPOSCustomer.SetMainPhone(const Value: String);
begin
  FMainPhone := Value;
end;

procedure TCayanPOSCustomer.SetShipAddr1(const Value: String);
begin
  FShipAddr1 := Value;
end;

procedure TCayanPOSCustomer.SetShipAddr2(const Value: String);
begin
  FShipAddr2 := Value;
end;

procedure TCayanPOSCustomer.SetShipCity(const Value: String);
begin
  FShipCity := Value;
end;

procedure TCayanPOSCustomer.SetShipState(const Value: String);
begin
  FShipState := Value;
end;

procedure TCayanPOSCustomer.SetShipZip(const Value: String);
begin
  FShipZip := Value;
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
  try
    if Assigned(Obj) then begin
      Obj.SaveTo(Str);
      Str.Position:= 0;
    end;
    U:= GetUrl(Action, Params);
    S:= FWeb.Post(U, Str);
    Result:= SO(S);
  finally
    FreeAndNil(Str);
  end;
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
        C.MainPhone:= O.S['MainPhone'];
        C.CellPhone:= O.S['CellPhone'];
        C.Email:= O.S['Email'];
        C.BillAddr1:= O.S['BillAddr1'];
        C.BillAddr2:= O.S['BillAddr2'];
        C.BillCity:= O.S['BillCity'];
        C.BillState:= O.S['BillState'];
        C.BillZip:= O.S['BillZip'];
        C.ShipAddr1:= O.S['ShipAddr1'];
        C.ShipAddr2:= O.S['ShipAddr2'];
        C.ShipCity:= O.S['ShipCity'];
        C.ShipState:= O.S['ShipState'];
        C.ShipZip:= O.S['ShipZip'];
      end;
    finally
      Result:= Res;
    end;
  finally
    P.Free;
  end;
end;

end.
