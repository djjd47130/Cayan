unit Cayan.POS;

interface

uses
  System.Classes, System.SysUtils,
  IdHTTP,
  Cayan.XSuperObject;

type

  ICayanPOSSetup = interface
    ['{8F82F808-23B1-4873-AF0C-E866A13FD166}']
    procedure SetMerch_Key(const Value: String);
    procedure SetMerch_Name(const Value: String);
    procedure SetMerch_SiteId(const Value: String);
    function GetMerch_Key: String;
    function GetMerch_Name: String;
    function GetMerch_SiteId: String;
    function GetDba: String;
    function GetForceDuplicates: Boolean;
    procedure SetDba(const Value: String);
    procedure SetForceDuplicates(const Value: Boolean);

    property Merch_Name: String read GetMerch_Name write SetMerch_Name;
    property Merch_SiteId: String read GetMerch_SiteId write SetMerch_SiteId;
    property Merch_Key: String read GetMerch_Key write SetMerch_Key;
    property ForceDuplicates: Boolean read GetForceDuplicates write SetForceDuplicates;
    property Dba: String read GetDba write SetDba;
  end;

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

  ICayanPOSCard = interface
    ['{CC13AA4B-6611-421C-9460-B16220FE66BC}']
    procedure SetCaption(const Value: String);
    procedure SetCardholder(const Value: String);
    procedure SetCardNum(const Value: String);
    procedure SetExpiryMonth(const Value: Integer);
    procedure SetExpiryYear(const Value: Integer);
    procedure SetID(const Value: Integer);
    procedure SetToken(const Value: String);
    function GetCaption: String;
    function GetCardholder: String;
    function GetCardNum: String;
    function GetExpiryMonth: Integer;
    function GetExpiryYear: Integer;
    function GetID: Integer;
    function GetToken: String;

    property ID: Integer read GetID write SetID;
    property Caption: String read GetCaption write SetCaption;
    property Token: String read GetToken write SetToken;
    property Cardholder: String read GetCardholder write SetCardholder;
    property CardNum: String read GetCardNum write SetCardNum;
    property ExpiryMonth: Integer read GetExpiryMonth write SetExpiryMonth;
    property ExpiryYear: Integer read GetExpiryYear write SetExpiryYear;
  end;

  ICayanPOSCards = interface
    ['{CCDE306E-579F-46FA-B09C-272DD123C693}']
    function GetItem(Index: Integer): ICayanPOSCard;

    function Add: ICayanPOSCard;
    procedure Delete(const Index: Integer);
    procedure Clear;
    function Count: Integer;

    property Items[Index: Integer]: ICayanPOSCard read GetItem; default;
  end;









  TCayanPOSSetup = class(TInterfacedObject, ICayanPOSSetup)
  private
    FMerch_Key: String;
    FMerch_Name: String;
    FMerch_SiteId: String;
    FForceDuplicates: Boolean;
    FDba: String;
    procedure SetMerch_Key(const Value: String);
    procedure SetMerch_Name(const Value: String);
    procedure SetMerch_SiteId(const Value: String);
    function GetMerch_Key: String;
    function GetMerch_Name: String;
    function GetMerch_SiteId: String;
    function GetDba: String;
    function GetForceDuplicates: Boolean;
    procedure SetDba(const Value: String);
    procedure SetForceDuplicates(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property Merch_Name: String read GetMerch_Name write SetMerch_Name;
    property Merch_SiteId: String read GetMerch_SiteId write SetMerch_SiteId;
    property Merch_Key: String read GetMerch_Key write SetMerch_Key;
    property ForceDuplicates: Boolean read GetForceDuplicates write SetForceDuplicates;
    property Dba: String read GetDba write SetDba;
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

  TCayanPOSCard = class(TInterfacedObject, ICayanPOSCard)
  private
    FExpiryYear: Integer;
    FExpiryMonth: Integer;
    FID: Integer;
    FCaption: String;
    FCardholder: String;
    FToken: String;
    FCardNum: String;
    procedure SetCaption(const Value: String);
    procedure SetCardholder(const Value: String);
    procedure SetCardNum(const Value: String);
    procedure SetExpiryMonth(const Value: Integer);
    procedure SetExpiryYear(const Value: Integer);
    procedure SetID(const Value: Integer);
    procedure SetToken(const Value: String);
    function GetCaption: String;
    function GetCardholder: String;
    function GetCardNum: String;
    function GetExpiryMonth: Integer;
    function GetExpiryYear: Integer;
    function GetID: Integer;
    function GetToken: String;
  public
    constructor Create;
    destructor Destroy; override;

    property ID: Integer read GetID write SetID;
    property Caption: String read GetCaption write SetCaption;
    property Token: String read GetToken write SetToken;
    property Cardholder: String read GetCardholder write SetCardholder;
    property CardNum: String read GetCardNum write SetCardNum;
    property ExpiryMonth: Integer read GetExpiryMonth write SetExpiryMonth;
    property ExpiryYear: Integer read GetExpiryYear write SetExpiryYear;
  end;

  TCayanPOSCards = class(TInterfacedObject, ICayanPOSCards)
  private
    FItems: TInterfaceList;
    function GetItem(Index: Integer): ICayanPOSCard;
  public
    constructor Create;
    destructor Destroy; override;

    function Add: ICayanPOSCard;
    procedure Delete(const Index: Integer);
    procedure Clear;
    function Count: Integer;

    property Items[Index: Integer]: ICayanPOSCard read GetItem; default;
  end;







  TCayanPOS = class(TComponent)
  private
    FWeb: TIdHTTP;
    FPort: Integer;
    FHost: String;
    FToken: String;
    FUsername: String;
    FSetup: ICayanPOSSetup;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetUrl(const Action: String; Params: TStrings): String;
    function GetJSON(const Action: String; Params: TStrings): ISuperObject;
    function PostJSON(const Action: String; Params: TStrings; Obj: ISuperObject): ISuperObject;

    function UserLogin(const Username, Password: String): Boolean;
    function GetSetup: ICayanPOSSetup;
    function GetCustomers(const Q: String): ICayanPOSCustomers;
    function GetVaultCards(const CustomerID: Integer): ICayanPOSCards;
  published
    property Host: String read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
  end;

implementation

{ TCayanPOSSetup }

constructor TCayanPOSSetup.Create;
begin

end;

destructor TCayanPOSSetup.Destroy;
begin

  inherited;
end;

function TCayanPOSSetup.GetDba: String;
begin
  Result:= FDba;
end;

function TCayanPOSSetup.GetForceDuplicates: Boolean;
begin
  Result:= FForceDuplicates;
end;

function TCayanPOSSetup.GetMerch_Key: String;
begin
  Result:= FMerch_Key;
end;

function TCayanPOSSetup.GetMerch_Name: String;
begin
  Result:= FMerch_Name;
end;

function TCayanPOSSetup.GetMerch_SiteId: String;
begin
  Result:= FMerch_SiteId;
end;

procedure TCayanPOSSetup.SetDba(const Value: String);
begin
  FDba:= Value;
end;

procedure TCayanPOSSetup.SetForceDuplicates(const Value: Boolean);
begin
  FForceDuplicates:= Value;
end;

procedure TCayanPOSSetup.SetMerch_Key(const Value: String);
begin
  FMerch_Key := Value;
end;

procedure TCayanPOSSetup.SetMerch_Name(const Value: String);
begin
  FMerch_Name := Value;
end;

procedure TCayanPOSSetup.SetMerch_SiteId(const Value: String);
begin
  FMerch_SiteId := Value;
end;

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

{ TCayanPOSCard }

constructor TCayanPOSCard.Create;
begin

end;

destructor TCayanPOSCard.Destroy;
begin

  inherited;
end;

function TCayanPOSCard.GetCaption: String;
begin
  Result:= FCaption;
end;

function TCayanPOSCard.GetCardholder: String;
begin
  Result:= FCardholder;
end;

function TCayanPOSCard.GetCardNum: String;
begin
  Result:= FCardNum;
end;

function TCayanPOSCard.GetExpiryMonth: Integer;
begin
  Result:= FExpiryMonth;
end;

function TCayanPOSCard.GetExpiryYear: Integer;
begin
  Result:= FExpiryYear;
end;

function TCayanPOSCard.GetID: Integer;
begin
  Result:= FID;
end;

function TCayanPOSCard.GetToken: String;
begin
  Result:= FToken;
end;

procedure TCayanPOSCard.SetCaption(const Value: String);
begin
  FCaption := Value;
end;

procedure TCayanPOSCard.SetCardholder(const Value: String);
begin
  FCardholder := Value;
end;

procedure TCayanPOSCard.SetCardNum(const Value: String);
begin
  FCardNum := Value;
end;

procedure TCayanPOSCard.SetExpiryMonth(const Value: Integer);
begin
  FExpiryMonth := Value;
end;

procedure TCayanPOSCard.SetExpiryYear(const Value: Integer);
begin
  FExpiryYear := Value;
end;

procedure TCayanPOSCard.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TCayanPOSCard.SetToken(const Value: String);
begin
  FToken := Value;
end;

{ TCayanPOSCards }

constructor TCayanPOSCards.Create;
begin
  FItems:= TInterfaceList.Create;
end;

destructor TCayanPOSCards.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TCayanPOSCards.Add: ICayanPOSCard;
begin
  Result:= TCayanPOSCard.Create;
  Result._AddRef;
  FItems.Add(Result);
end;

procedure TCayanPOSCards.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TCayanPOSCards.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TCayanPOSCards.Delete(const Index: Integer);
begin
  ICayanPOSCard(FItems[Index])._Release;
  FItems.Delete(Index);
end;

function TCayanPOSCards.GetItem(Index: Integer): ICayanPOSCard;
begin
  Result:= ICayanPOSCard(FItems[Index]);
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
  if Assigned(FSetup) then begin
    FSetup._Release;
    FSetup:= nil;
  end;
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

function TCayanPOS.GetVaultCards(const CustomerID: Integer): ICayanPOSCards;
var
  P: TStringList;
  R, O: ISuperObject;
  Res: TCayanPOSCards;
  C: ICayanPOSCard;
  X: Integer;
begin
  Result:= nil;
  P:= TStringList.Create;
  try
    P.Values['CustomerID']:= IntToStr(CustomerID);
    R:= GetJson('Vault', P);
    Res:= TCayanPOSCards.Create;
    try
      for X := 0 to R.AsArray.Length-1 do begin
        O:= R.AsArray.O[X];
        C:= Res.Add;
        C.ID:= O.I['ID'];
        C.Caption:= O.S['Caption'];
        C.Token:= O.S['Token'];
        C.Cardholder:= O.S['Cardholder'];
        C.CardNum:= O.S['CardNumber'];
        C.ExpiryMonth:= O.I['ExpiryMonth'];
        C.ExpiryYear:= O.I['ExpiryYear'];
      end;
    finally
      Result:= Res;
    end;
  finally
    P.Free;
  end;
end;

function TCayanPOS.GetSetup: ICayanPOSSetup;
var
  P: TStringList;
  R: ISuperObject;
begin
  Result:= TCayanPOSSetup.Create;
  P:= TStringList.Create;
  try
    R:= GetJSON('Setup', P);
    Result.Merch_Name:= R.S['Merch_Name'];
    Result.Merch_SiteId:= R.S['Merch_SiteId'];
    Result.Merch_Key:= R.S['Merch_Key'];
    Result.ForceDuplicates:= R.I['Force_Duplicates'] = 1;
    Result.Dba:= R.S['Dba'];
    FSetup:= Result;
    FSetup._AddRef;
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
