unit Cayan.Common;

interface

uses
  System.Classes, System.SysUtils,
  Xml.XmlDoc, Xml.XmlDom, Xml.XmlIntf,
  StrUtils, DateUtils, IdURI,
  Cayan.XSuperObject;

const
  sLineBreak = #10#13;

type
  TCardType = (ctUnknown = 0, ctAmex = 1, ctDiscover = 2, ctMastercard = 3, ctVisa = 4,
    ctDebit = 5, ctEbt = 6, ctEgc = 7, ctWex = 8, ctVoyager = 9, ctJcb = 10, ctCup = 11);

  TEbtBasisType = (ebUnknown = 0, ebFoodStamp = 1, ebCashBenefit = 2);

  TPosEntryType = (peUnknown = 0, peKeyed = 1, peSwipe = 2, peAuthorization = 3);

  TSignatureType = (stUnknown = 0, stTiffZip = 1, stVectorText = 2);

  TTransactionType = (ttUnknown = 0, ttSale = 1, ttRefund = 2, ttVoid = 3, ttForce = 4,
    ttAuth = 5, ttCapture = 6, ttAdjust = 7, ttRepeatSale = 8, ttPostAuth = 9);

  TApprovalStatus = (asApproved, asDeclined, asReferral, asDuplicate);
  TApprovalStatusSet = set of TApprovalStatus;

  TGiftTransactionType = (gtUnknown = 0, gtRedemption = 1, gtAddValue = 2, gtRedeemPoints = 3,
    gtAddPoints = 4, gtBalanceInquire = 5, gtActivationPurchase = 6, gtActivationReturn = 7,
    gtCredit = 8, gtBalanceTransfer = 9, gtAddTip = 10, gtVoid = 11, gtTotalsInquiry = 12,
    gtRedemptionForce = 19);

  TCardSubType = (cstApproved, cstDeclined, cstApprovedNotCaptured, cstApprovedPurchCard,
    cstApprovedBusCard, cstApprovedCorpCard);

  ///  <summary>
  ///    Encapsulates a card number with validation routines
  ///  </summary>
  TCardNumber = record
    Num: String;
    class operator implicit(Value: TCardNumber): String;
    class operator implicit(Value: String): TCardNumber;
    function GetStr(const Delim: String = ''): String;
    function Masked: String;
    function IsValid: Boolean;
    function CardType: TCardType;
  end;

  ///  <summary>
  ///    Encapsulates a card expiration date - month and year
  ///  </summary>
  TExpirationDate = record
    Month, Year: Word;
    class operator implicit(Value: TExpirationDate): String;
    class operator implicit(Value: String): TExpirationDate;
    function GetStr(const Reverse: Boolean; const Slash: Boolean): String;
    function Expired: Boolean;
    function Expiring: Boolean;
  end;

  ///  <summary>
  ///    Encapsulates complete card information (keyed entry)
  ///  </summary>
  TCardInfo = record
    Num: TCardNumber;
    Exp: TExpirationDate;
    Appr: String;
    CVC: String;
    BillStreet: String;
    BillZip: String;
    class operator implicit(Value: TCardInfo): String;
    class operator implicit(Value: String): TCardInfo;
  end;

  ///  <summary>
  ///    Encapsulates URL parameter list for HTTP requests
  ///  </summary>
  TParamList = class(TObject)
  private
    FItems: TStringList;
    function GetParam(const Name: String): String;
    procedure SetParam(const Name, Value: String);
    function GetParamStr: String;
    procedure SetParamStr(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    procedure Clear;
    function Name(const Index: Integer): String;
    function Value(const Index: Integer): String;
    property Param[const Name: String]: String read GetParam write SetParam; default;
    property ParamStr: String read GetParamStr write SetParamStr;
  end;

  TDataFormat = (efXML, efJSON);

  TCombinedData = class(TPersistent)
  private
    FItems: TInterfaceList;
    FObj: ISuperObject;
    FCurObj: ISuperObject;
    FXmlDoc: IXmlDocument;
    FXml: IXmlNode;
    FCurXml: IXmlNode;
    FRootElement: String;
    FFormat: TDataFormat;
    procedure SetRootElement(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(AFormat: TDataFormat);
    procedure AddObject(const Name: String);
    procedure DoneObject;
    procedure AddStr(const Name: String; const Val: String);
    procedure AddInt(const Name: String; const Val: Integer);
    procedure AddCur(const Name: String; const Val: Currency);
    function AsXML: String;
    function AsJSON: String;
    function AsString: String;
  public
    property RootElement: String read FRootElement write SetRootElement;
  end;

  TXmlVersion = (xv10, xv11);

{ Common Functions }

function CardTypeToStr(const V: TCardType): String;
function IntToStrLen(const Value, Len: Integer): String;
function ExpDate(const Month, Year: Integer; const Swap: Boolean = False): String;

{ HTTP Functions }

function UriEncode(const Value: String): String;
function UriDecode(const Value: String): String;

{ XML Functions }

function XmlVal(const N: String; const V: String; const LineBreak: Boolean = False): String; overload;
function XmlVal(const N: String; const V: Integer; const LineBreak: Boolean = False): String; overload;
function XmlVal(const N: String; const V: Currency; const LineBreak: Boolean = False): String; overload;
function XmlVal(const N: String; const V: Boolean; const LineBreak: Boolean = False): String; overload;
function XmlVal(const N: String; const V: TDate; const LineBreak: Boolean = False): String; overload;
function XmlEncode(val: string):string;
function SelectNode(xnRoot: IXmlNode; const nodePath: String): IXmlNode;
function GetNodePath(DOM: IXMLDocument; StrPath: string): IXmlNode;
function GetSoapNodePath(DOM: IXMLDocument; StrPath: String): IXmlNode;
function GetSoapNodeRoot(DOM: IXMLDocument; Name: String; const Path: String = ''): IXmlNode;
function GetNodeValue(Node: IXmlNode; StrPath: string):string;

implementation

function UriEncode(const Value: String): String;
begin
  Result:= TIdURI.URLEncode(Value);
end;

function UriDecode(const Value: String): String;
begin
  Result:= TIdURI.URLDecode(Value);
end;

function XmlVal(const N: String; const V: String; const LineBreak: Boolean = False): String;
begin
  Result:= Format('<%s>%s</%s>', [N, XmlEncode(V), N]);
end;

function XmlVal(const N: String; const V: Integer; const LineBreak: Boolean = False): String;
begin
  Result:= XmlVal(N, IntToStr(V), LineBreak);
end;

function XmlVal(const N: String; const V: Currency; const LineBreak: Boolean = False): String;
begin
  Result:= XmlVal(N, CurrToStr(V), LineBreak);
end;

function XmlVal(const N: String; const V: Boolean; const LineBreak: Boolean = False): String;
begin
  Result:= XmlVal(N, IfThen(V, 'true', 'false'), LineBreak);
end;

function XmlVal(const N: String; const V: TDate; const LineBreak: Boolean = False): String;
begin
  Result:= XmlVal(N, FormatDateTime('yyyy-mm-dd', V), LineBreak);
end;

function CardTypeToStr(const V: TCardType): String;
begin
  case V of
    ctUnknown:      Result:= 'Unknown';
    ctAmex:         Result:= 'American Express';
    ctDiscover:     Result:= 'Discover';
    ctMastercard:   Result:= 'Mastercard';
    ctVisa:         Result:= 'Visa';
    ctDebit:        Result:= 'Debit';
    ctEbt:          Result:= 'EBT';
    ctEgc:          Result:= 'EGC';
    ctWex:          Result:= 'WEX';
    ctVoyager:      Result:= 'Voyager';
    ctJcb:          Result:= 'JCB';
    ctCup:          Result:= 'CUP';
  end;
end;

function IntToStrLen(const Value, Len: Integer): String;
begin
  Result:= IntToStr(Value);
  while Length(Result) < Len do
    Result:= '0' + Result;
  if Length(Result) > Len then
    raise Exception.Create('Result is larger than restricted length');
end;

function ExpDate(const Month, Year: Integer; const Swap: Boolean = False): String;
begin
  if not Swap then
    Result:= IntToStrLen(Month, 2) + IntToStrLen(Year, 2)
  else
    Result:= IntToStrLen(Year, 2) + IntToStrLen(Month, 2);
end;

{$IFDEF OLD}

function GetNodePath(DOM: IXMLDOMDocument2; StrPath: string): IXMLDOMNode;
var
  TestNode : IXMLDOMNode;
begin
  Result := nil;
  if (Not Assigned(DOM)) then
     Exit;

  TestNode := DOM.selectSingleNode(StrPath);
  if (Not Assigned(testnode)) then
    Exit;

  Result := TestNode
end;

function GetSoapNodePath(DOM: IXMLDOMDocument2; StrPath: String): IXMLDOMNode;
begin
  Result:= GetNodePath(DOM, '/soap:Envelope/soap:Body'+StrPath);

end;

function GetSoapNodeRoot(DOM: IXMLDOMDocument2; Name: String; const Path: String = ''): IXMLDOMNode;
begin
  Result:= GetNodePath(DOM, '/soap:Envelope/soap:Body/'+Name+'Response/'+Name+'Result'+Path);
end;

function GetNodeValue(Node: IXMLDOMNode; StrPath: string):string;
var
  TargetNode : IXMLDOMNode;
begin
  Result:= '';
  if (Not Assigned(Node)) then
      Exit;

  TargetNode := Node.selectSingleNode(StrPath);
  if (Not Assigned(targetnode)) then
     Exit;

  Result := TargetNode.text;
end;

{$ELSE}

function SelectNode(xnRoot: IXmlNode; const nodePath: String): IXmlNode;
var
  intfSelect : IDomNodeSelect;
  dnResult : IDomNode;
  intfDocAccess : IXmlDocumentAccess;
  doc: TXmlDocument;
begin
  Result := nil;
  if not Assigned(xnRoot) or not Supports(xnRoot.DOMNode, IDomNodeSelect, intfSelect) then
    Exit;
  dnResult := intfSelect.selectNode(nodePath);
  if Assigned(dnResult) then
  begin
    if Supports(xnRoot.OwnerDocument, IXmlDocumentAccess, intfDocAccess) then
      doc := intfDocAccess.DocumentObject
    else
      doc := nil;
    Result := TXmlNode.Create(dnResult, nil, doc);
  end;
end;

function GetNodePath(DOM: IXMLDocument; StrPath: string): IXmlNode;
begin
  Result:= SelectNode(DOM.DocumentElement, StrPath);
end;

function GetSoapNodePath(DOM: IXMLDocument; StrPath: String): IXmlNode;
begin
  Result:= GetNodePath(DOM, '/soap:Envelope/soap:Body'+StrPath);
end;

function GetSoapNodeRoot(DOM: IXMLDocument; Name: String; const Path: String = ''): IXmlNode;
begin
  Result:= GetNodePath(DOM, '/soap:Envelope/soap:Body/'+Name+'Response/'+Name+'Result'+Path);
end;

function GetNodeValue(Node: IXmlNode; StrPath: string):string;
var
  TargetNode : IXmlNode;
begin
  Result:= '';
  if (Not Assigned(Node)) then
      Exit;

  TargetNode := Node.ChildNodes.FindNode(StrPath);
  if (Not Assigned(targetnode)) then
     Exit;

  Result := TargetNode.text;
end;

{$ENDIF}

function XmlEncode(val: string):string;
var
  s : string;
begin
    s := AnsiReplaceStr(Trim(val), '&', '&amp;');
    s := AnsiReplaceStr(s, '<', '&lt;');
    s := AnsiReplaceStr(s, '>', '&gt;');
    result := s;
end;

{ TCardNumber }

class operator TCardNumber.implicit(Value: TCardNumber): String;
begin
  Result:= Value.Num;
end;

class operator TCardNumber.implicit(Value: String): TCardNumber;
var
  S: String;
  X: Integer;
  C: Char;
begin
  S:= Value;
  for X := Length(S) downto 1 do begin
    C:= S[X];
    if not CharInSet(C, ['0'..'9']) then
      Delete(S, X, 1);
  end;
  Result.Num:= S;
end;

function TCardNumber.IsValid: Boolean;
var
  S: String;
  C: Char;
  CheckSum: string;
  I, J: Integer;
  function ReverseStr(const Str: string): string;
  var
    I, Len: Integer;
  begin
    Len := Length(Str);
    SetLength(Result, Len);
    for I := 1 to Len do
      Result[I] := Str[Succ(Len-I)];
  end;
begin
  Result:= True;
  S:= Num;

  //Strip characters and validate numeric characters
  for I := Length(S) downto 1 do begin //From end to beginning
    C:= S[I];
    if CharInSet(C, ['-',' ']) then begin
      Delete(S, I, 1);
    end else
    if (not CharInSet(C, ['0'..'9'])) then begin
      Result:= False;
      Break;
    end;
  end;

  //Validate Length
  if Result then
    Result:= Length(S) in [13..19];
    //Result:= Length(S) in [15..16];
  //Cayan says range is 13 to 19 characters

  //Check first digit
  if Result then begin
    C:= S[1];
    //3 = American Express
    //4 = Visa
    //5 = MasterCard
    //6 = Discover
    //TODO: Detect gift cards...
    Result:= CharInSet(C, ['3'..'6']);
  end;

  //Validate Checksum
  //http://www.delphicode.co.uk/is-credit-card-number-valid/
  if Result then begin
    S := ReverseStr(S);
    CheckSum := '';
      for I := 1 to Length(S) do
      if Odd(I) then
        CheckSum := CheckSum + S[I]
      else
        CheckSum := CheckSum + IntToStr(StrToInt(S[I]) * 2);
    J := 0;
    for I := 1 to Length(CheckSum) do
      J := J + StrToInt(CheckSum[I]);
    Result := (J mod 10) = 0;
  end;
end;

function TCardNumber.CardType: TCardType;
var
  I: Integer;
begin
  Result:= TCardType.ctUnknown;
  if IsValid then begin
    I:= StrToIntDef(Num[1], 0);
    case I of
      3: Result:= ctAmex;
      4: Result:= ctVisa;
      5: Result:= ctMasterCard;
      6: Result:= ctDiscover;
    end;
  end;
end;

function TCardNumber.GetStr(const Delim: String): String;
var
  S: String;
begin
  S:= Num;
  if (Delim <> '') and (IsValid) then begin
    if CardType = ctAmex then begin
      //4 - 6 - 5
      //xxxx-xxxxxx-xxxxx (15 --> 17)
      Insert(Delim, S, 5);
      Insert(Delim, S, 12);
    end else begin
      //4 - 4 - 4 - 4
      //xxxx-xxxx-xxxx-xxxx (16 --> 19)
      Insert(Delim, S, 5);
      Insert(Delim, S, 10);
      Insert(Delim, S, 15);
    end;
  end;
  Result:= S;
end;

function TCardNumber.Masked: String;
begin
  Result:= Trim(Num);
  Result:= Copy(Result, Length(Result)-3, 4);
  Result:= '************'+Result;
end;

{ TExpirationDate }

class operator TExpirationDate.implicit(Value: TExpirationDate): String;
begin
  Result:= IntToStrLen(Value.Month, 2) + IntToStrLen(Value.Year, 2);
end;

class operator TExpirationDate.implicit(Value: String): TExpirationDate;
var
  S: String;
begin
  //Parse out value
  S:= Trim(Value);
  S:= StringReplace(S, '/', '', [rfReplaceAll]);
  S:= StringReplace(S, '\', '', [rfReplaceAll]);
  S:= StringReplace(S, '-', '', [rfReplaceAll]);
  S:= StringReplace(S, ' ', '', [rfReplaceAll]);
  if Length(S) = 3 then
    S:= '0'+S;
  Result.Month:= StrToIntDef(Copy(S, 1, 2), 0);
  Result.Year:= StrToIntDef(Copy(S, 3, 2), 0);
end;

function TExpirationDate.GetStr(const Reverse, Slash: Boolean): String;
var
  D: String;
begin
  D:= IfThen(Slash, '/', '');
  if Reverse then
    Result:= IntToStrLen(Year, 2) + D + IntToStrLen(Month, 2)
  else
    Result:= IntToStrLen(Month, 2) + D + IntToStrLen(Year, 2);
end;

function TExpirationDate.Expired: Boolean;
var
  S: String;
  D: TDateTime;
begin
  S:= IntToStr(Month) + '/1/' + IntToStr(Year);
  D:= StrToDateTimeDef(S, 0);
  D:= DateUtils.EndOfTheMonth(D);
  D:= DateUtils.EndOfTheDay(D);
  Result:= Now >= D;
end;

function TExpirationDate.Expiring: Boolean;
var
  D: TDateTime;
begin
  D:= StrToDateTimeDef(IntToStr(Year) + '/' + IntToStr(Month) + '/1', 0);
  D:= DateUtils.StartOfTheMonth(D);
  D:= DateUtils.StartOfTheDay(D);
  Result:= Now >= D;
end;

{ TCardInfo }

class operator TCardInfo.implicit(Value: TCardInfo): String;
  procedure A(const S: String);
  begin
    Result:= Result + S + ';';
  end;
begin
  //Serialize card information
  Result:= '';
  A(Value.Num);
  A(Value.Exp);
  A(Value.Appr);
  A(Value.CVC);
  A(Value.BillStreet);
  A(Value.BillZip);
end;

class operator TCardInfo.implicit(Value: String): TCardInfo;
var
  S: String;
  function N: String;
  begin
    Result:= Copy(S, 1, Pos(';', S)-1);
    Delete(S, 1, Pos(';', S));
  end;
begin
  //Parse card information
  S:= Value;
  Result.Num:= N;
  Result.Exp:= N;
  Result.Appr:= N;
  Result.CVC:= N;
  Result.BillStreet:= N;
  Result.BillZip:= N;
end;

{ TParamList }

constructor TParamList.Create;
begin
  FItems:= TStringList.Create;
end;

destructor TParamList.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TParamList.Clear;
begin
  FItems.Clear;
end;

function TParamList.Count: Integer;
begin
  Result:= FItems.Count;
end;

function TParamList.Name(const Index: Integer): String;
begin
  Result:= FItems.Names[Index];
end;

function TParamList.Value(const Index: Integer): String;
begin
  Result:= FItems.Values[FItems.Names[Index]];
end;

function TParamList.GetParam(const Name: String): String;
begin
  Result:= FItems.Values[Name];
end;

procedure TParamList.SetParam(const Name, Value: String);
begin
  FItems.Values[Name]:= Value;
end;

function TParamList.GetParamStr: String;
var
  X: Integer;
begin
  Result:= '';
  for X := 0 to FItems.Count-1 do begin
    if Result <> '' then Result:= Result + '&';
    Result:= Result + FItems.Names[X] + '=';
    Result:= Result + FItems.Values[FItems.Names[X]];  // UriEncode(FItems.Values[FItems.Names[X]]);
  end;
end;

procedure TParamList.SetParamStr(const Value: String);
var
  S, N, V: String;
  P: Integer;
begin
  Clear;
  S:= Value + '&';
  while S <> '' do begin
    P:= Pos('=', S);
    N:= Copy(S, 1, P-1);
    Delete(S, 1, P);
    P:= Pos('&', S);
    V:= Copy(S, 1, P-1);
    Delete(S, 1, P);
    if N <> '' then
      FItems.Values[N]:= V; // UriDecode(V);
  end;
end;

{ TCombinedData }

constructor TCombinedData.Create;
begin
  FXmlDoc:= nil;
  FXml:= nil;
  FObj:= nil;

  FItems:= TInterfaceList.Create;
end;

destructor TCombinedData.Destroy;
begin
  FreeAndNil(FItems);

  FObj:= nil;
  FXml:= nil;
  FXmlDoc:= nil;
  inherited;
end;

procedure TCombinedData.SetRootElement(const Value: String);
begin
  FRootElement := Value;
end;

procedure TCombinedData.Start(AFormat: TDataFormat);
begin
  //Start new root node...
  FFormat:= AFormat;
  case FFormat of
    efXML: begin
      FXmlDoc:= NewXmlDocument;
      FXml:= FXmlDoc.AddChild(FRootElement);
      FCurXml:= FXml;
      FItems.Add(FCurXml);
    end;
    efJSON: begin
      FObj:= SO;
      FCurObj:= FObj;
      FItems.Add(FCurObj);
    end;
  end;
end;

procedure TCombinedData.AddStr(const Name, Val: String);
var
  N: IXmlNode;
begin
  case FFormat of
    efXML: begin
      N:= FCurXml.AddChild(Name);
      N.Text:= Val;
    end;
    efJSON: begin
      FCurObj.S[Name]:= Val;
    end;
  end;
end;

function TCombinedData.AsJSON: String;
begin
  Result:= FObj.AsJSON(True);
end;

function TCombinedData.AsString: String;
begin
  case FFormat of
    efXML: Result:= AsXML;
    efJSON: Result:= AsJSON;
  end;
end;

function TCombinedData.AsXML: String;
begin
  Result:= FXmlDoc.DocumentElement.XML;
end;

procedure TCombinedData.AddCur(const Name: String; const Val: Currency);
var
  N: IXmlNode;
begin
  case FFormat of
    efXML: begin
      N:= FCurXml.AddChild(Name);
      N.Text:= CurrToStr(Val);
    end;
    efJSON: begin
      FCurObj.F[Name]:= Val;
    end;
  end;
end;

procedure TCombinedData.AddInt(const Name: String; const Val: Integer);
var
  N: IXmlNode;
begin
  case FFormat of
    efXML: begin
      N:= FCurXml.AddChild(Name);
      N.Text:= IntToStr(Val);
    end;
    efJSON: begin
      FCurObj.I[Name]:= Val;
    end;
  end;
end;

procedure TCombinedData.AddObject(const Name: String);
begin
  case FFormat of
    efXML: begin
      FItems.Add(FCurXml);
      FCurXml:= FCurXml.AddChild(Name);
    end;
    efJSON: begin
      FItems.Add(FCurObj);
      FCurObj.O[Name]:= SO;
      FCurObj:= FCurObj.O[Name];
    end;
  end;
end;

procedure TCombinedData.DoneObject;
begin
  case FFormat of
    efXML: begin
      FCurXml:= IXmlNode(FItems[FItems.Count-1]);
      FItems.Delete(FItems.Count-1);
    end;
    efJSON: begin
      FCurObj:= ISuperObject(FItems[FItems.Count-1]);
      FItems.Delete(FItems.Count-1);
    end;
  end;
end;

end.
