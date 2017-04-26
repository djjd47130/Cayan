unit Cayan.Genius.Agreement;

interface

uses
  System.Classes, System.SysUtils,
  Cayan.Common,
  Cayan.Genius,
  Cayan.Genius.Intf,
  Cayan.Genius.Impl,
  XmlDoc, XmlIntf, XmlDom,
  IdURI;

type
  TCayanGeniusAgreement = class;

  TCayanGeniusAgreementResponseEvent = procedure(Sender: TObject;
    const Response: IGeniusAgreementResponse) of object;

  TCayanGeniusAgreement = class(TComponent)
  private
    FGenius: TCayanGenius;
    FThread: TGeniusRequestThread;
    FOnAgreement: TCayanGeniusAgreementResponseEvent;
    FDeclineLabel: String;
    FAcceptLabel: String;
    FTitle: String;
    FAgreementText: TStringList;
    procedure SetGenius(const Value: TCayanGenius);
    procedure ThreadResponse(Sender: TObject; const AURL: String;
      const Response: String);
    function GetAgreementText: TStrings;
    procedure SetAcceptLabel(const Value: String);
    procedure SetAgreementText(const Value: TStrings);
    procedure SetDeclineLabel(const Value: String);
    procedure SetTitle(const Value: String);
  protected
    procedure DoOnAgreement(const Response: IGeniusAgreementResponse); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendRequest;
    procedure CancelRequest;
  published
    property Genius: TCayanGenius read FGenius write SetGenius;
    property Title: String read FTitle write SetTitle;
    property AgreementText: TStrings read GetAgreementText write SetAgreementText;
    property AcceptLabel: String read FAcceptLabel write SetAcceptLabel;
    property DeclineLabel: String read FDeclineLabel write SetDeclineLabel;

    property OnAgreement: TCayanGeniusAgreementResponseEvent
      read FOnAgreement write FOnAgreement;
  end;

implementation

{ TCayanGeniusAgreement }

procedure TCayanGeniusAgreement.CancelRequest;
begin
  FGenius.Genius.CancelTransaction;
end;

constructor TCayanGeniusAgreement.Create(AOwner: TComponent);
begin
  inherited;
  FAgreementText:= TStringList.Create;
end;

destructor TCayanGeniusAgreement.Destroy;
begin
  FreeAndNil(FAgreementText);
  inherited;
end;

procedure TCayanGeniusAgreement.DoOnAgreement(
  const Response: IGeniusAgreementResponse);
begin
  if Assigned(FOnAgreement) then
    FOnAgreement(Self, Response);
end;

function TCayanGeniusAgreement.GetAgreementText: TStrings;
begin
  Result:= TStrings(FAgreementText);
end;

procedure TCayanGeniusAgreement.SendRequest;
var
  URL: String;
begin

  //FGenius.Genius.IsInAgreement:= True;
  URL:= Format('http://%s:%d/v1/pos?Action=GetAgreement&RequestID=1&'+
    'Title=%s&AgreementText=%s&AcceptLabel=%s&DeclineLabel=%s&Format=XML',
    [FGenius.Device.DeviceAddress, FGenius.Device.DevicePort,
    FTitle, FAgreementText.Text, FAcceptLabel, FDeclineLabel]);

  URL:= TIdURI.URLEncode(URL);

  FThread:= TGeniusRequestThread.Create(FGenius.Genius, URL, ThreadResponse);
  FThread.Start;

end;

procedure TCayanGeniusAgreement.SetAcceptLabel(const Value: String);
begin
  FAcceptLabel := Value;
end;

procedure TCayanGeniusAgreement.SetAgreementText(const Value: TStrings);
begin
  FAgreementText.Assign(Value);
end;

procedure TCayanGeniusAgreement.SetDeclineLabel(const Value: String);
begin
  FDeclineLabel := Value;
end;

procedure TCayanGeniusAgreement.SetGenius(const Value: TCayanGenius);
begin
  FGenius := Value;
end;

procedure TCayanGeniusAgreement.SetTitle(const Value: String);
begin
  FTitle := Value;
end;

procedure TCayanGeniusAgreement.ThreadResponse(Sender: TObject; const AURL,
  Response: String);
var
  Result: IGeniusAgreementResponse;
  XML: IXMLDocument;
begin
  Result:= TGeniusAgreementResponse.Create;
  //FGenius.Genius.IsInAgreement:= False;
  XML:= NewXMLDocument;
  XML.LoadFromXML(Response);
  ParseAgreementResponse(XML, Result);
  DoOnAgreement(Result);
end;

end.
