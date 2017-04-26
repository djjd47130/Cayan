unit Cayan;

interface

uses
  System.Classes, System.SysUtils;

type
  ///  <summary>
  ///    Encapsulates a general connection and authentication with Cayan systems.
  ///  </summary>
  ///  <remarks>
  ///    This does not actually implement anything, but is attached to other
  ///    specific components for further use. Each particular solution, such
  ///    as Genius CED, will require an instance of this in its "Cayan" property.
  ///  </remarks>
  TCayan = class(TComponent)
  private
    FMerchantSiteId: String;
    FMerchantKey: String;
    FMerchantName: String;
    FClerkID: String;
    FSoftwareVersion: String;
    FSoftwareName: String;
    FStationID: String;
    FDba: String;
    FTestMode: Boolean;
    procedure SetMerchantKey(const Value: String);
    procedure SetMerchantName(const Value: String);
    procedure SetMerchantSiteID(const Value: String);
    procedure SetClerkID(const Value: String);
    procedure SetDba(const Value: String);
    procedure SetSoftwareName(const Value: String);
    procedure SetSoftwareVersion(const Value: String);
    procedure SetStationID(const Value: String);
    procedure SetTestMode(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MerchantName: String read FMerchantName write SetMerchantName;
    property MerchantSiteId: String read FMerchantSiteId write SetMerchantSiteID;
    property MerchantKey: String read FMerchantKey write SetMerchantKey;
    property Dba: String read FDba write SetDba;
    property ClerkID: String read FClerkID write SetClerkID;
    property StationID: String read FStationID write SetStationID;
    property SoftwareName: String read FSoftwareName write SetSoftwareName;
    property SoftwareVersion: String read FSoftwareVersion write SetSoftwareVersion;
    property TestMode: Boolean read FTestMode write SetTestMode;
  end;

  TCayanBaseComponent = class(TComponent)
  private
    FCayan: TCayan;
    procedure SetCayan(const Value: TCayan);
  published
    property Cayan: TCayan read FCayan write SetCayan;
  end;

implementation

{ TCayan }

constructor TCayan.Create(AOwner: TComponent);
begin
  inherited;
  FMerchantName:= '';
  FMerchantSiteID:= '';
  FMerchantKey:= '';
  FDba:= 'My Company Name';
  FClerkID:= 'My Name';
  FStationID:= '1';
  FSoftwareName:= 'My Software';
  FSoftwareVersion:= '1.0';
  FTestMode:= False;
end;

destructor TCayan.Destroy;
begin

  inherited;
end;

procedure TCayan.SetClerkID(const Value: String);
begin
  FClerkID := Value;
end;

procedure TCayan.SetDba(const Value: String);
begin
  FDba := Value;
end;

procedure TCayan.SetMerchantKey(const Value: String);
begin
  FMerchantKey := Value;
end;

procedure TCayan.SetMerchantName(const Value: String);
begin
  FMerchantName := Value;
end;

procedure TCayan.SetMerchantSiteID(const Value: String);
begin
  FMerchantSiteId := Value;
end;

procedure TCayan.SetSoftwareName(const Value: String);
begin
  FSoftwareName := Value;
end;

procedure TCayan.SetSoftwareVersion(const Value: String);
begin
  FSoftwareVersion := Value;
end;

procedure TCayan.SetStationID(const Value: String);
begin
  FStationID := Value;
end;

procedure TCayan.SetTestMode(const Value: Boolean);
begin
  FTestMode:= Value;
end;

{ TCayanBaseComponent }

procedure TCayanBaseComponent.SetCayan(const Value: TCayan);
begin
  FCayan := Value;
end;

end.
