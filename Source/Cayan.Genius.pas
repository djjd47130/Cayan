unit Cayan.Genius;

interface

uses
  System.Classes, System.SysUtils,
  Cayan,
  Cayan.Genius.Intf,
  Cayan.Genius.Impl,
  Cayan.MWv4.Intf,
  Cayan.MWv4.Impl,
  Cayan.Common;

type
  TCayanGeniusDevice = class;
  TCayanGenius = class;

  TCayanGeniusDevice = class(TPersistent)
  private
    FOwner: TCayanGenius;
    function GetDeviceAddress: String;
    function GetDevicePort: Integer;
    function GetDeviceProtocol: TGeniusProtocol;
    function GetDeviceTimeout: Integer;
    function GetDeviceVersion: TGeniusDeviceVersion;
    procedure SetDeviceAddress(const Value: String);
    procedure SetDevicePort(const Value: Integer);
    procedure SetDeviceProtocol(const Value: TGeniusProtocol);
    procedure SetDeviceTimeout(const Value: Integer);
    procedure SetDeviceVersion(const Value: TGeniusDeviceVersion);
    function GetMonitoring: Boolean;
    procedure SetMonitoring(const Value: Boolean);
  public
    constructor Create(AOwner: TCayanGenius);
    destructor Destroy; override;
  published
    property DeviceAddress: String read GetDeviceAddress write SetDeviceAddress;
    property DevicePort: Integer read GetDevicePort write SetDevicePort;
    property DeviceProtocol: TGeniusProtocol read GetDeviceProtocol write SetDeviceProtocol;
    property DeviceVersion: TGeniusDeviceVersion read GetDeviceVersion write SetDeviceVersion;
    property DeviceTimeout: Integer read GetDeviceTimeout write SetDeviceTimeout;
    property Monitoring: Boolean read GetMonitoring write SetMonitoring;
  end;

  ///  <summary>
  ///    Encapsulates the full transaction process using a Genius CED
  ///    payment terminal provided by Cayan.
  ///  </summary>
  TCayanGenius = class(TCayanBaseComponent)
  private
    FDevice: TCayanGeniusDevice;
    FMerchantWare: TMerchantWare;
    FGenius: TGenius;
    FForceDuplicate: Boolean;
    FOnDeviceStatus: TGeniusStatusResponseEvent;
    FOnCancel: TNotifyEvent;
    procedure SetForceDuplicate(const Value: Boolean);
    procedure PrepareGenius;
    procedure SetDevice(const Value: TCayanGeniusDevice);
    procedure GeniusDeviceStatus(Sender: IGenius;
      const Status: IGeniusStatusResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Genius: TGenius;
    function MerchantWare: TMerchantWare;
    function Cancel: IGeniusCancelTransactionResponse;
  published
    property Device: TCayanGeniusDevice read FDevice write SetDevice;
    property ForceDuplicate: Boolean read FForceDuplicate write SetForceDuplicate;

    property OnDeviceStatus: TGeniusStatusResponseEvent
      read FOnDeviceStatus write FOnDeviceStatus;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

implementation

{ TCayanGeniusDevice }

constructor TCayanGeniusDevice.Create(AOwner: TCayanGenius);
begin
  FOwner:= AOwner;
end;

destructor TCayanGeniusDevice.Destroy;
begin

  inherited;
end;

function TCayanGeniusDevice.GetDeviceAddress: String;
begin
  Result:= FOwner.FGenius.DeviceAddress;
end;

function TCayanGeniusDevice.GetDevicePort: Integer;
begin
  Result:= FOwner.FGenius.DevicePort;
end;

function TCayanGeniusDevice.GetDeviceProtocol: TGeniusProtocol;
begin
  Result:= FOwner.FGenius.DeviceProtocol;
end;

function TCayanGeniusDevice.GetDeviceTimeout: Integer;
begin
  Result:= FOwner.FGenius.DeviceTimeout;
end;

function TCayanGeniusDevice.GetDeviceVersion: TGeniusDeviceVersion;
begin
  Result:= FOwner.FGenius.DeviceVersion;
end;

procedure TCayanGeniusDevice.SetDeviceAddress(const Value: String);
begin
  FOwner.FGenius.DeviceAddress:= Value;
end;

procedure TCayanGeniusDevice.SetDevicePort(const Value: Integer);
begin
  FOwner.FGenius.DevicePort:= Value;
end;

procedure TCayanGeniusDevice.SetDeviceProtocol(const Value: TGeniusProtocol);
begin
  FOwner.FGenius.DeviceProtocol:= Value;
end;

procedure TCayanGeniusDevice.SetDeviceTimeout(const Value: Integer);
begin
  FOwner.FGenius.DeviceTimeout:= Value;
end;

procedure TCayanGeniusDevice.SetDeviceVersion(const Value: TGeniusDeviceVersion);
begin
  FOwner.FGenius.DeviceVersion:= Value;
end;

function TCayanGeniusDevice.GetMonitoring: Boolean;
begin
  Result:= FOwner.FGenius.Monitoring;
end;

procedure TCayanGeniusDevice.SetMonitoring(const Value: Boolean);
begin
  FOwner.FGenius.Monitoring:= Value;
end;

{ TCayanGenius }

constructor TCayanGenius.Create(AOwner: TComponent);
begin
  inherited;
  FMerchantWare:= TMerchantWare.Create;
  IMerchantWare(FMerchantWare)._AddRef;
  FGenius:= TGenius.Create(FMerchantWare);
  IGenius(FGenius)._AddRef;
  FGenius.OnStatus:= GeniusDeviceStatus;
  FDevice:= TCayanGeniusDevice.Create(Self);
end;

destructor TCayanGenius.Destroy;
begin
  FDevice.Monitoring:= False;
  IGenius(FGenius)._Release;
  FGenius:= nil;
  IMerchantWare(FMerchantWare)._Release;
  FMerchantWare:= nil;
  FreeAndNil(FDevice);
  inherited;
end;

function TCayanGenius.Cancel: IGeniusCancelTransactionResponse;
begin
  if Assigned(Self.FOnCancel) then
    Self.FOnCancel(Self);
  Result:= FGenius.CancelTransaction;
end;

function TCayanGenius.MerchantWare: TMerchantWare;
begin
  Result:= FMerchantWare;
end;

function TCayanGenius.Genius: TGenius;
begin
  Result:= FGenius;
end;

procedure TCayanGenius.GeniusDeviceStatus(Sender: IGenius;
  const Status: IGeniusStatusResponse);
begin
  if Assigned(Self.FOnDeviceStatus) then
    Self.FOnDeviceStatus(Sender, Status);
end;

procedure TCayanGenius.SetDevice(const Value: TCayanGeniusDevice);
begin
  Self.FDevice.Assign(Value);
end;

procedure TCayanGenius.SetForceDuplicate(const Value: Boolean);
begin
  FForceDuplicate := Value;
end;

procedure TCayanGenius.PrepareGenius;
begin
  Self.FMerchantWare.Name:= Self.Cayan.MerchantName;
  Self.FMerchantWare.SiteId:= Self.Cayan.MerchantSiteId;
  Self.FMerchantWare.Key:= Self.Cayan.MerchantKey;
  Self.FMerchantWare.TestMode:= Self.Cayan.TestMode;
end;

end.
