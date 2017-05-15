unit Cayan.Vault;

interface

uses
  System.Classes, System.SysUtils,
  Cayan,
  Cayan.Common,
  Cayan.MerchantWare,
  Cayan.MWv4.Intf;

type
  TCayanVault = class;

  TCayanMWVaultInfoEvent = procedure(Sender: TObject; const Info: IMWVaultPaymentInfoResponse) of object;

  TCayanVault = class(TComponent)
  private
    FMerchantWare: TCayanMerchantWare;
    FOnCardInfo: TCayanMWVaultInfoEvent;
    procedure SetMerchantWare(const Value: TCayanMerchantWare);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVault(const Token: String): IMWVaultPaymentInfoResponse;
  published
    property MerchantWare: TCayanMerchantWare read FMerchantWare write SetMerchantWare;

    property OnCardInfo: TCayanMWVaultInfoEvent read FOnCardInfo write FOnCardInfo;
  end;

implementation

{ TCayanVault }

constructor TCayanVault.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TCayanVault.Destroy;
begin

  inherited;
end;

procedure TCayanVault.SetMerchantWare(const Value: TCayanMerchantWare);
begin
  FMerchantWare := Value;
end;

function TCayanVault.GetVault(const Token: String): IMWVaultPaymentInfoResponse;
begin
  Result:= FMerchantWare.MerchantWare.Vault.VaultFindPaymentInfo(Token);
end;

end.
