unit Cayan.MerchantWare;

interface

uses
  System.Classes, System.SysUtils,
  Cayan.Common,
  Cayan,
  Cayan.MWv4.Intf,
  Cayan.MWv4.Impl;

type
  TCayanMerchantWare = class(TCayanBaseComponent)
  private
    FMW: IMerchantWare;
    function GetCheck: IMWCheckTransactions;
    function GetVault: IMWVaultTransactions;
    function GetCredit: IMWCreditTransactions;
    function GetEbt: IMWEbtTransactions;
    function GetGiftCard: IMWGiftCardTransactions;
    function GetLevelUp: IMWLevelUpTransactions;
    function GetReport: IMWReportTransactions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MerchantWare: IMerchantWare;
    property Credit: IMWCreditTransactions read GetCredit;
    property Vault: IMWVaultTransactions read GetVault;
    property Ebt: IMWEbtTransactions read GetEbt;
    property GiftCard: IMWGiftCardTransactions read GetGiftCard;
    property LevelUp: IMWLevelUpTransactions read GetLevelUp;
    property Check: IMWCheckTransactions read GetCheck;
    property Report: IMWReportTransactions read GetReport;
  published

  end;

implementation

{ TCayanMerchantWare }

constructor TCayanMerchantWare.Create(AOwner: TComponent);
begin
  inherited;
  FMW:= TMerchantWare.Create;
  FMW._AddRef;
end;

destructor TCayanMerchantWare.Destroy;
begin
  FMW._Release;
  FMW:= nil;
  inherited;
end;

function TCayanMerchantWare.MerchantWare: IMerchantWare;
begin
  Result:= FMW;
end;

function TCayanMerchantWare.GetCheck: IMWCheckTransactions;
begin
  Result:= FMW.Check;
end;

function TCayanMerchantWare.GetCredit: IMWCreditTransactions;
begin
  Result:= FMW.Credit;
end;

function TCayanMerchantWare.GetEbt: IMWEbtTransactions;
begin
  Result:= FMW.Ebt;
end;

function TCayanMerchantWare.GetGiftCard: IMWGiftCardTransactions;
begin
  Result:= FMW.GiftCard;
end;

function TCayanMerchantWare.GetLevelUp: IMWLevelUpTransactions;
begin
  Result:= FMW.LevelUp;
end;

function TCayanMerchantWare.GetReport: IMWReportTransactions;
begin
  Result:= FMW.Report;
end;

function TCayanMerchantWare.GetVault: IMWVaultTransactions;
begin
  Result:= FMW.Vault;
end;

end.
