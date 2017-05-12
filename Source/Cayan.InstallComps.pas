unit Cayan.InstallComps;

interface

uses
  System.Classes, System.SysUtils,
  Cayan,
  Cayan.Genius,
  Cayan.Genius.Transactions,
  Cayan.Genius.Agreement,
  Cayan.Genius.LineItems,
  Cayan.POS.Server,
  Cayan.POS,
  Cayan.Genius.Emulator,
  Cayan.MerchantWare,
  Cayan.Vault;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Cayan', [TCayan, TCayanGenius, TCayanGeniusTransaction,
    TCayanGeniusAgreement, TCayanGeniusLineItems,
    TCayanMerchantWare, TCayanVault,
    TCayanPOSServer, TCayanPOS,
    TCayanGeniusEmulator]);
end;

end.
