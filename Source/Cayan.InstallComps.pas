unit Cayan.InstallComps;

interface

uses
  System.Classes, System.SysUtils,
  Cayan,
  Cayan.Genius,
  Cayan.Genius.Agreement,
  Cayan.Genius.LineItems,
  Cayan.Genius.Emulator;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Cayan', [TCayan, TCayanGenius, TCayanGeniusAgreement,
    TCayanGeniusLineItems, TCayanGeniusEmulator]);
end;

end.
