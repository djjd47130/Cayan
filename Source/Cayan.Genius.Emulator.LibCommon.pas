unit Cayan.Genius.Emulator.LibCommon;

interface

uses
  System.SysUtils,
  System.Classes;

type
  TGeniusEmulatorStatus = (gesOffline = 0, gesOnline = 1, gesNeedUpdate = 2, gesError = 3);

  IGeniusEmulator = interface
    procedure Start;
    procedure Stop;
    function Status: TGeniusEmulatorStatus;
  end;

implementation

end.
