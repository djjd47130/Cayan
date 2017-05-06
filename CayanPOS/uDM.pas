unit uDM;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, Cayan, Cayan.POS;

type
  TDM = class(TDataModule)
    CayanPOSStyle: TStyleBook;
    Cayan: TCayan;
    POS: TCayanPOS;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  DM: TDM;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  uCayanPOSMain;

end.
