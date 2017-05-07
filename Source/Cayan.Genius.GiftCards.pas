unit Cayan.Genius.GiftCards;

interface

uses
  System.Classes, System.SysUtils,
  Cayan,
  Cayan.Genius,
  Cayan.Genius.Intf;

type
  TCayanGeniusGiftCard = class(TComponent)
  private
    FCardNum: String;
    FGenius: TCayanGenius;
    procedure SetCardNum(const Value: String);
    procedure SetGenius(const Value: TCayanGenius);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Activate: Boolean;
  published
    property Genius: TCayanGenius read FGenius write SetGenius;
    property CardNum: String read FCardNum write SetCardNum;
  end;

implementation

{ TCayanGeniusGiftCard }

constructor TCayanGeniusGiftCard.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TCayanGeniusGiftCard.Destroy;
begin

  inherited;
end;

function TCayanGeniusGiftCard.Activate: Boolean;
begin
  Result:= False;

end;

procedure TCayanGeniusGiftCard.SetCardNum(const Value: String);
begin
  FCardNum := Value;
end;

procedure TCayanGeniusGiftCard.SetGenius(const Value: TCayanGenius);
begin
  FGenius := Value;
end;

end.
