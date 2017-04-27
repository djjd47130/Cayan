unit uVeriFoneMX915;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMX.Controls.Presentation,
  uGeniusEmulatorMain, FMX.ListBox;

type



  TGeniusEmulatorFormClass = class of TForm;


  TfrmVeriFoneMX915 = class(TForm)
    imgDevice: TImageControl;
    imgSwipe1: TImageControl;
    imgSwipe2: TImageControl;
    imgSwipe3: TImageControl;
    tmrSwipe: TTimer;
    imgSwipeUnlit: TImageControl;
    imgSwipeLit: TImageControl;
    lKeypad: TScaledLayout;
    GridPanelLayout1: TGridPanelLayout;
    GridPanelLayout2: TGridPanelLayout;
    GridPanelLayout3: TGridPanelLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Image4: TImage;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Image5: TImage;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Image6: TImage;
    lScreen: TLayout;
    Panel1: TPanel;
    cboSkin: TComboBox;
    pBottom: TPanel;
    Button14: TButton;
    Image1: TImage;
    Button15: TButton;
    Image2: TImage;
    Button16: TButton;
    Image3: TImage;
    Button17: TButton;
    Image7: TImage;
    procedure tmrSwipeTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cboSkinChange(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure KeypadButtonClick(Sender: TObject);
  private
    FSwipeLED: Boolean;
    FSwipePos: Integer;
    FForm: TfrmGeniusEmulatorMain;
    procedure SetSwipeLED(const Value: Boolean);
  public
    property SwipeLED: Boolean read FSwipeLED write SetSwipeLED;
  end;

var
  frmVeriFoneMX915: TfrmVeriFoneMX915;

implementation

{$R *.fmx}

uses
  uDM;

{ TfrmVeriFoneMX915 }

procedure TfrmVeriFoneMX915.Button14Click(Sender: TObject);
begin
  FForm.Emulator.PhoneContacted;
end;

procedure TfrmVeriFoneMX915.Button15Click(Sender: TObject);
begin
  FForm.Emulator.PhoneScanned;
end;

procedure TfrmVeriFoneMX915.Button16Click(Sender: TObject);
begin
  FForm.Emulator.CardSwiped;
end;

procedure TfrmVeriFoneMX915.Button17Click(Sender: TObject);
begin
  FForm.Emulator.CardInserted;
end;

procedure TfrmVeriFoneMX915.cboSkinChange(Sender: TObject);
begin
  lKeypad.Size.Width:= 242;
  lKeypad.Size.Height:= 203;

  case cboSkin.ItemIndex of
    0: begin
      //No Skin
      imgDevice.Visible:= False;
      imgSwipe1.Visible:= False;
      imgSwipe2.Visible:= False;
      imgSwipe3.Visible:= False;

      pBottom.Visible:= True;
      pBottom.Height:= lKeypad.Height + 10;
      lKeypad.Parent:= pBottom;
      lKeypad.Align:= TAlignLayout.Center;

      lScreen.Align:= TAlignLayout.Client;

      FForm.Container.Align:= TAlignLayout.Fit;

      Width:= 580;
      Height:= 665;
    end;
    1: begin
      //MX915
      imgDevice.Visible:= True;
      imgSwipe1.Visible:= True;
      imgSwipe2.Visible:= True;
      imgSwipe3.Visible:= True;
      pBottom.Visible:= False;

      lKeypad.Parent:= Self;
      lKeypad.Align:= TAlignLayout.None;
      lKeypad.Position.Point:= Point(143, 374);

      lScreen.Align:= TAlignLayout.None;
      lScreen.Position.Point:= Point(98, 106);
      lScreen.Size.Width:= 330;
      lScreen.Size.Height:= 187;

      FForm.Container.Align:= TAlignLayout.Client;

      Width:= 580;
      Height:= 665;

    end;
    2: begin
      //MX925
      imgDevice.Visible:= True;
      imgSwipe1.Visible:= True;
      imgSwipe2.Visible:= True;
      imgSwipe3.Visible:= True;
      pBottom.Visible:= False;

      lKeypad.Parent:= Self;
      lKeypad.Align:= TAlignLayout.None;
      lKeypad.Position.Point:= Point(143, 374);

      lScreen.Align:= TAlignLayout.None;
      lScreen.Position.Point:= Point(42, 82);
      lScreen.Size.Width:= 439;
      lScreen.Size.Height:= 255;

      FForm.Container.Align:= TAlignLayout.Client;

      Width:= 580;
      Height:= 665;

    end;
    3: begin
      //Handheld
      imgDevice.Visible:= False;
      imgSwipe1.Visible:= False;
      imgSwipe2.Visible:= False;
      imgSwipe3.Visible:= False;

      pBottom.Visible:= True;
      pBottom.Height:= lKeypad.Height + 10;
      lKeypad.Parent:= pBottom;
      lKeypad.Align:= TAlignLayout.Center;

      lKeypad.Parent:= pBottom;
      lKeypad.Align:= TAlignLayout.Center;

      FForm.Container.Align:= TAlignLayout.Client;

      lScreen.Align:= TAlignLayout.None;

    end;
  end;
end;

procedure TfrmVeriFoneMX915.FormCreate(Sender: TObject);
begin
  FForm:= TfrmGeniusEmulatorMain.Create(Self);
  FForm.Container.Parent:= lScreen;
  FForm.Container.Align:= TAlignLayout.Client;
  Self.StyleBook:= FForm.StyleBook;

  SwipeLED:= True;

  cboSkinChange(cboSkin);

end;

procedure TfrmVeriFoneMX915.KeypadButtonClick(Sender: TObject);
begin
  //
end;

procedure TfrmVeriFoneMX915.SetSwipeLED(const Value: Boolean);
begin
  FSwipeLED := Value;
end;

procedure TfrmVeriFoneMX915.tmrSwipeTimer(Sender: TObject);
  procedure SetLed(const One, Two, Three: Boolean);
  begin
    if One then
      imgSwipe1.Bitmap.Assign(imgSwipeLit.Bitmap)
    else
      imgSwipe1.Bitmap.Assign(imgSwipeUnlit.Bitmap);
    if Two then
      imgSwipe2.Bitmap.Assign(imgSwipeLit.Bitmap)
    else
      imgSwipe2.Bitmap.Assign(imgSwipeUnlit.Bitmap);
    if Three then
      imgSwipe3.Bitmap.Assign(imgSwipeLit.Bitmap)
    else
      imgSwipe3.Bitmap.Assign(imgSwipeUnlit.Bitmap);
  end;
begin
  FSwipeLED:= Self.FForm.Emulator.CanSwipeInScreen;
  if FSwipeLED then begin
    if FSwipePos = 5 then FSwipePos:= 0 else FSwipePos:= FSwipePos + 1;
    case FSwipePos of
      0: SetLed(False, False, False);
      1: SetLed(True, False, False);
      2: SetLed(True, True, False);
      3: SetLed(True, True, True);
      4: SetLed(False, True, True);
      5: SetLed(False, False, True);
    end;
  end else begin
    SetLed(False, False, False);
  end;
end;

end.
