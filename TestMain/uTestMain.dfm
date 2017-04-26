object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Genius Test'
  ClientHeight = 382
  ClientWidth = 308
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    308
    382)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 13
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label2: TLabel
    Left = 16
    Top = 62
    Width = 78
    Height = 13
    Caption = 'Agreement Text'
  end
  object Label3: TLabel
    Left = 16
    Top = 255
    Width = 94
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Decline Button Text'
  end
  object Label4: TLabel
    Left = 168
    Top = 255
    Width = 93
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Accept Button Text'
  end
  object lblResult: TLabel
    Left = 16
    Top = 345
    Width = 189
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'Decline Button Text'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnSendCommand: TBitBtn
    Left = 16
    Top = 314
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Send Command'
    TabOrder = 0
    OnClick = btnSendCommandClick
  end
  object txtDeclineText: TEdit
    Left = 16
    Top = 274
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    MaxLength = 18
    TabOrder = 1
    Text = 'I Decline'
  end
  object txtAcceptText: TEdit
    Left = 168
    Top = 274
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    MaxLength = 18
    TabOrder = 2
    Text = 'I Accept'
  end
  object txtAgreementText: TMemo
    Left = 16
    Top = 81
    Width = 273
    Height = 160
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      
        'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin v' +
        'estibulum tortor id auctor efficitur. Proin ac tincidunt nibh. M' +
        'auris ultricies placerat viverra. Nulla ultricies venenatis veli' +
        't, quis dictum nisl dignissim vitae. Vivamus vitae libero leo. N' +
        'ulla sit amet justo tincidunt purus rutrum imperdiet. Phasellus ' +
        'lacinia in ex eu laoreet. Quisque finibus bibendum scelerisque. ' +
        'Curabitur viverra rutrum orci at imperdiet. Etiam tincidunt cond' +
        'imentum sem et lobortis. Praesent rutrum ut leo ac sagittis. Ves' +
        'tibulum molestie pretium est, vitae ultricies leo sollicitudin e' +
        'u. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusc' +
        'e dictum odio sit amet arcu pharetra vulputate. Sed euismod ante' +
        ' et tortor euismod venenatis.'
      ''
      
        'Sed gravida eleifend lacus, nec sagittis erat accumsan eget. Ut ' +
        'ex nunc, facilisis et rutrum a, tempor eu est. Praesent malesuad' +
        'a vitae arcu quis molestie. Donec convallis leo diam, et vulputa' +
        'te risus rutrum sit amet.')
    MaxLength = 1000
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object txtTitle: TEdit
    Left = 16
    Top = 32
    Width = 273
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 36
    TabOrder = 4
    Text = 'Terms and Conditions'
  end
  object BitBtn2: TBitBtn
    Left = 168
    Top = 314
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel Command'
    TabOrder = 5
    OnClick = BitBtn2Click
  end
  object Genius: TCayanGenius
    Cayan = Cayan
    Device.DeviceAddress = '192.168.0.2'
    Device.DevicePort = 8989
    Device.DeviceProtocol = prHTTP
    Device.DeviceVersion = gdVer1
    Device.DeviceTimeout = 900
    Device.Monitoring = False
    TransactionType = gtSale
    ForceDuplicate = False
    Left = 208
  end
  object Agree: TCayanGeniusAgreement
    Genius = Genius
    Title = 'Agreement'
    AgreementText.Strings = (
      'I agree to the terms')
    AcceptLabel = 'Yes'
    DeclineLabel = 'No'
    OnAgreement = AgreeAgreement
    Left = 256
  end
  object Cayan: TCayan
    MerchantName = 'PSTEST'
    MerchantSiteId = '22222222'
    MerchantKey = '22222-22222-22222-22222-22222'
    Dba = 'My Company Name'
    ClerkID = 'My Name'
    StationID = '1'
    SoftwareName = 'My Software'
    SoftwareVersion = '1.0'
    TestMode = False
    Left = 160
  end
end
