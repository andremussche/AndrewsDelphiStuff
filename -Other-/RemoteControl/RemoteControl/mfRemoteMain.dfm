object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'Form7'
  ClientHeight = 361
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnTestModal: TButton
    Left = 8
    Top = 88
    Width = 137
    Height = 25
    Caption = 'btnTestModal'
    TabOrder = 0
    OnClick = btnTestModalClick
  end
  object btnTestFloating: TButton
    Left = 160
    Top = 88
    Width = 137
    Height = 25
    Caption = 'btnTestFloating'
    TabOrder = 1
    OnClick = btnTestFloatingClick
  end
  object btnSetText: TButton
    Left = 96
    Top = 135
    Width = 113
    Height = 25
    Caption = 'btnSetText'
    TabOrder = 2
    OnClick = btnSetTextClick
  end
  object btnPushButton: TButton
    Left = 96
    Top = 166
    Width = 113
    Height = 25
    Caption = 'btnPushButton'
    TabOrder = 3
    OnClick = btnPushButtonClick
  end
  object btnPressOk: TButton
    Left = 96
    Top = 197
    Width = 113
    Height = 25
    Caption = 'btnPressOk'
    TabOrder = 4
    OnClick = btnPressOkClick
  end
  object btnStartRemoteApp: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'btnStartRemoteApp'
    TabOrder = 5
    OnClick = btnStartRemoteAppClick
  end
  object btnStopRemoteApp: TButton
    Left = 8
    Top = 240
    Width = 137
    Height = 25
    Caption = 'btnStopRemoteApp'
    TabOrder = 6
    OnClick = btnStopRemoteAppClick
  end
  object Button1: TButton
    Left = 8
    Top = 328
    Width = 137
    Height = 25
    Caption = 'Single click test'
    TabOrder = 7
    OnClick = Button1Click
  end
  object btnConnect: TButton
    Left = 8
    Top = 39
    Width = 137
    Height = 25
    Caption = 'btnConnect'
    TabOrder = 8
    OnClick = btnConnectClick
  end
end
