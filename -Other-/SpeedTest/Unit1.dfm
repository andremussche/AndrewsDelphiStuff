object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Speed test'
  ClientHeight = 454
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnFill1: TButton
    Left = 8
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Memo fill 1'
    TabOrder = 0
    OnClick = btnFill1Click
  end
  object Memo1: TMemo
    Left = 168
    Top = 8
    Width = 246
    Height = 438
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object btnFill2: TButton
    Left = 8
    Top = 39
    Width = 145
    Height = 25
    Caption = 'Memo fill 2'
    TabOrder = 2
    OnClick = btnFill2Click
  end
  object btnFill3: TButton
    Left = 8
    Top = 70
    Width = 145
    Height = 25
    Caption = 'Memo fill 3'
    TabOrder = 3
    OnClick = btnFill3Click
  end
  object btnSearch1: TButton
    Left = 8
    Top = 120
    Width = 145
    Height = 25
    Caption = 'Memo search 1'
    TabOrder = 4
    OnClick = btnSearch1Click
  end
  object btnSearch2: TButton
    Left = 8
    Top = 151
    Width = 145
    Height = 25
    Caption = 'Memo search 2'
    TabOrder = 5
    OnClick = btnSearch2Click
  end
  object btnSearch3: TButton
    Left = 8
    Top = 182
    Width = 145
    Height = 25
    Caption = 'Memo search 3'
    TabOrder = 6
    OnClick = btnSearch3Click
  end
  object btnSearch4: TButton
    Left = 8
    Top = 213
    Width = 145
    Height = 25
    Caption = 'Memo search 4'
    TabOrder = 7
    OnClick = btnSearch4Click
  end
  object btnWait1: TButton
    Left = 8
    Top = 272
    Width = 145
    Height = 25
    Caption = 'Wait 1'
    TabOrder = 8
    OnClick = btnWait1Click
  end
  object btnWait2: TButton
    Left = 8
    Top = 303
    Width = 145
    Height = 25
    Caption = 'Wait 2'
    TabOrder = 9
    OnClick = btnWait2Click
  end
  object btnJpegTest1: TButton
    Left = 192
    Top = 360
    Width = 145
    Height = 25
    Caption = 'JPEG test 1'
    TabOrder = 10
    Visible = False
    OnClick = btnJpegTest1Click
  end
  object btnJpegTest2: TButton
    Left = 192
    Top = 391
    Width = 145
    Height = 25
    Caption = 'JPEG test 2'
    TabOrder = 11
    Visible = False
    OnClick = btnJpegTest2Click
  end
end
