object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 686
  ClientWidth = 855
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 544
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 24
    Top = 8
    Width = 479
    Height = 193
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object mmoGen: TMemo
    Left = 24
    Top = 238
    Width = 479
    Height = 193
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Button2: TButton
    Left = 24
    Top = 207
    Width = 75
    Height = 25
    Caption = 'Generate'
    TabOrder = 3
    OnClick = Button2Click
  end
  object mmoExports: TMemo
    Left = 24
    Top = 437
    Width = 479
    Height = 193
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 4
  end
end
