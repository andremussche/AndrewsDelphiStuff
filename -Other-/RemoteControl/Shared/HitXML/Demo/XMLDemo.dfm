object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Demo'
  ClientHeight = 305
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 428
    Height = 75
    Align = alTop
    TabOrder = 0
    object lbl1: TLabel
      Left = 8
      Top = 7
      Width = 234
      Height = 13
      Caption = 'Hitsoft XML Object Library (HitXML) DEMO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Button1: TButton
      Left = 24
      Top = 36
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 120
      Top = 36
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 75
    Width = 428
    Height = 230
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    ExplicitTop = 59
    ExplicitHeight = 246
    object mmo1: TMemo
      Left = 8
      Top = 6
      Width = 413
      Height = 235
      TabOrder = 0
    end
  end
end
