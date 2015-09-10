object frmTransparent: TfrmTransparent
  Left = 0
  Top = 0
  AlphaBlendValue = 150
  Caption = 'frmTransparent'
  ClientHeight = 87
  ClientWidth = 247
  Color = clBtnFace
  TransparentColor = True
  TransparentColorValue = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 32
    Top = 24
    Width = 185
    Height = 41
    Caption = 'Panel1'
    Color = clLime
    ParentBackground = False
    TabOrder = 0
    Visible = False
  end
  object ApplicationEvents1: TApplicationEvents
    OnMessage = ApplicationEvents1Message
    Left = 24
    Top = 8
  end
end
