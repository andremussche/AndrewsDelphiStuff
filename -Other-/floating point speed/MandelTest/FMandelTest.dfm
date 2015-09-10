object Form20: TForm20
  Left = 290
  Top = 107
  Caption = 'Mandelbrot test'
  ClientHeight = 529
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Image1: TImage
    Left = 8
    Top = 39
    Width = 480
    Height = 480
    OnMouseDown = Image1MouseDown
    OnMouseMove = Image1MouseMove
    OnMouseUp = Image1MouseUp
  end
  object Shape1: TShape
    Left = 264
    Top = 16
    Width = 105
    Height = 97
    Brush.Style = bsClear
    Pen.Color = 4325290
    Visible = False
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 0
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 105
    Top = 12
    Width = 144
    Height = 17
    Caption = 'SSE enhanced version'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
end
