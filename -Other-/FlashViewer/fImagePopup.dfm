object frmImagePopup: TfrmImagePopup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 222
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
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 100
    Height = 100
    AutoSize = True
    Center = True
    Proportional = True
    Transparent = True
    OnClick = Image1Click
    OnDblClick = Image1DblClick
  end
end
