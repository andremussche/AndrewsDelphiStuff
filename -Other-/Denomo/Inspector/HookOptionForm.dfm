object FormHookOption: TFormHookOption
  Left = 240
  Top = 137
  BorderStyle = bsDialog
  Caption = 'Host Options'
  ClientHeight = 174
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxMainFlags: TGroupBox
    Left = 0
    Top = 0
    Width = 352
    Height = 89
    Align = alTop
    Caption = 'Flags'
    TabOrder = 0
  end
  object BtnOK: TButton
    Left = 192
    Top = 138
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object BtnCancel: TButton
    Left = 272
    Top = 138
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    Default = True
    ModalResult = 2
    TabOrder = 2
  end
end
