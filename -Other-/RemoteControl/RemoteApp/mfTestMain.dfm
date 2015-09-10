object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 337
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
  object btnShowModal: TButton
    Left = 168
    Top = 40
    Width = 177
    Height = 25
    Caption = 'Show modal'
    TabOrder = 0
    OnClick = btnShowModalClick
  end
  object btnShowFloating: TButton
    Left = 168
    Top = 112
    Width = 177
    Height = 25
    Caption = 'Show floating'
    TabOrder = 1
    OnClick = btnShowFloatingClick
  end
end
