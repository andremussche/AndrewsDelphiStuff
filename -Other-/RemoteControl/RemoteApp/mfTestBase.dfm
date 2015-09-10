object frmTestBase: TfrmTestBase
  Left = 0
  Top = 0
  Caption = 'frmTestBase'
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
  object Panel1: TPanel
    Left = 0
    Top = 296
    Width = 527
    Height = 41
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 0
    object btnOk: TButton
      Left = 359
      Top = 8
      Width = 75
      Height = 25
      Action = actOk
      Default = True
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 440
      Top = 8
      Width = 75
      Height = 25
      Action = actCancel
      Cancel = True
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    Left = 24
    Top = 8
    object actOk: TAction
      Caption = 'Ok'
      OnExecute = actOkExecute
    end
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
  end
end
