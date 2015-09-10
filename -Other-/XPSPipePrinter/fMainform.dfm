object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'XPS print server'
  ClientHeight = 516
  ClientWidth = 738
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
  object Splitter1: TSplitter
    Left = 0
    Top = 349
    Width = 738
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  inline framPipePrinter1: TframPipePrinter
    Left = 0
    Top = 30
    Width = 738
    Height = 38
    Align = alTop
    TabOrder = 0
    inherited btnDelete: TButton
      Visible = False
      OnClick = framPipePrinter1btnDeleteClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 738
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnPipe: TButton
      Left = 8
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Add pipe'
      TabOrder = 0
      OnClick = btnPipeClick
    end
    object btnLoad: TButton
      Left = 118
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Reload'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object btnSave: TButton
      Left = 196
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 2
      OnClick = btnSaveClick
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 68
    Width = 738
    Height = 281
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 2
  end
  object Memo1: TMemo
    Left = 0
    Top = 352
    Width = 738
    Height = 164
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
end
