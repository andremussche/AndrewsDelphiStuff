object framPipePrinter: TframPipePrinter
  Left = 0
  Top = 0
  Width = 743
  Height = 38
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 82
    Height = 13
    Caption = 'Input pipe name:'
  end
  object Label2: TLabel
    Left = 216
    Top = 11
    Width = 73
    Height = 13
    Caption = 'Output printer:'
  end
  object lblPrinted: TLabel
    Left = 592
    Top = 11
    Width = 47
    Height = 13
    Caption = 'Printed: 0'
  end
  object edtPipe: TEdit
    Left = 96
    Top = 8
    Width = 105
    Height = 21
    TabOrder = 0
  end
  object cmbxPrinter: TComboBox
    Left = 297
    Top = 8
    Width = 280
    Height = 21
    Style = csDropDownList
    DropDownCount = 25
    TabOrder = 1
  end
  object btnDelete: TButton
    Left = 671
    Top = 8
    Width = 51
    Height = 21
    Caption = 'Delete'
    TabOrder = 2
  end
  object tmrPrinted: TTimer
    Enabled = False
    Interval = 1500
    OnTimer = tmrPrintedTimer
    Left = 648
  end
end
