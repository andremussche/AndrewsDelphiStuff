inherited frmTest: TfrmTest
  Caption = 'frmTest'
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inline framTest1: TframTest [0]
    Left = 24
    Top = 16
    Width = 320
    Height = 240
    TabOrder = 0
  end
  inherited Panel1: TPanel [1]
    TabOrder = 1
  end
  inherited ActionList1: TActionList
    OnUpdate = ActionList1Update
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 408
    Top = 88
  end
end
