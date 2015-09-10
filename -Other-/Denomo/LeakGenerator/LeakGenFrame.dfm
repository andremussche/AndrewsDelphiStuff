object FrameLeakGen: TFrameLeakGen
  Left = 0
  Top = 0
  Width = 526
  Height = 337
  TabOrder = 0
  object ListViewStrategy: TListView
    Left = 0
    Top = 27
    Width = 161
    Height = 310
    Align = alLeft
    Columns = <
      item
        Caption = 'Strategy'
        Width = 150
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListViewStrategyChange
  end
  object PanelRight: TPanel
    Left = 161
    Top = 27
    Width = 365
    Height = 310
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object RichEditDesc: TRichEdit
      Left = 0
      Top = 0
      Width = 365
      Height = 57
      Align = alTop
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = GB2312_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      PlainText = True
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 0
      Top = 57
      Width = 365
      Height = 26
      Align = alTop
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Caption = 'Report'
      TabOrder = 1
      object BtnClear: TButton
        Left = 2
        Top = 0
        Width = 75
        Height = 23
        Caption = 'Clear'
        TabOrder = 0
        OnClick = BtnClearClick
      end
    end
    object RichEditReport: TRichEdit
      Left = 0
      Top = 83
      Width = 365
      Height = 227
      Align = alClient
      Font.Charset = GB2312_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      PlainText = True
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 2
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 526
    Height = 27
    ButtonHeight = 21
    ButtonWidth = 37
    Caption = 'ToolBar1'
    ShowCaptions = True
    TabOrder = 2
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Action = ActionLeak
    end
  end
  object ActionList1: TActionList
    Left = 184
    Top = 48
    object ActionLeak: TAction
      Caption = '&Leak'
      OnExecute = ActionLeakExecute
      OnUpdate = ActionLeakUpdate
    end
  end
end
