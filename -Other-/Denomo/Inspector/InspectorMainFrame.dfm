inherited FrameMain: TFrameMain
  Width = 599
  Height = 459
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 128
    Height = 459
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBoxParam: TGroupBox
      Left = 0
      Top = 0
      Width = 128
      Height = 169
      Align = alTop
      Caption = 'Parameters'
      TabOrder = 0
      object PanelMemTypes: TPanel
        Left = 2
        Top = 15
        Width = 124
        Height = 74
        Align = alTop
        Caption = 'PanelMemTypes'
        TabOrder = 0
        object LabelMemTypes: TLabel
          Left = 4
          Top = 0
          Width = 65
          Height = 13
          Caption = 'Memory types'
        end
      end
      object PanelGroupID: TPanel
        Left = 2
        Top = 89
        Width = 124
        Height = 56
        Align = alTop
        Caption = 'PanelGroupID'
        TabOrder = 1
        object LabelGroupID: TLabel
          Left = 4
          Top = 8
          Width = 54
          Height = 13
          Caption = 'Session ID:'
        end
        object CBoxGroupID: TComboBox
          Left = 4
          Top = 24
          Width = 116
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
    object ButtonMemCount: TButton
      Left = 4
      Top = 190
      Width = 120
      Height = 23
      Action = ActionMemCount
      TabOrder = 1
    end
    object Button1: TButton
      Left = 4
      Top = 218
      Width = 120
      Height = 23
      Action = ActionIncMemSessionLeakBegin
      TabOrder = 2
    end
    object Button2: TButton
      Left = 4
      Top = 246
      Width = 120
      Height = 23
      Action = ActionIncMemSessionLeakEnd
      TabOrder = 3
    end
    object Button3: TButton
      Left = 4
      Top = 275
      Width = 120
      Height = 23
      Action = ActionListMem
      TabOrder = 4
    end
    object Button4: TButton
      Left = 4
      Top = 307
      Width = 120
      Height = 23
      Action = ActionHostOption
      TabOrder = 5
    end
  end
  object PanelRight: TPanel
    Left = 128
    Top = 0
    Width = 471
    Height = 459
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object RichEditOutput: TRichEdit
      Left = 0
      Top = 25
      Width = 471
      Height = 434
      Align = alClient
      Font.Charset = GB2312_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      HideSelection = False
      ParentFont = False
      PlainText = True
      PopupMenu = PopupMenuRichEdit
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 471
      Height = 25
      Align = alTop
      BevelOuter = bvLowered
      Caption = 'Output'
      TabOrder = 1
      object BtnClear: TButton
        Left = 0
        Top = 2
        Width = 75
        Height = 23
        Action = ActionClear
        TabOrder = 0
      end
      object BtnSaveToFile: TButton
        Left = 80
        Top = 2
        Width = 75
        Height = 23
        Caption = '&Save to file'
        TabOrder = 1
        OnClick = BtnSaveToFileClick
      end
    end
  end
  object ActionListInspect: TActionList
    OnExecute = ActionListInspectExecute
    OnUpdate = ActionListInspectUpdate
    Left = 40
    Top = 328
    object ActionMemCount: TAction
      Caption = '&Memory Counter'
    end
    object ActionIncMemSessionLeakBegin: TAction
      Caption = 'Inc Session Leak Begin'
    end
    object ActionIncMemSessionLeakEnd: TAction
      Caption = 'Inc Session Leak End'
    end
    object ActionListMem: TAction
      Caption = 'List Memory'
    end
    object ActionHostOption: TAction
      Caption = 'Host Options...'
    end
  end
  object ActionListUI: TActionList
    OnUpdate = ActionListUIUpdate
    Left = 232
    Top = 160
    object ActionCopy: TAction
      Caption = '&Copy'
      OnExecute = ActionCopyExecute
    end
    object ActionSelectAll: TAction
      Caption = '&Select All'
      OnExecute = ActionSelectAllExecute
    end
    object ActionClear: TAction
      Caption = 'C&lear'
      OnExecute = ActionClearExecute
    end
    object ActionFind: TAction
      Caption = '&Find...'
      ShortCut = 16454
      OnExecute = ActionFindExecute
    end
    object ActionFindAgain: TAction
      Caption = 'Find &Again'
      ShortCut = 114
      OnExecute = ActionFindAgainExecute
      OnUpdate = ActionFindAgainUpdate
    end
  end
  object PopupMenuRichEdit: TPopupMenu
    Left = 144
    Top = 120
    object Copy1: TMenuItem
      Action = ActionCopy
    end
    object Clear1: TMenuItem
      Action = ActionClear
    end
    object SelectAll1: TMenuItem
      Action = ActionSelectAll
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Find1: TMenuItem
      Action = ActionFind
    end
    object FindAgain1: TMenuItem
      Action = ActionFindAgain
    end
  end
  object FindDlg: TFindDialog
    Options = [frDown, frDisableUpDown]
    OnFind = FindDlgFind
    Left = 256
    Top = 96
  end
end
