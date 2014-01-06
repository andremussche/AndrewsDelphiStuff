object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Profiling Demos'
  ClientHeight = 482
  ClientWidth = 723
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
  object Label1: TLabel
    Left = 308
    Top = 65
    Width = 79
    Height = 13
    Caption = 'lblExecuteQuery'
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 723
    Height = 482
    ActivePage = TabSheet6
    Align = alClient
    TabOrder = 0
    object TabSheet4: TTabSheet
      Caption = 'Sleep'
      ImageIndex = 3
      object Button1: TButton
        Left = 3
        Top = 3
        Width = 75
        Height = 25
        Caption = 'Sleep'
        TabOrder = 0
        OnClick = Button1Click
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'For loop'
      object lblForLoop: TLabel
        Left = 88
        Top = 14
        Width = 49
        Height = 13
        Caption = 'lblForLoop'
      end
      object lblForLoop2: TLabel
        Left = 88
        Top = 46
        Width = 55
        Height = 13
        Caption = 'lblForLoop2'
      end
      object prgForLoop2: TProgressBar
        Left = 3
        Top = 72
        Width = 273
        Height = 17
        TabOrder = 0
      end
      object btnForLoop: TButton
        Left = 3
        Top = 9
        Width = 75
        Height = 25
        Caption = 'For loop'
        TabOrder = 1
        OnClick = btnForLoopClick
      end
      object btnForLoopProgress: TButton
        Left = 3
        Top = 41
        Width = 75
        Height = 25
        Caption = 'For loop 2'
        TabOrder = 2
        OnClick = btnForLoopProgressClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'ADO / FireDAC'
      ImageIndex = 1
      object lblExecuteQuery: TLabel
        Left = 104
        Top = 10
        Width = 79
        Height = 13
        Caption = 'lblExecuteQuery'
      end
      object lblExecuteQuery2: TLabel
        Left = 304
        Top = 42
        Width = 85
        Height = 13
        Caption = 'lblExecuteQuery2'
      end
      object lblExecuteQueryLow: TLabel
        Left = 304
        Top = 10
        Width = 79
        Height = 13
        Caption = 'lblExecuteQuery'
      end
      object mmoQuery: TMemo
        Left = 0
        Top = 64
        Width = 715
        Height = 390
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'mmoQuery')
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object btnExecuteQuery: TButton
        Left = 4
        Top = 5
        Width = 93
        Height = 25
        Caption = 'TADOQuery 100'
        TabOrder = 1
        OnClick = btnExecuteQueryClick
      end
      object btnExecuteLowlevel: TButton
        Left = 189
        Top = 5
        Width = 108
        Height = 25
        Caption = 'Execute 1000'
        TabOrder = 2
        OnClick = btnExecuteLowlevelClick
      end
      object btnExecuteQuery2: TButton
        Left = 189
        Top = 36
        Width = 109
        Height = 25
        Caption = 'Execute mmo 1000'
        TabOrder = 3
        OnClick = btnExecuteQuery2Click
      end
      object Button2: TButton
        Left = 428
        Top = 5
        Width = 93
        Height = 25
        Caption = 'TADQuery 1000'
        TabOrder = 4
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 527
        Top = 5
        Width = 114
        Height = 25
        Caption = 'ExecSQLScalar 1000'
        TabOrder = 5
        OnClick = Button3Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'OpenOffice/OLE'
      ImageIndex = 2
      object bntOpenOffice: TButton
        Left = 8
        Top = 8
        Width = 89
        Height = 25
        Caption = 'bntOpenOffice'
        TabOrder = 0
        OnClick = bntOpenOfficeClick
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'List'
      ImageIndex = 6
      object Button6: TButton
        Left = 16
        Top = 8
        Width = 105
        Height = 25
        Caption = 'THashedStringList'
        TabOrder = 0
        OnClick = Button6Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Minidump/watchdog'
      ImageIndex = 4
      object Button4: TButton
        Left = 3
        Top = 3
        Width = 118
        Height = 25
        Caption = 'Test watchdog'
        TabOrder = 0
        OnClick = Button4Click
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'load profiler dll'
      ImageIndex = 5
      object Button5: TButton
        Left = 16
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Load dll'
        TabOrder = 0
        OnClick = Button5Click
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Strings'
      ImageIndex = 7
      TabVisible = False
      object Button7: TButton
        Left = 3
        Top = 5
        Width = 75
        Height = 25
        Caption = 'Strings'
        TabOrder = 0
        OnClick = Button7Click
      end
      object Memo1: TMemo
        Left = 3
        Top = 36
        Width = 486
        Height = 317
        Lines.Strings = (
          'Memo1')
        TabOrder = 1
      end
      object Button8: TButton
        Left = 97
        Top = 5
        Width = 75
        Height = 25
        Caption = 'Profile strings'
        TabOrder = 2
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 178
        Top = 5
        Width = 75
        Height = 25
        Caption = 'sample profile'
        TabOrder = 3
        OnClick = Button9Click
      end
    end
  end
  object ADOConnection1: TADOConnection
    ConnectionString = 
      'Provider=SQLNCLI10.1;Integrated Security=SSPI;Persist Security I' +
      'nfo=False;User ID="";Initial Catalog="";Data Source=TEST-VMWARE\' +
      'LAPTOPAM;Initial File Name="";Server SPN=""'
    KeepConnection = False
    LoginPrompt = False
    Provider = 'SQLNCLI10.1'
    Left = 328
    Top = 32
  end
  object ADOQuery1: TADOQuery
    Connection = ADOConnection1
    CursorType = ctOpenForwardOnly
    Parameters = <>
    SQL.Strings = (
      'SELECT TOP 1 [name]  FROM [master].[sys].[all_views]')
    Left = 328
    Top = 88
  end
  object dbMain: TADConnection
    Params.Strings = (
      'Server=TEST-VMWARE\LAPTOPAM'
      'User_Name=sa'
      'Password=sprvsr'
      'DriverID=MSSQL')
    LoginPrompt = False
    Left = 312
    Top = 160
  end
  object ADGUIxWaitCursor1: TADGUIxWaitCursor
    ScreenCursor = gcrHourGlass
    Left = 424
    Top = 160
  end
  object qryFire: TADQuery
    Connection = dbMain
    FetchOptions.AssignedValues = [evMode, evRecordCountMode, evUnidirectional, evCursorKind]
    FetchOptions.Mode = fmAll
    FetchOptions.CursorKind = ckForwardOnly
    FetchOptions.Unidirectional = True
    ResourceOptions.AssignedValues = [rvDirectExecute, rvStoreItems]
    ResourceOptions.DirectExecute = True
    ResourceOptions.StoreItems = [siData]
    SQL.Strings = (
      'select * from Categories')
    Left = 424
    Top = 215
  end
  object ADPhysMSSQLDriverLink1: TADPhysMSSQLDriverLink
    Left = 312
    Top = 216
  end
end
