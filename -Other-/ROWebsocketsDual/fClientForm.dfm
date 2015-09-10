object ClientForm: TClientForm
  Left = 372
  Top = 277
  Caption = 'RemObjects Client'
  ClientHeight = 104
  ClientWidth = 432
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Sum(1,2)'
    TabOrder = 0
    OnClick = Button1Click
  end
  object btnServertime: TButton
    Left = 89
    Top = 8
    Width = 156
    Height = 25
    Caption = 'server time: '
    TabOrder = 1
    OnClick = btnServertimeClick
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 49
    Width = 234
    Height = 17
    Smooth = True
    TabOrder = 2
  end
  object Button3: TButton
    Left = 251
    Top = 8
    Width = 78
    Height = 25
    Caption = 'Long progress'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 335
    Top = 8
    Width = 75
    Height = 25
    Caption = 'timer'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button2: TButton
    Left = 8
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 5
    OnClick = Button2Click
  end
  object ROMessage: TROJSONMessage
    Envelopes = <>
    SessionIdAsId = True
    WrapResult = True
    ExtendedExceptionClass = 'ROJSONException'
    Left = 97
    Top = 8
  end
  object ROChannel: TROIndyTCPChannel
    DispatchOptions = []
    ServerLocators = <>
    Port = 8090
    Host = '127.0.0.1'
    IndyClient.ConnectTimeout = 0
    IndyClient.Host = '127.0.0.1'
    IndyClient.IPVersion = Id_IPv4
    IndyClient.Port = 8090
    IndyClient.ReadTimeout = -1
    Left = 24
    Top = 8
  end
  object RORemoteService: TRORemoteService
    ServiceName = 'NewService'
    Channel = ROChannel
    Message = ROMessage
    Left = 168
    Top = 8
  end
  object ROIndyHTTPChannel1: TROIndyHTTPChannel
    UserAgent = 'RemObjects SDK'
    DispatchOptions = []
    ServerLocators = <>
    IndyClient.AllowCookies = True
    IndyClient.ProxyParams.BasicAuthentication = False
    IndyClient.ProxyParams.ProxyPort = 0
    IndyClient.Request.ContentLength = -1
    IndyClient.Request.Accept = 'text/html, */*'
    IndyClient.Request.BasicAuthentication = False
    IndyClient.Request.UserAgent = 'RemObjects SDK'
    IndyClient.HTTPOptions = [hoForceEncodeParams]
    Left = 24
    Top = 56
  end
  object ROEventReceiver1: TROEventReceiver
    Message = ROBinMessage1
    Channel = ROIndyHTTPChannel1
    Left = 168
    Top = 56
  end
  object ROBinMessage1: TROBinMessage
    Envelopes = <>
    Left = 96
    Top = 56
  end
end
