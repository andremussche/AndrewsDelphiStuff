object ServerForm: TServerForm
  Left = 372
  Top = 277
  BorderStyle = bsDialog
  Caption = 'RemObjects Server'
  ClientHeight = 322
  ClientWidth = 403
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RoPoweredByRemObjectsButton1: TROPoweredByRemObjectsButton
    Left = 8
    Top = 8
    Width = 212
    Height = 48
    Cursor = crHandPoint
  end
  object Button1: TButton
    Left = 32
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ROMessage: TROJSONMessage
    Envelopes = <>
    ExtendedExceptionClass = 'ROJSONException'
    Left = 108
    Top = 16
  end
  object ROServer: TROIndyHTTPServer
    Dispatchers = <
      item
        Name = 'ROMessage'
        Message = ROMessage
        Enabled = True
        PathInfo = 'JSON'
      end
      item
        Name = 'ROSOAPMessage1'
        Message = ROSOAPMessage1
        Enabled = True
        PathInfo = 'SOAP'
      end>
    IndyServer.Bindings = <>
    IndyServer.DefaultPort = 8099
    IndyServer.ListenQueue = 50
    IndyServer.Scheduler = IdSchedulerOfThreadPool1
    IndyServer.KeepAlive = True
    Port = 8099
    KeepAlive = True
    DisableNagle = True
    Left = 24
    Top = 8
  end
  object ROSOAPMessage1: TROSOAPMessage
    Envelopes = <>
    SerializationOptions = [xsoSendUntyped, xsoStrictStructureFieldOrder, xsoDocument, xsoSplitServiceWsdls]
    Left = 104
    Top = 72
  end
  object ROIndyHTTPChannel1: TROIndyHTTPChannel
    UserAgent = 'RemObjects SDK'
    TargetURL = 'http://localhost:8099/JSON'
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
    Left = 56
    Top = 224
  end
  object ROJSONMessage1: TROJSONMessage
    Envelopes = <>
    ExtendedExceptionClass = 'ROJSONException'
    Left = 160
    Top = 224
  end
  object RORemoteService1: TRORemoteService
    Channel = ROIndyHTTPChannel1
    Message = ROJSONMessage1
    Left = 272
    Top = 224
  end
  object IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool
    MaxThreads = 100
    PoolSize = 50
    Left = 256
    Top = 16
  end
  object ROBPDXHTTPServer1: TROBPDXHTTPServer
    Dispatchers = <
      item
        Name = 'ROMessage'
        Message = ROMessage
        Enabled = True
        PathInfo = 'JSON'
      end>
    BPDXServer.ReleaseDate = '2002-09-01'
    BPDXServer.ListenerThreadPriority = tpIdle
    BPDXServer.SpawnedThreadPriority = tpIdle
    BPDXServer.Suspend = False
    BPDXServer.UseSSL = False
    BPDXServer.UseThreadPool = True
    BPDXServer.ServerPort = 8099
    BPDXServer.ProtocolToBind = wpTCPOnly
    BPDXServer.SocketOutputBufferSize = bsfNormal
    BPDXServer.ServerType = stThreadBlocking
    BPDXServer.ThreadCacheSize = 50
    BPDXServer.Timeout = 50000
    BPDXServer.SupportKeepAlive = True
    Port = 8099
    SupportKeepAlive = True
    Left = 56
    Top = 128
  end
  object ROIpHTTPServer1: TROIpHTTPServer
    Dispatchers = <
      item
        Name = 'ROMessage'
        Message = ROMessage
        Enabled = True
        PathInfo = 'JSON'
      end>
    Port = 8099
    KeepAlive = True
    Left = 160
    Top = 128
  end
end
