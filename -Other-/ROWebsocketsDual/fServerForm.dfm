object ServerForm: TServerForm
  Left = 372
  Top = 277
  BorderStyle = bsDialog
  Caption = 'RemObjects Server'
  ClientHeight = 153
  ClientWidth = 419
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RoPoweredByRemObjectsButton1: TROPoweredByRemObjectsButton
    Left = 8
    Top = 7
    Width = 212
    Height = 48
    Cursor = crHandPoint
  end
  object Button2: TButton
    Left = 8
    Top = 60
    Width = 75
    Height = 25
    Caption = 'Send event'
    TabOrder = 0
    OnClick = Button2Click
  end
  object ROMessage: TROJSONMessage
    Envelopes = <>
    SessionIdAsId = True
    WrapResult = True
    ParamsAsArray = True
    Indent = True
    ExtendedExceptionClass = 'ROJSONException'
    Left = 224
    Top = 56
  end
  object ROIndyHTTPServer1: TROIndyHTTPServer
    Dispatchers = <
      item
        Name = 'ROMessage'
        Message = ROMessage
        Enabled = True
        PathInfo = 'JSON'
      end
      item
        Name = 'ROBinMessage1'
        Message = ROBinMessage1
        Enabled = True
        PathInfo = 'Bin'
      end>
    SendCrossOriginHeader = True
    IndyServer.Bindings = <>
    IndyServer.DefaultPort = 8099
    IndyServer.ListenQueue = 1
    IndyServer.KeepAlive = True
    Port = 8099
    KeepAlive = True
    DisableNagle = True
    Left = 56
    Top = 8
  end
  object ROBinMessage1: TROBinMessage
    Envelopes = <>
    UseCompression = False
    Left = 224
    Top = 8
  end
  object ROHTTPFileDispatcher1: TROHTTPFileDispatcher
    Server = ROIndyHTTPServer1
    Path = '/html/'
    Folder = '.\html'
    DefaultFile = 'index.html'
    Left = 336
    Top = 8
  end
  object ROInMemorySessionManager1: TROInMemorySessionManager
    OnCustomCreateSession = ROInMemorySessionManager1CustomCreateSession
    Left = 56
    Top = 104
  end
  object ROInMemoryEventRepository1: TROInMemoryEventRepository
    Message = ROBinMessage1
    SessionManager = ROInMemorySessionManager1
    Left = 224
    Top = 104
  end
end
