object ClientForm: TClientForm
  Left = 372
  Top = 277
  Caption = 'RemObjects Client'
  ClientHeight = 54
  ClientWidth = 233
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
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
  object ROMessage: TROJSONMessage
    Envelopes = <>
    WrapResult = True
    ExtendedExceptionClass = 'ROJSONException'
    Left = 92
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
end
