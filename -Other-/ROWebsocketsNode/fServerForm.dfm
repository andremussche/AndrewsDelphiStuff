object ServerForm: TServerForm
  Left = 372
  Top = 277
  BorderStyle = bsDialog
  Caption = 'RemObjects Server'
  ClientHeight = 63
  ClientWidth = 227
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
  object ROMessage: TROJSONMessage
    Envelopes = <>
    WrapResult = True
    ParamsAsArray = True
    Indent = True
    ExtendedExceptionClass = 'ROJSONException'
    Left = 92
    Top = 8
  end
  object ROServer: TROIpTCPServer
    Dispatchers = <
      item
        Name = 'ROMessage'
        Message = ROMessage
        Enabled = True
      end>
    Port = 8090
    Timeout = 0
    Left = 16
    Top = 8
  end
  object ROIndyHTTPServer1: TROIndyHTTPServer
    Active = True
    Dispatchers = <
      item
        Name = 'ROMessage'
        Message = ROMessage
        Enabled = True
        PathInfo = 'JSON'
      end>
    SendCrossOriginHeader = True
    IndyServer.Bindings = <
      item
        Port = 8099
      end>
    IndyServer.DefaultPort = 8099
    Port = 8099
    Left = 176
    Top = 8
  end
end
