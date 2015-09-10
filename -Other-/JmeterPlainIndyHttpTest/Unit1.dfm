object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 337
  ClientWidth = 527
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
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    DefaultPort = 777
    ListenQueue = 50
    Scheduler = IdSchedulerOfThreadPool1
    KeepAlive = True
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 56
    Top = 24
  end
  object IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool
    MaxThreads = 100
    PoolSize = 50
    Left = 192
    Top = 24
  end
end
