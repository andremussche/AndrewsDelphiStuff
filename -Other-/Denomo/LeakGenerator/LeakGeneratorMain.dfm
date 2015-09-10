object FormMain: TFormMain
  Left = 197
  Top = 110
  Width = 561
  Height = 375
  Caption = 'Denomo Memory Leak Generator -- for demo only'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inline FrameLeakGen: TFrameLeakGen
    Left = 10
    Top = 4
    Width = 526
    Height = 337
    TabOrder = 0
  end
end
