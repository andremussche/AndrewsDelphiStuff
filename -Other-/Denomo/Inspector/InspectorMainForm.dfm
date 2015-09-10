object FormMain: TFormMain
  Left = 197
  Top = 110
  Width = 599
  Height = 459
  Caption = 'Denomo Inspector'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 256
    object File1: TMenuItem
      Caption = '&File'
      object MMExit: TMenuItem
        Caption = '&Exit'
        OnClick = MMExitClick
      end
    end
    object About1: TMenuItem
      Caption = '&Help'
      object MMHelpContent: TMenuItem
        Caption = '&Content'
        OnClick = MMHelpContentClick
      end
      object MMHelpFAQs: TMenuItem
        Caption = '&FAQs'
        OnClick = MMHelpFAQsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MMAbout: TMenuItem
        Caption = '&About'
        OnClick = MMAboutClick
      end
    end
  end
end
