object FormAbout: TFormAbout
  Left = 228
  Top = 105
  BorderStyle = bsDialog
  Caption = 'About Denomo'
  ClientHeight = 196
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    391
    196)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelURL: TLabel
    Left = 4
    Top = 130
    Width = 61
    Height = 16
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'LabelURL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = LabelURLClick
    OnMouseEnter = LabelURLMouseEnter
    OnMouseLeave = LabelURLMouseLeave
  end
  object LabelMail: TLabel
    Left = 4
    Top = 162
    Width = 59
    Height = 16
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'LabelMail'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = LabelURLClick
    OnMouseEnter = LabelURLMouseEnter
    OnMouseLeave = LabelURLMouseLeave
  end
  object RichEdit: TRichEdit
    Left = 0
    Top = 0
    Width = 391
    Height = 113
    TabStop = False
    Align = alTop
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object BtnOK: TButton
    Left = 312
    Top = 167
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
