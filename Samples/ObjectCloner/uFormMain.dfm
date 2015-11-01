object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Object cloner sample'
  ClientHeight = 87
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonClone: TButton
    AlignWithMargins = True
    Left = 3
    Top = 30
    Width = 521
    Height = 25
    Align = alTop
    Caption = 'Clone'
    TabOrder = 0
    OnClick = ButtonCloneClick
  end
  object EditOriginal: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 521
    Height = 21
    Align = alTop
    TabOrder = 1
    Text = '1234567890 '#9650#8595#9835#8596
  end
  object EditCloned: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 61
    Width = 521
    Height = 21
    Align = alTop
    TabOrder = 2
  end
end
