object FormTestSimpleLogger: TFormTestSimpleLogger
  Left = 0
  Top = 0
  Caption = 'FormSimpleLoggerSample'
  ClientHeight = 287
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
  object MemoLogOutput: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 521
    Height = 188
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 152
    ExplicitTop = 72
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
  object ButtonGenerateInfo: TButton
    AlignWithMargins = True
    Left = 3
    Top = 197
    Width = 521
    Height = 25
    Align = alBottom
    Caption = 'Generate info'
    TabOrder = 1
    OnClick = ButtonGenerateInfoClick
    ExplicitLeft = 240
    ExplicitTop = 144
    ExplicitWidth = 75
  end
  object ButtonGenerateWarning: TButton
    AlignWithMargins = True
    Left = 3
    Top = 228
    Width = 521
    Height = 25
    Align = alBottom
    Caption = 'Generate warning'
    TabOrder = 2
    OnClick = ButtonGenerateWarningClick
    ExplicitLeft = 6
    ExplicitTop = 220
  end
  object ButtonGenerateError: TButton
    AlignWithMargins = True
    Left = 3
    Top = 259
    Width = 521
    Height = 25
    Align = alBottom
    Caption = 'Generate error'
    TabOrder = 3
    OnClick = ButtonGenerateErrorClick
    ExplicitLeft = 6
    ExplicitTop = 220
  end
end
