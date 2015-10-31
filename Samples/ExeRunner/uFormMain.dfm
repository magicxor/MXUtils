object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 163
  ClientWidth = 298
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
  object ButtonRunTheCalculator: TButton
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 292
    Height = 25
    Align = alTop
    Caption = 'Run calc.exe'
    TabOrder = 0
    OnClick = ButtonRunTheCalculatorClick
  end
  object ButtonRunTheNotepad: TButton
    AlignWithMargins = True
    Left = 3
    Top = 34
    Width = 292
    Height = 25
    Align = alTop
    Caption = 'Run notepad.exe and wait'
    TabOrder = 1
    OnClick = ButtonRunTheNotepadClick
  end
  object MemoLogOutput: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 65
    Width = 292
    Height = 95
    Align = alClient
    TabOrder = 2
  end
end
