object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'PE info retriever sample'
  ClientHeight = 245
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
  object ButtonGetSelfInfo: TButton
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 521
    Height = 46
    Align = alTop
    Caption = 'Get info about this PE'
    TabOrder = 0
    OnClick = ButtonGetSelfInfoClick
  end
  object MemoPEInfo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 55
    Width = 521
    Height = 187
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 56
    ExplicitTop = 88
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
end
