object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Device info sample'
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
  object MemoDeviceInfo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 521
    Height = 208
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object ButtonGetDeviceInfo: TButton
    AlignWithMargins = True
    Left = 3
    Top = 217
    Width = 521
    Height = 25
    Align = alBottom
    Caption = 'Get Device Info'
    TabOrder = 1
    OnClick = ButtonGetDeviceInfoClick
  end
end
