object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'MarshalManagerSample'
  ClientHeight = 452
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
  object EditOriginal: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 521
    Height = 21
    Align = alTop
    TabOrder = 0
    Text = 'This is TEdit for marshal'
  end
  object ButtonMarshalUnmarshalTBytes: TButton
    AlignWithMargins = True
    Left = 3
    Top = 30
    Width = 521
    Height = 25
    Align = alTop
    Caption = 'Marshal and unmarshal (TBytes)'
    TabOrder = 1
    OnClick = ButtonMarshalUnmarshalTBytesClick
  end
  object MemoLogOutput: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 150
    Width = 521
    Height = 299
    Align = alClient
    TabOrder = 2
    ExplicitTop = 88
    ExplicitHeight = 141
  end
  object EditMarshaled: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 123
    Width = 521
    Height = 21
    Align = alTop
    TabOrder = 3
    ExplicitTop = 61
  end
  object ButtonMarshalUnmarshalString: TButton
    AlignWithMargins = True
    Left = 3
    Top = 61
    Width = 521
    Height = 25
    Align = alTop
    Caption = 'Marshal and unmarshal (string)'
    TabOrder = 4
    OnClick = ButtonMarshalUnmarshalStringClick
    ExplicitLeft = 6
    ExplicitTop = 38
  end
  object ButtonMarshalUnmarshalJSONVal: TButton
    AlignWithMargins = True
    Left = 3
    Top = 92
    Width = 521
    Height = 25
    Align = alTop
    Caption = 'Marshal and unmarshal (TJSONValue)'
    TabOrder = 5
    OnClick = ButtonMarshalUnmarshalJSONValClick
    ExplicitLeft = 6
    ExplicitTop = 69
  end
end
