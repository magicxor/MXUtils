object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'MarshalManagerSample'
  ClientHeight = 150
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
  object EditMarshaled: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 123
    Width = 521
    Height = 21
    Align = alTop
    TabOrder = 2
  end
  object ButtonMarshalUnmarshalString: TButton
    AlignWithMargins = True
    Left = 3
    Top = 61
    Width = 521
    Height = 25
    Align = alTop
    Caption = 'Marshal and unmarshal (string)'
    TabOrder = 3
    OnClick = ButtonMarshalUnmarshalStringClick
  end
  object ButtonMarshalUnmarshalJSONVal: TButton
    AlignWithMargins = True
    Left = 3
    Top = 92
    Width = 521
    Height = 25
    Align = alTop
    Caption = 'Marshal and unmarshal (TJSONValue)'
    TabOrder = 4
    OnClick = ButtonMarshalUnmarshalJSONValClick
  end
end
