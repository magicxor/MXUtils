unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections,
  MX.MarshalManager;

type
  TFormMain = class(TForm)
    EditOriginal: TEdit;
    ButtonMarshalUnmarshalTBytes: TButton;
    EditMarshaled: TEdit;
    ButtonMarshalUnmarshalString: TButton;
    ButtonMarshalUnmarshalJSONVal: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonMarshalUnmarshalTBytesClick(Sender: TObject);
    procedure ButtonMarshalUnmarshalStringClick(Sender: TObject);
    procedure ButtonMarshalUnmarshalJSONValClick(Sender: TObject);
  private
    FMarshalManager: IMarshalManager;
    FUnmarshalManager: IUnmarshalManager<TList<string>>;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.JSON;

{$R *.dfm}

procedure TFormMain.ButtonMarshalUnmarshalJSONValClick(Sender: TObject);
var
  EditText: TList<string>;
  MarshaledEditText: TJSONValue;
begin
  // marshal
  EditText := TList<string>.Create;
  try
    EditText.Add(EditOriginal.Text);
    MarshaledEditText := FMarshalManager.MarshalToJSONValue(EditText);
  finally
    FreeAndNil(EditText);
  end;

  try
    // unmarshal
    EditText := FUnmarshalManager.UnMarshalFromJSONValue(MarshaledEditText);
    try
      EditMarshaled.Text := EditText.Items[0];
    finally
      FreeAndNil(EditText);
    end;
  finally
    FreeAndNil(MarshaledEditText);
  end;
end;

procedure TFormMain.ButtonMarshalUnmarshalStringClick(Sender: TObject);
var
  EditText: TList<string>;
  MarshaledEditText: string;
begin
  // marshal
  EditText := TList<string>.Create;
  try
    EditText.Add(EditOriginal.Text);
    MarshaledEditText := FMarshalManager.MarshalToJSONString(EditText);
  finally
    FreeAndNil(EditText);
  end;

  // unmarshal
  EditText := FUnmarshalManager.UnMarshalFromJSONString(MarshaledEditText);
  try
    EditMarshaled.Text := EditText.Items[0];
  finally
    FreeAndNil(EditText);
  end;
end;

procedure TFormMain.ButtonMarshalUnmarshalTBytesClick(Sender: TObject);
var
  EditText: TList<string>;
  MarshaledEditText: TBytes;
begin
  // marshal
  EditText := TList<string>.Create;
  try
    EditText.Add(EditOriginal.Text);
    MarshaledEditText := FMarshalManager.MarshalToJSONBytes(EditText);
  finally
    FreeAndNil(EditText);
  end;

  // unmarshal
  EditText := FUnmarshalManager.UnMarshalFromJSONBytes(MarshaledEditText);
  try
    EditMarshaled.Text := EditText.Items[0];
  finally
    FreeAndNil(EditText);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FMarshalManager := TMarshalManager.Create;
  FUnmarshalManager := TUnmarshalManager<TList<string>>.Create;
end;

end.
