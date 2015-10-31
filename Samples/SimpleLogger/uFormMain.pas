unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MX.SimpleLogger;

type
  TFormTestSimpleLogger = class(TForm)
    MemoLogOutput: TMemo;
    ButtonGenerateInfo: TButton;
    ButtonGenerateWarning: TButton;
    ButtonGenerateError: TButton;
    procedure ButtonGenerateInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonGenerateWarningClick(Sender: TObject);
    procedure ButtonGenerateErrorClick(Sender: TObject);
  private
    FLogger: ILogger;
  public
    { Public declarations }
  end;

var
  FormTestSimpleLogger: TFormTestSimpleLogger;

implementation

{$R *.dfm}

procedure TFormTestSimpleLogger.FormCreate(Sender: TObject);
begin
  FLogger := TSimpleLogger.Create(True, lmlDebug, MemoLogOutput.Lines);
end;

procedure TFormTestSimpleLogger.ButtonGenerateInfoClick(Sender: TObject);
begin
  FLogger.Info((Sender as TButton).Caption);
end;

procedure TFormTestSimpleLogger.ButtonGenerateWarningClick(Sender: TObject);
begin
  FLogger.Warn((Sender as TButton).Caption);
end;

procedure TFormTestSimpleLogger.ButtonGenerateErrorClick(Sender: TObject);
var
  E: Exception;
begin
  E := EProgrammerNotFound.Create('Can''t found a programmer!');
  try
    FLogger.Error((Sender as TButton).Caption, E);
  finally
    FreeAndNil(E);
  end;
end;

end.
