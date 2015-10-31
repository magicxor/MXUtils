unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MX.SimpleLogger, MX.ExeRunner;

type
  TFormMain = class(TForm)
    ButtonRunTheCalculator: TButton;
    ButtonRunTheNotepad: TButton;
    MemoLogOutput: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ButtonRunTheCalculatorClick(Sender: TObject);
    procedure ButtonRunTheNotepadClick(Sender: TObject);
  private
    FLogger: ILogger;
    FExeRunner: IExeRunner;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.ButtonRunTheCalculatorClick(Sender: TObject);
begin
  FExeRunner.Run('C:\Windows\System32\calc.exe');
end;

procedure TFormMain.ButtonRunTheNotepadClick(Sender: TObject);
begin
  FExeRunner.RunAndWait('C:\Windows\System32\notepad.exe');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FLogger := TSimpleLogger.Create(True, lmlDebug, MemoLogOutput.Lines);
  FExeRunner := TExeRunner.Create(FLogger);
end;

end.
