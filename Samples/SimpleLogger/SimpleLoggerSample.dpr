program SimpleLoggerSample;

uses
  Vcl.Forms,
  uFormMain in 'uFormMain.pas' {FormTestSimpleLogger},
  MX.SimpleLogger in '..\..\Source\MX.SimpleLogger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTestSimpleLogger, FormTestSimpleLogger);
  Application.Run;
end.
