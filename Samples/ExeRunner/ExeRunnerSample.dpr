program ExeRunnerSample;

uses
  Vcl.Forms,
  uFormMain in 'uFormMain.pas' {FormMain},
  MX.ExeRunner in '..\..\Source\MX.ExeRunner.pas',
  MX.SimpleLogger in '..\..\Source\MX.SimpleLogger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
