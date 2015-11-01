program ObjectClonerSample;

uses
  Vcl.Forms,
  uFormMain in 'uFormMain.pas' {FormMain},
  MX.ObjectCloner in '..\..\Source\MX.ObjectCloner.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
