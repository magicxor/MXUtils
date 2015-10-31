program PEInfoRetriever;

uses
  Vcl.Forms,
  uFormMain in 'uFormMain.pas' {FormMain},
  MX.PEInfoRetriever in '..\..\Source\MX.PEInfoRetriever.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
