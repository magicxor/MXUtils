program DeviceInfoSample;

uses
  Vcl.Forms,
  uFormMain in 'uFormMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
