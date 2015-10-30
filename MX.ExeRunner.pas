unit MX.ExeRunner;

interface

uses System.SysUtils;

type
  IExeRunner = interface
    ['{D2F27346-80D9-4F9A-9A20-76BF1E0AFE08}']
    procedure Run(AFilePath: string; AParameters: string = string.Empty);
  end;

  TExeRunner = class(TInterfacedObject, IExeRunner)
    procedure Run(AFilePath: string; AParameters: string = string.Empty);
  end;

const
  C_POLL_INTERVAL = 100;

implementation

uses Winapi.ShellApi, Winapi.Windows;

{ TExeRunner }

procedure TExeRunner.Run(AFilePath: string; AParameters: string = string.Empty);
var
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  FillChar(SEInfo, Sizeof(SEInfo), 0);
  SEInfo.cbSize := Sizeof(TShellExecuteInfo);
  SEInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  SEInfo.Wnd := 0;
  SEInfo.lpFile := PChar(AFilePath);

  SEInfo.lpParameters := PChar(AParameters);
  { StartInString specifies the name of the working directory.
    If ommited, the current directory is used. }
  // lpDirectory := PChar(StartInString);

  SEInfo.nShow := SW_SHOWNORMAL;
  if ShellExecuteEx(@SEInfo) then
  begin
    repeat
      sleep(C_POLL_INTERVAL);
      GetExitCodeProcess(SEInfo.hProcess, ExitCode);
    until (ExitCode <> STILL_ACTIVE);
  end
  else
  begin
    raise Exception.Create('Can''t run: ' + SysErrorMessage(GetLastError));
  end;
end;

end.
