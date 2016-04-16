unit MX.ExeRunner;

interface

uses System.SysUtils;

type
  IExeRunner = interface
    ['{30715361-92DE-402B-B738-ACF49C471C28}']
    procedure RunAndWait(AFilePath: string; AParameters: string = string.Empty);
    procedure Run(AFilePath: string; AParameters: string = string.Empty);
  end;

  TExeRunner = class(TInterfacedObject, IExeRunner)
  strict private
  const
    C_POLL_INTERVAL = 100;
  private
    procedure DoRun(ADoWait: boolean; AFilePath: string; AParameters: string);
  public
    procedure RunAndWait(AFilePath: string; AParameters: string = string.Empty);
    procedure Run(AFilePath: string; AParameters: string = string.Empty);
  end;

implementation

uses Winapi.ShellApi, Winapi.Windows;

{ TExeRunner }

procedure TExeRunner.DoRun(ADoWait: boolean; AFilePath, AParameters: string);
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
    if ADoWait then
    begin
      repeat
        sleep(C_POLL_INTERVAL);
        GetExitCodeProcess(SEInfo.hProcess, ExitCode);
      until (ExitCode <> STILL_ACTIVE);
    end;
  end
  else
  begin
    raise Exception.Create(SysErrorMessage(GetLastError));
  end;
end;

procedure TExeRunner.Run(AFilePath, AParameters: string);
begin
  DoRun(False, AFilePath, AParameters);
end;

procedure TExeRunner.RunAndWait(AFilePath: string; AParameters: string = string.Empty);
begin
  DoRun(True, AFilePath, AParameters);
end;

end.
