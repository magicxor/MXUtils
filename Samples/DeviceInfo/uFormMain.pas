unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    MemoDeviceInfo: TMemo;
    ButtonGetDeviceInfo: TButton;
    procedure ButtonGetDeviceInfoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses MX.DeviceInfo;

{$R *.dfm}

procedure TFormMain.ButtonGetDeviceInfoClick(Sender: TObject);
begin
  MemoDeviceInfo.Lines.Add(TDeviceInfo.CPUCores.ToString());
  MemoDeviceInfo.Lines.Add(TDeviceInfo.OSVersion);
  MemoDeviceInfo.Lines.Add(TDeviceInfo.Resolution);
  MemoDeviceInfo.Lines.Add(TDeviceInfo.DisplayNamesStr);
  MemoDeviceInfo.Lines.Add(TDeviceInfo.BytesToGigabytesStr(TDeviceInfo.MemoryTotalPhys));
  MemoDeviceInfo.Lines.Add(TDeviceInfo.UserName);
  MemoDeviceInfo.Lines.Add(TDeviceInfo.ComputerName);
  MemoDeviceInfo.Lines.Add(TDeviceInfo.TimeZone);
  MemoDeviceInfo.Lines.Add(TDeviceInfo.HardwareProfile);
  MemoDeviceInfo.Lines.Add(TDeviceInfo.HardDiskCSerialNumber);

//  MemoDeviceInfo.Lines.Add(string.Join(sLineBreak, TDeviceInfo.InstalledUpdatesList));

  {
  if TDeviceInfo.ISHotFixID_Installed('KB890830') then
    MemoDeviceInfo.Lines.Add('true')
  else
    MemoDeviceInfo.Lines.Add('false');
    }
end;

end.
