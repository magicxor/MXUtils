unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    ButtonGetSelfInfo: TButton;
    MemoPEInfo: TMemo;
    procedure ButtonGetSelfInfoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.IOUtils, MX.PEInfoRetriever;

{$R *.dfm}

procedure TFormMain.ButtonGetSelfInfoClick(Sender: TObject);
var
  TmpFileName: string;
begin
  MemoPEInfo.Clear;

  TmpFileName := TPath.GetTempFileName;
  TFile.Copy(ParamStr(0), TmpFileName, True);
  try
    MemoPEInfo.Lines.Add('MD5: ' + TPEInfoRetriever.GetFileHash(TmpFileName, 'MD5'));
    MemoPEInfo.Lines.Add('SHA256: ' + TPEInfoRetriever.GetFileHash(TmpFileName, 'SHA256'));
    MemoPEInfo.Lines.Add('Size: ' + TPEInfoRetriever.GetFileSize(TmpFileName).ToString);
    MemoPEInfo.Lines.Add('Short version number: ' + TPEInfoRetriever.GetShortVersionNum
      (TmpFileName));
    MemoPEInfo.Lines.Add('Full version number: ' + TPEInfoRetriever.GetFullVersionNum(TmpFileName));
  finally
    TFile.Delete(TmpFileName);
  end;
end;

end.
