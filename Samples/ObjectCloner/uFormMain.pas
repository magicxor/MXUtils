unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections;

type
  TFormMain = class(TForm)
    ButtonClone: TButton;
    EditOriginal: TEdit;
    EditCloned: TEdit;
    procedure ButtonCloneClick(Sender: TObject);
  private
    ABuffer1, ABuffer2: TList<string>; // you MUST declare objects to clone in INTERFACE section
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  MX.ObjectCloner;

{$R *.dfm}

procedure TFormMain.ButtonCloneClick(Sender: TObject);
begin
  ABuffer1 := TList<string>.Create;
  try
    ABuffer1.Add(EditOriginal.Text);
    ABuffer2 := TObjectCloner.Clone < TList < string >> (ABuffer1);
    try
      EditCloned.Text := ABuffer2.Items[0];
    finally
      FreeAndNil(ABuffer2);
    end;
  finally
    FreeAndNil(ABuffer1);
  end;
end;

end.
