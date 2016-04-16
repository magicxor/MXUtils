unit MX.MarshalManager;

interface

uses System.SysUtils, System.JSON;

type
  IUnmarshalManager<T: class> = interface
    ['{55EEF554-B351-411F-8209-D44796FC5480}']
    function UnMarshalFromJSONValue(AJSONValue: TJSONValue): T;
    function UnMarshalFromJSONBytes(AJSONBytes: TBytes): T;
    function UnMarshalFromJSONString(AJSONString: string): T;
    function UnMarshalFromJSONFile(AJSONFilePath: string): T;
  end;

  TUnmarshalManager<T: class> = class(TInterfacedObject, IUnmarshalManager<T>)
  public
    function UnMarshalFromJSONValue(AJSONValue: TJSONValue): T;
    function UnMarshalFromJSONBytes(AJSONBytes: TBytes): T;
    function UnMarshalFromJSONString(AJSONString: string): T;
    function UnMarshalFromJSONFile(AJSONFilePath: string): T;
  end;

  IMarshalManager = interface
    ['{15558927-756B-422E-B36B-0EA82988F12C}']
    function MarshalToJSONValue(AObject: TObject): TJSONValue;
    function MarshalToJSONBytes(AObject: TObject): TBytes;
    function MarshalToJSONString(AObject: TObject): string;
    procedure MarshalToJSONFile(AObject: TObject; AFilePath: string);
  end;

  TMarshalManager = class(TInterfacedObject, IMarshalManager)
  public
    function MarshalToJSONValue(AObject: TObject): TJSONValue;
    function MarshalToJSONBytes(AObject: TObject): TBytes;
    function MarshalToJSONString(AObject: TObject): string;
    procedure MarshalToJSONFile(AObject: TObject; AFilePath: string);
  end;

implementation

uses Data.DBXJSONReflect, System.IOUtils, System.Classes;

{ TUnmarshalManager<T> }

function TUnmarshalManager<T>.UnMarshalFromJSONBytes(AJSONBytes: TBytes): T;
var
  JSONVal: TJSONValue;
begin
  JSONVal := TJSONObject.ParseJSONValue(AJSONBytes, 0, True);
  try
    Result := UnMarshalFromJSONValue(JSONVal);
  finally
    FreeAndNil(JSONVal);
  end;
end;

function TUnmarshalManager<T>.UnMarshalFromJSONFile(AJSONFilePath: string): T;
begin
  Result := UnMarshalFromJSONBytes(TFile.ReadAllBytes(AJSONFilePath));
end;

function TUnmarshalManager<T>.UnMarshalFromJSONString(AJSONString: string): T;
var
  JSONVal: TJSONValue;
begin
  JSONVal := TJSONObject.ParseJSONValue(AJSONString);
  try
    Result := UnMarshalFromJSONValue(JSONVal);
  finally
    FreeAndNil(JSONVal);
  end;
end;

function TUnmarshalManager<T>.UnMarshalFromJSONValue(AJSONValue: TJSONValue): T;
var
  Unmarshaller: TJSONUnMarshal;
  LWarning: TTransientField;
begin
  Unmarshaller := TJSONUnMarshal.Create;
  try
    Result := Unmarshaller.UnMarshal(AJSONValue) as T;
  finally
    FreeAndNil(Unmarshaller);
  end;
end;

{ TMarshalManager }

function TMarshalManager.MarshalToJSONValue(AObject: TObject): TJSONValue;
var
  Marshaller: TJSONMarshal;
begin
  Result := nil;
  Marshaller := TJSONMarshal.Create;
  try
    if Assigned(AObject) then
    begin
      Result := Marshaller.Marshal(AObject);
    end
    else
      raise Exception.Create('Can''t marshal the nil value');
  finally
    FreeAndNil(Marshaller);
  end;
end;

function TMarshalManager.MarshalToJSONString(AObject: TObject): string;
var
  JSONVal: TJSONValue;
begin
  JSONVal := MarshalToJSONValue(AObject);
  try
    Result := JSONVal.ToJSON;
  finally
    FreeAndNil(JSONVal);
  end;
end;

function TMarshalManager.MarshalToJSONBytes(AObject: TObject): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(MarshalToJSONString(AObject));
end;

procedure TMarshalManager.MarshalToJSONFile(AObject: TObject; AFilePath: string);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Text := MarshalToJSONString(AObject);
    StringList.SaveToFile(AFilePath);
  finally
    FreeAndNil(StringList);
  end;
end;

end.
