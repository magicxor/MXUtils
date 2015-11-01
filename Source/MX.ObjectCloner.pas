unit MX.ObjectCloner;

interface

type
  TObjectCloner = class
  public
    class function Clone<T: class>(ASourceObj: T): T; static;
  end;

implementation

uses
  System.SysUtils, System.JSON, Data.DBXJSONReflect;

class function TObjectCloner.Clone<T>(ASourceObj: T): T;
var
  MarshalObj: TJSONMarshal;
  UnMarshalObj: TJSONUnMarshal;
  JSONValue: TJSONValue;
begin
  Result := nil;
  MarshalObj := TJSONMarshal.Create;
  try
    UnMarshalObj := TJSONUnMarshal.Create;
    try
      JSONValue := MarshalObj.Marshal(ASourceObj);
      try
        if Assigned(JSONValue) then
          Result := UnMarshalObj.Unmarshal(JSONValue) as T;
      finally
        FreeAndNil(JSONValue);
      end;
    finally
      FreeAndNil(UnMarshalObj);
    end;
  finally
    FreeAndNil(MarshalObj);
  end;
end;

end.
