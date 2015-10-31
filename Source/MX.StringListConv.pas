unit MX.StringListConv;

interface

uses
  System.Types, System.Classes;

type
  TStringListConv = class
  public
    class function GenericToTStringDynArray(const Src: TArray<string>): TStringDynArray;
    class procedure GenericToTStringList(const Src: TArray<string>; var Dest: TStringList);
    class procedure GenericToTStrings(const Src: TArray<string>; var Dest: TStrings);
    class function TStringDynArrayToGeneric(const Src: TStringDynArray): TArray<string>;
    class procedure TStringDynArrayToTStringList(const Src: TStringDynArray; var Dest: TStringList);
    class procedure TStringDynArrayToTStrings(const Src: TStringDynArray; var Dest: TStrings);
    class function TStringListToTStringDynArray(const Src: TStringList): TStringDynArray;
    class function TStringListToGeneric(const Src: TStringList): TArray<string>;
    class procedure TStringListToTStrings(const Src: TStringList; var Dest: TStrings);
    class function TStringsToTStringDynArray(const Src: TStrings): TStringDynArray;
    class function TStringsToGeneric(const Src: TStrings): TArray<string>;
    class procedure TStringsToTStringList(const Src: TStrings; var Dest: TStringList);
  end;

implementation

class function TStringListConv.GenericToTStringDynArray(const Src: TArray<string>): TStringDynArray;
begin
  Result := TStringDynArray(Src);
end;

class procedure TStringListConv.GenericToTStringList(const Src: TArray<string>;
  var Dest: TStringList);
begin
  Dest.AddStrings(Src);
end;

class procedure TStringListConv.GenericToTStrings(const Src: TArray<string>; var Dest: TStrings);
begin
  Dest.AddStrings(Src);
end;

// ------------------------------------------------------------------------------

class function TStringListConv.TStringDynArrayToGeneric(const Src: TStringDynArray): TArray<string>;
begin
  Result := TArray<string>(Src);
end;

class procedure TStringListConv.TStringDynArrayToTStringList(const Src: TStringDynArray;
  var Dest: TStringList);
begin
  Dest.AddStrings(TArray<string>(Src));
end;

class procedure TStringListConv.TStringDynArrayToTStrings(const Src: TStringDynArray;
  var Dest: TStrings);
begin
  Dest.AddStrings(TArray<string>(Src));
end;

// ------------------------------------------------------------------------------

class function TStringListConv.TStringListToTStringDynArray(const Src: TStringList): TStringDynArray;
begin
  Result := TStringDynArray(Src.ToStringArray);
end;

class function TStringListConv.TStringListToGeneric(const Src: TStringList): TArray<string>;
begin
  Result := Src.ToStringArray;
end;

class procedure TStringListConv.TStringListToTStrings(const Src: TStringList; var Dest: TStrings);
begin
  Dest.AddStrings(Src);
end;

// ------------------------------------------------------------------------------

class function TStringListConv.TStringsToTStringDynArray(const Src: TStrings): TStringDynArray;
begin
  Result := TStringDynArray(Src.ToStringArray);
end;

class function TStringListConv.TStringsToGeneric(const Src: TStrings): TArray<string>;
begin
  Result := Src.ToStringArray;
end;

class procedure TStringListConv.TStringsToTStringList(const Src: TStrings; var Dest: TStringList);
begin
  Dest.AddStrings(Src);
end;

end.
