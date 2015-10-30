unit MX.PEInfoGetter;

interface

type
  IPEInfoGetter = interface
    ['{479C0D89-E33F-4619-BF45-69AD97F8E540}']
    function GetFileHash(AFilePath, AHashName: string): string;
    function GetFileSize(AFilePath: string): int64;
    function GetShortVersionNum(AFilePath: string): string;
    function GetFullVersionNum(AFilePath: string): string;
  end;

  TPEInfoGetter = class(TInterfacedObject, IPEInfoGetter)
    function GetFileHash(AFilePath, AHashName: string): string;
    function GetFileSize(AFilePath: string): int64;
    function GetShortVersionNum(AFilePath: string): string;
    function GetFullVersionNum(AFilePath: string): string;
  end;

implementation

uses System.SysUtils, System.Hash, System.IOUtils, Winapi.Windows;

{ TPEInfoGetter }

function TPEInfoGetter.GetFileHash(AFilePath, AHashName: string): string;
var
  HashMD5: THashMD5;
  HashSHA1: THashSHA1;
  HashSHA2: THashSHA2;
begin
  if not(TFile.Exists(AFilePath)) then
    raise Exception.Create('The file does not exist: ' + AFilePath)
  else if AHashName = 'MD5' then
  begin
    HashMD5 := THashMD5.Create;
    HashMD5.Update(TFile.ReadAllBytes(AFilePath));
    Result := HashMD5.HashAsString;
  end
  else if AHashName = 'SHA1' then
  begin
    HashSHA1 := THashSHA1.Create;
    HashSHA1.Update(TFile.ReadAllBytes(AFilePath));
    Result := HashSHA1.HashAsString;
  end
  else if ((AHashName = 'SHA2') or (AHashName = 'SHA256')) then
  begin
    HashSHA2 := THashSHA2.Create(SHA256);
    HashSHA2.Update(TFile.ReadAllBytes(AFilePath));
    Result := HashSHA2.HashAsString;
  end
  else if AHashName = 'SHA512' then
  begin
    HashSHA2 := THashSHA2.Create(SHA512);
    HashSHA2.Update(TFile.ReadAllBytes(AFilePath));
    Result := HashSHA2.HashAsString;
  end
  else
    raise Exception.Create('Unknown AHashName: ' + AHashName);
end;

function TPEInfoGetter.GetFileSize(AFilePath: string): int64;
var
  info: TWin32FileAttributeData;
begin
  Result := -1;

  if NOT GetFileAttributesEx(PChar(AFilePath), GetFileExInfoStandard, @info) then
    EXIT;

  Result := int64(info.nFileSizeLow) or int64(info.nFileSizeHigh shl 32);
end;

function TPEInfoGetter.GetFullVersionNum(AFilePath: string): string;
var
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Size := GetFileVersionInfoSize(PChar(AFilePath), Handle);
  if Size = 0 then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(AFilePath), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  Result := Format('%d.%d.%d.%d', [ //
    LongRec(FixedPtr.dwFileVersionMS).Hi, // major
    LongRec(FixedPtr.dwFileVersionMS).Lo, // minor
    LongRec(FixedPtr.dwFileVersionLS).Hi, // release
    LongRec(FixedPtr.dwFileVersionLS).Lo]) // build
end;

function TPEInfoGetter.GetShortVersionNum(AFilePath: string): string;
var
  Rec: LongRec;
begin
  Rec := LongRec(GetFileVersion(AFilePath));
  Result := Format('%d.%d', [Rec.Hi, Rec.Lo])
end;

end.
