unit MX.DeviceInfo;

interface

uses
  Winapi.Windows;

type
  TDeviceInfo = class
  strict private
  const
    C_MAX_POSSIBLE_UPDATES_COUNT = 10000;
  private
    class function InitMemoryStatus: TMemoryStatusEx;
  public
    class function BytesToGigabytes(ABytes: DWORDLONG): single;
    class function BytesToGigabytesStr(ABytes: DWORDLONG): string;
    /// <summary>
    /// Represents the number of CPU cores detected.
    /// </summary>
    class function CPUCores: integer;
    /// <summary>
    /// Describes the current operating system: platform and version. <br /><br />
    /// Use TOSVersion to get information about the current operating system,
    /// such as: <br /><list type="bullet">
    /// <item>
    /// Platform (Windows, MAC OS X, ...)
    /// </item>
    /// <item>
    /// Version
    /// </item>
    /// </list>
    /// Other information:
    /// <list type="bullet">
    /// <item>
    /// Architecture (Intel x86 or Intel x64) <br />
    /// </item>
    /// <item>
    /// Installed service pack
    /// </item>
    /// </list>
    /// <br />
    /// </summary>
    /// <example>
    /// Possible output: <br /><code lang="Delphi">Windows XP Service Pack 3 (Version 5.1, Build 2600, 32-bit Edition)</code>
    /// </example>
    class function OSVersion: string;
    class function Resolution: string;
    /// <summary>
    /// An array containing the devices contexts strings. This is either a
    /// descriptions of the display adapters or of the display monitors.
    /// </summary>
    class function DisplayNames: TArray<string>;
    /// <summary>
    /// An array containing the devices contexts strings. This is either a
    /// descriptions of the display adapters or of the display monitors.
    /// </summary>
    class function DisplayNamesStr: string;
    /// <summary>
    /// A number between 0 and 100 that specifies the approximate percentage
    /// of physical memory that is in use (0 indicates no memory use and 100
    /// indicates full memory use).
    /// </summary>
    class function MemoryPercentInUse: DWORD;
    /// <summary>
    /// The amount of actual physical memory, in bytes.
    /// </summary>
    class function MemoryTotalPhys: DWORDLONG;
    /// <summary>
    /// The amount of physical memory currently available, in bytes. This is
    /// the amount of physical memory that can be immediately reused without
    /// having to write its contents to disk first. It is the sum of the size
    /// of the standby, free, and zero lists.
    /// </summary>
    class function MemoryAvailPhys: DWORDLONG;
    /// <summary>
    /// The size of the user-mode portion of the virtual address space of the
    /// calling process, in bytes. This value depends on the type of process,
    /// the type of processor, and the configuration of the operating system.
    /// For example, this value is approximately 2 GB for most 32-bit
    /// processes on an x86 processor and approximately 3 GB for 32-bit
    /// processes that are large address aware running on a system with
    /// 4-gigabyte tuning enabled.
    /// </summary>
    class function MemoryTotalVirtual: DWORDLONG;
    /// <summary>
    /// The amount of unreserved and uncommitted memory currently in the
    /// user-mode portion of the virtual address space of the calling
    /// process, in bytes. <br />
    /// </summary>
    class function MemoryAvailVirtual: DWORDLONG;
    /// <summary>
    /// Retrieves the name of the user associated with the current thread.
    /// </summary>
    class function UserName: string;
    /// <summary>
    /// Retrieves the NetBIOS name of the local computer. This name is
    /// established at system startup, when the system reads it from the
    /// registry.
    /// </summary>
    class function ComputerName: string;
    /// <summary>
    /// Retrieves the current time zone and dynamic daylight saving time
    /// settings. These settings control the translations between Coordinated
    /// Universal Time (UTC) and local time.
    /// </summary>
    class function TimeZone: string;
    /// <summary>
    /// szHwProfileGuid <br /><br />The globally unique identifier (GUID)
    /// string for the current hardware profile. The string returned by
    /// GetCurrentHwProfile encloses the GUID in curly braces, {}; for
    /// example: <br /><br />{12340001-4980-1920-6788-123456789012} <br /><br />
    /// You can use this string as a registry subkey under your application's
    /// configuration settings key in HKEY_CURRENT_USER. This enables you to
    /// store settings for each hardware profile. <br />szHwProfileName <br /><br />
    /// The display name for the current hardware profile.
    /// </summary>
    class function HardwareProfile: string;
    /// <summary>
    /// Performs a synchronous search for updates. <b>(FOR DEBUG ONLY: may be
    /// VERY slow; your application may freeze until forever because of
    /// svchost bug)</b>
    /// </summary>
    class function InstalledUpdatesList: TArray<string>;
    /// <summary>
    /// Performs a synchronous search for given update. <b>(FOR DEBUG ONLY:
    /// may be VERY slow; your application may freeze until forever because
    /// of svchost bug)</b>
    /// </summary>
    class function ISHotFixID_Installed(const HotFixID: string): boolean;
    /// <summary>
    ///   Retrieves information about the volume associated with the specified
    ///   root directory (C:).
    /// </summary>
    class function HardDiskCSerialNumber: string;
  end;

implementation

uses
  System.SysUtils, System.DateUtils, System.TimeSpan, System.Math, System.Variants,
  System.Win.ComObj,
  Vcl.Forms, Winapi.ActiveX;

{ TDeviceInfo }

class function TDeviceInfo.BytesToGigabytes(ABytes: DWORDLONG): single;
begin
  Result := SimpleRoundTo((TDeviceInfo.MemoryTotalPhys / 1024 / 1024 / 1024), -2);
end;

class function TDeviceInfo.BytesToGigabytesStr(ABytes: DWORDLONG): string;
begin
  Result := BytesToGigabytes(ABytes).ToString(ffGeneral, 4, 0);
end;

class function TDeviceInfo.ComputerName: string;
var
  buf: array [0 .. MAX_COMPUTERNAME_LENGTH] of char;
  sizebuf: DWORD;
begin
  GetComputerName(buf, sizebuf);
  Result := buf;
end;

class function TDeviceInfo.CPUCores: integer;
begin
  Result := CPUCount;
end;

class function TDeviceInfo.DisplayNames: TArray<string>;
var
  lpDisplayDevice: TDisplayDevice;
  dwFlags: DWORD;
  cc: DWORD;
begin
  Result := [];
  lpDisplayDevice.cb := sizeof(lpDisplayDevice);
  dwFlags := 0;
  cc := 0;
  while EnumDisplayDevices(nil, cc, lpDisplayDevice, dwFlags) do
  begin
    Inc(cc);
    if (lpDisplayDevice.StateFlags and DISPLAY_DEVICE_ATTACHED_TO_DESKTOP) = DISPLAY_DEVICE_ATTACHED_TO_DESKTOP
    then
      Result := Result + [lpDisplayDevice.DeviceString];
  end;
end;

class function TDeviceInfo.DisplayNamesStr: string;
begin
  Result := string.Join('; ', DisplayNames);
end;

class function TDeviceInfo.InstalledUpdatesList: TArray<string>;
var
  updateSession: OleVariant;
  updateSearcher: OleVariant;
  updateSearchResult: OleVariant;
  updateEntry: OleVariant;
  UpdateCollection: OleVariant;
  oEnum: IEnumvariant;
  iValue: LongWord;
  i: integer;
begin
  Result := [];
  i := 0;
  updateSession := CreateOleObject('Microsoft.Update.Session');
  try
    updateSearcher := updateSession.CreateUpdateSearcher;
    try
      // IUpdateSearcher::Search Method http://msdn.microsoft.com/en-us/library/aa386526%28v=VS.85%29.aspx
      updateSearcher.online := False;
      updateSearchResult := updateSearcher.Search(Format('IsInstalled = 1 and Type=%s',
        [QuotedStr('Software')]));
      try
        UpdateCollection := updateSearchResult.Updates;
        try
          oEnum := IUnknown(UpdateCollection._NewEnum) as IEnumvariant;
          // IUpdate Interface http://msdn.microsoft.com/en-us/library/aa386099%28v=VS.85%29.aspx
          while (oEnum.Next(1, updateEntry, iValue) = 0) and (i <= C_MAX_POSSIBLE_UPDATES_COUNT) do
          begin
            Result := Result + [updateEntry.Title];
            updateEntry := Unassigned;
            Inc(i);
          end;
        finally
          UpdateCollection := Unassigned;
        end;
      finally
        updateSearchResult := Unassigned;
      end;
    finally
      updateSearcher := Unassigned;
    end;
  finally
    updateSession := Unassigned;
  end;
end;

class function TDeviceInfo.ISHotFixID_Installed(const HotFixID: string): boolean;
var
  updateSession: OleVariant;
  updateSearcher: OleVariant;
  updateEntry: OleVariant;
  updateSearchResult: OleVariant;
  UpdateCollection: OleVariant;
  oEnum: IEnumvariant;
  iValue: LongWord;
  LUpdEntryTitle: string;
  i: integer;
begin
  Result := False;
  i := 0;
  updateSession := CreateOleObject('Microsoft.Update.Session');
  try
    updateSearcher := updateSession.CreateUpdateSearcher;
    try
      {
        this line improves the performance, the online porperty indicates whether the UpdateSearcher
        goes online to search for updates.
        so how we are looking for already installed updates we can set this value to false
      }
      updateSearcher.online := False;
      updateSearchResult := updateSearcher.Search(Format('IsInstalled = 1 and Type=%s',
        [QuotedStr('Software')]));
      try
        UpdateCollection := updateSearchResult.Updates;
        try
          oEnum := IUnknown(UpdateCollection._NewEnum) as IEnumvariant;
          while (oEnum.Next(1, updateEntry, iValue) = 0) and (i <= C_MAX_POSSIBLE_UPDATES_COUNT) do
          begin
            LUpdEntryTitle := updateEntry.Title;
            Result := LUpdEntryTitle.Contains(HotFixID);
            updateEntry := Unassigned;
            if Result then
              break;
            Inc(i);
          end;
        finally
          UpdateCollection := Unassigned;
        end;
      finally
        updateSearchResult := Unassigned;
      end;
    finally
      updateSearcher := Unassigned;
    end;
  finally
    updateSession := Unassigned;
  end;
end;

class function TDeviceInfo.HardDiskCSerialNumber: string;
var
  NotUsed: DWORD;
  VolumeFlags: DWORD;
  VolumeInfo: array [0 .. MAX_PATH] of char;
  VolumeSerialNumber: DWORD;
begin
// todo: add argument "driveLetter";
// todo: add function that retrieves all availiable drives
  GetVolumeInformation(PChar('C:\'), nil, sizeof(VolumeInfo), @VolumeSerialNumber, NotUsed,
    VolumeFlags, nil, 0);
  Result := Format('VolSer = %8.8X', [VolumeSerialNumber]);
end;

class function TDeviceInfo.HardwareProfile: string;
var
  LHWProfile: THWProfileInfo;
  SB: TStringBuilder;
begin
  FillChar(LHWProfile, sizeof(THWProfileInfo), 0);
  GetCurrentHwProfile(LHWProfile);
  SB := TStringBuilder.Create;
  try
    Result := SB.Append(LHWProfile.szHwProfileGuid).Append(' ')
      .Append(LHWProfile.szHwProfileName).ToString;
  finally
    FreeAndNil(SB);
  end;
end;

class function TDeviceInfo.InitMemoryStatus: TMemoryStatusEx;
var
  LMemoryStatusEx: TMemoryStatusEx;
begin
  FillChar(LMemoryStatusEx, sizeof(TMemoryStatusEx), 0);
  LMemoryStatusEx.dwLength := sizeof(TMemoryStatusEx);
  GlobalMemoryStatusEx(LMemoryStatusEx);
  Result := LMemoryStatusEx;
end;

class function TDeviceInfo.MemoryAvailPhys: DWORDLONG;
begin
  Result := InitMemoryStatus.ullAvailPhys;
end;

class function TDeviceInfo.MemoryAvailVirtual: DWORDLONG;
begin
  Result := InitMemoryStatus.ullAvailVirtual;
end;

class function TDeviceInfo.MemoryPercentInUse: DWORD;
begin
  Result := InitMemoryStatus.dwMemoryLoad;
end;

class function TDeviceInfo.MemoryTotalPhys: DWORDLONG;
begin
  Result := InitMemoryStatus.ullTotalPhys;
end;

class function TDeviceInfo.MemoryTotalVirtual: DWORDLONG;
begin
  Result := InitMemoryStatus.ullTotalVirtual;
end;

class function TDeviceInfo.OSVersion: string;
begin
  Result := TOSVersion.ToString;
end;

class function TDeviceInfo.Resolution: string;
begin
  Result := IntToStr(Screen.Width) + 'x' + IntToStr(Screen.Height);
end;

class function TDeviceInfo.TimeZone: string;
begin
  Result := string.Join('; ', [TTimeZone.Local.Abbreviation, TTimeZone.Local.DisplayName]);
end;

class function TDeviceInfo.UserName: string;
var
  buf: array [0 .. MAX_COMPUTERNAME_LENGTH] of char;
  sizebuf: DWORD;
begin
  GetUserName(buf, sizebuf);
  Result := buf;
end;

end.
