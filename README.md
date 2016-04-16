# MXUtils
A set of classes to perform common tasks

### MX.StringListConv

TStringListConv provides methods for simple convert between TArray<string>, TStringList, TStrings, TStringDynArray.

### MX.SimpleLogger

TSimpleLogger implements ILogger and supports the following log destinations:

- Windows Debug Output
- Console
- Event Log
- File
- TStrings instance

### MX.PEInfoRetriever

TPEInfoRetriever provides the following methods:

- GetFileHash(AFilePath, AHashName: string): string
- GetFileSize(AFilePath: string): int64
- GetShortVersionNum(AFilePath: string): string
- GetFullVersionNum(AFilePath: string): string

### MX.ObjectCloner

TObjectCloner can clone simple objects using Data.DBXJSONReflect marshall/unmarshall.

### MX.MarshalManager

TUnmarshalManager<T> implements IUnmarshalManager<T> and can unmarshal objects from TJSONValue, TBytes, JSON string and JSON file.

TMarshalManager implements IMarshalManager and can marshal objects to TJSONValue, TBytes, JSON string and JSON file.

### MX.ExeRunner

TExeRunner implements IExeRunner and has two methods:

- RunAndWait(AFilePath: string; AParameters: string = string.Empty)
- Run(AFilePath: string; AParameters: string = string.Empty)

### MX.DeviceInfo

TDeviceInfo provides the following methods:

- CPUCores: integer;
- OSVersion: string;
- Resolution: string;
- DisplayNames: TArray<string>; and DisplayNamesStr: string;
- MemoryPercentInUse: DWORD;
- MemoryTotalPhys: DWORDLONG;
- MemoryAvailPhys: DWORDLONG;
- MemoryTotalVirtual: DWORDLONG;
- MemoryAvailVirtual: DWORDLONG;
- UserName: string;
- ComputerName: string;
- TimeZone: string;
- HardwareProfile: string;
- InstalledUpdatesList: TArray<string>;
- ISHotFixID_Installed(const HotFixID: string): boolean;
- HardDiskCSerialNumber: string;