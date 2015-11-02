unit MX.SimpleLogger;

interface

uses
  System.SysUtils, System.Classes;

type
  TLogDestinations = (ldNull, ldWinDebugOut, ldConsole, ldEventLog, ldFile, ldStrings);
  TNonInstantiatableDestinations = ldNull .. ldEventLog;

  TLogMsgLevel = (lmlFatal, lmlError, lmlWarn, lmlInfo, lmlDebug);

  ILogger = interface
    ['{078C6B4A-D7F5-495B-9CFC-24B58758F9C0}']
    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; const e: Exception); overload;

    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; const e: Exception); overload;

    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; const e: Exception); overload;

    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; const e: Exception); overload;

    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; const e: Exception); overload;

    function GetLoggerState: boolean;
    procedure SetLoggerState(AIsEnabled: boolean);
    property IsEnabled: boolean read GetLoggerState write SetLoggerState;
  end;

  TSimpleLogger = class(TInterfacedObject, ILogger)
  strict private
  const
    CDateTimeFormat = 'dd.mm.yyyy hh:nn:ss';
  private
    FLoggerEnabled: boolean;
    FLogDetailLevel: TLogMsgLevel;
    FLogDestination: TLogDestinations;
    FLDFile: TextFile;
    FLDStrings: TStrings;

    procedure DoLog(const ALogMsgLevel: TLogMsgLevel; const ALogStr: string);
    function LogLevelToString(ALogMsgLevel: TLogMsgLevel): string;
    function LogLevelToEventLogConst(ALogMsgLevel: TLogMsgLevel): integer;

    constructor CreateWithAnyDest(ALoggerEnabled: boolean; ALogDetailLevel: TLogMsgLevel;
      ALogDestination: TLogDestinations); overload;
  public
    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; const e: Exception); overload;

    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; const e: Exception); overload;

    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; const e: Exception); overload;

    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; const e: Exception); overload;

    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; const e: Exception); overload;

    function GetLoggerState: boolean;
    procedure SetLoggerState(AIsEnabled: boolean);
    property IsEnabled: boolean read GetLoggerState write SetLoggerState;

    constructor Create(ALoggerEnabled: boolean; ALogDetailLevel: TLogMsgLevel;
      ALogDestination: TNonInstantiatableDestinations); overload;
    constructor Create(ALoggerEnabled: boolean; ALogDetailLevel: TLogMsgLevel;
      ALogFilePath: string); overload;
    constructor Create(ALoggerEnabled: boolean; ALogDetailLevel: TLogMsgLevel;
      ALogStrings: TStrings); overload;
  end;

implementation

uses
  System.IOUtils, Winapi.Windows, Vcl.SvcMgr;

{ TSimpleLogger }

constructor TSimpleLogger.CreateWithAnyDest(ALoggerEnabled: boolean; ALogDetailLevel: TLogMsgLevel;
  ALogDestination: TLogDestinations);
begin
  inherited Create;

  FLoggerEnabled := ALoggerEnabled;
  FLogDetailLevel := ALogDetailLevel;
  FLogDestination := ALogDestination;
end;

constructor TSimpleLogger.Create(ALoggerEnabled: boolean; ALogDetailLevel: TLogMsgLevel;
  ALogDestination: TNonInstantiatableDestinations);
begin
  CreateWithAnyDest(ALoggerEnabled, ALogDetailLevel, ALogDestination);
end;

constructor TSimpleLogger.Create(ALoggerEnabled: boolean; ALogDetailLevel: TLogMsgLevel;
  ALogStrings: TStrings);
begin
  FLDStrings := ALogStrings;
  CreateWithAnyDest(ALoggerEnabled, ALogDetailLevel, ldStrings);
end;

constructor TSimpleLogger.Create(ALoggerEnabled: boolean; ALogDetailLevel: TLogMsgLevel;
  ALogFilePath: string);
begin
  AssignFile(FLDFile, ALogFilePath);
  CreateWithAnyDest(ALoggerEnabled, ALogDetailLevel, ldFile);
end;

procedure TSimpleLogger.Fatal(const msg: string);
begin
  DoLog(lmlFatal, msg);
end;

procedure TSimpleLogger.Fatal(const msg: string; const e: Exception);
begin
  DoLog(lmlFatal, msg + ' ' + e.ClassName + ': ' + e.Message);
end;

procedure TSimpleLogger.Error(const msg: string);
begin
  DoLog(lmlError, msg);
end;

procedure TSimpleLogger.Error(const msg: string; const e: Exception);
begin
  DoLog(lmlError, msg + ' ' + e.ClassName + ': ' + e.Message);
end;

procedure TSimpleLogger.Warn(const msg: string);
begin
  DoLog(lmlWarn, msg);
end;

procedure TSimpleLogger.Warn(const msg: string; const e: Exception);
begin
  DoLog(lmlWarn, msg + ' ' + e.ClassName + ': ' + e.Message);
end;

procedure TSimpleLogger.Info(const msg: string);
begin
  DoLog(lmlInfo, msg);
end;

procedure TSimpleLogger.Info(const msg: string; const e: Exception);
begin
  DoLog(lmlInfo, msg + ' ' + e.ClassName + ': ' + e.Message);
end;

procedure TSimpleLogger.Debug(const msg: string);
begin
  DoLog(lmlDebug, msg);
end;

procedure TSimpleLogger.Debug(const msg: string; const e: Exception);
begin
  DoLog(lmlDebug, msg + ' ' + e.ClassName + ': ' + e.Message);
end;

procedure TSimpleLogger.DoLog(const ALogMsgLevel: TLogMsgLevel; const ALogStr: string);
var
  LogMsg: string;
begin
  if (IsEnabled and (FLogDetailLevel >= ALogMsgLevel) and (not(FLogDestination = ldNull))) then
  begin
    LogMsg := string.Join(' ', [FormatDateTime(CDateTimeFormat, Now), LogLevelToString(ALogMsgLevel)
      + ':', ALogStr]);

    case FLogDestination of
      ldFile:
        begin
          Append(FLDFile);
          try
            WriteLn(FLDFile, LogMsg);
          finally
            CloseFile(FLDFile);
          end;
        end;
      ldStrings:
        if Assigned(FLDStrings) then
          FLDStrings.Add(LogMsg);
      ldWinDebugOut:
        OutputDebugString(PWideChar(LogMsg));
      ldConsole:
        WriteLn(LogMsg);
      ldEventLog:
        begin
          with TEventLogger.Create(TPath.GetFileNameWithoutExtension(ParamStr(0))) do
          begin
            try
              LogMessage(ALogStr, LogLevelToEventLogConst(ALogMsgLevel));
            finally
              Free;
            end;
          end;
        end;
    end;
  end;
end;

function TSimpleLogger.GetLoggerState: boolean;
begin
  Result := FLoggerEnabled;
end;

procedure TSimpleLogger.SetLoggerState(AIsEnabled: boolean);
begin
  FLoggerEnabled := AIsEnabled;
end;

function TSimpleLogger.LogLevelToEventLogConst(ALogMsgLevel: TLogMsgLevel): integer;
begin
  case ALogMsgLevel of
    lmlFatal:
      Result := EVENTLOG_ERROR_TYPE;
    lmlError:
      Result := EVENTLOG_ERROR_TYPE;
    lmlWarn:
      Result := EVENTLOG_WARNING_TYPE;
    lmlInfo:
      Result := EVENTLOG_INFORMATION_TYPE;
    lmlDebug:
      Result := EVENTLOG_INFORMATION_TYPE;
  else
    Result := integer(ALogMsgLevel);
  end;
end;

function TSimpleLogger.LogLevelToString(ALogMsgLevel: TLogMsgLevel): string;
begin
  case ALogMsgLevel of
    lmlFatal:
      Result := 'Fatal';
    lmlError:
      Result := 'Error';
    lmlWarn:
      Result := 'Warning';
    lmlInfo:
      Result := 'Info';
    lmlDebug:
      Result := 'Debug';
  else
    Result := integer(ALogMsgLevel).ToString;
  end;
end;

end.