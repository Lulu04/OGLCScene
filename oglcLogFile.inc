{$ifdef oglcINTERFACE}
type

// define a callback procedure to receive all log message from the Scene instance
TOGLCLogCallback = procedure(const mess: string) of object;

{ TLog }

TLog = class
private const
  FORMAT_DATETIME_DEFAULT = 'hh:nn:ss:zzz';
  MARGIN_STRING = '   ';
private
  FFileName: string;
  FFile: TextFile;
  FFileIsOpened: boolean;
  FTimeMeasured: QWord;
  FCallback: TOGLCLogCallback;
  FCallbackData: Pointer;
  procedure OpenLogFile;
  procedure CloseLogFile;
  procedure Add(const aMsg: string; ShowTime: boolean=False);
  function Margin(aMarginCount: integer): string;
public
  constructor Create(aFileName: string; aCallBack: TOGLCLogCallback; aCallbackData: pointer);
  destructor Destroy; override;

  procedure DeleteLogFile;
  procedure AddEmptyLine(aSeparator: string='');
  // log a message without prefix
  procedure Mess(const aMsg: string; aMarginCount: integer=0; ShowTime: boolean=False);
  // Log an info message with prefix [II]
  procedure Info(const aMsg: string; aMarginCount: integer=0; ShowTime: boolean=False);
  // Log a warning message with prefix [WW]
  procedure Warning(const aMsg: string; aMarginCount: integer=0; ShowTime: boolean=False);
  // Log an error message with prefix [EE]
  procedure Error(const aMsg: string; aMarginCount: integer=0; ShowTime: boolean=False);
  // Log a debug message with prefix [DD]
  // Debug messages are logged only if the application is compiled in debug
  // mode (with D+ compile flag). When you compile in release mode,the
  // messages are ignored.
  procedure Debug(const aMsg: string; aMarginCount: integer=0; ShowTime: boolean=False);

  procedure StartMeasuringTime(const aMsg: string; aMarginCount: integer=0);
  procedure StopMeasuringTime(const aMsg: string; aMarginCount: integer=0);
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TLog }

procedure TLog.OpenLogFile;
begin
  AssignFile(FFile, FFilename);

  if not FileExists(FFileName)
    then Rewrite(FFile)
    else Append(FFile);

  FFileIsOpened := FileExists(FFileName);
end;

procedure TLog.CloseLogFile;
begin
  if FFileIsOpened then begin
    Flush(FFile);
    Close(FFile);
    FFileIsOpened:=FALSE;
  end;
end;

constructor TLog.Create(aFileName: string; aCallBack: TOGLCLogCallback; aCallbackData: pointer);
begin
  FFileName:=aFileName;
  FCallback := aCallback;
  FCallbackData := aCallbackData;
end;

destructor TLog.Destroy;
begin
  CloseLogFile;
  inherited Destroy;
end;

procedure TLog.DeleteLogFile;
begin
  CloseLogFile;
  if FileExists(FFilename)
    then DeleteFile(FFilename);
end;

procedure TLog.AddEmptyLine(aSeparator: string);
begin
  if not FFileIsOpened then
    OpenLogFile;
  if not FFileIsOpened then exit;

  Writeln(FFile, aSeparator);
  Flush(FFile);
end;

procedure TLog.Add(const aMsg: string; ShowTime: boolean);
var s: string;
begin
  if not FFileIsOpened then OpenLogFile;
  if not FFileIsOpened then exit;

  if ShowTime then s := Format('%s [%s]', [aMsg, FormatDateTime(FORMAT_DATETIME_DEFAULT, Now)])
    else s := aMsg;
  Writeln(FFile, s);
  Flush(FFile);

  if FCallback <> NIL then FCallback(s);
end;

function TLog.Margin(aMarginCount: integer): string;
var i: integer;
begin
  Result := '';
  for i:=0 to aMarginCount-1 do
    Result := Result + MARGIN_STRING;
end;

procedure TLog.Debug(const aMsg: string; aMarginCount: integer; ShowTime: boolean);
begin
{$ifopt D+}
  Add('[DD]  '+Margin(aMarginCount)+aMsg, ShowTime);
{$else}
// avoid compilation hint
  if aMsg = '' then;
  if aMarginCount=0 then;
  if ShowTime then;
{$endif}
end;

procedure TLog.StartMeasuringTime(const aMsg: string; aMarginCount: integer=0);
begin
  Add(Margin(aMarginCount)+aMsg, False);
  FTimeMeasured := GetTickCount64;
end;

procedure TLog.StopMeasuringTime(const aMsg: string; aMarginCount: integer=0);
begin
  FTimeMeasured := GetTickCount64 - FTimeMeasured;
  Add(Margin(aMarginCount)+aMsg+' '+SecondsToString(FTimeMeasured*0.001, True), False);
end;

procedure TLog.Error(const aMsg: string; aMarginCount: integer; ShowTime: boolean);
begin
  Add('[EE]  '+Margin(aMarginCount)+aMsg, ShowTime);
end;

procedure TLog.Warning(const aMsg: string; aMarginCount: integer; ShowTime: boolean);
begin
  Add('[WW]  '+Margin(aMarginCount)+aMsg, ShowTime);
end;

procedure TLog.Info(const aMsg: string; aMarginCount: integer; ShowTime: boolean);
begin
  Add('[II]  '+Margin(aMarginCount)+aMsg, ShowTime);
end;

procedure TLog.Mess(const aMsg: string; aMarginCount: integer; ShowTime: boolean);
begin
  Add('      '+Margin(aMarginCount)+aMsg, ShowTime);
end;

{$endif oglcIMPLEMENTATION}

