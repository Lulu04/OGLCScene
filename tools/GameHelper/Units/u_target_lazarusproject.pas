unit u_target_lazarusproject;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  OGLCScene;

type

{ TTargetLazarusProject }

TTargetLazarusProject = record
private // config
  function GetFilenameProject_Config: string;
  function CreateStringListFromFile(const aFilename: string): TStringList;
  function LineIsActive(const s: string): boolean; overload;
  procedure SetLineActive(var s: string; aValue: boolean); overload;
  function LineIsActive(const aFilename: string; aLineIndex: integer): boolean; overload;
  procedure SetLineActive(const aFilename: string; aLineIndex: integer; aValue: boolean); overload;
  function GetProjectConfig_MaximizeSceneOnMonitor: boolean;
  procedure SetProjectConfig_MaximizeSceneOnMonitor(AValue: boolean);
  function GetProjectConfig_WindowedMode: boolean;
  procedure SetProjectConfig_WindowedMode(AValue: boolean);
public // config
  property ProjectConfig_MaximizeSceneOnMonitor: boolean read GetProjectConfig_MaximizeSceneOnMonitor write SetProjectConfig_MaximizeSceneOnMonitor;
  property ProjectConfig_WindowedMode: boolean read GetProjectConfig_WindowedMode write SetProjectConfig_WindowedMode;
private // unit utils
  function SearchTags(const aUnitFilename, aTagName: string; out FirstTagIndex, LastTagIndex: integer): TStringList;
  function ReadTagContent(const aUnitFilename, aTagName: string): TStringArray;
  procedure ReplaceTagContentBy(const aUnitFilename, aTagName: string; const aLines: TStringArray);
private // u_common
  function GetFilenameUCommon: string;
public // u_common
  function UCommonGetLayerNames: TStringArray;
  procedure UCommonSetLayerNames(const aNames: TStringArray);
  function UCommonGetDesignValues: TArrayOfInteger;
  procedure UCommonSetDesignValues(const aValues: TArrayOfInteger);
end;

implementation
uses u_project, Math;

{ TTargetLazarusProject }

function TTargetLazarusProject.GetFilenameProject_Config: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Project.Filename)) +
            'Units' + DirectorySeparator + 'project_config.cfg';
end;

function TTargetLazarusProject.CreateStringListFromFile(const aFilename: string): TStringList;
begin
  if not FileExists(aFilename)
    then raise exception.Create('File "'+aFilename+'" not found !')
    else begin
      Result := TStringList.Create;
      Result.LoadFromFile(aFilename);
    end;
end;

function TTargetLazarusProject.LineIsActive(const s: string): boolean;
begin
  Result := Copy(s, 1, 2) <> '//';
end;

procedure TTargetLazarusProject.SetLineActive(var s: string; aValue: boolean);
begin
  case aValue of
    True: if not LineIsActive(s) then system.Delete(s, 1, 2);
    False: if LineIsActive(s) then s := '//'+s;
  end;
end;

function TTargetLazarusProject.LineIsActive(const aFilename: string; aLineIndex: integer): boolean;
begin
  with CreateStringListFromFile(aFilename) do begin
    Result := LineIsActive(Strings[aLineIndex]);
    Free;
  end;
end;

procedure TTargetLazarusProject.SetLineActive(const aFilename: string; aLineIndex: integer; aValue: boolean);
var line: string;
begin
  with CreateStringListFromFile(aFilename) do begin
    line := Strings[aLineIndex];
    SetLineActive(line, aValue);
    Strings[aLineIndex] := line;
    SaveToFile(aFilename);
    Free;
  end;
end;

function TTargetLazarusProject.GetProjectConfig_MaximizeSceneOnMonitor: boolean;
begin
  Result := LineIsActive(GetFilenameProject_Config, 0);
end;

procedure TTargetLazarusProject.SetProjectConfig_MaximizeSceneOnMonitor(AValue: boolean);
begin
  SetLineActive(GetFilenameProject_Config, 0, AValue);
end;

function TTargetLazarusProject.GetProjectConfig_WindowedMode: boolean;
begin
  Result := LineIsActive(GetFilenameProject_Config, 1);
end;

procedure TTargetLazarusProject.SetProjectConfig_WindowedMode(AValue: boolean);
begin
  SetLineActive(GetFilenameProject_Config, 1, AValue);
end;

function TTargetLazarusProject.SearchTags(const aUnitFilename,
  aTagName: string; out FirstTagIndex, LastTagIndex: integer): TStringList;
var i: integer;
begin
  Result := TStringList.Create;
  FirstTagIndex := -1;
  LastTagIndex := -1;
  if not FileExists(aUnitFilename) then exit;

  Result.LoadFromFile(aUnitFilename);
  for i:=0 to Result.Count-2 do
    if Result.Strings[i] = aTagName then begin
      FirstTagIndex := i;
      break;
    end;
  if FirstTagIndex = -1 then exit;
  for i:=FirstTagIndex+1 to Result.Count-1 do
    if Result.Strings[i] = aTagName then begin
      LastTagIndex := i;
      break;
    end;
end;

function TTargetLazarusProject.ReadTagContent(const aUnitFilename, aTagName: string): TStringArray;
var first, last, i: integer;
begin
  Result := NIL;
  with SearchTags(aUnitFilename, aTagName, first, last) do begin
    if (first <> -1) and (last <> -1) and (last > first+1) then begin
      SetLength(Result, last-first-1);
      for i:=0 to High(Result) do
        Result[i] := Strings[first+i+1];
    end;
    Free;
  end;
end;

procedure TTargetLazarusProject.ReplaceTagContentBy(const aUnitFilename,
  aTagName: string; const aLines: TStringArray);
var first, last, i: integer;
begin
  with SearchTags(aUnitFilename, aTagName, first, last) do begin
    try
      if (first <> -1) and (last <> -1) then begin
        for i:=last-1 downto first+1 do
          Delete(i);
        if Length(aLines) > 0 then
          for i:=High(aLines) downto 0 do
            Insert(first+1, aLines[i]);
        SaveToFile(aUnitFilename);
      end;
    finally
      Free;
    end;
  end;
end;

function TTargetLazarusProject.GetFilenameUCommon: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Project.Filename)) +
            'Units' + DirectorySeparator + 'u_common.pas';
end;

function TTargetLazarusProject.UCommonGetLayerNames: TStringArray;
var i, k: integer;
begin
  Result := ReadTagContent(GetFilenameUCommon, '//LAYERS');
  if Length(Result) = 0 then exit;
  // delete the layer count
  system.Delete(Result, 0, 1);
  // keep only the names
  for i:=0 to High(Result) do begin
    k := Pos(' ', Result[i], 3);
    if k > 0 then Result[i] := Copy(Result[i], 3, k-3);
  end;
end;

procedure TTargetLazarusProject.UCommonSetLayerNames(const aNames: TStringArray);
var A: TStringArray;
  s: string;
  i, maxlen: integer;
begin
  if Length(aNames) = 0 then raise exception.create('bug: layer count is zero');

  A := Copy(aNames);
  // add layer count
  s := 'LAYER_COUNT = ' + integer(Length(aNames)).ToString + ';';
  system.Insert(s, A, 0);

  // retrieve the max name length
  maxlen := 0;
  for i:=0 to High(aNames) do
    maxlen := Max(maxlen, Length(aNames[i]));
  s := '  %-'+maxlen.ToString+'s = %d;';

  for i:=1 to High(A) do
    A[i] := Format(s, [A[i], i-1]);
  ReplaceTagContentBy(GetFilenameUCommon, '//LAYERS', A);
end;

function TTargetLazarusProject.UCommonGetDesignValues: TArrayOfInteger;
var i, k: integer;
  A: TStringArray;
begin
  Result := NIL;
  A := ReadTagContent(GetFilenameUCommon, '//DESIGN');
  if Length(A) <> 3 then exit;
  SetLength(Result, Length(A));
  // keep only values
  for i:=0 to High(A) do begin
    k := Pos('=', A[i]);
    system.Delete(A[i], 1, k+1);
    system.Delete(A[i], Length(A[i]), 1);
    Result[i] := A[i].ToInteger;
  end;
end;

procedure TTargetLazarusProject.UCommonSetDesignValues(const aValues: TArrayOfInteger);
var A: TStringArray;
begin
  A := NIL;
  SetLength(A, Length(aValues));
  A[0] := '  SCREEN_WIDTH_AT_DESIGN_TIME: single = '+aValues[0].ToString+';';
  A[1] := '  SCREEN_HEIGHT_AT_DESIGN_TIME: single = '+aValues[1].ToString+';';
  A[2] := '  SCREEN_PPI_AT_DESIGN_TIME: integer = '+aValues[2].ToString+';';
  ReplaceTagContentBy(GetFilenameUCommon, '//DESIGN', A);
end;

end.

