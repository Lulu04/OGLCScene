unit u_target_lazarusproject;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  laz2_DOM, laz2_XMLRead, laz2_XMLWrite,
  OGLCScene, u_connection_to_ide;


type

{ TTargetLazarusProject }

TTargetLazarusProject = record
private // LPI file utils
  function LPI_ReadUnitsSection: TStringArray;
  function LPI_CheckIfHaveUnit(const aUnitName: string;
                               aUnitLocation: TUnitLocation;
                               aUnitExtension: TUnitExtension): boolean;
  function LPI_ExpandUnitName(const aUnitName: string;
                              aUnitLocation: TUnitLocation;
                              aUnitExtension: TUnitExtension): string;
  procedure LPI_AddUnit(const aUnitName: string;
                        aUnitLocation: TUnitLocation;
                        aUnitExtension: TUnitExtension);
  procedure LPI_RemoveUnit(const aUnitName: string;
                           aUnitLocation: TUnitLocation;
                           aUnitExtension: TUnitExtension);
public // string utils
  // return True if the char is found -> IndexFound is the index
  function FindIndexOfNextChar(const Src: string; aChar: char;
                               aStartIndex: integer; out IndexFound: integer): boolean;
  // return True if the char is found -> IndexFound is the index
  function FindIndexOfPreviousChar(const Src: string; aChar: char;
                                   aStartIndex: integer; out IndexFound: integer): boolean;
  procedure RemoveNextConsecutiveChar(var Src: string; aChar: char; aFromIndex: integer);
  procedure RemovePreviousConsecutiveChar(var Src: string; aChar: char; aFromIndex: integer);
public // folder and files
  function GetProjectFolder: string;
  function GetFolderGameHelperFiles: string;
  function GetFolderUnits: string;
  function GetFolderUnitsSprites: string;
  function GetFolderUnitsLevels: string;
  function GetFolderBinary: string;
  function GetFolderData: string;
  function GetFolderDataTextures: string;
  function GetFilenameGameLevels: string;
  function GetFilenameProject_Config: string;
  function GetFilenameProject_LPR: string;
  function GetFilenameProject_LPI: string;
private // TStringList utils
  function StringList_CreateFromFile(const aFilename: string): TStringList;
  // -1 if not found
  function StringList_SearchLineThatContain(t: TStringList; const aStringToSearch: string;
                                 aSearchFromLineIndex: integer;
                                 aSearchToLineIndex: integer=MaxInt): integer;
  procedure StringList_RemoveUnitNameOnLine(t: TStringList; aLineIndex: integer; const aUnitName: string);
  procedure StringList_ReplaceAllOccurrencesOf(t: TStringList; const aOldValue, aNewValue: string);
private // project options in configuration file
  function ConfigOption_IsActive(const aFilename, aOptionName: string): boolean;
  procedure ConfigOption_SetActive(const aFilename, aOptionName: string; aValue: boolean);
  function ConfigOption_GetMaximizeSceneOnMonitor: boolean;
  procedure ConfigOption_SetMaximizeSceneOnMonitor(AValue: boolean);
  function ConfigOption_GetWindowedMode: boolean;
  procedure ConfigOption_SetWindowedMode(AValue: boolean);
public // config
  property ProjectConfig_MaximizeSceneOnMonitor: boolean read ConfigOption_GetMaximizeSceneOnMonitor write ConfigOption_SetMaximizeSceneOnMonitor;
  property ProjectConfig_WindowedMode: boolean read ConfigOption_GetWindowedMode write ConfigOption_SetWindowedMode;
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

private // entry in USES clause into an unit
  function Unit_USES_CheckIfHaveEntry(t: TStringList; const aUnitNameToCheck: string;
          aCheckInInterface, aCheckInImplementation: boolean): boolean;
public // entry in USES clause into an unit

  procedure Unit_USES_AddEntry(const aTargetUnit, aUnitNameToAdd: string; aAddInInterface: boolean);
  procedure Unit_USES_RemoveEntry(const aTargetUnit, aUnitNameToRemove: string;
                                  aRemoveInInterface, aRemoveInImplementation: boolean);
  procedure Unit_ReplaceAllOccurrencesOf(const aTargetUnitFilename, aOldValue, aNewValue: string);

public // add/remove unit to project
  function Unit_GetFullFilename(const aObjectName: string;
                               aUnitLocation: TUnitLocation;
                               aUnitExtension: TUnitExtension): string;
  procedure Unit_AddToProject(const aObjectName: string;
                              aUnitLocation: TUnitLocation;
                              aUnitExtension: TUnitExtension);
  procedure Unit_RemoveFromProject(const aObjectName: string;
                                   aUnitLocation: TUnitLocation;
                                   aUnitExtension: TUnitExtension);
  procedure Unit_DeleteFile(const aObjectName: string;
                            aUnitLocation: TUnitLocation;
                            aUnitExtension: TUnitExtension);

  // i.e. for a sprite named 'Car' return 'u_sprite_car'
  function SpriteNameToUnitNameWithoutExt(const aSpriteName: string): string;
  // i.e. for a sprite named 'Car' return 'fullpath + Units\Sprites\u_sprite_car.pas
  function SpriteNameToUnitFullFilename(const aSpriteName: string): string;
end;


implementation
uses u_project, u_common, Math, utilitaire_fichier, Forms;


{ TTargetLazarusProject }

function TTargetLazarusProject.FindIndexOfNextChar(const Src: string;
  aChar: char; aStartIndex: integer; out IndexFound: integer): boolean;
var i: integer;
begin
  if aStartIndex = 0 then aStartIndex := 1;
  for i:=aStartIndex to Length(Src) do
    if Src[i] = aChar then begin
      IndexFound := i;
      exit(True);
    end;
  IndexFound := -1;
  Result := False;
end;

function TTargetLazarusProject.FindIndexOfPreviousChar(const Src: string;
  aChar: char; aStartIndex: integer; out IndexFound: integer): boolean;
var i: integer;
begin
  for i:=aStartIndex downto 1 do
    if Src[i] = aChar then begin
      IndexFound := i;
      exit(True);
    end;
  IndexFound := -1;
  Result := False;
end;

procedure TTargetLazarusProject.RemoveNextConsecutiveChar(var Src: string;
  aChar: char; aFromIndex: integer);
 var i: integer;
begin
  if Src = '' then exit;
  if (aFromIndex < 1) or (aFromIndex > Length(Src)) then raise exception.create('aFromIndex: bad value');

  i := aFromIndex;
  while (i <= Length(Src)) and (Src[i] = aChar) do
    inc(i);
  system.Delete(Src, aFromIndex, i-aFromIndex+1);
end;

procedure TTargetLazarusProject.RemovePreviousConsecutiveChar(var Src: string;
  aChar: char; aFromIndex: integer);
var i: integer;
begin
  if Src = '' then exit;
  if (aFromIndex < 1) or (aFromIndex > Length(Src)) then raise exception.create('aFromIndex: bad value');

  i := aFromIndex;
  while (i >= 1) and (Src[i] = aChar) do
    dec(i);
  if i <> aFromIndex then
    system.Delete(Src, i, aFromIndex-i+1);
end;

function TTargetLazarusProject.GetProjectFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Project.Filename));
end;

function TTargetLazarusProject.GetFolderGameHelperFiles: string;
begin
  Result := GetProjectFolder + 'GameHelperFiles' + DirectorySeparator;
end;

function TTargetLazarusProject.GetFolderUnits: string;
begin
  Result := GetProjectFolder + 'Units' + DirectorySeparator;
end;

function TTargetLazarusProject.GetFolderUnitsSprites: string;
begin
  Result := GetFolderUnits + 'Sprites' + DirectorySeparator;
end;

function TTargetLazarusProject.GetFolderUnitsLevels: string;
begin
  Result := GetFolderUnits + 'Levels' + DirectorySeparator;
end;

function TTargetLazarusProject.GetFolderBinary: string;
begin
  Result := GetProjectFolder + 'Binary' + DirectorySeparator;
end;

function TTargetLazarusProject.GetFolderData: string;
begin
  Result := GetFolderBinary + 'Data' + DirectorySeparator;
end;

function TTargetLazarusProject.GetFolderDataTextures: string;
begin
  Result := GetFolderData + 'Textures' +DirectorySeparator;
end;

function TTargetLazarusProject.GetFilenameGameLevels: string;
begin
  Result := GetFolderUnitsLevels + LEVEL_UNIT_NAME+'.pas';
end;

function TTargetLazarusProject.GetFilenameProject_Config: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Project.Filename)) +
            'Units' + DirectorySeparator + 'project_config.cfg';
end;

function TTargetLazarusProject.GetFilenameProject_LPR: string;
begin
  Result := ChangeFileExt(Project.Filename, '.lpr');
end;

function TTargetLazarusProject.GetFilenameProject_LPI: string;
begin
  Result := ChangeFileExt(Project.Filename, '.lpi');
end;

function TTargetLazarusProject.LPI_ReadUnitsSection: TStringArray;
var doc: TXMLDocument;
  projectoptionNode, unitsNode, childUnit, childFileName, childValue: TDOMNode;
begin
  Result := NIL;
  if not FileExists(GetFilenameProject_LPI) then raise exception.create('Project LPI file not found');
  ReadXMLFile(doc, GetFilenameProject_LPI);
  try
    projectoptionNode := doc.DocumentElement.FindNode('ProjectOptions');
    if projectoptionNode = NIL then raise exception.create('section ProjectOptions not found');

    unitsNode := projectoptionNode.FindNode('Units');
    if unitsNode = NIL then raise exception.create('section UNITS not found');

    childUnit := unitsNode.FirstChild;
    while Assigned(childUnit) do begin
      childFileName := childUnit.FindNode('Filename');
      if Assigned(childFileName) then begin
        childValue := childFileName.Attributes.GetNamedItem('Value');
        if Assigned(childValue) then begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)] := childValue.NodeValue;
        end;
      end;
      childUnit := childUnit.NextSibling;
    end;

  finally
    doc.Free;
  end;
end;

procedure TTargetLazarusProject.LPI_AddUnit(const aUnitName: string;
  aUnitLocation: TUnitLocation; aUnitExtension: TUnitExtension);
var doc: TXMLDocument;
  projectoptionNode, unitsNode, newUnitNode, newFilenameNode, newIsPartOfProjectNode: TDOMNode;
  fullname: string;
begin
  if LPI_CheckIfHaveUnit(aUnitName, aUnitLocation, aUnitExtension) then exit;

  fullname := LPI_ExpandUnitName(aUnitName, aUnitLocation, aUnitExtension);

  ReadXMLFile(doc, GetFilenameProject_LPI);
  try
    projectoptionNode := doc.DocumentElement.FindNode('ProjectOptions');
    if projectoptionNode = NIL then raise exception.create('section ProjectOptions not found');

    unitsNode := projectoptionNode.FindNode('Units');
    if unitsNode = NIL then raise exception.create('section UNITS not found');

    newUnitNode := doc.CreateElement('Unit');
    unitsNode.AppendChild(newUnitNode);

    newFilenameNode := doc.CreateElement('Filename');
    TDOMElement(newFilenameNode).SetAttribute('Value', fullname);
    newUnitNode.AppendChild(newFilenameNode);

    newIsPartOfProjectNode := doc.CreateElement('IsPartOfProject');
    TDOMElement(newIsPartOfProjectNode).SetAttribute('Value', 'True');
    newUnitNode.AppendChild(newIsPartOfProjectNode);

    writeXMLFile(doc, GetFilenameProject_LPI);
  finally
    doc.Free;
  end;
end;

procedure TTargetLazarusProject.LPI_RemoveUnit(const aUnitName: string;
  aUnitLocation: TUnitLocation; aUnitExtension: TUnitExtension);
var doc: TXMLDocument;
  projectoptionNode, unitsNode, childUnit, childFileName, childValue: TDOMNode;
  fullname: string;
begin
  if not LPI_CheckIfHaveUnit(aUnitName, aUnitLocation, aUnitExtension) then exit;

  fullname := LPI_ExpandUnitName(aUnitName, aUnitLocation, aUnitExtension);

  ReadXMLFile(doc, GetFilenameProject_LPI);
  try
    projectoptionNode := doc.DocumentElement.FindNode('ProjectOptions');
    if projectoptionNode = NIL then raise exception.create('section ProjectOptions not found');

    unitsNode := projectoptionNode.FindNode('Units');
    if unitsNode = NIL then raise exception.create('section UNITS not found');

    childUnit := unitsNode.FirstChild;
    while Assigned(childUnit) do begin
      childFileName := childUnit.FindNode('Filename');
      if Assigned(childFileName) then begin
        childValue := childFileName.Attributes.GetNamedItem('Value');
        if Assigned(childValue) then begin
          if childValue.NodeValue = fullname then begin
            unitsNode.RemoveChild(childUnit);
            writeXMLFile(doc, GetFilenameProject_LPI);
            break;
          end;
        end;
      end;
      childUnit := childUnit.NextSibling;
    end;

  finally
    doc.Free;
  end;
end;

function TTargetLazarusProject.LPI_CheckIfHaveUnit(const aUnitName: string;
  aUnitLocation: TUnitLocation; aUnitExtension: TUnitExtension): boolean;
var A: TStringArray;
  i: integer;
  fullname: string;
begin
  Result := False;
  A := LPI_ReadUnitsSection;
  if Length(A) = 0 then raise exception.create('LPI_ReadUnitsSection have returned 0 items');

  fullname := LPI_ExpandUnitName(aUnitName, aUnitLocation, aUnitExtension);

  for i:=0 to High(A) do
    if A[i] = fullname then exit(True);
end;

function TTargetLazarusProject.LPI_ExpandUnitName(const aUnitName: string;
  aUnitLocation: TUnitLocation; aUnitExtension: TUnitExtension): string;
begin
  Result := UNIT_SUBFOLDER[aUnitLocation] + aUnitName + UNIT_EXT[aUnitExtension];
end;

function TTargetLazarusProject.StringList_CreateFromFile(const aFilename: string): TStringList;
begin
  if not FileExists(aFilename)
    then raise exception.Create('File "'+aFilename+'" not found !')
    else begin
      Result := TStringList.Create;
      Result.LoadFromFile(aFilename);
    end;
end;

function TTargetLazarusProject.StringList_SearchLineThatContain(t: TStringList;
  const aStringToSearch: string; aSearchFromLineIndex: integer; aSearchToLineIndex: integer): integer;
var i: integer;
begin
  if aSearchToLineIndex > t.Count-1 then
    aSearchToLineIndex := t.Count-1;

  for i:=aSearchFromLineIndex to aSearchToLineIndex do
    if Pos(aStringToSearch, t.Strings[i]) <> 0 then exit(i);
  Result := -1;
end;

procedure TTargetLazarusProject.StringList_RemoveUnitNameOnLine(t: TStringList;
  aLineIndex: integer; const aUnitName: string);
var i, icoma: integer;
  s: string;
begin
  s := t.Strings[aLineIndex];
  i := Pos(aUnitName, s);
  system.Delete(s, i, Length(aUnitName)); // delete the unit name

  // search next coma
  if FindIndexOfNextChar(s, ',', i, icoma) then begin
    system.Delete(s, i, icoma-i); // delete the next coma
    RemoveNextConsecutiveChar(s, ' ', i);
    t.Strings[aLineIndex] := s;
  end
  else
  if FindIndexOfPreviousChar(s, ',', i, icoma) then begin
    system.Delete(s, icoma, i-icoma); // delete the previous coma
    RemovePreviousConsecutiveChar(s, ' ', icoma-1);
    t.Strings[aLineIndex] := s;
  end
  else
  if Pos(';', s) <> 0 then begin
    //delete the line
    t.Delete(aLineIndex);
    dec(aLineIndex);
    s := t.Strings[aLineIndex];
    if not FindIndexOfPreviousChar(s, ',', Length(s), icoma) then raise exception.create('bug');
    system.Delete(s, icoma, 1);
    RemovePreviousConsecutiveChar(s, ' ', icoma-1);
    t.Strings[aLineIndex] := s+';';
  end;
end;

procedure TTargetLazarusProject.StringList_ReplaceAllOccurrencesOf(
  t: TStringList; const aOldValue, aNewValue: string);
var i: integer;
begin
  for i:=0 to t.Count-1 do
    t.Strings[i] := t.Strings[i].Replace(aOldValue, aNewValue, [rfReplaceAll]);
end;

function TTargetLazarusProject.ConfigOption_IsActive(const aFilename, aOptionName: string): boolean;
var line: integer;
  t: TStringList;
  s: string;
begin
  t := StringList_CreateFromFile(aFilename);
  try
    line := StringList_SearchLineThatContain(t, aOptionName, 0);
    if line = -1 then raise exception.create('line with option '+aOptionName+' not found!');
    s := t.Strings[line];
    RemoveNextConsecutiveChar(s, ' ', 1);
    Result := Copy(s, 1, 2) <> '//';
  finally
    t.Free;
  end;
end;

procedure TTargetLazarusProject.ConfigOption_SetActive(const aFilename, aOptionName: string; aValue: boolean);
var line: integer;
  t: TStringList;
  s: string;
begin
  t := StringList_CreateFromFile(aFilename);
  try
    line := StringList_SearchLineThatContain(t, aOptionName, 0);
    if line = -1 then raise exception.create('line with option '+aOptionName+' not found!');
    s := t.Strings[line];
    RemoveNextConsecutiveChar(s, ' ', 1);
    if aValue then begin
      while s[1] = '/' do system.Delete(s, 1, 1);
    end else begin
      if Copy(s, 1, 2) <> '//' then s := '//'+s;
    end;
    t.Strings[line] := s;
    t.SaveToFile(aFilename);
  finally
    t.Free;
  end;
end;

function TTargetLazarusProject.ConfigOption_GetMaximizeSceneOnMonitor: boolean;
begin
  Result := ConfigOption_IsActive(GetFilenameProject_Config, 'MAXIMIZE_SCENE_ON_MONITOR');
end;

procedure TTargetLazarusProject.ConfigOption_SetMaximizeSceneOnMonitor(AValue: boolean);
begin
  ConfigOption_SetActive(GetFilenameProject_Config, 'MAXIMIZE_SCENE_ON_MONITOR', AValue);
end;

function TTargetLazarusProject.ConfigOption_GetWindowedMode: boolean;
begin
  Result := ConfigOption_IsActive(GetFilenameProject_Config, 'WINDOWED_MODE');
end;

procedure TTargetLazarusProject.ConfigOption_SetWindowedMode(AValue: boolean);
begin
  ConfigOption_SetActive(GetFilenameProject_Config, 'WINDOWED_MODE', AValue);
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

function TTargetLazarusProject.Unit_USES_CheckIfHaveEntry(t: TStringList;
  const aUnitNameToCheck: string; aCheckInInterface, aCheckInImplementation: boolean): boolean;
var usesLine, implementationLine, semicolonLine, i: integer;
begin
  Result := True;
  if aUnitNameToCheck = '' then raise exception.create('unit name to check is empty');

  Result := False;
  implementationLine := StringList_SearchLineThatContain(t, 'implementation', 0);
  if implementationLine = -1 then raise exception.create('implementation not found! unit malformed');

  if aCheckInInterface then begin
    usesLine := StringList_SearchLineThatContain(t, 'uses', 0);
    if (usesLine <> -1) and (usesLine < implementationLine) then begin
      semicolonLine := StringList_SearchLineThatContain(t, ';', usesLine);
      if semicolonLine <> -1 then begin
        i := StringList_SearchLineThatContain(t, aUnitNameToCheck, usesLine, semicolonLine);
        if i <> -1 then exit(True);
      end;
    end;
  end;

  if aCheckInImplementation then begin
    usesLine := StringList_SearchLineThatContain(t, 'uses', implementationLine);
    if usesLine = -1 then exit(False);
    semicolonLine := StringList_SearchLineThatContain(t, ';', usesLine);
    if semicolonLine <> -1 then begin
      i := StringList_SearchLineThatContain(t, aUnitNameToCheck, usesLine, semicolonLine);
      if i <> -1 then exit(True);
    end;
  end;
end;

procedure TTargetLazarusProject.Unit_USES_AddEntry(const aTargetUnit,
  aUnitNameToAdd: string; aAddInInterface: boolean);
var usesLine, implementationLine, semicolonLine, i: integer;
  t: TStringList;
  s: string;
begin
  if not FileExists(aTargetUnit) then raise exception.create('target unit not found');
  if aUnitNameToAdd = '' then raise exception.create('unit name to add is empty');

  t := StringList_CreateFromFile(aTargetUnit);
  try

    // check if unit name is already present
    if aAddInInterface then begin
      if Unit_USES_CheckIfHaveEntry(t, aUnitNameToAdd, False, True) then
        Unit_USES_RemoveEntry(aTargetUnit, aUnitNameToAdd, False, True);
      if Unit_USES_CheckIfHaveEntry(t, aUnitNameToAdd, True, False) then exit;
    end else begin
      if Unit_USES_CheckIfHaveEntry(t, aUnitNameToAdd, True, False) then
        Unit_USES_RemoveEntry(aTargetUnit, aUnitNameToAdd, True, False);
      if Unit_USES_CheckIfHaveEntry(t, aUnitNameToAdd, False, True) then exit;
    end;

    // want in implementation area ?
    if not aAddInInterface then begin
      implementationLine := StringList_SearchLineThatContain(t, 'implementation',  0);
      if implementationLine = -1 then raise exception.create('section "implementation" not found! unit malformed');
      usesLine := StringList_SearchLineThatContain(t, 'uses',  implementationLine);
      if usesLine = -1 then begin
        // create uses section in implementation
        t.Insert(implementationLine+1, '');
        t.Insert(implementationLine+1, 'uses '+aTargetUnit+';');
        t.SaveToFile(aTargetUnit);
        exit;
      end;
    end else begin
      usesLine := StringList_SearchLineThatContain(t, 'uses', 0);
      if usesLine = -1 then raise exception.create('interface uses clause not found!');
    end;

    // search next line that contain semicolon (can be on same line than USES)
    semicolonLine := StringList_SearchLineThatContain(t, ';', usesLine);
    if semicolonLine = -1 then raise exception.create('semicolon of end of USES not found! unit malformed');
    s := t.Strings[semicolonLine];
    i := Pos(';', s);
    if i < 60 then begin
      system.Insert(', '+aUnitNameToAdd, s, i);
      t.Strings[semicolonLine] := s;
    end else begin
      // add a new line to avoid very long line
      s[i] := ',';
      t.Strings[semicolonLine] := s;
      t.Insert(semicolonLine+1, aUnitNameToAdd+';');
    end;
    t.SaveToFile(aTargetUnit);
  finally
    t.Free;
  end;
end;

procedure TTargetLazarusProject.Unit_USES_RemoveEntry(const aTargetUnit,
  aUnitNameToRemove: string; aRemoveInInterface, aRemoveInImplementation: boolean);
var implementationLine, line: integer;
  t: TStringList;
begin
  if not FileExists(aTargetUnit) then raise exception.create('target unit not found');
  if aUnitNameToRemove = '' then raise exception.create('unit name to remove is empty');

  t := StringList_CreateFromFile(aTargetUnit);
  try

    // remove in interface area
    if aRemoveInInterface then begin
      implementationLine := StringList_SearchLineThatContain(t, 'implementation',  0);
      if implementationLine = -1 then raise exception.create('section "implementation" not found! unit malformed');
      line := StringList_SearchLineThatContain(t, aUnitNameToRemove, 0, implementationLine);
      if line <> -1 then begin
        StringList_RemoveUnitNameOnLine(t, line, aUnitNameToRemove);
        t.SaveToFile(aTargetUnit);
      end;
    end;

    // remove in implementation area
    if aRemoveInImplementation then begin
      implementationLine := StringList_SearchLineThatContain(t, 'implementation',  0);
      if implementationLine = -1 then raise exception.create('section "implementation" not found! unit malformed');
      line := StringList_SearchLineThatContain(t, aUnitNameToRemove, implementationLine);
      if line <> -1 then begin
        StringList_RemoveUnitNameOnLine(t, line, aUnitNameToRemove);
        t.SaveToFile(aTargetUnit);
      end;
    end;
  finally
    t.Free;
  end;
end;

procedure TTargetLazarusProject.Unit_ReplaceAllOccurrencesOf(
  const aTargetUnitFilename, aOldValue, aNewValue: string);
var t: TStringList;
begin
  t := StringList_CreateFromFile(aTargetUnitFilename);
  try
    StringList_ReplaceAllOccurrencesOf(t, aOldValue, aNewValue);
    t.SaveToFile(aTargetUnitFilename);
  finally
    t.Free;
  end;
end;

function TTargetLazarusProject.Unit_GetFullFilename(const aObjectName: string;
  aUnitLocation: TUnitLocation; aUnitExtension: TUnitExtension): string;
begin
  case aUnitLocation of
    ulUnits: Result := GetFolderUnits;
    ulSprites: Result := GetFolderUnitsSprites;
    ulLevels: Result := GetFolderUnitsLevels;
    else raise exception.create('forget to implement');
  end;
  Result := Result + UNIT_NAME_PREFIX[aUnitLocation] +
                     LowerCase(aObjectName) +
                     UNIT_EXT[aUnitExtension];
end;

procedure TTargetLazarusProject.Unit_AddToProject(const aObjectName: string;
  aUnitLocation: TUnitLocation; aUnitExtension: TUnitExtension);
{var t: TStringList;
  usesLine, semicolonLine, i: integer;
  s: string;}
begin
  if aObjectName = '' then raise exception.create('unit name to add is empty');

  IdeConnect.AskIdeToAddUnitToProject(aObjectName, aUnitLocation, aUnitExtension);

  // two things:
  //  - add unit name in USES clause of LPR file
  //  - add XML entry in LPI file

{
  t := StringList_CreateFromFile(GetFilenameProject_LPR);
  try
    usesLine := StringList_SearchLineThatContain(t, 'uses',  0);
    semicolonLine := StringList_SearchLineThatContain(t, ';',  usesLine);
    i := StringList_SearchLineThatContain(t, aUnitName, usesLine, semicolonLine);
    if i <> -1 then exit; // already present

    s := t.Strings[semicolonLine];
    i := Pos(';', s);
    s[i] := ',';
    if i < 60 then begin
      s := s+' '+aUnitName+';';
      t.Strings[semicolonLine] := s;
    end else begin
      // add a new line
      t.Strings[semicolonLine] := s;
      t.Insert(semicolonLine+1, '  '+aUnitName+';');
    end;
    t.SaveToFile(GetFilenameProject_LPR);
  finally
    t.Free;
  end;

  //LPI_AddUnit(aUnitName, aUnitLocation, aUnitExtension);  }
end;

procedure TTargetLazarusProject.Unit_RemoveFromProject(const aObjectName: string;
  aUnitLocation: TUnitLocation; aUnitExtension: TUnitExtension);
{var t: TStringList;
  lineIndex: integer;  }
begin
  if aObjectName = '' then raise exception.create('unit name to remove is empty');

  IdeConnect.AskIdeToRemoveUnitFromProject(aObjectName, aUnitLocation, aUnitExtension);

{  // remove entry in USES clause of LPR file
  t := StringList_CreateFromFile(GetFilenameProject_LPR);
  try
    lineIndex := StringList_SearchLineThatContain(t, aUnitName, 0);
    if lineIndex <> -1 then begin
      StringList_RemoveUnitNameOnLine(t, lineIndex, aUnitName);
      t.SaveToFile(GetFilenameProject_LPR);
    end;
  finally
    t.Free;
  end;

  // remove from LPI file
  //LPI_RemoveUnit(aUnitName, aUnitLocation, aUnitExtension);   }
end;

procedure TTargetLazarusProject.Unit_DeleteFile(const aObjectName: string;
  aUnitLocation: TUnitLocation; aUnitExtension: TUnitExtension);
var unitFilename: String;
begin
  //if GameHelperIsStartedFromLazarusIDE then exit; // the deletion is done in the IDE package

  unitFilename := Unit_GetFullFilename(aObjectName, aUnitLocation, aUnitExtension);

  if FileExists(unitFilename) then
    SupprimeFichier(unitFilename);
end;

function TTargetLazarusProject.SpriteNameToUnitNameWithoutExt(const aSpriteName: string): string;
begin
  Result := UNIT_NAME_PREFIX[ulSprites] + LowerCase(aSpriteName);
end;

function TTargetLazarusProject.SpriteNameToUnitFullFilename(const aSpriteName: string): string;
begin
  Result := GetFolderUnitsSprites + SpriteNameToUnitNameWithoutExt(aSpriteName)+'.pas';
end;

end.

