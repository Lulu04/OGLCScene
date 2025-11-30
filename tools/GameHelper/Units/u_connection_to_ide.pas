unit u_connection_to_ide;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils;

{

  a respons have two lines in a TStringList:
  [RESPONS_FROM_GAME_HELPER]   <- the header of a respons, next line is the data
  Type|xxx|UnitName|xxxxxxx|UnitLocation|xxx|UnitExtension|xxx


  structure of command line when Game Helper is launched from the IDE:
    <executable> <path_where_to_write_the_respons> <full_oglc_project_filename>
}
type

TUnitLocation = (ulUnits, ulSprites, ulLevels);
TUnitExtension = (uePas, ueInc, ueLpr);
const
UNIT_SUBFOLDER: array[TUnitLocation] of string = ('Units\', 'Units\Sprites\', 'Units\Levels\');
UNIT_EXT: array[TUnitExtension] of string = ('.pas', '.inc', '.lpr');
// the prefix to add to the object name to have the unit name without extension
UNIT_NAME_PREFIX: array[TUnitLocation] of string = ('', 'u_sprite_', '');

RESPONS_FILENAME = 'oglcscene_game_helper_respons.txt';
ITEM_RESPONS_HEADER = '[RESPONS_FROM_GAME_HELPER]';

// header as first command line parameter
CI_PARAMETER_HEADER = '[GameHelper_Called_By_IDE]';

GAMEHELPERFILES_SUBFOLDER = 'GameHelperFiles';

type
TTypeOfResponsToIde = (rtUnknown,
                       rtAddUnitToProject,
                       rtRemoveUnitFromProject,
                       rtTerminated);  // user have close Game Helper

{ TResponsItem }

TResponsItem = record
  ResponsType: TTypeOfResponsToIde;
  ObjectName: string; // the name of the object without prefix, without extension
  UnitLocation: TUnitLocation;
  UnitExtension: TUnitExtension;

  function SaveFieldsToString: string;
  procedure LoadFieldsFromString(const data: string);
  procedure SendToIDE;


  procedure BuildResponsTo(t: TStringList);

  // i.e. for a sprite named 'Car' return 'Units\Sprites\u_sprite_car.pas'
  function ExpandToShortFilename: string;
  // i.e. for a sprite named 'Car' return 'u_sprite_car.pas'
  function ExpandToUnitNameWithExt: string;
  // i.e. for a sprite named 'Car' return 'u_sprite_car'
  function ExpandToUnitNameWithoutExt: string;
end;


// util to send command to the ide
{ TConnectionToIDE }

TConnectionToIDE = record
private
  FTargetLazarusProjectFilename,
  FTargetOGLCProjectFilename: string;
  FActivated: boolean;
public
  procedure CheckCommandLine;
  procedure AskIdeToAddUnitToProject(const aObjectName: string;
                                     aUnitLocation: TUnitLocation;
                                     aUnitExtension: TUnitExtension);
  procedure AskIdeToRemoveUnitFromProject(const aObjectName: string;
                                          aUnitLocation: TUnitLocation;
                                          aUnitExtension: TUnitExtension);
  procedure InformIDEThatGameHelperTerminate;

  property TargetLazarusProjectFilename: string read FTargetLazarusProjectFilename;
  property TargetOGLCProjectFilename: string read FTargetOGLCProjectFilename;
  // True if game helper is started from the Lazarus IDE
  property Activated: boolean read FActivated;
end;

var IdeConnect: TConnectionToIDE;

// return True if game helper is started from the Lazarus IDE
function GameHelperIsStartedFromLazarusIDE: boolean;

implementation
uses Forms, Dialogs,  LazFileUtils, OGLCScene, u_common, u_utils;

function GameHelperIsStartedFromLazarusIDE: boolean;
begin
  Result := IdeConnect.Activated;
end;

{ TConnectionToIDE }

procedure TConnectionToIDE.CheckCommandLine;
var header: string;
begin
  header := '';
  FTargetLazarusProjectFilename := '';
  FTargetOGLCProjectFilename := '';

  // retrieve the parameters on the command line
  if Application.ParamCount >= 2 then begin
    FScene.LogInfo('Found parameters on command line');
    header := Application.Params[1];
    FTargetLazarusProjectFilename := Application.Params[2];
    FTargetOGLCProjectFilename := ChangeFileExt(FTargetLazarusProjectFilename, '.oglc');

    // if the oglc project file doesn't exists, create an empty one
    if not FileExistsUTF8(FTargetOGLCProjectFilename) then begin
      FScene.LogInfo('OGLC project not found, try to create an empty one '+FTargetOGLCProjectFilename);
      SaveEmptyOGLCProject(FTargetOGLCProjectFilename);
    end;
  end;

  FActivated := (header = CI_PARAMETER_HEADER) and
                FileExistsUTF8(FTargetLazarusProjectFilename) and
                FileExistsUTF8(FTargetOGLCProjectFilename);

  if FActivated then begin
    FScene.LogInfo('Game Helper was started by IDE');
    FScene.LogInfo('Lazarus project: '+FTargetLazarusProjectFilename, 1);
    FScene.LogInfo('OGLC project: '+FTargetOGLCProjectFilename, 1);
  end else begin
    FScene.LogInfo('Game Helper started by user');
    FScene.LogInfo('communication with IDE is disabled', 1);
  end;
end;

procedure TConnectionToIDE.AskIdeToAddUnitToProject(const aObjectName: string;
  aUnitLocation: TUnitLocation; aUnitExtension: TUnitExtension);
var mess: TResponsItem;
begin
  if not FActivated then exit;

  mess := Default(TResponsItem);
  mess.ResponsType := rtAddUnitToProject;
  mess.ObjectName := aObjectName;
  mess.UnitLocation := aUnitLocation;
  mess.UnitExtension := aUnitExtension;

  //mess.BuildResponsTo(FResponsBuffer);
  mess.SendToIDE;
end;

procedure TConnectionToIDE.AskIdeToRemoveUnitFromProject(const aObjectName: string;
  aUnitLocation: TUnitLocation; aUnitExtension: TUnitExtension);
var mess: TResponsItem;
begin
  if not FActivated then exit;

  mess := Default(TResponsItem);
  mess.ResponsType := rtRemoveUnitFromProject;
  mess.ObjectName := aObjectName;
  mess.UnitLocation := aUnitLocation;
  mess.UnitExtension := aUnitExtension;

  //mess.BuildResponsTo(FResponsBuffer);
  mess.SendToIDE;
end;

procedure TConnectionToIDE.InformIDEThatGameHelperTerminate;
var mess: TResponsItem;
begin
  if not FActivated then exit;

  mess := Default(TResponsItem);
  mess.ResponsType := rtTerminated;
  mess.SendToIDE;
end;

function TResponsItem.SaveFieldsToString: string;
var prop: TProperties;
begin
  prop.Init('|');
  prop.Add('Type', Ord(ResponsType));
  prop.Add('ObjectName', ObjectName);
  prop.Add('UnitLocation', Ord(UnitLocation));
  prop.Add('UnitExtension', Ord(UnitExtension));
  Result := prop.PackedProperty;
end;

procedure TResponsItem.LoadFieldsFromString(const data: string);
var prop: TProperties;
  v: integer;
  flag: boolean;
begin
  v := 0;
  flag := True;
  prop.Split(data, '|');
  flag := flag and prop.IntegerValueOf('Type', v, 0);
  ResponsType := TTypeOfResponsToIde(v);

  flag := flag and prop.StringValueOf('ObjectName', ObjectName, '');

  flag := flag and prop.IntegerValueOf('UnitLocation', v, 0);
  UnitLocation := TUnitLocation(v);

  flag := flag and prop.IntegerValueOf('UnitExtension', v, 0);
  UnitExtension := TUnitExtension(v);

  if not flag then ResponsType := rtUnknown;
end;

procedure TResponsItem.SendToIDE;
begin
  writeln(SaveFieldsToString);
end;

procedure TResponsItem.BuildResponsTo(t: TStringList);
begin
  t.Add(ITEM_RESPONS_HEADER);
  t.Add(SaveFieldsToString);
end;

function TResponsItem.ExpandToShortFilename: string;
begin
  Result := UNIT_SUBFOLDER[UnitLocation] + ExpandToUnitNameWithExt;
  // adjust \/ for the platform
  Result := GetForcedPathDelims(Result);
end;

function TResponsItem.ExpandToUnitNameWithExt: string;
begin
  Result := ExpandToUnitNameWithoutExt + UNIT_EXT[UnitExtension];
end;

function TResponsItem.ExpandToUnitNameWithoutExt: string;
begin
  Result := UNIT_NAME_PREFIX[UnitLocation] + ObjectName;
end;

end.

