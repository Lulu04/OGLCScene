unit u_respons_commontype;

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


ITEM_RESPONS_HEADER = '[RESPONS_FROM_GAME_HELPER]';

// header as first command line parameter
CI_PARAMETER_HEADER = '[GameHelper_Called_By_IDE]';

type
TTypeOfResponsToIde = (rtUnknown,
                       rtAddUnitToProject,
                       rtRemoveUnitFromProject,
                       rtTerminated);  // user have close Game Helper

{ TResponsItem }

TResponsItem = record
  ResponsType: TTypeOfResponsToIde;
  ObjectName: string;
  UnitLocation: TUnitLocation;
  UnitExtension: TUnitExtension;

  function SaveFieldsToString: string;
  procedure LoadFieldsFromString(const data: string);
  procedure BuildResponsTo(t: TStringList);

  // i.e. for a sprite named 'Car' return 'Units\Sprites\u_sprite_car.pas'
  function ExpandToShortFilename: string;
  // i.e. for a sprite named 'Car' return 'u_sprite_car.pas'
  function ExpandToUnitNameWithExt: string;
  // i.e. for a sprite named 'Car' return 'u_sprite_car'
  function ExpandToUnitNameWithoutExt: string;
end;

implementation
uses OGLCScene, LazFileUtils;

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
  Result := UNIT_NAME_PREFIX[UnitLocation] + LowerCase(ObjectName);
end;


end.

