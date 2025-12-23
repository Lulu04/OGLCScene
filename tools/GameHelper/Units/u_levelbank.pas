unit u_levelbank;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  OGLCScene, gvector,
  BGRABitmap, BGRABitmapTypes,
  u_undo_redo, u_texture_list, u_layerlist;

//  Bank level have a single texture list shared by all level items.

type

{ TWorldInfo }

TWorldInfo = record
  x, y, width, height: single;
  boundscolor: TBGRAPixel;
  showbounds: boolean;
  useskygradient: boolean;
  skyx, skyy: single;
  skywidth, skyheight, skylayer: integer;
  skygradientdata: string;
  procedure InitDefault;
  function SaveToString(aSaveAllProperties: boolean=True): string;
  procedure LoadFromString(const s: string);
end;

{ TLevelBankItem }

PLevelBankItem = ^TLevelBankItem;
TLevelBankItem = record
  name,        // the name of the level
  description, // the description of the level to display in game
  worldinfo,
  layerloopinfo,
  surfaces : string;
  procedure InitDefault;
  procedure DuplicateTo(aItem: PLevelBankItem);

  // return an array with the index of user layer used in this level
  function GetUserLayerIndexesUsed: TArrayOfInteger;
  // called when user shift the index of a layer
  procedure ExchangeLayerIndexInAllSurfaces(Index1, Index2: integer);
  // called when user delete a layer
  procedure DecreaseLayerIndexGreaterOrEqualThan(Index: integer);
  // export pascal code to declare the level as constant
  procedure ExportToPascalConst(t: TStringList; const aGroupName: string; aTextures: TTexturelist);
end;

{ TLevelGroup }

TLevelGroup = class(specialize TVector<TLevelBankItem>)
private
  FGroupName, FGroupDescription: string;
public
  Textures: TTextureList;
  constructor Create;
  destructor Destroy; override;

  function AddEmpty: PLevelBankItem;
  function NameExists(const aName: string): boolean;
  function GetItemByName(const aName: string): PLevelBankItem;
  procedure DeleteByIndex(aIndex: integer);
  procedure DeleteByName(const aName: string);
  // return true if the layer index is used by a level
  function UseThisLayer(Index: integer): boolean;

  procedure Clear; reintroduce;

  function SaveToString: string;
  procedure LoadFromString(const data: string);
  property GroupName: string read FGroupName write FGroupName;
  property GroupDescription: string read FGroupDescription write FGroupDescription;
end;

TLevelBank = class(specialize TVector<TLevelGroup>)
  destructor Destroy; override;

  function GroupNameExists(const aName: string): boolean;
  function ThereIsAtLeastOneLevelDefined: boolean;

  function AddGroup(const aGroupName: string): TLevelGroup;
  function IndexOfGroup(const aGroupName: string): integer;
  procedure DeleteGroupByName(const aGroupName: string);
  procedure RenameGroupByName(const aCurrentName, aNewName: string);
  procedure Clear; reintroduce;

  function GetGroupByName(const aGroupName: string): TLevelGroup;

  // called when user shift the index of a layer
  procedure ExchangeLayerIndexInAllSurfaces(Index1, Index2: integer);
  // return true if the layer index is used by a level
  function UseThisLayer(Index: integer): boolean;
  // called when used delete a layer
  procedure DecreaseLayerIndexGreaterOrEqualThan(aIndex: integer);

  function LevelCountInAllGroups: integer;

  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);
  // save into file \GameHelperSave\LevelBank.oglc
  procedure SaveToPath(const aPath: string);
  procedure LoadFromPath(const aPath: string);
  // save in folder GameHelperFiles\
  procedure Save;

  procedure ExportToFileGameLevel;
end;

var LevelBank: TLevelBank;
    WorkingLevelGroup: TLevelGroup=NIL;


type
TLevelBankUndoRedoActionType = (uratLevelBankUndefined,
                           uratLevelBankDeleteItem, // user delete a level
                           uratLevelBankRenameItem, // user rename a level
                           uratLevelBankAddItem     // user duplicate a level
                          );
TLevelBankUndoRedoItem = record
  action: TLevelBankUndoRedoActionType;
  data: TLevelBankItem;
  newData: TLevelBankItem;
  oldName: string;
end;

{ TLevelBankUndoRedoManager }

TLevelBankUndoRedoManager = class(specialize TGenericUndoRedoManager<TLevelBankUndoRedoItem>)
  procedure ProcessUndo(var aItem: TLevelBankUndoRedoItem); override;
  procedure ProcessRedo(var aItem: TLevelBankUndoRedoItem); override;

  procedure AddActionDeleteLevel(aIndex: integer);
  procedure AddActionRenameLevel(aIndex: integer; const aOldName: string);
  procedure AddActionDuplicateLevel(aIndexNewItem: integer);
end;

implementation

uses u_common, u_surface_list, u_utils, u_project, LazFileUtils;

{ TWorldInfo }

procedure TWorldInfo.InitDefault;
begin
  Self := Default(TWorldInfo);
  useskygradient := True;
  skygradientdata := DEFAULT_GRADIENT;
end;

function TWorldInfo.SaveToString(aSaveAllProperties: boolean): string;
var prop: TProperties;
begin
  prop.Init('|');
  prop.Add('WorldX', x);
  prop.Add('WorldY', y);
  prop.Add('WorldWidth', width);
  prop.Add('WorldHeight', height);
  if aSaveAllProperties then begin
    prop.Add('WorldBoundsColor', boundscolor);
    prop.Add('WorldShowBounds', showbounds);
  end;

  prop.Add('UseSkyGradient', useskygradient);
  if useskygradient then begin
    prop.Add('SkyX', skyx);
    prop.Add('SkyY', skyy);
    prop.Add('SkyLayer', skylayer);
    prop.Add('SkyWidth', skywidth);
    prop.Add('SkyHeight', skyheight);
    prop.Add('SkyGradientData', skygradientdata);
  end;

  Result := prop.PackedProperty;
end;

procedure TWorldInfo.LoadFromString(const s: string);
var prop: TProperties;
begin
  prop.Split(s, '|');
  prop.SingleValueOf('WorldX', x, 0);
  prop.SingleValueOf('WorldY', y, 0);
  prop.SingleValueOf('WorldWidth', width, FScene.Width);
  prop.SingleValueOf('WorldHeight', height, FScene.Height);
  prop.BGRAPixelValueOf('WorldBoundsColor', boundscolor, BGRA(255,0,0));
  prop.BooleanValueOf('WorldShowBounds', showbounds, True);
  prop.BooleanValueOf('UseSkyGradient', useskygradient, False);
  prop.IntegerValueOf('SkyLayer', skylayer, Layers.Count-1);
  prop.SingleValueOf('SkyX', skyx, 0.0);
  prop.SingleValueOf('SkyY', skyy, 0.0);
  prop.IntegerValueOf('SkyWidth', skywidth, 1);
  prop.IntegerValueOf('SkyHeight', skyheight, 1);
  prop.StringValueOf('SkyGradientData', skygradientdata, DEFAULT_GRADIENT);
end;

{ TLevelBankItem }

procedure TLevelBankItem.InitDefault;
begin
  Self := Default(TLevelBankItem);
end;

procedure TLevelBankItem.DuplicateTo(aItem: PLevelBankItem);
begin
  aItem^.name := name;
  aItem^.worldinfo := worldinfo;
  aItem^.surfaces := surfaces;
  aItem^.layerloopinfo := layerloopinfo;
end;

function TLevelBankItem.GetUserLayerIndexesUsed: TArrayOfInteger;
var i: integer;
  sl: TSurfaceList;
begin
  Result := NIL;
  sl := TSurfaceList.Create;
  sl.SetModeForLevelEditor;
  try
    sl.LoadFromString(surfaces, False);
    if sl.Size > 0 then
      for i:=0 to sl.Size-1 do
        Result.AddOnlyOneTime(sl.Mutable[i]^.layerindex-APP_LAYER_COUNT);
  finally
    sl.Free;
  end;
end;

procedure TLevelBankItem.ExchangeLayerIndexInAllSurfaces(Index1, Index2: integer);
var i: integer;
  sl: TSurfaceList;
begin
  Index1 := Index1 + APP_LAYER_COUNT;
  Index2 := Index2 + APP_LAYER_COUNT;
  sl := TSurfaceList.Create;
  sl.SetModeForLevelEditor;
  try
    sl.LoadFromString(surfaces, False);
    if sl.Size > 0 then begin
      for i:=0 to sl.Size-1 do
        with sl.Mutable[i]^ do
          if layerindex = Index1 then layerindex := Index2
            else if layerindex = Index2 then layerindex := Index1;
      surfaces := sl.SaveToString;
    end;
  finally
    sl.Free;
  end;
end;

procedure TLevelBankItem.DecreaseLayerIndexGreaterOrEqualThan(Index: integer);
var i: integer;
  sl: TSurfaceList;
begin
  Index := Index + APP_LAYER_COUNT;
  sl := TSurfaceList.Create;
  sl.SetModeForLevelEditor;
  try
    sl.LoadFromString(surfaces, False);
    if sl.Size > 0 then begin
      for i:=0 to sl.Size-1 do
        with sl.Mutable[i]^ do
          if layerindex >= Index then layerindex := layerindex - 1;
      surfaces := sl.SaveToString;
    end;
  finally
    sl.Free;
  end;
end;

procedure TLevelBankItem.ExportToPascalConst(t: TStringList; const aGroupName: string;
  aTextures: TTexturelist);
var wi: TWorldInfo;
  s: string;
  sl: TSurfaceList;
  j: SizeUInt;
begin
  wi := Default(TWorldInfo);
  wi.LoadFromString(worldinfo);
  wi.skylayer := wi.skylayer - APP_LAYER_COUNT; // adjust the layer index
  s := 'Data_'+aGroupName+'_'+name+'='''+ wi.SaveToString(False)+'|''+'#10+
       '  ''LayerLoopMode|'+layerloopinfo+'|''+'#10;
  sl := TSurfaceList.Create;
  sl.SetModeForLevelEditor;
  try
    sl.LoadFromString(surfaces, False);
    s := s + '  ''SurfaceCount|'+sl.Size.ToString+'|''+';
    t.Add(s);
    if sl.Size > 0 then begin
      s := '';
      for j:=0 to sl.Size-1 do begin
        s := s+'  ''S'+j.ToString+'|'+sl.Mutable[j]^.ExportToPascalString(aTextures);
        if j < sl.Size-1 then s := s + '|''+'#10
          else s := s + ''';';
      end;
    end;
    t.AddText(s);
    t.Add('');
  finally
    sl.Free;
  end;
end;

constructor TLevelGroup.Create;
begin
  inherited Create;
  Textures := TTextureList.Create;
end;

destructor TLevelGroup.Destroy;
begin
  Clear;
  Textures.Free;
  Textures := NIL;
  inherited Destroy;
end;

function TLevelGroup.AddEmpty: PLevelBankItem;
var o: TLevelBankItem;
begin
  o.InitDefault;
  PushBack(o);
  Result := Mutable[Size-1];
end;

function TLevelGroup.NameExists(const aName: string): boolean;
begin
  Result := GetItemByName(aName) <> NIL;
end;

function TLevelGroup.GetItemByName(const aName: string): PLevelBankItem; // PLevelBankItem;
var i: SizeUInt;
begin
  Result := NIL;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then begin
      Result := Mutable[i];
      exit;
    end;
end;

procedure TLevelGroup.DeleteByIndex(aIndex: integer);
begin
  Erase(aIndex);
end;

procedure TLevelGroup.DeleteByName(const aName: string);
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then begin
      DeleteByIndex(i);
      exit;
    end;
end;

function TLevelGroup.UseThisLayer(Index: integer): boolean;
var i: SizeUInt;
  layersUsed: TArrayOfInteger;
begin
  Result := False;
  if Size = 0 then exit;

  for i:=0 to Size-1 do begin
    layersUsed := Mutable[i]^.GetUserLayerIndexesUsed;
    if layersUsed.Have(Index) then exit(True);
  end;
end;

procedure TLevelGroup.Clear;
begin
  Textures.Clear;
  inherited Clear;
end;

function TLevelGroup.SaveToString: string;
var i: SizeUInt;
  prop: TProperties;
begin
  prop.Init('#');
  prop.Add('GroupName', FGroupName);
  prop.Add('GroupDescription', FGroupDescription);
  prop.Add('GroupTextures', Textures.SaveToString);
  prop.Add('LevelCount', integer(Size));
  if Size > 0 then
    for i:=0 to Size-1 do begin
      prop.Add('LevelName'+i.ToString, Mutable[i]^.name);
      if Mutable[i]^.description <> '' then
        prop.Add('LevelDescription'+i.ToString, Mutable[i]^.description);
      prop.Add('Surfaces'+i.ToString, Mutable[i]^.surfaces);
      prop.Add('WorldInfo'+i.ToString, Mutable[i]^.worldinfo);
      prop.Add('LayerLoopMode'+i.ToString, Mutable[i]^.layerloopinfo);
    end;

  Result := prop.PackedProperty;
end;

procedure TLevelGroup.LoadFromString(const data: string);
var c, i: integer;
  o: TLevelBankItem;
  prop: TProperties;
  s, level_Name, level_Desc, level_Surfaces, level_WorldInfo, level_LayerLoopInfo: string;
begin
  Clear;
  prop.Split(data, '#');

  s := '';
  level_Name := '';
  level_Desc := '';
  level_Surfaces := '';
  level_WorldInfo := '';
  level_LayerLoopInfo := '';
  prop.StringValueOf('GroupName', FGroupName, 'Level');
  prop.StringValueOf('GroupDescription', FGroupDescription, '');
  prop.StringValueOf('GroupTextures', s, '');
  Textures.LoadFromString(s);
  c := 0;

  prop.IntegerValueOf('LevelCount', c, 0);
  if c = 0 then exit;
  for i:=0 to c-1 do begin
    o.InitDefault;
    prop.StringValueOf('LevelDescription'+i.ToString, level_Desc, '');
    if prop.StringValueOf('LevelName'+i.ToString, level_name, '') and
       prop.StringValueOf('Surfaces'+i.ToString, level_Surfaces, '') and
       prop.StringValueOf('WorldInfo'+i.ToString, level_WorldInfo, '') and
       prop.StringValueOf('LayerLoopMode'+i.ToString, level_LayerLoopInfo, '') then begin
      o.name := level_name;
      o.description := level_Desc;
      o.surfaces := level_Surfaces;
      o.worldinfo := level_WorldInfo;
      o.layerloopinfo := level_LayerLoopInfo;
      PushBack(o);
    end else FScene.LogError('Error while loading Level index '+i.ToString+' : one or more properties are missing...');
  end;
end;

destructor TLevelBank.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TLevelBank.GroupNameExists(const aName: string): boolean;
begin
  Result := IndexOfGroup(aName) <> -1;
end;

function TLevelBank.ThereIsAtLeastOneLevelDefined: boolean;
var i: SizeUInt;
begin
  Result := False;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.Size > 0 then exit(True);
end;

function TLevelBank.AddGroup(const aGroupName: string): TLevelGroup;
begin
  Result := TLevelGroup.Create;
  Result.GroupName := aGroupName;
  PushBack(Result);
end;

function TLevelBank.IndexOfGroup(const aGroupName: string): integer;
var i: SizeUInt;
begin
  Result := -1;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.GroupName = aGroupName then exit(i);
end;

procedure TLevelBank.DeleteGroupByName(const aGroupName: string);
var i: integer;
begin
  i := IndexOfGroup(aGroupName);
  if i = -1 then exit;
  Mutable[i]^.Free;
  Self.Erase(i);
end;

procedure TLevelBank.RenameGroupByName(const aCurrentName, aNewName: string);
var i: Integer;
begin
  if IndexOfGroup(aNewName) <> -1 then exit;

  i := IndexOfGroup(aCurrentName);
  if i = -1 then exit;
  Mutable[i]^.GroupName := aNewName;
end;

procedure TLevelBank.Clear;
var i: SizeUInt;
begin
  if Size > 0 then
    for i:=0 to Size-1 do begin
      Mutable[i]^.Clear;
      Mutable[i]^.Free;
    end;
  inherited Clear;
end;

function TLevelBank.GetGroupByName(const aGroupName: string): TLevelGroup;
var i: Integer;
begin
  i := IndexOfGroup(aGroupName);
  if i = -1 then exit(NIL)
    else Result := Mutable[i]^;
end;

procedure TLevelBank.ExchangeLayerIndexInAllSurfaces(Index1, Index2: integer);
var i, j: SizeUInt;
  group: TLevelGroup;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do begin
    group := Mutable[i]^;
    if group.Size > 0 then
      for j:=0 to group.Size-1 do
        group.Mutable[j]^.ExchangeLayerIndexInAllSurfaces(Index1, Index2);
  end;
end;

function TLevelBank.UseThisLayer(Index: integer): boolean;
var i: SizeUInt;
begin
  Result := False;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.UseThisLayer(Index) then exit(True);
end;

procedure TLevelBank.DecreaseLayerIndexGreaterOrEqualThan(aIndex: integer);
var i, j: SizeUInt;
  group: TLevelGroup;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do begin
    group := Mutable[i]^;
    if group.Size > 0 then
      for j:=0 to group.Size-1 do
        group.Mutable[j]^.DecreaseLayerIndexGreaterOrEqualThan(aIndex);
  end;
end;

function TLevelBank.LevelCountInAllGroups: integer;
var i: SizeUInt;
begin
  if Size = 0 then exit(0);
  Result := 0;
  for i:=0 to Size-1 do
    Result := Result + Mutable[i]^.Size;
end;

procedure TLevelBank.SaveTo(t: TStringList);
var i: SizeUInt;
  prop: TProperties;
begin
  prop.Init('{');
  prop.Add('GroupCount', integer(Size));
  if Size > 0 then
    for i:=0 to Size-1 do
      prop.Add('Group'+i.ToString, Mutable[i]^.SaveToString);

  t.Add('[LEVEL BANK]');
  t.Add(prop.PackedProperty);
end;

procedure TLevelBank.LoadFrom(t: TStringList);
var i: SizeUInt;
  prop: TProperties;
  c: integer;
  o: TLevelGroup;
  s: string;
begin
  Clear;
  if not prop.SplitFrom(t, '[LEVEL BANK]', '{') then exit;

  s := '';
  c := 0;
  prop.IntegerValueOf('GroupCount', c, 0);
  if c = 0 then exit;

  for i:=0 to c do begin
    if prop.StringValueOf('Group'+i.ToString, s, '') then begin
      o := TLevelGroup.Create;
      o.LoadFromString(s);
      PushBack(o);
    end;
  end;
end;

procedure TLevelBank.SaveToPath(const aPath: string);
var t: TStringList;
  var filename: string;
begin
  filename := aPath + 'LevelBank.oglc';
  FScene.LogInfo('Saving Level Bank to '+filename, 1);
  t := TStringList.Create;
  try
    try
      SaveTo(t);
      t.SaveToFile(filename);
      FScene.LogInfo('done', 2);
    except
      On E :Exception do begin
        FScene.logError(E.Message, 2);
     end;
    end;
  finally
    t.Free;
  end;
end;

procedure TLevelBank.LoadFromPath(const aPath: string);
var t: TStringList;
  filename: string;
begin
  Clear;
  filename := aPath + 'LevelBank.oglc';
  if not FileExistsUTF8(filename) then begin
    FScene.LogInfo('No Level Bank found', 1);
    exit;
  end;

  FScene.LogInfo('Found Level Bank, loading...', 1);
  t := TStringList.Create;
  try
    try
      t.LoadFromFile(filename);
      LoadFrom(t);
      FScene.LogInfo('success, '+Size.ToString+' group(s), '+LevelCountInAllGroups.ToString+' level(s)', 2);
    except
      On E :Exception do begin
        FScene.logError(E.Message, 2);
     end;
    end;
  finally
    t.Free;
  end;
end;

procedure TLevelBank.Save;
begin
  SaveToPath(Project.Config.TargetLazarusProject.GetFolderGameHelperFiles);
end;

procedure TLevelBank.ExportToFileGameLevel;
var t: TStringlist;
  nameUnit, nameClass, unitFileName: string;
  i, j: integer;
  prefixSpace: string;
  haveSingleGroup: Boolean;
  group: TLevelGroup;
begin
  {
     on exporte tous les groupes de level dans un même fichier u_game_levels.pas
     gérés par une seule classe:

     TGamelevels = class(TOGLCDecorManager)
     public
       class procedure LoadTexture(aAtlas: TAtlas; aGroupIndex: integer); override;
       // return the number of group of level as defined in Game Helper
       function GetGroupCount: integer;
       // return the name of a group as defined in Game Helper
       function GetGroupName(aGroupIndex: integer): string;
       // return the description of the group of level as defined in Game Helper
       function GetGroupDescription(aGroupIndex: integer): string;
       // return the number of level in the specified level group as defined in Game Helper
       function GetLevelCount(aGroupIndex: integer): integer;
       // return the name of the level as defined in Game Helper
       function GetLevelName(aGroupIndex, aLevelIndex: integer): string;
       // return the level description as defined in Game Helper
       function GetLevelDescription(aGroupIndex, aLevelIndex: integer): string;

       // Load a level
       procedure LoadLevel(aGroupIndex, aLevelIndex: integer);
     end;

  IMPLEMENTATION

  const
   DATA_<name of group level 1>_<name of level 1> = '';

  }

  if Size = 0 then exit;

  haveSingleGroup := Size = 1;
  if haveSingleGroup then nameClass := 'T'+Mutable[0]^.GroupName
    else nameClass := 'TGameLevels';
  nameUnit := LEVEL_UNIT_NAME;
  unitFileName := Project.Config.TargetLazarusProject.GetFilenameGameLevels;

  t := TStringList.Create;

  //unit interface
  t.Add('{');
  CodeGen.AddFileGeneratedByGameHelper(t);
  t.AddText('  Usage:'#10+
            '    - call '+nameClass+'.LoadTexture() when you construct your texture atlas.'#10+
            '    - call LoadLevel() to load the needed game level.'#10+
            '    - retrieve the world area with property WordArea.'#10+
            '}');
  t.Add('');
  CodeGen.AddInterface(t, nameUnit);
  t.AddText('{ '+nameClass+' }'#10#10+
            nameClass+' = class(TOGLCDecorManager)');

  t.AddText('protected'#10+
            '  function ScaleWF(AValue: single): single; override;'#10+
            '  function ScaleHF(AValue: single): single; override;'#10+
            'public');

  if haveSingleGroup then
    t.AddText('  class procedure LoadTexture(aAtlas: TAtlas);'#10#10+
              '  // call this method to build the needed level'#10+
              '  procedure BuildLevel(aIndex: integer; aAtlas: TAtlas);')
  else
    t.AddText('  // Use this method to load the textures used by the specified group index in an atlas'#10+
              '  // If needed, you can call this method several time with a different group index'#10+
              '  class procedure LoadTexture(aAtlas: TAtlas; aGroupIndex: integer);'#10+
              '  // Use this method to load in a single call textures of all groups'#10+
              '  class procedure LoadTexturesForAllGroups(aAtlas: TAtlas);'#10#10+
              '  // return the number of level group as defined in Game Helper');

  t.AddText('  class function GetGroupCount: integer;'#10+
            '  // return the name of the group as defined in Game Helper'#10+
            '  class function GetGroupName(aGroupIndex: integer): string;'#10+
            '  // return the description of the group of level as defined in Game Helper'#10+
            '  class function GetGroupDescription(aGroupIndex: integer): string;'#10+
            '  // return the number of level in the specified level group as defined in Game Helper'#10+
            '  class function GetLevelCount(aGroupIndex: integer): integer;'#10+
            '  // return the name of the level as defined in Game Helper'#10+
            '  class function GetLevelName(aGroupIndex, aLevelIndex: integer): string;'#10+
            '  // return the level description as defined in Game Helper'#10+
            '  class function GetLevelDescription(aGroupIndex, aLevelIndex: integer): string;'#10#10+
            '  // call this method to build the needed level'#10+
            '  procedure BuildLevel(aGroupIndex, aLevelIndex: integer; aAtlas: TAtlas);');

  t.AddText('public // defined in ancestor'#10+
            '{'+#10+
            '  // return the indexes of the layers used by the decors'#10+
            '  function GetUsedLayerIndexes: TArrayOfInteger;'#10#10+
            '  // The world bounds as defined in the level'#10+
            '  property WorldArea: TRectF read FWorldArea;'#10#10+
            '  // The gradient instance to render the sky.'#10+
            '  // If the level don''t define a gradient for the sky, this property is NIL'#10+
            '  property SkyGradient: TGradientRectangle read FSkyGradient;'#10#10+
            '  property DecorCount: integer read GetDecorCount;'#10+
            '  // access to the decor instances'#10+
            '  property Decors[index:integer]: TOGLCDecorContainer read GetDecor;'#10+
            '}');
  t.Add('end;');
  t.Add('');
  // implementation
  CodeGen.AddImplementation(t);

  // CONST level data in string format
  if ThereIsAtLeastOneLevelDefined then begin
    t.Add('const');
    for i:=0 to Size-1 do begin
      group := Mutable[i]^;
      if group.Size > 0 then
        for j:=0 to group.Size-1 do begin
          group.Mutable[j]^.ExportToPascalConst(t, group.GroupName, group.Textures);
          t.Add('');
        end;
    end;
  end;

  t.AddText('{ '+nameClass+' }'+#10);

  // decor protected method
  CodeGen.AddImplementationOfDecorProtectedMethod(t, nameClass);
  t.Add('');

  // class method LoadTexture()
  if haveSingleGroup
    then t.Add('class procedure '+nameClass+'.LoadTexture(aAtlas: TAtlas);')
    else t.Add('class procedure '+nameClass+'.LoadTexture(aAtlas: TAtlas; aGroupIndex: integer);');
  t.AddText('var texFolder: string;'#10+
            'begin'#10+
            '  texFolder := u_common.TexturesFolder;');

  if not haveSingleGroup then
    t.Add('  case aGroupIndex of');

  for i:=0 to Size-1 do begin
    group := Mutable[i]^;
    if not haveSingleGroup then
      t.Add('    '+i.ToString+': begin  // '+group.GroupName);

    if haveSingleGroup then prefixSpace := '  '
      else prefixSpace := '      ';
    if group.Size > 0 then begin
      for j:=0 to group.Size-1 do
        t.Add(prefixSpace+group.Textures.Mutable[j]^.PascalCodeToAddTextureToAtlas(False))
    end else t.Add(prefixSpace+'// this group is empty');

    if not haveSingleGroup then
      t.Add('    end;');
  end;// for i
  if not haveSingleGroup then
    t.Add('  end;//case');
  t.Add('end;');
  t.Add('');

  // class procedure LoadTexturesForAllGroups(aAtlas: TAtlas);
  if not haveSingleGroup then begin
    t.AddText('class procedure '+nameClass+'.LoadTexturesForAllGroups(aAtlas: TAtlas);'#10+
              'var i: integer;'#10+
              'begin'#10+
              '  for i:=0 to GetGroupCount-1 do'#10+
              '    LoadTexture(aAtlas, i);'#10+
              'end;');
    t.Add('');
  end;

  // function GetGroupCount
  t.AddText('class function '+nameClass+'.GetGroupCount: integer;'#10+
            'begin'#10+
            '  Result := '+integer(Size).ToString+';'#10+
            'end;');
  // function GetGroupName
  t.AddText('class function '+nameClass+'.GetGroupName(aGroupIndex: integer): string;'#10+
            'begin'#10+
            '  case aGroupIndex of');
  for i:=0 to Size-1 do
      t.Add('    '+i.ToString+': Result := '''+Mutable[i]^.GroupName+''';');
  t.AddText('  end;//case'#10+
            'end;');
  // function GetGroupDescription
  t.AddText('class function '+nameClass+'.GetGroupDescription(aGroupIndex: integer): string;'#10+
            'begin'#10+
            '  case aGroupIndex of');
  for i:=0 to Size-1 do
      t.Add('    '+i.ToString+': Result := '''+Mutable[i]^.GroupDescription+''';');
  t.AddText('  end;//case'#10+
            'end;');
  // function GetLevelCount
  t.AddText('class function '+nameClass+'.GetLevelCount(aGroupIndex: integer): integer;'#10+
            'begin'#10+
            '  case aGroupIndex of');
  for i:=0 to Size-1 do
    t.Add('    '+i.ToString+': Result := '+integer(Mutable[i]^.Size).ToString+';');
  t.AddText('  end;//case'#10+
              'end;');
  // function GetLevelName
  t.AddText('class function '+nameClass+'.GetLevelName(aGroupIndex, aLevelIndex: integer): string;'#10+
            'begin'#10+
            '  case aGroupIndex of');
  for i:=0 to Size-1 do begin
    group := Mutable[i]^;
    t.Add('    '+i.ToString+': begin  // '+group.GroupName);
    if group.Size > 0 then begin
      t.Add('      case aLevelIndex of');
      for j:=0 to group.Size-1 do
        t.Add('        '+j.ToString+': Result := '''+group.Mutable[j]^.name+''';');
      t.Add('      end;');
    end else t.Add('      Result := ''''; // this group is empty');
    t.Add('    end;');
  end;
  t.AddText('  end;'#10+
            'end;');
    t.Add('');
  // function GetLevelDescription
  t.AddText('class function '+nameClass+'.GetLevelDescription(aGroupIndex, aLevelIndex: integer): string;'#10+
            'begin'#10+
            '  case aGroupIndex of');
  for i:=0 to Size-1 do begin
    group := Mutable[i]^;
    t.Add('    '+i.ToString+': begin  // '+group.GroupName);
    if group.Size > 0 then begin
      t.Add('      case aLevelIndex of');
      for j:=0 to group.Size-1 do
        t.Add('      '+j.ToString+': Result := '''+group.Mutable[j]^.description+''';');
      t.Add('      end;');
    end else t.Add('      Result := ''''; // this group is empty');
    t.Add('    end;');
  end;
  t.AddText('  end;'#10+
            'end;');
  t.Add('');

  // procedure to build a level
  if haveSingleGroup then begin
    group := Mutable[0]^;
    t.AddText('procedure '+nameClass+'.BuildLevel(aIndex: integer; aAtlas: TAtlas);'#10+
              'begin');
    if group.Size > 0 then begin
      t.Add('  case aIndex of');
      for i:=0 to group.Size-1 do
        t.Add('    '+i.ToString+': DoBuildLevel(DATA_'+group.GroupName+'_'+group.Mutable[i]^.name+', aAtlas);');
      t.Add('  end;');
    end;
  end else begin
    t.AddText('procedure '+nameClass+'.BuildLevel(aGroupIndex, aLevelIndex: integer; aAtlas: TAtlas);'#10+
              'begin'#10+
              '  case aGroupIndex of');
    for i:=0 to Size-1 do begin
      group := Mutable[i]^;
      if group.Size = 0 then t.Add('    '+i.ToString+': ; // this group is empty')
      else begin
        t.AddText('    '+i.ToString+':  // '+group.GroupName+#10+
                  '      case aLevelIndex of');
        for j:=0 to group.Size-1 do
          t.Add('        '+j.ToString+': DoBuildLevel(DATA_'+group.GroupName+'_'+group.Mutable[j]^.name+', aAtlas);');
        t.AddText('        else raise exception.create(''level index out of bounds'');'#10+
                  '      end;');
      end;
    end;// for i
    t.AddText('    else raise exception.create(''group index out of bounds'');'#10+
              '  end;//case');
  end;
  t.Add('end;');
  t.Add('');

  // end of file
  t.Add('end.');
  try
    t.SaveToFile(unitFileName);
  finally
    t.Free;
  end;
end;

{ TLevelBankUndoRedoManager }

procedure TLevelBankUndoRedoManager.ProcessUndo(var aItem: TLevelBankUndoRedoItem);
//var o: PLevelBankItem;
//  i: integer;
begin
{  case aItem.action of
    uratLevelBankDeleteItem: begin
      o := LevelBank.AddEmpty;
      o^.name := aItem.data.name;
      o^.surfaces := aItem.data.surfaces;
      FrameToolLevelBank.LB.ItemIndex := FrameToolLevelBank.LB.Items.Add(o^.name);
    end;

    uratLevelBankRenameItem: begin
      o := LevelBank.GetItemByName(aItem.data.name);
      if o = NIL then begin raise exception.Create('bug: name not found in LevelBank'); exit; end;
      o^.name := aItem.oldName;
      i := FrameToolLevelBank.LB.ItemIndex;
      if i <> -1 then FrameToolLevelBank.LB.Items.Strings[i] := aItem.oldName;
    end;

    uratLevelBankAddItem: begin
      LevelBank.DeleteByName(aItem.data.name);
      i := FrameToolLevelBank.LB.Items.IndexOf(aItem.data.name);
      if i <> -1 then FrameToolLevelBank.LB.Items.Delete(i);
    end;
  end;
  }
end;

procedure TLevelBankUndoRedoManager.ProcessRedo(var aItem: TLevelBankUndoRedoItem);
//var i: Integer;
//  o: PLevelBankItem;
begin
{  case aItem.action of
    uratLevelBankAddItem: begin
      o := LevelBank.AddEmpty;
      o^.name := aItem.data.name;
      o^.surfaces := aItem.data.surfaces;
      FrameToolLevelBank.LB.ItemIndex := FrameToolLevelBank.LB.Items.Add(o^.name);
    end;

    uratLevelBankDeleteItem: begin
      LevelBank.DeleteByName(aItem.data.name);
      i := FrameToolLevelBank.LB.Items.IndexOf(aItem.data.name);
      if i <> -1 then FrameToolLevelBank.LB.Items.Delete(i);
    end;

    uratLevelBankRenameItem: begin
      o := LevelBank.GetItemByName(aItem.oldName);
      if o = NIL then begin raise exception.Create('bug: oldName not found in LevelBank'); exit; end;
      o^.name := aItem.data.name;
      i := FrameToolLevelBank.LB.ItemIndex;
      if i <> -1 then FrameToolLevelBank.LB.Items.Strings[i] := aItem.data.name;
    end;
  end;
  }
end;

procedure TLevelBankUndoRedoManager.AddActionDeleteLevel(aIndex: integer);
//var o: TLevelBankUndoRedoItem;
begin
{  o := Default(TLevelBankUndoRedoItem);
  o.action := uratLevelBankDeleteItem;
  o.data.name := LevelBank.Mutable[aIndex]^.name;
  o.data.surfaces := LevelBank.Mutable[aIndex]^.surfaces;
  AddItem(o);  }
end;

procedure TLevelBankUndoRedoManager.AddActionRenameLevel(aIndex: integer; const aOldName: string);
//var o: TLevelBankUndoRedoItem;
begin
 { o := Default(TLevelBankUndoRedoItem);
  o.action := uratLevelBankRenameItem;
  o.data.name := LevelBank.Mutable[aIndex]^.name;
  o.data.surfaces := LevelBank.Mutable[aIndex]^.surfaces;
  o.oldName := aOldName;
  AddItem(o); }
end;

procedure TLevelBankUndoRedoManager.AddActionDuplicateLevel(aIndexNewItem: integer);
//var o: TLevelBankUndoRedoItem;
begin
{  o := Default(TLevelBankUndoRedoItem);
  o.action := uratLevelBankAddItem;
  o.data.name := LevelBank.Mutable[aIndexNewItem]^.name;
  o.data.surfaces := LevelBank.Mutable[aIndexNewItem]^.surfaces;
  AddItem(o);  }
end;

end.

