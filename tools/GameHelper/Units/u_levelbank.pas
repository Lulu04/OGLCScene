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
  name,
  worldinfo,
  layerloopinfo,
  surfaces : string;
  procedure InitDefault;
  procedure DuplicateTo(aItem: PLevelBankItem);
  function SaveToString: string;
  procedure LoadFromString(const s: string);

  // return an array with the index of user layer used in this level
  function GetUserLayerIndexesUsed: TArrayOfInteger;
  // export pascal code to declare the level as constant
  procedure ExportToPascalConst(t: TStringList; const aGroupName: string; aTextures: TTexturelist);
end;

{ TLevelGroup }

TLevelGroup = class(specialize TVector<TLevelBankItem>)
private
  FGroupName: string;
public
  Textures: TTextureList;
  constructor Create;
  destructor Destroy; override;

  function AddEmpty: PLevelBankItem;
  function NameExists(const aName: string): boolean;
  function GetItemByName(const aName: string): PLevelBankItem;
  procedure DeleteByIndex(aIndex: integer);
  procedure DeleteByName(const aName: string);

  procedure Clear; reintroduce;

  function SaveToString: string;
  procedure LoadFromString(const data: string);
  property GroupName: string read FGroupName write FGroupName;
end;

TLevelBank = class(specialize TVector<TLevelGroup>)
  destructor Destroy; override;

  function GroupNameExists(const aName: string): boolean;

  function AddGroup(const aGroupName: string): TLevelGroup;
  function IndexOfGroup(const aGroupName: string): integer;
  procedure DeleteGroupByName(const aGroupName: string);
  procedure RenameGroupByName(const aCurrentName, aNewName: string);
  procedure Clear; reintroduce;

  function GetGroupByName(const aGroupName: string): TLevelGroup;

  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);

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

uses u_common, u_surface_list, u_utils,
  u_project;

{ TWorldInfo }

procedure TWorldInfo.InitDefault;
begin
  Self := Default(TWorldInfo);
  useskygradient := True;
  skygradientdata := DEFAULT_SKY_GRADIENT;
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
  prop.StringValueOf('SkyGradientData', skygradientdata, DEFAULT_SKY_GRADIENT);
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

function TLevelBankItem.SaveToString: string;
begin
  Result := name+'#'+surfaces+'#'+worldinfo+'#'+layerloopinfo;
end;

procedure TLevelBankItem.LoadFromString(const s: string);
var A: TStringArray;
begin
  A := s.Split(['#']);
  name := A[0];
  surfaces := A[1];
  if Length(A) > 2 then worldinfo := A[2] else worldinfo := '';
  if Length(A) > 3 then layerloopinfo := A[3] else layerloopinfo := '';
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
  prop.Add('GroupTextures', Textures.SaveToString);
  prop.Add('LevelCount', integer(Size));
  if Size > 0 then
    for i:=0 to Size-1 do begin
      prop.Add('LevelName'+i.ToString, Mutable[i]^.name);
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
  s, level_Name, level_Surfaces, level_WorldInfo, level_LayerLoopInfo: string;
begin
  Clear;
  prop.Split(data, '#');

  s := '';
  level_Name := '';
  level_Surfaces := '';
  level_WorldInfo := '';
  level_LayerLoopInfo := '';
  prop.StringValueOf('GroupName', FGroupName, 'Level');
  prop.StringValueOf('GroupTextures', s, '');
  Textures.LoadFromString(s);
  c := 0;

  prop.IntegerValueOf('LevelCount', c, 0);
  if c = 0 then exit;
  for i:=0 to c-1 do begin
    o.InitDefault;
    if prop.StringValueOf('LevelName'+i.ToString, level_name, '') and
       prop.StringValueOf('Surfaces'+i.ToString, level_Surfaces, '') and
       prop.StringValueOf('WorldInfo'+i.ToString, level_WorldInfo, '') and
       prop.StringValueOf('LayerLoopMode'+i.ToString, level_LayerLoopInfo, '') then begin
      o.name := level_name;
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

procedure TLevelBank.SaveTo(t: TStringList);
var i: SizeUInt;
  prop: TProperties;
begin
  prop.Init('!');
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
  if not prop.SplitFrom(t, '[LEVEL BANK]', '!') then exit;

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

procedure TLevelBank.ExportToFileGameLevel;
var t: TStringlist;
  nameUnit, nameClass, unitFileName: string;
  i, j: integer;
  s, texFilename, sw, sh: string;
  p: SizeInt;
  haveSingleGroup: Boolean;
  group: TLevelGroup;
  item: PTextureItem;
begin
  {
     on exporte tous les groupes de level dans un même fichier u_game_levels.pas
     gérés par une seule classe:

     TGamelevels = class(TOGLCDecorManager)
     private
       function GetGroupCount: integer;
       function GetGroupName(groupindex: integer): string;
       function GetLevelCount(groupindex: integer): integer;
     public
       class procedure LoadTexture(aAtlas: TAtlas; aGroupIndex: integer); override;
       // return the number of level group as defined in Game Helper
       function GetGroupCount: integer;
       // return the name of the group as defined in Game Helper
       function GetGroupName(aGroupIndex: integer): string;
       // return the number of level in the specified level group as defined in Game Helper
       function GetLevelCount(aGroupIndex: integer): integer;
       // return the name of the level as defined in Game Helper
       function GetLevelName(aGroupIndex, aLevelIndex: integer): string;

       // Load a level
       procedure LoadLevel(aGroupIndex, aLevelIndex: integer);
       // After a call to LoadLevel, retrieve the world bounds with this property
       function GetWorldArea: TRectF;
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
  AddFileGeneratedByGameHelper(t);
  t.AddText('  Usage:'#10+
            '    - call '+nameClass+'.LoadTexture() when you construct your texture atlas.'#10+
            '    - call LoadLevel() to load the needed game level.'#10+
            '    - retrieve the world area with property WordArea.'#10+
            '}');
  t.Add('');
  AddInterface(t, nameUnit);
  t.AddText('{ '+nameClass+' }'#10#10+
            nameClass+' = class(TOGLCDecorManager)');

  if not haveSingleGroup then
    t.AddText('private'#10+
              '  function GetGroupCount: integer;'#10+
              '  function GetGroupName(groupindex: integer): string;'#10+
              '  function GetLevelCount(groupindex: integer): integer;');

  t.AddText('protected'#10+
            '  function ScaleWF(AValue: single): single; override;'#10+
            '  function ScaleHF(AValue: single): single; override;'#10+
            'public');

  if haveSingleGroup then
    t.AddText('  class procedure LoadTexture(aAtlas: TAtlas); override;'#10#10+
              '  // call this method to build the needed level'#10+
              '  procedure BuildLevel(aIndex: integer);')
  else
    t.AddText('  class procedure LoadTexture(aAtlas: TAtlas; aGroupIndex: integer);'#10+
              '  // return the number of level group as defined in Game Helper'#10+
              '  function GetGroupCount: integer;'#10+
              '  // return the name of the group as defined in Game Helper'#10+
              '  function GetGroupName(aGroupIndex: integer): string;'#10+
              '  // return the number of level in the specified level group as defined in Game Helper'#10+
              '  function GetLevelCount(aGroupIndex: integer): integer;'#10+
              '  // return the name of the level as defined in Game Helper'#10+
              '  function GetLevelName(aGroupIndex, aLevelIndex: integer): string;'#10#10+
              '  // call this method to build the needed level'#10+
              '  procedure BuildLevel(aGroupIndex, aLevelIndex: integer);');

  T.AddText('  // After a call to BuildLevel, retrieve the world bounds with this property'#10+
            '  function GetWorldArea: TRectF;'#10+
            'end;');
  t.Add('');
  // implementation
  AddImplementation(t);
  // CONST level data in string format
  t.Add('const');
  for i:=0 to Size-1 do begin
    group := Mutable[i]^;
    for j:=0 to group.Size-1 do begin
      group.Mutable[j]^.ExportToPascalConst(t, group.GroupName, group.Textures);
      t.Add('');
    end;
 end;

  t.AddText('{ '+nameClass+' }'+#10);

  if not haveSingleGroup then begin
    t.AddText('function '+nameClass+'.GetGroupCount: integer;'#10+
              'begin'#10+
              '  Result := '+integer(Size).ToString+';'#10+
              'end;'#10#10+
              'function '+nameClass+'.GetGroupName(groupindex: integer): string;'#10+
              'begin'#10+
              '  case groupindex of');
    for i:=0 to Size-1 do
        t.Add('    '+i.ToString+': Result := '''+Mutable[i]^.GroupName+''';');
    t.AddText('  end;//case'#10+
              'end;'#10#10+
              '  function '+nameClass+'.GetLevelCount(groupindex: integer): integer;'#10+
              'begin'#10+
              '  case groupindex of');
    for i:=0 to Size-1 do
        t.Add('    '+i.ToString+': Result := '+integer(Mutable[i]^.Size).ToString+';');
    t.AddText('  end;//case'#10+
              'end;');
    t.Add('');
  end;

  // decor protected method
  AddImplementationOfDecorProtectedMethod(t, nameClass);

  // method for class LoadTexture()
  if haveSingleGroup
    then t.Add('class procedure '+nameClass+'.LoadTexture(aAtlas: TAtlas);')
    else t.Add('class procedure '+nameClass+'.LoadTexture(aAtlas: TAtlas; aGroupIndex: integer);');
  t.AddText('var dataFolder: string;'#10+
            'begin'#10+
            '  dataFolder := u_common.DataFolder;');

  if not haveSingleGroup then
    t.Add('  case aGroupIndex of');

  for i:=0 to Size-1 do begin
    if not haveSingleGroup then
      t.Add('    '+i.ToString+': begin');
    group := Mutable[i]^;

    if group.Size > 0 then begin
      with group.Textures do
        for j:=0 to Size-1 do begin
          item := Mutable[j];
          // texture filename must be relative to application Data folder
          texFilename := item^.filename;
          p := texFilename.LastIndexOf(DirectorySeparator+'Data'+DirectorySeparator);
          texFilename := texFilename.Remove(0, p+6);
          texFilename := 'dataFolder+'''+texFilename+'''';

          s := '  ';
          if ExtractFileExt(item^.filename) = '.svg' then begin
            if item^.width = -1 then sw := '-1'
              else sw := 'u_common.ScaleW('+item^.width.ToString+')';
            if item^.height = -1 then sh := '-1'
              else sh := 'u_common.ScaleH('+item^.height.ToString+')';

            if item^.isMultiFrame then
              s := s + 'aAtlas.AddMultiFrameImageFromSVG('+texFilename+
                 ', '+sw+', '+sh+
                 ', '+(item^.width div item^.frameWidth).ToString+
                 ', '+(item^.height div item^.frameHeight).ToString+
                 ', 0);'
            else
              s := s + 'aAtlas.AddFromSVG('+texFilename+', '+sw+', '+sh+');';
          end else begin
            if item^.isMultiFrame then
              s := s + 'aAtlas.AddMultiFrameImage('+texFilename+
              ', '+(item^.width div item^.frameWidth).ToString+
              ', '+(item^.height div item^.frameHeight).ToString+');'
            else
              s := s + 'aAtlas.Add('+texFilename+');';
          end;
          t.Add(s);
        end;// for j
    end;
    if not haveSingleGroup then
      t.Add('    end;');
  end;// for i
  if not haveSingleGroup then
    t.Add('  end;//case');
  t.Add('end;');
  t.Add('');

  // procedure to build a level
  if haveSingleGroup then begin
    group := Mutable[0]^;
    t.AddText('procedure '+nameClass+'.BuildLevel(aIndex: integer);'#10+
              'begin');
    if group.Size > 0 then begin
      t.Add('  case aIndex of');
      for i:=0 to group.Size-1 do
        t.Add('    '+i.ToString+': DoBuildLevel(DATA_'+group.GroupName+'_'+group.Mutable[i]^.name+');');
      t.Add('  end;');
    end;
  end else begin
    t.AddText('procedure '+nameClass+'.BuildLevel(aGroupIndex, aLevelIndex: integer);'#10+
              'begin'#10+
              '  case aGroupIndex of');
    for i:=0 to Size-1 do begin
      group := Mutable[i]^;
      if group.Size = 0 then t.Add('    '+i.ToString+': ; // this group is empty')
      else begin
        t.AddText('    '+i.ToString+':'#10+
                  '      case aLevelIndex Of');
        for j:=0 to group.Size-1 do
          t.Add('        '+j.ToString+': DoBuildLevel(DATA_'+group.GroupName+'_'+group.Mutable[j]^.name+');');
        t.AddText('        else raise exception.create(''level index out of bounds'');'#10+
                  '      end;');
      end;
    end;// for i
    t.AddText('    else raise exception.create(''group index out of bounds'');'#10+
              '  end;//case');
  end;
  t.Add('end;');

  // GetWorldArea
  t.AddText('function '+nameClass+'.GetWorldArea: TRectF;'#10+
            'begin'#10+
            '  Result.Left := ScaleW(Round(FNonScaledWorldArea.Left));'#10+
            '  Result.Top := ScaleH(Round(FNonScaledWorldArea.Top));'#10+
            '  Result.Right := ScaleW(Round(FNonScaledWorldArea.Right));'#10+
            '  Result.Bottom := ScaleH(Round(FNonScaledWorldArea.Bottom));'#10+
            'end;');
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

