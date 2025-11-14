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

TLevelBankItem = record
  name,
  worldinfo,
  layerloopinfo,
  surfaces : string;
  procedure InitDefault;
  function SaveToString: string;
  procedure LoadFromString(const s: string);

  // return an array with the index of user layer used in this level
  function GetUserLayerIndexesUsed: TArrayOfInteger;
  // export pascal code to declare the level as constant
  procedure ExportToPascalConst(t: TStringList);
end;
PLevelBankItem = ^TLevelBankItem;

{ TLevelBank }

TLevelBank = class(specialize TVector<TLevelBankItem>)
  Textures: TTextureList;
  constructor Create;
  destructor Destroy; override;

  function AddEmpty: PLevelBankItem;
  function NameExists(const aName: string): boolean;
  function GetItemByName(const aName: string): PLevelBankItem;
  procedure DeleteByIndex(aIndex: integer);
  procedure DeleteByName(const aName: string);

  procedure Clear; reintroduce;

  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);
end;

var LevelBank: TLevelBank;


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

uses frame_tool_levelbank, form_main, u_common, u_surface_list, u_utils;

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

procedure TLevelBankItem.ExportToPascalConst(t: TStringList);
var wi: TWorldInfo;
  s: string;
  sl: TSurfaceList;
  j: SizeUInt;
begin
  wi := Default(TWorldInfo);
  wi.LoadFromString(worldinfo);
  wi.skylayer := wi.skylayer - APP_LAYER_COUNT; // adjust the layer index
  s := 'Data_'+name+'='''+ wi.SaveToString(False)+'|''+'#10+
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
        s := s+'  ''S'+j.ToString+'|'+sl.Mutable[j]^.ExportToPascalString(LevelBank.Textures);
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

constructor TLevelBank.Create;
begin
  inherited Create;
  Textures := TTextureList.Create;
end;

destructor TLevelBank.Destroy;
begin
  Clear;
  Textures.Free;
  Textures := NIL;
  inherited Destroy;
end;

function TLevelBank.AddEmpty: PLevelBankItem;
var o: TLevelBankItem;
begin
  o.InitDefault;
  PushBack(o);
  Result := Mutable[Size-1];
end;

function TLevelBank.NameExists(const aName: string): boolean;
begin
  Result := GetItemByName(aName) <> NIL;
end;

function TLevelBank.GetItemByName(const aName: string): PLevelBankItem; // PLevelBankItem;
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

procedure TLevelBank.DeleteByIndex(aIndex: integer);
begin
  Erase(aIndex);
end;

procedure TLevelBank.DeleteByName(const aName: string);
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then begin
      DeleteByIndex(i);
      exit;
    end;
end;

procedure TLevelBank.Clear;
begin
  Textures.Clear;
  inherited Clear;
end;

procedure TLevelBank.SaveTo(t: TStringList);
var i: SizeUInt;
  prop: TProperties;
begin
  prop.Init('#');
  prop.Add('LevelTextures', Textures.SaveToString);
  prop.Add('LevelCount', integer(Size));
  if Size > 0 then
    for i:=0 to Size-1 do begin
      prop.Add('LevelName'+i.ToString, Mutable[i]^.name);
      prop.Add('Surfaces'+i.ToString, Mutable[i]^.surfaces);
      prop.Add('WorldInfo'+i.ToString, Mutable[i]^.worldinfo);
      prop.Add('LayerLoopMode'+i.ToString, Mutable[i]^.layerloopinfo);
    end;

  t.Add('[LEVEL BANK]');
  t.Add(prop.PackedProperty);
end;

procedure TLevelBank.LoadFrom(t: TStringList);
var c, i: integer;
  o: TLevelBankItem;
  prop: TProperties;
  s, level_Name, level_Surfaces, level_WorldInfo, level_LayerLoopInfo: string;
begin
  Clear;
  if not prop.SplitFrom(t, '[LEVEL BANK]', '#') then exit;

  s := '';
  level_Name := '';
  level_Surfaces := '';
  level_WorldInfo := '';
  level_LayerLoopInfo := '';
  prop.StringValueOf('LevelTextures', s, '');
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

{ TLevelBankUndoRedoManager }

procedure TLevelBankUndoRedoManager.ProcessUndo(var aItem: TLevelBankUndoRedoItem);
var o: PLevelBankItem;
  i: integer;
begin
  case aItem.action of
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
end;

procedure TLevelBankUndoRedoManager.ProcessRedo(var aItem: TLevelBankUndoRedoItem);
var i: Integer;
  o: PLevelBankItem;
begin
  case aItem.action of
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
end;

procedure TLevelBankUndoRedoManager.AddActionDeleteLevel(aIndex: integer);
var o: TLevelBankUndoRedoItem;
begin
  o := Default(TLevelBankUndoRedoItem);
  o.action := uratLevelBankDeleteItem;
  o.data.name := LevelBank.Mutable[aIndex]^.name;
  o.data.surfaces := LevelBank.Mutable[aIndex]^.surfaces;
  AddItem(o);
end;

procedure TLevelBankUndoRedoManager.AddActionRenameLevel(aIndex: integer; const aOldName: string);
var o: TLevelBankUndoRedoItem;
begin
  o := Default(TLevelBankUndoRedoItem);
  o.action := uratLevelBankRenameItem;
  o.data.name := LevelBank.Mutable[aIndex]^.name;
  o.data.surfaces := LevelBank.Mutable[aIndex]^.surfaces;
  o.oldName := aOldName;
  AddItem(o);
end;

procedure TLevelBankUndoRedoManager.AddActionDuplicateLevel(aIndexNewItem: integer);
var o: TLevelBankUndoRedoItem;
begin
  o := Default(TLevelBankUndoRedoItem);
  o.action := uratLevelBankAddItem;
  o.data.name := LevelBank.Mutable[aIndexNewItem]^.name;
  o.data.surfaces := LevelBank.Mutable[aIndexNewItem]^.surfaces;
  AddItem(o);
end;

end.

