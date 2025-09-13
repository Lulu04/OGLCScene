unit u_levelbank;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  OGLCScene, gvector,
  BGRABitmap, BGRABitmapTypes,
  u_undo_redo, u_texture_list;

//  Bank level have a single texture list shared by all level items.

type

{ TWorldInfo }

TWorldInfo = record
  x, y, width, height: single;
  boundscolor: TBGRAPixel;
  showbounds: boolean;
  procedure InitDefault;
  function SaveToString(aSaveAllProperties: boolean=True): string;
  procedure LoadFromString(const s: string);
end;

TLevelBankItem = record
  name,
  worldinfo,
  surfaces : string;
  procedure InitDefault;
  function SaveToString: string;
  procedure LoadFromString(const s: string);
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

uses frame_tool_levelbank, form_main, u_common;

{ TWorldInfo }

procedure TWorldInfo.InitDefault;
begin
  Self := Default(TWorldInfo);
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
end;

{ TLevelBankItem }

procedure TLevelBankItem.InitDefault;
begin
  Self := Default(TLevelBankItem);
end;

function TLevelBankItem.SaveToString: string;
begin
  Result := name+'#'+surfaces+'#'+worldinfo;
end;

procedure TLevelBankItem.LoadFromString(const s: string);
var A: TStringArray;
begin
  A := s.Split(['#']);
  name := A[0];
  surfaces := A[1];
  if Length(A) > 2 then worldinfo := A[2] else worldinfo := '';
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
begin
  t.Add('[LEVEL BANK]');
  t.Add(Textures.SaveToString);
  t.Add(integer(Size).ToString);
  if Size > 0 then
    for i:=0 to Size-1 do begin
      t.Add(Mutable[i]^.name);
      t.Add(Mutable[i]^.surfaces);
      t.Add(Mutable[i]^.worldinfo);
    end;
end;

procedure TLevelBank.LoadFrom(t: TStringList);
var k, c, i: integer;
  o: TLevelBankItem;
begin
  Clear;
  k := t.IndexOf('[LEVEL BANK]');
  if (k = -1) or (k = t.Count-1) then exit;

  inc(k);
  Textures.LoadFromString(t.Strings[k]);

  inc(k);
  c := t.Strings[k].ToInteger;

  if c = 0 then exit;
  for i:=0 to c-1 do begin
    inc(k);
    o.InitDefault;
    o.name := t.Strings[k];
    inc(k);
    o.surfaces := t.Strings[k];
    inc(k);
    o.worldinfo := t.Strings[k];
    PushBack(o);
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

