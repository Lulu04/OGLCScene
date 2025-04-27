unit u_texture_list;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, StdCtrls,
  OGLCScene, gvector, BGRABitmap, BGRABitmapTypes,
  u_undo_redo;

type

{ TTextureItem }

TTextureItem = record
  texture: PTexture;
  filename, name: string;
  width, height: integer;

  isMultiFrame: boolean;
  frameWidth, frameHeight: integer;
  procedure InitDefault;
  procedure CreateTexture;
  procedure FreeTexture;
  function SaveToString: string;
  procedure LoadFromString(const s: string);
end;
PTextureItem = ^TTextureItem;

{ TTextureList }

TTextureList = class(specialize TVector<TTextureItem>)
private
  procedure DoUpdate(aItem: PTextureItem; const aFilename, aName: string; aWidth, aHeight: integer; aIsFramed: boolean;
                    aFrameWidth, aFrameHeight: integer);
public
  destructor Destroy; override;
  procedure Clear; reintroduce;
  procedure Add(const aFilename, aName: string; aWidth, aHeight: integer; aIsFramed: boolean;
                    aFrameWidth, aFrameHeight: integer);
  procedure Update(aItem: PTextureItem; const aFilename, aName: string; aWidth,
    aHeight: integer; aIsFramed: boolean; aFrameWidth, aFrameHeight: integer);
  function GetItemByFilename(const aFilename: string): PTextureItem;
  function GetItemByName(const aName: string): PTextureItem;
  function GetItemIndexByName(const aName: string): integer;
  function GetItemByIndex(aIndex: SizeUInt): PTextureItem;

  procedure DeleteByName(const aName: string);
  function NameAlreadyExists(const aName: string): boolean;

  function SaveToString: string;
  procedure LoadFromString(const s: string);
  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);

  procedure FillComboBox(aCB: TComboBox);
  procedure FillListBox(aLB: TListBox);
end;


type
TTextureUndoRedoActionType = (
                           turatUndefined,
                           turatAddTexture,
                           turatDeleteTexture,
                           turatModifyTexture
                          );
TTextureUndoRedoItem = record
  action: TTextureUndoRedoActionType;
  data: TTextureItem;
  newData: TTextureItem;
end;

{ TTextureUndoRedoManager }

TTextureUndoRedoManager = class(specialize TGenericUndoRedoManager<TTextureUndoRedoItem>)
  ParentTexturelist: TTextureList;
  procedure ProcessUndo(var aItem: TTextureUndoRedoItem); override;
  procedure ProcessRedo(var aItem: TTextureUndoRedoItem); override;

  procedure AddActionAddTexture(const aName: string);
  procedure AddActionDeleteTexture(const aName: string);
  procedure AddActionModifyTexture(aItem: PTextureItem; const aFilename, aName: string;
                  aWidth, aHeight: integer; aIsFramed: boolean; aFrameWidth, aFrameHeight: integer);
end;

implementation

uses u_common, form_main, u_utils;

{ TTextureItem }

procedure TTextureItem.InitDefault;
begin
  Self := Default(TTextureItem);
end;

procedure TTextureItem.CreateTexture;
begin
  case ExtractFileExt(filename) of
    '.svg': if not isMultiFrame then texture := FScene.TexMan.AddFromSVG(filename, width, height)
             else raise exception.create('multi frame svg not yet implemented');
    else if not isMultiFrame then texture := FScene.TexMan.Add(filename)
             else texture := FScene.TexMan.Add(filename, frameWidth, frameHeight);
  end;
end;

procedure TTextureItem.FreeTexture;
begin
  if texture <> NIL then
    FScene.TexMan.Delete(texture);
end;

function TTextureItem.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('~');
  prop.Add('Filename', filename);
  prop.Add('Name', name);
  prop.Add('Width', width);
  prop.Add('Height', height);
  prop.Add('IsMultiFrame', isMultiFrame);
  prop.Add('FrameWidth', frameWidth);
  prop.Add('FrameHeight', frameHeight);
  Result := prop.PackedProperty;
end;

procedure TTextureItem.LoadFromString(const s: string);
var prop: TProperties;
begin
  InitDefault;
  prop.Split(s, '~');
  prop.StringValueOf('Filename', filename, filename);
  prop.StringValueOf('Name', name, ChangeFileExt(ExtractFilename(filename), ''));
  prop.IntegerValueOf('Width', width, width);
  prop.IntegerValueOf('Height', height, height);
  prop.BooleanValueOf('IsMultiFrame', isMultiFrame, isMultiFrame);
  prop.IntegerValueOf('FrameWidth', frameWidth, frameWidth);
  prop.IntegerValueOf('FrameHeight', frameHeight, frameHeight);
end;

{ TTextureList }

procedure TTextureList.DoUpdate(aItem: PTextureItem; const aFilename,
  aName: string; aWidth, aHeight: integer; aIsFramed: boolean; aFrameWidth,
  aFrameHeight: integer);
begin
  aItem^.filename := aFilename;
  aItem^.name := aName;
  aItem^.width := aWidth;
  aItem^.height := aHeight;
  aItem^.isMultiFrame := aIsFramed;
  aItem^.frameWidth := aFrameWidth;
  aItem^.frameHeight := aFrameHeight;
  aItem^.CreateTexture;
end;

destructor TTextureList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTextureList.Clear;
var i: SizeUInt;
begin
  if Size > 0 then
    for i:=0 to Size-1 do
      FScene.TexMan.Delete(Mutable[i]^.texture);
  inherited Clear;
end;

procedure TTextureList.Add(const aFilename, aName: string; aWidth, aHeight: integer;
  aIsFramed: boolean; aFrameWidth, aFrameHeight: integer);
var o: TTextureItem;
begin
  o.InitDefault;
  DoUpdate(@o, aFilename, aName, aWidth, aHeight, aIsFramed, aFrameWidth, aFrameHeight);
  PushBack(o);
end;

procedure TTextureList.Update(aItem: PTextureItem; const aFilename, aName: string; aWidth,
  aHeight: integer; aIsFramed: boolean; aFrameWidth, aFrameHeight: integer);
begin
  if aItem^.texture <> NIL then
    FScene.TexMan.Delete(aItem^.texture);
  DoUpdate(aItem, aFilename, aName, aWidth, aHeight, aIsFramed, aFrameWidth, aFrameHeight);
end;

function TTextureList.GetItemByFilename(const aFilename: string): PTextureItem;
var i: integer;
begin
  Result := NIL;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.filename = aFilename then begin
      Result := Mutable[i];
      exit;
    end;
end;

function TTextureList.GetItemByName(const aName: string): PTextureItem;
var i: integer;
begin
  Result := NIL;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then begin
      Result := Mutable[i];
      exit;
    end;
end;

function TTextureList.GetItemIndexByName(const aName: string): integer;
var i: integer;
begin
  Result := -1;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then begin
      Result := i;
      exit;
    end;
end;

function TTextureList.GetItemByIndex(aIndex: SizeUInt): PTextureItem;
begin
  Result := NIL;
  if aIndex >= Size then exit;
  Result := Mutable[aIndex];
end;

procedure TTextureList.DeleteByName(const aName: string);
var i: integer;
begin
  i := GetItemIndexByName(aName);
  Mutable[i]^.FreeTexture;
  Erase(i);
end;

function TTextureList.NameAlreadyExists(const aName: string): boolean;
begin
  Result := GetItemIndexByName(aName) <> -1;
end;

function TTextureList.SaveToString: string;
var prop: TProperties;
  i: SizeUInt;
begin
  Result := '';
  prop.Init('|');
  prop.Add('Count', Integer(Size));
  if Size > 0 then
    for i:=0 to Size-1 do
      prop.Add('Item'+i.ToString, Mutable[i]^.SaveToString);
  Result := prop.PackedProperty;
end;

procedure TTextureList.LoadFromString(const s: string);
var o: TTextureItem;
  prop: TProperties;
  i: SizeUInt;
  c: integer;
  s1: string;
begin
  Clear;
  prop.Split(s, '|');
  c := 0;
  s1 := '';
  prop.IntegerValueOf('Count', c, 0);
  if c = 0 then exit;

  for i:=0 to c-1 do
    if prop.StringValueOf('Item'+i.ToString, s1, '') then begin
      o.LoadFromString(s1);
      o.CreateTexture;
      PushBack(o);
    end;
end;

const SPRITE_BUILDER_TEXTURES_SECTION = '[SPRITE_BUILDER_TEXTURES]';

procedure TTextureList.SaveTo(t: TStringList);
begin
  t.Add(SPRITE_BUILDER_TEXTURES_SECTION);
  t.Add(SaveToString);
end;

procedure TTextureList.LoadFrom(t: TStringList);
var k: integer;
begin
  Clear;
  k := t.IndexOf(SPRITE_BUILDER_TEXTURES_SECTION);
  if (k = -1) or (k = t.Count-1) then exit;
  LoadFromString(t.Strings[k+1]);
end;

procedure TTextureList.FillComboBox(aCB: TComboBox);
var i: SizeUInt;
begin
  aCB.Clear;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    aCB.Items.Add(Mutable[i]^.name);
end;

procedure TTextureList.FillListBox(aLB: TListBox);
var i: SizeUInt;
begin
  aLB.Clear;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    aLB.Items.Add(Mutable[i]^.name);
end;

{ TTextureUndoRedoManager }

procedure TTextureUndoRedoManager.ProcessUndo(var aItem: TTextureUndoRedoItem);
var i: integer;
  s: string;
  item: PTextureItem;
begin
  case aItem.Action of
    turatAddTexture: begin
      ParentTexturelist.DeleteByName(aItem.data.name);
      i := FrameToolsSpriteBuilder.LBTextureNames.Items.IndexOf(aItem.data.name);
      if i <> -1 then FrameToolsSpriteBuilder.LBTextureNames.Items.Delete(i);
    end;

    turatDeleteTexture: begin
      ParentTexturelist.Add(aItem.data.filename, aItem.data.name, aItem.data.width, aItem.data.height,
                            aItem.data.isMultiFrame, aItem.data.frameWidth, aItem.data.frameHeight);
      FrameToolsSpriteBuilder.LBTextureNames.Items.Add(aItem.data.name);
    end;

    turatModifyTexture: begin
      item := ParentTexturelist.GetItemByName(aItem.newData.name);
      ParentTexturelist.Update(item, aItem.data.filename, aItem.data.name, aItem.data.width,
                 aItem.data.height, aItem.data.isMultiFrame, aItem.data.frameWidth, aItem.data.frameHeight);
   {   ExchangeString(aItem.data.filename, aItem.newData.filename);
      ExchangeString(aItem.data.name, aItem.newData.name);
      ExchangeInteger(aItem.data.width, aItem.newData.width);
      ExchangeInteger(aItem.data.height, aItem.newData.height);
      ExchangeBoolean(aItem.data.isMultiFrame, aItem.newData.isMultiFrame);
      ExchangeInteger(aItem.data.frameWidth, aItem.newData.frameWidth);
      ExchangeInteger(aItem.data.frameHeight, aItem.newData.frameHeight); }
    end;
  end;
end;

procedure TTextureUndoRedoManager.ProcessRedo(var aItem: TTextureUndoRedoItem);
var i: Integer;
  item: PTextureItem;
begin
  case aItem.Action of
    turatAddTexture: begin
      ParentTexturelist.Add(aItem.data.filename, aItem.data.name, aItem.data.width, aItem.data.height,
                            aItem.data.isMultiFrame, aItem.data.frameWidth, aItem.data.frameHeight);
      FrameToolsSpriteBuilder.LBTextureNames.Items.Add(aItem.data.name);
    end;

    turatDeleteTexture: begin
      ParentTexturelist.DeleteByName(aItem.data.name);
      i := FrameToolsSpriteBuilder.LBTextureNames.Items.IndexOf(aItem.data.name);
      if i <> -1 then FrameToolsSpriteBuilder.LBTextureNames.Items.Delete(i);
    end;

    turatModifyTexture: begin
      item := ParentTexturelist.GetItemByName(aItem.data.name);
      ParentTexturelist.Update(item, aItem.newData.filename, aItem.newData.name, aItem.newData.width,
                               aItem.newData.height, aItem.newData.isMultiFrame,
                               aItem.newData.frameWidth, aItem.newData.frameHeight);
    end;
  end;
end;

procedure TTextureUndoRedoManager.AddActionAddTexture(const aName: string);
var o: TTextureUndoRedoItem;
begin
  o.action := turatAddTexture;
  o.data.InitDefault;
  o.newData.InitDefault;
  o.data := ParentTexturelist.GetItemByName(aName)^;
  AddItem(o);
end;

procedure TTextureUndoRedoManager.AddActionDeleteTexture(const aName: string);
var o: TTextureUndoRedoItem;
begin
  o.action := turatDeleteTexture;
  o.data.InitDefault;
  o.newData.InitDefault;
  o.data := ParentTexturelist.GetItemByName(aName)^;
  AddItem(o);
end;

procedure TTextureUndoRedoManager.AddActionModifyTexture(aItem: PTextureItem;
  const aFilename, aName: string; aWidth, aHeight: integer; aIsFramed: boolean;
  aFrameWidth, aFrameHeight: integer);
var o: TTextureUndoRedoItem;
begin
  o.action := turatModifyTexture;
  o.data.InitDefault;
  o.newData.InitDefault;
  o.data := aItem^;
  o.newData.filename := aFilename;
  o.newData.name := aName;
  o.newData.width := aWidth;
  o.newData.height := aHeight;
  o.newData.isMultiFrame := aIsFramed;
  o.newData.frameWidth := aFrameWidth;
  o.newData.frameHeight := aFrameHeight;
  AddItem(o);
end;

end.

