unit u_texture_list;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, StdCtrls,
  OGLCScene, gvector, BGRABitmap, BGRABitmapTypes;

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
  procedure Update(const aFilename, aName: string; aWidth, aHeight: integer; aIsFramed: boolean;
                    aFrameWidth, aFrameHeight: integer);
  function GetItemByFilename(const aFilename: string): PTextureItem;
  function GetItemByName(const aName: string): PTextureItem;
  function GetItemIndexByName(const aName: string): integer;
  function GetItemByIndex(aIndex: SizeUInt): PTextureItem;

  procedure DeleteByName(const aName: string);

  function SaveToString: string;
  procedure LoadFromString(const s: string);
  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);

  procedure FillComboBox(aCB: TComboBox);
  procedure FillListBox(aLB: TListBox);
end;


implementation

uses u_common;

{ TTextureItem }

procedure TTextureItem.InitDefault;
begin
  FillChar(Self, SizeOf(TTextureItem), 0);
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
FScene.LogDebug('Texture '+filename+' width '+width.ToString+' height '+height.ToString);
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
var i: SizeUInt;
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

procedure TTextureList.Update(const aFilename, aName: string; aWidth,
  aHeight: integer; aIsFramed: boolean; aFrameWidth, aFrameHeight: integer);
var p: PTextureItem;
begin
  p := GetItemByFilename(aFilename);
  if p = NIL then exit;
  if p^.texture <> NIL then
    FScene.TexMan.Delete(p^.texture);
  DoUpdate(p, aFilename, aName, aWidth, aHeight, aIsFramed, aFrameWidth, aFrameHeight);
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
var prop: TProperties;
  i: SizeUInt;
  c: integer;
  s1: string;
  o: TTextureItem;
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

end.

