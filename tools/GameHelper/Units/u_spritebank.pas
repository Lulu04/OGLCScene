unit u_spritebank;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  OGLCScene, gvector,
  u_surface_list, u_undo_redo;

type

{ TSpriteBankItem }

TSpriteBankItem = record
  name,
  textures,
  surfaces,
  collisionbodies,
  postures: string;
  procedure InitDefault;
  function SaveToString: string;
  procedure LoadFromString(const s: string);
end;
PSpriteBankItem = ^TSpriteBankItem;

{ TSpriteBank }

TSpriteBank = class(specialize TVector<TSpriteBankItem>)
  function AddEmpty: PSpriteBankItem;
  function SpriteNameExists(const aName: string): boolean;
  function GetItemByName(const aName: string): PSpriteBankItem;

  procedure DeleteByIndex(aIndex: integer);
  procedure DeleteByName(const aName: string);

  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);
end;

var
  SpriteBank: TSpriteBank;

type
TBankUndoRedoActionType = (uratBankUndefined,
                           uratBankDeleteItem, // user delete a sprite
                           uratBankRenameItem, // user rename a sprite
                           uratBankAddItem     // user duplicate a sprite
                          );
TBankUndoRedoItem = record
  action: TBankUndoRedoActionType;
  data: TSpriteBankItem;
  newData: TSpriteBankItem;
  //name, textures, surfaces, collisionbodies, postures: string;
  oldName: string;
end;

{ TBankUndoRedoManager }

TBankUndoRedoManager = class(specialize TGenericUndoRedoManager<TBankUndoRedoItem>)
  procedure ProcessUndo(var aItem: TBankUndoRedoItem); override;
  procedure ProcessRedo(var aItem: TBankUndoRedoItem); override;

  procedure AddActionDeleteSprite(aIndex: integer);
  procedure AddActionRenameSprite(aIndex: integer; const aOldName: string);
  procedure AddActionDuplicateSprite(aIndexNewItem: integer);
end;

implementation

uses form_main;

{ TSpriteBankItem }

procedure TSpriteBankItem.InitDefault;
begin
  FillChar(Self, SizeOf(TSpriteBankItem), 0);
end;

function TSpriteBankItem.SaveToString: string;
begin
  Result := name+'#'+textures+'#'+surfaces+'#'+collisionbodies+'#'+postures;
end;

procedure TSpriteBankItem.LoadFromString(const s: string);
var A: TStringArray;
begin
  A := s.Split(['#']);
  name := A[0];
  textures := A[1];
  surfaces := A[2];
  if Length(A) = 4 then collisionbodies := A[3]
    else collisionbodies := '';
  if Length(A) = 5 then postures := A[4]
    else postures := '';
end;

{ TSpriteBank }

function TSpriteBank.AddEmpty: PSpriteBankItem;
var o: TSpriteBankItem;
begin
  o.InitDefault;
  PushBack(o);
  Result := Mutable[Size-1];
end;

function TSpriteBank.SpriteNameExists(const aName: string): boolean;
begin
  Result := GetItemByName(aName) <> NIL;
end;

function TSpriteBank.GetItemByName(const aName: string): PSpriteBankItem;
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

procedure TSpriteBank.DeleteByIndex(aIndex: integer);
begin
  Erase(aIndex);
end;

procedure TSpriteBank.DeleteByName(const aName: string);
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then begin
      DeleteByIndex(i);
      exit;
    end;

end;

procedure TSpriteBank.SaveTo(t: TStringList);
var i: SizeUInt;
begin
  t.Add('[SPRITE BANK]');
  t.Add(integer(Size).ToString);
  if Size > 0 then
    for i:=0 to Size-1 do begin
      t.Add(Mutable[i]^.name);
      t.Add(Mutable[i]^.textures);
      t.Add(Mutable[i]^.surfaces);
      t.Add(Mutable[i]^.collisionbodies);
      t.Add(Mutable[i]^.postures);
    end;
end;

procedure TSpriteBank.LoadFrom(t: TStringList);
var k, c, i: integer;
  o: TSpriteBankItem;
begin
  Clear;
  k := t.IndexOf('[SPRITE BANK]');
  if (k = -1) or (k = t.Count-1) then exit;

  inc(k);
  c := t.Strings[k].ToInteger;

  if c = 0 then exit;
  for i:=0 to c-1 do begin
    inc(k);
    o.name := t.Strings[k];
    inc(k);
    o.textures := t.Strings[k];
    inc(k);
    o.surfaces := t.Strings[k];
    inc(k);
    o.collisionbodies := t.Strings[k];
    inc(k);
    o.postures := t.Strings[k];
    PushBack(o);
  end;
end;

{ TBankUndoRedoManager }

procedure TBankUndoRedoManager.ProcessUndo(var aItem: TBankUndoRedoItem);
var o: PSpriteBankItem;
  i: integer;
begin
  case aItem.action of
    uratBankDeleteItem: begin
      o := SpriteBank.AddEmpty;
      o^.name := aItem.data.name;
      o^.textures := aItem.data.textures;
      o^.surfaces := aItem.data.surfaces;
      o^.collisionbodies := aItem.data.collisionbodies;
      o^.postures := aItem.data.postures;
      FrameToolsSpriteBank.LB.ItemIndex := FrameToolsSpriteBank.LB.Items.Add(o^.name);
    end;

    uratBankRenameItem: begin
      o := SpriteBank.GetItemByName(aItem.data.name);
      if o = NIL then begin raise exception.Create('bug: name not found in SpriteBank'); exit; end;
      o^.name := aItem.oldName;
      i := FrameToolsSpriteBank.LB.ItemIndex;
      if i <> -1 then FrameToolsSpriteBank.LB.Items.Strings[i] := aItem.oldName;
    end;

    uratBankAddItem: begin
      SpriteBank.DeleteByName(aItem.data.name);
      i := FrameToolsSpriteBank.LB.Items.IndexOf(aItem.data.name);
      if i <> -1 then FrameToolsSpriteBank.LB.Items.Delete(i);
    end;
  end;
end;

procedure TBankUndoRedoManager.ProcessRedo(var aItem: TBankUndoRedoItem);
var i: Integer;
  o: PSpriteBankItem;
begin
  case aItem.action of
    uratBankAddItem: begin
      o := SpriteBank.AddEmpty;
      o^.name := aItem.data.name;
      o^.textures := aItem.data.textures;
      o^.surfaces := aItem.data.surfaces;
      o^.collisionbodies := aItem.data.collisionbodies;
      o^.postures := aItem.data.postures;
      FrameToolsSpriteBank.LB.ItemIndex := FrameToolsSpriteBank.LB.Items.Add(o^.name);
    end;

    uratBankDeleteItem: begin
      SpriteBank.DeleteByName(aItem.data.name);
      i := FrameToolsSpriteBank.LB.Items.IndexOf(aItem.data.name);
      if i <> -1 then FrameToolsSpriteBank.LB.Items.Delete(i);
    end;

    uratBankRenameItem: begin
      o := SpriteBank.GetItemByName(aItem.oldName);
      if o = NIL then begin raise exception.Create('bug: oldName not found in SpriteBank'); exit; end;
      o^.name := aItem.data.name;
      i := FrameToolsSpriteBank.LB.ItemIndex;
      if i <> -1 then FrameToolsSpriteBank.LB.Items.Strings[i] := aItem.data.name;
    end;
  end;
end;

procedure TBankUndoRedoManager.AddActionDeleteSprite(aIndex: integer);
var o: TBankUndoRedoItem;
begin
  o := Default(TBankUndoRedoItem);
  o.action := uratBankDeleteItem;
  o.data.name := SpriteBank.Mutable[aIndex]^.name;
  o.data.textures := SpriteBank.Mutable[aIndex]^.textures;
  o.data.surfaces := SpriteBank.Mutable[aIndex]^.surfaces;
  o.data.collisionbodies := SpriteBank.Mutable[aIndex]^.collisionbodies;
  o.data.postures := SpriteBank.Mutable[aIndex]^.postures;
  AddItem(o);
end;

procedure TBankUndoRedoManager.AddActionRenameSprite(aIndex: integer; const aOldName: string);
var o: TBankUndoRedoItem;
begin
  o := Default(TBankUndoRedoItem);
  o.action := uratBankRenameItem;
  o.data.name := SpriteBank.Mutable[aIndex]^.name;
  o.data.textures := SpriteBank.Mutable[aIndex]^.textures;
  o.data.surfaces := SpriteBank.Mutable[aIndex]^.surfaces;
  o.data.collisionbodies := SpriteBank.Mutable[aIndex]^.collisionbodies;
  o.data.postures := SpriteBank.Mutable[aIndex]^.postures;
  o.oldName := aOldName;
  AddItem(o);
end;

procedure TBankUndoRedoManager.AddActionDuplicateSprite(aIndexNewItem: integer);
var o: TBankUndoRedoItem;
begin
  o := Default(TBankUndoRedoItem);
  o.action := uratBankAddItem;
  o.data.name := SpriteBank.Mutable[aIndexNewItem]^.name;
  o.data.textures := SpriteBank.Mutable[aIndexNewItem]^.textures;
  o.data.surfaces := SpriteBank.Mutable[aIndexNewItem]^.surfaces;
  o.data.collisionbodies := SpriteBank.Mutable[aIndexNewItem]^.collisionbodies;
  o.data.postures:= SpriteBank.Mutable[aIndexNewItem]^.postures;
  AddItem(o);
end;

end.

