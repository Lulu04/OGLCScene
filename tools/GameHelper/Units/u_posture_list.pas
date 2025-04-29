unit u_posture_list;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, StdCtrls,
  OGLCScene, gvector,
  u_surface_list, u_undo_redo;

type

{ TPostureValues }

TPostureValues = record
  angle, x, y: single;
  function SaveToString: string;
  procedure LoadFromString(const s: string);
end;
ArrayOfPostureValues = array of TPostureValues;

{ TPostureItem }

TPostureItem = record
  name: string;
  Values: ArrayOfPostureValues;  // values for each child
  procedure Initdefault;
  procedure TakeValuesFrom(aSurfaceList: TSurfaceList);
  function SaveToString: string;
  procedure LoadFromString(const s: string);
end;
PPostureItem =^TPostureItem;

{ TPostureList }

TPostureList = class(specialize TVector<TPostureItem>)

public
  //procedure Clear; reintroduce;
  function AddEmpty: PPostureItem;
  function InsertEmpty(aIndex: SizeUInt): PPostureItem;
  procedure DeleteItemByName(const aName: string);
  function NameAlreadyExists(const aName: string): boolean;
  function GetItemByName(const aName: string): PPostureItem;

  function SaveToString: string;
  procedure LoadFromString(const s: string);

  procedure FillListBox(aLB: TListBox);
end;

type
TPostureUndoRedoActionType = (
                           puratUndefined,
                           puratAddPosture,
                           puratDeletePosture,
                           puratModifyPosture,
                           puratRenamePosture
                          );
TPostureUndoRedoItem = record
  action: TPostureUndoRedoActionType;
  data: TPostureItem;
  newData: TPostureItem;
  ListBoxitemIndexWhenDeleted: integer;
end;
PPostureUndoRedoItem = ^TPostureUndoRedoItem;

{ TPostureUndoRedoManager }

TPostureUndoRedoManager = class(specialize TGenericUndoRedoManager<TPostureUndoRedoItem>)
  procedure ProcessUndo(var aItem: TPostureUndoRedoItem); override;
  procedure ProcessRedo(var aItem: TPostureUndoRedoItem); override;

  procedure AddActionAddPosture(aItem: PPostureItem);
end;

implementation

uses u_screen_spritebuilder, form_main;

{ TPostureValues }

function TPostureValues.SaveToString: string;
begin
  Result := FormatFloatWithDot('0.000', x)+' '+
            FormatFloatWithDot('0.000', y)+' '+
            FormatFloatWithDot('0.000', angle);
end;

procedure TPostureValues.LoadFromString(const s: string);
var A: TStringArray;
begin
  A := s.Split([' ']);
  x := StringToSingle(A[0]);
  y := StringToSingle(A[1]);
  angle := StringToSingle(A[2]);
end;

{ TPostureItem }

procedure TPostureItem.Initdefault;
begin
  Self := Default(TPostureItem);
end;

procedure TPostureItem.TakeValuesFrom(aSurfaceList: TSurfaceList);
var item: PSurfaceDescriptor;
  i: SizeUInt;
begin
  Values := NIL;
  SetLength(Values, aSurfaceList.Size);
  if aSurfaceList.Size = 0 then exit;

  for i:=0 to aSurfaceList.Size-1 do begin
    item := aSurfaceList.Mutable[i];
    Values[i].x := item^.surface.X.Value;
    Values[i].y := item^.surface.Y.Value;
    Values[i].angle := item^.surface.Angle.Value;
  end;
end;

function TPostureItem.SaveToString: string;
var prop: TProperties;
  i: integer;
begin
  prop.Init('~');
  prop.Add('Name', name);
  prop.Add('Count', Length(Values));
  for i:=0 to High(Values) do
    prop.Add('Value'+i.ToString, Values[i].SaveToString);
  Result := prop.PackedProperty;
end;

procedure TPostureItem.LoadFromString(const s: string);
var prop: TProperties;
  i, c: integer;
  s1: string;
begin
  c := 0;
  s1 := '';
  Initdefault;
  prop.Split(s, '~');
  prop.StringValueOf('Name', name, name);
  prop.IntegerValueOf('Count', c, 0);
  Values := NIL;
  if c = 0 then exit;
  SetLength(Values, c);
  for i:=0 to c-1 do
    if prop.StringValueOf('Value'+i.ToString, s1, '') then
      Values[i].LoadFromString(s1);
end;

{ TPostureList }

function TPostureList.AddEmpty: PPostureItem;
var o: TPostureItem;
begin
  o.Initdefault;
  PushBack(o);
  Result := Mutable[Size-1];
end;

function TPostureList.InsertEmpty(aIndex: SizeUInt): PPostureItem;
var o: TPostureItem;
begin
  o.Initdefault;
  Self.Insert(aIndex, o);
  Result := Mutable[aIndex];
end;

procedure TPostureList.DeleteItemByName(const aName: string);
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then
      Self.Erase(i);
end;

function TPostureList.NameAlreadyExists(const aName: string): boolean;
var i: SizeUInt;
begin
  Result := False;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then exit(True);
end;

function TPostureList.GetItemByName(const aName: string): PPostureItem;
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

function TPostureList.SaveToString: string;
var prop: TProperties;
  i: SizeUInt;
begin
  prop.Init('|');
  prop.Add('Count', integer(Size));
  if Size > 0 then
    for i:=0 to Size-1 do begin
       prop.Add('Posture'+i.ToString, Mutable[i]^.SaveToString);
    end;
  Result := prop.PackedProperty;
end;

procedure TPostureList.LoadFromString(const s: string);
var prop: TProperties;
  c, i: integer;
  s1: string;
  o: TPostureItem;
begin
  Clear;
  c := 0;
  s1 := '';
  prop.Split(s, '|');
  prop.IntegerValueOf('Count', c, 0);
  if c = 0 then exit;
  for i:=0 to c-1 do begin
     o.Initdefault;
     if prop.StringValueOf('Posture'+i.ToString, s1, '') then
       o.LoadFromString(s1);
     PushBack(o);
  end;
end;

procedure TPostureList.FillListBox(aLB: TListBox);
var i: SizeUInt;
begin
  aLB.Clear;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    aLB.Items.Add(Mutable[i]^.name);
end;

{ TPostureUndoRedoManager }

procedure TPostureUndoRedoManager.ProcessUndo(var aItem: TPostureUndoRedoItem);
var i: integer;
  item: PPostureItem;
begin
  case aItem.action of
    puratAddPosture: begin
      ScreenSpriteBuilder.Postures.DeleteItemByName(aItem.data.name);
      i := FrameToolsSpriteBuilder.LBPostureNames.Items.IndexOf(aItem.data.name);
      if i <> -1 then FrameToolsSpriteBuilder.LBPostureNames.Items.Delete(i);
    end;

    puratDeletePosture: begin
      item := ScreenSpriteBuilder.Postures.InsertEmpty(aItem.ListBoxitemIndexWhenDeleted);
      item^.name := aItem.data.name;
      item^.Values := Copy(aItem.data.Values);
      with FrameToolsSpriteBuilder.LBPostureNames do begin
        Items.Insert(aItem.ListBoxitemIndexWhenDeleted, aItem.data.name);
        ItemIndex := aItem.ListBoxitemIndexWhenDeleted;
      end;
    end;

    puratRenamePosture: begin
      item := ScreenSpriteBuilder.Postures.GetItemByName(aItem.newData.name);
      if item <> NIL then item^.name := aItem.data.name;
      i := FrameToolsSpriteBuilder.LBPostureNames.Items.IndexOf(aItem.newData.name);
      if i <> -1 then FrameToolsSpriteBuilder.LBPostureNames.Items.Strings[i] := aItem.data.name;
    end;

    puratModifyPosture: begin
      item := ScreenSpriteBuilder.Postures.GetItemByName(aItem.newData.name);
      if item <> NIL then begin
        item^.name := aItem.data.name;
        item^.Values := Copy(aItem.data.Values);
        i := FrameToolsSpriteBuilder.LBPostureNames.Items.IndexOf(aItem.newData.name);
        if i <> -1 then FrameToolsSpriteBuilder.LBPostureNames.Items.Strings[i] := aItem.data.name;
      end;
    end;
  end;
end;

procedure TPostureUndoRedoManager.ProcessRedo(var aItem: TPostureUndoRedoItem);
var item: PPostureItem;
  i: integer;
begin
  case aItem.action of
    puratAddPosture: begin
      item := ScreenSpriteBuilder.Postures.AddEmpty;
      item^.name := aItem.data.name;
      item^.Values := Copy(aItem.data.Values);
      with FrameToolsSpriteBuilder.LBPostureNames do
        ItemIndex := Items.Add(aItem.data.name);
    end;

    puratDeletePosture: begin
      ScreenSpriteBuilder.Postures.DeleteItemByName(aItem.data.name);
      FrameToolsSpriteBuilder.LBPostureNames.Items.Delete(aItem.ListBoxitemIndexWhenDeleted);
      //i := FrameToolsSpriteBuilder.LBPostureNames.Items.IndexOf(aItem.data.name);
      //if i <> -1 then FrameToolsSpriteBuilder.LBPostureNames.Items.Delete(i);
    end;

    puratRenamePosture: begin
      item := ScreenSpriteBuilder.Postures.GetItemByName(aItem.Data.name);
      if item <> NIL then item^.name := aItem.newData.name;
      i := FrameToolsSpriteBuilder.LBPostureNames.Items.IndexOf(aItem.Data.name);
      if i <> -1 then FrameToolsSpriteBuilder.LBPostureNames.Items.Strings[i] := aItem.newData.name;
    end;

    puratModifyPosture: begin
      item := ScreenSpriteBuilder.Postures.GetItemByName(aItem.Data.name);
      if item <> NIL then begin
        item^.name := aItem.newData.name;
        item^.Values := Copy(aItem.newData.Values);
        i := FrameToolsSpriteBuilder.LBPostureNames.Items.IndexOf(aItem.Data.name);
        if i <> -1 then FrameToolsSpriteBuilder.LBPostureNames.Items.Strings[i] := aItem.newData.name;
      end;
    end;
  end;
end;

procedure TPostureUndoRedoManager.AddActionAddPosture(aItem: PPostureItem);
var o: TPostureUndoRedoItem;
begin
  o := Default(TPostureUndoRedoItem);
  o.action := puratAddPosture;
  o.data.name := aItem^.name;
  o.data.Values := Copy(aItem^.Values);
  AddItem(o);
end;

end.

