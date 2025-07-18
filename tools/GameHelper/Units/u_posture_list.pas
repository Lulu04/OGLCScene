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
  angle, x, y, scalex, scaley: single;
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

{$define _INTERFACE}
{$I u_posture_undoredo.inc}
{$undef _INTERFACE}

{ TPostureList }

TPostureList = class(specialize TVector<TPostureItem>)
private
  FUndoRedoManager: TPostureUndoRedoManager;
public
  constructor Create;
  destructor Destroy; override;

  procedure Clear; reintroduce;
  function AddEmpty: PPostureItem;
  function InsertEmpty(aIndex: SizeUInt): PPostureItem;
  procedure DeleteItemByName(const aName: string);
  function NameAlreadyExists(const aName: string): boolean;
  function GetItemByName(const aName: string): PPostureItem;

  // called when user delete a surface
  procedure DeleteValueEntryInEachPosture(aIndex: integer);
  // called when user add a surface
  procedure AddValueEntryOnEachPosture;

  function SaveToString: string;
  procedure LoadFromString(const s: string);

  procedure FillListBox(aLB: TListBox);

  property UndoRedoManager: TPostureUndoRedoManager read FUndoRedoManager;
end;

implementation

uses u_screen_spritebuilder, form_main;

{$define _IMPLEMENTATION}
{$I u_posture_undoredo.inc}
{$undef _IMPLEMENTATION}

{ TPostureValues }

function TPostureValues.SaveToString: string;
begin
  Result := FormatFloatWithDot('0.000', x)+' '+
            FormatFloatWithDot('0.000', y)+' '+
            FormatFloatWithDot('0.000', angle)+' '+
            FormatFloatWithDot('0.000', scalex)+' '+
            FormatFloatWithDot('0.000', scaley);
end;

procedure TPostureValues.LoadFromString(const s: string);
var A: TStringArray;
begin
  A := s.Split([' ']);
  x := StringToSingle(A[0]);
  y := StringToSingle(A[1]);
  angle := StringToSingle(A[2]);
  if Length(A) > 3 then scalex := StringToSingle(A[3]) else scalex := 1.0;
  if Length(A) > 4 then scaley := StringToSingle(A[4]) else scaley := 1.0;
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
    Values[i].scalex := item^.surface.Scale.X.Value;
    Values[i].scaley := item^.surface.Scale.Y.Value;
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

constructor TPostureList.Create;
begin
  inherited Create;
  FUndoRedoManager := TPostureUndoRedoManager.Create;
end;

destructor TPostureList.Destroy;
begin
  FUndoRedoManager.Free;
  FUndoRedoManager := NIL;
  inherited Destroy;
end;

procedure TPostureList.Clear;
begin
  inherited Clear;
  FUndoRedoManager.Clear;
end;

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

procedure TPostureList.DeleteValueEntryInEachPosture(aIndex: integer);
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    Delete(Mutable[i]^.Values, aIndex, 1);
end;

procedure TPostureList.AddValueEntryOnEachPosture;
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    SetLength(Mutable[i]^.Values, Length(Mutable[i]^.Values)+1);
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

end.

