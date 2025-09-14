unit u_undo_redo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  UNDO_REDO_MAX_ITEMS = 100;
type

{ TGenericUndoRedoManager }

generic TGenericUndoRedoManager<T> = class
private type PT = ^T;
private
  FItems: array[0..UNDO_REDO_MAX_ITEMS-1] of T;
  FNextIndex: integer;
  FAvailableUndo, FAvailableRedo: integer;
public
  procedure Clear;
  function AddEmpty: PT;
  procedure AddItem(const aItem: T);
  function CanUndo: boolean;
  function CanRedo: boolean;

  procedure Undo; virtual;
  procedure Redo; virtual;

  procedure ProcessUndo(var aItem: T); virtual; abstract;
  procedure ProcessRedo(var aItem: T); virtual; abstract;
end;

implementation

{ TGenericUndoRedoManager }

procedure TGenericUndoRedoManager.Clear;
begin
  FNextIndex := 0;
  FAvailableUndo := 0;
  FAvailableRedo := 0;
end;

function TGenericUndoRedoManager.AddEmpty: PT;
var o: T;
begin
  Result := @FItems[FNextIndex];
  o := Default(T);
  AddItem(o);
end;

procedure TGenericUndoRedoManager.AddItem(const aItem: T);
begin
  FItems[FNextIndex] := aItem;
  inc(FNextIndex);

  if FNextIndex = Length(FItems) then
    FNextIndex := 0;

  if FAvailableUndo < UNDO_REDO_MAX_ITEMS-1 then
    inc(FAvailableUndo);

  FAvailableRedo := 0;
end;

function TGenericUndoRedoManager.CanUndo: boolean;
begin
  Result := FAvailableUndo > 0;
end;

function TGenericUndoRedoManager.CanRedo: boolean;
begin
  Result := FAvailableRedo > 0;
end;

procedure TGenericUndoRedoManager.Undo;
begin
  if not CanUndo then exit;
  dec(FAvailableUndo);
  inc(FAvailableRedo);
  dec(FNextIndex);
  if FNextIndex = -1 then FNextIndex := High(FItems);
  ProcessUndo(FItems[FNextIndex]);
end;

procedure TGenericUndoRedoManager.Redo;
begin
  if not CanRedo then exit;
  dec(FAvailableRedo);
  inc(FAvailableUndo);
  ProcessRedo(FItems[FNextIndex]);
  inc(FNextIndex);
  if FNextIndex = Length(FItems) then
    FNextIndex := 0;
end;

end.

