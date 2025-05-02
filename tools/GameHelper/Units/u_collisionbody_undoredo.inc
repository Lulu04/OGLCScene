{$ifdef _INTERFACE}

 // undo redo for tab collision body

type
TBodyUndoRedoActionType = (
                           buratUndefined,
                           buratAddShape,
                           buratDeleteShape,
                           buratModifyShape
                          );

TBodyUndoRedoItem = record
  action: TBodyUndoRedoActionType;
  ID: integer;
  itemDescriptor: TOGLCBodyItem;
  newItem: TOGLCBodyItem;
end;
PBodyUndoRedoItem = ^TBodyUndoRedoItem;

TBodyItemList = class;
{ TBodyUndoRedoManager }

TBodyUndoRedoManager = class(specialize TGenericUndoRedoManager<TBodyUndoRedoItem>)
  ParentBodylist: TBodyItemList;
  procedure ProcessUndo(var aItem: TBodyUndoRedoItem); override;
  procedure ProcessRedo(var aItem: TBodyUndoRedoItem); override;

  procedure AddActionAddShape(aItem: PBodyItem);
  procedure AddActionDelete(aItem: PBodyItem);
  function AddActionModify(const aPreviousDescriptor: TOGLCBodyItem; aNewItem: PBodyItem): PBodyUndoRedoItem;
end;


{$endif _INTERFACE}
{$ifdef _IMPLEMENTATION}

{ TBodyUndoRedoManager }

procedure TBodyUndoRedoManager.ProcessUndo(var aItem: TBodyUndoRedoItem);
begin
  case aItem.action of
    buratAddShape: ParentBodylist.DeleteByID(aItem.ID);

    buratDeleteShape: ParentBodylist.CreateItemByDescriptor(aItem.itemDescriptor, aItem.ID);

    buratModifyShape: ParentBodyList.ReplaceNodes(aItem.itemDescriptor, aItem.ID);
  end;
end;

procedure TBodyUndoRedoManager.ProcessRedo(var aItem: TBodyUndoRedoItem);
begin
  case aItem.action of
    buratAddShape: ParentBodylist.CreateItemByDescriptor(aItem.itemDescriptor, aItem.ID);

    buratDeleteShape: ParentBodylist.DeleteByID(aItem.ID);

    buratModifyShape: ParentBodyList.ReplaceNodes(aItem.newItem, aItem.ID);
  end;
end;

procedure TBodyUndoRedoManager.AddActionAddShape(aItem: PBodyItem);
begin
  with AddEmpty^ do begin
    action := buratAddShape;
    ID := aItem^.ID;
    itemDescriptor.CopyFrom(aItem^.ItemDescriptor);
  end;
end;

procedure TBodyUndoRedoManager.AddActionDelete(aItem: PBodyItem);
begin
  with AddEmpty^ do begin
    action := buratDeleteShape;
    ID := aItem^.ID;
    itemDescriptor.CopyFrom(aItem^.ItemDescriptor);
  end;
end;

function TBodyUndoRedoManager.AddActionModify(const aPreviousDescriptor: TOGLCBodyItem; aNewItem: PBodyItem
  ): PBodyUndoRedoItem;
begin
  Result := AddEmpty;
  Result^.action := buratModifyShape;
  Result^.ID := aNewItem^.ID;
  Result^.itemDescriptor.CopyFrom(aPreviousDescriptor);
  Result^.newItem.CopyFrom(aNewItem^.ItemDescriptor);
end;

{$endif _IMPLEMENTATION}
