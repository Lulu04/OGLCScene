{$ifdef _INTERFACE}

// undo redo for tab root/child

type
TSurfaceUndoRedoActionType = (
                           suratUndefined,
                           suratAddSurface,
                           suratDeleteSurface,
                           suratModifySurface
                          );
TSurfaceUndoRedoItem = record
  action: TSurfaceUndoRedoActionType;
  data: TSurfaceDescriptor;
  newData: TSurfaceDescriptor;
end;
PSurfaceUndoRedoItem = ^TSurfaceUndoRedoItem;



{ TSurfaceUndoRedoManager }

TSurfaceUndoRedoManager = class(specialize TGenericUndoRedoManager<TSurfaceUndoRedoItem>)
  procedure ProcessUndo(var aItem: TSurfaceUndoRedoItem); override;
  procedure ProcessRedo(var aItem: TSurfaceUndoRedoItem); override;

end;

{$endif _INTERFACE}
{$ifdef _IMPLEMENTATION}

{ TSurfaceUndoRedoManager }

procedure TSurfaceUndoRedoManager.ProcessUndo(var aItem: TSurfaceUndoRedoItem);
begin
  case aItem.action of
    suratAddSurface: begin
    end;

    suratDeleteSurface: begin
    end;

    suratModifySurface: begin
    end;
  end;
end;

procedure TSurfaceUndoRedoManager.ProcessRedo(var aItem: TSurfaceUndoRedoItem);
begin
  case aItem.action of
    suratAddSurface: begin
    end;

    suratDeleteSurface: begin
    end;

    suratModifySurface: begin
    end;
  end;
end;

{$endif _IMPLEMENTATION}

