unit u_undoredo_spritebuilder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  u_undo_redo;

type
TSpriteBuilderUndoRedoActionType = (
                           spuratUndefined
                          );
TSpriteBuilderUndoRedoItem = record
  action: TSpriteBuilderUndoRedoActionType;
  name, textures, surfaces, collisionbodies: string;
  oldName: string;
end;

{TSpriteBuilderUndoRedoManager = class(specialize TGenericUndoRedoManager<TSpriteBuilderUndoRedoItem>)
  procedure ProcessUndo(var aItem: TBankUndoRedoItem); override;
  procedure ProcessRedo(var aItem: TBankUndoRedoItem); override;

end;  }

implementation

end.

