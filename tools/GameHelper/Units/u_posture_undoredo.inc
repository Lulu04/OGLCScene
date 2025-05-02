{$ifdef _INTERFACE}

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

{$endif _INTERFACE}
{$ifdef _IMPLEMENTATION}

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

{$endif _IMPLEMENTATION}

