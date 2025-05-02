{$ifdef _INTERFACE}

// undo redo for tab texture

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

TTextureList = class;

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

{$endif _INTERFACE}
{$ifdef _IMPLEMENTATION}

{ TTextureUndoRedoManager }

procedure TTextureUndoRedoManager.ProcessUndo(var aItem: TTextureUndoRedoItem);
var i: integer;
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

{$endif _IMPLEMENTATION}

