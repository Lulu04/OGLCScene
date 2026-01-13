unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene,
  BGRABitmap, BGRABitmapTypes;


const

// LAYER
LAYER_COUNT = 5;
  Layer_InfoMap         = 0;
  Layer_Pattern         = 1;   // to show the pattern's list
  Layer_WorkTileSet     = 2;   // to show/edit the selected tileset
  Layer_WorkMap         = 3;   // to edit the layer's map
  Layer_Grid            = 4;   // to show the checkerboard


var

FScene: TOGLCScene=NIL;

FWorkingTileEngine: TTileEngine=NIL;
FTileSetTileEngine: TTileEngine;

FImageBackGround: TBGRABitmap=NIL;

FEventFont,
FHintFont: TTexturedFont;

FLabelCurrentLayerName,
FLabelMapPosition,
FLabelTileIndexes,
FLabelSelectionInfo,
FLabelGroundType,
FLabelEventName,
FLabelDebug: TFreeText;

FToolWindowIsVisible: boolean=TRUE;

FReady: boolean =FALSE;

FProjectIsModified: boolean=FALSE;
procedure SetProjectModified;

procedure EvalKey(Key: Word);

procedure OnlyThisLayerVisible(aL: integer);

implementation
uses form_main, LCLType,
  form_tools,
  u_tileset_edit,
  u_tool_minimap,
  u_tool_layer;

procedure SetProjectModified;
begin
 if not FProjectIsModified then
 begin
   FProjectIsModified := TRUE;
   FormMain.Caption := FormMain.Caption + ' - modified';
 end;
end;

procedure EvalKey(Key: Word);
var i: integer;
begin
 case Key of
  VK_PRIOR: begin  // PAGE_UP -> show previous tileset
    i := FormTools.CB1.ItemIndex - 1;
    if  i>=0 then FormTools.CB1.ItemIndex := i;
    TileSetEdit.ShowPreviousTileset;
    Key := VK_UNKNOWN;
  end;
  VK_NEXT: begin   // PAGE_DOWN -> show next tileset
    i := FormTools.CB1.ItemIndex + 1;
    if i<FormTools.CB1.Items.Count then FormTools.CB1.ItemIndex := i;
    TileSetEdit.ShowNextTileset;
    Key := VK_UNKNOWN;
  end;

  VK_F1: begin    // F1 -> Work on previous layer
   i := FormTools.CLBLayer.ItemIndex;
   if i>0 then FormTools.CLBLayer.ItemIndex:=i-1;
  end;

  VK_F2: begin    // F2 -> Work on next layer
   i := FormTools.CLBLayer.ItemIndex;
   if i<FormTools.CLBLayer.Count-1 then FormTools.CLBLayer.ItemIndex:=i+1;
  end;

  VK_F6: begin    // F6 -> show/hide tool window
   if FToolWindowIsVisible then begin
    FToolWindowIsVisible:=FALSE;
    FormTools.Hide;
   end else begin
    FToolWindowIsVisible:=TRUE;
    FormTools.Show;
   end;
  end;

  VK_W: if TileSetEdit.IsActive          // W -> toogle tileset view
     then SetViewOnLayer
     else SetViewOnTileSetEdit;

  VK_P: if PatternList.IsActive          // P -> toogle pattern list view
     then SetViewOnLayer
     else SetViewOnPatternList;

  VK_M: if not TileSetEdit.IsActive then begin   // M -> show tool MiniMap
   Form_Minimap.Show;
  end;

  VK_L: if not TileSetEdit.IsActive and not PatternList.IsActive then begin  // L -> show tool Layers
   Form_ToolLayer.Show;
  end;
 end;//case
end;

procedure OnlyThisLayerVisible(aL: integer);
var i: integer;
begin
 for i:=1 to LAYER_COUNT-2 do
   FScene.Layer[i].Visible := i = aL;
 FScene.Layer[Layer_InfoMap].Visible := TRUE;
 FScene.Layer[Layer_Grid].Visible := TRUE;
end;

end.

