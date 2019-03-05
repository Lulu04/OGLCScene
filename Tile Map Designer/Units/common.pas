unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene,
  BGRABitmap, BGRABitmapTypes;


const

// LAYER
LAYER_COUNT = 4;
  Layer_InfoMap         = 0;
  Layer_WorkTileSet     = 1;
  Layer_WorkMap         = 2;
  Layer_Grid            = 3;


var

FScene: TOGLCScene=NIL;

FWorkingTileEngine: TTileEngine;

FImageBackGround: TBGRABitmap;


FTitleFont: TGuiFont;

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

procedure EvalKey( Key: Word );

implementation
uses u_main, LCLType,
  u_tool_window,
  u_tileset_edit;

procedure SetProjectModified;
begin
 if not FProjectIsModified then
 begin
   FProjectIsModified := TRUE;
   Form_Main.Caption := Form_Main.Caption + ' - modified';
 end;
end;

procedure EvalKey(Key: Word);
var i: integer;
begin
 case Key of
  VK_PRIOR: begin  // PAGE_UP -> show previous tileset
    i := Form_Tools.CB1.ItemIndex - 1;
    if  i>=0 then Form_Tools.CB1.ItemIndex := i;
    TileSetEdit.ShowPreviousTileset;
    Key := VK_UNKNOWN;
  end;
  VK_NEXT: begin   // PAGE_DOWN -> show next tileset
    i := Form_Tools.CB1.ItemIndex + 1;
    if i<Form_Tools.CB1.Items.Count then Form_Tools.CB1.ItemIndex := i;
    TileSetEdit.ShowNextTileset;
    Key := VK_UNKNOWN;
  end;

  VK_F1: begin    // F1 -> Work on previous layer
   i := Form_Tools.CLBLayer.ItemIndex;
   if i>0 then Form_Tools.CLBLayer.ItemIndex:=i-1;
  end;

  VK_F2: begin    // F2 -> Work on next layer
   i := Form_Tools.CLBLayer.ItemIndex;
   if i<Form_Tools.CLBLayer.Count-1 then Form_Tools.CLBLayer.ItemIndex:=i+1;
  end;

  VK_F6: begin    // F6 -> show/hide tool window
   if FToolWindowIsVisible then begin
    FToolWindowIsVisible:=FALSE;
    Form_Tools.Hide;
   end else begin
    FToolWindowIsVisible:=TRUE;
    Form_Tools.Show;
   end;
  end;

  VK_W: with TileSetEdit do if IsActive          // W -> toogle tileset view
     then ExitModeEditTileset
     else EnterModeEditTileset;

  VK_M: if not TileSetEdit.IsActive then begin   // M -> go to the MiniMap tool
   with Form_Tools do PageControl1.ActivePage:=pageMiniMap;
  end;
 end;//case
end;

end.

