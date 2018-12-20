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
  Layer_Info            = 0;
  Layer_SecondMapTop    = 1;
  Layer_WorkMap         = 2;
  Layer_SecondMapBelow  = 3;
  Layer_Grid            = 4;


var

FScene: TOGLCScene=NIL;

FTileEngine: TTileEngine=NIL;
FTopOverlayTileEngine,
FBelowOverlayTileEngine,
FTileEngineGrid: TTileEngine;

FImageBackGround: TBGRABitmap;


FTitleFont: TGuiFont;

FEventFont,
FHintFont: TTexturedFont;

FLabelMapPosition,
FLabelTileIndexes,
FLabelSelectionInfo,
FLabelGroundType,
FLabelEventName,
FLabelDebug: TFreeText;



FReady: boolean =FALSE;

FProjectIsModified: boolean=FALSE;
procedure SetProjectModified;

implementation
uses Main;

procedure SetProjectModified;
begin
 if not FProjectIsModified then
 begin
   FProjectIsModified := TRUE;
   Form_Principale.Caption := Form_Principale.Caption + ' - modified';
 end;
end;

end.

