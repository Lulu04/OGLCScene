unit u_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene;


const

// Scene layers
APP_LAYER_COUNT = 10;
   LAYER_TOP              = 0;
   LAYER_UI               = 1;
   LAYER_COLLISION_BODY   = 2;
   LAYER_SPRITEBUILDER    = 3; LAYER_FONT = 3; LAYER_UIPANEL = 3; LAYER_PATH = 3;
   LAYER_SPRITEBANK       = 4;

   LAYER_LEVELBANK        = 5;
   LAYER_LEVELWORLDBOUNDS = 6;
   LAYER_LEVELEDITOR      = 7;

   LAYER_SCENEBOUNDS      = 8; // to show the scene bounds (ex: 1024/768)

   LAYER_NOTVISIBLE       = 9;

   FIRST_LAYER_USER       = 10;  // this is the layer index 0 for the user


DEFAULT_GRADIENT = 'Visible false RowCount 2 YPos0 0.000 NodeCount0 2 R0_0XPos 0.000 R0_0Color 0080FFFF R0_1XPos 1.000 R0_1Color 0080FFFF YPos1 1.000 NodeCount1 2 R1_0XPos 0.000 R1_0Color 000080FF R1_1XPos 1.000 R1_1Color 000080FF';
DEFAULT_BODYSHAPE = 'ShapeType,1,RoundX,30.0000,RoundY,30.0000,Width,150,Height,50,FillData,Color 1E1E1EFF BlendMode 0,BorderData,Color DCDCDCFF Width 3.0000 BlendMode 0 LinePos 1';
DEFAULT_SLIDER_BODYSHAPE_H = 'ShapeType,1,RoundX,7.0000,RoundY,7.0000,Width,20,Height,13,FillData,Color 1E1E1EFF BlendMode 0,BorderData,Color DCDCDCFF Width 3.0000 BlendMode 0 LinePos 1';
DEFAULT_SLIDER_BODYSHAPE_V = 'ShapeType,1,RoundX,7.0000,RoundY,7.0000,Width,13,Height,20,FillData,Color 1E1E1EFF BlendMode 0,BorderData,Color DCDCDCFF Width 3.0000 BlendMode 0 LinePos 1';


LEVEL_UNIT_NAME = 'u_gamelevels';

var
  FScene: TOGLCScene;
  FAtlas: TAtlas;
  FHintFont: TTexturedFont;  // font for volatile hints
  FErrorFont: TTexturedFont; // font for error messages

  LastClickedIsControl: boolean=False;

  // handle textures
  texHandlePivot, texHandleRotate, texArrowH,
  texHandlePathNode, texHandlePathNodeCircle: PTexture;
  // mouse cursor textures
  texMouseNormal,
  texSelectSurfaceByRect,
  texMouseOverSurface, texMouseOverPivot, texMouseRotateSurface, texMouseScaleSurface,
  texMouseOverNode, texMouseMovingNode, texMouseAddNode,
  texmouseAddNodeCircle, texMouseInsertNodeCircle,
  texMouseToolPoint, texMouseToolLine, texMouseToolCircle, texMouseToolRectangle, texMouseToolPolygon: PTexture;

implementation


end.

