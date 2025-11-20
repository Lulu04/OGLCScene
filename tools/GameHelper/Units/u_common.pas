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
   LAYER_SPRITEBUILDER    = 3;
   LAYER_SPRITEBANK       = 4;

   LAYER_LEVELBANK        = 5;
   LAYER_LEVELWORLDBOUNDS = 6;
   LAYER_LEVELEDITOR      = 7;

   FIRST_LAYER_USER       = 10;  // this is the layer index 0 for the user


DEFAULT_SKY_GRADIENT = '2 0.000 2 0.000 0080FFFF 1.000 0080FFFF 1.000 2 0.000 000080FF 1.000 000080FF';

PREFIX_FOR_SPRITE_UNIT_NAME = 'u_sprite_';
LEVEL_UNIT_NAME = 'u_gamelevels';

var
  FScene: TOGLCScene;
  FAtlas: TAtlas;
  FHintFont: TTexturedFont; // font for hint
  FErrorFont: TTexturedFont; // font for error message

  LastClickedIsControl: boolean=False;

  // handle textures
  texHandlePivot, texHandleRotate, texArrowH,
  texHandlePathNode: PTexture;
  // mouse cursor textures
  texMouseNormal,
  texSelectSurfaceByRect,
  texMouseOverSurface, texMouseOverPivot, texMouseRotateSurface, texMouseScaleSurface,
  texMouseOverNode, texMouseMovingNode, texMouseAddNode,
  texMouseToolPoint, texMouseToolLine, texMouseToolCircle, texMouseToolRectangle, texMouseToolPolygon: PTexture;

implementation


end.

