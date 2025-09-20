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


var
  FScene: TOGLCScene;
  FAtlas: TAtlas;
  FHintFont: TTexturedFont; // font for hint

  LastClickedIsControl: boolean=False;

implementation


end.

