unit u_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene;


const

// Scene layers
LAYER_COUNT = 6;
   LAYER_TOP             = 0;
   LAYER_UI              = 1;
   LAYER_COLLISION_BODY  = 2;
   LAYER_SPRITEBUILDER   = 3;
   LAYER_SPRITEBANK      = 4;
   LAYER_LEVELBANK       = 5;


var
  FScene: TOGLCScene;

implementation


end.

