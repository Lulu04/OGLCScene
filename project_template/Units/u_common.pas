unit u_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OGLCScene;


const

// LAYERS
LAYER_COUNT = 9;

   LAYER_TOP    = 0;
   LAYER_UI     = 1;
   LAYER_PLAYER = 2;
   LAYER_ENEMY  = 3;
   LAYER_FX     = 4;
   LAYER_DECOR  = 5;
   LAYER_BG1    = 6;
   LAYER_BG2    = 7;
   LAYER_BG3    = 8;

var
FScene: TOGLCScene;


implementation

end.

