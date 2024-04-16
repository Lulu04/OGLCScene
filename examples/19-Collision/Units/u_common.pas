unit u_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OGLCScene;


const

// LAYER
   LAYER_COUNT = 6;
     LAYER_GUI    = 0;
     LAYER_FX     = 1;
     LAYER_LASER  = 2;
     LAYER_METEOR = 3;
     LAYER_PLAYER = 4;
     LAYER_BACK   = 5;


var
FScene: TOGLCScene;


implementation

end.

