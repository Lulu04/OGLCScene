unit u_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OGLCScene;


const

// LAYER for scene1
   LAYER_COUNT_SCENE1 = 1;

// LAYER for scene2
   LAYER_COUNT_SCENE2 = 3;
      LAYER_TOP    = 0;
      LAYER_MIDDLE = 1;
      LAYER_BACK   = 2;

// LAYER for scene3
   LAYER_COUNT_SCENE3 = 1;

// LAYER for scene4
   LAYER_COUNT_SCENE4 = 1;

var
FScene1, FScene2, FScene3, FScene4: TOGLCScene;


implementation

end.

