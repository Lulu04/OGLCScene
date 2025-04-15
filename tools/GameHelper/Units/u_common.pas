unit u_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OGLCScene;


const

// Scene layers
LAYER_COUNT = 4;
   LAYER_TOP           = 0;
   LAYER_UI            = 1;
   LAYER_SPRITEBUILDER = 2;
   LAYER_SPRITEBANK    = 3;


var
  FScene: TOGLCScene;
  FContainer: TSpriteContainer; // used to contain sprite

implementation


end.

