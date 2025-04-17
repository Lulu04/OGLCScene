unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene;


const

// LAYERS
   LAYER_COUNT = 2;
 LAYER_PARTICLE   = 0;
 LAYER_BACKGROUND = 1;

var
FScene: TOGLCScene;

//FPEngineLocked: boolean = FALSE;

FBackGroundRainbow,
FBackGroundColor : TQuad4Color;
FBackgroundImage : TSprite = NIL;

FModified: boolean = FALSE;
FProjectName: string='MyProject.par';

implementation

end.

