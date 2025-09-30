unit u_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene;


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


// return the path to Data folder
function DataFolder: string;

const
  // Change this values according to the size and ppi of the window when you designed the graphics
   SCREEN_WIDTH_AT_DESIGN_TIME: single = 1024;
   SCREEN_HEIGHT_AT_DESIGN_TIME: single = 768;
   SCREEN_PPI_AT_DESIGN_TIME: integer = 96;

var
  AdditionnalScale: single = 1.0;

  // Scaling utils
  function PPIScale(AValue: integer): integer;
  function ScaleW(AValue: integer): integer;
  function ScaleH(AValue: integer): integer;
  function ScaleWF(AValue: single): single;
  function ScaleHF(AValue: single): single;


implementation

function DataFolder: string;
begin
  Result := FScene.App.DataFolder;
end;

function PPIScale(AValue: integer): integer;
begin
  Result := FScene.ScaleDesignToScene(AValue);
end;

function ScaleW(AValue: integer): integer;
begin
  Result := Round(FScene.Width*AValue/SCREEN_WIDTH_AT_DESIGN_TIME*AdditionnalScale);
end;

function ScaleH(AValue: integer): integer;
begin
  Result := Round(FScene.Height*AValue/SCREEN_HEIGHT_AT_DESIGN_TIME*AdditionnalScale);
end;

function ScaleWF(AValue: single): single;
begin
  Result := FScene.Width*AValue/SCREEN_WIDTH_AT_DESIGN_TIME*AdditionnalScale;
end;

function ScaleHF(AValue: single): single;
begin
  Result := FScene.Height*AValue/SCREEN_HEIGHT_AT_DESIGN_TIME*AdditionnalScale;
end;

end.

