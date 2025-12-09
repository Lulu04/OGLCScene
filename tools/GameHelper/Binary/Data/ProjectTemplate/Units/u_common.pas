unit u_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene;

{
  Please do not remove the comments in this file !
  they are tag used to mark code area for Game Helper
  You can edit manually the code inside a tagged area but keep the tags !
  NOTE 1: edit the code manually inside the tagged areas when Game Helper is not running
           because it overwrite their content when the project is saved
  NOTE 2:  when Game helper start, it read the content of the tagged areas and
           update the project with them.
}

const

{LAYERS}
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
{/LAYERS}

{DESIGN}
  SCREEN_WIDTH_AT_DESIGN_TIME: single = 1024;
  SCREEN_HEIGHT_AT_DESIGN_TIME: single = 768;
  SCREEN_PPI_AT_DESIGN_TIME: integer = 96;
{/DESIGN}

var

  AdditionnalScale: single = 1.0;
  FScene: TOGLCScene;
{VAR}
{/VAR}

// return the path to Data folder
function DataFolder: string;
function TexturesFolder: string;

// Scaling utils
function PPIScale(AValue: integer): integer;
function ScaleW(AValue: integer): integer;
function ScaleH(AValue: integer): integer;
function ScaleWF(AValue: single): single;
function ScaleHF(AValue: single): single;

implementation
{$I project_config.cfg}

function DataFolder: string;
begin
  Result := FScene.App.DataFolder;
end;

function TexturesFolder: string;
begin
  Result := DataFolder+'Textures'+DirectorySeparator;
end;

function PPIScale(AValue: integer): integer;
begin
  Result := FScene.ScaleDesignToScene(AValue);
end;

function ScaleW(AValue: integer): integer;
begin
{$ifdef MAXIMIZE_SCENE_ON_MONITOR}
  Result := Round(FScene.Width*AValue/SCREEN_WIDTH_AT_DESIGN_TIME*AdditionnalScale);
{$else}
  Result := Round(AValue*AdditionnalScale);
{$endif}
end;

function ScaleH(AValue: integer): integer;
begin
{$ifdef MAXIMIZE_SCENE_ON_MONITOR}
  Result := Round(FScene.Height*AValue/SCREEN_HEIGHT_AT_DESIGN_TIME*AdditionnalScale);
{$else}
  Result := Round(AValue*AdditionnalScale);
{$endif}
end;

function ScaleWF(AValue: single): single;
begin
{$ifdef MAXIMIZE_SCENE_ON_MONITOR}
  Result := FScene.Width*AValue/SCREEN_WIDTH_AT_DESIGN_TIME*AdditionnalScale;
{$else}
  Result := AValue*AdditionnalScale;
{$endif}
end;

function ScaleHF(AValue: single): single;
begin
{$ifdef MAXIMIZE_SCENE_ON_MONITOR}
  Result := FScene.Height*AValue/SCREEN_HEIGHT_AT_DESIGN_TIME*AdditionnalScale;
{$else}
  Result := AValue*AdditionnalScale;
{$endif}
end;

end.

