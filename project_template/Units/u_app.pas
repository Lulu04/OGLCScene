unit u_app;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, OGLCScene, fgl;

// Change this values with yours
const
  SCREEN_WIDTH_AT_DESIGN_TIME: single = 1024;
  SCREEN_HEIGHT_AT_DESIGN_TIME: single = 768;

  function DataFolder: string;

  // in case you use ALSound to play musics and sounds
  function ALSoundLibrariesSubFolder: string;

  function PPIScale(AValue: integer): integer;
  function ScaleW(AValue: integer): integer;
  function ScaleH(AValue: integer): integer;
  function ScaleWF(AValue: single): single;
  function ScaleHF(AValue: single): single;

var
  AdditionnalScale: single = 1.0;

implementation
uses Forms, u_common;

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

function DataFolder: string;
begin
  Result := FScene.App.DataFolder;
end;

function ALSoundLibrariesSubFolder: string;
begin
  Result := FScene.App.ALSoundLibrariesSubFolder;
end;


end.

