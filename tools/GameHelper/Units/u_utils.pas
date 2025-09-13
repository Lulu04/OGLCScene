unit u_utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene;

// return the size of an png, jpg, bmp image
function GetImageSize(const aFilename: string): TSize;

function PPIScale(AValue: integer): integer;
function PPIScaleF(AValue: single): single;

// Return the color:  c + c*percent   with percent]-1..1[
function PercentColor(c: TColor; percent: single): TColor;

implementation

uses u_common, Math;

function GetImageSize(const aFilename: string): TSize;
var ima: TBGRABitmap;
begin
  ima := TBGRABitmap.Create(aFilename);
  Result.cx := ima.Width;
  Result.cy := ima.Height;
  ima.Free;
end;

function PPIScale(AValue: integer): integer;
begin
  Result := FScene.ScaleDesignToScene(AValue);
end;

function PPIScale(AValue: single): single;
begin
  Result := FScene.ScaleDesignToSceneF(AValue);
end;

function PPIScaleF(AValue: single): single;
begin
  Result := FScene.ScaleDesignToSceneF(AValue);
end;

function PercentColor(c: TColor; percent: single): TColor;
var r, g, b: integer;
begin
  r := Red(c);
  g := Green(c);
  b := Blue(c);
  r := EnsureRange(Round(r + ( r * percent )), 0, 255);
  g := EnsureRange(Round(g + ( g * percent )), 0, 255);
  b := EnsureRange(Round(b + ( b * percent )), 0, 255);
  Result := RGBToColor(r, g, b);
end;

end.

