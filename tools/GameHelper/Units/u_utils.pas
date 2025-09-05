unit u_utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene;

// return the size of an png, jpg, bmp image
function GetImageSize(const aFilename: string): TSize;

function PPIScale(AValue: integer): integer;
function PPIScaleF(AValue: single): single;


implementation

uses u_common;

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

end.

