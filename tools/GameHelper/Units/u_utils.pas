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

procedure ExchangeString(var s1, s2: string);
procedure ExchangeInteger(var v1, v2: integer);
procedure ExchangeBoolean(var b1, b2: boolean);

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

end;

function PPIScaleF(AValue: single): single;
begin
  Result := FScene.ScaleDesignToSceneF(AValue);
end;

procedure ExchangeString(var s1, s2: string);
var temp: string;
begin
  temp := s1;
  s1 := s2;
  s2 := temp;
end;

procedure ExchangeInteger(var v1, v2: integer);
var temp: integer;
begin
  temp := v1;
  v1 := v2;
  v2 := temp;
end;

procedure ExchangeBoolean(var b1, b2: boolean);
var temp: boolean;
begin
  temp := b1;
  b1 := b2;
  b2 := temp;
end;

end.

