unit u_utils;

{$mode ObjFPC}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene;

// return the size of an png, jpg, bmp image
function GetImageSize(const aFilename: string): TSize;
function GetSVGImageSize(const aSVGFilename: string): TSize;

function PPIScale(AValue: integer): integer;
function PPIScaleF(AValue: single): single;

// Return the color:  c + c*percent   with percent]-1..1[
function PercentColor(c: TColor; percent: single): TColor;

// utils for array of integer
type

{ TArrayOfIntegerHelper }

TArrayOfIntegerHelper = type helper for TArrayOfInteger
  function Count: integer; inline;
  function Have(aValue: integer): boolean;
  function IndexOf(aValue: integer): integer;
  procedure Add(aValue: integer);
  procedure AddOnlyOneTime(aValue: integer);
  procedure SortFromSmallToHigh;
end;


implementation

uses u_common, Math, BGRASVG;

function GetImageSize(const aFilename: string): TSize;
var ima: TBGRABitmap;
begin
  ima := TBGRABitmap.Create(aFilename);
  Result.cx := ima.Width;
  Result.cy := ima.Height;
  ima.Free;
end;

function GetSVGImageSize(const aSVGFilename: string): TSize;
var svg: TBGRASVG;
begin
  svg := TBGRASVG.Create(aSVGFileName);
  Result.cx := Round(svg.WidthAsPixel);
  Result.cy := Round(svg.HeightAsPixel);
  svg.Free;
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

{ TArrayOfIntegerHelper }

function TArrayOfIntegerHelper.Count: integer;
begin
  Result := Length(Self);
end;

function TArrayOfIntegerHelper.Have(aValue: integer): boolean;
var i: integer;
begin
  for i:=0 to High(Self) do
    if Self[i] = aValue then exit(True);
  Result := False;
end;

function TArrayOfIntegerHelper.IndexOf(aValue: integer): integer;
var i: integer;
begin
  for i:=0 to High(Self) do
    if Self[i] = aValue then exit(i);
  Result := -1;
end;

procedure TArrayOfIntegerHelper.Add(aValue: integer);
begin
  SetLength(Self, Length(Self)+1);
  Self[High(Self)] := aValue;
end;

procedure TArrayOfIntegerHelper.AddOnlyOneTime(aValue: integer);
begin
  if not Have(aValue) then Add(aValue);
end;

procedure TArrayOfIntegerHelper.SortFromSmallToHigh;
var i, k: integer;
  flag: boolean;
begin
  repeat
    flag := False;
    for i:=0 to High(Self)-1 do
      if Self[i] > Self[i+1] then begin
        k := Self[i+1];
        Self[i+1] := Self[i];
        Self[i] := k;
        flag := true;
      end;
  until not flag;
end;

end.

