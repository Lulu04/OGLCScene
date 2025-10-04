unit u_utils;

{$mode ObjFPC}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, u_surface_list;

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

// return True if the passed class type use a texture
function ChildClassTypeIsTextured(const aClassName: string): boolean;


// utils to export pascal unit
function PointFToPascal(const aX, aY: single): string; overload;
function PointFToPascal(const aPt: TPointF): string; overload;
function BGRAToPascal(aColor: TBGRAPixel): string;

function CommonPropertiesToPascalCode(aSurface: PSurfaceDescriptor; const aSpacePrefix: string): string;
function ExtraPropertiesToPascalCode(aSurface: PSurfaceDescriptor; const aSpacePrefix: string): string;

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

function ChildClassTypeIsTextured(const aClassName: string): boolean;
begin
  case aClassName of
    'TSprite', 'TSpriteWithElasticCorner', 'TTiledSprite', 'TPolarSprite',
    'TScrollableSprite', 'TDeformationGrid': Result := True;

     'TShapeOutline', 'TGradientRectangle', 'TQuad4Color', 'TSpriteContainer': Result := False;

     else raise exception.create('"'+aClassName+'" not implemented');
  end;
end;

function PointFToPascal(const aX, aY: single): string;
begin
  Result := 'PointF('+FormatFloatWithDot('0.00', aX)+', '+FormatFloatWithDot('0.00', aY)+')';
end;

function PointFToPascal(const aPt: TPointF): string;
begin
  Result := PointFToPascal(aPt.x, aPt.y);
end;

function BGRAToPascal(aColor: TBGRAPixel): string;
begin
  Result := 'BGRA('+aColor.red.ToString+','+aColor.green.ToString+','+aColor.blue.ToString;
  if aColor.alpha <> 255 then Result := Result + ','+aColor.alpha.ToString;
  Result := Result + ')';
end;

function CommonPropertiesToPascalCode(aSurface: PSurfaceDescriptor; const aSpacePrefix: string): string;
begin
  Result := '';
  if (aSurface^.pivotX <> 0.5) or (aSurface^.pivotY <> 0.5) then
    Result := Result+aSpacePrefix+'Pivot := '+PointFToPascal(aSurface^.pivotX, aSurface^.pivotY)+';';
  if aSurface^.angle <> 0.0 then
    Result := Result+aSpacePrefix+'Angle.Value := '+FormatFloatWithDot('0.00', aSurface^.angle)+';';
  if (aSurface^.scaleX <> 1.0) or (aSurface^.scaleY <> 1.0) then
    Result := Result+aSpacePrefix+'Scale.Value := '+PointFToPascal(aSurface^.scaleX, aSurface^.scaleY)+';';
  if aSurface^.flipH then
    Result := Result+aSpacePrefix+'FlipH := True;';
  if aSurface^.flipV then
    Result := Result+aSpacePrefix+'FlipV := True;';
  if aSurface^.opacity <> 255 then
    Result := Result+aSpacePrefix+'Opacity.Value := '+ Round(aSurface^.opacity).ToString+';';
  if aSurface^.tint <> BGRA(0,0,0,0) then
    Result := Result+aSpacePrefix+'Tint.Value := '+BGRAToPascal(aSurface^.tint)+';';
  if aSurface^.tintMode <> tmReplaceColor then
    Result := Result+aSpacePrefix+'TintMode := tmMixColor;';
  if aSurface^.IsTextured and (aSurface^.frameindex <> 1.0) then
    Result := Result+aSpacePrefix+'Frame := '+FormatFloatWithDot('0.0', aSurface^.frameindex)+';';
end;

function ExtraPropertiesToPascalCode(aSurface: PSurfaceDescriptor; const aSpacePrefix: string): string;
begin
  case aSurface^.classtype.ClassName of
    'TSprite': begin
      Result := aSpacePrefix+'SetSize(ScaleW('+aSurface^.width.ToString+'), ScaleH('+
                aSurface^.height.ToString+'));';
    end;

    'TQuad4Color': begin
      Result := aSpacePrefix+'SetSize(ScaleW('+aSurface^.width.ToString+'), ScaleH('+aSurface^.height.ToString+'));'#10;
      if (aSurface^.TopLeftColor = aSurface^.TopRightColor) and
         (aSurface^.TopRightColor = aSurface^.BottomRightColor) and
         (aSurface^.BottomRightColor = aSurface^.BottomLeftColor) then begin
        Result := Result + aSpacePrefix+'SetAllColorsTo('+BGRAToPascal(aSurface^.TopLeftColor)+');';
      end else begin
        Result := Result + aSpacePrefix+'TopLeftColor.Value := '+BGRAToPascal(aSurface^.TopLeftColor)+');'#10+
                 aSpacePrefix+'TopRightColor.Value := '+BGRAToPascal(aSurface^.TopRightColor)+');'#10+
                 aSpacePrefix+'BottomRightColor.Value := '+BGRAToPascal(aSurface^.BottomRightColor)+');'#10+
                 aSpacePrefix+'BottomLeftColor.Value := '+BGRAToPascal(aSurface^.BottomLeftColor)+');';
      end;
    end;

    'TDeformationGrid': begin
      Result := aSpacePrefix+'SetSize(ScaleW('+aSurface^.width.ToString+'), ScaleH('+aSurface^.height.ToString+'));'#10+
                aSpacePrefix+'LoadDeformationDataFromString('''+aSurface^.DeformationGridData+''');';

    end

    else raise exception.Create('forgot to implement '+aSurface^.classtype.ClassName);
  end;
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

