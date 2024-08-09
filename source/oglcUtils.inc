{$ifdef oglcINTERFACE}

function GLErrorToString(aError: GLenum): string;

function FormatFloatWithDot(const aFmt: string; aValue: single): string;
// convert the string to single. The string can have '.' or ','
function StringToSingle(const aStr: string): single;


// Add chars to a charset, avoiding redundant char. Return the modified charset.
// Usefull to create a charset for Textured Font.
// The function replace all kind of LineEnding by #10.
function AddToCharset(const aCharSet, aCharsToAdd: string): string; overload;
function AddToCharset(const aCharset: string; aCharsToAdd: TStringArray): string; overload;

// Replace all kind of LineEnding by a simple #10.
function AdjustLineEnding(const aText: string): string;

function BGRAPixelToHex(aColor: TBGRAPixel): string;
function HexToBGRAPixel(const s: string): TBGRAPixel;
function PercentColor(const aColor : TBGRAPixel ;aPercent:single) :TBGRAPixel;

// returns a time formated string HH:MM:SS.xx
// if aShowFractionalPart is set to FALSE the .xx part is removed
function SecondsToString(aTime: single; aShowFractionalPart: boolean): string;

function NextPowerOfTwo(aValue: integer): integer;

// return a TBGRABitmap from an SVG file.
// aWidth and aHeight = -1 => return an image with original size as described in svg.
// aWidth and aHeight <> -1 => return a maximized image that fit in the given rectangle (keep aspect ratio).
// Only aImageWidth = '-1' => returned image width is computed from aHeight to keep aspect ratio.
// Only aImageHeight = '-1' => returned image height is computed from aWidth to keep aspect ratio.
// the drawing is centered in the returned bitmap
function LoadBitmapFromSVG(const aSVGFilename: string; aWidth, aHeight: integer): TBGRABitmap;

// return a TBGRABitmap from an SVG file with a size as described in svg file, but adjusted to PPI scene.
function LoadBitmapFromSVGScaledPPI(aScene: TOGLCScene; const aSVGFilename: string): TBGRABitmap;

// splits each words of a text. #10(line feed) is considered as a line break
function SplitTextToWordArray(aText: string; aCharSeparator: char=' '): TStringArray;

// draw a box (without filling) using the current scene MVP matrix.
procedure DrawBox(const aScene: TOGLCScene; const aX, aY, aWidth, aHeight: single; const aColor: TBGRAPixel; w: single=1.0; aOpacity: single=1.0); overload;
procedure DrawBox(const aScene: TOGLCScene; const aRect: TRect; const aColor: TBGRAPixel; w: single=1.0; aOpacity: single=1.0); overload;
procedure FillBox(const aScene: TOGLCScene; aX, aY, aWidth, aHeight: single; const aFillColor: TBGRAPixel; aOpacity: single=1.0); overload;
procedure FillBox(const aScene: TOGLCScene; const aRect: TRect; const aFillColor: TBGRAPixel; aOpacity: single=1.0); overload;
procedure DrawLine(const aScene: TOGLCScene; x1, y1, x2, y2: single; aColor: TBGRAPixel; w: single=1.0); overload;
procedure DrawLine(const aScene: TOGLCScene; pt1, pt2: TPointF; aColor: TBGRAPixel; w: single=1.0); overload;
procedure DrawEllipse(const aScene: TOGLCScene; aX, aY, aRx, aRy: single; aColor: TBGRAPixel; w: single=1.0; aOpacity: single=1.0);

// Compute the scale value to maximize the passed texture on the scene. Aspect ration is keeped.
function ScaleValueToFitScene(aTexture: PTexture; AScene: TOGLCScene; aSideMargin: integer=0): TPointF;
// Compute the scale value to maximize the passed texture on the surface. Aspect ration is keeped.
function ScaleValueToFitSurface(aTexture: PTexture; aTargetSurface: TSimpleSurfaceWithEffect): TPointF;
// Compute the scale value to maximize the passed texture on a rectangle. Aspect ration is keeped.
function ScaleValueToFitRect(aRect: TRectF; aTargetSurface: TSimpleSurfaceWithEffect): TPointF;

// This is an helper to distribute regularly child surfaces on their parent vertically or horizontally.
//     aArea: the length of the parent area where the childs will be positionned
//     aItemSize: the size of a child
//     aItemCount: the child count
//     aMargin: the computed margin between childs
//     aDelta: the computed delta to add to separate childs
procedure Distribute(aArea, aItemSize, aItemCount: integer; out aMargin, aDelta: single);

function Distance(aPt1, aPt2: TPointF): single;

function PointFToRectF( p1, p2: TPointF ): TRectF;

// COLLISION TEST
function CollisionPointCircle(const aPt, aCenter: TPointF; const aRadius: single): boolean;
function CollisionPointRectF(const aPt: TPointF; const r: TRectF): boolean;
function CollisionCircleCircle(const aCenter1: TPointF; const aRadius1: single;
                               const aCenter2: TPointF; const aRadius2: single): boolean;
function CollisionCircleRectF(const aCenter: TPointF; const aRadius: single; const r: TRectF): boolean;
function CollisionRectFRectF(const r1, r2: TRectF): boolean;
function CollisionLinePoint(aLinePt1, aLinePt2, aPt: TPointF): boolean;
function CollisionLineLine(aLine1Pt1, aLine1Pt2, aLine2Pt1, aLine2Pt2: TPointF): boolean;
function CollisionLineCircle(aLinePt1, aLinePt2, aCircleCenter: TPointF; aRadius: single): boolean;
function CollisionLineRectF(aLinePt1, aLinePt2: TPointF; aRect: TRectF): boolean;

// return the appropriate GL_COLOR_ATTACHMENTx
// ie: aIndex=0  returns GL_COLOR_ATTACHMENT0
//     aIndex=8  returns GL_COLOR_ATTACHMENT8
// aIndex [0..15]
function IndexToColorAttachment( aIndex: integer ): GLenum;
// return the total color attachment available on your system
// the max value will be the returned value minus 1.
function ColorAttachmentCount: GLint;

const
  deg2rad: single = 0.017453292;
  rad2deg: single = 57.29578049;
  OneDiv255: single = 1/255;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

function GLErrorToString(aError: GLenum): string;
begin
  case aError of
    GL_NO_ERROR: Result := 'no error';
    GL_INVALID_ENUM: Result := 'invalid enum';
    GL_INVALID_VALUE: Result := 'invalid value';
    GL_INVALID_OPERATION: Result := 'invalid operation';
    GL_STACK_OVERFLOW: Result := 'stack overflow';
    GL_STACK_UNDERFLOW: Result := 'stack underflow';
    GL_OUT_OF_MEMORY: Result := 'out of memory';
    else Result := '???';
  end;
end;

function FormatFloatWithDot(const aFmt: string; aValue: single): string;
var i: integer;
begin
  Result := FormatFloat(aFmt, aValue);
  i := Pos(',', Result);
  if i > 0 then
    Result[i] := '.';
end;

function StringToSingle(const aStr: string): single;
var i: integer;
    err: word;
    s: string;
begin
  Result := 0;
  Val(aStr, Result, err);
  if err <> 0 then begin
    i := Pos(',', aStr);
    if i = 0 then raise exception.Create('string to single conversion error with "'+aStr+'"')
      else begin
        s := aStr;
        s[i] := '.';
        Val(s, Result, err);
      end;
  end;
end;

function AddToCharset(const aCharSet, aCharsToAdd: string): string;
var pCur, pEnd: PChar;
  len: integer;
  codePoint: string;
  i: PtrInt;
begin
  Result := Copy(aCharset, 1, Length(aCharSet));
  if aCharsToAdd = '' then exit;
  codePoint := '';
  pCur := PChar(aCharsToAdd);
  pEnd := pCur + Length(aCharsToAdd);
  while pCur < pEnd do begin
    len := UTF8CodepointSize(pCur);
    SetLength(codePoint, len);
    Move(pCur^, codePoint[1], len);
    if (codePoint = #10) or (codePoint = #13) then i := 1 // remove all lineEnding
      else i := UTF8Pos(codePoint, Result); //aCharSet);
    if i = 0 then Result := Result + codepoint;
    inc(pCur, len);
  end;
end;

function AddToCharset(const aCharset: string; aCharsToAdd: TStringArray): string;
var i: Integer;
begin
  Result := aCharset;
  for i:=0 to High(aCharsToAdd) do
    Result := AddToCharset(Result, aCharsToAdd[i]);
end;

function AdjustLineEnding(const aText: string): string;
begin
  Result := StringReplace(aText, #13#10, #10, [rfReplaceAll{, rfIgnoreCase}]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll{, rfIgnoreCase}]);
end;

function BGRAPixelToHex(aColor: TBGRAPixel): string;
begin
 Result := inttoHex(aColor.red,2)+inttoHex(aColor.green,2)+inttoHex(aColor.blue,2)+inttoHex(aColor.alpha,2);
end;

function HexToBGRAPixel(const s: string): TBGRAPixel;
var ss: string;
begin
 ss := copy( s, 1, 2);
 Result.red:=Hex2Dec(ss);
 ss := copy( s, 3, 2);
 Result.green:=Hex2Dec(ss);
 ss := copy( s, 5, 2);
 Result.blue:=Hex2Dec(ss);
 ss := copy( s, 7, 2);
 Result.alpha:=Hex2Dec(ss);
end;

function PercentColor ( const aColor : TBGRAPixel ;aPercent:single ) :TBGRAPixel;
var b,g,r:integer ;
begin
 b := round(aColor.blue*aPercent); if b>255 then b:=255 else if b<0 then b:=0;
 g := round(aColor.green*aPercent); if g>255 then g:=255 else if g<0 then g:=0;
 r := round(aColor.red*aPercent); if r>255 then r:=255 else if r<0 then r:=0;
 Result.blue:=b;
 Result.green:=g;
 Result.red:=r;
 Result.alpha:=aColor.alpha;
end;

function SecondsToString(aTime: single; aShowFractionalPart: boolean): string;
var t,h,m,s: integer;
begin
 t := trunc(aTime);
 h := t div 3600;
 t:=t-h*3600;
 m := t div 60;
 s:=t-m*60;
 t := trunc(Frac(aTime)*100);
 Result:='';
 if h>0 then Result:=inttostr(h)+':';

 if m>0 then
  if m>10
   then Result+=inttostr(m)+':'
   else Result+='0'+inttostr(m)+':';

 if s<10 then Result+='0';
 Result+=inttostr(s);

 if aShowFractionalPart
   then if t<10
     then Result+='.0'+inttostr(t)
     else Result+='.'+inttostr(t);
end;

function NextPowerOfTwo(aValue: integer): integer;
begin
  Result := 1;
  while Result < aValue do Result := Result shl 1;
end;

function LoadBitmapFromSVG(const aSVGFilename: string; aWidth, aHeight: integer): TBGRABitmap;
var svg: TBGRASVG;
  FWHFactor, cw, ch: single;
begin
  Result := NIL;
  svg := TBGRASVG.Create( aSVGFileName );
  try
    cw := svg.WidthAsPixel;
    ch := svg.HeightAsPixel;
    FWHFactor := cw/ch;

    if (aWidth > -1) and (aHeight > -1) then begin
      Result := TBGRABitmap.Create(aWidth, aHeight, BGRAPixelTransparent);
      svg.StretchDraw(Result.Canvas2D, 0, 0, Result.Width, Result.Height, True);
      exit;
    end;

    if (aWidth = -1) and (aHeight = -1) then begin
      aWidth := Round(cw);
      aHeight := Round(ch);
    end
    else
    if aWidth = -1
      then aWidth := round(aHeight*FWHFactor)
    else
    if aHeight = -1
      then aHeight := round(aWidth/FWHFactor);

    Result := TBGRABitmap.Create(aWidth, aHeight, BGRAPixelTransparent);
    svg.StretchDraw(Result.Canvas2D, taCenter, tlCenter, 0, 0, Result.Width, Result.Height);
  finally
    svg.Free;
  end;
end;

function LoadBitmapFromSVGScaledPPI(aScene: TOGLCScene; const aSVGFilename: string): TBGRABitmap;
var svg: TBGRASVG;
  w, h: Integer;
begin
 svg := TBGRASVG.Create(aSVGFilename);
 try
   w := aScene.ScaleDesignToScene(Round(svg.WidthAsPixel));
   h := aScene.ScaleDesignToScene(Round(svg.HeightAsPixel));
   Result := TBGRABitmap.Create(w, h, BGRAPixelTransparent);
   svg.StretchDraw(Result.Canvas2D, taCenter, tlCenter, 0, 0, w, h);
 finally
   svg.Free;
 end;
end;

function SplitTextToWordArray(aText: string; aCharSeparator: char): TStringArray;
var i: integer;
begin
  Result := NIL;
  if Length(aText) = 0 then exit;

  // replace all possible LineEnding combinations by #10
  aText := StringReplace(aText, #13#10, #10, [rfReplaceAll{, rfIgnoreCase}]);
  aText := StringReplace(aText, #13, #10, [rfReplaceAll{, rfIgnoreCase}]);

  // add a aCharSeparator before and after each #10
  for i:=Length(aText) downTo 1 do
    if (aText[i] = #10) and (i > 1) then begin
      Insert(aCharSeparator, aText, i+1);
      Insert(aCharSeparator, aText, i);
    end;

  Result := aText.Split([aCharSeparator]);

  // delete all empty characters
  for i:=High(Result) downto 0 do
    if Length(Result[i]) = 0 then Delete(Result, i , 1);
end;

procedure DrawBox(const aScene: TOGLCScene; const aX, aY, aWidth, aHeight: single; const aColor: TBGRAPixel; w: single; aOpacity: single);
var path: TOGLCPath;
begin
  path := NIL;
  path.CreateRect(aX, aY, aWidth, aHeight, True);
  aScene.FastLineRenderer.Prepare(aScene.MVPMatrix, FX_BLEND_NORMAL);
  aScene.FastLineRenderer.PushPath(path, aColor, w, lpMiddle);
end;

procedure DrawBox(const aScene: TOGLCScene; const aRect: TRect; const aColor: TBGRAPixel; w: single; aOpacity: single);
begin
  DrawBox(aScene, aRect.Left, aRect.Top, aRect.Width, aRect.Height, aColor, w, aOpacity);
end;

procedure FillBox(const aScene: TOGLCScene; aX, aY, aWidth, aHeight: single; const aFillColor: TBGRAPixel; aOpacity: single);
var computedTint: TColorF;
begin
  computedTint.SetAsTransparent;
  aScene.ThreeColorTriangleRenderer.Prepare(ptTriangleStrip, aScene.MVPMatrix, aOpacity, computedTint, 0);
  aScene.ThreeColorTriangleRenderer.PushRectangle(aX, aY, aWidth, aHeight, aFillColor);
end;

procedure FillBox(const aScene: TOGLCScene; const aRect: TRect; const aFillColor: TBGRAPixel; aOpacity: single);
begin
  FillBox(aScene, aRect.Left, aRect.Top, aRect.Width, aRect.Height, aFillColor, aOpacity);
end;

procedure DrawLine(const aScene: TOGLCScene; x1, y1, x2, y2: single; aColor: TBGRAPixel; w: single);
begin
  aScene.FastLineRenderer.Prepare(aScene.MVPMatrix, FX_BLEND_NORMAL);
  aScene.FastLineRenderer.PushLine(PointF(x1, y1), PointF(x2, y2), aColor, w, lpMiddle);
end;

procedure DrawLine(const aScene: TOGLCScene; pt1, pt2: TPointF; aColor: TBGRAPixel; w: single);
begin
  aScene.FastLineRenderer.Prepare(aScene.MVPMatrix, FX_BLEND_NORMAL);
  aScene.FastLineRenderer.PushLine(pt1, pt2, aColor, w, lpMiddle);
end;

procedure DrawEllipse(const aScene: TOGLCScene; aX, aY, aRx, aRy: single; aColor: TBGRAPixel;
  w: single; aOpacity: single);
var path: TOGLCPath;
begin
  path := NIL;
  path.CreateEllipse(aX, aY, aRx, aRy, True);
  //path.ClosePath;
  aScene.SmoothLineRenderer.PushPath(path, lpInside, aColor, w, aOpacity, True);
end;

procedure Distribute(aArea, aItemSize, aItemCount: integer; out aMargin, aDelta: single);
var delta: Integer;
begin
  delta := aArea - aItemSize * aItemCount;
  if delta < 0 then begin
    aMargin := 0;
    aDelta := aItemSize;
  end else begin
    aMargin := delta/(aItemCount+1);
    aDelta := aItemSize + aMargin;
  end;
end;

function Distance(aPt1, aPt2: TPointF): single;
begin
 Result := sqrt(sqr(aPt2.x-aPt1.x)+sqr(aPt2.y-aPt1.y));
end;

function PointFToRectF(p1, p2: TPointF): TRectF;
var topleft, bottomright: TPointF;
begin
 if p1.x<p2.x then topleft.x:=p1.x else topleft.x:=p2.x;
 if p1.y<p2.y then topleft.y:=p1.y else topleft.y:=p2.y;

 if p1.x<p2.x then bottomright.x:=p2.x else bottomright.x:=p1.x;
 if p1.y<p2.y then bottomright.y:=p2.y else bottomright.y:=p1.y;

 Result.TopLeft:=topleft;
 Result.BottomRight:=bottomright;
end;

function CollisionPointCircle(const aPt, aCenter: TPointF; const aRadius: single): boolean;
begin
  Result := Distance(aPt, aCenter) < aRadius;
end;

function CollisionPointRectF(const aPt: TPointF; const r: TRectF): boolean;
begin
  Result := (aPt.x >= r.Left) and (aPt.x < r.Right) and
            (aPt.y >= r.Top) and (aPt.y < r.Bottom);
end;

function CollisionCircleCircle(const aCenter1: TPointF;
  const aRadius1: single; const aCenter2: TPointF; const aRadius2: single): boolean;
begin
  Result := Distance(aCenter1, aCenter2) < aRadius1 + aRadius2;
end;

function CollisionCircleRectF(const aCenter: TPointF;
  const aRadius: single; const r: TRectF): boolean;
var closestX, closestY{, dX, dY}: single;
begin
  closestX := aCenter.x;
  if closestX <= r.Left then closestX := r.Left
    else if closestX > r.Right then closestX := r.Right;

  closestY := aCenter.y;
  if closestY < r.Top then closestY := r.Top
    else if closestY > r.Bottom then closestY := r.Bottom;

  Result := Distance(PointF(closestX, closestY), aCenter) < aRadius;

{  dX := aCenter.x - closestX;
  dY := aCenter.y - closestY;

  Result := sqr(dX) + sqr(dY) < sqr(aRadius);  }
end;

function CollisionRectFRectF(const r1, r2: TRectF): boolean;
begin
  Result := (r1.Right >= r2.Left) and ( r1.Left <= r2.Right) and
            (r1.Bottom >= r2.Top) and (r1.Top <= r2.Bottom);
end;

function CollisionLinePoint(aLinePt1, aLinePt2, aPt: TPointF): boolean;
var d1, d2, lineLen, buffer: single;
begin
  // get distance from the point to the two ends of the line
  d1 := Distance(aPt, aLinePt1);
  d2 := Distance(aPt, aLinePt2);
  // get the length of the line
  lineLen := Distance(aLinePt1, aLinePt2);
  // since floats are so minutely accurate, add
  // a little buffer zone that will give collision
  buffer := 0.1;    // higher # = less accurate
  // if the two distances are equal to the line's length, the point is on the line!
  // note we use the buffer here to give a range, rather than one #
  Result := (d1+d2 >= lineLen-buffer) and (d1+d2 <= lineLen+buffer);
end;

function CollisionLineLine(aLine1Pt1, aLine1Pt2, aLine2Pt1, aLine2Pt2: TPointF): boolean;
var uA, uB, x4_x3, y1_y3, y4_y3, x1_x3, x2_x1, y2_y1: single;
begin
  x4_x3 := aLine2Pt2.x - aLine2Pt1.x;
  y1_y3 := aLine1Pt1.y - aLine2Pt1.y;
  y4_y3 := aLine2Pt2.y - aLine2Pt1.y;
  x1_x3 := aLine1Pt1.x - aLine2Pt1.x;
  x2_x1 := aLine1Pt2.x - aLine1Pt1.x;
  y2_y1 := aLine1Pt2.y - aLine1Pt1.y;
  // calculate the distance to intersection point
  uA := (x4_x3 * y1_y3 - y4_y3 * x1_x3) / (y4_y3 * x2_x1 - x4_x3 * y2_y1);
  uB := (x2_x1 * y1_y3 - y2_y1 * x1_x3) / (y4_y3 * x2_x1 - x4_x3 * y2_y1);

  // if uA and uB are between 0-1, lines are colliding
  Result := (uA >= 0) and (uA <= 1) and (uB >= 0) and (uB <= 1);
end;

// from https://github.com/jeffThompson/CollisionDetection
function CollisionLineCircle(aLinePt1, aLinePt2, aCircleCenter: TPointF; aRadius: single): boolean;
var distX, distY, len, dot, closestX, closestY: single;
begin
  // is either end INSIDE the circle? if so, return true immediately
  if CollisionPointCircle(aLinePt1, aCircleCenter, aRadius) or
     CollisionPointCircle(aLinePt2, aCircleCenter, aRadius)  then exit(True);
  // get length of the line
  distX := aLinePt1.x - aLinePt2.x;
  distY := aLinePt1.y - aLinePt2.y;
  len := sqrt((distX*distX) + (distY*distY));
  // get dot product of the line and circle
  dot := (((aCircleCenter.x-aLinePt1.x)*(aLinePt2.x-aLinePt1.x)) +
         ((aCircleCenter.y-aLinePt1.y)*(aLinePt2.y-aLinePt1.y))) / (len*len);
  // find the closest point on the line
  closestX := aLinePt1.x + (dot * (aLinePt2.x-aLinePt1.x));
  closestY := aLinePt1.y + (dot * (aLinePt2.y-aLinePt1.y));
  // is this point actually on the line segment? if so keep going, but if not, return false
  if not CollisionLinePoint(aLinePt1, aLinePt2, PointF(closestX,closestY)) then exit(False);
  // get distance to closest point
  distX := closestX - aCircleCenter.x;
  distY := closestY - aCircleCenter.y;
  Result := sqrt((distX*distX) + (distY*distY)) <= aRadius;
end;

function CollisionLineRectF(aLinePt1, aLinePt2: TPointF; aRect: TRectF): boolean;
begin
  // check if the line has hit any of the rectangle's sides
  // uses the Line/Line function
  Result := CollisionLineLine(aLinePt1, aLinePt2, aRect.TopLeft, PointF(aRect.Left, aRect.Bottom)) or
            CollisionLineLine(aLinePt1, aLinePt2, PointF(aRect.Right, aRect.Top), aRect.BottomRight) or
            CollisionLineLine(aLinePt1, aLinePt2, aRect.TopLeft, PointF(aRect.Right, aRect.Top)) or
            CollisionLineLine(aLinePt1, aLinePt2, PointF(aRect.Left, aRect.Bottom), aRect.BottomRight);
end;

function IndexToColorAttachment(aIndex: integer): GLenum;
begin
 Result := GL_COLOR_ATTACHMENT0+aIndex;
end;

function ColorAttachmentCount: GLint;
var c: GLint;
begin
 glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, @c);
 Result := c;
end;

function ScaleValueToFitScene(aTexture: PTexture; AScene: TOGLCScene; aSideMargin: integer): TPointF;
var rl,rh,r: single;
begin
 rl := (AScene.Width-aSideMargin) / aTexture^.TextureWidth;
 rh := (AScene.Height-aSideMargin) / aTexture^.TextureHeight;
 if rl < rh then r := rl else r := rh;
 Result := PointF(r, r);
end;

function ScaleValueToFitSurface(aTexture: PTexture; aTargetSurface: TSimpleSurfaceWithEffect): TPointF;
var rl,rh,r: single;
begin
 rl := aTargetSurface.Width / aTexture^.TextureWidth;
 rh := aTargetSurface.Height / aTexture^.TextureHeight;
 if rl < rh then r := rl else r := rh;
 Result := PointF(r, r);
end;

function ScaleValueToFitRect(aRect: TRectF; aTargetSurface: TSimpleSurfaceWithEffect): TPointF;
var rl,rh,r: single;
begin
 rl := aTargetSurface.Width / aRect.Width;
 rh := aTargetSurface.Height / aRect.Height;
 if rl < rh then r := rl else r := rh;
 Result := PointF(r, r);
end;


{$endif oglcIMPLEMENTATION}


