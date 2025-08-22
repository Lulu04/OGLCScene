{$ifdef oglcINTERFACE}
type

TOGLCCorner = (cTL=0,   // top left
               cTR,     // top right
               cBR,     // bottom right
               cBL);    // bottom left
POGLCCorner = ^TOGLCCorner;
TQuadCornerIndexes = array[ 0..3 ] of TOGLCCorner;
PQuadCornerIndexes = ^TQuadCornerIndexes;

// quad definition for texture coordinates or other quadrilateral area
TQuadCoor = array[TOGLCCorner] of TPointF;
PQuadCoor = ^TQuadCoor;
const
NoFlipQuadUV: TQuadCoor=((x:0.0; y:1.0), (x:1.0; y:1.0), (x:1.0; y:0.0), (x:0.0; y:0.0));

// indexes to flip a texture
FLIP_INDEXES: array[ 0..3 ] of TQuadCornerIndexes = (
               (cBL, cTL, cBR, cTR), // normal
               (cBR, cTR, cBL, cTL), // flip H
               (cTL, cBL, cTR, cBR), // flip V
               (cTR, cBR, cTL, cBL));// flip HV

type

{ TQuadCoorHelper }

TQuadCoorHelper = type helper for TQuadCoor
  procedure SetAsEmpty; inline;
  procedure Create(const aTL, aTR, aBR, aBL: TPointF);
  procedure SetAsRectangle(aWidth, aHeight: single); inline;
  procedure SetAsRectangle(aX, aY, aWidth, aHeight: single); inline;
  procedure CopyFrom(const aQuad: TQuadCoor); inline;
  procedure AddOffset(const aOffsets: TQuadParamF); inline;
  procedure Translate(dx, dy: single);
  function Bounds: TRectF;
end;
function QuadCoor(const aTL, aTR, aBR, aBL: TPointF): TQuadCoor;

type
  { TStraightLineEquation }

TStraightLineEquation = record
  a, b,           // y = a*x + b
  c: single;      // for vertical straight line ( for all y, x = c)
  IsVertical: boolean;
  procedure InitFrom(const pt1, pt2: TPointF); inline;
  function IsParallelWith(const aLine: TStraightLineEquation): boolean; inline;
  function ComputeIntersectionPoint(const aLine: TStraightLineEquation): TPointF; inline;
  // returns the angle in degree between this line and another
  function ComputeAngle(const aLine: TStraightLineEquation): single;
end;

TQuadF = array[0..3] of TPointF;


//   q0                   q1    ￪        antialiasing is done here
//   |                    |    width1
//  pt1<------line------>pt2
//   |                    |    width2
//   q3                   q2    ￬        antialiasing is done here
function LineSegmentToQuadF_PosMiddle(const pt1, pt2: TPointF; const aWidth1, aWidth2: single): TQuadF;


//  pt1<------line------>pt2    ￪
//   |                    |   width1 small value to allow antialising
//   q0                   q1
//   |                    |   width2
//   q3                   q2    ￬
function LineSegmentToQuadF_PosInside(const pt1, pt2: TPointF; const aWidth1, aWidth2: single): TQuadF;

// Q1 right side is modified and Q2 left side is modified
procedure AdjustCornerOnConsecutiveQuadF(var Q1, Q2: TQuadF);

function NextPowerOfTwo(aValue: integer): integer;
// return the distance between two point
function Distance(aPt1, aPt2: TPointF): single;
// return the squared distance
function Distance2(aPt1, aPt2: TPointF): single;
function MiddleOf(aPt1, aPt2: TPointF): TPointF;
// Return the angle/s computed from the specified speed value.
function LinearSpeedToAngleRotation(aSpeed, aWheelRadius: single): single;
// Return aAngle value in range [0..360]
function KeepAngleInRange360(aAngle: single): single;
// Return the smallest value in range 0..360
// from https://stackoverflow.com/questions/1878907/how-can-i-find-the-smallest-difference-between-two-angles-around-a-point
function DeltaAngle(a1, a2: single): single;

// reproduce the GLSL smoothstep function
function SmoothStep(aEdgeMin, aEdgeMax, aValue: single): single;
// reproduce the GLSL mix function
function Mix(x, y, a: single): single;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TStraightLineEquation }

procedure TStraightLineEquation.InitFrom(const pt1, pt2: TPointF);
begin
  IsVertical := pt1.x = pt2.x;

  if not IsVertical then begin
    a := (pt2.y - pt1.y) / (pt2.x - pt1.x);  // F(x)=ax+b
    b := pt1.y - pt1.x*a;
  end else c := pt1.x; // for all y values, one have the same x
end;

function TStraightLineEquation.IsParallelWith(const aLine: TStraightLineEquation): boolean;
begin
  //Result := (IsVertical and aLine.IsVertical) or (a = aLine.a);
  Result := IsVertical and aLine.IsVertical;
  if not Result then Result := a = aLine.a;
end;

function TStraightLineEquation.ComputeIntersectionPoint(const aLine: TStraightLineEquation): TPointF;
begin
  if IsVertical and aLine.IsVertical then
    raise exception.create('both lines are vertical (parallel), intersection doesn''t exists')

  else if not IsVertical and not aLine.IsVertical then begin
    if a = aLine.a then raise exception.create('both lines are parallel, intersection doesn''t exists');
    Result.x := (aLine.b - b) / (a - aLine.a);
    Result.y := a * Result.x + b;

  end else if IsVertical then begin
    Result.x := c;
    Result.y := aLine.a * c + aLine.b;
  end else begin
    Result.x := aLine.c;
    Result.y := a * aLine.c + b;
  end;
end;

function TStraightLineEquation.ComputeAngle(const aLine: TStraightLineEquation): single;
begin
  if IsParallelWith(aLine) then Result := 0
    else Result := ArcTan((a-aLine.a)/(1+a*aLine.a))*rad2deg;
end;

function QuadCoor(const aTL, aTR, aBR, aBL: TPointF): TQuadCoor;
begin
  Result[cTL] := aTL;
  Result[cTR] := aTR;
  Result[cBR] := aBR;
  Result[cBL] := aBL;
end;

function LineSegmentToQuadF_PosMiddle(const pt1, pt2: TPointF; const aWidth1, aWidth2: single): TQuadF;
var v, vv, normal: TPointF;
begin
  v := pt2-pt1;                     //   q0                   q1    ￪     ̂̌
  v.Normalize;                      //   |                    |    width1
  vv := v * aWidth1;                //  pt1.......line.......pt2
  normal := PointF(vv.y, -vv.x);   //   |                    |    width2
  Result[0] := pt1 + normal;        //   q3                   q2    ￬
  Result[1] := pt2 + normal;
  vv := v * aWidth2;
  normal := PointF(-vv.y, vv.x);
  Result[2] := pt2 + normal;
  Result[3] := pt1 + normal;
end;

function LineSegmentToQuadF_PosInside(const pt1, pt2: TPointF; const aWidth1, aWidth2: single): TQuadF;
var v, vv, normal: TPointF;
begin
  v := pt2-pt1;
  v.Normalize;
  vv := v * aWidth1;
  normal := PointF(vv.y, -vv.x);
  Result[0] := pt1 + normal;
  Result[1] := pt2 + normal;
  vv := v * aWidth2;
  normal := PointF(vv.y, -vv.x);
  Result[2] := Result[1] + normal;
  Result[3] := Result[0] + normal;
end;

procedure AdjustCornerOnConsecutiveQuadF(var Q1, Q2: TQuadF);
var F1, F2: TStraightLineEquation;
begin
  F1.InitFrom(Q1[0], Q1[1]);
  F2.InitFrom(Q2[0], Q2[1]);
  if not F1.IsParallelWith(F2) then begin
    Q1[1] := F1.ComputeIntersectionPoint(F2);
    Q2[0] := Q1[1];
  end;

  F1.InitFrom(Q1[3], Q1[2]);
  F2.InitFrom(Q2[3], Q2[2]);
  if not F1.IsParallelWith(F2) then begin
    Q1[2] := F1.ComputeIntersectionPoint(F2);
    Q2[3] := Q1[2];
  end;
end;

function NextPowerOfTwo(aValue: integer): integer;
begin
  Result := 1;
  while Result < aValue do Result := Result shl 1;
end;

function Distance(aPt1, aPt2: TPointF): single;
begin
  Result := sqrt(sqr(aPt2.x-aPt1.x)+sqr(aPt2.y-aPt1.y));
end;

function Distance2(aPt1, aPt2: TPointF): single;
var xx, yy: single;
begin
  xx := aPt2.x - aPt1.x;
  yy := aPt2.y - aPt1.y;
  Result := xx*xx + yy*yy;
end;

function MiddleOf(aPt1, aPt2: TPointF): TPointF;
begin
  Result.x := (aPt1.x + aPt2.x) * 0.5;
  Result.y := (aPt1.y + aPt2.y) * 0.5;
end;

function LinearSpeedToAngleRotation(aSpeed, aWheelRadius: single): single;
begin
  if aWheelRadius <> 0 then
    Result := 360*aSpeed/(6.28318*aWheelRadius)   // 2*PI*R
  else
    Result :=0;
end;

function KeepAngleInRange360(aAngle: single): single;
begin
  Result := aAngle;
  while Result > 360 do Result := Result - 360;
  while Result < 0 do Result := Result + 360;
end;

function DeltaAngle(a1, a2: single): single;
begin
  Result := abs(KeepAngleInRange360(a1) - KeepAngleInRange360(a2));
  Result := (180 - Abs(Result - 180))*Sign(Result);
end;

function SmoothStep(aEdgeMin, aEdgeMax, aValue: single): single;
begin
  Result := EnsureRange((aValue - aEdgeMin) / (aEdgeMax - aEdgeMin), 0.0, 1.0);
  Result := Result * Result * (3.0 - 2.0 * Result);
end;

function Mix(x, y, a: single): single;
begin
  Result := x * (1 - a) + y * a;
end;

{ TQuadCoorHelper }

procedure TQuadCoorHelper.SetAsEmpty;
begin
  FillChar(Self, SizeOf(TPointF)*4, 0);
end;

procedure TQuadCoorHelper.Create(const aTL, aTR, aBR, aBL: TPointF);
begin
  Self[cTL] := aTL;
  Self[cTR] := aTR;
  Self[cBR] := aBR;
  Self[cBL] := aBL;
end;

procedure TQuadCoorHelper.SetAsRectangle(aWidth, aHeight: single);
begin
  FillChar(Self, SizeOf(TPointF)*4, 0);
  Self[cTL] := PointF(0, 0);
  Self[cTR].x := aWidth;
  Self[cBR].x := aWidth;
  Self[cBR].y := aHeight;
  Self[cBL].y := aHeight;
end;

procedure TQuadCoorHelper.SetAsRectangle(aX, aY, aWidth, aHeight: single);
begin
  Self[cTL].x := aX;
  Self[cTL].y := aY;
  Self[cTR].x := aX+aWidth;
  Self[cTR].y := aY;
  Self[cBR].x := aX+aWidth;
  Self[cBR].y := aY+aHeight;
  Self[cBL].x := aX;
  Self[cBL].y := aY+aHeight;
end;

procedure TQuadCoorHelper.CopyFrom(const aQuad: TQuadCoor);
begin
  Move(aQuad, Self, SizeOf(TQuadCoor));
end;

procedure TQuadCoorHelper.AddOffset(const aOffsets: TQuadParamF);
begin
  Self[cTL] := Self[cTL] + aOffsets.TopLeft.Value;
  Self[cTR] := Self[cTR] + aOffsets.TopRight.Value;
  Self[cBR] := Self[cBR] + aOffsets.BottomRight.Value;
  Self[cBL] := Self[cBL] + aOffsets.BottomLeft.Value;
end;

procedure TQuadCoorHelper.Translate(dx, dy: single);
begin
  Self[cTL].x := Self[cTL].x + dx;  Self[cTL].y := Self[cTL].y + dy;
  Self[cTR].x := Self[cTR].x + dx;  Self[cTR].y := Self[cTR].y + dy;
  Self[cBR].x := Self[cBR].x + dx;  Self[cBR].y := Self[cBR].y + dy;
  Self[cBL].x := Self[cBL].x + dx;  Self[cBL].y := Self[cBL].y + dy;
end;

function TQuadCoorHelper.Bounds: TRectF;
begin
  Result.Left := Min(Min(Min(Self[cTL].x, Self[cTR].x), Self[cBR].x), Self[cBL].x);
  Result.Right := Max(Max(Max(Self[cTL].x, Self[cTR].x), Self[cBR].x), Self[cBL].x);
  Result.Top := Min(Min(Min(Self[cTL].y, Self[cTR].y), Self[cBR].y), Self[cBL].y);
  Result.Bottom := Max(Max(Max(Self[cTL].y, Self[cTR].y), Self[cBR].y), Self[cBL].y);
end;

{$endif oglcIMPLEMENTATION}

