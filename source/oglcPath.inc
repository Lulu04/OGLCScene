{$ifdef oglcINTERFACE}
type

TOGLCPath = ArrayOfTPointF;
TArrayOfOGLCPath = array of TOGLCPath;

{ TOGLCPathHelper }

TOGLCPathHelper = type helper for TOGLCPath
  procedure CreateLine(pt1, pt2: TPointF); overload;
  procedure CreateLine(x1, y1, x2, y2: single); overload;
  procedure CreateTriangle(pt1, pt2, pt3: TPointF; aClosePath: boolean);
  procedure CreateRect(aX, aY, aWidth, aHeight: single; aClosePath: boolean);
  procedure CreateRoundRect(aX, aY, aWidth, aHeight, aRx, aRy: single;
                            aOptions: TRoundRectangleOptions; aClosePath: boolean);
  procedure CreateEllipse(aX, aY, aRx, aRy: single; aClosePath: boolean);
  procedure CreatePolygon(const aPts: ArrayOfTPointF; aClosePath: boolean);
  procedure ConcatPoints(const aPts: ArrayOfTPointF);
  procedure RemoveIdenticalConsecutivePoint;
  procedure ClosePath;
  function IsClosed: boolean;
  function Bounds: TRectF;
  procedure Translate(aVector: TPointF);
  procedure Reduce(aOffset: single);

  procedure ForceTopLeftToOrigin;
  procedure Scale(aValue: single);
  procedure FlipHorizontally;
  procedure FlipVertically;
end;




{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCPathHelper }

procedure TOGLCPathHelper.CreateLine(pt1, pt2: TPointF);
begin
  Self := NIL;
  SetLength(Self, 2);
  Self[0] := pt1;
  Self[1] := pt2;
end;

procedure TOGLCPathHelper.CreateLine(x1, y1, x2, y2: single);
begin
  Self := NIL;
  SetLength(Self, 2);
  Self[0].x := x1;
  Self[0].y := y1;
  Self[1].x := x2;
  Self[1].y := y2;
end;

procedure TOGLCPathHelper.CreateTriangle(pt1, pt2, pt3: TPointF; aClosePath: boolean);
begin
  Self := NIL;
  SetLength(Self, 3);
  Self[0] := pt1;
  Self[1] := pt2;
  Self[2] := pt3;
  if aClosePath then Self.ClosePath;
end;

procedure TOGLCPathHelper.CreateRect(aX, aY, aWidth, aHeight: single; aClosePath: boolean);
begin
  Self := NIL;
  SetLength(Self, 4);
  Self[0] := PointF(aX+aWidth-1, aY);
  Self[1] := PointF(aX, aY);
  Self[2] := PointF(aX, aY+aHeight-1);
  Self[3] := PointF(aX+aWidth-1, aY+aHeight-1);
  if aClosePath then Self.ClosePath;
end;

procedure TOGLCPathHelper.CreateRoundRect(aX, aY, aWidth, aHeight, aRx, aRy: single;
  aOptions: TRoundRectangleOptions; aClosePath: boolean);
begin
  Self := ComputeRoundRect(aX, aY, aX+aWidth-1, aY+aHeight-1, aRx, aRy, aOptions);
  Self.RemoveIdenticalConsecutivePoint;
  if aClosePath then Self.ClosePath;
end;

procedure TOGLCPathHelper.CreateEllipse(aX, aY, aRx, aRy: single; aClosePath: boolean);
begin
  Self := ComputeEllipse(aX, aY, aRx, aRy);
  Self.RemoveIdenticalConsecutivePoint;
  if aClosePath then Self.ClosePath;
end;

procedure TOGLCPathHelper.CreatePolygon(const aPts: ArrayOfTPointF; aClosePath: boolean);
begin
  Self := Copy(aPts);
  if aClosePath then Self.ClosePath;
end;

procedure TOGLCPathHelper.ConcatPoints(const aPts: ArrayOfTPointF);
var index, i: integer;
begin
  if Length(aPts) = 0 then exit;
  if Length(Self) = 0 then begin
    Self := Copy(aPts, 0, Length(aPts));
    exit;
  end;
  index := Length(Self);
  SetLength(Self, Length(Self) + Length(aPts));

  for i:=0 to High(aPts) do
    Self[index+i] := aPts[i];
end;

procedure TOGLCPathHelper.RemoveIdenticalConsecutivePoint;
var i, j: integer;
begin
  if Length(Self) = 0 then exit;
  j := 0;
  for i:=High(Self) downto 0 do
  begin
    if Self[i] = Self[j] then begin
      Delete(Self, i, 1);
      dec(j);
    end;

    dec(j);
    if j < 0 then j := j+High(Self);
  end;
end;

procedure TOGLCPathHelper.ClosePath;
begin
  if Length(Self) = 0 then exit;
  if Self[0] <> Self[High(Self)] then begin
    SetLength(Self, Length(Self)+1);
    Self[High(Self)] := Self[0];
  end;
end;

function TOGLCPathHelper.IsClosed: boolean;
begin
  if Length(Self) < 2 then exit(False);
  Result := Self[0] = Self[High(Self)];
end;

function TOGLCPathHelper.Bounds: TRectF;
var i: integer;
begin
  FillChar(Result{%H-}, SizeOf(TRectF), $00);
  for i:=0 to High(Self) do
    with Result, Self[i] do begin
    if Left > x then Left := x
      else if Right < x then Right := x;
    if Top > y then Top := y
      else if Bottom < y then Bottom := y;
  end;
end;

procedure TOGLCPathHelper.Translate(aVector: TPointF);
var i: Integer;
begin
  if (aVector.x = 0) and (aVector.y = 0) then exit;
  for i:=0 to High(Self) do
    Self[i] := Self[i] + aVector;
end;

procedure TOGLCPathHelper.Reduce(aOffset: single);
var r: TRectF;
  xCenter, yCenter: single;
  i: Integer;
begin
  if aOffset = 0 then exit;
  r := Bounds;
  xCenter := (r.Right-r.Left)*0.5;
  yCenter := (r.Bottom-r.Top)*0.5;
  for i:=0 to High(Self) do
    with Self[i] do begin
    if x > xCenter then x := x + aOffset
      else if x < xCenter then x := x - aOffset;
    if y > yCenter then y := y + aOffset
      else if y < yCenter then y := y - aOffset;
  end;
end;

procedure TOGLCPathHelper.ForceTopLeftToOrigin;
var r: TRectF;
begin
  r := Bounds;
  if (r.Top <> 0) or (r.Left <> 0) then
    Translate(PointF(-r.Left, -r.Top));
end;

procedure TOGLCPathHelper.Scale(aValue: single);
var i: integer;
begin
  for i:=0 to High(Self) do
    Self[i] := PointF(Self[i].x*aValue, Self[i].y*aValue);
end;

procedure TOGLCPathHelper.FlipHorizontally;
var i: integer;
  r: TRectF;
  pivot, delta: single;
begin
  r := Bounds;
  pivot := r.Width*0.5;
  for i:=0 to High(Self) do begin
    delta := pivot - Self[i].x;
    Self[i].x := pivot + delta;
  end;
end;

procedure TOGLCPathHelper.FlipVertically;
var i: integer;
  r: TRectF;
  pivot, delta: single;
begin
  r := Bounds;
  pivot := r.Height*0.5;
  for i:=0 to High(Self) do begin
    delta := pivot - Self[i].y;
    Self[i].y := pivot + delta;
  end;
end;

{$endif oglcIMPLEMENTATION}

