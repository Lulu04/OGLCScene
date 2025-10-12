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

  procedure ExpandNormalizedTo(aWidth, aHeight: integer);
  procedure ConvertToSpline(aSplineStyle: TSplineStyle);

  procedure LoadFromFile(const aFilename: string);
  // load a normalized path from file and expand the path to the specified size
  procedure LoadFromNormalizedFileAndExpand(const aFilename: string; aWidth, aHeight: integer);
end;


{ TPathToFollow }

TPathToFollow = record
private
  FDistanceAndAngle: ArrayOfTPointF; // contains x=distance to the next point, y=angle between current and next point
  FPathLength: single;
  FLoop: boolean;
  FDistanceTravelled: single;
  procedure CreateObjectInstance;
public
  Path: TOGLCPath;
  Speed: TFParam;
  procedure InitFrom(aPath: TOGLCPath; aLoop: boolean);
  procedure InitFromFile(const aFilename: string; aClosePath, aLoop: boolean);
  procedure InitFromNormalizedFile(const aFilename: string; aClosePath, aLoop: boolean; aWidth, aHeight: integer);
  procedure ComputeDistanceAndAngle;
  procedure Free;
  procedure Update(const aElapsedTime: single);
  // adjust the distance traveled
  function TruncDistanceTraveled(aDistanceTraveled: single): single;
  // return the position and angle(direction in degree) from the distance traveled
  procedure GetPosAndAngle(out aPos: TPointF; out aAngle: single);

  property DistanceTravelled: single read FDistanceTravelled write FDistanceTravelled;
  // return the total path length
  property PathLength: single read FPathLength;
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

procedure TOGLCPathHelper.ExpandNormalizedTo(aWidth, aHeight: integer);
var i: integer;
begin
  for i:=0 to High(Self) do begin
    Self[i].x := Self[i].x * aWidth;
    Self[i].y := Self[i].y * aHeight;
  end;
end;

procedure TOGLCPathHelper.ConvertToSpline(aSplineStyle: TSplineStyle);
var spline: TOGLCPath;
begin
  spline := NIL;
  spline.ConcatPoints(ComputeOpenedSpline(Self, aSplineStyle));
  Self := Copy(spline, 0, Length(spline));
end;

procedure TOGLCPathHelper.LoadFromFile(const aFilename: string);
var t: TStringList;
    A: TStringArray;
    c, k, i, j: integer;
    s: string;
    prop: TProperties;
    activateSplineMode: boolean;
    splineStyle: TSplineStyle;
begin
  Self := NIL;
  t := TStringList.Create;
  try
   t.LoadFromFile(aFilename);
   c := 0;
   k := 0;
   s := '';
   activateSplineMode := False;
   prop.SplitFrom(t, '[OGLC_PATH]', '|');
   prop.IntegerValueOf('NodeCount', c, 0);
   SetLength(Self, c);
   prop.BooleanValueOf('UseSpline', activateSplineMode, False);
   prop.IntegerValueOf('SplineStyle', k, 0);
   splineStyle := TSplineStyle(k);
   prop.StringValueOf('Nodes', s, '');
   A := s.Split([' ']);
   i := 0;
   j := 0;
   while c > 0 do begin
    Self[j].x := StringToSingle(A[i]);
    Self[j].y := StringToSingle(A[i+1]);
    inc(i, 2);
    inc(j);
    dec(c);
   end;
   if activateSplineMode then
     ConvertToSpline(splineStyle);
  finally
    t.Free;
  end;
end;

procedure TOGLCPathHelper.LoadFromNormalizedFileAndExpand(const aFilename: string; aWidth, aHeight: integer);
var t: TStringList;
    A: TStringArray;
    c, k, i, j: integer;
    s: string;
    prop: TProperties;
    activateSplineMode: boolean;
    splineStyle: TSplineStyle;
begin
  Self := NIL;
  t := TStringList.Create;
  try
   t.LoadFromFile(aFilename);
   c := 0;
   k := 0;
   s := '';
   activateSplineMode := False;
   prop.SplitFrom(t, '[OGLC_PATH]', '|');
   prop.IntegerValueOf('NodeCount', c, 0);
   SetLength(Self, c);
   prop.BooleanValueOf('UseSpline', activateSplineMode, False);
   prop.IntegerValueOf('SplineStyle', k, 0);
   splineStyle := TSplineStyle(k);
   prop.StringValueOf('Nodes', s, '');
   A := s.Split([' ']);
   i := 0;
   j := 0;
   while c > 0 do begin
    Self[j].x := StringToSingle(A[i]) * aWidth;
    Self[j].y := StringToSingle(A[i+1]) * aHeight;
    inc(i, 2);
    inc(j);
    dec(c);
   end;
   if activateSplineMode then
     ConvertToSpline(splineStyle);
  finally
    t.Free;
  end;
end;

{ TPathToFollow }

procedure TPathToFollow.CreateObjectInstance;
begin
  Speed := TFParam.Create;
end;

procedure TPathToFollow.InitFrom(aPath: TOGLCPath; aLoop: boolean);
begin
  FPathLength := 0;
  Path := Copy(aPath);
  FLoop := aLoop;
  ComputeDistanceAndAngle;
  CreateObjectInstance;
end;

procedure TPathToFollow.InitFromFile(const aFilename: string; aClosePath, aLoop: boolean);
begin
  Path.LoadFromFile(aFilename);
  if aClosePath then Path.ClosePath;
  FLoop := aLoop;
  ComputeDistanceAndAngle;
  CreateObjectInstance;
end;

procedure TPathToFollow.InitFromNormalizedFile(const aFilename: string; aClosePath,
  aLoop: boolean; aWidth, aHeight: integer);
begin
  Path.LoadFromNormalizedFileAndExpand(aFilename, aWidth, aHeight);
  if aClosePath then Path.ClosePath;
  FLoop := aLoop;
  ComputeDistanceAndAngle;
  CreateObjectInstance;
end;

procedure TPathToFollow.ComputeDistanceAndAngle;
var i: integer;
  p: TPolarCoor;
begin
  FDistanceAndAngle := NIL;
  SetLength(FDistanceAndAngle, Length(Path));
  for i:=1 to high(Path) do begin
    p := CartesianToPolar(Path[i-1], Path[i]);
    FDistanceAndAngle[i] := PointF(p.Distance, p.Angle);
    FPathLength := FPathLength + p.Distance;
  end;
  FDistanceAndAngle[0] := PointF(0, FDistanceAndAngle[1].y); // first point: distance=0 angle=same as next point
end;

procedure TPathToFollow.Free;
begin
  FreeAndNil(Speed);
end;

procedure TPathToFollow.Update(const aElapsedTime: single);
begin
  Speed.OnElapse(aElapsedTime);
  FDistanceTravelled := FDistanceTravelled + Speed.Value*aElapsedTime;
  if not FLoop then
    FDistanceTravelled := EnsureRange(FDistanceTravelled, 0, FPathLength);
end;

function TPathToFollow.TruncDistanceTraveled(aDistanceTraveled: single): single;
begin
  if FPathLength <= 0 then begin
    Result := 0;
    exit;
  end;

  Result := aDistanceTraveled;
  if Result >= FPathLength then begin
    if FLoop then begin
      repeat
       Result := Result - FPathLength;
      until Result < FPathLength;
    end else Result := FPathLength;
  end else if Result < 0 then begin
    if FLoop then begin
      repeat
       Result := Result + FPathLength;
      until Result > 0;
    end else Result := 0;
  end;
end;

procedure TPathToFollow.GetPosAndAngle(out aPos: TPointF; out aAngle: single);
var i: integer;
  d, dnext, distTraveled: single;
  p: TPolarCoor;
begin
  distTraveled := TruncDistanceTraveled(FDistanceTravelled);

  if distTraveled = PathLength then begin
    aPos := Path[high(Path)];
    aAngle := FDistanceAndAngle[high(Path)].y;
    exit;
  end;

  d := 0;
  for i:=1 to high(Path) do begin
    dnext := d + FDistanceAndAngle[i].x;
    if (distTraveled >= d) and (distTraveled < dnext) then begin
      aAngle := FDistanceAndAngle[i].y;
      p := CartesianToPolar(Path[i-1], Path[i]);
      p.Distance := distTraveled-d;
      aPos := PolarToCartesian(Path[i-1], p);
      exit;
    end;
    d := dnext;
  end;
end;

{$endif oglcIMPLEMENTATION}

