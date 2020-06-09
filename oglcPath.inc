{$ifdef oglcINTERFACE}
type

{ TOGLCPath }

TOGLCPath=class
private
  FPathLength: single;
  FPts: ArrayOfTPointF;
  FDistanceAndAngle: ArrayOfTPointF; // contains x=distance to the path node, y=angle to the path node
  FLooped: boolean;
  FRect: TRectF;
  procedure SetPts( aPts: ArrayOfTPointF );
public
  // return TRUE if the distance traveled value is on the path
  // 0 <= aDis < PathLength
  function IsOnThePath( aDis: single ): boolean;
  // adjust the distance traveled
  function AdjustDistanceTraveled( aDis: single ): single;
  // return the current scene position and angle(direction in degree) from a distance traveled
  procedure GetPosAndAngle( aDistanceTraveled: single; out Pos: TPointF; out Angle: single );

  // use this property to set the path nodes (in scene coordinates)
  property Pts: ArrayOfTPointF read FPts write SetPts;
  // for each node: x=distance to the next node, y=angle(direction) to the next node
  property DistanceAndAngle: ArrayOfTPointF read FDistanceAndAngle;
  // return the total path length in pixel
  property PathLength: single read FPathLength;
  property Looped: boolean read FLooped write FLooped;
  property RectArea: TRectF read FRect;
end;



{ TOGLCPathDrawing }

TOGLCPathDrawing=class(TSimpleSurfaceWithEffect)
private
  FDottedLine: ArrayOfSingle;
  FPath: TOGLCPath;
  FPathOwner: boolean;
  FLineWidth: single;
protected
 function GetWidth: integer; override;
 function GetHeight: integer; override;
 procedure ReleaseCurrentPath;
 procedure DoDraw( const pt1, pt2: TPointF; aLayerPercentOpacity: single); virtual;
 function GetPoint( aIndex: integer ): TPointF; virtual;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single );  override;
public
  DistanceTraveled: TFParam;
  LineColor: TBGRAParam;
  Constructor Create;
  Destructor Destroy; override;
  // assign a path to draw
  // if Owner is TRUE, the path is automatically destroyed when the sprite is killed
  procedure AssignPath( aPath: TOGLCPath; Owner: boolean=FALSE );
  // Use this method to define how to draw lines on the path:
  // [n1, n2, n3, n4,...]   n1,n3... are the length of the drawn part  n2,n4,... the length of holes
  // use [] for solid line (default)
  procedure SetDottedLine( const AArray: array of single );
  property LineWidth: single read FLineWidth write FLineWidth;
  property Path: TOGLCPath read FPath;
end;



{ TSpriteOnPath }
// a sprite that moves along a path
TSpriteOnPath=class(TSprite)
private
  FPath: TOGLCPath;
  FPathOwner: boolean;
public
 Constructor Create( ATexture: PTexture; Owner: boolean=FALSE );
 Destructor Destroy; override;
 procedure Update( const aElapsedTime: single ); override;
public
 // Distance traveled on the path
 DistanceTraveled: TFParam;
 // 'TRUE' force the sprite to rotate on the right direction along the path. Default is TRUE.
 AutoRotate: boolean;
 // set this value to ajust the right orientation of the sprite along the path. Default is '0'.
 AngleAdjust: single;
 // use this method to assign a path to the sprite.
 // if Owner is TRUE, the path is automatically destroyed when the sprite is killed
 procedure AssignPath( aPath: TOGLCPath; Owner: boolean=FALSE );
 property Path: TOGLCPath read FPath;
end;



{$endif oglcINTERFACE}



{$ifdef oglcIMPLEMENTATION}
{ TOGLCPath }

procedure TOGLCPath.SetPts(aPts: ArrayOfTPointF);
var i: Integer;
  p: TPolarCoor;
begin
 FRect:=RectF(100000,100000,-100000,-100000);
 FPathLength:=0;

 SetLength(FPts, Length(aPts));
 SetLength(FDistanceAndAngle, Length(aPts));
 FDistanceAndAngle[0]:=PointF(0,0);
 for i:=0 to high(aPts) do begin
   // retrieves the rectangle boundary of the path
   if FRect.Left>aPts[i].x then FRect.Left:=aPts[i].x;
   if FRect.Right<aPts[i].x then FRect.Right:=aPts[i].x;
   if FRect.Top>aPts[i].y then FRect.Top:=aPts[i].y;
   if FRect.Bottom<aPts[i].y then FRect.Bottom:=aPts[i].y;

   FPts[i]:=aPts[i];
   // computes total path length, and the distance and angle (direction) between each node
   if i>0 then begin
     p:=CartesianToPolar(aPts[i-1], aPts[i]);
     FDistanceAndAngle[i] := PointF(p.Distance, p.Angle);

     FPathLength+=p.Distance;
   end;
 end;
end;

function TOGLCPath.IsOnThePath(aDis: single): boolean;
begin
 Result:= (aDis>=0) and (aDis<FPathLength);
end;

function TOGLCPath.AdjustDistanceTraveled( aDis: single ): single;
begin
 Result:=aDis;
 if FPathLength<=0 then exit;

 if Result>FPathLength then begin
   if FLooped then begin
     repeat
      Result:=Result-FPathLength;
     until Result<FPathLength;
   end else Result:=FPathLength;
 end;
end;

procedure TOGLCPath.GetPosAndAngle(aDistanceTraveled: single; out Pos: TPointF; out Angle: single);
var i: integer;
  d, dnext: single;
  p: TPolarCoor;
begin
 aDistanceTraveled:=AdjustDistanceTraveled(aDistanceTraveled);

 if aDistanceTraveled=PathLength then begin
   Pos:=FPts[high(FPts)];
   Angle:=FDistanceAndAngle[high(FPts)].y;
   exit;
 end;

 d:=0;
 for i:=1 to high(FPts) do begin
   dnext:=d+FDistanceAndAngle[i].x;
   if (aDistanceTraveled>=d) and (aDistanceTraveled<dnext) then begin
     Angle := FDistanceAndAngle[i].y;
     p:= CartesianToPolar(FPts[i-1], FPts[i]);
     p.Distance:=aDistanceTraveled-d;
     Pos:=PolarToCartesian( FPts[i-1], p );
     exit;
   end;
   d:=dnext;
 end;
end;


{ TOGLCPathDrawing }

procedure TOGLCPathDrawing.SetDottedLine(const AArray: array of single);
var i: Integer;
  s: single;
begin
 SetLength(FDottedLine, Length(AArray));
 s:=0;
 for i:=0 to high(AArray) do begin
  FDottedLine[i]:=AArray[i];
  s+=AArray[i];
 end;
 if s=0 then SetLength(FDottedLine, 0);
end;

function TOGLCPathDrawing.GetWidth: integer;
begin
 Result:= round(FPath.RectArea.Width);
end;

function TOGLCPathDrawing.GetHeight: integer;
begin
 Result:= round(FPath.RectArea.Height);
end;

procedure TOGLCPathDrawing.ReleaseCurrentPath;
begin
 if (FPath<>NIL) and FPathOwner then FPath.Free;
 FPath:=NIL;
end;

procedure TOGLCPathDrawing.DoDraw(const pt1, pt2: TPointF; aLayerPercentOpacity: single);
begin
 DrawLine( pt1, pt2, LineColor.Value, FLineWidth, aLayerPercentOpacity);
end;

function TOGLCPathDrawing.GetPoint(aIndex: integer): TPointF;
begin
 Result := FPath.Pts[aIndex];
end;

procedure TOGLCPathDrawing.Update(const aElapsedTime: single);
begin
 inherited Update(AElapsedTime);
 if FFreeze then exit;

 DistanceTraveled.OnElapse( AElapsedTime );
 LineColor.OnElapse( AElapsedTime );
end;

procedure TOGLCPathDrawing.Draw(const aLayerPercentOpacity: single);
var i: integer;
  p1,p2: TPointF;
  d, d1: single;
  po: TPolarCoor;
  iDL, lp: integer;
  DLRemainingDistance: single;
  DLFlag, Notdotted: boolean;

  procedure NextIndex;
  begin
   repeat
    inc(iDL);
    if iDL>high(FDottedLine) then iDL:=0;
    DLRemainingDistance:=FDottedLine[iDL];
    DLFlag:=not DLFlag;
   until DLRemainingDistance<>0;
  end;

  procedure DrawDottedLine;
  var p3: TPointF;
  begin
   if DLRemainingDistance>=po.Distance then begin
     if DLFlag then DoDraw( p1, PolarToCartesian(p1, po), aLayerPercentOpacity );
     DLRemainingDistance-=po.Distance;
     if DLRemainingDistance=0 then NextIndex;
   end else begin
       p3:= PolarToCartesian(p1, PolarCoor(DLRemainingDistance, po.Angle));
       if DLFlag then DoDraw( p1, p3, aLayerPercentOpacity );
       p1:=p3;
       po.Distance:=po.Distance-DLRemainingDistance;
       NextIndex;
       DrawDottedLine;
   end;
  end;

begin
 if not FVisible then exit;
 if Opacity.Value=0 then exit;

 if FPath=NIL then exit;
 lp:=Length(FPath.Pts);
 if lp<2 then exit;
 if DistanceTraveled.Value<0 then exit;

 SetBlendMode( FBlendMode );
 PushAndPrepareMatrix;
 TextureManager.DisableTextureUsage;
 glLineWidth( FLineWidth );

 d:=0;
 p1 := GetPoint(0);
 i := 1;

 Notdotted:=Length(FDottedLine)=0;
 if not Notdotted then begin //Length(FDottedLine)>0 then begin
   iDL:=0;
   DLRemainingDistance:=FDottedLine[0];
   DLFlag:=TRUE;
 end;

 repeat
   p2 := GetPoint(i);
   d1:=d;
   d+=FPath.DistanceAndAngle[i].x;
   if DistanceTraveled.Value>=d then begin // draw whole line segment
     if Notdotted
       then DoDraw( p1, p2, aLayerPercentOpacity )
       else begin
         po:=CartesianToPolar( p1, p2 );
         DrawDottedLine;
       end;
   end else begin // draw only part of line segment
       po:=CartesianToPolar( p1, p2 );
       po.Distance:=DistanceTraveled.Value-d1;
       if Notdotted
         then DoDraw( p1, PolarToCartesian(p1, po), aLayerPercentOpacity )
         else DrawDottedLine;
   end;
   if d>DistanceTraveled.Value then break;

   p1 := p2;
   inc( i );
 until i=lp;

// DrawBox(FPath.RectArea.Left, FPath.RectArea.Top, FPath.RectArea.Width, FPath.RectArea.Height, BGRA(255,0,0),2);

 DrawChildSurfaces( aLayerPercentOpacity * Opacity.pcValue );
 PopMatrix;
end;

constructor TOGLCPathDrawing.Create;
begin
 inherited Create;
 DistanceTraveled:= TFParam.Create;
 LineColor:= TBGRAParam.Create;
 LineColor.Value := BGRAWhite;
 FLineWidth :=1.5;
end;

destructor TOGLCPathDrawing.Destroy;
begin
 DistanceTraveled.Free;
 ReleaseCurrentPath;
 LineColor.Free;
 inherited Destroy;
end;

procedure TOGLCPathDrawing.AssignPath(aPath: TOGLCPath; Owner: boolean);
begin
 ReleaseCurrentPath;

 FPathOwner:= Owner;
 FPath:=aPath;
end;


{ TSpriteOnPath }

constructor TSpriteOnPath.Create(ATexture: PTexture; Owner: boolean);
begin
 inherited Create(ATexture, Owner);
 DistanceTraveled:= TFParam.Create;
 AutoRotate:=TRUE;
end;

destructor TSpriteOnPath.Destroy;
begin
 DistanceTraveled.Free;
 if (FPath<>NIL) and FPathOwner then FPath.Free;
 inherited Destroy;
end;

procedure TSpriteOnPath.Update(const aElapsedTime: single);
var p: TPointF;
  a: single;
begin
 inherited Update(aElapsedTime);
 if Freeze then exit;

 DistanceTraveled.OnElapse( aElapsedTime );
 if FPath<>NIL then begin
   FPath.GetPosAndAngle( DistanceTraveled.Value, p, a);
   MoveCenterTo( p, 0.2 );
   if AutoRotate then Angle.ChangeTo( a+AngleAdjust, 0.2);
 end;
end;

procedure TSpriteOnPath.AssignPath(aPath: TOGLCPath; Owner: boolean);
begin
 if (FPath<>NIL) and FPathOwner then FPath.Free;

 FPathOwner:= Owner;
 FPath:=aPath;
end;


{$endif oglcIMPLEMENTATION}

