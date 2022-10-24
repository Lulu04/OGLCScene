{$ifdef oglcINTERFACE}
type
{ TDrawingPoint }

TPointDrawingMode = (pdmAll, pdmOnlyPositionned, pdmNone );

TDrawingPoint = class(TSprite)
  IsJump: boolean; // set to True if the point is a jump
  OriginalPt: TPointF;
  IsGoodPositionned: boolean;
  procedure MoveSpriteToOriginalPosition;
end;
ArrayOfDrawingPoint = array of TDrawingPoint;
ArrayofParticleEmitter = array of TParticleEmitter;

{ TPathToDraw }
TPathToDraw = class( TSimpleSurfaceWithEffect )
  Constructor Create( aSquareSize: integer );
  Destructor Destroy; override;
protected
 function GetWidth: integer; override;
 function GetHeight: integer; override;
private
  FSquareSize: integer;
  FPtArray: ArrayOfDrawingPoint;
  FPointTexture: PTexture;
  FLineWidth: single;
  FDrawingName: string;
  function GetCount: integer;
private
  FPEArray: ArrayofParticleEmitter;
  FPointVisible: boolean;
  function GetPoint(Index: integer): TDrawingPoint;
  procedure SetPointVisible(AValue: boolean);
  function ScalePoint( aPoint: TPointF ): TPointF;
private
  FShakerEnable: boolean;
  FShakerAmplitude: integer;     // [0..100]
  procedure SetShakerAmplitude(AValue: integer);
public
  procedure Draw(const aLayerPercentOpacity: single); override;
  procedure Update( const AElapsedTime: single ); override;
public
  procedure ChangePointOpacity( NewValue: byte; aTimeSec: single; aVelocityCurve: word=idcLinear );
  procedure LoadFromFile( const aFilename: string );
  procedure AddPointToPath( aP: TPointF; aIsJump: boolean=FALSE );
  procedure Clear;
  procedure RandomizePointOnScene( aTimeSec: single; aVelocityCurve: word=idcLinear );
  function AllPointsInPlace: boolean;
  property Point[Index:integer]: TDrawingPoint read GetPoint;
  property Count: integer read GetCount;
public
  ParticleOpacity: TFParam;
  ParticleCountFactor: single;
  procedure CreateParticleEmitters( const aParticleFilename:string );
public
  Mode: TPointDrawingMode;
  LineColor: TBGRAParam;
  procedure SetTextureForPoint( aTexture: PTexture );
  property LineWidth: single read FLineWidth write FLineWidth;

  property PointVisible: boolean read FPointVisible write SetPointVisible;

  property EnableShaker: boolean read FShakerEnable write FShakerEnable;
  property ShakerAmplitude: integer read FShakerAmplitude write SetShakerAmplitude; // [0..100]
  // give the name of the drawing from the previous loaded filename
  property DrawingName: string read FDrawingName;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

procedure TDrawingPoint.MoveSpriteToOriginalPosition;
begin
 MoveCenterTo( OriginalPt, 1.4 );
 Angle.ChangeTo(0, 1.4 );
 Tint.ChangeTo(BGRA(64,200,120,255), 1.4);
 Blink(-1, 7.66, 0.02 );
 IsGoodPositionned := TRUE;
end;

constructor TPathToDraw.Create(aSquareSize: integer);
begin
 inherited Create;
 FSquareSize := aSquareSize;
 LineColor:= TBGRAParam.Create;
 LineColor.Value := BGRAWhite;
 FLineWidth :=1.5;
 Mode := pdmAll;
 FPointVisible:=TRUE;
 ParticleOpacity:= TFParam.Create;
 ParticleOpacity.Value:=255;
 ParticleCountFactor := 0.9;
end;

destructor TPathToDraw.Destroy;
begin
 LineColor.Free;
 ParticleOpacity.Free;
 inherited Destroy;
end;

function TPathToDraw.GetWidth: integer;
begin
 Result := FSquareSize;
end;

function TPathToDraw.GetHeight: integer;
begin
 Result := FSquareSize;
end;

function TPathToDraw.GetCount: integer;
begin
 Result := Length( FPtArray );
end;

function TPathToDraw.GetPoint(Index: integer): TDrawingPoint;
begin
 Result := FPtArray[Index];
end;

procedure TPathToDraw.SetPointVisible(AValue: boolean);
var i: integer;
begin
  if FPointVisible=AValue then Exit;
  FPointVisible:=AValue;
  for i:=0 to GetCount-1 do
   GetPoint(i).Visible:=AValue;
end;

function TPathToDraw.ScalePoint(aPoint: TPointF): TPointF;
begin
 Result.x := aPoint.x * FSquareSize;
 Result.y := aPoint.y * FSquareSize;
end;

procedure TPathToDraw.SetShakerAmplitude(AValue: integer);
begin
  EnsureRange( AValue, 0, 100 );
  if FShakerAmplitude=AValue then Exit;
  FShakerAmplitude:=AValue;
end;

procedure TPathToDraw.Draw(const aLayerPercentOpacity: single);
var i: integer;
  p1,p2: TPointF;
  c: TBGRAPixel;
begin
 if not FVisible then exit;
 if Opacity.Value=0 then exit;

 if GetCount < 2 then exit;

 SetBlendMode( FBlendMode );
 PushAndPrepareMatrix;
 TextureManager.DisableTextureUsage;
 glLineWidth( FLineWidth );    // 3.5

 c := LineColor.Value;
 c.alpha := round( c.alpha*aLayerPercentOpacity*Opacity.pcValue );

 p1 := PointF( FPtArray[0].CenterX, FPtArray[0].CenterY );
 if FShakerEnable then p1 := p1.Add(PointF(random(FShakerAmplitude),random(FShakerAmplitude)));
 i := 1;
 repeat
   p2 := PointF( FPtArray[i].CenterX, FPtArray[i].CenterY );
   if FShakerEnable then p2 := p2.Add(PointF(random(FShakerAmplitude),random(FShakerAmplitude)));
   if not FPtArray[i].IsJump then begin
     if (Mode = pdmAll) or
        ((Mode = pdmOnlyPositionned) and FPtArray[i].IsGoodPositionned)
       then DrawLine( p1, p2, c, FLineWidth, aLayerPercentOpacity);
   end;
   p1 := p2;
   inc( i );
 until i=Count;

 DrawChildSurfaces( aLayerPercentOpacity * Opacity.pcValue );
 PopMatrix;
end;

procedure TPathToDraw.Update(const AElapsedTime: single);
var v: integer;
begin
 inherited Update(AElapsedTime);
 if FFreeze then exit;

 LineColor.OnElapse( AElapsedTime );

 v := round(ParticleOpacity.Value);
 ParticleOpacity.OnElapse( AElapsedTime );
 if v<>round(ParticleOpacity.Value) then
   for v:=0 to Length(FPEArray)-1 do
    FPEArray[v].Opacity.Value:=ParticleOpacity.Value;
end;

function TPathToDraw.AllPointsInPlace: boolean;
var i: integer;
begin
 Result := TRUE;
 for i:=0 to Count-1 do
  Result := Result and Point[i].IsGoodPositionned and
     (Point[i].X.State=psNO_CHANGE) and
     (Point[i].Y.State=psNO_CHANGE)
end;

procedure TPathToDraw.SetTextureForPoint(aTexture: PTexture);
begin
 FPointTexture := aTexture;
end;

procedure TPathToDraw.ChangePointOpacity(NewValue: byte; aTimeSec: single;
  aVelocityCurve: word);
var i: integer;
begin
 for i:=0 to GetCount-1 do
  GetPoint(i).Opacity.ChangeTo( NewValue, aTimeSec, aVelocityCurve );
end;

procedure TPathToDraw.LoadFromFile(const aFilename: string);
var s: TStringList;
  SplittedTxt: ArrayOfString;
  c, k: integer;
begin
 try
   s := TStringList.Create;
   s.LoadFromFile( aFilename );

   k := s.IndexOf('[DRAWING_PATH]');
   if k=-1
     then Exception.Create('File '+ExtractFileName(aFilename)+' bad format...'+LINEENDING+
                           'This is not a drawing file')
     else begin
      FDrawingName := s.Strings[k+1];
      k := s.IndexOf('PointCount');
      if k=-1
        then Exception.Create('File '+ExtractFileName(aFilename)+' Point count not found...')
        else c := strtoint( s.Strings[k+1] );

      k := s.IndexOf('ListOfPointF');
      if k=-1
        then Exception.Create('File '+ExtractFileName(aFilename)+' List of points not found...')
        else begin
          Clear;
          repeat
            inc(k);
            SplittedTxt := SplitLineToStringArray( s.Strings[k], ' ' );

            if Length( SplittedTxt )=2
              then AddPointToPath( ScalePoint( PointF(StringToSingle(SplittedTxt[0]), StringToSingle(SplittedTxt[1]))), FALSE )
              else if Length( SplittedTxt )=3
                then AddPointToPath( ScalePoint( PointF(StringToSingle(SplittedTxt[1]), StringToSingle(SplittedTxt[2]))), TRUE )
                else begin
                 Exception.Create('File '+ExtractFileName(aFilename)+LINEENDING+' Bad point in list...');
                 c:=0;
                end;
            dec(c);
          until c=0;
        end;
     end;
 finally
   s.Free;
 end;
end;

procedure TPathToDraw.Clear;
begin
 DeleteAllChilds; // delete all points and particles emitters

 SetLength( FPtArray, 0 );
 SetLength(FPEArray, 0 );
end;

procedure TPathToDraw.AddPointToPath(aP: TPointF; aIsJump: boolean);
var i: integer;
begin
 i := Length(FPtArray);
 SetLength( FPtArray, i+1 );

 FPtArray[i] := TDrawingPoint.Create( FPointTexture );
 FPtArray[i].OriginalPt := aP;
 FPtArray[i].IsJump := aIsJump;
 FPtArray[i].SetCenterCoordinate( aP.x, aP.y );

 AddChild( FPtArray[i] );
end;

procedure TPathToDraw.RandomizePointOnScene(aTimeSec: single; aVelocityCurve: word);
var i,mini: integer;
  ang, siz, xx, yy: single;
begin
 mini := (Min( FParentScene.Width, FParentScene.Height )-30) div 2;
 xx := FParentScene.Center.x;
 yy := FParentScene.Center.y;
 for i:=0 to Count-1 do begin
  ang := random(360)*deg2rad;
  siz := random(round(mini*1000))*0.001;
  FPtArray[i].MoveCenterTo( PointF( xx+cos(ang)*siz, yy+sin(ang)*siz ), aTimeSec, aVelocityCurve );
 end;

 LineColor.Alpha.ChangeTo( 0, aTimeSec );
end;

procedure TPathToDraw.CreateParticleEmitters(const aParticleFilename:string);
var p1, p2: TPointF;
  i,ip: integer;
  d: single;
begin
 if Count < 2 then exit;

 SetLength( FPEArray, 0 );

 p1 := FPtArray[0].OriginalPt;
 i := 1;
 repeat
   if not FPtArray[i].IsJump then begin
    p2 := FPtArray[i].OriginalPt;
    d := sqrt( (p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y) );

    ip := Length( FPEArray );
    SetLength( FPEArray, ip+1 );
    FPEArray[ip] := TParticleEmitter.Create;
   AddChild( FPEArray[ip] );
    FPEArray[ip].LoadFromFile( aParticleFilename );
    FPEArray[ip].SetCoordinate( p1 );
    FPEArray[ip].SetEmitterTypeLine( p2 );
    FPEArray[ip].ParticlesToEmit.Value := d * ParticleCountFactor;
    FPEArray[ip].Opacity.Value:=ParticleOpacity.Value;
   end;
   p1 := FPtArray[i].OriginalPt;
   inc(i);
 until i=Count;

end;

{$endif oglcIMPLEMENTATION}

