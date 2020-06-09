{$ifdef oglcINTERFACE}
type

{ TMouseSatelliteSprite }
// Satellite sprite that turn like an electron in rectangular area around mouse cursor

TMouseSatelliteSprite = class( TSprite )
protected
  Accumulator: TPointF;
  procedure SetCoordinates; virtual;
public
  Pulse: TPointFParam; // sprite's speed
  MovingRectangle: TPointFParam; // width and height of the rectangle area where the sprite move
  PulseFactor: single;
  Constructor Create( ATexture: PTexture; Owner: boolean=FALSE );
  Destructor Destroy; override;
  procedure Update( const aElapsedTime: single ); override;
end;


{ TSatelliteSprite }

TSatelliteSprite = class( TMouseSatelliteSprite )
private
  FSatCoor: TPointF;
protected
  procedure SetCoordinates; override;
public
  procedure Draw( const aLayerPercentOpacity: single );  override;
end;


{ TPolarSprite }

TPolarSprite = class(TSprite)
public
  Constructor Create( ATexture: PTexture; Owner: boolean=FALSE );
  Destructor Destroy; override;
  procedure Update( const aElapsedTime: single ); override;
public
  // changing the distance and/or angle automatically moves the sprite accordingly
  Polar: TPolarSystemParam;
end;

TArrayOfTPolarSprite= array of TPolarSprite;


{ TDrawPoints }

TDrawPoints = class( TSimpleSurfaceWithEffect )
private
  FPoints: TOGLCPath;
  FPointSize: single;
  function GetPoints: ArrayOfTPointF;
  function GetRectangularArea: TRectF;
  procedure SetPoints(AValue: ArrayOfTPointF);
protected
 function GetWidth: integer; override;
 function GetHeight: integer; override;
public
 procedure Draw( const aLayerPercentOpacity: single ); override;
public
  constructor Create;
  destructor Destroy; override;
  // fill this array with the coordinates of the points you want to be drawn
  property Pts: ArrayOfTPointF read GetPoints write SetPoints;
  property RectArea: TRectF read GetRectangularArea;
  property PointSize: single read FPointSize write FPointSize;
end;



Txyuv = Packed Record
  x, y: single; //vertex coordinates
  u, v: single; //texture coordinates
end;

{ TOGLCSprite }

TOGLCSprite=class(TSprite)
private
  FShader: TOGLCPostProcessingFX;
  FVBO, FVAO: TGLint;
  procedure CreateBufferData;
public
  procedure Draw( const aLayerPercentOpacity: single );  override;
public
  Constructor Create( ATexture: PTexture; Owner: boolean=FALSE );
  Destructor Destroy; override;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}
{ TOGLCSprite }

procedure TOGLCSprite.CreateBufferData;
const
    kATTRIB_POINT = 0; //XY position on screen
    kATTRIB_UV = 1; //UV coordinates of texture
begin
  glGenBuffers(1, @FVBO);
  glBindBuffer(GL_ARRAY_BUFFER, FVBO);
  //glBufferData(GL_ARRAY_BUFFER, sizeof(sq), @sq[0], GL_DYNAMIC_DRAW); //GL_STATIC_DRAW
  glBufferData(GL_ARRAY_BUFFER, 4 * sizeof(Txyuv), nil, GL_DYNAMIC_DRAW); //GL_STATIC_DRAW
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenVertexArrays(1, @FVAO);
  glBindVertexArray(FVAO);
  glBindBuffer(GL_ARRAY_BUFFER, FVBO);
  glVertexAttribPointer(kATTRIB_POINT, 2, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(kATTRIB_POINT);
  glVertexAttribPointer(kATTRIB_UV, 2, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(sizeof(single)*2));
  glEnableVertexAttribArray(kATTRIB_UV);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
//  uniform_mtx := glGetUniformLocation(shaderProgram, pAnsiChar('ModelViewProjectionMatrix'));
end;

procedure TOGLCSprite.Draw(const aLayerPercentOpacity: single);
begin

end;

constructor TOGLCSprite.Create(ATexture: PTexture; Owner: boolean);
begin
 inherited Create( ATexture, Owner );
 FShader:= TOGLCPostProcessingFX.Create;
end;

destructor TOGLCSprite.Destroy;
begin
 glDeleteBuffers(1, @FVBO);
 glDeleteVertexArrays(1, @FVAO);
 FShader.Free;
 inherited Destroy;
end;

{ TDrawPoints }

function TDrawPoints.GetPoints: ArrayOfTPointF;
begin
 Result := FPoints.Pts;
end;

function TDrawPoints.GetRectangularArea: TRectF;
begin
 Result := FPoints.RectArea;
end;

procedure TDrawPoints.SetPoints(AValue: ArrayOfTPointF);
begin
 FPoints.Pts := AValue;
end;

function TDrawPoints.GetWidth: integer;
begin
 Result := round(FPoints.RectArea.Width);
end;

function TDrawPoints.GetHeight: integer;
begin
 Result := round(FPoints.RectArea.Height);
end;

procedure TDrawPoints.Draw(const aLayerPercentOpacity: single);
var a: byte;
  i: Integer;
begin
 if not FVisible then exit;
 a:=round( Tint.alpha.Value * Opacity.Value / 255 * aLayerPercentOpacity );
 if a=0 then exit;

 PushAndPrepareMatrix;
 SetBlendMode( FBlendMode );
 TextureManager.DisableTextureUsage;

 glPointSize( FPointSize );
 glColor4ub( round(Tint.red.Value), round(Tint.Green.Value), round(Tint.Blue.Value), a);
 glBegin(GL_POINTS);
 for i:=0 to Length(Pts)-1 do glVertex2f(Pts[i].x, Pts[i].y);//glVertex2fv(@Pts[i]);
 glEnd();

 DrawChildSurfaces( aLayerPercentOpacity*Opacity.pcValue );
 PopMatrix;
end;

constructor TDrawPoints.Create;
begin
 inherited create;
 FPoints:= TOGLCPath.Create;
 FPointSize :=1.5;
 Tint.Value:=BGRAWhite;
end;

destructor TDrawPoints.Destroy;
begin
 FPoints.Free;
 inherited Destroy;
end;

{ TPolarSprite }

constructor TPolarSprite.Create(ATexture: PTexture; Owner: boolean);
begin
 inherited Create(ATexture, Owner);
 Polar:= TPolarSystemParam.Create;
end;

destructor TPolarSprite.Destroy;
begin
 Polar.Free;
 inherited Destroy;
end;

procedure TPolarSprite.Update(const aElapsedTime: single);
begin
 if FFreeze then exit;
 inherited Update(aElapsedTime);
 Polar.OnElapse( aElapsedTime );
 SetCenterCoordinate( Polar.CartesianValue );
end;

{ TSatelliteSprite }

procedure TSatelliteSprite.SetCoordinates;
begin
 FSatCoor.x := cos( Accumulator.x ) * MovingRectangle.x.Value*0.5 + X.Value;
 FSatCoor.y := sin( Accumulator.y ) * MovingRectangle.y.Value*0.5 + Y.Value;
end;

procedure TSatelliteSprite.Draw(const aLayerPercentOpacity: single);
var t: TPointF;
begin
 if FTexture=NIL then exit;

 t:=GetXY;
 CenterX := FSatCoor.x;
 CenterY := FSatCoor.y;

 DrawSurface( FTexture, trunc( FFrame ), aLayerPercentOpacity );

 X.Value := t.x;
 Y.Value:=t.y;
end;

{ TMouseSatelliteSprite }

procedure TMouseSatelliteSprite.SetCoordinates;
begin
 CenterX := (cos( Accumulator.x )) * MovingRectangle.x.Value + MouseManager.MousePosition.x;
 CenterY := (sin( Accumulator.y )) * MovingRectangle.y.Value + MouseManager.MousePosition.y;
end;

constructor TMouseSatelliteSprite.Create( ATexture: PTexture; Owner: boolean);
begin
 inherited Create( ATexture, Owner );
 Pulse := TPointFParam.Create;
 Pulse.Value := PointF(PI*1.17, pi*0.777);
 MovingRectangle:= TPointFParam.Create;
 MovingRectangle.Value := PointF( 50, 50 );
 PulseFactor := 1.0;
 Angle.AddConstant( 360 );
end;

destructor TMouseSatelliteSprite.Destroy;
begin
 Pulse.Free;
 MovingRectangle.Free;
 inherited Destroy;
end;

procedure TMouseSatelliteSprite.Update(const aElapsedTime: single);
begin
 if FFreeze then exit;
 inherited Update(aElapsedTime);
 Pulse.OnElapse( aElapsedTime );
 Accumulator.x += Pulse.x.Value * PulseFactor * AElapsedTime;
 Accumulator.y += Pulse.y.Value * PulseFactor * AElapsedTime;
 SetCoordinates;
end;

{$endif oglcIMPLEMENTATION}

