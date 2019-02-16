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
  Polar: TPolarSystemParam;
end;

TArrayOfTPolarSprite= array of TPolarSprite;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}
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

