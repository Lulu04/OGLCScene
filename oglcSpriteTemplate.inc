{$ifdef oglcINTERFACE}
type

{ TSatelliteSprite }
// Satellite sprite that turn like an electron in rectangular area

TSatelliteSprite = class( TSprite )
protected
  Accumulator: TPointF;
public
  Pulse: TPointFParam; // sprite's speed
  MovingRectangle: TPointFParam; // width and height of the rectangle area where the sprite move
  PulseFactor: single;
  Constructor Create( ATexture: PTexture; Owner: boolean=FALSE );
  Destructor Destroy; override;
  procedure Update( const aElapsedTime: single ); override;
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

constructor TSatelliteSprite.Create( ATexture: PTexture; Owner: boolean);
begin
 inherited Create( ATexture, Owner );
 Pulse := TPointFParam.Create;
 Pulse.Value := PointF(PI*1.17, pi*0.777);
 MovingRectangle:= TPointFParam.Create;
 MovingRectangle.Value := PointF( 50, 50 );
 PulseFactor := 1.0;
 Angle.AddConstant( 360 );
end;

destructor TSatelliteSprite.Destroy;
begin
 Pulse.Free;
 MovingRectangle.Free;
 inherited Destroy;
end;

procedure TSatelliteSprite.Update(const aElapsedTime: single);
begin
 if FFreeze then exit;
 inherited Update(aElapsedTime);
 Pulse.OnElapse( aElapsedTime );
 Accumulator.x += Pulse.x.Value * PulseFactor * AElapsedTime;
 Accumulator.y += Pulse.y.Value * PulseFactor * AElapsedTime;
 X.Value := (cos( Accumulator.x )) * MovingRectangle.x.Value + MouseManager.MousePosition.x;
 Y.Value := (sin( Accumulator.y )) * MovingRectangle.y.Value + MouseManager.MousePosition.y;
end;

{$endif oglcIMPLEMENTATION}

