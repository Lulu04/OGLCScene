{$ifdef oglcINTERFACE}
type

{ TOGLCShakerDescriptor }

TOGLCShakerDescriptor = record
  Amount: TBoundedFParam;
  AmplitudeX, AmplitudeY, DurationMin, DurationMax,
  TimeReference, TimeAccu, ComputedX, ComputedY: single;
  procedure Create;
  procedure Free;
  procedure Update(const AElapsedTime: single);
  // Start the shaker effect.
  // aAmplitudeX, aAmplitudeY are in pixel, aRefreshTime in second.
  // Set aSetAmountTo1 to True to force amount value to be 1.0 (usefull after a fadeout)
  procedure Start(aAmplitudeX, aAmplitudeY, aRefreshTime: single; aSetAmountTo1: boolean=True);
  procedure Start(aAmplitudeX, aAmplitudeY, aRefreshTimeMin, aRefreshTimeMax: single; aSetAmountTo1: boolean=True);
  // Stop the shaker effect
  procedure Stop;
  // Fade in the shaker effect.
  // first call Start() to initialize the wanted shaker amplitude and refresh time
  procedure FadeIn(aAmountValue, aFadeDuration: single; aCurve: Word=idcLinear);
  // Fade out the shaker effect. When the fade is done, Amount.Value is equal to 0.0,
  // don't forget to set it to 1.0 before another call to Start().
  procedure FadeOut(aFadeDuration: single; aCurve: Word=idcLinear);
end;


{ TOGLCCamera }

TOGLCCamera = class
  FParentScene: TOGLCScene;
  FPreviousModelViewMatrix: TOGLCMatrix;
  // Don't create a TCamera instance directly by TOGLCCamera.Create
  // Use instead TOGLCScene.CreateCamera
  constructor Create;
  destructor Destroy; override;
  procedure Update(const AElapsedTime: single);
protected
  procedure Use;
  procedure Release;
public
  // By default, LookAt is initialized with the center coordinates of the scene
  // Use instead MoveTo to easily translate the camera to the target position
  LookAt: TPointFParam;
  Scale: TPointFParam;
  Angle: TFParam;
  // Default value is the center of the scene (0.5, 0.5)
  Pivot: TPointF;
  // Shaker effect
  // The idea is to add an offset to the camera LookAt to shake the view i.e. to simulate an earthquake.
  Shaker: TOGLCShakerDescriptor;

  // move the camera to the specified location
  procedure MoveTo( aPt: TPointF ); overload;
  // move the camera to the specified location
  procedure MoveTo( aPt: TPointF; aDuration: single; aCurve:Word= idcLinear ); overload;
  // reset camera to scene center with scale of 1:1 and no rotation, in specified time in seconds
  procedure Reset( aDuration: single; aCurve:Word= idcLinear);

  // gives the distance between a point and the camera lookAt value
  function DistanceToPoint( aPt: TPointF ): single;

  // converts a world coordinates to TOpenGLCcontrol coordinates according to camera transformations
  function WorldToControl( aPt: TPointF ): TPoint;
  function WorldToControlF( aPt: TPointF ): TPointF;
  // converts a TOpenGLCcontrol coordinates to world coordinates according to camera transformations
  function ControlToWorld(aX, aY: integer): TPointF;

  // return the bounds of the view currently seen by the camera (in scene coordinates)
  function GetViewRect: TRectF;

  // the camera will be used by the specified layer, given by its index
  // if another camera was assigned on this layer, the new one replace it
  procedure AssignToLayer( aLayerIndex: integer ); overload;
  // the camera will be used to several layers, given by their indexes
  procedure AssignToLayer( const aLayerIndexList: array of integer ); overload;
  // the camera will be used by all layers
  procedure AssignToAllLayers;
  // The camera will no longer be used by any layer
  procedure Unassign;

  function GetInverseTransformMatrix: TOGLCMatrix;
end;
{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCShakerDescriptor }

procedure TOGLCShakerDescriptor.Create;
begin
  Amount := CreateBoundedFParam(0.0, MaxSingle-1, False);
  Amount.Value := 1.0;
end;

procedure TOGLCShakerDescriptor.Free;
begin
  Amount.Free;
  Amount := NIL;
end;

procedure TOGLCShakerDescriptor.Update(const AElapsedTime: single);
begin
  Amount.OnElapse(AElapsedTime);

 // update shaker effect
 if (AmplitudeX = 0) and (AmplitudeY = 0) then exit;
 TimeAccu := TimeAccu + AElapsedTime;
 if TimeAccu >= TimeReference then begin
   TimeAccu := TimeAccu - TimeReference;
   TimeReference := Random*(DurationMax-DurationMin) + DurationMin;
   // compute new offset values
   ComputedX := (random*AmplitudeX - AmplitudeX*0.5) * Amount.Value;
   ComputedY := (random*AmplitudeY - AmplitudeY*0.5) * Amount.Value;
 end;
end;

procedure TOGLCShakerDescriptor.Start(aAmplitudeX, aAmplitudeY, aRefreshTime: single;
  aSetAmountTo1: boolean);
begin
  Start(aAmplitudeX, aAmplitudeY, aRefreshTime, aRefreshTime, aSetAmountTo1);
 end;

procedure TOGLCShakerDescriptor.Start(aAmplitudeX, aAmplitudeY,
  aRefreshTimeMin, aRefreshTimeMax: single; aSetAmountTo1: boolean);
begin
  if aSetAmountTo1 then Amount.Value := 1.0;
  AmplitudeX := aAmplitudeX;
  AmplitudeY := aAmplitudeY;
  DurationMin := aRefreshTimeMin;
  DurationMax := aRefreshTimeMax;
  TimeAccu := 0;
  TimeReference := Random*(aRefreshTimeMax - aRefreshTimeMin) + aRefreshTimeMin;
  if (aAmplitudeX = 0) and (aAmplitudeY = 0) then begin
    ComputedX := 0;
    ComputedY := 0;
  end else begin
    ComputedX := (random*AmplitudeX - AmplitudeX*0.5) * Amount.Value;
    ComputedY := (random*AmplitudeY - AmplitudeY*0.5) * Amount.Value;
  end;
end;

procedure TOGLCShakerDescriptor.FadeIn(aAmountValue, aFadeDuration: single; aCurve: Word);
begin
  Amount.ChangeTo(aAmountValue, aFadeDuration, aCurve);
end;

procedure TOGLCShakerDescriptor.FadeOut(aFadeDuration: single; aCurve: Word=idcLinear);
begin
  Amount.ChangeTo(0, aFadeDuration, aCurve);
end;

procedure TOGLCShakerDescriptor.Stop;
begin
  AmplitudeX := 0;
  AmplitudeY := 0;
  ComputedX := 0;
  ComputedY := 0;
end;

{ TOGLCCamera }

constructor TOGLCCamera.Create;
begin
  LookAt := TPointFParam.Create;
  Scale := TPointFParam.Create;
  Scale.Value := PointF(1,1);
  Angle := TFParam.Create;
  Pivot.x := 0.5;
  Pivot.y := 0.5;
  Shaker.Create;
end;

destructor TOGLCCamera.Destroy;
begin
  LookAt.Free;
  LookAt := NIL;
  Scale.Free;
  Scale := NIL;
  Angle.Free;
  Angle := NIL;
  Shaker.Free;
  inherited Destroy;
end;

procedure TOGLCCamera.Update(const AElapsedTime: single);
begin
  LookAt.OnElapse(AElapsedTime);
  Scale.OnElapse(AElapsedTime);
  Angle.OnElapse(AElapsedTime);
  Shaker.Update(AElapsedTime);
end;

procedure TOGLCCamera.Reset(aDuration: single; aCurve:Word);
begin
  LookAt.ChangeTo( PointF(0,0), aDuration, aCurve );
  Scale.ChangeTo( PointF(1,1), aDuration, aCurve );
  Angle.ChangeTo( 0, aDuration, aCurve );
end;

function TOGLCCamera.DistanceToPoint(aPt: TPointF): single;
begin
  Result := aPt.Distance(LookAt.Value);
end;

function TOGLCCamera.WorldToControl(aPt: TPointF): TPoint;
var r: TRectF;
begin
  r := GetViewRect;
  Result.x := Round(aPt.x / r.Width * FParentScene.Width);
  Result.y := Round(aPt.y / r.Height * FParentScene.Height);
end;

function TOGLCCamera.WorldToControlF(aPt: TPointF): TPointF;
var r: TRectF;
begin
  r := GetViewRect;
  Result.x := aPt.x / r.Width * FParentScene.Width;
  Result.y := aPt.y / r.Height * FParentScene.Height;
end;

function TOGLCCamera.ControlToWorld(aX, aY: integer): TPointF;
var r: TRectF;
begin
  Result.x := aX / FParentScene.Width;
  Result.y := aY / FParentScene.Height;
  r := GetViewRect;
  Result.x := r.Left + r.Width * Result.x;
  Result.y := r.Top + r.Height * Result.y;
end;

function TOGLCCamera.GetViewRect: TRectF;
var m: TOGLCMatrix;
begin
  m := GetInverseTransformMatrix;
  Result.TopLeft := m.Transform(PointF(0, 0));
  Result.BottomRight := m.Transform(PointF(FParentScene.Width, FParentScene.Height));
end;

procedure TOGLCCamera.AssignToLayer(aLayerIndex: integer);
begin
  FParentScene.Layer[aLayerIndex].Camera := self;
end;

procedure TOGLCCamera.AssignToLayer(const aLayerIndexList: array of integer);
var i: Integer;
begin
  with FParentScene do
   for i:=0 to Length(aLayerIndexList)-1 do
     AssignToLayer( aLayerIndexList[i] );
end;

procedure TOGLCCamera.AssignToAllLayers;
var i: Integer;
begin
  for i:=0 to FParentScene.LayerCount-1 do
    AssignToLayer(i);
end;

procedure TOGLCCamera.Unassign;
var i: Integer;
begin
  for i:=0 to FParentScene.LayerCount-1 do
    with FParentScene.Layer[i] do
      if Camera = Self then Camera := NIL;
end;

function TOGLCCamera.GetInverseTransformMatrix: TOGLCMatrix;
var m: TOGLCMatrix;
begin
  m.LoadIdentity;
  m.AddTransform(0, 0, Scale.x.Value, Scale.y.Value, Angle.Value,
                 FParentScene.Width*0.5, FParentScene.Height*0.5,
                 LookAt.x.Value+Shaker.ComputedX, LookAt.y.Value+Shaker.ComputedY);
  Result := m.Inverse;
end;

procedure TOGLCCamera.Use;
begin
  FPreviousModelViewMatrix.CopyFrom(FParentScene.ModelViewMatrix);
  FParentScene.ModelViewMatrix.AddTransform(0, 0, Scale.x.Value, Scale.y.Value, Angle.Value,
                                            FParentScene.Width*Pivot.x, FParentScene.Height*Pivot.y,
                                            LookAt.x.Value+Shaker.ComputedX, LookAt.y.Value+Shaker.ComputedY);
end;

procedure TOGLCCamera.Release;
begin
  FParentScene.ModelViewMatrix.CopyFrom(FPreviousModelViewMatrix);
end;

procedure TOGLCCamera.MoveTo(aPt: TPointF);
begin
  LookAt.Value := FParentScene.Center-aPt;
end;

procedure TOGLCCamera.MoveTo(aPt: TPointF; aDuration: single; aCurve: Word);
begin
  LookAt.ChangeTo( FParentScene.Center-aPt, aDuration, aCurve);
end;


{$endif oglcIMPLEMENTATION}

