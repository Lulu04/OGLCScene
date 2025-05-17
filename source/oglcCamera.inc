{$ifdef oglcINTERFACE}
type

{ TOGLCShakerDescriptor }

TOGLCShakerDescriptor = record
  // range [0..1]
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


{ TSmoothFollow }
// allow to change coordinates with smooth movement.
{TSmoothFollow = record
private type TTargetMode = (_fmNone, _fmSurface, _fmPoint);
private
  FFollowMode: TTargetMode;
  FMovesAmount: single;
public
  TargetSurface: TSimpleSurfaceWithEffect;
  TargetSurfaceOffset: TPointF;
  TargetPoint: TPointF;
  // default value is 0.05 corresponds to a smooth movement.
  // Decrease to smooth the movement, increase for a rougher movement.
  Speed: single;
  procedure InitDefault;
  procedure Update(var aCurrentPos: TPointF; const AElapsedTime: single);
  // Ask to follow a surface.
  procedure SetTargetSurface(aSurface: TSimpleSurfaceWithEffect); overload;
  // Ask to follow a surface. The offset is applied to the surface position.
  procedure SetTargetSurface(aSurface: TSimpleSurfaceWithEffect; aOffset: TPointF); overload;
  // Ask to follow the center of a surface.
  procedure SetTargetSurfaceCenter(aSurface: TSimpleSurfaceWithEffect);
  // Ask to move to the specified point.
  procedure SetTargetPoint(aPt: TPointF);
  // Stops the following
  procedure CancelTargeting;
end; }

{ TOGLCCameraAutoFollow }

TOGLCCameraAutoFollow = record
private type TCameraTargetMode = (_ctmNone, _ctmSurface, _ctmPoint);
private
  FParentScene: TOGLCScene;
  FParentCamera: TOGLCCamera;
  FSmoothPointF: TSmoothPointF;
  FTargetMode: TCameraTargetMode;
  FTargetSurface: TSimpleSurfaceWithEffect;
  FTargetSurfaceOffset: TPointF;
  FTargetPoint: TPointF;
  function GetSpeed: single;
  function GetTargetSurface: TSimpleSurfaceWithEffect;
  procedure Reset; // sets all parameters to their default values.
  procedure Init(aParentScene: TOGLCScene; aParentCamera: TOGLCCamera);
  procedure SetSpeed(AValue: single);
  procedure Update(const AElapsedTime: single);
  function ApplyBoundsOnPoint(aPt: TPointF): TPointF;
  function InternalSurfaceToScene(aSurface: TSimpleSurfaceWithEffect; aPt: TPointF): TPointF;
public
  // Use this property to initialize the bounds of the camera.
  Bounds: TRectF;

  // Ask the camera to follow a surface.
  // if aMoveImmediately is True the camera moves immediately to the target and continue to follow it.
  procedure SetTargetSurface(aSurface: TSimpleSurfaceWithEffect; aMoveImmediately: boolean=False); overload;
  // Ask the camera to follow a surface. The offset is applied to the surface position.
  // if aMoveImmediately is True the camera moves immediately to the target and continue to follow it.
  procedure SetTargetSurface(aSurface: TSimpleSurfaceWithEffect; aOffset: TPointF; aMoveImmediately: boolean=False); overload;
  // Ask to follow the center of a surface.
  // if aMoveImmediately is True the camera moves immediately to the center target and continue to follow it.
  procedure SetTargetSurfaceCenter(aSurface: TSimpleSurfaceWithEffect; aMoveImmediately: boolean=False);
  // Ask the camera to move to the specified point.
  // if aMoveImmediately is True the camera moves immediately to the target and stay fixed on it.
  procedure SetTargetPoint(aPt: TPointF; aMoveImmediately: boolean=False);

  // Stops the following
  procedure CancelTargeting;

  // default value is 0.05 corresponds to a smooth movement.
  // Decrease to smooth the movement, increase for a rougher movement
  property Speed: single read GetSpeed write SetSpeed;
  property TargetSurface: TSimpleSurfaceWithEffect read GetTargetSurface;
end;


{ TOGLCCamera }

TOGLCCamera = class
  FParentScene: TOGLCScene;
  FPreviousSceneModelViewMatrix: TOGLCMatrix;
  // Don't create a TCamera instance directly by TOGLCCamera.Create
  // Use instead TOGLCScene.CreateCamera
  constructor Create;
  destructor Destroy; override;
  procedure Update(const AElapsedTime: single);
protected
  procedure Use;
  procedure Release;
public
  // By default, LookAt is initialized to (0,0) which means the center of the scene.
  // Use instead MoveTo() to easily translate the camera to a target position
  LookAt: TPointFParam;
  Scale: TPointFParam;
  Angle: TFParam;
  // Default value is the center of the scene (0.5, 0.5)
  Pivot: TPointF;
  // Shaker effect
  // The idea is to add periodicaly an offset to the camera LookAt to shake
  // the view, for example, to simulate an earthquake.
  Shaker: TOGLCShakerDescriptor;

  // move the camera immediatly to the specified location.
  // the center of the scene is (Scene.Width*0.5, Scene.Height*0.5).
  // Deactivate the AutoFollow feature.
  procedure MoveTo(aPt: TPointF); overload;
  // move the camera to the specified location.
  // the center of the scene is (Scene.Width*0.5, Scene.Height*0.5).
  // Deactivate the AutoFollow feature.
  procedure MoveTo(aPt: TPointF; aDuration: single; aCurve:Word= idcLinear); overload;

  // reset camera to scene center with scale of 1:1 and no rotation, in specified time in seconds
  procedure Reset(aDuration: single; aCurve:Word= idcLinear);

  // gives the distance between a point and the camera lookAt value
  function DistanceToPoint(aPt: TPointF): single;

  // converts a world coordinates to TOpenGLCcontrol coordinates according to camera transformations
  function WorldToControl(aPt: TPointF): TPoint;
  function WorldToControlF(aPt: TPointF): TPointF;
  // converts a TOpenGLCcontrol coordinates to world coordinates according to camera transformations
  function ControlToWorld(aX, aY: integer): TPointF;
  function ControlToWorld(aPt: TPointF): TPointF;

  // return the bounds of the view currently seen by the camera (in scene coordinates)
  function GetViewRect: TRectF;

  // Assign the camera to the layer specified by its index.
  // All previous assigned camera is replaced by the new one.
  procedure AssignToLayer(aLayerIndex: integer); overload;
  // Assign the camera to several layers specified by their indexes
  // All previous assigned camera is replaced by the new one.
  procedure AssignToLayers(const aLayerIndexList: array of integer); overload;
  // Assign the camera to the layers specified by a range index.
  // All previous assigned camera is replaced by the new one.
  procedure AssignToLayerRange(aFirstLayerIndex, aLastLayerIndex: integer);
  // the camera will be used by all layers
  procedure AssignToAllLayers;
  // The camera will no longer be used by any layer
  procedure Unassign;

  function GetTransformMatrix: TOGLCMatrix;
  function GetInverseTransformMatrix: TOGLCMatrix;
  procedure AddTransformTo(var aMatrix: TOGLCMatrix);

public // camera can follow a surface
  AutoFollow: TOGLCCameraAutoFollow;
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

{ TSmoothFollow }
{
procedure TSmoothFollow.InitDefault;
begin
  TargetSurface := NIL;
  TargetSurfaceOffset := PointF(0,0);
  FMovesAmount := 0;
  Speed := 0.05;
  FFollowMode := _fmNone;
end;

procedure TSmoothFollow.Update(var aCurrentPos: TPointF; const AElapsedTime: single);
var targetPos, delta: TPointF;
  deltaIsZero: boolean;
begin
  case FFollowMode of
    _fmNone: exit;
    _fmSurface: begin
      if TargetSurface = NIL then exit;
      targetPos := TargetSurface.SurfaceToScene(TargetSurfaceOffset);
    end;
    _fmPoint: targetPos := TargetPoint;
  end;

  delta := targetPos - aCurrentPos;

  deltaIsZero := (delta.x = 0) and (delta.y = 0);
  if deltaIsZero
    then FMovesAmount := Max(FMovesAmount-AElapsedTime*Speed, 0.0) // decelerates
    else FMovesAmount := Min(FMovesAmount+AElapsedTime*Speed, 1.0); // accelerates

  if (FMovesAmount <> 0) and not deltaIsZero then
    aCurrentPos := aCurrentPos + delta * FMovesAmount;
end;

procedure TSmoothFollow.SetTargetSurface(aSurface: TSimpleSurfaceWithEffect);
begin
  SetTargetSurface(aSurface, PointF(0,0));
end;

procedure TSmoothFollow.SetTargetSurface(aSurface: TSimpleSurfaceWithEffect; aOffset: TPointF);
begin
  TargetSurface := aSurface;
  TargetSurfaceOffset := aOffset;
  if aSurface <> NIL
    then FFollowMode := _fmSurface
    else FFollowMode := _fmNone;
  FMovesAmount := 0;
end;

procedure TSmoothFollow.SetTargetSurfaceCenter(aSurface: TSimpleSurfaceWithEffect);
begin
  SetTargetSurface(aSurface, PointF(aSurface.Width*0.5, aSurface.Height*0.5));
end;

procedure TSmoothFollow.SetTargetPoint(aPt: TPointF);
begin
  TargetPoint := aPt;
  FFollowMode := _fmPoint;
  TargetSurface := NIL;
end;

procedure TSmoothFollow.CancelTargeting;
begin
  FFollowMode :=_fmNone;
  TargetSurface := NIL;
end;    }

{ TOGLCCameraAutoFollow }

procedure TOGLCCameraAutoFollow.Init(aParentScene: TOGLCScene; aParentCamera: TOGLCCamera);
begin
  FParentScene := aParentScene;
  FParentCamera := aParentCamera;
  Reset;
end;

procedure TOGLCCameraAutoFollow.SetSpeed(AValue: single);
begin
  FSmoothPointF.Speed := AValue;
end;

procedure TOGLCCameraAutoFollow.Update(const AElapsedTime: single);
var cameraPos, targetPos: TPointF;
begin
  case FTargetMode of
    _ctmNone: exit;
    _ctmSurface: begin
      if FTargetSurface = NIL then exit;
      targetPos := InternalSurfaceToScene(FTargetSurface, FTargetSurfaceOffset);
    end;
    _ctmPoint: targetPos := FTargetPoint;
  end;

  //Move() = LookAt.Value := FParentScene.Center-aPt;
  cameraPos := FParentScene.Center - FParentCamera.LookAt.Value;

  FSmoothPointF.Update(cameraPos, targetPos, AElapsedTime);

  // apply bounds
  cameraPos.x := EnsureRange(cameraPos.x, Bounds.Left, Bounds.Right);
  cameraPos.y := EnsureRange(cameraPos.y, Bounds.Top, Bounds.Bottom);

  FParentCamera.LookAt.Value := FParentScene.Center - cameraPos;
end;

function TOGLCCameraAutoFollow.ApplyBoundsOnPoint(aPt: TPointF): TPointF;
begin
  Result.x := EnsureRange(aPt.x, Bounds.Left, Bounds.Right);
  Result.y := EnsureRange(aPt.y, Bounds.Top, Bounds.Bottom);
end;

function TOGLCCameraAutoFollow.InternalSurfaceToScene(
  aSurface: TSimpleSurfaceWithEffect; aPt: TPointF): TPointF;
var s: TSimpleSurfaceWithEffect;
begin
  s := aSurface;
  Result := aPt;
  repeat
    Result := s.GetMatrix.Transform(Result);
    s := s.ParentSurface;
  until s = NIL
end;

procedure TOGLCCameraAutoFollow.SetTargetSurface(aSurface: TSimpleSurfaceWithEffect; aMoveImmediately: boolean);
begin
  SetTargetSurface(aSurface, PointF(0,0), aMoveImmediately);
end;

procedure TOGLCCameraAutoFollow.SetTargetSurface(aSurface: TSimpleSurfaceWithEffect;
  aOffset: TPointF; aMoveImmediately: boolean);
begin
  if aSurface = NIL then
    CancelTargeting
  else begin
    if aMoveImmediately then
      FParentCamera.MoveTo(ApplyBoundsOnPoint(InternalSurfaceToScene(aSurface, aOffset)));
    FTargetSurface := aSurface;
    FTargetSurfaceOffset := aOffset;
    if aSurface <> NIL
      then FTargetMode := _ctmSurface
      else FTargetMode := _ctmNone;
    FSmoothPointF.MovesAmount := 0.0;
  end;
end;

procedure TOGLCCameraAutoFollow.SetTargetSurfaceCenter(
  aSurface: TSimpleSurfaceWithEffect; aMoveImmediately: boolean);
begin
  SetTargetSurface(aSurface, PointF(aSurface.Width*0.5, aSurface.Height*0.5), aMoveImmediately);
end;

procedure TOGLCCameraAutoFollow.SetTargetPoint(aPt: TPointF; aMoveImmediately: boolean);
begin
  if aMoveImmediately then
    FParentCamera.MoveTo(ApplyBoundsOnPoint(aPt));

  FTargetPoint := aPt;
  FTargetMode := _ctmPoint;
  FTargetSurface := NIL;
end;

procedure TOGLCCameraAutoFollow.CancelTargeting;
begin
  FTargetMode :=_ctmNone;
  FTargetSurface := NIL;
end;

procedure TOGLCCameraAutoFollow.Reset;
begin
  FSmoothPointF.InitDefault;
  FSmoothPointF.Speed := 0.05;
  Bounds := RectF(FParentScene.Width*0.5, FParentScene.Height*0.5,
                  FParentScene.Width*0.5, FParentScene.Height*0.5);
end;

function TOGLCCameraAutoFollow.GetSpeed: single;
begin
  Result := FSmoothPointF.Speed;
end;

function TOGLCCameraAutoFollow.GetTargetSurface: TSimpleSurfaceWithEffect;
begin
  Result := FTargetSurface;
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
  // AutoFollow.Init() is called from TOGLCScene.CreateCamera
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
  AutoFollow.Update(AElapsedTime);
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
//var r: TRectF;
begin
{  r := GetViewRect;
  Result.x := Round(aPt.x / r.Width * FParentScene.Width);
  Result.y := Round(aPt.y / r.Height * FParentScene.Height); }
  Result := GetTransformMatrix.Transform(aPt).Round;
end;

function TOGLCCamera.WorldToControlF(aPt: TPointF): TPointF;
//var r: TRectF;
begin
{  r := GetViewRect;
  Result.x := aPt.x / r.Width * FParentScene.Width;
  Result.y := aPt.y / r.Height * FParentScene.Height; }
  Result := GetTransformMatrix.Transform(aPt);
end;

function TOGLCCamera.ControlToWorld(aX, aY: integer): TPointF;
//var r: TRectF;
begin
{  Result.x := aX / FParentScene.Width;
  Result.y := aY / FParentScene.Height;
  r := GetViewRect;
  Result.x := r.Left + r.Width * Result.x;
  Result.y := r.Top + r.Height * Result.y; }
  Result := GetInverseTransformMatrix.Transform(PointF(aX, aY));
end;

function TOGLCCamera.ControlToWorld(aPt: TPointF): TPointF;
begin
  Result := GetInverseTransformMatrix.Transform(aPt);
end;

function TOGLCCamera.GetViewRect: TRectF;
var m: TOGLCMatrix;
{  tl,tr,bl,br, mini, maxi: TPointF;
  function GetMin(const v1, v2: single): single; inline;
  begin
    if v1 < v2 then Result := v1 else Result := v2;
  end;
  function GetMax(const v1, v2: single): single; inline;
  begin
    if v1 > v2 then Result := v1 else Result := v2;
  end;  }
begin
  m := GetInverseTransformMatrix;
  Result.TopLeft := m.Transform(PointF(0, 0));
  Result.BottomRight := m.Transform(PointF(FParentScene.Width, FParentScene.Height));

  // the version above take in acount the rotation
{  tl := m.Transform(PointF(0, 0));
  tr := m.Transform(PointF(FParentScene.Width, 0));
  bl := m.Transform(PointF(0, FParentScene.Height));
  br := m.Transform(PointF(FParentScene.Width, FParentScene.Height));

  mini := PointF(GetMin(tl.x, GetMin(tr.x, GetMin(bl.x, br.x))),
                 GetMin(tl.y, GetMin(tr.y, GetMin(bl.y, br.y))));
  maxi := PointF(GetMax(tl.x, GetMax(tr.x, GetMax(bl.x, br.x))),
                 GetMax(tl.y, GetMax(tr.y, GetMax(bl.y, br.y))));

  Result.Left := mini.x;
  Result.Top := mini.y;
  Result.Right := maxi.x;
  Result.Bottom := maxi.y; }
end;

procedure TOGLCCamera.AssignToLayer(aLayerIndex: integer);
begin
  FParentScene.Layer[aLayerIndex].Camera := self;
end;

procedure TOGLCCamera.AssignToLayers(const aLayerIndexList: array of integer);
var i: Integer;
begin
  with FParentScene do
   for i:=0 to Length(aLayerIndexList)-1 do
     AssignToLayer(aLayerIndexList[i]);
end;

procedure TOGLCCamera.AssignToLayerRange(aFirstLayerIndex, aLastLayerIndex: integer);
var i: integer;
begin
  if aFirstLayerIndex > aLastLayerIndex then begin
    i := aLastLayerIndex;
    aLastLayerIndex := aFirstLayerIndex;
    aFirstLayerIndex := i;
  end;
  for i:=aFirstLayerIndex to aLastLayerIndex do
    AssignToLayer(i);
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

function TOGLCCamera.GetTransformMatrix: TOGLCMatrix;
begin
  Result.LoadIdentity;
  Result.AddTransform(0, 0, Scale.x.Value, Scale.y.Value, Angle.Value,
                      FParentScene.Width*0.5, FParentScene.Height*0.5,
                      LookAt.x.Value+Shaker.ComputedX, LookAt.y.Value+Shaker.ComputedY);
end;

function TOGLCCamera.GetInverseTransformMatrix: TOGLCMatrix;
var m: TOGLCMatrix;
begin
  m := GetTransformMatrix;
  Result := m.Inverse;
end;

procedure TOGLCCamera.AddTransformTo(var aMatrix: TOGLCMatrix);
begin
  aMatrix.AddTransform(0, 0, Scale.x.Value, Scale.y.Value, Angle.Value,
                       FParentScene.Width*0.5, FParentScene.Height*0.5,
                       LookAt.x.Value+Shaker.ComputedX, LookAt.y.Value+Shaker.ComputedY);
end;

procedure TOGLCCamera.Use;
begin
  FPreviousSceneModelViewMatrix.CopyFrom(FParentScene.ModelViewMatrix);
  FParentScene.ModelViewMatrix.AddTransform(0, 0, Scale.x.Value, Scale.y.Value, Angle.Value,
                                            FParentScene.Width*Pivot.x, FParentScene.Height*Pivot.y,
                                            LookAt.x.Value+Shaker.ComputedX, LookAt.y.Value+Shaker.ComputedY);
end;

procedure TOGLCCamera.Release;
begin
  FParentScene.ModelViewMatrix.CopyFrom(FPreviousSceneModelViewMatrix);
end;

procedure TOGLCCamera.MoveTo(aPt: TPointF);
begin
  LookAt.Value := FParentScene.Center-aPt;
  AutoFollow.SetTargetSurface(NIL);
end;

procedure TOGLCCamera.MoveTo(aPt: TPointF; aDuration: single; aCurve: Word);
begin
  LookAt.ChangeTo(FParentScene.Center-aPt, aDuration, aCurve);
  AutoFollow.SetTargetSurface(NIL);
end;


{$endif oglcIMPLEMENTATION}

