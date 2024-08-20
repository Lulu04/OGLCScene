{$ifdef oglcINTERFACE}
type

{
available action for camera scenario

acAngle                = 'Angle'    ; // Angle NewAngle
acRotate               = 'Rotate'   ; // Rotate AnglePerSecond
acRotateTo             = 'RotateTo' ; // RotateTo Angle Duration Curve

acScale                = 'Scale'          ; // Scale HVValue      // [1]= normal size, [0..1[->reduced, ]1..inf[->enlarged
acScaleChange          = 'ScaleChange'    ; // ScaleChange HVNewValue Duration Curve
acScaleH               = 'ScaleH'         ; // ScaleH HValue      // to set scale value for horizontaly axis
acScaleHChange         = 'ScaleHChange'   ; // ScaleHChange HNewValue Duration Curve
acScaleV               = 'ScaleV'         ; // ScaleV VValue      // to set scale value for verticaly axis
acScaleVChange         = 'ScaleVChange'   ; // ScaleVChange VNewValue Duration Curve

acMoveTo               = 'MoveTo'         ; // MoveTo X Y Duration Curve
acMoveXTo              = 'MoveXTo'        ; // MoveXTo X Duration Curve
acMoveYTo              = 'MoveYTo'        ; // MoveYTo Y Duration Curve
acCenterOnScene        = 'CenterOnScene'  ; // CenterOnScene Duration Curve
acSendEvent            = 'SendEvent' ; // SendEvent UserEvent


}

{ TScenarioForCamera }

TScenarioForCamera=class(TScenario)
 protected
   FParentCamera: TOGLCCamera;
   procedure DecodeAction; override;
 public

end;

{ TOGLCCamera }

TOGLCCamera = class
  FParentScene: TOGLCScene;
  FPreviousModelViewMatrix: TOGLCMatrix;
  // Don't create a TCamera instance directly by TOGLCCamera.Create
  // Use instead TOGLCScene.CreateCamera
  constructor Create;
  destructor Destroy; override;
  procedure Update( const AElapsedTime: single );
private
  FShakerDeltaX, FShakerDeltaY, FShakerDurationMin, FShakerDurationMax,
  FShakerTimeReference, FShakerTimeAccu, FShakerComputedX, FShakerComputedY: single;
protected
  procedure Use;
  procedure Release;
public
  // by default, LookAt is initialized with the center coodinates of its parent scene
  // use instead MoveTo to easily translate the camera to the target position
  LookAt: TPointFParam;
  Scale: TPointFParam;
  Angle: TFParam;

  // move the camera to the specified location
  procedure MoveTo( aPt: TPointF ); overload;
  // move the camera to the specified location
  procedure MoveTo( aPt: TPointF; aDuration: single; aCurve:Word= idcLinear ); overload;
  // reset camera to scene center with scale of 1:1 and no rotation, in specified time in seconds
  procedure Reset( aDuration: single; aCurve:Word= idcLinear);

  procedure StartShaker(aDeltaX, aDeltaY, aDuration: single);
  procedure StartShaker(aDeltaX, aDeltaY, aDurationMin, aDurationMax: single);
  procedure StopShaker;

  // gives the distance between a point and the camera lookAt value
  function DistanceToPoint( aPt: TPointF ): single;

  // converts a world point coordinate to screen coordinate relative to camera view
  function WorldToScreen( aPt: TPointF ): TPointF;
  // converts a screen coordinates to world coordinates relative to camera view
  function ScreenToWorld( aPt: TPointF ): TPointF;

  // converts a screen coordinates [0..ScreenWidth],[0..ScreenHeight] to scene coordinate
  // taking in account the camera offset and scaling value. (not its rotate value)
  function ScreenToScene( aPt: TPointF ): TPointF;

  // translate the camera to the center of the target surface
//  procedure CenterAndZoomOn( aTargetSurface: TSimpleSurfaceWithEffect );

  // return the bounds of the view currently seen by the camera
  // the coordinates are relative to the scene
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
  {
  // Scenario
  function AddScenario( const aFilename: string; ACallBackEvent: TScenarioEvent=NIL ): TScenarioForCamera;overload; // return Scenario's ID
  function AddScenario( aScenario: TScenario ): TScenarioForCamera; overload;
  procedure PlayScenario( AIDScenario: TIDScenario; AFromBeginning: boolean=TRUE );
  function AddAndPlayScenario( const aFilename: string; ACallBackEvent: TScenarioEvent=NIL ): TIDScenario;overload; // return Scenario's ID
  function AddAndPlayScenario( aScenario: TScenario ): TIDScenario; overload;
  procedure StopScenario( AIDScenario: TIDScenario );
  procedure StopAllScenario;
  function ScenarioIsPlaying( AIDScenario: TIDScenario ): boolean;
  }
end;
{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCCamera }

constructor TOGLCCamera.Create;
begin
  LookAt := TPointFParam.Create;
  Scale := TPointFParam.Create;
  Scale.Value := PointF(1,1);
  Angle := TFParam.Create;
end;

destructor TOGLCCamera.Destroy;
begin
 FreeAndNil( LookAt );
 FreeAndNil( Scale );
 FreeAndNil( Angle );
 inherited Destroy;
end;

procedure TOGLCCamera.Update(const AElapsedTime: single);
begin
  LookAt.OnElapse(AElapsedTime);
  Scale.OnElapse(AElapsedTime);
  Angle.OnElapse(AElapsedTime);

  // update shaker effect
  if (FShakerDeltaX = 0) and (FShakerDeltaY = 0) then exit;
  FShakerTimeAccu := FShakerTimeAccu + AElapsedTime;
  if FShakerTimeAccu >= FShakerTimeReference then begin
    FShakerTimeAccu := FShakerTimeAccu - FShakerTimeReference;
    FShakerTimeReference := Random*(FShakerDurationMax-FShakerDurationMin) + FShakerDurationMin;
    // compute new offset values
    FShakerComputedX := random*FShakerDeltaX - FShakerDeltaX*0.5;
    FShakerComputedY := random*FShakerDeltaY - FShakerDeltaY*0.5;
  end;
end;

procedure TOGLCCamera.Reset(aDuration: single; aCurve:Word);
begin
 LookAt.ChangeTo( PointF(0,0), aDuration, aCurve );
 Scale.ChangeTo( PointF(1,1), aDuration, aCurve );
 Angle.ChangeTo( 0, aDuration, aCurve );
end;

procedure TOGLCCamera.StartShaker(aDeltaX, aDeltaY, aDuration: single);
begin
  StartShaker(aDeltaX, aDeltaY, aDuration, aDuration);
end;

procedure TOGLCCamera.StartShaker(aDeltaX, aDeltaY, aDurationMin, aDurationMax: single);
begin
  FShakerDeltaX := aDeltaX;
  FShakerDeltaY := aDeltaY;
  FShakerDurationMin := aDurationMin;
  FShakerDurationMax := aDurationMax;
  FShakerTimeAccu := 0;
  FShakerTimeReference := Random*(aDurationMax-aDurationMin) + aDurationMin;
  if (aDeltaX = 0) and (aDeltaY = 0) then begin
    FShakerComputedX := 0;
    FShakerComputedY := 0;
  end else begin
   FShakerComputedX := Random*aDeltaX - aDeltaX*0.5;
   FShakerComputedY := Random*aDeltaY - aDeltaY*0.5;
  end;
end;

procedure TOGLCCamera.StopShaker;
begin
  FShakerDeltaX := 0;
  FShakerDeltaY := 0;
  FShakerComputedX := 0;
  FShakerComputedY := 0;
end;

function TOGLCCamera.DistanceToPoint(aPt: TPointF): single;
begin
 Result := aPt.Distance(LookAt.Value);
end;

function TOGLCCamera.WorldToScreen(aPt: TPointF): TPointF;
begin
 Result := aPt + LookAt.Value;
end;

function TOGLCCamera.ScreenToWorld(aPt: TPointF): TPointF;
begin
 Result := aPt - LookAt.Value;
end;

function TOGLCCamera.ScreenToScene(aPt: TPointF): TPointF;
var coefx, coefy: single;
  vr: TRectF;
  m, mInv: TOGLCMatrix;
begin
{ coefx := aPt.x/FParentScene.Width;
 coefy := aPt.y/FParentScene.Height;
 vr := GetViewRect;
 Result.x := vr.Left+vr.Width*coefx;
 Result.y := vr.Top+vr.Height*coefy;  }

 m.LoadIdentity;
 m.AddTransform(0, 0, Scale.x.Value, Scale.y.Value, Angle.Value,
                FParentScene.Width*0.5, FParentScene.Height*0.5,
                LookAt.x.Value, LookAt.y.Value);
//m.Matrix := MultMat(m.Matrix, FParentScene.ProjectionMatrix.Matrix);
 mInv.Matrix := InverseMat(m.Matrix);
 Result := mInv.Transform(aPt);
end;

function TOGLCCamera.GetViewRect: TRectF;
var vieww, viewh: single;
begin
 vieww := FParentScene.Width/Scale.x.Value;
 viewh := FParentScene.Height/Scale.y.Value;

 Result.Left := LookAt.x.Value+FParentScene.Width*0.5-vieww*0.5;
 Result.Top := LookAt.y.Value+FParentScene.Height*0.5-viewh*0.5;
 Result.Width := vieww;
 Result.Height:= viewh;
end;

procedure TOGLCCamera.AssignToLayer(aLayerIndex: integer);
begin
 FParentScene.Layer[aLayerIndex].Camera := self;
end;

procedure TOGLCCamera.AssignToLayer(const aLayerIndexList: array of integer);
var
  i: Integer;
begin
 with FParentScene do
  for i:=0 to Length(aLayerIndexList)-1 do
    AssignToLayer( aLayerIndexList[i] );
end;

procedure TOGLCCamera.AssignToAllLayers;
var
  i: Integer;
begin
  for i:=0 to FParentScene.LayerCount-1 do
    AssignToLayer(i);
end;

procedure TOGLCCamera.Unassign;
var
  i: Integer;
begin
 for i:=0 to FParentScene.LayerCount-1 do
   with FParentScene.Layer[i] do
     if Camera = Self then Camera := NIL;
end;

procedure TOGLCCamera.Use;
var tx, ty, sx, sy, a: single;
  doscale, dorotate: boolean;
begin
 FPreviousModelViewMatrix.CopyFrom(FParentScene.ModelViewMatrix);
  FParentScene.ModelViewMatrix.AddTransform(0, 0, Scale.x.Value, Scale.y.Value, Angle.Value,
                                           FParentScene.Width*0.5, FParentScene.Height*0.5,
                                           LookAt.x.Value+FShakerComputedX, LookAt.y.Value+FShakerComputedY);

{ tx := FParentScene.Width*0.5;
 ty := FParentScene.Height*0.5;
 sx := Scale.x.Value;
 sy := Scale.y.Value;
 a := Angle.Value;
 with FParentScene.ModelViewMatrix do begin
  Translate(tx, ty);
  Scale(sx, sy);
  Rotate(a);
  Translate(-tx+LookAt.x.Value, -ty+LookAt.y.Value);
 end;      }

{ glPushMatrix;

 glTranslatef( FParentScene.Width * 0.5, FParentScene.Height * 0.5 , 0 );

 glScalef( Scale.x.Value, Scale.y.Value, 0 );
 glRotatef( Angle.Value, 0, 0, 1 );
 glTranslatef( -FParentScene.Width * 0.5 + LookAt.x.Value, -FParentScene.Height * 0.5 + LookAt.y.Value, 0 );
}
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

{ TScenarioForCamera }

procedure TScenarioForCamera.DecodeAction;
var i: integer;
begin
 if Length(Actions.Strings[CurrentLine]) = 0 then exit;
 FTemp := Actions.Strings[CurrentLine].Split([' ']);

 case FTemp[0] of
  acWait : WaitTime := StringToSingle( FTemp[1] );

  acAngle : FParentCamera.Angle.Value := StringToSingle( FTemp[1] ) ;
  acRotate : FParentCamera.Angle.AddConstant( StringToSingle( FTemp[1] ) );
  acRotateTo : FParentCamera.Angle.ChangeTo( StringToSingle( FTemp[1] ), StringToSingle( FTemp[2] ),  StringToCurveID( FTemp[3] )  );

  acScale : FParentCamera.Scale.Value := PointF( StringToSingle( FTemp[1] ), StringToSingle( FTemp[1] ) );
  acScaleChange : begin
   FParentCamera.Scale.x.ChangeTo( StringToSingle( FTemp[1] ), StringToSingle( FTemp[2] ), StringToCurveID( FTemp[3] ));
   FParentCamera.Scale.y.ChangeTo( StringToSingle( FTemp[1] ), StringToSingle( FTemp[2] ), StringToCurveID( FTemp[3] ));
  end;
  acScaleH : FParentCamera.Scale.x.Value := StringToSingle( FTemp[1] );
  acScaleHChange : FParentCamera.Scale.x.ChangeTo( StringToSingle( FTemp[1] ), StringToSingle( FTemp[2] ), StringToCurveID( FTemp[3] ));
  acScaleV : FParentCamera.Scale.y.Value := StringToSingle( FTemp[1] );
  acScaleVChange : FParentCamera.Scale.y.ChangeTo( StringToSingle( FTemp[1] ), StringToSingle( FTemp[2] ), StringToCurveID( FTemp[3] ));


  acMoveTo  : FParentCamera.LookAt.ChangeTo(PointF( StringToSingle( FTemp[1] ), StringToSingle( FTemp[2] )), StringToSingle( FTemp[3] ), StringToCurveID( FTemp[4] ));
  acMoveXTo : FParentCamera.LookAt.x.ChangeTo( StringToSingle( FTemp[1] ), StringToSingle( FTemp[2] ), StringToCurveID( FTemp[3] ));
  acMoveYTo : FParentCamera.LookAt.y.ChangeTo( StringToSingle( FTemp[1] ), StringToSingle( FTemp[2] ), StringToCurveID( FTemp[3] ));

  acCenterOnScene: FParentCamera.LookAt.ChangeTo( FParentCamera.FParentScene.Center,
                                                  StringToSingle( FTemp[1] ),
                                                  StringToCurveID( FTemp[2] ));

  acLoop : CurrentLine := -1 ;
  acLabel :;
  acGotoLabel : begin
   i := Actions.IndexOf( acLabel + ' ' + FTemp[1] );
   if i <> -1 then CurrentLine := i - 1;
  end;

  else if FScenarioDecodeProc<>NIL then FScenarioDecodeProc(FTemp); //DecodeAdditionnalAction( FTemp );
 end;//case
end;


{$endif oglcIMPLEMENTATION}

