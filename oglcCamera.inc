{$ifdef oglcINTERFACE}
type
// Don't create a TCamera instance directly by TOGLCCamera.Create
// Use instead TOGLCScene.CreateCamera

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
 Constructor Create;
 Destructor Destroy; override;
 procedure Update( const AElapsedTime: single );
protected
  procedure Use;
  procedure Release;
public
  LookAt: TPointFParam;
  Scale: TPointFParam;
  Angle: TFParam;

  // move the camera to the specified location
  procedure MoveTo( aPt: TPointF ); overload;
  procedure MoveTo( aPt: TPointF; aDuration: single; aCurve:Word= idcLinear ); overload;
  // reset camera to screen center with scale of 1:1 and no rotation, in specified time in seconds
  procedure Reset( aDuration: single; aCurve:Word= idcLinear);

  // the camera will be used by the specified layer(s)
  procedure AssignToLayer( aLayerIndex: integer ); overload;
  procedure AssignToLayer( const aLayerIndexList: array of integer ); overload;
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
 LookAt.OnElapse( AElapsedTime );
 Scale.OnElapse( AElapsedTime );
 Angle.OnElapse( AElapsedTime );
end;

procedure TOGLCCamera.Reset(aDuration: single; aCurve:Word);
begin
 LookAt.ChangeTo( PointF(0,0), aDuration, aCurve );
 Scale.ChangeTo( PointF(1,1), aDuration, aCurve );
 Angle.ChangeTo( 0, aDuration, aCurve );
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

procedure TOGLCCamera.Unassign;
var
  i: Integer;
begin
 for i:=0 to FParentScene.LayerCount-1 do
   with FParentScene.Layer[i] do if Camera=self then Camera := NIL;
end;

procedure TOGLCCamera.Use;
begin
 glPushMatrix;

 glTranslatef( FParentScene.Width * 0.5, FParentScene.Height * 0.5 , 0 );

 glScalef( Scale.x.Value, Scale.y.Value, 0 );
 glRotatef( Angle.Value, 0, 0, 1 );
 glTranslatef( -FParentScene.Width * 0.5 + LookAt.x.Value, -FParentScene.Height * 0.5 + LookAt.y.Value, 0 );
end;

procedure TOGLCCamera.Release;
begin
 glPopMatrix;
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
 FTemp := SplitLineToStringArray( Actions.Strings[CurrentLine], ' ' );

 case FTemp.[0] of
  acWait : WaitTime := StrToFloat( FTemp[1] );

  acAngle : FParentCamera.Angle.Value := StrToFloat( FTemp[1] ) ;
  acRotate : FParentCamera.Angle.AddConstant( StrToFloat( FTemp[1] ) );
  acRotateTo : FParentCamera.Angle.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] )  );

  acScale : FParentCamera.Scale.Value := PointF( StrToFloat( FTemp[1] ), StrToFloat( FTemp[1] ) );
  acScaleChange : begin
   FParentCamera.Scale.x.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
   FParentCamera.Scale.y.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  end;
  acScaleH : FParentCamera.Scale.x.Value := StrToFloat( FTemp[1] );
  acScaleHChange : FParentCamera.Scale.x.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  acScaleV : FParentCamera.Scale.y.Value := StrToFloat( FTemp[1] );
  acScaleVChange : FParentCamera.Scale.y.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));


  acMoveTo  : FParentCamera.LookAt.ChangeTo(PointF( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] )), StrToFloat( FTemp[3] ), StringToCurveID( FTemp[4] ));
  acMoveXTo : FParentCamera.LookAt.x.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  acMoveYTo : FParentCamera.LookAt.y.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));

  acCenterOnScene: FParentCamera.LookAt.ChangeTo( FParentCamera.FParentScene.Center,
                                                  StrToFloat( FTemp[1] ),
                                                  StringToCurveID( FTemp[2] ));

  acSendEvent : if FScenarioEvent <> NIL then FScenarioEvent( NIL, ID, strtoint( FTemp[1] ) );

  acLoop : CurrentLine := -1 ;
  acLabel :;
  acGotoLabel : begin
   i := Actions.IndexOf( acLabel + ' ' + FTemp[1] );
   if i <> -1 then CurrentLine := i - 1;
  end;

  else DecodeAdditionnalAction( FTemp );
 end;//case
end;


{$endif oglcIMPLEMENTATION}
