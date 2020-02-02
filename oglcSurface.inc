{$ifdef oglcINTERFACE}
type

{ TSimpleSurface }
TSimpleSurface = class
 Constructor Create;
 Destructor Destroy; override;
protected
 FParentScene: TOGLCScene;
 FParentLayer: TLayer;
 FVisible: boolean;
 FIndex: integer;
 FTag1: integer;
 FTag2: boolean;
 FGroup: integer;
 FKill: boolean;
 function GetCenterX: single;
 function GetRightX: single;
 function GetBottomY: single;
 function GetCenterY: single;
 procedure SetCenterX(const AValue: single);
 procedure SetRightX(const AValue: single);
 procedure SetBottomY(const AValue: single);
 procedure SetCenterY(const AValue: single);
protected
 function GetWidth: integer; virtual; abstract;
 function GetHeight: integer; virtual; abstract;
public
 procedure Draw( const aLayerPercentOpacity: single ); virtual; abstract;
 procedure Update( const AElapsedTime: single ); virtual; abstract;
public
 X, Y: TFParam;
 // return rounded rectangle area of the surface
 function GetRectArea: TRect;
 // return TopLeft coordinates of the surface
 function GetXY: TPointF;
 // return the center coordinates of the surface
 function Center: TPointF;
 // remove the surface from the scene and free it
 procedure Kill;
 // Coordinates
 procedure SetCoordinate( aX, aY: single );overload;
 procedure SetCoordinate( aP: TPointF );overload;
 procedure SetCenterCoordinate ( aCenterX , aCenterY : single);overload;
 procedure SetCenterCoordinate ( aP: TPointF);overload;
 property CenterX : single read GetCenterX write SetCenterX;
 property RightX : single read GetRightX write SetRightX;
 property CenterY : single read GetCenterY write SetCenterY;
 property BottomY : single read GetBottomY write SetBottomY;
 // Size
 property Width : integer read GetWidth;
 property Height : integer read GetHeight;

 property Visible : boolean read FVisible write FVisible; // =TRUE set the surface visible on scene
 property SceneIndex : integer read FIndex write FIndex; // surface index layer list
 property Tag1 : integer read FTag1 write FTag1;
 property Tag2 : boolean read FTag2 write FTag2;
 property Group : integer read FGroup write FGroup;
 property ParentLayer: TLayer read FParentLayer write FParentLayer;
 property ParentScene: TOGLCScene read FParentScene write FParentScene;
end;

TScenarioEvent = procedure ( aSurface: TSimpleSurfaceWithEffect;
                             aScenarioID: TIDScenario;
                             aUserEvent: integer ) of object; // CallBack for scenario event

TScenarioDecode = procedure(aAction: ArrayOfString) of object;
{ TScenario }

TScenario = class
 Constructor Create ;
 Destructor Destroy ; override ;
protected
 ID           : integer     ;
 Actions      : TStringlist ;
 CurrentLine  : integer     ;
 WaitTime     : single      ;
 ParentSurface: TSimpleSurfaceWithEffect ;
 FIsRunning   : boolean     ;
 FTemp        : ArrayOfString ;
 FScenarioEvent : TScenarioEvent ;
 FScenarioDecodeProc: TScenarioDecode;
 procedure Execute ( const aElapsedTime : double ) ;
 procedure LoadFromFile ( const aFilename: string ) ;
 procedure SaveToFile ( const aFilename: string ) ;
 procedure DecodeAction; virtual;
// procedure DecodeAdditionnalAction( {%H-}aAction: ArrayOfString ); virtual;
public
 procedure AddLine( const aLine: string ) ;  // to construct the scenario manually
 property OnEvent: TScenarioEvent read FScenarioEvent write FScenarioEvent ; // Scenario event callback
 // this callback is only called when an action has not been recognized
 property OnDecode: TScenarioDecode read FScenarioDecodeProc write FScenarioDecodeProc;
end;

{ TSimpleSurfaceWithEffect }

TSimpleSurfaceWithEffect = class( TSimpleSurface )
  Constructor Create;
  Destructor Destroy; override;
private
  FMessageList: TMessageList;
  FEnableProcessMessage: boolean;
private
  procedure Init;
  procedure SetFrame(AValue: single);
protected
 FChilds: TList;
 FParentSurface: TSimpleSurfaceWithEffect;
 FSurfaceOrigin: TPointF;
 FGuiEnabled: boolean;
 function GetChild( aIndex: integer ): TSimpleSurfaceWithEffect;
 procedure SetParentSurface( aSurface: TSimpleSurfaceWithEffect );
 procedure SetParentScene( aScene: TOGLCScene ); virtual;
 procedure SetParentLayer( aLayer: TLayer );
protected
   FBlendMode: byte;
   FFreeze       : boolean ;
   FTimeBeforeKill: single;
   // Blink
   FBlink : TBlink;
   // Rotation around axis
   FRotationAroundPt     : TRotationAroundPoint ;
   // Flip
   FFlipH:boolean;
   FFlipV:boolean;
   // Frame animation
   FFrame: single;
   FFrameDeltaPerSecond: single;
   FFrameLoopStart,
   FFrameLoopEnd: integer;
   // Bind Surface
   FBindToSprite  : TBindToSprite ;
protected
   FListScenario : array of TScenario ;    // surface can execute several scenario at the same time
   FScenarioAdditionnalDecode: TScenarioDecode;
   function GetScenario( AIDScenario: TIDScenario ): TScenario;
protected
   procedure PushAndPrepareMatrix; inline;
   procedure PopMatrix; inline;
   procedure DrawChildSurfaces( const aLayerPercentOpacity: single );
   procedure DrawSurface( aTexture: PTexture; AFrameIndex: integer; const aLayerPercentOpacity : single ); overload;
   procedure DrawSurface( const aTopLeftColor, aTopRightColor, aBottomRightColor, aBottomLeftColor: TBGRAPixel; const aLayerPercentOpacity : single ); overload;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single );  override;
public
   Speed   : TPointFParam;
   Angle   : TBoundedFParam;
   Opacity : TBoundedFParam;
   Tint    : TBGRAParam;
   Scale   : TPointFParam;
   Skew    : TPointFParam;
   // post a message to the surface. the message will be processed later in the Update method
   // the delay allows to process the message only after a lapse of time (in seconds)
   procedure AddMessage( UserValue: word; aDelay: single=0 );
   // override to process the message received from another surface
   procedure ProcessMessage( {%H-}UserValue: word ); virtual;
   // lock the call to process message
   procedure LockProcessMessage;
   // unlock the call to processmessage
   procedure UnlockProcessMessage;

   // add a child to the surface
   procedure AddChild( ASurface: TSimpleSurfaceWithEffect );overload;
   procedure DeleteChild( aIndex: integer ); overload;
   procedure DeleteChild( aSurface: TSimpleSurfaceWithEffect ); overload;
   procedure DeleteAllChilds;
   function SurfaceToScene( aPts: TPointF ): TPointF;
   function SceneToSurface( aPts: TPointF ): TPointF;
   function SurfaceToScreen( aPts: TPointF ): TPointF;
   function ScreenToSurface( aPts: TPointF ): TPointF;
   function SurfaceToParent( aPts: TPointF; aParent: TSimpleSurfaceWithEffect=NIL): TPointF;
   function ParentToSurface( aPts: TPointF; aParent: TSimpleSurfaceWithEffect=NIL): TPointF;

   function GetScaledArea: TRect;

   // Kill defered
   procedure KillDefered( ASec: single );
   // Move
   procedure MoveTo( aX, aY: single ; aDuration: single; aCurve:Word= idcLinear );
   procedure MoveTo( aPt: TPointF; aDuration: single; aCurve:Word= idcLinear );
   procedure MoveTo( aX, aY: single ; aDuration: single; aXCurve, aYCurve: Word );
   procedure MoveTo( aPt: TPointF; aDuration: single; aXCurve, aYCurve: Word );
   procedure MoveCenterTo( aCenterX, aCenterY, aDuration: single; aCurve:Word = idcLinear );
   procedure MoveCenterTo( aPt: TPointF; aDuration: single; aCurve:Word = idcLinear );
   procedure MoveXCenterTo( aX, aDuration: single; aCurve:Word = idcLinear );
   procedure MoveYCenterTo( aY, aDuration: single; aCurve:Word = idcLinear );
   procedure MoveRelative( aDeltaX, aDeltaY: single; aDuration: single; aCurve: Word = idcLinear );
   procedure MoveRelative( aDelta: TPointF; aDuration: single; aCurve: Word = idcLinear );
   procedure MoveXRelative( aDeltaX, aDuration: single; aCurve: Word= idcLinear );
   procedure MoveYRelative( aDeltaY, aDuration: single; aCurve: Word= idcLinear );
   // Blink
   procedure Blink ( aNumberOfBlink : integer ; aVisibleTime , aInvisibleTime : single ) ; // aNumberOfBlink=-1 for infinite blink
   procedure StopBlink;
   // Rotate around point
   procedure RotationAroundPoint( aXPt, aYPt: single; aAnglePerSecond: single; aSelfRotate: boolean=TRUE ) ; overload;
   procedure RotationAroundPoint( aPt: TPointF; aAnglePerSecond: single; aSelfRotate: boolean=TRUE ) ; overload;
   // Bind this surface to another
   procedure BindToSprite ( aParentSurface:TSimpleSurface; aOffsetX, aOffsetY: single );
   procedure BindCenterToSprite( aParentSurface:TSimpleSurface );
   procedure UnBind;
   // Scenario
   function AddScenario( const aFilename: string; ACallBackEvent: TScenarioEvent=NIL ): TIDScenario;overload; // return Scenario's ID
   function AddScenario( aScenario: TScenario ): TIDScenario;overload;
   procedure PlayScenario( AIDScenario: TIDScenario; AFromBeginning: boolean=TRUE );
   function AddAndPlayScenario( const aFilename: string; ACallBackEvent: TScenarioEvent=NIL ): TIDScenario;overload; // return Scenario's ID
   function AddAndPlayScenario( aScenario: TScenario ): TIDScenario; overload;
   procedure StopScenario( AIDScenario: TIDScenario );
   procedure StopAllScenario;
   function ScenarioIsPlaying( AIDScenario: TIDScenario ): boolean;
   // set a callbak proc to decode custom additionnal actions
   procedure SetScenarioAdditionnalDecode( aCallbackDecode: TScenarioDecode );
   // Frame anime
   procedure SetFrameLoopBounds( AStartFrameIndex, AEndFrameIndex: integer );
   procedure FrameAddPerSecond( ADeltaFramePerSecond: single );
   property Frame: single read FFrame write SetFrame;
   // Flip H and V
   property FlipH : boolean read FFlipH write FFlipH ;
   property FlipV : boolean read FFlipV write FFlipV ;
   // utils
   procedure CenterOnScene;
   procedure CenterOnSurface( ASurface: TSimpleSurface );
   // Collision test
   function CollisionTestWith( aPoint: TPointF ): boolean; overload;
   function CollisionTestWith( aSurface: TSimpleSurfaceWithEffect ): boolean; overload;
   function CollisionTestWith( aSurface: TSimpleSurfaceWithEffect; HTolerance, VTolerance: integer ): boolean; overload;
   function CollisionTestWith( const aX, aY, aWidth, aHeight: single): boolean; overload;
   function CollisionTestWith( aRect: TRect ): boolean; overload;

   property Freeze : boolean read FFreeze write FFreeze ;  // set to TRUE to stop all variations on surface ( move, opacity change, scale change, etc...)
   property BlendMode: byte read FBlendMode write FBlendMode;
public
   function ChildCount: integer;
   property Child[index:integer]: TSimpleSurfaceWithEffect read GetChild;
   property ParentSurface: TSimpleSurfaceWithEffect read FParentSurface;

   procedure EnableGui;  // enable mouse effect on gui surfaces
   procedure DisableGui; // disable mouse effect on gui surfaces
end;

TArrayOfPTexture = array of PTexture;

{ TSprite }

TSprite = class ( TSimpleSurfaceWithEffect )
private
   FTextureOwner: boolean;
   procedure Init; virtual;
protected
   FTexture: PTexture;
   function GetWidth: integer; override;
   function GetHeight: integer; override;
public
   procedure Draw( const aLayerPercentOpacity: single ); override;
public
   // create a sprite from a texture.
   // if Owner is set to TRUE, the texture will be freed when the sprite will be killed
   Constructor Create( ATexture: PTexture; Owner: boolean=FALSE );
   Destructor Destroy; override;

   property Texture: PTexture read FTexture;
end;

TArrayOfSprite = array of TSprite ;
PArrayOfSprite = ^TArrayOfSprite ;


{ TColorBackground }

TColorBackground = class( TSimpleSurfaceWithEffect )
   Constructor Create( aX, aY: single; aWidth, aHeight : integer );
   Destructor Destroy; override;
protected
   FWidth, FHeight: integer;
   function GetWidth: integer; override;
   function GetHeight: integer; override;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single ); override;
public
   TopLeftColor,
   TopRightColor,
   BottomRightColor,
   BottomLeftColor: TBGRAParam;
   procedure SetSize( aWidth, aHeight: integer );
   procedure SetAllColorsTo( AColor: TBGRAPixel );
   procedure SetLeftColors( AColor: TBGRAPixel );
   procedure SetRightColors( AColor: TBGRAPixel );
   procedure SetTopColors( AColor: TBGRAPixel );
   procedure SetBottomColors( AColor: TBGRAPixel );
end;

{ TEllipse }

TEllipse = class( TSimpleSurfaceWithEffect )
  Constructor Create( aCenter: TPointF; aRx, aRy: single );
  Destructor Destroy; override;
protected
  FPath: ArrayOfTPointF;
  FOldRx,
  FOldRy: single;
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Update( const aElapsedTime: single ); override;
  procedure Draw( const aLayerPercentOpacity: single ); override;
public
  RadiusX,
  RadiusY: TFParam;
  // Tint property is the color of the ellipse
end;

{$endif oglcINTERFACE}



{$ifdef oglcIMPLEMENTATION}
{TEllipse }
constructor TEllipse.Create( aCenter: TPointF; aRx, aRy: single );
begin
 inherited Create;
 X.Value := aCenter.x;
 Y.Value := aCenter.y;
 FPath := ComputeEllipse( 0, 0, aRx, aRy );
 FOldRx := aRx;
 FOldRy := aRy;
 RadiusX := TFParam.Create;
 RadiusX.Value := aRx;
 RadiusY := TFParam.Create;
 RadiusY.Value := aRy;
end;

destructor TEllipse.Destroy;
begin
 RadiusX.Free;
 RadiusY.Free;
 inherited Destroy;
end;

function TEllipse.GetWidth: integer;
begin
 Result := round( RadiusX.Value*2);
end;

function TEllipse.GetHeight: integer;
begin
 Result := round( RadiusY.Value*2);
end;

procedure TEllipse.Update(const aElapsedTime: single);
begin
 inherited Update(aElapsedTime);
 if FFreeze then exit;

 RadiusX.OnElapse( aElapsedTime );
 RadiusY.OnElapse( aElapsedTime );

 if (RadiusX.Value <> FOldRx) or
    (RadiusY.Value <> FOldRy) then begin
  FOldRx := RadiusX.Value;
  FOldRy := RadiusY.Value;
  FPath := ComputeEllipse( 0, 0, RadiusX.Value, RadiusY.Value );
 end;
end;

procedure TEllipse.Draw(const aLayerPercentOpacity: single);
var i: integer;
begin
 if not Visible or
    (Tint.Alpha.Value = 0) or
    (Length(FPath)=0) then exit;

 PushAndPrepareMatrix;
 SetBlendMode( FBlendMode );
 TextureManager.DisableTextureUsage;

 for i:=1 to Length( FPath )-1 do
  DrawLine( FPath[i-1], FPath[i], Tint.Value, aLayerPercentOpacity );

 PopMatrix;
end;

{ TColorBackground }

constructor TColorBackground.Create(aX, aY: single; aWidth, aHeight: integer);
begin
 inherited Create;
 X.Value := aX;
 Y.Value := aY;
 SetSize( aWidth, aHeight );

 TopLeftColor := TBGRAParam.Create;
 TopRightColor := TBGRAParam.Create;
 BottomRightColor := TBGRAParam.Create;
 BottomLeftColor := TBGRAParam.Create;

 TopLeftColor.Value := BGRA(255,0,0);
 TopRightColor.Value := BGRA(0,255,0);
 BottomRightColor.Value := BGRA(0,0,255);
 BottomLeftColor.Value := BGRA(255,255,0);
end;

destructor TColorBackground.Destroy;
begin
 FreeAndNil( TopLeftColor );
 FreeAndNil( TopRightColor );
 FreeAndNil( BottomRightColor );
 FreeAndNil( BottomLeftColor );
 inherited Destroy;
end;

function TColorBackground.GetWidth: integer;
begin
 Result := FWidth;
end;

function TColorBackground.GetHeight: integer;
begin
 Result := FHeight;
end;

procedure TColorBackground.Update(const aElapsedTime: single);
begin
  if FFreeze then exit;
  inherited Update(aElapsedTime);
  TopLeftColor.OnElapse( aElapsedTime );
  TopRightColor.OnElapse( aElapsedTime );
  BottomRightColor.OnElapse( aElapsedTime );
  BottomLeftColor.OnElapse( aElapsedTime );
end;

procedure TColorBackground.Draw(const aLayerPercentOpacity: single);
begin
 DrawSurface(TopLeftColor.Value, TopRightColor.Value, BottomRightColor.Value, BottomLeftColor.Value, aLayerPercentOpacity );
end;

procedure TColorBackground.SetSize(aWidth, aHeight: integer);
begin
 FWidth := aWidth;
 FHeight := aHeight;
end;

procedure TColorBackground.SetAllColorsTo(AColor: TBGRAPixel);
begin
 TopLeftColor.Value := AColor;
 TopRightColor.Value := AColor;
 BottomRightColor.Value := AColor;
 BottomLeftColor.Value := AColor;
end;

procedure TColorBackground.SetLeftColors(AColor: TBGRAPixel);
begin
 TopLeftColor.Value := AColor;
 BottomLeftColor.Value := AColor;
end;

procedure TColorBackground.SetRightColors(AColor: TBGRAPixel);
begin
 TopRightColor.Value := AColor;
 BottomRightColor.Value := AColor;
end;

procedure TColorBackground.SetTopColors(AColor: TBGRAPixel);
begin
 TopLeftColor.Value := AColor;
 TopRightColor.Value := AColor;
end;

procedure TColorBackground.SetBottomColors(AColor: TBGRAPixel);
begin
 BottomRightColor.Value := AColor;
 BottomLeftColor.Value := AColor;
end;


{ TSprite }

constructor TSprite.Create(ATexture: PTexture; Owner: boolean);
begin
 inherited Create;
 FTextureOwner := Owner;
 FTexture := ATexture;
 Init;
end;

destructor TSprite.Destroy;
begin
 if FTextureOwner then TextureManager.Delete( FTexture );
 inherited Destroy;
end;

procedure TSprite.Draw(const aLayerPercentOpacity : single);
begin
 if FTexture=NIL then exit;
 DrawSurface( FTexture, trunc( FFrame ), aLayerPercentOpacity );
end;

procedure TSprite.Init;
begin
end;

function TSprite.GetWidth: integer;
begin
 if FTexture=NIL
   then Result := 0
   else Result := FTexture^.FrameWidth;
end;

function TSprite.GetHeight: integer;
begin
  if FTexture=NIL
   then Result := 0
   else Result := FTexture^.FrameHeight;
end;


{ TSimpleSurfaceWithEffect }

constructor TSimpleSurfaceWithEffect.Create;
begin
 inherited Create ;
 Init;
end;

procedure TSimpleSurfaceWithEffect.Init;
begin
 FBlendMode := FX_BLEND_NORMAL;
 FFreeze := FALSE ;
 FTimeBeforeKill := 0.0;

 Speed := TPointFParam.Create;

 Angle:= CreateBoundedFParam( 0, 359.9999, TRUE ); //    TFParam.Create;
 Angle.Value := 0;

 Opacity := CreateBoundedFParam(0, 255);
 Opacity.Value := 255;

 Tint := TBGRAParam.Create;
 Tint.Value := BGRA(0,0,0,0);

 Scale := TPointFParam.Create;
 Scale.Value := PointF(1, 1);

 Skew := TPointFParam.Create;

 // Blink
 FBlink.BlinkToGo := 0;

 // Rotation Around Axis
 FRotationAroundPt.InProgress := FALSE ;

 FFlipH := FALSE;
 FFlipV := FALSE;

 FBindToSprite.InProgress:=FALSE;
 FBindToSprite.ParentSurface:=NIL;

 FFrame := 0;
 FFrameDeltaPerSecond := 0.0;
 FFrameLoopStart := 0;
 FFrameLoopEnd := 0;

 FChilds := TList.Create;
 FParentSurface := NIL;
 FGuiEnabled := TRUE;

 FMessageList := TMessageList.Create;
 FEnableProcessMessage:=TRUE;
end;

procedure TSimpleSurfaceWithEffect.SetFrame(AValue: single);
begin
 FFrame:=AValue;
end;

procedure TSimpleSurfaceWithEffect.EnableGui;
var i: integer;
begin
 FGuiEnabled:=TRUE;
 for i:=0 to ChildCount-1 do
  TSimpleSurfaceWithEffect(FChilds.Items[i]).EnableGui;
end;

procedure TSimpleSurfaceWithEffect.DisableGui;
var i: integer;
begin
 FGuiEnabled:=FALSE;
 for i:=0 to ChildCount-1 do
  TSimpleSurfaceWithEffect(FChilds.Items[i]).DisableGui;
end;

procedure TSimpleSurfaceWithEffect.SetParentSurface(aSurface: TSimpleSurfaceWithEffect);
begin
 FParentSurface := aSurface;
end;

procedure TSimpleSurfaceWithEffect.SetParentScene(aScene: TOGLCScene);
var i: integer;
begin
 FParentScene := aScene;
 for i:=0 to ChildCount-1 do
  GetChild(i).SetParentScene( aScene );
end;

procedure TSimpleSurfaceWithEffect.SetParentLayer(aLayer: TLayer);
var i: integer;
begin
 FParentLayer := aLayer;
 for i:=0 to ChildCount-1 do
  GetChild(i).SetParentLayer( aLayer );
end;

function TSimpleSurfaceWithEffect.GetScenario(AIDScenario: TIDScenario ): TScenario;
begin
 if ( AIDScenario > -1 ) and ( AIDScenario < Length(FListScenario) )
   then Result := FListScenario[AIDScenario]
   else Result := NIL;
end;

destructor TSimpleSurfaceWithEffect.Destroy;
var i: integer;
begin
 while FChilds.Count > 0 do
  begin
   TSimpleSurfaceWithEffect( FChilds.Items[0] ).Free;
   FChilds.Delete( 0 );
  end;
 FreeAndNil( FChilds );

 FreeAndNil( Speed );
 FreeAndNil( Angle );
 FreeAndNil( Opacity );
 FreeAndNil( Tint );
 FreeAndNil( Scale );
 FreeAndNil( Skew );

 for i:=0 to Length( FListScenario )-1 do
  FListScenario[i].Free;
 SetLength( FListScenario, 0 );

 FMessageList.Free;

 inherited Destroy;
end;


procedure TSimpleSurfaceWithEffect.Update(const aElapsedTime: single);
var Xr,Yr,s,c: single ;
    i, loopLength: integer;
begin
 FSurfaceOrigin := PointF(X.Value,Y.Value);
 if FParentSurface<>NIL then FSurfaceOrigin+= FParentSurface.FSurfaceOrigin;

 if FEnableProcessMessage then FMessageList.ProcessMessages( aElapsedTime );

 if FFreeze then exit ;

 Speed.OnElapse( aElapsedTime );

  if X.State <> psNO_CHANGE
    then X.OnElapse( aElapsedTime )
    else X.Value := X.Value + Speed.x.Value * aElapsedTime;
  if Y.State <> psNO_CHANGE
    then Y.OnElapse( aElapsedTime )
    else Y.Value := Y.Value + Speed.y.Value * aElapsedTime;

  if FTimeBeforeKill <> 0
   then begin
         FTimeBeforeKill -= aElapsedTime;
         if FTimeBeforeKill <= 0 then FKill := TRUE;
   end;

 FFrame += aElapsedTime * FFrameDeltaPerSecond;
 if (FFrameLoopEnd <> 0) and (FFrameLoopStart <> 0) then
   begin
     loopLength := FFrameLoopEnd-FFrameLoopStart+1;

     while trunc(FFrame)>FFrameLoopEnd do FFrame-=loopLength;
     while trunc(FFrame)<FFrameLoopStart do FFrame+=loopLength;
{
     if FFrame >= FFrameLoopEnd then
       begin
         if loopLength <= 1 then
           FFrame := FFrameLoopEnd
         else
         begin
           FFrame := FFrame - Trunc((FFrame-(FFrameLoopStart))/loopLength)*loopLength;
           if FFrame > FFrameLoopEnd then FFrame := FFrameLoopStart;
           if FFrame < FFrameLoopStart then FFrame := FFrameLoopStart;
         end;
       end else
     if FFrame < FFrameLoopStart then
       begin
         if loopLength <= 1 then
           FFrame := FFrameLoopStart
         else
         begin
           FFrame += Trunc((FFrameLoopEnd-FFrame)/loopLength)*loopLength;
           if FFrame > FFrameLoopEnd then FFrame := FFrameLoopEnd;
           if FFrame < FFrameLoopStart then FFrame := FFrameLoopEnd;
         end;
       end;
}

   end;

 // Blink
 with FBlink do
 if BlinkToGo <> 0
 then begin
       if CountInvisible > 0
         then begin // surface is invisible
               FVisible := FALSE ;
               CountInvisible -= aElapsedTime ;
               CountVisible := VisibleTime ;
              end
         else begin // surface is visible
               FVisible := TRUE ;
               CountVisible -= aElapsedTime ;
               if CountVisible <= 0
                 then begin
                       CountInvisible := InvisibleTime ;
                       if BlinkToGo > 0 then dec ( BlinkToGo ) ;
                       if BlinkToGo = 0 then FVisible := TRUE ;
                      end;
              end;
      end;
 // Angle
 Angle.OnElapse( aElapsedTime );
 // Do Rotation around an axis
 with FRotationAroundPt do
  if InProgress
    then begin
           Xr := GetCenterX - XPt ;
           Yr := GetCenterY - YPt ;
           sincos( AnglePerSecond * aElapsedTime * deg2rad, s, c );
           SetCenterCoordinate( XPt + Xr * c + Yr * s, YPt - Xr * s  + c * Yr );
   end;
 // Do Opacity Variation
 Opacity.OnElapse( aElapsedTime );
 // Do Tint Variation
 Tint.OnElapse( aElapsedTime );
 // Do Scale Variation
 Scale.OnElapse( aElapsedTime );
 // Do Skew Variation
 Skew.OnElapse( aElapsedTime );
 // Do Bind to another sprite
 if (FBindToSprite.InProgress) and ( FBindToSprite.ParentSurface<>NIL )
   then begin
         X.Value := FBindToSprite.ParentSurface.X.Value + FBindToSprite.OffsetX ;
         Y.Value := FBindToSprite.ParentSurface.Y.Value + FBindToSprite.OffsetY ;
   end;
 // Scenario
 for i:=0 to Length( FListScenario )-1 do
  if FListScenario[i].FIsRunning
    then FListScenario[i].Execute( aElapsedTime );

 // childs update
 for i:=0 to FChilds.Count-1 do
  TSimpleSurfaceWithEffect( FChilds.Items[i] ).Update( aElapsedTime );

 // delete all childs with FKill=TRUE
 for i:=FChilds.Count-1 downto 0 do
  if TSimpleSurfaceWithEffect( FChilds.Items[i] ).FKill
    then begin
          TSimpleSurfaceWithEffect( FChilds.Items[i] ).Free;
          FChilds.Delete( i );
    end;
end;

procedure TSimpleSurfaceWithEffect.Draw(const aLayerPercentOpacity : single);
begin
 // draw is done in descendant classes
 if aLayerPercentOpacity = 0 then;
end;

procedure TSimpleSurfaceWithEffect.AddMessage(UserValue: word; aDelay: single);
begin
 FMessageList.Add( UserValue, @ProcessMessage, aDelay );
end;

procedure TSimpleSurfaceWithEffect.ProcessMessage(UserValue: word);
begin
// override to process the received message
end;

procedure TSimpleSurfaceWithEffect.LockProcessMessage;
begin
 FEnableProcessMessage:=FALSE;
end;

procedure TSimpleSurfaceWithEffect.UnlockProcessMessage;
begin
 FEnableProcessMessage:=TRUE;
end;

procedure TSimpleSurfaceWithEffect.AddChild(ASurface: TSimpleSurfaceWithEffect);
begin
 FChilds.Add( ASurface );

 ASurface.SetParentSurface( self );
 ASurface.SetParentScene( FParentScene );
 ASurface.SetParentLayer( FParentLayer );
end;

procedure TSimpleSurfaceWithEffect.DeleteChild(aIndex: integer);
begin
 if (aIndex<0) or (aIndex>FChilds.Count-1) then exit;
 GetChild( aIndex ).Kill;
// FChilds.Delete(aIndex);
end;

procedure TSimpleSurfaceWithEffect.DeleteChild(
  aSurface: TSimpleSurfaceWithEffect);
begin
 DeleteChild( FChilds.IndexOf( aSurface ));
end;

procedure TSimpleSurfaceWithEffect.DeleteAllChilds;
begin
 while FChilds.Count>0 do DeleteChild(0);
end;

function TSimpleSurfaceWithEffect.SurfaceToScene(aPts: TPointF): TPointF;
begin
 Result := FSurfaceOrigin + aPts;
end;

function TSimpleSurfaceWithEffect.SceneToSurface(aPts: TPointF): TPointF;
begin
 Result := aPts - FSurfaceOrigin;
end;

function TSimpleSurfaceWithEffect.SurfaceToScreen(aPts: TPointF): TPointF;
begin
 Result := SurfaceToScene( aPts );
 Result.x := Result.x + FParentScene.FOGLC.Left;
 Result.y := Result.y + FParentScene.FOGLC.Top;
end;

function TSimpleSurfaceWithEffect.ScreenToSurface(aPts: TPointF): TPointF;
begin
 Result := aPts - PointF( FParentScene.FOGLC.Left,FParentScene.FOGLC.Top );
 Result := SceneToSurface( Result );
end;

function TSimpleSurfaceWithEffect.SurfaceToParent(aPts: TPointF;
  aParent: TSimpleSurfaceWithEffect): TPointF;
begin
 if not Assigned(aParent) then aParent := FParentSurface;
 Result := aParent.SceneToSurface( SurfaceToScene( aPts ) );
end;

function TSimpleSurfaceWithEffect.ParentToSurface(aPts: TPointF;
  aParent: TSimpleSurfaceWithEffect): TPointF;
begin
 if not Assigned(aParent) then aParent := FParentSurface;
 Result := SceneToSurface( aParent.SurfaceToScene( aPts ) );
end;

function TSimpleSurfaceWithEffect.GetScaledArea: TRect;
var xx,yy,ww,hh: single;
begin
 ww := Width*Scale.x.Value;
 hh := Height*Scale.y.Value;
 xx := FSurfaceOrigin.x-ww*0.5;//X.Value-ww*0.5;
 yy := FSurfaceOrigin.y-hh*0.5;//Y.Value-hh*0.5;
 Result.Create(Point(round(xx),round(yy)), round(ww), round(hh));
end;

procedure TSimpleSurfaceWithEffect.KillDefered(ASec: single);
begin
 FTimeBeforeKill := ASec;
end;

procedure TSimpleSurfaceWithEffect.MoveTo(aX, aY: single; aDuration: single; aCurve: Word);
begin
 MoveTo( aX, aY, aDuration, aCurve, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveTo(aPt: TPointF; aDuration: single;
  aCurve: Word);
begin
 X.ChangeTo( aPt.x, aDuration, aCurve );
 Y.ChangeTo( aPt.y, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveTo(aX, aY: single; aDuration: single; aXCurve, aYCurve: Word);
begin
 X.ChangeTo( aX, aDuration, aXCurve );
 Y.ChangeTo( aY, aDuration, aYCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveTo(aPt: TPointF; aDuration: single;
  aXCurve, aYCurve: Word);
begin
 X.ChangeTo( aPt.x, aDuration, aXCurve );
 Y.ChangeTo( aPt.y, aDuration, aYCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveCenterTo(aCenterX, aCenterY, aDuration: single; aCurve: Word);
begin
 MoveXCenterTo( aCenterX, aDuration, aCurve );
 MoveYCenterTo( aCenterY, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveCenterTo(aPt: TPointF; aDuration: single; aCurve: Word);
begin
 MoveXCenterTo( aPt.x, aDuration, aCurve );
 MoveYCenterTo( aPt.y, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveXCenterTo(aX, aDuration: single; aCurve: Word);
begin
 X.ChangeTo( aX - GetWidth * 0.5, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveYCenterTo(aY, aDuration: single; aCurve: Word);
begin
 Y.ChangeTo( aY - GetHeight * 0.5, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveRelative(aDeltaX, aDeltaY: single; aDuration: single; aCurve: Word);
begin
 MoveTo (X.Value+aDeltaX, Y.Value+aDeltaY, aDuration, aCurve ) ;
end;

procedure TSimpleSurfaceWithEffect.MoveRelative(aDelta: TPointF;
  aDuration: single; aCurve: Word);
begin
 MoveTo (X.Value+aDelta.x, Y.Value+aDelta.y, aDuration, aCurve ) ;
end;

procedure TSimpleSurfaceWithEffect.MoveXRelative(aDeltaX, aDuration: single; aCurve: Word);
begin
 X.ChangeTo( X.Value + aDeltaX, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveYRelative(aDeltaY, aDuration: single; aCurve: Word);
begin
 Y.ChangeTo( Y.Value + aDeltaY, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.Blink(aNumberOfBlink: integer; aVisibleTime, aInvisibleTime: single );
begin
 with FBlink do
  begin
   BlinkToGo := aNumberOfBlink ;
   VisibleTime := aVisibleTime ;
   InvisibleTime := aInvisibleTime ;
   CountVisible := aVisibleTime ;
   CountInvisible := aInvisibleTime ;
  end;
end;

procedure TSimpleSurfaceWithEffect.StopBlink;
begin
 FBlink.BlinkToGo := 0;
end;

procedure TSimpleSurfaceWithEffect.RotationAroundPoint( aXPt , aYPt : single ; aAnglePerSecond : single ; aSelfRotate : boolean) ;
var xx,yy:single;
begin
 with FRotationAroundPt do
  begin
   InProgress := TRUE ;
   XPt := aXPt ;
   YPt := aYPt ;
   AnglePerSecond := aAnglePerSecond ;
   // calculate actual angle and radius
   xx:=CenterX-aXPt; yy:=CenterY-aYPt;
   Radius:=sqrt( xx * xx + yy * yy );
   if Radius>0 then ActualAngleRad := arccos( xx / Radius )
               else ActualAngleRad := 0;
  end;
 if aSelfRotate then Angle.AddConstant( -aAnglePerSecond );
end;

procedure TSimpleSurfaceWithEffect.RotationAroundPoint(aPt: TPointF;
  aAnglePerSecond: single; aSelfRotate: boolean);
begin
 RotationAroundPoint( aPt.x, aPt.y, aAnglePerSecond, aSelfRotate );
end;

procedure TSimpleSurfaceWithEffect.PushAndPrepareMatrix;
begin
 glPushMatrix;
 if FParentSurface = NIL then glTranslatef( X.Value + Width * 0.5, Y.Value + Height * 0.5 , 0 )
                         else glTranslatef( X.Value + Width * 0.5 * Scale.x.Value, Y.Value + Height * 0.5 * Scale.y.Value, 0 );

 glScalef( Scale.x.Value, Scale.y.Value, 0 );
 glRotatef( Angle.Value, 0, 0, 1 );
 glTranslatef( -Width * 0.5, -Height * 0.5, 0 );
end;

procedure TSimpleSurfaceWithEffect.PopMatrix;
begin
 glPopMatrix;
end;

procedure TSimpleSurfaceWithEffect.DrawChildSurfaces(const aLayerPercentOpacity: single);
var i: integer;
begin
 for i:=0 to FChilds.Count-1 do
  TSimpleSurfaceWithEffect( FChilds.Items[i] ).Draw( aLayerPercentOpacity );
end;


type
  PTextureCoordIndex = ^TTextureCoordIndex;
  TTextureCoordIndex = array[ 0..3 ] of integer;

const                                                      // normal       // flip H       // flip V       // flip HV
  FLIP_TEXCOORD : array[ 0..3 ] of TTextureCoordIndex = ( ( 0, 1, 2, 3 ), ( 1, 0, 3, 2 ), ( 3, 2, 1, 0 ), ( 2, 3, 0, 1 ) );

procedure TSimpleSurfaceWithEffect.DrawSurface(aTexture: PTexture; AFrameIndex: integer; const aLayerPercentOpacity: single);
var
  quad : array[ 0..3 ] of TPointF;
  tci  : PTextureCoordIndex;
  TexCoord : PTexCoor;
  AlphaTint:byte;
  i: integer;
  procedure DrawVertex ;
   begin
    glBegin( GL_QUADS );
      glTexCoord2fv( @TexCoord^[ tci^[ 0 ] ] );  glVertex2fv( @quad[ 0 ] );
      glTexCoord2fv( @TexCoord^[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @TexCoord^[ tci^[ 2 ] ] );  glVertex2fv( @quad[ 2 ] );
      glTexCoord2fv( @TexCoord^[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
   end;
begin
 if aTexture = nil then exit;
 if not FVisible then exit;

 if AFrameIndex < 0
   then AFrameIndex := 0
   else if AFrameIndex >= aTexture^.FrameCount
          then AFrameIndex := aTexture^.FrameCount-1;

 TexCoord := @aTexture^.FramesCoord[AFrameIndex];

 PushAndPrepareMatrix;

 quad[ 0 ].X := 0;  quad[ 0 ].Y := 0;
 quad[ 1 ].X := Width; quad[ 1 ].Y := 0;
 quad[ 2 ].X := Width; quad[ 2 ].Y := Height;
 quad[ 3 ].X := 0;  quad[ 3 ].Y := Height;

 // Skew
 quad[0].X += Skew.x.Value * Scale.x.Value;
 quad[0].Y += Skew.y.Value * Scale.y.Value;
 quad[1].X += Skew.x.Value * Scale.x.Value;
 quad[1].Y += Skew.y.Value * Scale.y.Value;

 // Flip
 i:=0;
 if FFlipH then inc( i );
 if FFlipV then inc( i, 2 );
 tci := @FLIP_TEXCOORD[ i ];

// glEnable(GL_BLEND);
 SetBlendMode( FBlendMode );

 TextureManager.BindTexture( aTexture );

 if Tint.alpha.Value < 255
   then begin
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
         glColor4ub(255,255,255,round(Opacity.Value * aLayerPercentOpacity));
         DrawVertex;
        end;

 if Tint.alpha.Value <> 0
   then begin  //Apply Tint
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
         glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
         glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
         AlphaTint:=round( Tint.alpha.Value * Opacity.Value / 255 * aLayerPercentOpacity );
         glColor4ub( round(Tint.red.Value), round(Tint.Green.Value), round(Tint.Blue.Value), AlphaTint);
         DrawVertex;
        end;

 TextureManager.DisableTextureUsage;
 {$IFDEF DEBUG_MODE_ON}
   DrawBox(X.Value, Y.Value, aTexture^.TextureWidth, aTexture^.TextureHeight, BGRA(255,0,0));
 {$ENDIF}

 DrawChildSurfaces( aLayerPercentOpacity*Opacity.pcValue );
 PopMatrix;
end;

procedure TSimpleSurfaceWithEffect.DrawSurface(const aTopLeftColor, aTopRightColor,
  aBottomRightColor, aBottomLeftColor: TBGRAPixel; const aLayerPercentOpacity: single);
var quad : array[ 0..3 ] of TPointF;
    a : Single ;
begin
 if not FVisible then exit;

 a := aLayerPercentOpacity * Opacity.Value / 255;

 PushAndPrepareMatrix;

 quad[ 0 ].X := 0;  quad[ 0 ].Y := 0;
 quad[ 1 ].X := Width; quad[ 1 ].Y := 0;
 quad[ 2 ].X := Width; quad[ 2 ].Y := Height;
 quad[ 3 ].X := 0;  quad[ 3 ].Y := Height;

 // Skew
 quad[0].X += Skew.x.Value * Scale.x.Value;
 quad[0].Y += Skew.y.Value * Scale.y.Value;
 quad[1].X += Skew.x.Value * Scale.x.Value;
 quad[1].Y += Skew.y.Value * Scale.y.Value;

 // Flip property is ignored

 SetBlendMode( FBlendMode );

 TextureManager.DisableTextureUsage;

 if Tint.alpha.Value < 255
   then begin
         glBegin( GL_QUADS );
           with aTopLeftColor do
             glColor4ub( red, green, blue, round(alpha * a) ); glVertex2fv( @quad[ 0 ] );
           with aTopRightColor do
             glColor4ub( red, green, blue, round(alpha * a) ); glVertex2fv( @quad[ 1 ] );
           with aBottomRightColor do
             glColor4ub( red, green, blue, round(alpha * a) ); glVertex2fv( @quad[ 2 ] );
           with aBottomLeftColor do
             glColor4ub( red, green, blue, round(alpha * a) ); glVertex2fv( @quad[ 3 ] );
        glEnd;
   end;

 if Tint.alpha.Value <> 0
   then begin  //Apply Tint
         glColor4ub( round(Tint.red.Value), round(Tint.Green.Value), round(Tint.Blue.Value), round(Tint.alpha.Value * a));
         glBegin( GL_QUADS );
           glVertex2fv( @quad[ 0 ] );
           glVertex2fv( @quad[ 1 ] );
           glVertex2fv( @quad[ 2 ] );
           glVertex2fv( @quad[ 3 ] );
         glEnd;
        end;

 {$IFDEF DEBUG_MODE_ON}
   DrawBox(X.Value, Y.Value, Width, Height, BGRA(255,0,0));
 {$ENDIF}

 DrawChildSurfaces( aLayerPercentOpacity*Opacity.pcValue );
 PopMatrix;
end;

procedure TSimpleSurfaceWithEffect.SetFrameLoopBounds( AStartFrameIndex, AEndFrameIndex: integer );
var temp: integer;
begin
 if AStartFrameIndex > AEndFrameIndex
   then begin
         temp := AStartFrameIndex;
         AStartFrameIndex := AEndFrameIndex;
         AEndFrameIndex := temp;
   end;

 FFrameLoopStart := AStartFrameIndex;
 FFrameLoopEnd := AEndFrameIndex;

 if not InRange(trunc(FFrame), AStartFrameIndex, AEndFrameIndex)
   then FFrame:= AStartFrameIndex;
end;

procedure TSimpleSurfaceWithEffect.FrameAddPerSecond(
  ADeltaFramePerSecond: single);
begin
 FFrameDeltaPerSecond := ADeltaFramePerSecond;
end;

procedure TSimpleSurfaceWithEffect.CenterOnScene;
begin
 if FParentScene<>NIL
   then SetCenterCoordinate( FParentScene.Width*0.5, FParentScene.Height*0.5);
end;

procedure TSimpleSurfaceWithEffect.CenterOnSurface(ASurface: TSimpleSurface);
begin
 SetCenterCoordinate( ASurface.CenterX, ASurface.CenterY );
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(aPoint: TPointF): boolean;
begin
 Result := GetScaledArea.Contains(Point(round(aPoint.x), round(aPoint.y)));
// Result := (aPoint.x>=FSurfaceOrigin.x) and (aPoint.x<=FSurfaceOrigin.x+Width) and
//           (aPoint.y>=FSurfaceOrigin.y) and (aPoint.y<=FSurfaceOrigin.y+Height);
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(aSurface: TSimpleSurfaceWithEffect): boolean;
begin
 Result := GetScaledArea.IntersectsWith( aSurface.GetScaledArea );
// CollisionTestWith( aSurface.GetScaledArea );

// Result := CollisionTestWith(aSurface.FSurfaceOrigin.x, aSurface.FSurfaceOrigin.y, aSurface.Width, aSurface.Height);
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(aSurface: TSimpleSurfaceWithEffect; HTolerance, VTolerance: integer): boolean;
var r: TRect;
begin
 r:= aSurface.GetScaledArea;
 r.Inflate(-HTolerance, -VTolerance, HTolerance, VTolerance);
 if (r.Width<=0) or (r.Height<=0) then begin
   Result := FALSE;
   exit;
 end;

 Result := GetScaledArea.IntersectsWith( r );

// Result := CollisionTestWith(aSurface.FSurfaceOrigin.x+HTolerance, aSurface.FSurfaceOrigin.y+VTolerance,
//                            aSurface.Width-HTolerance*2, aSurface.Height-VTolerance*2 );
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(const aX, aY, aWidth, aHeight: single ): boolean;
var r: TRect;
begin
 r.Create(Point(round(aX), round(aY)), round(aWidth), round( aHeight));
 Result := CollisionTestWith( r );
// Result := not (( aX > FSurfaceOrigin.x+Width ) or (aX+aWidth < FSurfaceOrigin.x) or
//              (aY > FSurfaceOrigin.y+Height) or ( aY+aHeight < FSurfaceOrigin.y));
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(aRect: TRect): boolean;
begin
 Result := GetScaledArea.IntersectsWith(aRect);
end;

function TSimpleSurfaceWithEffect.GetChild(aIndex: integer): TSimpleSurfaceWithEffect;
begin
 Result := TSimpleSurfaceWithEffect( FChilds.Items[aIndex] );
end;

function TSimpleSurfaceWithEffect.ChildCount: integer;
begin
 Result := FChilds.Count;
end;

function TSimpleSurfaceWithEffect.AddScenario(const aFilename: string; ACallBackEvent: TScenarioEvent): TIDScenario;
var o: TScenario ;
begin
 o := TScenario.Create;
 o.LoadFromFile( aFilename );
 o.CurrentLine := 0;
 o.OnEvent := ACallBackEvent;
 o.OnDecode:=FScenarioAdditionnalDecode;
 Result := AddScenario( o );
end;

function TSimpleSurfaceWithEffect.AddScenario(aScenario: TScenario ): TIDScenario;
begin
 aScenario.OnDecode:=FScenarioAdditionnalDecode;
 Result := Length( FListScenario );
 SetLength( FListScenario, Result + 1 );
 FListScenario[Result] := aScenario;
 aScenario.ParentSurface := Self;
 aScenario.ID := Result;
end;

procedure TSimpleSurfaceWithEffect.PlayScenario(AIDScenario: TIDScenario; AFromBeginning: boolean);
var s: TScenario;
begin
 s:= GetScenario(AIDScenario);
 if s = NIL then exit;

 if AFromBeginning then
 begin
  s.CurrentLine := 0;
  s.WaitTime := 0.0;
 end;

 s.FIsRunning:=TRUE;
end;

function TSimpleSurfaceWithEffect.AddAndPlayScenario(const aFilename: string;
  ACallBackEvent: TScenarioEvent): TIDScenario;
begin
 Result := AddScenario( aFilename, ACallBackEvent );
 PlayScenario( Result );
end;

function TSimpleSurfaceWithEffect.AddAndPlayScenario(aScenario: TScenario
  ): TIDScenario;
begin
 Result := AddScenario( aScenario );
 PlayScenario( Result );
end;

procedure TSimpleSurfaceWithEffect.StopScenario(AIDScenario: TIDScenario);
var s: TScenario;
begin
 s:= GetScenario( AIDScenario );
 if s <> NIL then s.FIsRunning := FALSE;
end;

procedure TSimpleSurfaceWithEffect.StopAllScenario;
var i: integer;
begin
 for i :=0 to Length( FListScenario )-1 do
  FListScenario[i].FIsRunning := FALSE;
end;

function TSimpleSurfaceWithEffect.ScenarioIsPlaying(AIDScenario: TIDScenario ): boolean;
var s: TScenario;
begin
 s:= GetScenario( AIDScenario );
 if s = NIL
   then Result := FALSE
   else Result := S.FIsRunning;
end;

procedure TSimpleSurfaceWithEffect.SetScenarioAdditionnalDecode(
  aCallbackDecode: TScenarioDecode);
begin
 FScenarioAdditionnalDecode := aCallbackDecode;
end;


procedure TSimpleSurfaceWithEffect.BindToSprite(aParentSurface: TSimpleSurface; aOffsetX, aOffsetY: single);
begin
 FBindToSprite.InProgress:=TRUE;
 FBindToSprite.ParentSurface:=aParentSurface;
 FBindToSprite.OffsetX:=aOffsetX;
 FBindToSprite.OffsetY:=aOffsetY;
end;

procedure TSimpleSurfaceWithEffect.BindCenterToSprite(
  aParentSurface: TSimpleSurface);
begin
 BindToSprite( aParentSurface,
        (aParentSurface.Width-Width)*0.5, (aParentSurface.Height-Height)*0.5 );
end;

procedure TSimpleSurfaceWithEffect.UnBind;
begin
 FBindToSprite.InProgress := FALSE;
end;

{ TScenario }

constructor TScenario.Create;
begin
 ID := -1 ;
 Actions := TStringList.Create ;
 CurrentLine := 0;
 WaitTime := 0.0 ;
 ParentSurface := NIL ;
 FScenarioEvent := NIL;
 FIsRunning := FALSE ;
end;

destructor TScenario.Destroy;
begin
 FreeAndNil(Actions);
end;

procedure TScenario.Execute(const aElapsedTime: double);
var OriginLine: integer;
begin
 if WaitTime > 0
   then WaitTime -= aElapsedTime ;
 if WaitTime <= 0.000
   then begin
         OriginLine := CurrentLine;
         repeat
          DecodeAction;
          inc(CurrentLine);
         until ( WaitTime > 0 ) or ( CurrentLine >= Actions.Count ) or
               not FIsRunning or ( OriginLine = CurrentLine ); // avoid infinite loop in buggy scenario
         if CurrentLine >= Actions.Count then FIsRunning := FALSE ;
   end;
end;

procedure TScenario.LoadFromFile(const aFilename: string);
begin
 Actions.Clear ;
 Actions.LoadFromFile( aFilename ) ;
end;

procedure TScenario.SaveToFile(const aFilename: string);
begin
 Actions.SaveToFile( aFilename ) ;
end;

procedure TScenario.DecodeAction;
var i: integer;
begin
 if Length(Actions.Strings[CurrentLine]) = 0 then exit;

 FTemp := SplitLineToStringArray( Actions.Strings[CurrentLine], ' ' );

 case FTemp.[0] of
  acWait : WaitTime := StrToFloat( FTemp[1] );
  acKill : ParentSurface.Kill;
  acFreeze : ParentSurface.Freeze := FTemp[1] = acTRUE ;
  acVisible : ParentSurface.Visible := FTemp[1] = acTRUE ;

  acOpacity : ParentSurface.Opacity.Value := strtoint( FTemp[1] );
  acOpacityChange : ParentSurface.Opacity.ChangeTo( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] ) );

  acTint : ParentSurface.Tint.Value := BGRA( strtoint( FTemp[1] ), strtoint( FTemp[2] ), strtoint( FTemp[3] ), strtoint( FTemp[4] ) );
  acTintChange : ParentSurface.Tint.ChangeTo( BGRA( strtoint( FTemp[1] ),
                                                    strtoint( FTemp[2] ),
                                                    strtoint( FTemp[3] ),
                                                    strtoint( FTemp[4] )),
                                                    StrToFloat( FTemp[5] ),
                                                    StringToCurveID( FTemp[6] ));
  acTintRedChange: ParentSurface.Tint.Red.ChangeTo( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] ));
  acTintGreenChange: ParentSurface.Tint.Green.ChangeTo( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] ));
  acTintBlueChange: ParentSurface.Tint.Blue.ChangeTo( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] ));
  acTintAlphaChange : ParentSurface.Tint.Alpha.ChangeTo( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] ));

  acAngle : ParentSurface.Angle.Value := StrToFloat( FTemp[1] ) ;
  acRotate : ParentSurface.Angle.AddConstant( StrToFloat( FTemp[1] ) );
  acRotateTo : ParentSurface.Angle.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] )  );
  acRotationAroundAxis : ParentSurface.RotationAroundPoint( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StrToFloat( FTemp[3] ), FTemp[4] = acTRUE );

  acSkew : ParentSurface.Skew.ChangeTo( PointF( strtoint( FTemp[1] ), strtoint( FTemp[2] ) ), StrToFloat( FTemp[3] ),  StringToCurveID( FTemp[4]  ) );

  acScale : ParentSurface.Scale.Value := PointF( StrToFloat( FTemp[1] ), StrToFloat( FTemp[1] ) );
  acScaleChange : begin
   ParentSurface.Scale.x.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
   ParentSurface.Scale.y.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  end;
  acScaleH : ParentSurface.Scale.x.Value := StrToFloat( FTemp[1] );
  acScaleHChange : ParentSurface.Scale.x.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  acScaleV : ParentSurface.Scale.y.Value := StrToFloat( FTemp[1] );
  acScaleVChange : ParentSurface.Scale.y.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));

  acBlink: ParentSurface.Blink( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ), StrToFloat( FTemp[3] ) );
  acStopBlink: ParentSurface.StopBlink;

  acFlipH: ParentSurface.FlipH := FTemp[1] = acTRUE ;
  acFlipV: ParentSurface.FlipV := FTemp[1] = acTRUE ;
  acToggleFlipH: ParentSurface.FlipH := not ParentSurface.FlipH;
  acToggleFlipV: ParentSurface.FlipV := not ParentSurface.FlipV;

  acAnimate: begin
              ParentSurface.SetFrameLoopBounds( strtoint( FTemp[1] ), strtoint( FTemp[2] ) );
              ParentSurface.FrameAddPerSecond( StrToFloat( FTemp[3] ));
  end;
  acIncFrame : ParentSurface.Frame := ParentSurface.Frame + 1;
  acDecFrame : ParentSurface.Frame := ParentSurface.Frame - 1;
  acSetFrame : ParentSurface.Frame := strtoint( FTemp[1] );


  acMoveTo  : ParentSurface.MoveTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StrToFloat( FTemp[3] ), StringToCurveID( FTemp[4] ));
  acMoveXTo : ParentSurface.X.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  acMoveYTo : ParentSurface.Y.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));

  acMoveCenterTo  : ParentSurface.MoveCenterTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StrToFloat( FTemp[3] ), StringToCurveID( FTemp[4] ) );
  acMoveXCenterTo : ParentSurface.MoveXCenterTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ) );
  acMoveYCenterTo : ParentSurface.MoveYCenterTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ) );

  acMoveRelative   : ParentSurface.MoveRelative( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StrToFloat( FTemp[3] ), StringToCurveID( FTemp[4] ));
  acMoveXRelative  : ParentSurface.MoveXRelative( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  acMoveYRelative  : ParentSurface.MoveYRelative( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));

  acSetCoor : begin
    ParentSurface.X.Value := StrToFloat( FTemp[1] );
    ParentSurface.Y.Value := StrToFloat( FTemp[2] );
  end;
  acSetCenterCoor : ParentSurface.SetCenterCoordinate( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ));

  acCenterOnScene: ParentSurface.CenterOnScene;

  acSendEvent : if FScenarioEvent <> NIL then FScenarioEvent( ParentSurface, ID, strtoint( FTemp[1] ) );

  acLoop : CurrentLine := -1 ;
  acLabel :;
  acGotoLabel : begin
   i := Actions.IndexOf( acLabel + ' ' + FTemp[1] );
   if i <> -1 then CurrentLine := i - 1;
  end;
  acEnableGui: ParentSurface.EnableGui;
  acDisableGui: ParentSurface.DisableGui

  else if FScenarioDecodeProc<>NIL then FScenarioDecodeProc(FTemp); //DecodeAdditionnalAction( FTemp );
 end;//case
end;

procedure TScenario.AddLine(const aLine: string);
begin
 Actions.Add( aLine );
end;


{ TSimpleSurface }

constructor TSimpleSurface.Create;
begin
 X := TFParam.Create;
 Y := TFParam.Create;

 FVisible := TRUE ;
 FKill := FALSE ;
 FIndex := 0 ;
 FTag1 := 0 ;
 FTag2 := FALSE ;

 FParentLayer := NIL;
 FParentScene := NIL;
end;

destructor TSimpleSurface.Destroy;
begin
 FreeAndNil( X );
 FreeAndNil( Y );
 inherited Destroy;
end;

function TSimpleSurface.GetCenterX: single;
begin
 Result := x.Value + GetWidth * 0.5 ;
end;

function TSimpleSurface.GetRightX: single;
begin
 Result := x.Value + GetWidth - 1 ;
end;

function TSimpleSurface.GetBottomY: single;
begin
 Result := y.Value + GetHeight -1 ;
end;

function TSimpleSurface.GetCenterY: single;
begin
 Result := y.Value + GetHeight * 0.5 ;
end;

procedure TSimpleSurface.SetCenterX(const AValue: single);
begin
 x.Value := AValue - GetWidth * 0.5 ;
end;

procedure TSimpleSurface.SetRightX(const AValue: single);
begin
 x.Value := AValue - GetWidth - 1 ;
end;

procedure TSimpleSurface.SetBottomY(const AValue: single);
begin
 y.Value := AValue - GetHeight - 1 ;
end;

procedure TSimpleSurface.SetCenterY(const AValue: single);
begin
 y.Value := AValue - GetHeight * 0.5 ;
end;

function TSimpleSurface.GetRectArea: TRect;
begin
 Result.Create( Point(round(X.Value), round(Y.Value)), Width, Height );
end;

function TSimpleSurface.GetXY: TPointF;
begin
 Result := PointF( X.Value, Y.Value );
end;

function TSimpleSurface.Center: TPointF;
begin
  Result := PointF(CenterX, CenterY);
end;

procedure TSimpleSurface.Kill;
begin
 FKill := TRUE;
end;

procedure TSimpleSurface.SetCoordinate(aX, aY: single);
begin
 x.Value := aX;
 Y.Value := aY;
end;

procedure TSimpleSurface.SetCoordinate(aP: TPointF);
begin
 x.Value := aP.x;
 y.Value := aP.y;
end;

procedure TSimpleSurface.SetCenterCoordinate(aCenterX, aCenterY:single);
begin
 SetCenterX( aCenterX );
 SetCenterY( aCenterY );
end;

procedure TSimpleSurface.SetCenterCoordinate(aP: TPointF);
begin
 SetCenterX( aP.x );
 SetCenterY( aP.y );
end;






{$endif oglcIMPLEMENTATION}


