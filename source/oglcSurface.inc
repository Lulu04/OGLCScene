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
 procedure Draw(const aLayerPercentOpacity: single); virtual; abstract;
 procedure Update(const AElapsedTime: single); virtual; abstract;
public
 X, Y: TFParam;
 // the rectangle area of the surface
 function GetRectArea: TRect;
 // TopLeft coordinates of the surface
 function GetXY: TPointF;
 // Center coordinates of the surface
 function Center: TPointF;
 // remove the surface from the scene and free it
 procedure Kill;
 // Coordinates
 procedure SetCoordinate(aX, aY: single);overload;
 procedure SetCoordinate(aP: TPointF);overload;
 procedure SetCenterCoordinate(aCenterX, aCenterY : single);overload;
 procedure SetCenterCoordinate(aP: TPointF);overload;
 property CenterX: single read GetCenterX write SetCenterX;
 property RightX: single read GetRightX write SetRightX;
 property CenterY: single read GetCenterY write SetCenterY;
 property BottomY: single read GetBottomY write SetBottomY;
 // Size
 property Width: integer read GetWidth;
 property Height: integer read GetHeight;

 property Visible: boolean read FVisible write FVisible; // =TRUE set the surface visible on scene
 property Tag1: integer read FTag1 write FTag1;
 property Tag2: boolean read FTag2 write FTag2;
 property Group: integer read FGroup write FGroup;
 property ParentLayer: TLayer read FParentLayer write FParentLayer;
 property ParentScene: TOGLCScene read FParentScene write FParentScene;
end;

TScenarioDecode = procedure(const aActions: TStringArray) of object;
{ TScenario }

TScenario = class
private
  ID: integer;
  Actions: TStringlist;
  CurrentLine: integer;
  WaitTime: single;
  ParentSurface: TSimpleSurfaceWithEffect;
  FIsRunning: boolean;
  FRunError: boolean;
  FKilled: boolean;
  FTemp: TStringArray;
  FScenarioDecodeProc: TScenarioDecode;
  procedure Execute(const aElapsedTime: single);
  procedure LoadFromFile(const aFilename: string);
  procedure DecodeAction; virtual;
public
  Constructor Create;
  Destructor Destroy; override;
  // aLines is a string that contains the actions to execute,
  // one action per line and each line separated by #10
  procedure InitWith(const aLines: string);
  procedure AddLine(const aLine: string);  // to construct the scenario manually
  procedure Stop;
  procedure Kill;
  // this callback is only called when an action has not been recognized
  property OnDecode: TScenarioDecode read FScenarioDecodeProc write FScenarioDecodeProc;
  // True if an error occurs while running the scenario.
  property RunError: boolean read FRunError;
end;

{ TSimpleSurfaceWithEffect }

TSimpleSurfaceWithEffect = class(TSimpleSurface)
  Constructor Create;
  Destructor Destroy; override;
private
  FMessageList: TMessageList;
  FEnableProcessMessage: boolean;
private
  FPreviousModelViewMatrix: TOGLCMatrix;
  function GetScaledAreaF: TRectF;
  function GetScaledBottomY: single;
  function GetScaledHeight: single;
  function GetScaledRightX: single;
  function GetScaledWidth: single;
  function GetScaledX: single;
  function GetScaledY: single;
  procedure SetFrame(AValue: single);
  procedure SetScaledAreaF(AValue: TRectF);
  procedure SetScaledBottomY(AValue: single);
  procedure SetScaledHeight(AValue: single);
  procedure SetScaledRightX(AValue: single);
  procedure SetScaledWidth(AValue: single);
  procedure SetScaledX(AValue: single);
  procedure SetScaledY(AValue: single);
protected
  FChilds: TList;
  FParentSurface: TSimpleSurfaceWithEffect;
  FChildsUseParentOpacity: boolean;
  FApplySymmetryWhenFlip: boolean;
  FReferenceSurfaceForSymmetry: TSimpleSurfaceWithEffect;
  FSurfaceOrigin: TPointF;
  FZOrderAsChild: integer; // z order in case the surface is a child because child can be below or above its parent surface.
   function GetChild(aIndex: integer): TSimpleSurfaceWithEffect;
  procedure SetParentSurface(aSurface: TSimpleSurfaceWithEffect);
  procedure SetParentScene( aScene: TOGLCScene ); virtual;
  procedure SetParentLayer( aLayer: TLayer );
protected
  FBlendMode: byte;
  FComputedOpacity: single;
  FComputedTint: TColorF;
  FFreeze: boolean;
  FTimeBeforeKill: single;
  // Blink
  FBlink: TBlink;
  // Rotation around axis
  FRotationAroundPt: TRotationAroundPoint;
  // Flip
  FFlipH, FFlipV: boolean;
  // Frame animation
  FFrame: single;
  FFrameDeltaPerSecond: single;
  FFrameLoopStart,
  FFrameLoopEnd: integer;
  // Bind Surface
  FBindToSprite: TBindToSprite;
private
  FListScenario: array of TScenario;
  FScenarioAdditionnalDecode: TScenarioDecode;
  function GetFirstScenarioFreeID: integer;
  function GetScenario(aID: TIDScenario): TScenario;
  function DoAddScenarioToList(aScenario: TScenario): TIDScenario;
private
  FCollisionBody: TOGLCCollisionBody;
protected
  function FlipToIndex: integer; inline;
  procedure PushAndPrepareSceneMV; inline;
  procedure PopSceneMV; inline;
  procedure DrawChildSurfacesWithPositiveZOrder(const aLayerPercentOpacity: single); virtual;
  procedure DrawChildSurfacesWithNegativeZOrder(const aLayerPercentOpacity: single); virtual;
  procedure SetFlipH(AValue: boolean); virtual;
  procedure SetFlipV(AValue: boolean); virtual;
public
  // called every frames.
  procedure Update(const aElapsedTime: single); override;
  procedure Draw(const aLayerPercentOpacity: single);  override;
  procedure DoDraw; virtual; abstract;
public
  Speed   : TPointFParam;  // the speed of the surface in pixels/sec, on both axis
  Angle   : TBoundedFParam; // the angle to rotate the surface CW. Range is -360° to 359.999°
  Pivot   : TPointF; // percent position of the center rotation. (0,0) mean topleft, (0.5,0.5) mean the center, (1,1) mean bottomright.
  Scale   : TPointFParam; // the scale values on both axis. (1,1) means no scaling (default)
  Opacity : TBoundedFParam; // the opacity of the surface, range is [0..255]
  Tint    : TBGRAParam; // the tint applyed to the surface. Alpha channel controls the amount.
  TintMode: TTintMode; // Can be tmMixColor or tmReplaceColor. Default value is tmReplaceColor.
  // To flip horizontally the surface
  property FlipH: boolean read FFlipH write SetFlipH;
  // To flip vertically the surface
  property FlipV: boolean read FFlipV write SetFlipV;
  // All the surface properties that change throught time (like Tint, Angle, Scale, X, Y...)
  // are freezed if you sets this property to True.
  property Freeze: boolean read FFreeze write FFreeze ;  // set to TRUE to stop all variations on surface ( move, opacity change, scale change, etc...)
  // You can change the blend mode for this surface.
  // Possible value are: FX_BLEND_NORMAL, FX_BLEND_ADD, FX_BLEND_MULT, FX_NOBLEND
  property BlendMode: byte read FBlendMode write FBlendMode;
public // messages
  // Post a message to the surface itself.
  // The delay allows to process the message only after a lapse of time (in seconds): it's usefull to construct chained animation.
  // if aDelay is 0, the message is posted and will be processed into the next frame.
  // If delay is > 0, the message is posted and will be processed only after the lapse of time expressed by aDelay (in seconds).
  // When a message is ready to be processed, method ProcessMessage is called with the message user value.
  procedure PostMessage(UserValue: TUserMessageValue; aDelay: single=0);
  // Override to process the message received.
  procedure ProcessMessage({%H-}UserValue: TUserMessageValue); virtual;
  // Clear the message list
  procedure ClearMessageList;
  // Enable or disable messages processing. Messages can always be posted.
  property ProcessMessageEnabled: boolean read FEnableProcessMessage write FEnableProcessMessage;

public // transformation matrix
  function GetMatrix: TOGLCMatrix;
  function GetInverseMatrix: TOGLCMatrix;
public  // utils for scaled surface
  function GetScaledArea: TRect;
  property ScaledAreaF: TRectF read GetScaledAreaF write SetScaledAreaF;
  property ScaledWidth: single read GetScaledWidth write SetScaledWidth;
  property ScaledHeight: single read GetScaledHeight write SetScaledHeight;
  property ScaledX: single read GetScaledX write SetScaledX;
  property ScaledY: single read GetScaledY write SetScaledY;
  property ScaledRightX: single read GetScaledRightX write SetScaledRightX;
  property ScaledBottomY: single read GetScaledBottomY write SetScaledBottomY;
public
  // Kill defered
  procedure KillDefered(ASec: single);
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
  // aNumberOfBlink=-1 for infinite blink
  procedure Blink( aNumberOfBlink: integer; aVisibleTime, aInvisibleTime: single );
  procedure StopBlink;
  // Rotate around point
  procedure RotationAroundPoint( aXPt, aYPt: single; aAnglePerSecond: single; aSelfRotate: boolean=TRUE ) ; overload;
  procedure RotationAroundPoint( aPt: TPointF; aAnglePerSecond: single; aSelfRotate: boolean=TRUE ) ; overload;
  procedure StopRotationAroundPoint;
  // Bind this surface to another
  procedure BindToSprite(aTargetSurface:TSimpleSurface; aOffsetX, aOffsetY: single);
  // Bind the center of this surface to the center of another
  procedure BindCenterToSprite(aTargetSurface:TSimpleSurface);
  procedure UnBind;
public  // Scenario
  function AddScenarioFromFile(const aFilename: string): TIDScenario; overload;
  function AddScenario(const aLines: string): TIDScenario;
  procedure PlayScenario(aID: TIDScenario; aFromBeginning: boolean=TRUE);
  function AddAndPlayScenarioFromFile(const aFilename: string): TIDScenario;
  function AddAndPlayScenario(const aLines: string): TIDScenario;
  procedure ReplaceScenario(aID: TIDScenario; const aLines: string);
  procedure ReplaceScenarioFromFile(aID: TIDScenario; const aFilename: string);
  procedure StopScenario(aID: TIDScenario);
  procedure StopAllScenario;
  procedure DeleteScenario(aID: TIDScenario);
  function ScenarioIsPlaying(aID: TIDScenario): boolean;
  // set a callbak proc to decode custom additionnal actions.
  procedure SetScenarioAdditionnalDecode(aCallbackDecode: TScenarioDecode);
public // Frame animation
  // You can set the range of frame index to use to animate the surface.
  // A texture frame start from 1 because 0 means the whole texture.
  procedure SetFrameLoopBounds(AStartFrameIndex, AEndFrameIndex: integer);
  // Specify how many frame must be added to the frame index per second to perform animation.
  procedure FrameAddPerSecond(ADeltaFramePerSecond: single);
  // You can set/get the frame index. Frame index start at 1.
  property Frame: single read FFrame write SetFrame;
public // utils
  // Center the surface on its parent scene
  procedure CenterOnScene;
  // Center the surface on another surface
  procedure CenterOnSurface(ASurface: TSimpleSurface);
  // Center the surface on its parent
  procedure CenterOnParent;
public // Local <-> World
  // Transform local coordinates to scene coordinates.
  // (take in acount the camera assigned to the parent layer of the surface)
  function SurfaceToScene(aPt: TPointF): TPointF;
  // Transform scene coordinates to local coordinates.
  // (take in acount the camera assigned to the parent layer of the surface)
  function SceneToSurface(aPt: TPointF): TPointF;
  // Transform local coordinates to world coordinates.
  // (don't take in acount the camera assigned to the parent layer of the surface)
  function SurfaceToWorld(aPt: TPointF): TPointF;
  function WorldToSurface(aPt: TPointF): TPointF;
  function SurfaceToParent(aPt: TPointF; aParent: TSimpleSurfaceWithEffect=NIL): TPointF;
  function ParentToSurface(aPt: TPointF; aParent: TSimpleSurfaceWithEffect=NIL): TPointF;
  // Apply surface transformations (translation, scale, rotation+pivot) on the given matrix.
  procedure AddTransformTo(var aMatrix: TOGLCMatrix);
  // Gives a matrix to transform surface coordinates to scene coordinates.
  // (take in acount the camera assigned to the parent layer of the surface)
  function GetMatrixSurfaceToScene: TOGLCMatrix;
  // Gives a matrix to transform surface coordinates to world coordinates.
  // (don't take in acount the camera assigned to the parent layer of the surface)
  function GetMatrixSurfaceToWorld: TOGLCMatrix;
  // Returns the rectangular area that contains the transformed surface in scene coordinate.
  function GetRectAreaInSceneSpace(aGiveRectThatContainRotatedBounds: boolean): TRectF; deprecated 'use GetQuadAreaInSceneSpace.Bounds instead';
  // Returns the rectangular area that contains the transformed surface area in world coordinate.
  function GetRectAreaInWorldSpace: TRectF; deprecated 'use GetQuadAreaInWorldSpace.Bounds instead';
  // Return a Quad that represent the transformed rectangular surface area in world coordinate.
  function GetQuadAreaInWorldSpace: TQuadF;
  // Return a Quad that represent the transformed rectangular surface area in scene coordinate.
  function GetQuadAreaInSceneSpace: TQuadF;
  // The rectangular area (single) of the surface expressed in the surface space (TopLeft=0,0)
  // and without applying model view matrix.
  function GetRectAreaInLocalSpace: TRectF;
  // The rectangular area of the surface expressed in the parent space (TopLeft=X.Value,Y.Value)
  function GetRectAreaInParentSpace: TRectF;
public // Collision
  // Collision test
  function CollisionTestWith(aPoint: TPointF): boolean; overload; deprecated 'use property CollisionBody instead';
  function CollisionTestWith(aSurface: TSimpleSurfaceWithEffect): boolean; overload; deprecated 'use property CollisionBody instead';
  function CollisionTestWith(aSurface: TSimpleSurfaceWithEffect; HTolerance, VTolerance: integer): boolean; overload; deprecated 'use property CollisionBody instead';
  function CollisionTestWith(const aX, aY, aWidth, aHeight: single): boolean; overload; deprecated 'use property CollisionBody instead';
  function CollisionTestWith(aRect: TRectF): boolean; overload; deprecated 'use property CollisionBody instead';
  function CollisionTestWith(aRect: TRect): boolean; overload; deprecated 'use property CollisionBody instead';
  property CollisionBody: TOGLCCollisionBody read FCollisionBody;
public // Childs
  // add a child to the surface.
  // a positive aZOrder means the child is above its parent, a negative aZOrder means the child is below its parent.
  // The drawing order is: ...-3 -2 -1 parent 0 1 2 3...
  procedure AddChild(ASurface: TSimpleSurfaceWithEffect; aZOrder: integer=0); virtual;
  // set the ZOrder of the surface. Do nothing if the surface don't have a parent.
  // The drawing order is: ...-3 -2 -1 parent 0 1 2 3...
  procedure SetZOrder(aZOrder: integer);
  procedure DeleteChild(aIndex: integer); overload;
  procedure DeleteChild(aSurface: TSimpleSurfaceWithEffect); overload;
  procedure DeleteAllChilds; virtual;
  // Remove the child without freeing it. Its coordinates are not modified.
  procedure RemoveChild(aSurface: TSimpleSurfaceWithEffect);
  // Remove the child dependency and add the surface to the scene. Do nothing if the surface don't have a parent.
  procedure MoveFromChildToScene(aLayerIndex: integer);
  // Remove the surface (self) from its layer and add it as child of the specified one.
  procedure MoveFromSceneToChildOf(aParentSurface: TSimpleSurfaceWithEffect; aZOrder: integer);
  // If any, remove current child dependency and add the surface as child of another.
  procedure SetChildOf(aParentSurface: TSimpleSurfaceWithEffect; aZOrder: integer);
  // Extract the surface from its owner (another layer or parent surface)
  // then add it to a layer.
  procedure MoveToLayer(aLayerIndex: integer);
  // Extract the surface from its owner (another layer or parent surface)
  // then insert it to a layer at the specified index.
  procedure InsertToLayer(aSurfaceIndex, aLayerIndex: integer);
  // Extract the surface from the scene. The surface can be on a layer or child of another surface.
  // The surface is not freed, its coodinates are not modified.
  procedure RemoveFromScene;
  // if the surface is child of another, its Z order is changed so that is on top of the other childs.
  // if the surface is on layer, it is moved to be on top of the other surface in this layer.
  procedure MoveToTop;
  procedure MoveToTopOneStep;
  // if the surface is child of another, its Z order is changed so that is below the other childs.
  // if the surface is on layer, it is moved to be below the other surface in this layer.
  procedure MoveBack;
  procedure MoveBackOneStep;
  function IsParentOf(aSurface: TSimpleSurfaceWithEffect): boolean;
  function ChildCount: integer;
  function IsChild(aSurface: TSimpleSurfaceWithEffect): boolean;
  property Childs[index:integer]: TSimpleSurfaceWithEffect read GetChild;
  property ParentSurface: TSimpleSurfaceWithEffect read FParentSurface;
  property ChildsUseParentOpacity: boolean read FChildsUseParentOpacity write FChildsUseParentOpacity; // default is True
  // if this surface is a child, you can change its Z order.
  // a positive value means the surface is above its parent, a negative value means the surface is below its parent.
  // The drawing order is: ...-3 -2 -1 parent 0 1 2 3...
  // ZOrderAsChild is ignored if the surface is not a child of another.
  property ZOrderAsChild: integer read FZOrderAsChild write SetZOrder;
  // if this surface is a child, sets this property to True to apply an horizontal and/or vertical symmetry
  // on its coordinates when its parent is flipped.
  // Don't sets this property to True if the surface is not a child of another surface.
  // NOTE: If you change the parent FlipH/FlipV property, you must change it also on the concerned childs by yourself.
  property ApplySymmetryWhenFlip: boolean read FApplySymmetryWhenFlip write FApplySymmetryWhenFlip;
public  // convenient function
  function CreateSpriteChild(aTexture: PTexture; aOwner: boolean=False; aZOrder: integer=0): TSprite;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

function CompareSurfaceChildsByZOrder(Item1, Item2: Pointer): Integer;
begin
  if TSimpleSurfaceWithEffect(Item1).FZOrderAsChild = TSimpleSurfaceWithEffect(Item2).FZOrderAsChild then Result := 0
  else if TSimpleSurfaceWithEffect(Item1).FZOrderAsChild > TSimpleSurfaceWithEffect(Item2).FZOrderAsChild then Result := 1
  else Result := -1;
end;

{ TSimpleSurfaceWithEffect }

constructor TSimpleSurfaceWithEffect.Create;
begin
  inherited Create;
  FBlendMode := FX_BLEND_NORMAL;
  FFreeze := FALSE ;
  FTimeBeforeKill := 0.0;

  Speed := TPointFParam.Create;

  Angle:= CreateBoundedFParam(-360, 359.9999, TRUE); //CreateBoundedFParam(0, 359.9999, TRUE);
  Angle.Value := 0;
  Pivot := PointF(0.5, 0.5);

  Opacity := CreateBoundedFParam(0, 255);
  Opacity.Value := 255;

  Tint := TBGRAParam.Create;
  Tint.Value := BGRA(0,0,0,0);
  TintMode := tmReplaceColor;

  Scale := TPointFParam.Create;
  Scale.Value := PointF(1, 1);

  // Blink
  FBlink.BlinkToGo := 0;

  // Rotation Around Axis
  FRotationAroundPt.InProgress := FALSE ;

  FFlipH := FALSE;
  FFlipV := FALSE;

  FBindToSprite.InProgress:=FALSE;
  FBindToSprite.TargetSurface:=NIL;

  FFrame := 0;
  FFrameDeltaPerSecond := 0.0;
  FFrameLoopStart := 0;
  FFrameLoopEnd := 0;

  FChilds := TList.Create;
  FParentSurface := NIL;
  FChildsUseParentOpacity := True;

  FMessageList := TMessageList.Create(@ProcessMessage);
  FEnableProcessMessage:=TRUE;

  FCollisionBody := TOGLCCollisionBody.Create(Self);
end;

procedure TSimpleSurfaceWithEffect.SetFrame(AValue: single);
begin
  FFrame := AValue;
end;

procedure TSimpleSurfaceWithEffect.SetScaledAreaF(AValue: TRectF);
begin
  ScaledWidth:= AValue.Width;
  ScaledHeight := AValue.Height;
  ScaledX := AValue.Left;
  ScaledY := AValue.Top;
end;

procedure TSimpleSurfaceWithEffect.SetScaledBottomY(AValue: single);
begin
  CenterY := AValue - GetScaledHeight*0.5;
end;

procedure TSimpleSurfaceWithEffect.SetScaledHeight(AValue: single);
begin
  if Height <> 0 then Scale.y.Value := AValue/Height;
end;

procedure TSimpleSurfaceWithEffect.SetScaledRightX(AValue: single);
begin
  CenterX := AValue - GetScaledWidth*0.5;
end;

procedure TSimpleSurfaceWithEffect.SetScaledWidth(AValue: single);
begin
  if Width <> 0 then Scale.x.Value := AValue/Width;
end;

procedure TSimpleSurfaceWithEffect.SetScaledX(AValue: single);
begin
  CenterX := AValue + GetScaledWidth*0.5;
end;

procedure TSimpleSurfaceWithEffect.SetScaledY(AValue: single);
begin
  CenterY := AValue + GetScaledHeight*0.5;
end;

function TSimpleSurfaceWithEffect.GetScaledAreaF: TRectF;
var xx, yy, ww, hh: single;
begin
  ww := Width*Scale.x.Value;
  hh := Height*Scale.y.Value;
  xx := FSurfaceOrigin.x-ww*0.5;
  yy := FSurfaceOrigin.y-hh*0.5;
  Result := RectF(xx, yy, xx+ww, yy+hh);
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
    GetChild(i).SetParentScene(aScene);
end;

procedure TSimpleSurfaceWithEffect.SetParentLayer(aLayer: TLayer);
var i: integer;
begin
  FParentLayer := aLayer;
  for i:=0 to ChildCount-1 do
    GetChild(i).SetParentLayer(aLayer);
end;

function TSimpleSurfaceWithEffect.FlipToIndex: integer;
begin
  Result := 0;
  if FFlipH then inc(Result);
  if FFlipV then inc(Result, 2);
end;

function TSimpleSurfaceWithEffect.GetFirstScenarioFreeID: integer;
var i: integer;
begin
  // looks for an empty slot
  for i:=0 to High(FListScenario) do
    if FListScenario[i] = NIL then exit(i);

  // not found -> create a new item
  Result := Length(FListScenario);
  SetLength(FListScenario, Result+1);
  FListScenario[Result] := NIL;
end;

function TSimpleSurfaceWithEffect.GetScenario(aID: TIDScenario ): TScenario;
begin
  if (aID > -1) and (aID < Length(FListScenario))
    then Result := FListScenario[aID]
    else Result := NIL;
end;

function TSimpleSurfaceWithEffect.DoAddScenarioToList(aScenario: TScenario): TIDScenario;
begin
  Result := GetFirstScenarioFreeID;
  FListScenario[Result] := aScenario;
  aScenario.ParentSurface := Self;
  aScenario.OnDecode := FScenarioAdditionnalDecode;
  aScenario.ID := Result;
  aScenario.CurrentLine := 0;
end;

procedure TSimpleSurfaceWithEffect.SetFlipV(AValue: boolean);
begin
  FFlipV := AValue;
end;

procedure TSimpleSurfaceWithEffect.SetFlipH(AValue: boolean);
begin
  FFlipH := AValue;
end;

procedure TSimpleSurfaceWithEffect.PushAndPrepareSceneMV;
begin
  FPreviousModelViewMatrix.CopyFrom(FParentScene.ModelViewMatrix);
  AddTransformTo(FParentScene.ModelViewMatrix);
end;

procedure TSimpleSurfaceWithEffect.PopSceneMV;
begin
  FParentScene.ModelViewMatrix.CopyFrom(FPreviousModelViewMatrix);
end;

procedure TSimpleSurfaceWithEffect.AddTransformTo(var aMatrix: TOGLCMatrix);
var xx, yy, pivotX, pivotY, ang: single;
begin
  if FApplySymmetryWhenFlip then begin
    // we apply a symmetry if a flip H or V is active
    if FParentSurface = FReferenceSurfaceForSymmetry then begin
      if FFlipH then xx := -X.Value-Width else xx := X.Value;
      if FFlipV then yy := -Y.Value-Height else yy := Y.Value;
    end else begin
     if FFlipH then xx := FParentSurface.Width-X.Value-Width else xx := X.Value;
     if FFlipV then yy := FParentSurface.Height-Y.Value-Height else yy := Y.Value;
    end;
    // pivot and angle must also be adjusted for the symmetry
    if FFlipH then begin
      pivotX := Width*(1-Pivot.x);
      ang := -Angle.Value;
    end else begin
      pivotX := Width*Pivot.x;
      ang := Angle.Value;
    end;
    if FFlipV then begin
      pivotY := Height*(1-Pivot.y);
      ang := -ang;
    end else pivotY := Height*Pivot.y;
    aMatrix.AddTransform(xx, yy, Scale.x.Value, Scale.y.Value, ang, PivotX, pivotY);
  end else
    aMatrix.AddTransform(X.Value, Y.Value, Scale.x.Value, Scale.y.Value,
                         Angle.Value, Width*Pivot.x, Height*Pivot.y);
end;

function TSimpleSurfaceWithEffect.GetMatrixSurfaceToScene: TOGLCMatrix;
  procedure ScanMatrixRecursive(aSurface: TSimpleSurfaceWithEffect; var m: TOGLCMatrix);
  begin
    if aSurface.ParentSurface <> NIL then ScanMatrixRecursive(aSurface.ParentSurface, m)
    else if (aSurface.ParentLayer <> NIL) and (aSurface.ParentLayer.Camera <> NIL) then
          aSurface.ParentLayer.Camera.AddTransformTo(m); // we add first the layer camera transformation

    aSurface.AddTransformTo(m);
  end;
begin
  Result.LoadIdentity;
  ScanMatrixRecursive(Self, Result);
end;

function TSimpleSurfaceWithEffect.GetMatrixSurfaceToWorld: TOGLCMatrix;
  procedure ScanMatrixRecursive(aSurface: TSimpleSurfaceWithEffect; var m: TOGLCMatrix);
  begin
    if aSurface.ParentSurface <> NIL then ScanMatrixRecursive(aSurface.ParentSurface, m);
    aSurface.AddTransformTo(m);
  end;
begin
  Result.LoadIdentity;
  ScanMatrixRecursive(Self, Result);
end;

function TSimpleSurfaceWithEffect.GetRectAreaInSceneSpace(
  aGiveRectThatContainRotatedBounds: boolean): TRectF;
var tl,tr,bl,br, mini, maxi: TPointF;
    m: TOGLCMatrix;
  function GetMin(const v1, v2: single): single; inline;
  begin
    if v1 < v2 then Result := v1 else Result := v2;
  end;
  function GetMax(const v1, v2: single): single; inline;
  begin
    if v1 > v2 then Result := v1 else Result := v2;
  end;
begin
  if aGiveRectThatContainRotatedBounds then begin
    m := GetMatrixSurfaceToScene;
    tl := m.Transform(PointF(0, 0));
    tr := m.Transform(PointF(Width, 0));
    br := m.Transform(PointF(Width, Height));
    bl := m.Transform(PointF(0, Height));

    mini := PointF(GetMin(tl.x, GetMin(tr.x, GetMin(bl.x, br.x))),
                   GetMin(tl.y, GetMin(tr.y, GetMin(bl.y, br.y))));
    maxi := PointF(GetMax(tl.x, GetMax(tr.x, GetMax(bl.x, br.x))),
                   GetMax(tl.y, GetMax(tr.y, GetMax(bl.y, br.y))));

    Result.Left := mini.x;
    Result.Top := mini.y;
    Result.Right := maxi.x;
    Result.Bottom := maxi.y;
  end
  else Result := GetMatrixSurfaceToScene.Transform(GetRectAreaInLocalSpace);
end;

function TSimpleSurfaceWithEffect.GetRectAreaInWorldSpace: TRectF;
begin
  {Result.TopLeft := SurfaceToWorld(PointF(0, 0));
  Result.BottomRight := SurfaceToWorld(PointF(Width, Height));  }
  Result := GetQuadAreaInWorldSpace.Bounds;
end;

function TSimpleSurfaceWithEffect.GetQuadAreaInWorldSpace: TQuadF;
var m: TOGLCMatrix;
begin
  m := GetMatrixSurfaceToWorld;
  Result[cTL] := m.Transform(PointF(0, 0));
  Result[cTR] := m.Transform(PointF(Width, 0));
  Result[cBR] := m.Transform(PointF(Width, Height));
  Result[cBL] := m.Transform(PointF(0, Height));
end;

function TSimpleSurfaceWithEffect.GetQuadAreaInSceneSpace: TQuadF;
var m: TOGLCMatrix;
begin
  m := GetMatrixSurfaceToScene;
  Result[cTL] := m.Transform(PointF(0, 0));
  Result[cTR] := m.Transform(PointF(Width, 0));
  Result[cBR] := m.Transform(PointF(Width, Height));
  Result[cBL] := m.Transform(PointF(0, Height));
end;

function TSimpleSurfaceWithEffect.GetRectAreaInLocalSpace: TRectF;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

function TSimpleSurfaceWithEffect.GetRectAreaInParentSpace: TRectF;
begin
 Result.Left := X.Value;
 Result.Top := Y.Value;
 Result.Right := X.Value + Width;
 Result.Bottom := Y.Value + Height;
end;

destructor TSimpleSurfaceWithEffect.Destroy;
var i: integer;
begin
  while FChilds.Count > 0 do begin
    TSimpleSurfaceWithEffect(FChilds.Items[0]).Free;
    FChilds.Delete(0);
  end;
  FChilds.Free;
  FChilds := NIL;
  Speed.Free;
  Speed := NIL;
  Angle.Free;
  Angle := NIL;
  Opacity.Free;
  Opacity := NIL;
  Tint.Free;
  Tint := NIL;
  Scale.Free;
  Scale := NIL;

  for i:=0 to High(FListScenario) do
    FListScenario[i].Free;

  FMessageList.Free;
  FMessageList := NIL;

  FCollisionBody.Free;
  FCollisionBody := NIL;

  inherited Destroy;
end;

function TSimpleSurfaceWithEffect.GetScaledBottomY: single;
begin
  Result := CenterY+GetScaledHeight*0.5;
end;

function TSimpleSurfaceWithEffect.GetScaledHeight: single;
begin
  Result := Height*Scale.y.Value;
end;

function TSimpleSurfaceWithEffect.GetScaledRightX: single;
begin
  Result := CenterX+GetScaledWidth*0.5;
end;

function TSimpleSurfaceWithEffect.GetScaledWidth: single;
begin
  Result := Width*Scale.x.Value;
end;

function TSimpleSurfaceWithEffect.GetScaledX: single;
begin
  Result := CenterX-GetScaledWidth*0.5;
end;

function TSimpleSurfaceWithEffect.GetScaledY: single;
begin
  Result := CenterY-GetScaledHeight*0.5;
end;

procedure TSimpleSurfaceWithEffect.Update(const aElapsedTime: single);
var Xr,Yr,s,c: single ;
    i, loopLength: integer;
    scenar: TScenario;
begin
  if FParentSurface <> NIL then begin
    FSurfaceOrigin.x := FParentSurface.FSurfaceOrigin.x + X.Value;
    FSurfaceOrigin.y := FParentSurface.FSurfaceOrigin.y + Y.Value;
  end else begin
    FSurfaceOrigin.x := X.Value;
    FSurfaceOrigin.y := Y.Value;
  end;

  if FEnableProcessMessage then FMessageList.ProcessMessages(aElapsedTime);

  if FFreeze then exit;

  Speed.OnElapse(aElapsedTime);

  if X.State <> psNO_CHANGE
    then X.OnElapse(aElapsedTime)
    else X.Value := X.Value + Speed.x.Value * aElapsedTime;
  if Y.State <> psNO_CHANGE
    then Y.OnElapse(aElapsedTime)
    else Y.Value := Y.Value + Speed.y.Value * aElapsedTime;

  if FTimeBeforeKill <> 0 then
  begin
    FTimeBeforeKill -= aElapsedTime;
    if FTimeBeforeKill <= 0 then FKill := TRUE;
  end;

  // Frame
  FFrame += aElapsedTime * FFrameDeltaPerSecond;
  if (FFrameLoopEnd <> 0) and (FFrameLoopStart <> 0) then
  begin
    loopLength := FFrameLoopEnd-FFrameLoopStart+1;

    while trunc(FFrame) > FFrameLoopEnd do FFrame -= loopLength;
    while trunc(FFrame) < FFrameLoopStart do FFrame += loopLength;
  end;

  // Blink
  with FBlink do
  if BlinkToGo <> 0 then
  begin
    if CountInvisible > 0 then
    begin
      // surface is invisible
      FVisible := FALSE;
      CountInvisible -= aElapsedTime;
      CountVisible := VisibleTime;
    end else begin
      // surface is visible
      FVisible := TRUE;
      CountVisible -= aElapsedTime;
      if CountVisible <= 0 then
      begin
        CountInvisible := InvisibleTime;
        if BlinkToGo > 0 then dec(BlinkToGo);
        if BlinkToGo = 0 then FVisible := TRUE;
      end;
    end;
  end;
  // Angle
  Angle.OnElapse(aElapsedTime);
  // Do Rotation around an axis
  with FRotationAroundPt do
   if InProgress then begin
     Xr := GetCenterX - XPt;
     Yr := GetCenterY - YPt;
     sincos(AnglePerSecond * aElapsedTime * deg2rad, s, c);
     SetCenterCoordinate(XPt + Xr * c + Yr * s, YPt - Xr * s  + c * Yr);
   end;
  // Do Opacity Variation
  Opacity.OnElapse(aElapsedTime);
  // Do Tint Variation
  Tint.OnElapse(aElapsedTime);
  // Do Scale Variation
  Scale.OnElapse(aElapsedTime);
  // Do Bind to another sprite
  if (FBindToSprite.InProgress) and ( FBindToSprite.TargetSurface <> NIL ) then
  begin
    X.Value := FBindToSprite.TargetSurface.X.Value + FBindToSprite.OffsetX;
    Y.Value := FBindToSprite.TargetSurface.Y.Value + FBindToSprite.OffsetY;
  end;
  // Scenario
  for i:=0 to High(FListScenario) do begin
    scenar := FListScenario[i];
    if scenar.FKilled then begin
      scenar.Free;
      FListScenario[i] := NIL;
    end
    else if scenar.FIsRunning then scenar.Execute(aElapsedTime);
  end;

  // childs update
  for i:=0 to FChilds.Count-1 do
   TSimpleSurfaceWithEffect( FChilds.Items[i] ).Update(aElapsedTime);

  // delete all childs with FKill=TRUE
  for i:=FChilds.Count-1 downto 0 do
   if TSimpleSurfaceWithEffect(FChilds.Items[i]).FKill then begin
     TSimpleSurfaceWithEffect(FChilds.Items[i]).Free;
     FChilds.Delete(i);
   end;
end;

procedure TSimpleSurfaceWithEffect.Draw(const aLayerPercentOpacity : single);
begin
  FComputedOpacity := aLayerPercentOpacity*Opacity.Value*OneDiv255;
  if (FComputedOpacity = 0) and FChildsUseParentOpacity then exit;
  FComputedTint.InitFromTint(TintMode, Tint.Value);
  PushAndPrepareSceneMV;

  if FChildsUseParentOpacity then DrawChildSurfacesWithNegativeZOrder(FComputedOpacity)
    else DrawChildSurfacesWithNegativeZOrder(aLayerPercentOpacity);

  // draw is done in descendant classes throught DoDraw.
  // If you don't want this mecanism override Draw(const aLayerPercentOpacity : single)
  DoDraw();

  if FChildsUseParentOpacity then DrawChildSurfacesWithPositiveZOrder(FComputedOpacity)
    else DrawChildSurfacesWithPositiveZOrder(aLayerPercentOpacity);

  {$IFDEF DEBUG_MODE_ON}
    FParentScene.BlendMode := FX_BLEND_NORMAL;
    DrawBox(FParentScene, 0, 0, Width, Height, BGRA(255,0,0));
  {$ENDIF}

  // restore the parent scene modelview matrix
  PopSceneMV;
end;

procedure TSimpleSurfaceWithEffect.PostMessage(UserValue: TUserMessageValue; aDelay: single);
begin
  FMessageList.Add(UserValue, aDelay);
end;

procedure TSimpleSurfaceWithEffect.ProcessMessage(UserValue: TUserMessageValue);
begin
  // override to process the received messages
end;

procedure TSimpleSurfaceWithEffect.ClearMessageList;
begin
  FMessageList.Clear;
end;

procedure TSimpleSurfaceWithEffect.AddChild(ASurface: TSimpleSurfaceWithEffect; aZOrder: integer);
begin
  FChilds.Add(ASurface);
  ASurface.SetParentSurface(self);
  ASurface.SetParentScene(FParentScene);
  ASurface.SetParentLayer(FParentLayer);
  ASurface.FZOrderAsChild := aZOrder;
  FChilds.Sort(@CompareSurfaceChildsByZOrder);
end;

procedure TSimpleSurfaceWithEffect.SetZOrder(aZOrder: integer);
begin
  if (ParentSurface = NIL) or (FZOrderAsChild = aZOrder) then exit;

  FZOrderAsChild := aZOrder;
  ParentSurface.FChilds.Sort(@CompareSurfaceChildsByZOrder);
end;

procedure TSimpleSurfaceWithEffect.DeleteChild(aIndex: integer);
begin
  if (aIndex >= 0) or (aIndex < FChilds.Count) then DeleteChild(GetChild(aIndex));
end;

procedure TSimpleSurfaceWithEffect.DeleteChild(aSurface: TSimpleSurfaceWithEffect);
begin
  // remove instance from mouse manager
  if FParentScene.Mouse.FSurfaceLockedByMouse = aSurface then
    FParentScene.Mouse.FSurfaceLockedByMouse := NIL;
  if FParentScene.Mouse.FSurfaceUnderMouse = aSurface then
    FParentScene.Mouse.FSurfaceUnderMouse := NIL;

  aSurface.Kill;
end;

procedure TSimpleSurfaceWithEffect.DeleteAllChilds;
var i: integer;
begin
  for i:=FChilds.Count-1 downto 0 do DeleteChild(i);
end;

procedure TSimpleSurfaceWithEffect.RemoveChild(aSurface: TSimpleSurfaceWithEffect);
//var p: TPointF;
begin
//  p := SurfaceToScene(PointF(0,0));
  FChilds.Remove(aSurface);
  aSurface.FParentSurface := NIL;
//  aSurface.SetCoordinate(p);
//  aSurface.FSurfaceOrigin := p;
end;

procedure TSimpleSurfaceWithEffect.MoveFromChildToScene(aLayerIndex: integer);
var p: TPointF;
begin
  if ParentSurface = NIL then exit;
  p := SurfaceToWorld(PointF(0,0)); //SurfaceToScene(PointF(0,0));
  ParentSurface.RemoveChild(Self);
  FParentScene.Add(Self, aLayerIndex);
 { if FParentScene.Layer[aLayerIndex].Camera <> NIL then
    p := FParentScene.Layer[aLayerIndex].Camera.ControlToWorld(p); }
  SetCoordinate(p);
end;

procedure TSimpleSurfaceWithEffect.MoveFromSceneToChildOf(aParentSurface: TSimpleSurfaceWithEffect; aZOrder: integer);
var p: TPointF;
begin
  if aParentSurface = NIL then exit;

  p := aParentSurface.WorldToSurface(GetXY); // aParentSurface.SceneToSurface(GetXY);
  ParentLayer.Remove(Self);
  aParentSurface.AddChild(Self, aZOrder);
  SetCoordinate(p);
end;

procedure TSimpleSurfaceWithEffect.SetChildOf(aParentSurface: TSimpleSurfaceWithEffect; aZOrder: integer);
begin
  if aParentSurface = NIL then exit;

  if ParentSurface = aParentSurface then SetZOrder(aZOrder)
  else
  if ParentSurface = NIL then begin
    if ParentLayer <> NIL
      then MoveFromSceneToChildOf(aParentSurface, aZOrder)
      else aParentSurface.AddChild(Self, aZOrder);
  end else begin
    MoveFromChildToScene(0);
    MoveFromSceneToChildOf(aParentSurface, aZOrder);
  end;
end;

procedure TSimpleSurfaceWithEffect.MoveToLayer(aLayerIndex: integer);
begin
  if ParentSurface <> NIL then
    MoveFromChildToScene(aLayerIndex)
  else begin
    if ParentLayer <> NIL then
      ParentLayer.Remove(Self);
    FParentScene.Add(Self, aLayerIndex)
  end;
end;

procedure TSimpleSurfaceWithEffect.InsertToLayer(aSurfaceIndex,
  aLayerIndex: integer);
begin
  if ParentSurface <> NIL then begin
    MoveFromChildToScene(aLayerIndex);
    ParentLayer.Remove(Self);
    FParentScene.Insert(aSurfaceIndex, Self, aLayerIndex);
  end else begin
    if ParentLayer <> NIL then
      ParentLayer.Remove(Self);
    FParentScene.Insert(aSurfaceIndex, Self, aLayerIndex);
  end;
end;

procedure TSimpleSurfaceWithEffect.RemoveFromScene;
begin
  if ParentSurface <> NIL then ParentSurface.RemoveChild(Self)
    else if ParentLayer <> NIL then ParentLayer.Remove(Self);
end;

procedure TSimpleSurfaceWithEffect.MoveToTop;
var par: TSimpleSurfaceWithEffect;
begin
  if ParentSurface = NIL then begin
    if ParentLayer = NIL then exit;
    ParentLayer.Remove(Self);
    ParentLayer.Add(Self);
  end else begin
    par := ParentSurface;
    ParentSurface.RemoveChild(Self); // here ParentSurface is lost
    par.AddChild(Self, par.Childs[par.ChildCount-1].ZOrderAsChild + 1);
  end;
end;

procedure TSimpleSurfaceWithEffect.MoveToTopOneStep;
var i: integer;
begin
  if ParentSurface = NIL then begin
    if ParentLayer = NIL then exit;
    i := ParentLayer.IndexOf(Self);
    if i < ParentLayer.SurfaceCount-1 then
      ParentLayer.Move(i, i+1);
  end else begin
    raise exception.Create('not yet implemented');
  end;
end;

procedure TSimpleSurfaceWithEffect.MoveBack;
var par: TSimpleSurfaceWithEffect;
begin
  if ParentSurface = NIL then begin
    if ParentLayer = NIL then exit;
    ParentLayer.Remove(Self);
    ParentLayer.Insert(0, Self);
  end else begin
   par := ParentSurface;
   ParentSurface.RemoveChild(Self); // here ParentSurface is lost
   par.AddChild(Self, par.Childs[0].ZOrderAsChild - 1);
  end;
end;

procedure TSimpleSurfaceWithEffect.MoveBackOneStep;
var i: integer;
begin
  if ParentSurface = NIL then begin
    if ParentLayer = NIL then exit;
    i := ParentLayer.IndexOf(Self);
    if i > 0 then
      ParentLayer.Move(i, i-1);
  end else begin
    raise exception.Create('not yet implemented');
  end;
end;

function TSimpleSurfaceWithEffect.SurfaceToScene(aPt: TPointF): TPointF;
//var s: TSimpleSurfaceWithEffect;
begin
{  s := Self;
  Result := aPt;
  repeat
    Result := s.GetMatrix.Transform(Result);
    s := s.ParentSurface;
  until s = NIL;     }

  Result := GetMatrixSurfaceToScene.Transform(aPt);
end;

function TSimpleSurfaceWithEffect.SceneToSurface(aPt: TPointF): TPointF;
//var s: TSimpleSurfaceWithEffect;
begin
{  s := Self;
  Result := aPt;
  repeat
    Result := s.GetInverseMatrix.Transform(Result);
    s := s.ParentSurface;
  until s = NIL;      }

  Result := GetMatrixSurfaceToScene.Inverse.Transform(aPt);
end;

function TSimpleSurfaceWithEffect.SurfaceToWorld(aPt: TPointF): TPointF;
var s: TSimpleSurfaceWithEffect;
begin
  s := Self;
  Result := aPt;
  repeat
    Result := s.GetMatrix.Transform(Result);
    s := s.ParentSurface;
  until s = NIL;
end;

function TSimpleSurfaceWithEffect.WorldToSurface(aPt: TPointF): TPointF;
var s: TSimpleSurfaceWithEffect;
begin
  s := Self;
  Result := aPt;
  repeat
    Result := s.GetInverseMatrix.Transform(Result);
    s := s.ParentSurface;
  until s = NIL;
end;

function TSimpleSurfaceWithEffect.SurfaceToParent(aPt: TPointF; aParent: TSimpleSurfaceWithEffect): TPointF;
var s: TSimpleSurfaceWithEffect;
begin
  Result := aPt;
  if aParent = NIL then aParent := FParentSurface  // use first parent
    else if not aParent.IsParentOf(Self) then exit;

  if aParent <> NIL then begin
    s := self;
    repeat
      //Result := Result + s.GetXY;
      Result := s.GetMatrix.Transform(Result);
      s := s.FParentSurface;
    until s = aParent;
  end;
end;

function TSimpleSurfaceWithEffect.ParentToSurface(aPt: TPointF; aParent: TSimpleSurfaceWithEffect): TPointF;
var s: TSimpleSurfaceWithEffect;
begin
  Result := aPt;
  if aParent = NIL then aParent := FParentSurface  // use first parent
    else if not aParent.IsParentOf(Self) then exit;

  if aParent <> NIL then begin
    s := self;
    repeat
      //Result := Result - s.GetXY;
      Result := s.GetInverseMatrix.Transform(Result);
      s := s.FParentSurface;
    until s = aParent;
  end;
end;

function TSimpleSurfaceWithEffect.GetMatrix: TOGLCMatrix;
begin
  Result.LoadIdentity;
  AddTransformTo(Result);
end;

function TSimpleSurfaceWithEffect.GetInverseMatrix: TOGLCMatrix;
begin
  Result := GetMatrix.Inverse;
end;

function TSimpleSurfaceWithEffect.IsParentOf(aSurface: TSimpleSurfaceWithEffect ): boolean;
var i: Integer;
begin
  for i:=0 to ChildCount-1 do
    if Childs[i] = aSurface then exit(True);
  Result := FALSE;
end;

function TSimpleSurfaceWithEffect.GetScaledArea: TRect;
var xx, yy, ww ,hh: single;
begin
  ww := Width*Scale.x.Value;
  hh := Height*Scale.y.Value;
  xx := FSurfaceOrigin.x - ww*0.5;//X.Value-ww*0.5;
  yy := FSurfaceOrigin.y - hh*0.5;//Y.Value-hh*0.5;
  Result.Create(Point(Round(xx), Round(yy)), Round(ww), Round(hh));
end;

procedure TSimpleSurfaceWithEffect.KillDefered(ASec: single);
begin
  FTimeBeforeKill := ASec;
end;

procedure TSimpleSurfaceWithEffect.MoveTo(aX, aY: single; aDuration: single; aCurve: Word);
begin
  MoveTo(aX, aY, aDuration, aCurve, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveTo(aPt: TPointF; aDuration: single; aCurve: Word);
begin
  X.ChangeTo( aPt.x, aDuration, aCurve);
  Y.ChangeTo( aPt.y, aDuration, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveTo(aX, aY: single; aDuration: single; aXCurve, aYCurve: Word);
begin
  X.ChangeTo(aX, aDuration, aXCurve);
  Y.ChangeTo(aY, aDuration, aYCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveTo(aPt: TPointF; aDuration: single; aXCurve, aYCurve: Word);
begin
  X.ChangeTo(aPt.x, aDuration, aXCurve);
  Y.ChangeTo(aPt.y, aDuration, aYCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveCenterTo(aCenterX, aCenterY, aDuration: single; aCurve: Word);
begin
  MoveXCenterTo(aCenterX, aDuration, aCurve);
  MoveYCenterTo(aCenterY, aDuration, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveCenterTo(aPt: TPointF; aDuration: single; aCurve: Word);
begin
  MoveXCenterTo(aPt.x, aDuration, aCurve);
  MoveYCenterTo(aPt.y, aDuration, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveXCenterTo(aX, aDuration: single; aCurve: Word);
begin
  X.ChangeTo(aX - GetWidth * 0.5, aDuration, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveYCenterTo(aY, aDuration: single; aCurve: Word);
begin
  Y.ChangeTo(aY - GetHeight * 0.5, aDuration, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveRelative(aDeltaX, aDeltaY: single; aDuration: single; aCurve: Word);
begin
  MoveTo(X.Value+aDeltaX, Y.Value+aDeltaY, aDuration, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveRelative(aDelta: TPointF; aDuration: single; aCurve: Word);
begin
  MoveTo(X.Value+aDelta.x, Y.Value+aDelta.y, aDuration, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveXRelative(aDeltaX, aDuration: single; aCurve: Word);
begin
  X.ChangeTo(X.Value + aDeltaX, aDuration, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveYRelative(aDeltaY, aDuration: single; aCurve: Word);
begin
  Y.ChangeTo(Y.Value + aDeltaY, aDuration, aCurve);
end;

procedure TSimpleSurfaceWithEffect.Blink(aNumberOfBlink: integer; aVisibleTime, aInvisibleTime: single );
begin
  with FBlink do begin
    BlinkToGo := aNumberOfBlink;
    VisibleTime := aVisibleTime;
    InvisibleTime := aInvisibleTime;
    CountVisible := aVisibleTime;
    CountInvisible := aInvisibleTime;
   end;
end;

procedure TSimpleSurfaceWithEffect.StopBlink;
begin
  FBlink.BlinkToGo := 0;
  Visible := True;
end;

procedure TSimpleSurfaceWithEffect.RotationAroundPoint(aXPt, aYPt: single; aAnglePerSecond: single; aSelfRotate: boolean);
var xx,yy:single;
begin
  with FRotationAroundPt do begin
    InProgress := TRUE ;
    XPt := aXPt ;
    YPt := aYPt ;
    AnglePerSecond := aAnglePerSecond ;
    // calculate actual angle and radius
    xx := CenterX - aXPt;
    yy := CenterY - aYPt;
    Radius := sqrt(xx * xx + yy * yy);
    if Radius > 0 then ActualAngleRad := arccos(xx / Radius)
      else ActualAngleRad := 0;
  end;
  if aSelfRotate then Angle.AddConstant(-aAnglePerSecond);
end;

procedure TSimpleSurfaceWithEffect.RotationAroundPoint(aPt: TPointF; aAnglePerSecond: single; aSelfRotate: boolean);
begin
  RotationAroundPoint(aPt.x, aPt.y, aAnglePerSecond, aSelfRotate);
end;

procedure TSimpleSurfaceWithEffect.StopRotationAroundPoint;
begin
  FRotationAroundPt.InProgress := False;
end;

procedure TSimpleSurfaceWithEffect.DrawChildSurfacesWithPositiveZOrder(const aLayerPercentOpacity: single);
var i: integer;
    o: TSimpleSurfaceWithEffect;
begin
  for i:=0 to FChilds.Count-1 do begin
    o := TSimpleSurfaceWithEffect(FChilds.Items[i]);
    if (o.FZOrderAsChild >= 0) and o.Visible then
      o.Draw(aLayerPercentOpacity);
  end;
end;

procedure TSimpleSurfaceWithEffect.DrawChildSurfacesWithNegativeZOrder(const aLayerPercentOpacity: single);
var i: integer;
    o: TSimpleSurfaceWithEffect;
begin
  for i:=0 to FChilds.Count-1 do begin
    o := TSimpleSurfaceWithEffect(FChilds.Items[i]);
    if o.FZOrderAsChild >= 0 then exit
      else if o.Visible then o.Draw(aLayerPercentOpacity);
  end;
end;

procedure TSimpleSurfaceWithEffect.SetFrameLoopBounds(AStartFrameIndex, AEndFrameIndex: integer);
var temp: integer;
begin
  if AStartFrameIndex > AEndFrameIndex then begin
    temp := AStartFrameIndex;
    AStartFrameIndex := AEndFrameIndex;
    AEndFrameIndex := temp;
  end;

  FFrameLoopStart := AStartFrameIndex;
  FFrameLoopEnd := AEndFrameIndex;

  if not InRange(trunc(FFrame), AStartFrameIndex, AEndFrameIndex)
    then FFrame := AStartFrameIndex;
end;

procedure TSimpleSurfaceWithEffect.FrameAddPerSecond(ADeltaFramePerSecond: single);
begin
  FFrameDeltaPerSecond := ADeltaFramePerSecond;
end;

procedure TSimpleSurfaceWithEffect.CenterOnScene;
begin
  if FParentScene <> NIL
    then SetCenterCoordinate(FParentScene.Width*0.5, FParentScene.Height*0.5);
end;

procedure TSimpleSurfaceWithEffect.CenterOnSurface(ASurface: TSimpleSurface);
begin
  SetCenterCoordinate(ASurface.CenterX, ASurface.CenterY);
end;

procedure TSimpleSurfaceWithEffect.CenterOnParent;
begin
  if FParentSurface <> NIL then begin
    CenterX := FParentSurface.Width * 0.5;
    CenterY := FParentSurface.Height * 0.5;
  end;
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(aPoint: TPointF): boolean;
begin
  Result := Collision.PointRectF(aPoint, GetScaledAreaF);
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(aSurface: TSimpleSurfaceWithEffect): boolean;
begin
  Result := Collision.RectFRectF(GetScaledAreaF, aSurface.GetScaledAreaF);
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(aSurface: TSimpleSurfaceWithEffect; HTolerance, VTolerance: integer): boolean;
var r: TRectF;
begin
  r := aSurface.GetScaledAreaF;
  r.Left := r.Left - HTolerance;
  r.Top := r.Top - VTolerance;
  r.Right := r.Right + HTolerance;
  r.Bottom := r.Bottom + VTolerance;

  if (r.Width <= 0) or (r.Height <= 0) then exit(False);

  Result := Collision.RectFRectF(GetScaledAreaF, r);
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(const aX, aY, aWidth, aHeight: single): boolean;
var r: TRectF;
begin
  r.Left := aX;
  r.Top := aY;
  r.Right := aX + aWidth;
  r.Bottom := aY + aHeight;
  Result := Collision.RectFRectF(GetScaledAreaF, r);
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(aRect: TRectF): boolean;
begin
  Result := Collision.RectFRectF(GetScaledAreaF, aRect);
end;

function TSimpleSurfaceWithEffect.CollisionTestWith(aRect: TRect): boolean;
var r: TRectF;
begin
  r := RectF(aRect);
  Result := Collision.RectFRectF(GetScaledAreaF, r);
end;

function TSimpleSurfaceWithEffect.GetChild(aIndex: integer): TSimpleSurfaceWithEffect;
begin
  Result := TSimpleSurfaceWithEffect(FChilds.Items[aIndex]);
end;

function TSimpleSurfaceWithEffect.ChildCount: integer;
begin
  Result := FChilds.Count;
end;

function TSimpleSurfaceWithEffect.IsChild(aSurface: TSimpleSurfaceWithEffect): boolean;
var i: integer;
begin
  for i:=0 to FChilds.Count-1 do
    if TSimpleSurfaceWithEffect(FChilds.Items[i]) = aSurface then exit(True);
  Result := False;
end;

function TSimpleSurfaceWithEffect.CreateSpriteChild(aTexture: PTexture;
  aOwner: boolean; aZOrder: integer): TSprite;
begin
  Result := TSprite.Create(aTexture, aOwner);
  AddChild(Result, aZOrder);
end;

function TSimpleSurfaceWithEffect.AddScenarioFromFile(const aFilename: string): TIDScenario;
var o: TScenario;
begin
  o := TScenario.Create;
  o.LoadFromFile(aFilename);
  Result := DoAddScenarioToList(o);
end;

function TSimpleSurfaceWithEffect.AddScenario(const aLines: string): TIDScenario;
var o: TScenario;
begin
  o := TScenario.Create;
  o.InitWith(aLines);
  o.CurrentLine := 0;
  o.OnDecode := FScenarioAdditionnalDecode;
  Result := DoAddScenarioToList(o);
end;

procedure TSimpleSurfaceWithEffect.PlayScenario(aID: TIDScenario; aFromBeginning: boolean);
var s: TScenario;
begin
  s := GetScenario(aID);
  if s = NIL then exit;

  if aFromBeginning then begin
    s.CurrentLine := 0;
    s.WaitTime := 0.0;
  end;

  s.FIsRunning := TRUE;
end;

function TSimpleSurfaceWithEffect.AddAndPlayScenarioFromFile(const aFilename: string): TIDScenario;
begin
  Result := AddScenarioFromFile(aFilename);
  PlayScenario(Result);
end;

function TSimpleSurfaceWithEffect.AddAndPlayScenario(const aLines: string): TIDScenario;
begin
  Result := AddScenario(aLines);
  PlayScenario(Result);
end;

procedure TSimpleSurfaceWithEffect.ReplaceScenario(aID: TIDScenario; const aLines: string);
var s: TScenario;
begin
  s := GetScenario(aID);
  if s <> NIL then begin
    s.FIsRunning := FALSE;
    s.InitWith(aLines);
  end;
end;

procedure TSimpleSurfaceWithEffect.ReplaceScenarioFromFile(aID: TIDScenario; const aFilename: string);
var s: TScenario;
begin
  s := GetScenario(aID);
  if s <> NIL then begin
    s.FIsRunning := FALSE;
    s.LoadFromFile(aFilename);
  end;
end;

procedure TSimpleSurfaceWithEffect.StopScenario(aID: TIDScenario);
var s: TScenario;
begin
  s := GetScenario(aID);
  if s <> NIL then s.FIsRunning := FALSE;
end;

procedure TSimpleSurfaceWithEffect.StopAllScenario;
var i: integer;
begin
  for i :=0 to Length(FListScenario)-1 do
    FListScenario[i].FIsRunning := FALSE;
end;

procedure TSimpleSurfaceWithEffect.DeleteScenario(aID: TIDScenario);
begin
  if (aID >= 0) and (aID < Length(FListScenario)) then begin
    //FListScenario[aID].
  end;
end;

function TSimpleSurfaceWithEffect.ScenarioIsPlaying(aID: TIDScenario): boolean;
var s: TScenario;
begin
  s := GetScenario(aID);
  if s = NIL then Result := FALSE
    else Result := S.FIsRunning;
end;

procedure TSimpleSurfaceWithEffect.SetScenarioAdditionnalDecode(aCallbackDecode: TScenarioDecode);
begin
  FScenarioAdditionnalDecode := aCallbackDecode;
end;


procedure TSimpleSurfaceWithEffect.BindToSprite(aTargetSurface: TSimpleSurface; aOffsetX, aOffsetY: single);
begin
  FBindToSprite.InProgress := TRUE;
  FBindToSprite.TargetSurface := aTargetSurface;
  FBindToSprite.OffsetX := aOffsetX;
  FBindToSprite.OffsetY := aOffsetY;
end;

procedure TSimpleSurfaceWithEffect.BindCenterToSprite(aTargetSurface: TSimpleSurface);
begin
  BindToSprite(aTargetSurface, (aTargetSurface.Width-Width)*0.5, (aTargetSurface.Height-Height)*0.5);
end;

procedure TSimpleSurfaceWithEffect.UnBind;
begin
  FBindToSprite.InProgress := FALSE;
end;

{ TScenario }

constructor TScenario.Create;
begin
  ID := -1;
  Actions := TStringList.Create;
  CurrentLine := 0;
  WaitTime := 0.0;
  ParentSurface := NIL;
  FIsRunning := FALSE;
end;

destructor TScenario.Destroy;
begin
  Actions.Free;
  Actions := NIL;
end;

procedure TScenario.Execute(const aElapsedTime: single);
var OriginLine: integer;
begin
  if WaitTime > 0
    then WaitTime -= aElapsedTime;
  if WaitTime <= 0.000 then begin
    OriginLine := CurrentLine;
    repeat
      DecodeAction;
      inc(CurrentLine);
    until (WaitTime > 0) or (CurrentLine >= Actions.Count) or
           not FIsRunning or (OriginLine = CurrentLine); // avoid infinite loop in buggy scenario
    if CurrentLine >= Actions.Count then FIsRunning := FALSE;
  end;
end;

procedure TScenario.LoadFromFile(const aFilename: string);
begin
 Actions.Clear;
 try
   Actions.LoadFromFile(aFilename);
 except
   ParentSurface.ParentScene.LogError('TScenario.LoadFromFile: error while loading scenario file "'+aFilename+'"');
 end;
end;

procedure TScenario.DecodeAction;
var i: integer;
begin
 if FRunError then exit;
 if Length(Actions.Strings[CurrentLine]) = 0 then exit;

 FTemp := Actions.Strings[CurrentLine].Split([' ']);

 try
 case FTemp[0] of
  acWait : WaitTime := StringToSingle(FTemp[1]);
  acKill : ParentSurface.Kill;
  acFreeze : ParentSurface.Freeze := FTemp[1] = acTRUE ;
  acVisible : ParentSurface.Visible := FTemp[1] = acTRUE ;

  acOpacity: ParentSurface.Opacity.Value := strtoint(FTemp[1]);
  acOpacityChange: ParentSurface.Opacity.ChangeTo(strtoint(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID( FTemp[3]));

  acTint: ParentSurface.Tint.Value := BGRA(strtoint( FTemp[1]), strtoint(FTemp[2]), strtoint(FTemp[3]), strtoint(FTemp[4]));
  acTintChange: ParentSurface.Tint.ChangeTo(BGRA(strtoint(FTemp[1]),
                                                 strtoint(FTemp[2]),
                                                 strtoint(FTemp[3]),
                                                 strtoint(FTemp[4])),
                                                 StringToSingle(FTemp[5]),
                                                 StringToCurveID(FTemp[6]));
  acTintRedChange: ParentSurface.Tint.Red.ChangeTo(strtoint(FTemp[1]), StringToSingle(FTemp[2]),  StringToCurveID(FTemp[3]));
  acTintGreenChange: ParentSurface.Tint.Green.ChangeTo(strtoint(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID(FTemp[3]));
  acTintBlueChange: ParentSurface.Tint.Blue.ChangeTo(strtoint(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID(FTemp[3]));
  acTintAlphaChange: ParentSurface.Tint.Alpha.ChangeTo(strtoint(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID(FTemp[3]));

  acAngle: ParentSurface.Angle.Value := StringToSingle( FTemp[1]) ;
  acRotate: ParentSurface.Angle.AddConstant(StringToSingle(FTemp[1]));
  acRotateTo: ParentSurface.Angle.ChangeTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID(FTemp[3]));
  acRotationAroundAxis: ParentSurface.RotationAroundPoint(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToSingle(FTemp[3]), FTemp[4] = acTRUE);

  acScale: ParentSurface.Scale.Value := PointF(StringToSingle(FTemp[1]), StringToSingle(FTemp[1]));
  acScaleChange: begin
   ParentSurface.Scale.x.ChangeTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID(FTemp[3]));
   ParentSurface.Scale.y.ChangeTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID(FTemp[3]));
  end;
  acScaleH: ParentSurface.Scale.x.Value := StringToSingle(FTemp[1]);
  acScaleHChange: ParentSurface.Scale.x.ChangeTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID(FTemp[3]));
  acScaleV: ParentSurface.Scale.y.Value := StringToSingle( FTemp[1] );
  acScaleVChange: ParentSurface.Scale.y.ChangeTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID(FTemp[3]));

  acBlink: ParentSurface.Blink(strtoint(FTemp[1]), StringToSingle(FTemp[2]), StringToSingle(FTemp[3]));
  acStopBlink: ParentSurface.StopBlink;

  acFlipH: ParentSurface.FlipH := FTemp[1] = acTRUE ;
  acFlipV: ParentSurface.FlipV := FTemp[1] = acTRUE ;
  acToggleFlipH: ParentSurface.FlipH := not ParentSurface.FlipH;
  acToggleFlipV: ParentSurface.FlipV := not ParentSurface.FlipV;

  acAnimate: begin
    ParentSurface.SetFrameLoopBounds(strtoint(FTemp[1]), strtoint(FTemp[2]));
    ParentSurface.FrameAddPerSecond(StringToSingle(FTemp[3]));
  end;
  acIncFrame: ParentSurface.Frame := ParentSurface.Frame + 1;
  acDecFrame: ParentSurface.Frame := ParentSurface.Frame - 1;
  acSetFrame: ParentSurface.Frame := strtoint(FTemp[1]);


  acMoveTo: ParentSurface.MoveTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToSingle(FTemp[3]), StringToCurveID(FTemp[4]));
  acMoveXTo: ParentSurface.X.ChangeTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID(FTemp[3]));
  acMoveYTo: ParentSurface.Y.ChangeTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID(FTemp[3]));

  acMoveCenterTo: ParentSurface.MoveCenterTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToSingle(FTemp[3]), StringToCurveID(FTemp[4]));
  acMoveXCenterTo: ParentSurface.MoveXCenterTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID( FTemp[3]));
  acMoveYCenterTo: ParentSurface.MoveYCenterTo(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID( FTemp[3]));

  acMoveRelative: ParentSurface.MoveRelative(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToSingle(FTemp[3]), StringToCurveID(FTemp[4]));
  acMoveXRelative: ParentSurface.MoveXRelative(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID( FTemp[3]));
  acMoveYRelative: ParentSurface.MoveYRelative(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]), StringToCurveID( FTemp[3]));

  acSetCoor: begin
    ParentSurface.X.Value := StringToSingle(FTemp[1]);
    ParentSurface.Y.Value := StringToSingle(FTemp[2]);
  end;
  acSetCenterCoor: ParentSurface.SetCenterCoordinate(StringToSingle(FTemp[1]), StringToSingle(FTemp[2]));

  acCenterOnScene: ParentSurface.CenterOnScene;

  acPostMessage: ParentSurface.PostMessage(strtoint(FTemp[1]), StringToSingle(FTemp[2]));

  acLoop: CurrentLine := -1;
  acLabel:;
  acGotoLabel: begin
   i := Actions.IndexOf(acLabel + ' ' + FTemp[1]);
   if i <> -1 then CurrentLine := i - 1;
  end;

  else if FScenarioDecodeProc <> NIL then FScenarioDecodeProc(FTemp);
 end;//case
 except
   On E :Exception do begin
      ParentSurface.ParentScene.LogError('TScenario.DecodeAction: an exception occurs: "'+E.Message+'"');
      FRunError := True;
    end;
 end;
end;

procedure TScenario.InitWith(const aLines: string);
begin
  Actions.Clear;
  FRunError := False;
  Actions.TextLineBreakStyle := tlbsLF;
  Actions.Text := aLines;
end;

procedure TScenario.AddLine(const aLine: string);
begin
 Actions.Add(aLine);
end;

procedure TScenario.Stop;
begin
  FIsRunning := False;
end;

procedure TScenario.Kill;
begin
  FKilled := True;
end;


{ TSimpleSurface }

constructor TSimpleSurface.Create;
begin
  X := TFParam.Create;
  Y := TFParam.Create;

  FVisible := TRUE ;
  FKill := FALSE ;
  FTag1 := 0 ;
  FTag2 := FALSE ;

  FParentLayer := NIL;
  FParentScene := NIL;
end;

destructor TSimpleSurface.Destroy;
begin
  X.Free;
  X := NIL;
  Y.Free;
  Y := NIL;
  inherited Destroy;
end;

function TSimpleSurface.GetCenterX: single;
begin
  Result := x.Value + GetWidth * 0.5 ;
end;

function TSimpleSurface.GetRightX: single;
begin
  Result := x.Value + GetWidth;
end;

function TSimpleSurface.GetBottomY: single;
begin
  Result := y.Value + GetHeight;
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
  x.Value := AValue - GetWidth;
end;

procedure TSimpleSurface.SetBottomY(const AValue: single);
begin
  y.Value := AValue - GetHeight;
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


