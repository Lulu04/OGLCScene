
{$ifdef oglcINTERFACE}
type

TSimpleSurface = class;
TSimpleSurfaceWithEffect = class;
TSprite = class;
TTileEngine = class;

TBlink = record
  BlinkToGo: integer;
  VisibleTime,
  InvisibleTime,
  CountVisible,
  CountInvisible: single;
end;

TRotationAroundPoint = record
  InProgress: boolean;
  XPt,
  YPt,
  AnglePerSecond,
  ActualAngleRad,
  Radius: single;
end;

TBindToSprite = record
  InProgress: boolean;
  TargetSurface : TSimpleSurface;
  OffsetX: single;
  OffsetY: single;
end;

TLayer = class;

{ TOGLCSurfaceList }

TOGLCSurfaceList = class
  Constructor Create;
  Destructor Destroy; override;
protected
  FList: TList;
  function GetSurfaceCount: integer; inline;
  function GetSurface(aIndex: integer): TSimpleSurfaceWithEffect; inline;
  procedure Delete(Index:integer);
public
  procedure Clear; virtual;
  function Add(aSurface: TSimpleSurfaceWithEffect): integer; virtual;
  procedure Remove(aSurface: TSimpleSurfaceWithEffect);
  procedure Insert(aIndex: integer ; aSurface : TSimpleSurfaceWithEffect); virtual;
  function IndexOf(aSurface: TSimpleSurfaceWithEffect): integer;
  procedure Move(aCurrentIndex, aNewIndex: integer);
  function Contain(aSurface: TSimpleSurfaceWithEffect): boolean;
  property SurfaceCount : integer read GetSurfaceCount ;
  property Surface[aIndex:integer]:TSimpleSurfaceWithEffect read GetSurface; default;
end;

TLayerEvent = procedure(Sender: TLayer) of object;
TLayerBeforePaintEvent = procedure(Sender: TLayer; const aLayerOpacity: single) of object;
TLayerBeforeUpdateEvent = procedure(Sender: TLayer; const aElapsedTime: single) of object;

{ TLayer }

TLayer = class(TOGLCSurfaceList)
private
  FParentScene: TOGLCScene;
  FVisible: boolean;
  FFreeze: boolean;
  // Blink
  FBlink: TBlink;
  FCamera: TOGLCCamera;
private
  FOnSortCompare: TListSortCompare;
  FOnBeforePaint: TLayerBeforePaintEvent;
  FOnAfterPaint: TLayerEvent;
  FOnBeforeUpdate: TLayerBeforeUpdateEvent;
  FOnAfterUpdate: TLayerEvent;
public
  Constructor Create;
  Destructor Destroy; override;
  procedure Draw;
  procedure Update(const aElapsedTime: single);
  procedure Clear; override;
public
  Opacity: TBoundedFParam;
  PostProcessing: TOGLCPostProcessingParameters;

  // Return the size of the view:
  //  - no camera assigned: the size of the scene
  //  - a camera is assigned to this layer: the view rect of the camera
  function GetViewRect: TRectF;

  procedure KillAll;
  // Blink  aNumberOfBlink=-1 for infinite blink
  procedure Blink(aNumberOfBlink: integer; aVisibleTime, aInvisibleTime: single);

// Methods below affect the surfaces of the layer
//    if  aGroup = -1  methods affect all surfaces of the layer.
//    if  aGroup > -1  methods affect only the surfaces with specified group number
// Note: every created surface have group property set to zero by default.
  procedure GroupKill(aGroup: integer);
  procedure GroupVisible(aGroup: integer; aValue:boolean);
  procedure GroupFreeze(aGroup: integer; aValue:boolean);
  procedure GroupOpacity(aGroup: integer; aNewOpacity:byte);
  procedure GroupOpacityVariation(aGroup: integer; aNewOpacity: byte; aDuration: single; aCurve: Word=idcLinear);
  procedure GroupTint(aGroup: integer; const aTint: TBGRAPixel);
  procedure GroupTintOpacityVariation(aGroup: integer; aNewOpacity: byte; aDuration: single; aCurve: Word=idcLinear);
  procedure GroupMoveTo(aGroup: integer; aX, aY: single; aDuration: single; aCurve:Word=idcLinear);
  procedure GroupMoveRelative(aGroup: integer; aDeltaX, aDeltaY: single; aDuration: single; aCurve: Word=idcLinear);
  procedure GroupBlink(aGroup: integer; aNumberOfBlink: integer; aVisibleTime, aInvisibleTime: single); // aNumberOfBlink=-1 for infinite blink
  procedure GroupScale(aGroup: integer; aHScale, aVScale: single; aDuration: single; aHScaleCurve: Word=idcLinear; aVScaleCurve: Word=idcLinear); // aHScale, aVScale = -1 for no change
  procedure GroupRotate(aGroup: integer; aAnglePerSecond: single);
  procedure GroupFlipH(aGroup: integer; aValue:boolean);
  procedure GroupFlipV(aGroup: integer; aValue:boolean);

  // Check if the passed surface collide with one in this layer.
  // Return the surface collided, NIL if no collision.
  function CollisionTest(aSurface: TSimpleSurfaceWithEffect) : TSimpleSurfaceWithEffect; overload;
  // Check if the passed surface collide with one in this layer.
  // Return the surface collided, NIL if no collision.
  function CollisionTest(aSurface: TSimpleSurfaceWithEffect; HTolerance, VTolerance: single): TSimpleSurfaceWithEffect; overload;
  // Check if the rect collide with one in this layer.
  // Return the surface collided, NIL if no collision.
  function CollisionTest(aX, aY, aWidth, aHeight: single) : TSimpleSurfaceWithEffect; overload;
  // Check if the rect collide with one in this layer, excluding the passed surface.
  // Return the surface collided, NIL if no collision.
  function CollisionTest(aX, aY, aWidth, aHeight: single; DontTestSurface: TSimpleSurfaceWithEffect) : TSimpleSurfaceWithEffect; overload;

  // Freeze the layer and its surfaces
  property Freeze : boolean read FFreeze write FFreeze;
  // Set the layer visible or not
  property Visible : boolean read FVisible write FVisible;
  // don't use this property directly. Use instead TOGLCCamera.AssignToLayer method to assign
  // a camera view to the layer
  property Camera: TOGLCCamera read FCamera write FCamera;
  // specify a function to sort the surfaces on this layer
  property OnSortCompare:TListSortCompare read FOnSortCompare write FOnSortCompare;

  // Assign this callback if you want to do some initializations before surfaces are rendered on the layer.
  // For example, you can bind a TOGLCRenderToTexture to render the layer in a target texture
  property OnBeforePaint: TLayerBeforePaintEvent read FOnBeforePaint write FOnBeforePaint;
  // Assign this callback if you want to do some finalizations after the layer is rendered.
  // For example, you can unbind a TOGLCRenderToTexture and render its texture with a specific shader
  property OnAfterPaint: TLayerEvent read FOnAfterPaint write FOnAfterPaint;
  // Called before layer update.
  property OnBeforeUpdate: TLayerBeforeUpdateEvent read FOnBeforeUpdate write FOnBeforeUpdate;
  // called at the end of layer update.
  property OnAfterUpdate: TLayerEvent read FOnAfterUpdate write FOnAfterUpdate;
end;


TSurfaceChangeLayer = record
  Surface: TSimpleSurfaceWithEffect;
  NewLayerIndex: integer;
end;
TArrayOfSurfaceChangeLayer = array of TSurfaceChangeLayer;

{ TLayerList }

TLayerList = class
private
  FList: TList;
  function GetLayerCount: integer;
  function GetLayer(Index: integer): TLayer;
  function GetSurfaceCount: integer;
  procedure SetLayerCount(AValue: integer); virtual;
  procedure AddSurfaceToLayer(aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer=0);

  procedure InsertSurfaceToLayer(aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer=0);
private
  FListSurfaceChangeLayer: TArrayOfSurfaceChangeLayer;
public
  Constructor Create;
  Destructor Destroy; override;
  // delete(kill) all surfaces in all layers
  procedure ClearAllLayer;
  // delete all surfaces in specified layer
  procedure ClearLayer(aLayerIndex:integer);

  // gives the index of the specified layer (-1 if not found)
  function LayerIndexOf(aLayer: TLayer): integer;

  // move a surface on another layer
  procedure SurfaceChangeLayer(aSurface: TSimpleSurfaceWithEffect; aNewLayerIndex: integer); deprecated 'use TOGLCScene.MoveSurfaceToLayer instead';

  procedure AllLayerVisible;
  procedure OnlyThisLayerVisible(const tab: array of integer);

  property LayerCount:integer read GetLayerCount write SetLayerCount;
  property Layer[Index:integer]:TLayer read GetLayer;
  property SurfaceCount:integer read GetSurfaceCount;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCSurfaceList }

constructor TOGLCSurfaceList.Create;
begin
  FList := TList.Create;
end;

destructor TOGLCSurfaceList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TOGLCSurfaceList.GetSurfaceCount: integer;
begin
  Result := FList.Count;
end;

function TOGLCSurfaceList.GetSurface(aIndex: integer): TSimpleSurfaceWithEffect;
begin
  Result := TSimpleSurfaceWithEffect(FList.Items[aIndex]);
end;


function TOGLCSurfaceList.Add(aSurface: TSimpleSurfaceWithEffect): integer;
begin
  Result := FList.Add(aSurface);
end;

procedure TOGLCSurfaceList.Remove(aSurface: TSimpleSurfaceWithEffect);
begin
  FList.Extract(aSurface);//Remove(aSurface);
end;


procedure TOGLCSurfaceList.Insert(aIndex: integer;aSurface: TSimpleSurfaceWithEffect);
begin
  if aIndex < 0 then aIndex := 0;

  if aIndex < FList.Count then FList.Insert(aIndex, aSurface)
    else FList.Add(aSurface);
end;

function TOGLCSurfaceList.IndexOf(aSurface: TSimpleSurfaceWithEffect): integer;
begin
  Result := FList.IndexOf(aSurface);
end;

procedure TOGLCSurfaceList.Move(aCurrentIndex, aNewIndex: integer);
begin
  FList.Move(aCurrentIndex, aNewIndex);
end;

function TOGLCSurfaceList.Contain(aSurface: TSimpleSurfaceWithEffect): boolean;
var i: integer;
begin
  for i:=0 to FList.Count-1 do
    if TSimpleSurfaceWithEffect(FList[i]) = aSurface then exit(True);
  Result := False;
end;

procedure TOGLCSurfaceList.Delete(Index:integer);
begin
  FList.Delete(Index);
end;

procedure TOGLCSurfaceList.Clear;
var o: TSimpleSurface;
begin
  while FList.Count > 0 do
   begin
    o := TSimpleSurface(FList.Items[0]);
    o.Free;
    FList.Delete(0);
   end;
end;


{ TLayer }

constructor TLayer.Create;
begin
  inherited Create;
  FVisible := TRUE;
  Opacity := CreateBoundedFParam(0, 255);
  Opacity.Value := 255;
  PostProcessing.InitDefault;
end;

destructor TLayer.Destroy;
begin
  Opacity.Free;
  Opacity := NIL;
  inherited Destroy;
end;

procedure TLayer.Draw;
var i: integer;
    computedOpacity: single;
begin
  if not FVisible then exit;

  // post processing
//  if PostProcessing.FXBitMask <> 0 then
  if FParentScene.FPostProcessingEngine.FRenderToTexture <> NIL then
    FParentScene.FPostProcessingEngine.ApplyFX(@PostProcessing);

  computedOpacity := Opacity.Value*OneDiv255;

  // Before paint CallBack
  if Assigned(FOnBeforePaint) then FOnBeforePaint(Self, computedOpacity);

  if FCamera <> NIL then FCamera.Use;

  if computedOpacity > 0 then
    for i:=0 to FList.Count-1 do
     with TSimpleSurfaceWithEffect(FList.Items[i]) do
      if Visible then Draw(computedOpacity);

  if FCamera <> NIL then FCamera.Release;

  // After paint CallBack
  if Assigned(FOnAfterPaint) then FOnAfterPaint(Self);
end;

procedure TLayer.Update(const aElapsedTime: single);
var i: integer;
begin
  if FOnBeforeUpdate <> NIL then FOnBeforeUpdate(Self, aElapsedTime);
  if FFreeze then exit;
  // Do Blink
  with FBlink do
  if BlinkToGo <> 0 then begin
    if CountVisible>0 then begin
      // Layer is visible
      FVisible := TRUE;
      CountVisible -= aElapsedTime;
      CountInvisible := InvisibleTime;
    end else begin // Layer is invisible
      FVisible := FALSE;
      CountInvisible -= aElapsedTime;
      if CountInvisible <= 0 then begin
        CountVisible := VisibleTime;
        if BlinkToGo > 0 then dec(BlinkToGo);
        if BlinkToGo = 0 then FVisible := TRUE;
      end;
    end;
  end;

  Opacity.OnElapse(aElapsedTime);

  //for i:=0 to FList.Count-1 do
  for i:=FList.Count-1 downto 0 do
    with TSimpleSurfaceWithEffect(FList.Items[i]) do
      if not FKill then Update(aElapsedTime);

  // sort the surface
  if FOnSortCompare <> NIL then FList.Sort(FOnSortCompare);

  if FOnAfterUpdate <> NIL then FOnAfterUpdate(Self);
end;

procedure TLayer.Clear;
begin
  inherited Clear;
end;

function TLayer.GetViewRect: TRectF;
begin
  if FCamera <> NIL then Result := FCamera.GetViewRect
    else Result := RectF(0, 0, FParentScene.Width, FParentScene.Height);
end;

procedure TLayer.KillAll;
var i: integer;
begin
  for i:=0 to SurfaceCount-1 do Surface[i].Kill;
end;

procedure TLayer.Blink(aNumberOfBlink: integer; aVisibleTime, aInvisibleTime: single);
begin
  with FBlink do begin
    BlinkToGo := aNumberOfBlink;
    VisibleTime := aVisibleTime;
    InvisibleTime := aInvisibleTime;
    CountVisible := aVisibleTime;
    CountInvisible := aInvisibleTime;
  end;
end;

procedure TLayer.GroupKill(aGroup: integer);
var i: integer;
begin
  if aGroup = -1 then
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then Kill;
end;

procedure TLayer.GroupVisible (aGroup : integer; aValue:boolean);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then Visible := aValue;
end;

procedure TLayer.GroupFreeze(aGroup: integer; aValue:boolean);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then Freeze := aValue;
end;

procedure TLayer.GroupTint(aGroup: integer; const aTint: TBGRAPixel);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then Tint.Value := aTint;
end;

procedure TLayer.GroupMoveTo( aGroup: integer; aX, aY: single; aDuration: single; aCurve:Word);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then MoveTo(aX, aY, aDuration, aCurve);
end;

procedure TLayer.GroupMoveRelative(aGroup: integer; aDeltaX, aDeltaY: single; aDuration: single; aCurve: Word);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
    if (aGroup = -1) or (Group = aGroup) then MoveRelative(aDeltaX, aDeltaY, aDuration, aCurve);
end;

procedure TLayer.GroupBlink(aGroup: integer; aNumberOfBlink: integer; aVisibleTime, aInvisibleTime: single);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then Blink(aNumberOfBlink, aVisibleTime, aInvisibleTime);
end;

procedure TLayer.GroupScale(aGroup: integer; aHScale, aVScale: single; aDuration: single; aHScaleCurve:Word; aVScaleCurve: Word);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then begin
       Scale.x.ChangeTo(aHScale, aDuration, aHScaleCurve);
       Scale.y.ChangeTo(aVScale, aDuration, aVScaleCurve);
     end;
end;

procedure TLayer.GroupRotate(aGroup: integer; aAnglePerSecond: single);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then Angle.AddConstant(aAnglePerSecond);
end;

procedure TLayer.GroupOpacityVariation(aGroup: integer; aNewOpacity: byte; aDuration: single; aCurve: Word);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect( FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then Opacity.ChangeTo(aNewOpacity,aDuration,aCurve);
end;

procedure TLayer.GroupTintOpacityVariation(aGroup: integer; aNewOpacity: byte; aDuration: single; aCurve: Word);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then Tint.ChangeTo(aNewOpacity,aDuration,aCurve);
end;

procedure TLayer.GroupOpacity(aGroup: integer; aNewOpacity: byte);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then Opacity.Value := aNewOpacity;
end;

procedure TLayer.GroupFlipH(aGroup: integer; aValue: boolean);
var i: integer ;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect(FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then FlipH := aValue;
end;

procedure TLayer.GroupFlipV(aGroup: integer; aValue: boolean);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
   with TSimpleSurfaceWithEffect( FList.Items[i]) do
     if (aGroup = -1) or (Group = aGroup) then FlipV := aValue;
end;

function TLayer.CollisionTest(aSurface : TSimpleSurfaceWithEffect): TSimpleSurfaceWithEffect;
begin
  Result := CollisionTest(aSurface.X.Value, aSurface.Y.Value, aSurface.Width, aSurface.Height);
end;

function TLayer.CollisionTest(aSurface: TSimpleSurfaceWithEffect; HTolerance, VTolerance: single): TSimpleSurfaceWithEffect;
begin
  with aSurface do
    Result := CollisionTest(X.Value+HTolerance, Y.Value+VTolerance,
                            Width-HTolerance, Height-VTolerance, aSurface);
end;

function TLayer.CollisionTest(aX, aY, aWidth, aHeight: single): TSimpleSurfaceWithEffect;
var i: integer;
    r: TRectF;
begin
  r.Left := aX;
  r.Top := aY;
  r.Right := aX + aWidth;
  r.Bottom := aY + aHeight;

  for i:=0 to FList.Count-1 do
    if Collision.RectFRectF(Surface[i].GetScaledAreaF, r) then begin
      Result := Surface[i];
      exit;
    end;

  Result := NIL;
end;

function TLayer.CollisionTest(aX, aY, aWidth, aHeight: single; DontTestSurface: TSimpleSurfaceWithEffect): TSimpleSurfaceWithEffect;
var i: integer;
    r: TRectF;
    o: TSimpleSurfaceWithEffect;
begin
  r.Left := aX;
  r.Top := aY;
  r.Right := aX + aWidth;
  r.Bottom := aY + aHeight;

  for i:=0 to FList.Count-1 do begin
    o := Surface[i];
    if (o <> DontTestSurface) and Collision.RectFRectF(o.GetScaledAreaF, r) then begin
      Result := o;
      exit;
    end;
  end;

  Result := NIL;
end;

{ TLayerList }

constructor TLayerList.Create;
begin
  FList := TList.Create;
  SetLayerCount(1);
end;

destructor TLayerList.Destroy;
begin
  FList.Free;
  FList := NIL;
  inherited Destroy;
end;

procedure TLayerList.ClearAllLayer;
var i:integer;
begin
  for i:=0 to GetLayerCount-1 do Layer[i].Clear;
end;

procedure TLayerList.ClearLayer(aLayerIndex: integer);
begin
  TLayer(FList.Items[aLayerIndex]).Clear;
end;

function TLayerList.LayerIndexOf(aLayer: TLayer): integer;
var i: Integer;
begin
  Result := -1;
  for i:=0 to GetLayerCount-1 do
   if Layer[i] = aLayer then begin
    Result := i;
    break;
   end;
end;

function TLayerList.GetLayerCount: integer;
begin
  Result := FList.Count;
end;

function TLayerList.GetLayer(Index: integer): TLayer;
begin
  Result := TLayer(FList.Items[Index]);
end;

function TLayerList.GetSurfaceCount: integer;
var i: integer;
begin
  Result := 0;
  for i:=0 to FList.Count-1 do Result += TLayer(FList.Items[i]).GetSurfaceCount;
end;

procedure TLayerList.SetLayerCount(AValue: integer);
var i:integer;
begin
  if AValue = FList.Count then exit;
  if AValue > FList.Count then begin
    for i:=1 to AValue-FList.Count do
      FList.Add( TLayer.Create );
  end else begin
     while AValue < FList.Count do begin
       Layer[FList.Count-1].Free;
       FList.Delete(FList.Count-1);
     end;
  end;
end;

procedure TLayerList.AddSurfaceToLayer(aSurface : TSimpleSurfaceWithEffect; aLayerIndex:integer);
begin
  if aLayerIndex >= FList.Count then aLayerIndex:=FList.Count-1
    else if aLayerIndex < 0 then aLayerIndex := 0;
  Layer[aLayerIndex].Add(aSurface);
end;

procedure TLayerList.InsertSurfaceToLayer(aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer);
begin
  if aLayerIndex >= FList.Count then aLayerIndex := FList.Count-1
    else if aLayerIndex < 0 then aLayerIndex := 0;
  Layer[aLayerIndex].Insert(aSurfaceIndex, aSurface);
end;

procedure TLayerList.SurfaceChangeLayer(aSurface: TSimpleSurfaceWithEffect;
  aNewLayerIndex: integer);
var i: integer;
begin
  if (aNewLayerIndex < 0) or (aNewLayerIndex >= LayerCount) then exit;
  if aSurface = NIL then exit;
  if aSurface.FParentLayer = NIL then exit;
  if aSurface.FParentLayer = Layer[aNewLayerIndex] then exit;

  // push the 'ChangeLayer' request in list. it will be processed in the next scene update
  i := Length(FListSurfaceChangeLayer);
  SetLength(FListSurfaceChangeLayer, i+1);
  FListSurfaceChangeLayer[i].Surface := aSurface;
  FListSurfaceChangeLayer[i].NewLayerIndex := aNewLayerIndex;
end;

procedure TLayerList.AllLayerVisible;
var i:integer;
begin
  for i:=0 to FList.Count-1 do TLayer(FList.Items[i]).Visible := TRUE;
end;

procedure TLayerList.OnlyThisLayerVisible(const tab: array of integer);
var i:integer;
begin
  for i:=0 to FList.Count-1 do TLayer(FList.Items[i]).Visible := FALSE;
  for i:=low(tab) to high(tab) do
    if (tab[i] < FList.Count) and (tab[i] >= 0) then
      TLayer(FList.Items[tab[i]]).Visible := TRUE;
end;
{$endif oglcIMPLEMENTATION}


