{$ifdef oglcINTERFACE}
type

{ TSprite }

TSprite = class(TSimpleSurfaceWithEffect)
private
  FTextureOwner: boolean;
  FTexture: PTexture;
  FModelArea: TQuadCoor;
  procedure Init; virtual;
  procedure SetTexture(AValue: PTexture); virtual;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure DoDraw; override;
public
  // create a sprite from a texture.
  // if Owner is set to TRUE, the texture is freed when the sprite is killed.
  constructor Create(ATexture: PTexture; Owner: boolean=FALSE);
  // Draw aCaption in a texture and create a sprite with it.
  constructor Create(aParentScene: TOGLCScene; const aFont: TFontDescriptor; const aCaption: string; aFillTexture: TBGRABitmap=NIL);
  Destructor Destroy; override;
  property Texture: PTexture read FTexture write SetTexture;
end;
TArrayOfSprite = array of TSprite;
PArrayOfSprite = ^TArrayOfSprite;


{ TSpriteWithElasticCorner }

TSpriteWithElasticCorner = class(TSprite)
private
  procedure Init; override;
public
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
  Destructor Destroy; override;
public
  // the idea is to add offsets to the coordinates of the surface corners to allow deformations.
  // CornerOffset contains the offsets in pixels. Initial values are no offset.
  CornerOffset: TQuadParamF;
end;


{ TPolarSprite }

TPolarSprite = class(TSprite)
private
  procedure Init; override;
public
  Destructor Destroy; override;
  procedure Update(const aElapsedTime: single); override;
public
  // the idea is to set the center coordinate of the surface from polar coordinate.
  // Polar coordinates are defined with a distance and an angle, and are relative to a center point.
  // changing the distance and/or angle automatically moves the sprite accordingly
  // First initialize Polar.Center, then play with Polar.Distance and Polar.Angle
  Polar: TPolarSystemParam;
end;
TArrayOfTPolarSprite = array of TPolarSprite;


{ TShapeOutline }

TShapeOutline = class(TSimpleSurfaceWithEffect)
private
  FAntialiasing: boolean;
  FLineColor: TBGRAPixel;
  FLinePosition: TOGLCLinePosition;
  FLineWidth: single;
  FPath: TOGLCPath;
  FWidth, FHeight: integer;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure DoDraw; override;
  constructor Create(aParentScene: TOGLCScene);

  procedure SetShapeLine(pt1, pt2: TPointF);
  procedure SetShapeRectangle(aX, aY: single; aWidth, aHeight: integer);
  // (aX, aY) are the coordinates of the Top/Left of the ellipse
  procedure SetShapeEllipse(aX, aY: single; aWidth, aHeight: integer);
  procedure SetShapeCustom(aX, aY: single; aPath: TOGLCPath);
  // convenient method to set all the parameters in one call.
  procedure SetParam(aLineWidth: single; aLineColor: TBGRAPixel; aLinePosition: TOGLCLinePosition; aAntialiasing: boolean);
  property LineWidth: single read FLineWidth write FLineWidth;
  property LineColor: TBGRAPixel read FLineColor write FLineColor;
  // default is lpMiddle
  property LinePosition: TOGLCLinePosition read FLinePosition write FLinePosition;
  // Set to True if you want antialised outline (slower). Default is False.
  property Antialiasing: boolean read FAntialiasing write FAntialiasing;
end;

{ TMultiColorRectangle }

TMultiColorRectangle = class(TSimpleSurfaceWithEffect)
private
  FQuadCornerColors: TQuadColorF;
  FModelArea: TQuadCoor;
protected
  FWidth, FHeight: integer;
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  Constructor Create(aWidth, aHeight: integer);
  Destructor Destroy; override;
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
public
  TopLeftColor,
  TopRightColor,
  BottomRightColor,
  BottomLeftColor: TBGRAParam;
  procedure SetSize(aWidth, aHeight: integer);

  procedure SetAllColorsTo(AColor: TBGRAPixel);
  procedure SetLeftColors(AColor: TBGRAPixel);
  procedure SetRightColors(AColor: TBGRAPixel);
  procedure SetTopColors(AColor: TBGRAPixel);
  procedure SetBottomColors(AColor: TBGRAPixel);
end;


{ TSpriteContainer }
// a virtual sprite without texture but only childs surface.
// use it to compose a complex sprite made with several child surfaces ordered by ZOrder.
TSpriteContainer = class(TSimpleSurfaceWithEffect)
private
  FShowOrigin: boolean;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Draw(const aLayerPercentOpacity: single);  override;
  procedure DoDraw; override;
  procedure AddChild(ASurface: TSimpleSurfaceWithEffect; aZOrder: integer=0); override;
  constructor Create(aParentScene: TOGLCScene);
  property ShowOrigin: boolean read FShowOrigin write FShowOrigin;
end;


{ TMouseSatelliteSprite }
// Satellite sprite that turn like an electron in rectangular area around mouse cursor

TMouseSatelliteSprite = class(TSprite)
protected
  Accumulator: TPointF;
  procedure SetCoordinates; virtual;
public
  Pulse: TPointFParam; // sprite's speed
  MovingRectangle: TPointFParam; // width and height of the rectangle area where the sprite move
  PulseFactor: single;
  Constructor Create(ATexture: PTexture; Owner: boolean=FALSE);
  Destructor Destroy; override;
  procedure Update(const aElapsedTime: single); override;
end;


{ TOGLCPathToFollow }
// a surface that draw a path. The path can be assigned to severals instances of TSpriteOnPath(see below)
TOGLCPathToFollow = class(TSimpleSurfaceWithEffect)
private
  FPath: TOGLCPath;
  FDistanceAndAngle: ArrayOfTPointF; // contains x=distance to the next point, y=angle between current and next point
  FPathLength: single;
  FLoop: boolean;
  FWidth, FHeight: integer;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure DoDraw; override;
public
  Border: TOGLCBorder;
  constructor Create(aParentScene: TOGLCScene);
  // Init the surface with the specified path.
  procedure InitFromPath(const aPath: TOGLCPath; aShiftPathToOrigin: boolean=True);
  // return TRUE if the distance traveled value is on the path
  // 0 <= aDis < PathLength
  function IsOnThePath(aDis: single): boolean;
  // adjust the distance traveled
  function TruncDistanceTraveled(aDis: single): single;
  // return the position and angle(direction in degree) from a distance traveled
  procedure GetPosAndAngle(aDistanceTraveled: single; out aPos: TPointF; out aAngle: single);

  property Loop: boolean read FLoop write FLoop;
  // return the total path length in pixel
  property PathLength: single read FPathLength;
end;


{ TSpriteOnPathToFollow }
// a sprite that moves along a TOGLCPathToFollow instance
TSpriteOnPathToFollow = class(TSprite)
private
  FAutoRotate: boolean;
  FAngleAdjust,
  FCoeffPositionOnPath: single;
  procedure SetCoeffPositionOnLine(AValue: single);
public
  procedure Draw(const aLayerPercentOpacity: single);  override;
  procedure Update(const aElapsedTime: single); override;
public
  // Distance traveled on path. Use this property to move the sprite along the path.
  DistanceTraveled: TFParam;
  // This sprite is created as a child of an TOGLCPathToFollow instance.
  // A positive ZOrder means the sprite is above its parent, negative value means the sprite is under its parent.
  Constructor CreateAsChildOf(aPathToFollow: TOGLCPathToFollow; ATexture: PTexture; Owner: boolean=FALSE; aZOrder: integer=0);
  Destructor Destroy; override;

  // 'TRUE' force the sprite to rotate on the right direction along the path. Default is TRUE.
  property AutoRotate: boolean read FAutoRotate write FAutoRotate;
  // Set this value to ajust the right orientation of the sprite. Default is '0'.
  property AngleAdjust: single read FAngleAdjust write FAngleAdjust;
  // Allow to control the position of the sprite on the path. Range is from 0 to 1. Default value is 0.5
  //   0 -> the sprite is above the path
  // 0.5 -> the sprite is centered on the path
  //   1 -> the sprite is below the path
  property CoeffPositionOnPath: single read FCoeffPositionOnPath write SetCoeffPositionOnLine;
end;

{ TOGLCPathDrawing }

TOGLCPathDrawing = class(TSimpleSurfaceWithEffect)
private
 { TODO : intégrer les pointillés à TOGLCBorder puis supprimer TOGLCPathDrawing }
  FDottedLine: ArrayOfSingle;
  FPathToFollow: TOGLCPathToFollow;
  FLineWidth: single;
  FWidth, FHeight: integer;
protected
 function GetWidth: integer; override;
 function GetHeight: integer; override;
 procedure InternalDraw(const pt1, pt2: TPointF); virtual;
 function GetPoint(aIndex: integer): TPointF; virtual;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single ); override;
public
 // the virtual distance traveled on the path.
 // virtual because it can be higher than path length when path is looped
  DistanceTraveled: TFParam;
  LineColor: TBGRAParam;
  DottedLineOffset: TFParam;

  Constructor Create;
  Destructor Destroy; override;
  // assign a path to draw
  // if Owner is TRUE, the path is automatically destroyed when the sprite is killed
  procedure AssignPath(aPath: TOGLCPathToFollow);
  // because DistanceTraveled can be higher than path length (in case where the path is looped)
  // this function return the real position on the path
  function RealDistanceTraveled: single;

  // Use this method to define how to draw lines on the path:
  // [n1, n2, n3, n4,...]   n1,n3... are the length of the drawn part  n2,n4,... the length of holes
  // use [] for solid line (default)
  procedure SetDottedLine(const AArray: array of single);
  property LineWidth: single read FLineWidth write FLineWidth;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TSprite }

procedure TSprite.Init;
begin
end;

procedure TSprite.SetTexture(AValue: PTexture);
begin
  if FTexture = AValue then Exit;
  if FTextureOwner then FParentScene.TexMan.Delete(FTexture);
  FTexture := AValue;

  if FTexture <> NIL then FModelArea.SetAsRectangle(FTexture^.FrameWidth, FTexture^.FrameHeight)
    else FModelArea.SetAsEmpty;
end;

function TSprite.GetWidth: integer;
begin
  if FTexture = NIL then
    Result := 0
  else
    Result := FTexture^.FrameWidth;
end;

function TSprite.GetHeight: integer;
begin
  if FTexture = NIL then
    Result := 0
  else
    Result := FTexture^.FrameHeight;
end;

procedure TSprite.DoDraw;
var fi: Int64;
begin
  if FTexture = NIL then exit;

  fi := EnsureRange(Trunc(Frame), 0, High(FTexture^.FramesCoord));
  FParentScene.TexturedMVTriangleRenderer.Prepare(ptTriangleStrip, FTexture, 0, BlendMode);
  FParentScene.TexturedMVTriangleRenderer.PushQuad2(FModelArea, FTexture^.FramesCoord[fi], FlipToIndex,
                                                    NIL, FComputedOpacity, FComputedTint);
end;

constructor TSprite.Create(ATexture: PTexture; Owner: boolean);
begin
  inherited Create;
  FTextureOwner := Owner;
  FTexture := ATexture;
  if FTexture <> NIL then FModelArea.SetAsRectangle(FTexture^.FrameWidth, FTexture^.FrameHeight)
    else FModelArea.SetAsEmpty;
  Init;
end;

constructor TSprite.Create(aParentScene: TOGLCScene; const aFont: TFontDescriptor; const aCaption: string; aFillTexture: TBGRABitmap);
var ima: TBGRABitmap;
begin
  ima := aFont.StringToBitmap(aCaption, aFillTexture);
  Create(aParentScene.TexMan.Add(ima), True);
  ima.Free;
end;

destructor TSprite.Destroy;
begin
  if FTextureOwner then FParentScene.TexMan.Delete(FTexture);
  inherited Destroy;
end;

{ TSpriteWithElasticCorner }

procedure TSpriteWithElasticCorner.Init;
begin
  inherited Init;
  CornerOffset := TQuadParamF.Create;
end;

procedure TSpriteWithElasticCorner.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  CornerOffset.OnElapse(aElapsedTime);
end;

procedure TSpriteWithElasticCorner.DoDraw;
var modelArea: TQuadCoor;
  fi: integer;
begin
  if FTexture = NIL then exit;

  fi := EnsureRange(Trunc(Frame), 0, High(FTexture^.FramesCoord));
  {%H-}modelArea.CopyFrom(FModelArea);
  modelArea.AddOffset(CornerOffset);

  FParentScene.TexturedMVTriangleRenderer.Prepare(ptTriangleStrip, FTexture, 0, FBlendMode);
  FParentScene.TexturedMVTriangleRenderer.PushQuad2(modelArea, FTexture^.FramesCoord[Trunc(fi)], FlipToIndex,
                                                    NIL, FComputedOpacity, FComputedTint);
end;

destructor TSpriteWithElasticCorner.Destroy;
begin
  CornerOffset.Free;
  CornerOffset := NIL;
  inherited Destroy;
end;

{ TShapeOutline }

function TShapeOutline.GetWidth: integer;
begin
  Result := FWidth;
end;

function TShapeOutline.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TShapeOutline.DoDraw;
var c: TBGRAPixel;
begin
  if (FLineWidth <= 0) or (FLineColor.alpha = 0) or (Length(FPath) = 0) then exit;
  if FAntialiasing then begin
    FParentScene.SmoothLineRenderer.PushPath(FPath, FLinePosition, FLineColor, FLineWidth, FComputedOpacity, False);
  end else begin
    c := FLineColor;
    c.alpha := Trunc(c.alpha*FComputedOpacity);
    if c.alpha = 0 then exit;
    FParentScene.FastLineRenderer.Prepare(FParentScene.MVPMatrix, FBlendMode);
    FParentScene.FastLineRenderer.PushPath(FPath, c, FLineWidth, FLinePosition);
  end;
end;

constructor TShapeOutline.Create(aParentScene: TOGLCScene);
begin
  inherited Create;
  FParentScene := aParentScene;
  FLineWidth := 1.0;
  FLineColor := BGRA(30,30,30);
end;

procedure TShapeOutline.SetShapeLine(pt1, pt2: TPointF);
begin
  SetCoordinate(pt1);
  FPath.CreateLine((PointF(0,0)), (pt2-pt1));
  FWidth := Trunc(Abs(pt1.x-pt2.x));
  FHeight := Trunc(Abs(pt1.y-pt2.y));
end;

procedure TShapeOutline.SetShapeRectangle(aX, aY: single; aWidth, aHeight: integer);
begin
  FPath.CreateRect(0, 0, aWidth, aHeight, True);
  FWidth := aWidth;
  FHeight := aHeight;
  SetCoordinate(aX, aY);
end;

procedure TShapeOutline.SetShapeEllipse(aX, aY: single; aWidth, aHeight: integer);
begin
  FPath.CreateEllipse(aWidth*0.5, aHeight*0.5, aWidth*0.5, aHeight*0.5, True);
  FWidth := aWidth;
  FHeight := aHeight;
  SetCoordinate(aX, aY);
end;

procedure TShapeOutline.SetShapeCustom(aX, aY: single; aPath: TOGLCPath);
var r: TRectF;
begin
  FPath := NIL;
  SetLength(FPath, Length(aPath));
  Move(aPath[0], FPath[0], Length(aPath));
  r := FPath.Bounds;
  FWidth := Round(r.Width);
  FHeight := Round(r.Height);
  SetCoordinate(aX, aY);
end;

procedure TShapeOutline.SetParam(aLineWidth: single; aLineColor: TBGRAPixel;
  aLinePosition: TOGLCLinePosition; aAntialiasing: boolean);
begin
  LineWidth := aLineWidth;
  LineColor := aLineColor;
  LinePosition := aLinePosition;
  Antialiasing := aAntialiasing;
end;

{ TMultiColorRectangle }

constructor TMultiColorRectangle.Create(aWidth, aHeight: integer);
begin
  inherited Create;
  SetSize(aWidth, aHeight);

  TopLeftColor := TBGRAParam.Create;
  TopRightColor := TBGRAParam.Create;
  BottomRightColor := TBGRAParam.Create;
  BottomLeftColor := TBGRAParam.Create;

  TopLeftColor.Value := BGRA(255,0,0);
  TopRightColor.Value := BGRA(0,255,0);
  BottomRightColor.Value := BGRA(0,0,255);
  BottomLeftColor.Value := BGRA(255,255,0);
end;

destructor TMultiColorRectangle.Destroy;
begin
  TopLeftColor.Free;
  TopLeftColor := NIL;
  TopRightColor.Free;
  TopRightColor := NIL;
  BottomRightColor.Free;
  BottomRightColor := NIL;
  BottomLeftColor.Free;
  BottomLeftColor := NIL;
  inherited Destroy;
end;

function TMultiColorRectangle.GetWidth: integer;
begin
  Result := FWidth;
end;

function TMultiColorRectangle.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TMultiColorRectangle.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if not FFreeze then begin
    TopLeftColor.OnElapse(aElapsedTime);
    TopRightColor.OnElapse(aElapsedTime);
    BottomRightColor.OnElapse(aElapsedTime);
    BottomLeftColor.OnElapse(aElapsedTime);
  end;
end;

procedure TMultiColorRectangle.DoDraw;
begin
  // put the colors in a array to allow flip
  FQuadCornerColors[0].InitFromBGRA(TopLeftColor.Value);
  FQuadCornerColors[1].InitFromBGRA(TopRightColor.Value);
  FQuadCornerColors[2].InitFromBGRA(BottomRightColor.Value);
  FQuadCornerColors[3].InitFromBGRA(BottomLeftColor.Value);

  FParentScene.ThreeColorMVTriangleRenderer.Prepare(ptTriangleStrip, ParentScene.ProjectionMatrix, FBlendMode);
  FParentScene.ThreeColorMVTriangleRenderer.PushQuad(FModelArea, FQuadCornerColors, FlipToIndex, NIL, FComputedOpacity, FComputedTint);
end;

procedure TMultiColorRectangle.SetSize(aWidth, aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
  FModelArea.SetAsRectangle(aWidth, aHeight);
end;

procedure TMultiColorRectangle.SetAllColorsTo(AColor: TBGRAPixel);
begin
  TopLeftColor.Value := AColor;
  TopRightColor.Value := AColor;
  BottomRightColor.Value := AColor;
  BottomLeftColor.Value := AColor;
end;

procedure TMultiColorRectangle.SetLeftColors(AColor: TBGRAPixel);
begin
  TopLeftColor.Value := AColor;
  BottomLeftColor.Value := AColor;
end;

procedure TMultiColorRectangle.SetRightColors(AColor: TBGRAPixel);
begin
  TopRightColor.Value := AColor;
  BottomRightColor.Value := AColor;
end;

procedure TMultiColorRectangle.SetTopColors(AColor: TBGRAPixel);
begin
  TopLeftColor.Value := AColor;
  TopRightColor.Value := AColor;
end;

procedure TMultiColorRectangle.SetBottomColors(AColor: TBGRAPixel);
begin
  BottomRightColor.Value := AColor;
  BottomLeftColor.Value := AColor;
end;

{ TSpriteContainer }

function TSpriteContainer.GetWidth: integer;
begin
  Result := 1;
end;

function TSpriteContainer.GetHeight: integer;
begin
  Result := 1;
end;

procedure TSpriteContainer.Draw(const aLayerPercentOpacity: single);
begin
  inherited Draw(aLayerPercentOpacity);
  if FShowOrigin then begin
    PushAndPrepareSceneMV;
    FillBox(FParentScene, -2,-2,4,4,BGRA(255,0,0));
    PopSceneMV;
  end;
end;

procedure TSpriteContainer.DoDraw;
begin
end;

procedure TSpriteContainer.AddChild(ASurface: TSimpleSurfaceWithEffect; aZOrder: integer);
begin
  inherited AddChild(ASurface, aZOrder);
  ASurface.FReferenceSurfaceForSymmetry := Self;
end;

constructor TSpriteContainer.Create(aParentScene: TOGLCScene);
begin
  inherited Create;
  FParentScene := aParentScene;
  ChildsUseParentOpacity := True;
end;

{ TPolarSprite }

procedure TPolarSprite.Init;
begin
  inherited Init;
  Polar := TPolarSystemParam.Create;
end;

destructor TPolarSprite.Destroy;
begin
  Polar.Free;
  inherited Destroy;
end;

procedure TPolarSprite.Update(const aElapsedTime: single);
begin
  if not FFreeze then begin
    Polar.OnElapse(aElapsedTime);
    SetCenterCoordinate(Polar.CartesianValue);
  end;
  inherited Update(aElapsedTime);
end;

{ TMouseSatelliteSprite }

procedure TMouseSatelliteSprite.SetCoordinates;
begin
  with FParentScene do begin
    CenterX := cos(Accumulator.x) * MovingRectangle.x.Value + Mouse.Position.x;
    CenterY := sin(Accumulator.y) * MovingRectangle.y.Value + Mouse.Position.y;
  end;
end;

constructor TMouseSatelliteSprite.Create( ATexture: PTexture; Owner: boolean);
begin
  inherited Create(ATexture, Owner);
  Pulse := TPointFParam.Create;
  Pulse.Value := PointF(PI*1.17, pi*0.777);
  MovingRectangle := TPointFParam.Create;
  MovingRectangle.Value := PointF(50, 50);
  PulseFactor := 1.0;
  Angle.AddConstant(360);
end;

destructor TMouseSatelliteSprite.Destroy;
begin
  Pulse.Free;
  MovingRectangle.Free;
  inherited Destroy;
end;

procedure TMouseSatelliteSprite.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if FFreeze then exit;
  Pulse.OnElapse(aElapsedTime);
  Accumulator.x += Pulse.x.Value * PulseFactor * AElapsedTime;
  Accumulator.y += Pulse.y.Value * PulseFactor * AElapsedTime;
  SetCoordinates;
end;

{ TOGLCPathToFollow }

function TOGLCPathToFollow.IsOnThePath(aDis: single): boolean;
begin
  Result := (aDis >= 0) and (aDis < FPathLength);
end;

function TOGLCPathToFollow.TruncDistanceTraveled(aDis: single): single;
begin
  if FPathLength <= 0 then begin
    Result := 0;
    exit;
  end;

  Result := aDis;
  if Result >= FPathLength then begin
    if Loop then begin
      repeat
       Result := Result - FPathLength;
      until Result < FPathLength;
    end else Result := FPathLength;
  end else if Result < 0 then begin
    if Loop then begin
      repeat
       Result := Result + FPathLength;
      until Result > 0;
    end else Result := 0;
  end;
end;

procedure TOGLCPathToFollow.GetPosAndAngle(aDistanceTraveled: single; out aPos: TPointF; out aAngle: single);
var i: integer;
  d, dnext: single;
  p: TPolarCoor;
begin
  aDistanceTraveled := TruncDistanceTraveled(aDistanceTraveled);

  if aDistanceTraveled = PathLength then begin
    aPos := FPath[high(FPath)];
    aAngle := FDistanceAndAngle[high(FPath)].y;
    exit;
  end;

  d := 0;
  for i:=1 to high(FPath) do begin
    dnext := d + FDistanceAndAngle[i].x;
    if (aDistanceTraveled >= d) and (aDistanceTraveled < dnext) then begin
      aAngle := FDistanceAndAngle[i].y;
      p := CartesianToPolar(FPath[i-1], FPath[i]);
      p.Distance := aDistanceTraveled-d;
      aPos := PolarToCartesian(FPath[i-1], p);
      exit;
    end;
    d := dnext;
  end;
end;

function TOGLCPathToFollow.GetWidth: integer;
begin
  Result := FWidth;
end;

function TOGLCPathToFollow.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TOGLCPathToFollow.DoDraw;
begin
  // render border
  if Border.Visible and (Border.VertexCount > 0) then
    FParentScene.SmoothLineRenderer.DrawBorder(Border, FComputedOpacity, FComputedTint);
end;

constructor TOGLCPathToFollow.Create(aParentScene: TOGLCScene);
begin
  inherited Create;
  FParentScene := aParentScene;
  Border.InitDefault;
  Border.Width := 2.5;
end;

procedure TOGLCPathToFollow.InitFromPath(const aPath: TOGLCPath; aShiftPathToOrigin: boolean);
var i: Integer;
  p: TPolarCoor;
  r: TRectF;
begin
  FPathLength := 0;

  FPath := Copy(aPath);
  r := FPath.Bounds;
  if ((r.Left <> 0) or (r.Top <> 0)) and aShiftPathToOrigin then begin
    FPath.Translate(PointF(-r.Left, -r.Top));
    r.Offset(-r.Left, -r.Top);
  end;
  FWidth := Round(r.Width);
  FHeight := Round(r.Height);

  // computes total path length, and the distance and angle (direction) between each node
  SetLength(FDistanceAndAngle, Length(FPath));
  FDistanceAndAngle[0] := PointF(0,0);
  for i:=1 to high(FPath) do begin
    p := CartesianToPolar(FPath[i-1], FPath[i]);
    FDistanceAndAngle[i] := PointF(p.Distance, p.Angle);
    FPathLength := FPathLength + p.Distance;
  end;

  Border.ClearArrays;
  Border.LinePosition := lpMiddle;
  Border.InitFromPath(FPath, Border.Width);
end;

{ TSpriteOnPathToFollow }

constructor TSpriteOnPathToFollow.CreateAsChildOf(aPathToFollow: TOGLCPathToFollow; ATexture: PTexture;
  Owner: boolean; aZOrder: integer);
begin
  inherited Create(ATexture, Owner);
  DistanceTraveled := TFParam.Create;
  AutoRotate := TRUE;
  aPathToFollow.AddChild(Self, aZOrder);
  FCoeffPositionOnPath := 0.5;
end;

destructor TSpriteOnPathToFollow.Destroy;
begin
  DistanceTraveled.Free;
  DistanceTraveled := NIL;
  inherited Destroy;
end;

procedure TSpriteOnPathToFollow.SetCoeffPositionOnLine(AValue: single);
begin
  FCoeffPositionOnPath := EnsureRange(AValue, 0.0, 1.0);
end;

procedure TSpriteOnPathToFollow.Draw(const aLayerPercentOpacity: single);
var m: TOGLCMatrix;
  fi: Int64;
  pos: TPointF;
  ang, yOffset: single;
begin
  FComputedOpacity := aLayerPercentOpacity*Opacity.Value*OneDiv255;
  if FComputedOpacity = 0 then exit;
  FComputedTint.InitFromTint(TintMode, Tint.Value);

  // render childs with negative ZOrder
  if (FChilds.Count > 0) and (TSimpleSurfaceWithEffect(FChilds[0]).FZOrderAsChild < 0) then begin
    PushAndPrepareSceneMV;
    if FChildsUseParentOpacity then DrawChildSurfacesWithNegativeZOrder(FComputedOpacity)
      else DrawChildSurfacesWithNegativeZOrder(aLayerPercentOpacity);
    PopSceneMV;
  end;

  if FTexture <> NIL then begin
    yOffset := FTexture^.FrameHeight - FTexture^.FrameHeight*FCoeffPositionOnPath;
    TOGLCPathToFollow(FParentSurface).GetPosAndAngle(DistanceTraveled.Value, pos, ang);
    m.CopyFrom(FParentScene.ModelViewMatrix);
    with m do begin
      Translate(pos.x, pos.y);
      Scale(Self.Scale.x.Value, Self.Scale.y.Value);
      if FAutoRotate then Rotate(ang + FAngleAdjust);
      Translate(0, -yOffset);
    end;

    fi := EnsureRange(Trunc(Frame), 0, High(FTexture^.FramesCoord));
    FParentScene.TexturedMVTriangleRenderer.Prepare(ptTriangleStrip, FTexture, 0, FBlendMode);
    FParentScene.TexturedMVTriangleRenderer.PushQuad2(FModelArea, FTexture^.FramesCoord[fi], FlipToIndex,
                                                      @m.Matrix, FComputedOpacity, FComputedTint);
  end;

  // render childs with positive ZOrder
  if FChilds.Count > 0 then begin
    PushAndPrepareSceneMV;
    if FChildsUseParentOpacity then DrawChildSurfacesWithPositiveZOrder(FComputedOpacity)
      else DrawChildSurfacesWithPositiveZOrder(aLayerPercentOpacity);
    PopSceneMV;
  end;
end;

procedure TSpriteOnPathToFollow.Update(const aElapsedTime: single);
var p: TPointF;
  a: single;
begin
  inherited Update(aElapsedTime);
  if Freeze then exit;

  DistanceTraveled.OnElapse(aElapsedTime);
  TOGLCPathToFollow(FParentSurface).GetPosAndAngle(DistanceTraveled.Value, p, a);
  CenterX := p.x;
  CenterY := p.y;
  if FAutoRotate then
    Angle.Value := a + FAngleAdjust;
end;

{ TOGLCPathDrawing }

procedure TOGLCPathDrawing.SetDottedLine(const AArray: array of single);
var i: Integer;
  s: single;
begin
  SetLength(FDottedLine, Length(AArray));
  s := 0;
  for i:=0 to high(AArray) do begin
   FDottedLine[i] := AArray[i];
   s += AArray[i];
  end;
  if s = 0 then SetLength(FDottedLine, 0);
end;

function TOGLCPathDrawing.GetWidth: integer;
begin
  Result := FWidth;
end;

function TOGLCPathDrawing.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TOGLCPathDrawing.InternalDraw(const pt1, pt2: TPointF);
begin
  DrawLine(FParentScene, pt1, pt2, LineColor.Value, FLineWidth{, FComputedOpacity});
end;

function TOGLCPathDrawing.GetPoint(aIndex: integer): TPointF;
begin
  Result := FPathToFollow.FPath[aIndex];
end;

procedure TOGLCPathDrawing.Update(const aElapsedTime: single);
begin
  inherited Update(AElapsedTime);
  if FFreeze then exit;

  DistanceTraveled.OnElapse(AElapsedTime);
  LineColor.OnElapse(AElapsedTime);
end;

procedure TOGLCPathDrawing.Draw(const aLayerPercentOpacity: single);
var i: integer;
  p1,p2: TPointF;
  d, d1: single;
  po: TPolarCoor;
  iDL, lp: integer;
  DLRemainingDistance: single;
  DLFlag, Notdotted: boolean;

  procedure NextIndex;
  begin
   repeat
    inc(iDL);
    if iDL > High(FDottedLine) then iDL := 0;
    DLRemainingDistance := FDottedLine[iDL];
    DLFlag := not DLFlag;
   until DLRemainingDistance <> 0;
  end;

  procedure DrawDottedLine;
  var p3: TPointF;
  begin
   if DLRemainingDistance >= po.Distance then begin
     if DLFlag then begin
       InternalDraw(p1, PolarToCartesian(p1, po));
     end;
     DLRemainingDistance -= po.Distance;
     if DLRemainingDistance = 0 then NextIndex;
   end else begin
       p3:= PolarToCartesian(p1, PolarCoor(DLRemainingDistance, po.Angle));
       if DLFlag then InternalDraw(p1, p3);
       p1 := p3;
       po.Distance := po.Distance-DLRemainingDistance;
       NextIndex;
       DrawDottedLine;
   end;
  end;

begin
  if not FVisible then exit;
  if Opacity.Value = 0 then exit;

  if FPathToFollow = NIL then exit;
  lp := Length(FPathToFollow.FPath);
  if lp < 2 then exit;
  if DistanceTraveled.Value < 0 then exit;

  FParentScene.BlendMode := FBlendMode;
//  PushAndPrepareMatrix;
  FParentScene.TexMan.UnbindTexture;
  glLineWidth(FLineWidth);

  d := 0;
  p1 := GetPoint(0);
  i := 1;

  Notdotted := Length(FDottedLine) = 0;
  if not Notdotted then begin //Length(FDottedLine)>0 then begin
    iDL := 0;
    DLRemainingDistance := FDottedLine[0];
    DLFlag := TRUE;
  end;

  repeat
    p2 := GetPoint(i);
    d1 := d;
    d += FPathToFollow.FDistanceAndAngle[i].x;
    if DistanceTraveled.Value >= d then begin // draw whole line segment
      if Notdotted
        then InternalDraw(p1, p2)
        else begin
          po := CartesianToPolar(p1, p2);
          DrawDottedLine;
        end;
    end else begin // draw only part of line segment
        po := CartesianToPolar(p1, p2);
        po.Distance := DistanceTraveled.Value - d1;
        if Notdotted
          then InternalDraw(p1, PolarToCartesian(p1, po))
          else DrawDottedLine;
    end;
    if d > DistanceTraveled.Value then break;

    p1 := p2;
    inc(i);
  until i = lp;

// DrawBox(FPath.RectArea.Left, FPath.RectArea.Top, FPath.RectArea.Width, FPath.RectArea.Height, BGRA(255,0,0),2);

  DrawChildSurfacesWithPositiveZOrder(aLayerPercentOpacity * Opacity.pcValue);
//  PopMatrix;
end;

constructor TOGLCPathDrawing.Create;
begin
  inherited Create;
  DistanceTraveled := TFParam.Create;
  LineColor := TBGRAParam.Create;
  LineColor.Value := BGRAWhite;
  FLineWidth := 1.5;
end;

destructor TOGLCPathDrawing.Destroy;
begin
  DistanceTraveled.Free;
  DistanceTraveled := NIL;
  LineColor.Free;
  LineColor := NIL;
  inherited Destroy;
end;

procedure TOGLCPathDrawing.AssignPath(aPath: TOGLCPathToFollow);
begin
  FPathToFollow := aPath;
  FWidth := FPathToFollow.Width;
  FHeight := FPathToFollow.Height;
end;

function TOGLCPathDrawing.RealDistanceTraveled: single;
begin
  Result := FPathToFollow.TruncDistanceTraveled(DistanceTraveled.Value);
end;

{$endif oglcIMPLEMENTATION}

