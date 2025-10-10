{$ifdef oglcINTERFACE}
type

{ TOGLCDecorItem }

TOGLCDecorItem = record
  texture: PTexture;
  x, y, w, h, pivotX, pivotY, angle, opacity: single;
  flipIndex, frameindex: integer;
  computedtint: TColorF;
  // return the layer index
  function LoadFromString(aAtlas: TAtlas; const s: string): integer;
end;

{ TOGLCDecorList }

TOGLCDecorList = class(TSimpleSurfaceWithEffect)
private
 type
  TDecorItemList = class(specialize TVector<TOGLCDecorItem>);
  var
  FItems: TDecorItemList;
  FLayerIndex: integer;
  procedure Add(const aItem: TOGLCDecorItem);
private
  FWorldArea: TRectF;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  //procedure Draw(const aLayerPercentOpacity: single);  override;
  procedure DoDraw; override;
public
  constructor Create(aParentScene: TOGLCScene; aLayerIndex: integer; const aWorldArea: TRectF);
  destructor Destroy; override;

  // The target layer index where to render the decor.
  property LayerIndex: integer read FLayerIndex;
end;


{ TOGLCDecorManager }

TOGLCDecorManager = class
private type TDecors = class(specialize TVector<TOGLCDecorList>);
private class var FAtlas: TAtlas;
private
  FParentScene: TOGLCScene;
  FDecors: TDecors;
  FWorldArea: TRectF;
  FSkyGradient: TGradientRectangle;
  function GetDecorWithLayerIndexOf(aLayerIndex: integer): TOGLCDecorList;
  procedure Clear;
protected
  function ScaleWF(AValue: single): single; virtual; abstract;
  function ScaleHF(AValue: single): single; virtual; abstract;
public
  class procedure LoadTexture(aAtlas: TAtlas); virtual;
  constructor Create(aParentScene: TOGLCScene);
  destructor Destroy; override;

  procedure DoBuildLevel(const s: string);

  // The coordinates and size of the world for the current level
  property WorldArea: TRectF read FWorldArea;
  // The gradient instance to render the sky.
  // If the level don't define a gradient for the sky, this property is NIL
  property SkyGradient: TGradientRectangle read FSkyGradient;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCDecorItem }

function TOGLCDecorItem.LoadFromString(aAtlas: TAtlas; const s: string): integer;
var prop: TProperties;
  texName: string;
  c: TBGRAPixel;
  tm: integer;
  v: single;
begin
  Result := 0;
  Self := Default(TOGLCDecorItem);
  prop.Split(s, ',');
  texName := '';
  tm := 0;
  prop.IntegerValueOf('L', Result, 0);
  prop.StringValueOf('N', texName, '');
  texture := aAtlas.RetrieveTextureByFileName(texName);
  prop.SingleValueOf('X', x, 0);
  prop.SingleValueOf('Y', y, 0);
  prop.SingleValueOf('W', w, 0);
  prop.SingleValueOf('H', h, 0);
  prop.SingleValueOf('PX', pivotX, 0.5);
  prop.SingleValueOf('PY', pivoty, 0.5);
  prop.SingleValueOf('A', angle, 0);
  prop.SingleValueOf('O', opacity, 255);
  opacity := opacity * OneDiv255;
  prop.IntegerValueOf('F', flipIndex, 0);

  c := BGRA(0, 0, 0, 0);
  prop.BGRAPixelValueOf('T', c, c);
  prop.IntegerValueOf('M', tm, Ord(tmReplaceColor));
  computedtint.InitFromTint(TTintMode(tm), c);

  v := 1.0;
  prop.SingleValueOf('I', v, v);
  frameindex := Trunc(v);
end;

{ TOGLCDecorList }

procedure TOGLCDecorList.Add(const aItem: TOGLCDecorItem);
begin
  FItems.PushBack(aItem);
end;

function TOGLCDecorList.GetWidth: integer;
begin
  Result := 1;
end;

function TOGLCDecorList.GetHeight: integer;
begin
  Result := 1;
end;

{procedure TOGLCDecorList.Draw(const aLayerPercentOpacity: single);
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
end;  }

procedure TOGLCDecorList.DoDraw;
var i: SizeUInt;
  modelArea: TQuadF;
  fi: integer;
  viewRect: TRectF;
  oldMatrix: TOGLCMatrix;
  newComputedOpacity, v: single;
  offsetCoor: TPointF;
  itemIndex: integer;
begin
  if FItems.Size = 0 then exit;
  //retrieve the view offset and size
  viewRect := ParentLayer.GetViewRect;

  // adjust the view rect to the world bounds (loop the view)
  v := viewRect.Width;
  while viewRect.Left < FWorldArea.Left do viewRect.Left := viewRect.Left + FWorldArea.Width;
  while viewRect.Left > FWorldArea.Right do viewRect.Left := viewRect.Left - FWorldArea.Width;
{  v := viewRect.Width;
  viewRect.Left := Frac(viewRect.Left / FWorldArea.Width) * FWorldArea.Width;
  viewRect.Width := v;
  v := viewRect.Height;
  viewRect.Top := Frac(viewRect.Top / FWorldArea.Height) * FWorldArea.Height;
  viewRect.Height := v;   }

  // retrieve the first item visible
  itemIndex := -1;
  for i:=0 to FItems.Size-1 do begin
    with FItems.Mutable[i]^ do
      {%H-}modelArea.SetAsRectangle(x, y, w, h);
      if modelArea.isVisibleIn(viewRect) then begin
        itemIndex := i;
        break;
      end;
  end;

  if itemIndex = -1 then begin
    itemIndex := 0;  // devrait être l'index du premier element visible (qui n'est pas forcément 0)
   // offsetCoor := PointF(FItems.Mutable[i]^.x-(FWorldArea.Right-viewRect.Left), 0);
    //offsetCoor.x := viewRect.Width;
    offsetCoor.x := Min(viewRect.Width, FWorldArea.Width);

    offsetCoor.y := 0;
  end else offsetCoor := PointF(0, 0);

  while (itemIndex < FItems.Size) and
        (FItems.Mutable[itemIndex]^.x+offsetCoor.x - viewRect.Left < viewRect.Width)
        {(FItems.Mutable[itemIndex]^.y+offsetCoor.y - viewRect.Top < viewRect.Height)}  do begin

    // render the item
    with FItems.Mutable[itemIndex]^ do begin
      newComputedOpacity := FComputedOpacity * opacity;
      if newComputedOpacity = 0 then continue;

      modelArea.SetAsRectangle(0, 0, w, h);

      // save and prepare matrix
      oldMatrix.CopyFrom(FParentScene.ModelViewMatrix);
      FParentScene.ModelViewMatrix.AddTransform(x+offsetCoor.x, y+offsetCoor.y, 1.0, 1.0,
                         angle, w*pivotX, h*pivotY);
      // frame index
      fi := EnsureRange(frameindex, 0, High(texture^.FramesCoord));
      // render the quad
    {  FParentScene.NoFilterTexturedMVTriangleRenderer.Prepare(ptTriangleStrip, texture, 0, BlendMode);
      FParentScene.NoFilterTexturedMVTriangleRenderer.PushQuad2(modelArea, texture^.FramesCoord[fi], flipIndex,
                                                     NIL, newComputedOpacity, computedtint);  }
      with FParentScene.TexturedMVTriangleRenderer do begin
        Prepare(ptTriangleStrip, texture, 0, BlendMode);
        PushQuad2(modelArea, texture^.FramesCoord[fi], flipIndex, NIL, newComputedOpacity, computedtint);
      end;

      // restore matrix
      FParentScene.ModelViewMatrix.CopyFrom(oldMatrix);
    end;

    if itemIndex < FItems.Size-1 then inc(itemIndex)
      else begin
        itemIndex := 0;
        //offsetCoor.x := offsetCoor.x + viewRect.Width;
        //offsetCoor.x := offsetCoor.x + FWorldArea.Width;
        offsetCoor.x := offsetCoor.x + Min(viewRect.Width, FWorldArea.Width);

     //   offsetCoor.y := offsetCoor.y + //viewRect.Height; // FWorldArea.Height;
      end;
  end;



{  for i:=0 to FItems.Size-1 do begin
    with FItems.Mutable[i]^ do begin
       newComputedOpacity := FComputedOpacity * opacity;
      if newComputedOpacity = 0 then continue;

      {%H-}modelArea.SetAsRectangle(x, y, w, h);
      if not modelArea.isVisibleIn(viewRect) then continue;

      // save and prepare matrix
      oldMatrix.CopyFrom(FParentScene.ModelViewMatrix);
      FParentScene.ModelViewMatrix.AddTransform(x, y, 1.0, 1.0,
                           angle, w*pivotX, h*pivotY);
      // frame index
      fi := EnsureRange(frameindex, 0, High(texture^.FramesCoord));
      // render the quad
      FParentScene.NoFilterTexturedMVTriangleRenderer.Prepare(ptTriangleStrip, texture, 0, BlendMode);
      FParentScene.NoFilterTexturedMVTriangleRenderer.PushQuad2(modelArea, texture^.FramesCoord[fi], flipIndex,
                                                        NIL, newComputedOpacity, computedtint);
      // restore matrix
      FParentScene.ModelViewMatrix.CopyFrom(oldMatrix);
      while FOffsetValue.y >= FWorldArea.Bottom do
        FOffsetValue.y := FOffsetValue.y - FWorldArea.Bottom;
    end;
  end;     }
end;

constructor TOGLCDecorList.Create(aParentScene: TOGLCScene; aLayerIndex: integer; const aWorldArea: TRectF);
begin
  inherited Create;
  FParentScene := aParentScene;
  FLayerIndex := aLayerIndex;
  if aLayerIndex <> -1 then FParentScene.Add(Self, aLayerIndex);
  ChildsUseParentOpacity := True;

  FItems := TDecorItemList.Create;

  FWorldArea := aWorldArea;
end;

destructor TOGLCDecorList.Destroy;
begin
  FItems.Clear;
  FItems.Free;
  FItems := NIL;
  inherited Destroy;
end;

{ TOGLCDecorManager }

function TOGLCDecorManager.GetDecorWithLayerIndexOf(aLayerIndex: integer): TOGLCDecorList;
var i: SizeUInt;
  o: TOGLCDecorList;
begin
  // search if there is a TOGLCDecorList assigned to the layer
  if FDecors.Size > 0 then
    for i:=0 to FDecors.Size-1 do
      if FDecors.Mutable[i]^.LayerIndex = aLayerIndex then begin
        Result := FDecors.Mutable[i]^;
        exit;
      end;

  // not found -> create a new instance for the layer
  o := TOGLCDecorList.Create(FParentScene, aLayerIndex, WorldArea);
  FDecors.PushBack(o);
  Result := FDecors.Mutable[FDecors.Size-1]^;
end;

class procedure TOGLCDecorManager.LoadTexture(aAtlas: TAtlas);
begin
  FAtlas := aAtlas;
end;

constructor TOGLCDecorManager.Create(aParentScene: TOGLCScene);
begin
  FParentScene := aParentScene;
  FDecors := TDecors.Create;
end;

destructor TOGLCDecorManager.Destroy;
begin
  //Clear;
  FDecors.Free;
  FDecors := NIL;
  inherited Destroy;
end;

procedure TOGLCDecorManager.Clear;
var i: SizeUInt;
begin
  if FDecors.Size = 0 then exit;

  for i:=0 to FDecors.Size-1 do
    FDecors.Mutable[i]^.Kill;
  FDecors.Clear;
end;

procedure TOGLCDecorManager.DoBuildLevel(const s: string);
var prop: TProperties;
  v: single;
  vb: boolean;
  skyx, skyy: single;
  skywidth, skyheight, skylayer: integer;
  skygradientdata: string;
  c, i, layerIndex: integer;
  data: string;
  o: TOGLCDecorItem;
begin
  Clear;
  prop.Split(s, '|');
  v := 0;
  vb := False;
  data := '';

  prop.SingleValueOf('WorldX', v, 0);
  FWorldArea.Left := ScaleWF(v);
  prop.SingleValueOf('WorldY', v, 0);
  FWorldArea.Top := ScaleHF(v);
  prop.SingleValueOf('WorldWidth', v, 0);
  FWorldArea.Width := ScaleWF(v);
  prop.SingleValueOf('WorldHeight', v, 0);
  FWorldArea.Height := ScaleHF(v);
  prop.BooleanValueOf('UseSkyGradient', vb, False);
  if vb then begin
    // use sky gradient
    skyx := 0;
    skyy := 0;
    skywidth := 0;
    skyheight := 0;
    skylayer := 0;
    skygradientdata := '';
    if prop.SingleValueOf('SkyX', skyx, 0.0) and
       prop.SingleValueOf('SkyY', skyy, 0.0) and
       prop.IntegerValueOf('SkyLayer', skylayer, FParentScene.LayerCount-1) and
       prop.IntegerValueOf('SkyWidth', skywidth, 0) and
       prop.IntegerValueOf('SkyHeight', skyheight, 0) and
       prop.StringValueOf('SkyGradientData', skygradientdata, '') then begin
      FSkyGradient := TGradientRectangle.Create(FParentScene);
      FSkyGradient.MoveToLayer(skylayer);
      FSkyGradient.SetCoordinate(skyx, skyy);
      FSkyGradient.Gradient.LoadGradientDataFromString(skygradientdata);
      FSkyGradient.SetSize(skywidth, skyheight);
    end else FSkyGradient := NIL;
  end;


  c := 0;
  prop.IntegerValueOf('Count', c, 0);
  if c = 0 then exit;

  for i:=0 to c-1 do begin
    if prop.StringValueOf('S'+i.ToString, data, '') then begin
      o := Default(TOGLCDecorItem);
      layerIndex := o.LoadFromString(FAtlas, data);
      // scale
      o.x := ScaleWF(o.x);
      o.y := ScaleHF(o.y);
      o.w := ScaleWF(o.w);
      o.h := ScaleHF(o.h);
      GetDecorWithLayerIndexOf(layerIndex).Add(o);
    end;
  end;
end;

{$endif oglcIMPLEMENTATION}

