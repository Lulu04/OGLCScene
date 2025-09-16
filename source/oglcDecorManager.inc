{$ifdef oglcINTERFACE}
type

{ TOGLCDecorItem }

TOGLCDecorItem = record
  texture: PTexture;
  x, y, w, h, pivotX, pivotY, angle, opacity: single;
  flipIndex: integer;
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
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Draw(const aLayerPercentOpacity: single);  override;
  procedure DoDraw; override;
public
  constructor Create(aParentScene: TOGLCScene; aLayerIndex: integer);
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
  function GetDecorWithLayerIndexOf(aLayerIndex: integer): TOGLCDecorList;
  procedure Clear;
protected
  function ScaleWF(AValue: single): single; virtual; abstract;
  function ScaleHF(AValue: single): single; virtual; abstract;
public
  class procedure LoadTexture(aAtlas: TAtlas); virtual;
  constructor Create(aParentScene: TOGLCScene);
  destructor Destroy; override;

  procedure DoLoadLevel(const s: string);


  property WorldArea: TRectF read FWorldArea;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCDecorItem }

function TOGLCDecorItem.LoadFromString(aAtlas: TAtlas; const s: string): integer;
var prop: TProperties;
  texName: string;
  c: TBGRAPixel;
  tm: integer;
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

  prop.BGRAPixelValueOf('T', c, BGRA(0, 0, 0, 0));
  prop.IntegerValueOf('M', tm, Ord(tmReplaceColor));
  computedtint.InitFromTint(TTintMode(tm), c);
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

procedure TOGLCDecorList.Draw(const aLayerPercentOpacity: single);
var i: SizeUInt;
  modelArea: TQuadCoor;
begin
  if FItems.Size = 0 then exit;

  for i:=0 to FItems.Size-1 do begin
    with FItems.Mutable[i]^ do begin
      FComputedOpacity := aLayerPercentOpacity*opacity*Self.Opacity.Value*OneDiv255;
      if FComputedOpacity = 0 then continue;
      {%H-}modelArea.SetAsRectangle(0, 0, w, h);

      // draw child with negative Zorder
      if FChildsUseParentOpacity then DrawChildSurfacesWithNegativeZOrder(FComputedOpacity)
        else DrawChildSurfacesWithNegativeZOrder(aLayerPercentOpacity);
      // save and prepare matrix
      FPreviousModelViewMatrix.CopyFrom(FParentScene.ModelViewMatrix);
      FParentScene.ModelViewMatrix.AddTransform(x, y, 1.0, 1.0,
                           angle, w*pivotX, h*pivotY);
      // render the quad
      FParentScene.NoFilterTexturedMVTriangleRenderer.Prepare(ptTriangleStrip, texture, 0, BlendMode);
      FParentScene.NoFilterTexturedMVTriangleRenderer.PushQuad2(modelArea, texture^.FramesCoord[0], flipIndex,
                                                        NIL, FComputedOpacity, computedtint);

      // restore matrix
      FParentScene.ModelViewMatrix.CopyFrom(FPreviousModelViewMatrix);
      // draw child with positive Zorder
      if FChildsUseParentOpacity then DrawChildSurfacesWithPositiveZOrder(FComputedOpacity)
        else DrawChildSurfacesWithPositiveZOrder(aLayerPercentOpacity);
    end;
  end;
end;

procedure TOGLCDecorList.DoDraw;
begin
  // nothing here
end;

constructor TOGLCDecorList.Create(aParentScene: TOGLCScene; aLayerIndex: integer);
begin
  inherited Create;
  FParentScene := aParentScene;
  FLayerIndex := aLayerIndex;
  if aLayerIndex <> -1 then FParentScene.Add(Self, aLayerIndex);
  ChildsUseParentOpacity := True;

  FItems := TDecorItemList.Create;
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
begin
  // search if there is a TOGLCDecorList assigned to the layer
  if FDecors.Size > 0 then
    for i:=0 to FDecors.Size-1 do
      if FDecors.Mutable[i]^.LayerIndex = aLayerIndex then begin
        Result := FDecors.Mutable[i]^;
        exit;
      end;

  // not found -> create a new instance for the layer
  FDecors.PushBack(TOGLCDecorList.Create(FParentScene, aLayerIndex));
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

procedure TOGLCDecorManager.DoLoadLevel(const s: string);
var prop: TProperties;
  v: single;
  c, i, layerIndex: integer;
  data: string;
  o: TOGLCDecorItem;
begin
  Clear;
  prop.Split(s, '|');
  v := 0;
  data := '';

  prop.SingleValueOf('WorldX', v, 0);
  FWorldArea.Left := ScaleWF(v);
  prop.SingleValueOf('WorldY', v, 0);
  FWorldArea.Top := ScaleHF(v);
  prop.SingleValueOf('WorldWidth', v, 0);
  FWorldArea.Width := ScaleWF(v);
  prop.SingleValueOf('WorldHeight', v, 0);
  FWorldArea.Height := ScaleHF(v);

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

