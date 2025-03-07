{$ifdef oglcINTERFACE}
type

{ TOGLCGlowRenderer }

TOGLCGlowRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyuvattenuationopacitycolorx2>)
private const
GLOW_VERTEX_SHADER_CORE =
  '#version 330 core'#10+
  '  layout(location = 0) in vec4 aVertexAndUVCoor;'#10+
  '  layout(location = 1) in vec2 aAttenuationAndOpacity;'#10+
  '  layout(location = 2) in vec4 aVertexColor;'#10+
  '  layout(location = 3) in vec4 aTint;'#10+
  '  uniform mat4 uMVP;'#10+
  '  out vec4 VertexColor;'#10+
  '  out vec2 UVCoords;'#10+
  '  flat out float Attenuation;'#10+
  '  flat out float Opacity;'#10+
  '  flat out vec4 Tint;'#10+
  'void main() {'#10+
  '  gl_Position = uMVP*vec4(aVertexAndUVCoor.xy, 0.0, 1.0);'#10+
  '  UVCoords = aVertexAndUVCoor.zw;'#10+
  '  VertexColor = aVertexColor;'#10+
  '  Attenuation = aAttenuationAndOpacity.x;'#10+
  '  Opacity = aAttenuationAndOpacity.y;'#10+
  '  Tint = aTint;'#10+
  '}';
GLOW_FRAGMENT_SHADER_CORE =
 '#version 330 core'#10+
 '  out vec4 FragColor;'#10+
 '  in vec4 VertexColor;'#10+
 '  in vec2 UVCoords;'#10+
 '  flat in float Attenuation;'#10+
 '  flat in float Opacity;'#10+
 '  flat in vec4 Tint;'#10+
 'void main() {'#10+
 '  float dist = 1.0-clamp(distance(UVCoords, vec2(0.5,0.5))*2.0, 0.0, 1.0);'#10+
 '  dist = sqrt(pow(dist, Attenuation))*Opacity;'#10+ //modified easeInCirc  https://easings.net/
 '  if (dist == 0.0) discard;'#10+
 '  vec4 col = vec4( VertexColor.rgb, dist);'#10+

 // Tint and opacity
  '  float tintAlphaX2 = Tint.a*2;'#10+
 '  if (Tint.a >= 0.5)'#10+
 '   {'#10+
 '     tintAlphaX2 = tintAlphaX2-1;'#10+
 '     col = mix(col, Tint, tintAlphaX2);'#10+ // replace mode
 '   }'#10+
 '  else if (Tint.a > 0)'#10+
 '     col = col + Tint*tintAlphaX2;'#10+     // mix mode
 '  col.a = dist;'#10+

 '  FragColor = col;'#10+
 '}';
private
  FLocMVP: GLint;
  FMVP: TOGLCMatrix;
protected
  procedure InitShaderCodeAndCallBack; override;
private
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix; aBlendMode: byte);
  procedure PushQuad(aWidth, aHeight, aFlipIndex: integer; aAttenuation, aOpacity: single;
                     const aComputedTint: TColorF;
                     const aTopLeftColor, aTopRightColor, aBottomRightColor, aBottomLeftColor: TBGRAPixel);
end;


type
{ TOGLCGlow }
// this surface display a colored glow that can be used to add light effects on your scene.
TOGLCGlow = class(TSimpleSurfaceWithEffect)
private const
  MIN_ATTENUATION_VALUE = 0.1;
  MAX_ATTENUATION_VALUE = 100.0;
  DEFAULT_ATTENUATION_VALUE = 0.895;
private
  FWidth, FHeight: integer;
  function GetAttenuationValue: single;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
public
  TopLeftColor,
  TopRightColor,
  BottomRightColor,
  BottomLeftColor: TBGRAPixel;
  // range [0..1]
  Power: TBoundedFParam;
  // use Tint to change the glow color at run time (TintMode is set to tmMixColor, don't change it)
  Constructor Create(aParentScene: TOGLCScene; aRadiusX, aRadiusY: single; aColor: TBGRAPixel; aBlendMode: byte=FX_BLEND_ADD);
  Destructor Destroy; override;
  // to sets the rectangular dimension that contain the light effect
  procedure SetSize(aWidth, aHeight: integer);
  // to sets the surface dimension with the glow radius
  procedure SetRadius(aRadiusX, aRadiusY: single);

  procedure SetAllColorsTo(AColor: TBGRAPixel);
  procedure SetLeftColors(AColor: TBGRAPixel);
  procedure SetRightColors(AColor: TBGRAPixel);
  procedure SetTopColors(AColor: TBGRAPixel);
  procedure SetBottomColors(AColor: TBGRAPixel);
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCGlowRenderer }

procedure TOGLCGlowRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(GLOW_VERTEX_SHADER_CORE);
  FFragmentShaderCode := PChar(GLOW_FRAGMENT_SHADER_CORE);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCGlowRenderer.DefineVertexAttribPointer;
begin
  // xy uv = 4 float
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvattenuationopacitycolorx2), NIL);
  glEnableVertexAttribArray(0);
  // attenuation opacity = 2 float
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(Txyuvattenuationopacitycolorx2), PChar(sizeof(single)*4));
  glEnableVertexAttribArray(1);
  // color = 4 float
  glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvattenuationopacitycolorx2), PChar(sizeof(single)*6));
  glEnableVertexAttribArray(2);
  // tint = 4 float
  glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvattenuationopacitycolorx2), PChar(sizeof(single)*10));
  glEnableVertexAttribArray(3);
end;

procedure TOGLCGlowRenderer.GetUniformLocation;
begin
  with FShader do
    FLocMVP := GetUniform('uMVP');
end;

procedure TOGLCGlowRenderer.SetUniformValuesAndTexture;
begin
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
  FParentScene.TexMan.UnbindTexture;
end;

procedure TOGLCGlowRenderer.Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix; aBlendMode: byte);
var forceFlush: Boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP);
  Batch_CheckIfNeedFlush(Self, aTriangleType, NIL, 0, aBlendMode, forceFlush);
  FMVP.CopyFrom(aMVP);
end;

procedure TOGLCGlowRenderer.PushQuad(aWidth, aHeight, aFlipIndex: integer;
  aAttenuation, aOpacity: single; const aComputedTint: TColorF; const aTopLeftColor, aTopRightColor,
  aBottomRightColor, aBottomLeftColor: TBGRAPixel);
var area, texCoords: TQuadCoor;
  tci: PQuadCornerIndexes;
    p: Pxyuvattenuationopacitycolorx2;
    pIndex: PVertexIndex;
    currentIndex: TVertexIndex;
begin
  area[cBL].x := 0;
  area[cBL].y := aHeight;
  area[cTL].x := 0;
  area[cTL].y := 0;
  area[cBR].x := aWidth;
  area[cBR].y := aHeight;
  area[cTR].x := aWidth;
  area[cTR].y := 0;

  texCoords[cBL].x := 0;
  texCoords[cBL].y := 0;
  texCoords[cTL].x := 0;
  texCoords[cTL].y := 1;
  texCoords[cBR].x := 1;
  texCoords[cBR].y := 0;
  texCoords[cTR].x := 1;
  texCoords[cTR].y := 1;

  tci := @FLIP_INDEXES[aFlipIndex];
  currentIndex := FIndexInAttribsArray;

  case Batch^.CurrentPrimitiveType of
    ptTriangleStrip: begin           // 24
      AddPrimitiveRestartIfNeeded;   // 13
      pIndex := QueryIndex(4);
      pIndex[0] := currentIndex;
      pIndex[1] := currentIndex+1;
      pIndex[2] := currentIndex+2;
      pIndex[3] := currentIndex+3;
    end;
    ptTriangles: begin              // B   BD
      pIndex := QueryIndex(6);      // AC  C
      pIndex[0] := currentIndex;
      pIndex[1] := currentIndex+1;
      pIndex[2] := currentIndex+2;
      pIndex[3] := currentIndex+2;
      pIndex[4] := currentIndex+1;
      pIndex[5] := currentIndex+3;
    end;
  end;

  // push the 4 vertex        // vertex coord      24
  p := QueryVertex(4);        //                   13
  with p[0] do begin
    x := area[cBL].x;
    y := area[cBL].y;
    u := texCoords[ tci^[0] ].x;
    v := texCoords[ tci^[0] ].y;
    attenuation := aAttenuation;
    opacity := aOpacity;
    color.InitFromBGRA(aBottomLeftColor);
    tint.CopyFrom(aComputedTint);
  end;
  with p[1] do begin
    x := area[cTL].x;
    y := area[cTL].y;
    u := texCoords[ tci^[1] ].x;
    v := texCoords[ tci^[1] ].y;
    attenuation := aAttenuation;
    opacity := aOpacity;
    color.InitFromBGRA(aTopLeftColor);
    tint.CopyFrom(aComputedTint);
  end;
  with p[2] do begin
    x := area[cBR].x;
    y := area[cBR].y;
    u := texCoords[ tci^[2] ].x;
    v := texCoords[ tci^[2] ].y;
    attenuation := aAttenuation;
    opacity := aOpacity;
    color.InitFromBGRA(aBottomRightColor);
    tint.CopyFrom(aComputedTint);
  end;
  with p[3] do begin
    x := area[cTR].x;
    y := area[cTR].y;
    u := texCoords[ tci^[3] ].x;
    v := texCoords[ tci^[3] ].y;
    attenuation := aAttenuation;
    opacity := aOpacity;
    color.InitFromBGRA(aTopRightColor);
    tint.CopyFrom(aComputedTint);
  end;
end;

{ TOGLCGlow }

function TOGLCGlow.GetAttenuationValue: single;
begin
  //Result := (1-Power.Value)*(MAX_ATTENUATION_VALUE-MIN_ATTENUATION_VALUE)+MIN_ATTENUATION_VALUE;
  Result := (1-sqrt(Power.Value))*(MAX_ATTENUATION_VALUE-MIN_ATTENUATION_VALUE)+MIN_ATTENUATION_VALUE;
end;

function TOGLCGlow.GetWidth: integer;
begin
  Result := FWidth;
end;

function TOGLCGlow.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TOGLCGlow.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if Freeze then exit;
  Power.OnElapse(aElapsedTime);
end;

procedure TOGLCGlow.DoDraw;
begin
  FParentScene.GlowRenderer.Prepare(ptTriangleStrip, FParentScene.MVPMatrix, FBlendMode);
  FParentScene.GlowRenderer.PushQuad(FWidth, FHeight, FlipToIndex, GetAttenuationValue,
                                     FComputedOpacity,
                                     FComputedTint,
                                     TopLeftColor, TopRightColor, BottomRightColor, BottomLeftColor);
end;

constructor TOGLCGlow.Create(aParentScene: TOGLCScene; aRadiusX,
  aRadiusY: single; aColor: TBGRAPixel; aBlendMode: byte);
begin
  inherited Create;
  FParentScene := aParentScene;
  SetRadius(aRadiusX, aRadiusY);
  SetAllColorsTo(aColor);

  Power := CreateBoundedFParam(0.0, 1.0);
  Power.Value := DEFAULT_ATTENUATION_VALUE;
  FBlendMode := aBlendMode;
end;

destructor TOGLCGlow.Destroy;
begin
  FreeAndNil(Power);
  inherited Destroy;
end;

procedure TOGLCGlow.SetSize(aWidth, aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
end;

procedure TOGLCGlow.SetRadius(aRadiusX, aRadiusY: single);
begin
  FWidth := Round(aRadiusX * 2.0);
  FHeight := Round(aRadiusY * 2.0);
end;

procedure TOGLCGlow.SetAllColorsTo(AColor: TBGRAPixel);
begin
  TopLeftColor := AColor;
  TopRightColor := AColor;
  BottomRightColor := AColor;
  BottomLeftColor := AColor;
end;

procedure TOGLCGlow.SetLeftColors(AColor: TBGRAPixel);
begin
  TopLeftColor := AColor;
  BottomLeftColor := AColor;
end;

procedure TOGLCGlow.SetRightColors(AColor: TBGRAPixel);
begin
  TopRightColor := AColor;
  BottomRightColor := AColor;
end;

procedure TOGLCGlow.SetTopColors(AColor: TBGRAPixel);
begin
  TopLeftColor := AColor;
  TopRightColor := AColor;
end;

procedure TOGLCGlow.SetBottomColors(AColor: TBGRAPixel);
begin
  BottomRightColor := AColor;
  BottomLeftColor := AColor;
end;

{$endif oglcIMPLEMENTATION}

