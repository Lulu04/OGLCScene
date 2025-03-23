{$ifdef oglcINTERFACE}
type

TGlowVertexAttributes = packed record
  x, y: single;        // vertex coordinates
  u, v: single;        // uv coordinates
  attenuation, opacity, mode: single;
  color,               // color of the vertex
  tint: TColorF;       // the computed tint of the surface
end;
PGlowVertexAttributes = ^TGlowVertexAttributes;

const _GLOW_MODE = 0.0;
      _SPOTLIGHT_MODE = 1.0;
type
{ TOGLCGlowRenderer }

TOGLCGlowRenderer = class(specialize TOGLCGenericPrimitiveRenderer<TGlowVertexAttributes>)
private const
GLOW_VERTEX_SHADER_CORE =
  '#version 330 core'#10+
  '  layout(location = 0) in vec4 aVertexAndUVCoor;'#10+
  '  layout(location = 1) in vec3 aAttenuationOpacityMode;'#10+
  '  layout(location = 2) in vec4 aVertexColor;'#10+
  '  layout(location = 3) in vec4 aTint;'#10+
  '  uniform mat4 uMVP;'#10+
  '  out vec4 VertexColor;'#10+
  '  out vec2 UVCoords;'#10+
  '  flat out float Attenuation;'#10+
  '  flat out float Opacity;'#10+
  '  flat out float Mode;'#10+
  '  flat out vec4 Tint;'#10+
  'void main() {'#10+
  '  gl_Position = uMVP*vec4(aVertexAndUVCoor.xy, 0.0, 1.0);'#10+
  '  UVCoords = aVertexAndUVCoor.zw;'#10+
  '  VertexColor = aVertexColor;'#10+
  '  Attenuation = aAttenuationOpacityMode.x;'#10+
  '  Opacity = aAttenuationOpacityMode.y;'#10+
  '  Mode = aAttenuationOpacityMode.z;'#10+
  '  Tint = aTint;'#10+
  '}';
GLOW_FRAGMENT_SHADER_CORE =
 '#version 330 core'#10+
 '#define _GLOW_MODE 0.0'#10+
 '  out vec4 FragColor;'#10+
 '  in vec4 VertexColor;'#10+
 '  in vec2 UVCoords;'#10+
 '  flat in float Attenuation;'#10+
 '  flat in float Opacity;'#10+
 '  flat in float Mode;'#10+      // 0.5=Glow   0.0=SpotLight
 '  flat in vec4 Tint;'#10+
 'void main() {'#10+
{ '  float dist = 1.0-clamp(distance(UVCoords, vec2(Mode, 0.5))*2.0, 0.0, 1.0);'#10+
 //'  float dist = 1.0-clamp(distance(UVCoords, vec2(0.5,0.5))*2.0, 0.0, 1.0);'#10+
 '  dist = sqrt(pow(dist, Attenuation))*Opacity;'#10+ //modified easeInCirc  https://easings.net/
 '  if (dist == 0.0) discard;'#10+
 '  vec4 col = vec4( VertexColor.rgb, dist);'#10+    }

 ' float dist;'#10+
 ' vec2 uv;'#10+
 ' if (Mode == _GLOW_MODE)'#10+    // glow
 '  {'#10+
 //'    dist = 1.0-clamp(distance(UVCoords, vec2(0.5,0.5))*2.0, 0.0, 1.0);'#10+
 '    uv = UVCoords + vec2(-0.5, -0.5);'#10+
 '    dist = 1.0 - length(uv);'#10+
 '    dist = (pow(dist, Attenuation))*Opacity;'#10+ //modified easeInCirc  https://easings.net/
 '  } else {'#10+
 '    uv = UVCoords + vec2(0.0, -0.5);'#10+
 '    dist = 1.0 - length(uv);'#10+
 '    dist = (pow(dist, Attenuation))*Opacity;'#10+ //modified easeInCirc  https://easings.net/
 '    dist -= step(0.6, UVCoords.y) *  smoothstep(0.6, 1.0, UVCoords.y);'#10+
 '    dist -= step(0.6, 1.0-UVCoords.y) * smoothstep(0.6, 1.0, 1.0-UVCoords.y);'#10+
 '  }'#10+
'  vec4 col = vec4( VertexColor.rgb, sqrt(pow(dist, Attenuation))*Opacity);'#10+

 // Tint and opacity
  '  float tintAlphaX2 = Tint.a*2;'#10+
 '  if (Tint.a >= 0.5)'#10+
 '   {'#10+
 '     tintAlphaX2 = tintAlphaX2-1;'#10+
 '     col = mix(col, Tint, tintAlphaX2);'#10+ // replace mode
 '   }'#10+
 '  else if (Tint.a > 0)'#10+
 '     col = col + Tint*tintAlphaX2;'#10+     // mix mode
// '  col.a = dist;'#10+

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
  procedure PushQuad(aWidth, aHeight, aFlipIndex: integer; aAttenuation,
    aOpacity, aMode: single; const aComputedTint: TColorF; const aTopLeftColor,
  aTopRightColor, aBottomRightColor, aBottomLeftColor: TBGRAPixel);
end;


type

{ TGlowBase }

TGlowBase = class(TSimpleSurfaceWithEffect)
private const
  MIN_ATTENUATION_VALUE = 0.1;
  MAX_ATTENUATION_VALUE = 100.0;
  GLOW_DEFAULT_ATTENUATION_VALUE = 0.945;
  SPOTLIGHT_DEFAULT_ATTENUATION_VALUE = 0.97;
private
  FWidth, FHeight: integer;
  function GetAttenuationValue: single;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Update(const aElapsedTime: single); override;
public
  // range [0..1]
  Power: TBoundedFParam;
  constructor Create;
  destructor Destroy; override;

  // to sets the rectangular dimension that contain the light effect
  procedure SetSize(aWidth, aHeight: integer);
  // to sets the surface dimension with the glow radius
  procedure SetRadius(aRadiusX, aRadiusY: single);
end;

{ TOGLCGlow }
// this surface display a colored glow that can be used to add light effects on your scene.
TOGLCGlow = class(TGlowBase)
public
  procedure DoDraw; override;
public
  TopLeftColor,
  TopRightColor,
  BottomRightColor,
  BottomLeftColor: TBGRAPixel;
  constructor Create(aParentScene: TOGLCScene; aRadiusX, aRadiusY: single; aColor: TBGRAPixel; aBlendMode: byte=FX_BLEND_ADD);
  constructor Create(aParentScene: TOGLCScene; aBlendMode: byte=FX_BLEND_ADD);

  procedure SetAllColorsTo(AColor: TBGRAPixel);
  procedure SetLeftColors(AColor: TBGRAPixel);
  procedure SetRightColors(AColor: TBGRAPixel);
  procedure SetTopColors(AColor: TBGRAPixel);
  procedure SetBottomColors(AColor: TBGRAPixel);
end;


TOGLCSpotLight = class(TGlowBase)
public
  procedure DoDraw; override;
public
  SourceColor, EndColor: TBGRAPixel;
  constructor Create(aParentScene: TOGLCScene; aBlendMode: byte=FX_BLEND_ADD);

  procedure SetAllColorsTo(AColor: TBGRAPixel);
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCGlowRenderer }

procedure TOGLCGlowRenderer.InitShaderCodeAndCallBack;
begin
  FShaderName := 'TOGLCGlowRenderer';
  FVertexShaderCode := PChar(GLOW_VERTEX_SHADER_CORE);
  FFragmentShaderCode := PChar(GLOW_FRAGMENT_SHADER_CORE);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCGlowRenderer.DefineVertexAttribPointer;
begin
  // xy uv = 4 float
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(TGlowVertexAttributes), NIL);
  glEnableVertexAttribArray(0);
  // attenuation opacity Mode = 3 float
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, sizeof(TGlowVertexAttributes), PChar(sizeof(single)*4));
  glEnableVertexAttribArray(1);
  // color = 4 float
  glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, sizeof(TGlowVertexAttributes), PChar(sizeof(single)*7));
  glEnableVertexAttribArray(2);
  // tint = 4 float
  glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, sizeof(TGlowVertexAttributes), PChar(sizeof(single)*11));
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
  aAttenuation, aOpacity, aMode: single; const aComputedTint: TColorF; const aTopLeftColor, aTopRightColor,
  aBottomRightColor, aBottomLeftColor: TBGRAPixel);
var tci: PQuadCornerIndexes;
    p: PGlowVertexAttributes;
    pIndex: PVertexIndex;
    currentIndex: TVertexIndex;
begin
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
    x := 0; //.x;
    y := aHeight; //area[cBL].y;
    u := NoFlipQuadUV[ tci^[0] ].x;
    v := NoFlipQuadUV[ tci^[0] ].y;
    attenuation := aAttenuation;
    opacity := aOpacity;
    mode := aMode;
    color.InitFromBGRA(aBottomLeftColor);
    tint.CopyFrom(aComputedTint);
  end;
  with p[1] do begin
    x := 0;
    y := 0;
    u := NoFlipQuadUV[ tci^[1] ].x;
    v := NoFlipQuadUV[ tci^[1] ].y;
    attenuation := aAttenuation;
    opacity := aOpacity;
    mode := aMode;
    color.InitFromBGRA(aTopLeftColor);
    tint.CopyFrom(aComputedTint);
  end;
  with p[2] do begin
    x := aWidth;
    y := aHeight;
    u := NoFlipQuadUV[ tci^[2] ].x;
    v := NoFlipQuadUV[ tci^[2] ].y;
    attenuation := aAttenuation;
    opacity := aOpacity;
    mode := aMode;
    color.InitFromBGRA(aBottomRightColor);
    tint.CopyFrom(aComputedTint);
  end;
  with p[3] do begin
    x := aWidth;
    y := 0;
    u := NoFlipQuadUV[ tci^[3] ].x;
    v := NoFlipQuadUV[ tci^[3] ].y;
    attenuation := aAttenuation;
    opacity := aOpacity;
    mode := aMode;
    color.InitFromBGRA(aTopRightColor);
    tint.CopyFrom(aComputedTint);
  end;
end;

{ TGlowBase }

function TGlowBase.GetAttenuationValue: single;
begin
  Result := (1-sqrt(Power.Value))*(MAX_ATTENUATION_VALUE-MIN_ATTENUATION_VALUE)+MIN_ATTENUATION_VALUE;
end;

function TGlowBase.GetWidth: integer;
begin
  Result := FWidth;
end;

function TGlowBase.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TGlowBase.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if Freeze then exit;
  Power.OnElapse(aElapsedTime);
end;

constructor TGlowBase.Create;
begin
  inherited Create;
  Power := CreateBoundedFParam(0.0, 1.0);
  Power.Value := GLOW_DEFAULT_ATTENUATION_VALUE;
end;

destructor TGlowBase.Destroy;
begin
  FreeAndNil(Power);
  inherited Destroy;
end;

procedure TGlowBase.SetSize(aWidth, aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
end;

procedure TGlowBase.SetRadius(aRadiusX, aRadiusY: single);
begin
  FWidth := Round(aRadiusX * 2.0);
  FHeight := Round(aRadiusY * 2.0);
end;

{ TOGLCGlow }

procedure TOGLCGlow.DoDraw;
begin
  FParentScene.GlowRenderer.Prepare(ptTriangleStrip, FParentScene.MVPMatrix, FBlendMode);
  FParentScene.GlowRenderer.PushQuad(FWidth, FHeight, FlipToIndex, GetAttenuationValue,
                                     FComputedOpacity, _GLOW_MODE,
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

  FBlendMode := aBlendMode;
end;

constructor TOGLCGlow.Create(aParentScene: TOGLCScene; aBlendMode: byte);
begin
  inherited Create;
  FParentScene := aParentScene;
  FBlendMode := aBlendMode;
  SetAllColorsTo(BGRA(255,255,255));
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

{ TOGLCSpotLight }

procedure TOGLCSpotLight.DoDraw;
begin
  FParentScene.GlowRenderer.Prepare(ptTriangleStrip, FParentScene.MVPMatrix, FBlendMode);
  FParentScene.GlowRenderer.PushQuad(FWidth, FHeight, FlipToIndex, GetAttenuationValue,
                                     FComputedOpacity, _SPOTLIGHT_MODE,
                                     FComputedTint,
                                     SourceColor, EndColor, EndColor, SourceColor);
end;

constructor TOGLCSpotLight.Create(aParentScene: TOGLCScene; aBlendMode: byte);
begin
  inherited Create;
  FParentScene := aParentScene;
  FBlendMode := aBlendMode;
  Power.Value := SPOTLIGHT_DEFAULT_ATTENUATION_VALUE;

  SetAllColorsTo(BGRAWhite);
end;

procedure TOGLCSpotLight.SetAllColorsTo(AColor: TBGRAPixel);
begin
  SourceColor := AColor;
  EndColor := AColor;
end;

{$endif oglcIMPLEMENTATION}

