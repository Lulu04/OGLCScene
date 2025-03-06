unit u_ProceduralPlanet;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmapTypes,
  OGLCScene, glcorearb;

type

TOGLCSpritePlanet = class;

{ TPlanetParams }

TPlanetParams = packed record
  FColorGround1F, FColorGround2F,
  FColorOceanF,
  FColorClouds1F, FColorClouds2F: TColorF;
  FGroundColorDistribution, FGroundAmount: single;
  FCloudsColorDistribution, FCloudsOpacity, FCloudsFragmentation, FCloudsTransformation, FCloudsBlendMode: single;
  FColorHaloF: TColorF;
  FHaloThreshold: single;
  FShadowAmount: single;
  FRotationSpeed: single;
  procedure InitDefault;
  // Load the planet's parameters from a string constant
  procedure LoadParamsFromString(const s: string);
  // Pack the planet's parameters into a string
  function SaveParamsToString: string;
  function CheckString(const s: string): boolean;
end;


{ TOGLCPlanetRenderer }

TOGLCPlanetRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyuv>)
private const
  VERTEX_SHADER =
    '#version 330 core'#10+
    '  layout(location = 0) in vec4 aVertexAndTextureCoor;'#10+
    '  uniform mat4 uMVP;'#10+
    '  out vec2 TexCoords;'#10+
    'void main()'#10+
    '{'#10+
    '  gl_Position = uMVP*vec4(aVertexAndTextureCoor.xy, 0.0, 1.0);'#10+
    '  TexCoords = aVertexAndTextureCoor.zw;'#10+
    '}';

// this fragment shader is a modified version from
// https://docs.godotengine.org/en/stable/tutorials/shaders/using_viewport_as_texture.html
  FRAGMENT_SHADER =
    '#version 330 core'#10+
    '  layout(location = 0) out vec4 FragColor;'#10+
    '  in vec2 TexCoords;'#10+
    '  uniform float uTime;'#10+
    '  uniform float uRotationSpeed = 0.0035;'#10+
    '  uniform float uOpacity = 1.0;'#10+
    '  uniform vec3 uColorGround1 = vec3(0.9, 0.4, 0.4);'#10+
    '  uniform vec3 uColorGround2 = vec3(0.2, 0.6, 0.3);'#10+
    '  uniform float uGroundColorDistribution = 0.5;'#10+
    '  uniform float uGroundAmount = 0.5;'#10+ // ]0..1]
    '  uniform vec3 uColorOcean = vec3(0.05, 0.3, 0.7);'#10+
    '  uniform vec3 uColorClouds1 = vec3(1.0, 1.0, 1.0);'#10+
    '  uniform vec3 uColorClouds2 = vec3(0.5, 0.5, 0.5);'#10+
    '  uniform float uCloudsColorDistribution = 0.5;'#10+  // [0..1]
    '  uniform float uCloudsOpacity = 0.5;'#10+  // ]0..1]
    '  uniform float uCloudsFragmentation = 20.0;'#10+  //  [1 .. 20]
    '  uniform float uCloudsTransformation = 0.0;'#10+  // [0..1]
    '  uniform float uCloudsBlendMode = 1.0;'#10+  // 1.0= blend mode ADD    -1.0= blend mode SUBSTRACT
    '  uniform vec3 uColorHalo = vec3(0.0,0.6,1.0);'#10+
    '  uniform float uHaloThreshold = 0.49;'#10+ // must be [0.1  to 0.49]
    '  uniform float uShadowAmount = 0.0;'#10+  // [0..1]

    // noise function
    'vec3 hash(vec3 p) {'#10+
    '    p = vec3(dot(p, vec3(127.1, 311.7, 74.7)),'#10+
    '             dot(p, vec3(269.5, 183.3, 246.1)),'#10+
    '             dot(p, vec3(113.5, 271.9, 124.6)));'#10+

    '    return -1.0 + 2.0 * fract(sin(p) * 43758.5453123);'#10+
    '}'#10+

    'float noise(vec3 p) {'#10+
    '  vec3 i = floor(p);'#10+
    '  vec3 f = fract(p);'#10+
    '  vec3 u = f * f * (3.0 - 2.0 * f);'#10+
    '  return mix(mix(mix(dot(hash(i + vec3(0.0, 0.0, 0.0)), f - vec3(0.0, 0.0, 0.0)),'#10+
    '                     dot(hash(i + vec3(1.0, 0.0, 0.0)), f - vec3(1.0, 0.0, 0.0)), u.x),'#10+
    '                 mix(dot(hash(i + vec3(0.0, 1.0, 0.0)), f - vec3(0.0, 1.0, 0.0)),'#10+
    '                     dot(hash(i + vec3(1.0, 1.0, 0.0)), f - vec3(1.0, 1.0, 0.0)), u.x), u.y),'#10+
    '             mix(mix(dot(hash(i + vec3(0.0, 0.0, 1.0)), f - vec3(0.0, 0.0, 1.0)),'#10+
    '                     dot(hash(i + vec3(1.0, 0.0, 1.0)), f - vec3(1.0, 0.0, 1.0)), u.x),'#10+
    '                 mix(dot(hash(i + vec3(0.0, 1.0, 1.0)), f - vec3(0.0, 1.0, 1.0)),'#10+
    '                     dot(hash(i + vec3(1.0, 1.0, 1.0)), f - vec3(1.0, 1.0, 1.0)), u.x), u.y), u.z );'#10+
    '}'#10+

    // matrix scale
    'mat2 scale(vec2 _scale){'#10+
    '    return mat2(_scale.x, 0.0, 0.0, _scale.y);'#10+
    '}'#10+

    'void main()'#10+
    '{'#10+
    '  vec2 UV = TexCoords;'#10+
    '  vec4 col = vec4(0.0);'#10+
    '  float dist = distance(TexCoords, vec2(0.5,0.5));'#10+
    '  if (dist <= 0.5)'#10+
    '  {'#10+
    // ground and ocean
         // add planet rotation
    '    vec2 translate = vec2(uRotationSpeed*uTime,0.0);'#10+    // 0.0035*uTime
    '    UV += translate;'#10+
         // scale x 2
    '    UV -= vec2(0.5);'#10+
    '    UV = scale( vec2(0.5) ) * UV;'#10+      //0.5
    '    UV += vec2(0.5);'#10+
    '    float theta = UV.y * 3.14159;'#10+
    '    float phi = UV.x * 3.14159 * 2.0;'#10+
    '    vec3 unit;'#10+

    '    unit.x = sin(phi) * sin(theta);'#10+
    '    unit.y = cos(theta) * -1.0;'#10+
    '    unit.z = cos(phi) * sin(theta);'#10+
    '    unit = normalize(unit);'#10+
    '    float n = noise(unit * 5.0) * 0.5;'#10+
    '    n += noise(unit * 10.0) * 0.25;'#10+
    '    n += noise(unit * 20.0) * 0.125;'#10+
    '    n += noise(unit * 40.0) * 0.0625;'#10+
    '    vec3 tempColor = mix(uColorGround1, uColorGround2, smoothstep(-0.3, 0.75*uGroundColorDistribution, n));'#10+
    '    col.rgb = mix(tempColor, uColorOcean, smoothstep(-0.2+0.4*uGroundAmount, 0.3, n));'#10+

    // add clouds
    '    UV = TexCoords;'#10+
         // add a rotation to the clouds same as the planet
    '    UV += translate;'#10+

    '    theta = UV.y * 1,57079;'#10+
    '    phi = UV.x * 3.14159;'#10+
    '    unit.x = sin(phi) * sin(theta);'#10+
    '    unit.y = cos(theta) * -1.0;'#10+
    '    unit.z = cos(phi) * sin(theta);'#10+
    '    unit = normalize(unit);'#10+
    //'    n = sin(noise(unit * 2.0 + vec3(uTime*0.05)) * 0.5);'#10+
    '    n = sin(noise(unit * uCloudsFragmentation + uTime*uCloudsTransformation) * 0.5);'#10+
    '    n += noise(unit * uCloudsFragmentation*2.0) * 0.25;'#10+
    '    n += noise(unit * uCloudsFragmentation*4.0) * 0.125;'#10+
    '    n += noise(unit * uCloudsFragmentation*8.0) * 0.0625;'#10+

    '    float brightness = pow(smoothstep(-0.1, 0.0, n), 3.0);'#10+
    '    tempColor = mix(uColorClouds2, uColorClouds1, smoothstep(-0.1, 0.25*uCloudsColorDistribution, n));'#10+
    '    tempColor *=brightness*2.0;'#10+
    '    col.rgb += tempColor*uCloudsOpacity*uCloudsBlendMode;'#10+

    // add halo around the planet
    '    if (dist >= uHaloThreshold)'#10+     // here dist is <= 0.5
    '      {'#10+
    '        float a = pow(1.0 - smoothstep(uHaloThreshold, 0.5, dist), 3.0);'#10+
    '        col = vec4(uColorHalo, a*uOpacity);'#10+
    '      }'#10+
    '      else '#10+
    // antialiasing halo/edge of the planet
    '    if (dist > uHaloThreshold-0.02)'#10+
    '      {'#10+
    '        col.rgb = mix(col.rgb, uColorHalo, smoothstep(uHaloThreshold-0.02, uHaloThreshold, dist));'#10+
    '        col.a = uOpacity;'#10+
    '      }'#10+
    '      else col.a = uOpacity;'#10+

    // add darkness to the side of the planet
    '    if (uShadowAmount != 0.0)'#10+
    '    {'#10+
    '      brightness = smoothstep(uShadowAmount, 1.0, TexCoords.x);'#10+
    '      brightness = pow(brightness, 0.5);'#10+
    '      col.rgb *= brightness;'#10+
    '    }'#10+
    '  }'#10+
    '  FragColor = col;'#10+
    '}';
private
  FLocMVP,
  FLocOpacity,
  FLocTime,
  FLocColorGround1, FLocColorGround2, FLocGroundColorDistribution, FLocGroundAmount,
  FLocColorOcean,
  FLocColorClouds1, FLocColorClouds2, FLocCloudsColorDistribution, FLocCloudsOpacity,
  FLocCloudsFragmentation, FLocCloudsTransformation, FLocCloudsBlendMode,
  FLocColorHalo, FLocHaloThreshold,
  FLocShadowAmount,
  FLocRotationSpeed: glint;

  FMVP: TOGLCMatrix;
  FOpacity: single;
  FParams: TPlanetParams;
  FTimeAccu: single;
protected
  procedure InitShaderCodeAndCallBack; override;
private
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix;
                    const aOpacity: single; aBlendMode: byte;
                    const aParams: TPlanetParams; aTimeAccu: single);
  procedure PushQuad(const aX, aY: single; const aWidth, aHeight: integer; aFlipIndex: integer);
end;


{ TOGLCSpritePlanet }

TOGLCSpritePlanet = class(TSimpleSurfaceWithEffect)
private
  FWidth, FHeight: integer;
  FPlanetRenderer: TOGLCPlanetRenderer;
  FParams: TPlanetParams;
  FTimeAccu: single;
  function GetCloudsBlendMode: single;
  function GetCloudsColorDistribution: single;
  function GetCloudsFragmentation: single;
  function GetCloudsOpacity: byte;
  function GetCloudsTransformation: single;
  function GetColorClouds1: TBGRAPixel;
  function GetColorClouds2: TBGRAPixel;
  function GetColorGround1: TBGRAPixel;
  function GetColorGround2: TBGRAPixel;
  function GetColorHalo: TBGRAPixel;
  function GetColorOcean: TBGRAPixel;
  function GetGroundAmount: single;
  function GetGroundColorDistribution: single;
  function GetHaloSize: single;
  function GetRotationSpeed: single;
  function GetShadowAmount: single;
  procedure SetCloudsBlendMode(AValue: single);
  procedure SetCloudsColorDistribution(AValue: single);
  procedure SetCloudsFragmentation(AValue: single);
  procedure SetCloudsOpacity(AValue: byte);
  procedure SetCloudsTransformation(AValue: single);
  procedure SetColorClouds1(AValue: TBGRAPixel);
  procedure SetColorClouds2(AValue: TBGRAPixel);
  procedure SetColorGround1(AValue: TBGRAPixel);
  procedure SetColorGround2(AValue: TBGRAPixel);
  procedure SetColorHalo(AValue: TBGRAPixel);
  procedure SetColorOcean(AValue: TBGRAPixel);
  procedure SetGroundAmount(AValue: single);
  procedure SetGroundColorDistribution(AValue: single);
  procedure SetRotationSpeed(AValue: single);
  procedure SetShadowAmount(AValue: single);
  procedure SetHaloSize(AValue: single);
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
public
  constructor Create(aParentScene: TOGLCScene; aPlanetRenderer: TOGLCPlanetRenderer);

  procedure SetSize(aWidth, aHeight: integer);

  // Load the planet's parameters from a string constant
  procedure LoadParamsFromString(const s: string);
  // Pack the planet's parameters into a string
  function SaveParamsToString: string;

  // alpha is not used
  property ColorGround1: TBGRAPixel read GetColorGround1 write SetColorGround1;
  // alpha is not used
  property ColorGround2: TBGRAPixel read GetColorGround2 write SetColorGround2;
  // [0..1]
  property GroundColorDistribution: single read GetGroundColorDistribution write SetGroundColorDistribution;
  // [0..1]
  property GroundAmount: single read GetGroundAmount write SetGroundAmount;

  // alpha is not used
  property ColorOcean: TBGRAPixel read GetColorOcean write SetColorOcean;

  // alpha is not used
  property ColorClouds1: TBGRAPixel read GetColorClouds1 write SetColorClouds1;
  // alpha is not used
  property ColorClouds2: TBGRAPixel read GetColorClouds2 write SetColorClouds2;
  // [0..1]
  property CloudsColorDistribution: single read GetCloudsColorDistribution write SetCloudsColorDistribution;
  // [0..255]
  property CloudsOpacity: byte read GetCloudsOpacity write SetCloudsOpacity;
  // [1..20]
  property CloudsFragmentation: single read GetCloudsFragmentation write SetCloudsFragmentation;
  // [0..1]
  property CloudsTransformation: single read GetCloudsTransformation write SetCloudsTransformation;
  // [-1..1]
  property CloudsBlendMode: single read GetCloudsBlendMode write SetCloudsBlendMode;

  // alpha is not used
  property ColorHalo: TBGRAPixel read GetColorHalo write SetColorHalo;
  // range is 0.0 to 1.0
  property HaloSize: single read GetHaloSize write SetHaloSize;

  // [0..1]
  property ShadowAmount: single read GetShadowAmount write SetShadowAmount;

  // 0.0 planet don't rotate   increase value to increase rotation speed
  // default value is 0.0035 (very slow rotation)
  property RotationSpeed: single read GetRotationSpeed write SetRotationSpeed;
end;

implementation
uses Math;

{ TPlanetParams }

procedure TPlanetParams.InitDefault;
begin
  FColorGround1F.InitFromBGRA(BGRA(229,102,102));
  FColorGround2F.InitFromBGRA(BGRA(51,153,76));
  FGroundColorDistribution := 0.5;
  FGroundAmount := 0.5;

  FColorOceanF.InitFromBGRA(BGRA(13,76,178));

  FColorClouds1F.InitFromBGRA(BGRA(255,255,255));
  FColorClouds2F.InitFromBGRA(BGRA(79,79,79));
  FCloudsColorDistribution := 0.5;
  FCloudsOpacity := 0.9;
  FCloudsFragmentation := 5.0;
  FCloudsTransformation := 0.0;
  FCloudsBlendMode := 1.0;

  FColorHaloF.InitFromBGRA(BGRA(0,153,255));
  FHaloThreshold := 0.49;

  FShadowAmount := 0.0;

  FRotationSpeed := 0.0035;
end;

procedure TPlanetParams.LoadParamsFromString(const s: string);
var p: TProperties;
  pcol: TProperties;
  vs: string;
  vi: integer;
  colF: TColorF;
  procedure UnpackColorF(aCols: string; var colorF: TColorF);
  begin
    pcol.Split(aCols, ',');
    if pcol.IntegerValueOf('r', vi, 0) then colorF.RedByte := vi;
    if pcol.IntegerValueOf('g', vi, 0) then colorF.GreenByte := vi;
    if pcol.IntegerValueOf('b', vi, 0) then colorF.BlueByte := vi;
    if pcol.IntegerValueOf('a', vi, 0) then colorF.AlphaByte := vi;
  end;
begin
  InitDefault;

  p.Split(s, '|');
  vs := '';
  vi := 0;
  colF.SetAsTransparent;

  if p.StringValueOf('ColorGround1', vs, '') then
    UnpackColorF(vs, FColorGround1F);
  if p.StringValueOf('ColorGround2', vs, '') then
    UnpackColorF(vs, FColorGround2F);
  p.SingleValueOf('GroundColorDistribution', FGroundColorDistribution, FGroundColorDistribution);
  p.SingleValueOf('GroundAmount', FGroundAmount, FGroundAmount);

  if p.StringValueOf('ColorOcean', vs, '') then
    UnpackColorF(vs, FColorOceanF);

  if p.StringValueOf('ColorClouds1', vs, '') then
    UnpackColorF(vs, FColorClouds1F);
  if p.StringValueOf('ColorClouds2', vs, '') then
    UnpackColorF(vs, FColorClouds2F);
  p.SingleValueOf('CloudsColorDistribution', FCloudsColorDistribution, FCloudsColorDistribution);
  p.SingleValueOf('CloudsOpacity', FCloudsOpacity, FCloudsOpacity);
  p.SingleValueOf('CloudsFragmentation', FCloudsFragmentation, FCloudsFragmentation);
  p.SingleValueOf('CloudsTransformation', FCloudsTransformation, FCloudsTransformation);
  p.SingleValueOf('CloudsBlendMode', FCloudsBlendMode, FCloudsBlendMode);

  if p.StringValueOf('ColorHalo', vs, '') then
    UnpackColorF(vs, FColorHaloF);
  p.SingleValueOf('HaloThreshold', FHaloThreshold, FHaloThreshold);

  p.SingleValueOf('ShadowAmount', FShadowAmount, FShadowAmount);

  p.SingleValueOf('RotationSpeed', FRotationSpeed, FRotationSpeed);
end;

function TPlanetParams.SaveParamsToString: string;
var p: TProperties;
  pcol: TProperties;
  function PackColorF(const aColorF: TColorF): string;
  begin
    pcol.Init(',');
    pcol.Add('r', aColorF.RedByte);
    pcol.Add('g', aColorF.GreenByte);
    pcol.Add('b', aColorF.BlueByte);
    pcol.Add('a', aColorF.AlphaByte);
    Result := pcol.PackedProperty;
  end;
begin
  p.Init('|');

  p.Add('ColorGround1', PackColorF(FColorGround1F));
  p.Add('ColorGround2', PackColorF(FColorGround2F));
  p.Add('GroundColorDistribution', FGroundColorDistribution);
  p.Add('GroundAmount', FGroundAmount);

  p.Add('ColorOcean', PackColorF(FColorOceanF));

  p.Add('ColorClouds1', PackColorF(FColorClouds1F));
  p.Add('ColorClouds2', PackColorF(FColorClouds2F));
  p.Add('CloudsColorDistribution', FCloudsColorDistribution);
  p.Add('CloudsOpacity', FCloudsOpacity);
  p.Add('CloudsFragmentation', FCloudsFragmentation);
  p.Add('CloudsTransformation', FCloudsTransformation);
  p.Add('CloudsBlendMode', FCloudsBlendMode);

  p.Add('ColorHalo', PackColorF(FColorHaloF));
  p.Add('HaloThreshold', FHaloThreshold);

  p.Add('ShadowAmount', FShadowAmount);

  p.Add('RotationSpeed', FRotationSpeed);

  Result := p.PackedProperty;
end;

function TPlanetParams.CheckString(const s: string): boolean;
var p: TProperties;
begin
  p.Split(s, '|');
  Result := p.CheckProperties(['ColorGround1', 'ColorGround2', 'GroundColorDistribution', 'GroundAmount',
        'ColorOcean',
        'ColorClouds1', 'ColorClouds2', 'CloudsColorDistribution', 'CloudsOpacity',
        'CloudsFragmentation', 'CloudsTransformation', 'CloudsBlendMode',
        'ColorHalo', 'HaloThreshold',
        'ShadowAmount',
        'RotationSpeed']);
end;

{ TOGLCPlanetRenderer }

procedure TOGLCPlanetRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCPlanetRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(0);
end;

procedure TOGLCPlanetRenderer.GetUniformLocation;
begin
  with Shader do begin
    FLocMVP := GetUniform('uMVP');
    FLocOpacity := GetUniform('uOpacity');
    FLocTime := GetUniform('uTime');

    FLocColorGround1 := GetUniform('uColorGround1');
    FLocColorGround2 := GetUniform('uColorGround2');
    FLocGroundColorDistribution := GetUniform('uGroundColorDistribution');
    FLocGroundAmount := GetUniform('uGroundAmount');

    FLocColorOcean := GetUniform('uColorOcean');

    FLocColorClouds1 := GetUniform('uColorClouds1');
    FLocColorClouds2 := GetUniform('uColorClouds2');
    FLocCloudsOpacity := GetUniform('uCloudsOpacity');
    FLocCloudsColorDistribution := GetUniform('uCloudsColorDistribution');
    FLocCloudsFragmentation := GetUniform('uCloudsFragmentation');
    FLocCloudsTransformation := GetUniform('uCloudsTransformation');
    FLocCloudsBlendMode := GetUniform('uCloudsBlendMode');

    FLocColorHalo := GetUniform('uColorHalo');
    FLocHaloThreshold := GetUniform('uHaloThreshold');

    FLocShadowAmount := GetUniform('uShadowAmount');

    FLocRotationSpeed := GetUniform('uRotationSpeed');
  end;
end;

procedure TOGLCPlanetRenderer.SetUniformValuesAndTexture;
begin
  with FParams do begin
    glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
    glUniform1f(FLocOpacity, FOpacity);
    glUniform1f(FLocTime, FTimeAccu);

    glUniform3fv(FLocColorGround1, 1, @FColorGround1F);
    glUniform3fv(FLocColorGround2, 1, @FColorGround2F);
    gluniform1f(FLocGroundColorDistribution, FGroundColorDistribution);
    glUniform1f(FLocGroundAmount, FGroundAmount);

    glUniform3fv(FLocColorOcean, 1, @FColorOceanF);

    glUniform3fv(FLocColorClouds1, 1, @FColorClouds1F);
    glUniform3fv(FLocColorClouds2, 1, @FColorClouds2F);
    glUniform1f(FLocCloudsColorDistribution, FCloudsColorDistribution);
    glUniform1f(FLocCloudsOpacity, FCloudsOpacity);
    glUniform1f(FLocCloudsFragmentation, FCloudsFragmentation);
    glUniform1f(FLocCloudsTransformation, FCloudsTransformation);
    glUniform1f(FLocCloudsBlendMode, FCloudsBlendMode);

    glUniform3fv(FLocColorHalo, 1, @FColorHaloF);
    glUniform1f(FLocHaloThreshold, FHaloThreshold);

    glUniform1f(FLocShadowAmount, FShadowAmount);

    glUniform1f(FLocRotationSpeed, FRotationSpeed);

  end;
//  glGetError();

  ParentScene.TexMan.UnbindTexture;
end;

procedure TOGLCPlanetRenderer.Prepare(aTriangleType: TTriangleType;
  const aMVP: TOGLCMatrix; const aOpacity: single; aBlendMode: byte;
  const aParams: TPlanetParams; aTimeAccu: single);
var forceFlush: Boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP) or
                (FOpacity <> aOpacity) or
                (aTimeAccu <> FTimeAccu) or
                 not CompareMem(@aParams, @FParams, SizeOf(TPlanetParams));
  Batch_CheckIfNeedFlush(Self, aTriangleType, NIL, 0, aBlendMode, forceFlush);
  FMVP.CopyFrom(aMVP);
  FOpacity := aOpacity;
  FTimeAccu := aTimeAccu;
  Move(aParams, FParams, SizeOf(TPlanetParams));
end;

procedure TOGLCPlanetRenderer.PushQuad(const aX, aY: single; const aWidth,
  aHeight: integer; aFlipIndex: integer);
var area, texCoords: TQuadCoor;
  tci: PQuadCornerIndexes;
    p: Pxyuv;
    pIndex: PVertexIndex;
    currentIndex: TVertexIndex;
begin
  area[cBL].x := aX;
  area[cBL].y := aHeight + aY;
  area[cTL].x := aX;
  area[cTL].y := aY;
  area[cBR].x := aWidth + aX;
  area[cBR].y := aHeight + aY;
  area[cTR].x := aWidth + aX;
  area[cTR].y := aY;

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
    //tint.CopyFrom(aComputedTint);
    //mv.CopyFrom(aModelViewMatrix);
    u := texCoords[ tci^[0] ].x;
    v := texCoords[ tci^[0] ].y;
    x := area[cBL].x;
    y := area[cBL].y;
    //opacity := aOpacity;
  end;
  with p[1] do begin
    //tint.CopyFrom(aComputedTint);
    //mv.CopyFrom(aModelViewMatrix);
    u := texCoords[ tci^[1] ].x;
    v := texCoords[ tci^[1] ].y;
    x := area[cTL].x;
    y := area[cTL].y;
    //opacity := aOpacity;
  end;
  with p[2] do begin
    //tint.CopyFrom(aComputedTint);
    //mv.CopyFrom(aModelViewMatrix);
    u := texCoords[ tci^[2] ].x;
    v := texCoords[ tci^[2] ].y;
    x := area[cBR].x;
    y := area[cBR].y;
    //opacity := aOpacity;
  end;
  with p[3] do begin
    //tint.CopyFrom(aComputedTint);
    //mv.CopyFrom(aModelViewMatrix);
    u := texCoords[ tci^[3] ].x;
    v := texCoords[ tci^[3] ].y;
    x := area[cTR].x;
    y := area[cTR].y;
    //opacity := aOpacity;
  end;
end;

{ TOGLCSpritePlanet }

function TOGLCSpritePlanet.GetCloudsBlendMode: single;
begin
  Result := FParams.FCloudsBlendMode
end;

function TOGLCSpritePlanet.GetCloudsColorDistribution: single;
begin
  Result := FParams.FCloudsColorDistribution;
end;

function TOGLCSpritePlanet.GetCloudsFragmentation: single;
begin
  Result := FPArams.FCloudsFragmentation;
end;

function TOGLCSpritePlanet.GetCloudsOpacity: byte;
begin
  Result := Round(FParams.FCloudsopacity*255);
end;

function TOGLCSpritePlanet.GetCloudsTransformation: single;
begin
  Result := FParams.FCloudsTransformation;
end;

function TOGLCSpritePlanet.GetColorClouds1: TBGRAPixel;
begin
  Result := FParams.FColorClouds1F.ToBGRA;
end;

function TOGLCSpritePlanet.GetColorClouds2: TBGRAPixel;
begin
  Result := FParams.FColorClouds2F.ToBGRA;
end;

function TOGLCSpritePlanet.GetColorGround1: TBGRAPixel;
begin
  Result := FParams.FColorGround1F.ToBGRA;
end;

function TOGLCSpritePlanet.GetColorGround2: TBGRAPixel;
begin
  Result := FParams.FColorGround2F.ToBGRA;
end;

function TOGLCSpritePlanet.GetColorHalo: TBGRAPixel;
begin
  Result := FParams.FColorHaloF.ToBGRA;
end;

function TOGLCSpritePlanet.GetColorOcean: TBGRAPixel;
begin
  Result := FParams.FColorOceanF.ToBGRA;
end;

function TOGLCSpritePlanet.GetGroundAmount: single;
begin
  Result := FParams.FGroundAmount;
end;

function TOGLCSpritePlanet.GetGroundColorDistribution: single;
begin
  Result := FParams.FGroundColorDistribution;
end;

function TOGLCSpritePlanet.GetHaloSize: single;
begin
  Result := (FParams.FHaloThreshold - 0.1) / (0.49 - 0.1);
end;

function TOGLCSpritePlanet.GetRotationSpeed: single;
begin
  Result := FParams.FRotationSpeed;
end;

function TOGLCSpritePlanet.GetShadowAmount: single;
begin
  Result := FParams.FShadowAmount;
end;

procedure TOGLCSpritePlanet.SetCloudsBlendMode(AValue: single);
begin
  FParams.FCloudsBlendMode := EnsureRange(AValue, -1.0, 1.0);
end;

procedure TOGLCSpritePlanet.SetCloudsColorDistribution(AValue: single);
begin
  FParams.FCloudsColorDistribution := EnsureRange(AValue, 0.0, 1.0);
end;

procedure TOGLCSpritePlanet.SetCloudsFragmentation(AValue: single);
begin
  FParams.FCloudsFragmentation := EnsureRange(AValue, 1.0, 20.0);
end;

procedure TOGLCSpritePlanet.SetCloudsOpacity(AValue: byte);
begin
  FParams.FCloudsopacity := AValue/255;
end;

procedure TOGLCSpritePlanet.SetCloudsTransformation(AValue: single);
begin
  FParams.FCloudsTransformation := EnsureRange(AValue, 0.0, 1.0);
end;

procedure TOGLCSpritePlanet.SetColorClouds1(AValue: TBGRAPixel);
begin
  FParams.FColorClouds1F.InitFromBGRA(AValue);
end;

procedure TOGLCSpritePlanet.SetColorClouds2(AValue: TBGRAPixel);
begin
  FParams.FColorClouds2F.InitFromBGRA(AValue);
end;

procedure TOGLCSpritePlanet.SetColorGround1(AValue: TBGRAPixel);
begin
  FParams.FColorGround1F.InitFromBGRA(AValue);
end;

procedure TOGLCSpritePlanet.SetColorGround2(AValue: TBGRAPixel);
begin
  FParams.FColorGround2F.InitFromBGRA(AValue);
end;

procedure TOGLCSpritePlanet.SetColorHalo(AValue: TBGRAPixel);
begin
  FParams.FColorHaloF.InitFromBGRA(AValue);
end;

procedure TOGLCSpritePlanet.SetColorOcean(AValue: TBGRAPixel);
begin
  FParams.FColorOceanF.InitFromBGRA(AValue);
end;

procedure TOGLCSpritePlanet.SetGroundAmount(AValue: single);
begin
  FParams.FGroundAmount := EnsureRange(AValue, 0.0, 1.0);
end;

procedure TOGLCSpritePlanet.SetGroundColorDistribution(AValue: single);
begin
  FParams.FGroundColorDistribution := EnsureRange(AValue, 0.0, 1.0);
end;

procedure TOGLCSpritePlanet.SetRotationSpeed(AValue: single);
begin
  FParams.FRotationSpeed := Max(AValue, 0.0);
end;

procedure TOGLCSpritePlanet.SetShadowAmount(AValue: single);
begin
  FParams.FShadowAmount := EnsureRange(AValue, 0.0, 1.0);
end;

procedure TOGLCSpritePlanet.SetHaloSize(AValue: single);
begin
  AValue := EnsureRange(AValue, 0.0, 1.0);
  FParams.FHaloThreshold := (0.49 - 0.1)*AValue + 0.1;
end;

function TOGLCSpritePlanet.GetWidth: integer;
begin
  Result := FWidth;
end;

function TOGLCSpritePlanet.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TOGLCSpritePlanet.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  with FParams do
    FTimeAccu := FTimeAccu + aElapsedTime;
end;

procedure TOGLCSpritePlanet.DoDraw;
begin
  FPlanetRenderer.Prepare(ptTriangleStrip, FParentScene.MVPMatrix, FComputedOpacity, FBlendMode,
                          FParams, FTimeAccu);
  FPlanetRenderer.PushQuad(0, 0, FWidth, FHeight, FlipToIndex);
end;

constructor TOGLCSpritePlanet.Create(aParentScene: TOGLCScene;
  aPlanetRenderer: TOGLCPlanetRenderer);
begin
  inherited Create;
  FParentScene := aParentScene;
  FPlanetRenderer := aPlanetRenderer;

  // default parameters
  FParams.InitDefault;

  FWidth := 100;
  FHeight := 100;
end;

procedure TOGLCSpritePlanet.SetSize(aWidth, aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
end;

procedure TOGLCSpritePlanet.LoadParamsFromString(const s: string);
begin
  FParams.InitDefault;
  FParams.LoadParamsFromString(s);
end;

function TOGLCSpritePlanet.SaveParamsToString: string;
begin
  Result := FParams.SaveParamsToString;
end;

end.

