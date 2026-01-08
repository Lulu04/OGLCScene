unit u_proceduralcloud;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmapTypes,
  OGLCScene, glcorearb;

type

TOGLCSpriteClouds = class;

{ TCloudsParams }

TCloudsParams = packed record
  // values saved in preset
  ColorF: TColorF;
  Fragmentation, Transformation, Density: single;
  Relief: boolean;
  ThresholdTopBottomRightLeft: packed array[0..3] of single;
  TranslationSpeed: single;

  // not saved in preset
  Opacity, TimeAccu: single;

  procedure InitDefault;
  // Load the planet's parameters from a string constant
  procedure LoadParamsFromString(const s: string);
  // Pack the planet's parameters into a string
  function SaveParamsToString: string;
  function CheckString(const s: string): boolean;
end;


{ TOGLCCloudsRenderer }

TOGLCCloudsRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyuv>)
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
  '  uniform vec3 uColor = vec3(1.0, 1.0, 1.0);'#10+
  '  uniform bool uRelief = false;'#10+  // false= normal color    true= add relief to color
//  '  uniform float uDensity = 0.5;'#10+
//  '  uniform float uTransformation = 0.0;'#10+  // [0..1]
  '  uniform vec4 uThresholdTopBottomRightLeft = vec4(0.0001);'#10+ //0.0001 to 0.5
  '  uniform vec4 uOpacityTimeTranslationFragmentation = vec4(1.0, 0.0, 0.0, 20);'#10+
  '  uniform vec2 uDensityTransformation = vec2(0.5, 0.0);'#10+

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

  'void main()'#10+        //36
  '{'#10+
  '  vec4 col = vec4(0.0);'#10+
  '  vec3 unit;'#10+
  '  vec2 UV = TexCoords;'#10+
  // translate
  '  UV += vec2(uOpacityTimeTranslationFragmentation.z*uOpacityTimeTranslationFragmentation.y, 0.5);'#10+

  '  float theta = UV.y * 1.57079;'#10+
  '  float phi = UV.x * 3.14159;'#10+
  '  unit.x = sin(phi) * sin(theta);'#10+
  '  unit.y = cos(theta) * -1.0;'#10+
  '  unit.z = cos(phi) * sin(theta);'#10+
  '  unit = normalize(unit);'#10+

  '  float n = sin(noise(unit * uOpacityTimeTranslationFragmentation.w +'+
  '                      uOpacityTimeTranslationFragmentation.y*uDensityTransformation.y) * 0.5);'#10+
  '  n += noise(unit * uOpacityTimeTranslationFragmentation.w*2.0) * 0.25;'#10+    //50
  '  n += noise(unit * uOpacityTimeTranslationFragmentation.w*4.0) * 0.125;'#10+
  '  n += noise(unit * uOpacityTimeTranslationFragmentation.w*8.0) * 0.0625;'#10+

  '  float brightness = smoothstep(-1.0*uDensityTransformation.x, 1.0-uDensityTransformation.x, n);'#10+

  // fade on image edge
  '  float fade = smoothstep(0.0, uThresholdTopBottomRightLeft.y, TexCoords.y)*'#10+      // top
  '               smoothstep(0.0, uThresholdTopBottomRightLeft.w, TexCoords.x)*'#10+      // left
  '               smoothstep(0.0, uThresholdTopBottomRightLeft.z, 1.0-TexCoords.x)*'#10+  // right
  '               smoothstep(0.0, uThresholdTopBottomRightLeft.x, 1.0-TexCoords.y);'#10+  // bottom
  //'  brightness *= fade * fade*(3.0 - 2.0 * fade);'#10+
  '  brightness *= fade;'#10+ // * fade;'#10+ // pow(fade, 2.0);'#10+


  '  vec3 tempColor = uColor;'#10+
  '  if (uRelief)'#10+
  '    tempColor *=brightness;'#10+

  '  col.rgb += tempColor;'#10+
  '  col.a = brightness*uOpacityTimeTranslationFragmentation.x;'#10+

  '  FragColor = col;'#10+
  '}';
private
  FLocMVP,
  FLocOpacityTimeTranslationFragmentation,
  FLocColor, FLocRelief,
  FLocDensityTransformation, FLocThresholdTopBottomRightLeft: glint;

  FMVP: TOGLCMatrix;
  FParams: TCloudsParams;
protected
  procedure InitShaderCodeAndCallBack; override;
private
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix;
                    aBlendMode: byte;
                    const aParams: TCloudsParams); // à mettre dans aParams
  procedure PushQuad(const aModelArea: TQuadCoor; aFlipIndex: integer);
end;


{ TOGLCSpriteClouds }

TOGLCSpriteClouds = class(TSimpleSurfaceWithEffect)
private
  FModelArea: TQuadCoor;
  FWidth, FHeight: integer;
  FCloudsRenderer: TOGLCCloudsRenderer;
  FParams: TCloudsParams;
  function GetFadeBottom: single;
  function GetFadeLeft: single;
  function GetFadeRight: single;
  function GetFragmentation: single;
  function GetTopFade: single;
  function GetTransformation: single;
  function GetColor: TBGRAPixel;
  function GetDensity: single;
  function GetRelief: boolean;
  function GetTranslationSpeed: single;
  procedure SetFadeBottom(AValue: single);
  procedure SetFadeLeft(AValue: single);
  procedure SetFadeRight(AValue: single);
  procedure SetFragmentation(AValue: single);
  procedure SetTopFade(AValue: single);
  procedure SetTransformation(AValue: single);
  procedure SetColor(AValue: TBGRAPixel);
  procedure SetDensity(AValue: single);
  procedure SetRelief(AValue: boolean);
  procedure SetTranslationSpeed(AValue: single);
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
public
  constructor Create(aParentScene: TOGLCScene; aPlanetRenderer: TOGLCCloudsRenderer);

  procedure SetSize(aWidth, aHeight: integer);

  // Load the planet's parameters from a string constant
  procedure LoadParamsFromString(const s: string);
  // Pack the planet's parameters into a string
  function SaveParamsToString: string;

  // alpha is not used
  property Color: TBGRAPixel read GetColor write SetColor;
  // [1..20]
  property Fragmentation: single read GetFragmentation write SetFragmentation;
  // [0..1]
  property Transformation: single read GetTransformation write SetTransformation;
  // [0..1]
  property Density: single read GetDensity write SetDensity;
  // add a pseudo relief to clouds
  property Relief: boolean read GetRelief write SetRelief;
  // Allow to control the horizontal speed of the clouds
  // default value is 0
  property TranslationSpeed: single read GetTranslationSpeed write SetTranslationSpeed;

  // [0..1]
  property FadeTop: single read GetTopFade write SetTopFade;
  property FadeBottom: single read GetFadeBottom write SetFadeBottom;
  property FadeLeft: single read GetFadeLeft write SetFadeLeft;
  property FadeRight: single read GetFadeRight write SetFadeRight;
end;

implementation
uses Math;

{ TCloudsParams }

procedure TCloudsParams.InitDefault;
begin
  ColorF.InitFromBGRA(BGRA(255,255,255));
  Fragmentation := 5.0;
  Transformation := 0.0;
  Relief := False;
  Density := 0.5;


  TimeAccu := 0;
  Opacity := 1.0;
  TranslationSpeed := 0.0;
  ThresholdTopBottomRightLeft[0] := 0.0001;
  ThresholdTopBottomRightLeft[1] := 0.0001;
  ThresholdTopBottomRightLeft[2] := 0.0001;
  ThresholdTopBottomRightLeft[3] := 0.0001;
end;

procedure TCloudsParams.LoadParamsFromString(const s: string);
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

  if p.StringValueOf('Color', vs, '') then
    UnpackColorF(vs, ColorF);
  p.SingleValueOf('Fragmentation', Fragmentation, Fragmentation);
  p.SingleValueOf('Transformation', Transformation, Transformation);
  p.BooleanValueOf('Relief', Relief, Relief);
  p.SingleValueOf('Density', Density, Density);
  p.SingleValueOf('TranslationSpeed', TranslationSpeed, TranslationSpeed);
  p.SingleValueOf('ThresholdTop', ThresholdTopBottomRightLeft[0], ThresholdTopBottomRightLeft[0]);
  p.SingleValueOf('ThresholdBottom', ThresholdTopBottomRightLeft[1], ThresholdTopBottomRightLeft[1]);
  p.SingleValueOf('ThresholdRight', ThresholdTopBottomRightLeft[2], ThresholdTopBottomRightLeft[2]);
  p.SingleValueOf('ThresholdLeft', ThresholdTopBottomRightLeft[3], ThresholdTopBottomRightLeft[3]);
end;

function TCloudsParams.SaveParamsToString: string;
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

  p.Add('Color', PackColorF(ColorF));
  p.Add('Fragmentation', Fragmentation);
  p.Add('Transformation', Transformation);
  p.Add('Relief', Relief);
  p.Add('Density', Density);
  p.Add('TranslationSpeed', TranslationSpeed);
  p.Add('ThresholdTop', ThresholdTopBottomRightLeft[0]);
  p.Add('ThresholdBottom', ThresholdTopBottomRightLeft[1]);
  p.Add('ThresholdRight', ThresholdTopBottomRightLeft[2]);
  p.Add('ThresholdLeft', ThresholdTopBottomRightLeft[3]);

  Result := p.PackedProperty;
end;

function TCloudsParams.CheckString(const s: string): boolean;
var p: TProperties;
begin
  p.Split(s, '|');
  Result := p.CheckProperties(['Color', 'Fragmentation', 'Transformation', 'Density', 'Relief',
                'TranslationSpeed', 'ThresholdTop', 'ThresholdBottom', 'ThresholdRight',
                'ThresholdLeft']);
end;

{ TOGLCCloudsRenderer }

procedure TOGLCCloudsRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCCloudsRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(0);
end;

procedure TOGLCCloudsRenderer.GetUniformLocation;
begin
  with Shader do begin
    FLocMVP := GetUniform('uMVP');
    FLocOpacityTimeTranslationFragmentation := GetUniform('uOpacityTimeTranslationFragmentation');

    FLocColor := GetUniform('uColor');
    FLocRelief := GetUniform('uRelief');
    FLocDensityTransformation := GetUniform('uDensityTransformation');
    FLocThresholdTopBottomRightLeft := GetUniform('uThresholdTopBottomRightLeft');
  end;
end;

procedure TOGLCCloudsRenderer.SetUniformValuesAndTexture;
begin
  with FParams do begin
    glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);

    glUniform3fv(FLocColor, 1, @ColorF);
    glUniform4f(FLocOpacityTimeTranslationFragmentation, Opacity, TimeAccu, -TranslationSpeed, Fragmentation);
    glUniform1i(FLocRelief, Relief.ToInteger);
    glUniform2f(FLocDensityTransformation, sqrt(Density), Transformation);
    glUniform4fv(FLocThresholdTopBottomRightLeft, 1, @ThresholdTopBottomRightLeft);
  end;
  glGetError();

  ParentScene.TexMan.UnbindTexture;
end;

procedure TOGLCCloudsRenderer.Prepare(aTriangleType: TTriangleType;
  const aMVP: TOGLCMatrix; aBlendMode: byte; const aParams: TCloudsParams);
var forceFlush: Boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP) or
                 not CompareMem(@aParams, @FParams, SizeOf(TCloudsParams));
  Batch_CheckIfNeedFlush(Self, aTriangleType, NIL, 0, aBlendMode, forceFlush);
  FMVP.CopyFrom(aMVP);
  Move(aParams, FParams, SizeOf(TCloudsParams));
end;

procedure TOGLCCloudsRenderer.PushQuad(const aModelArea: TQuadCoor; aFlipIndex: integer);
var tci: PQuadCornerIndexes;
    p: Pxyuv;
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
    u := NoFlipQuadUV[ tci^[0] ].x;
    v := NoFlipQuadUV[ tci^[0] ].y;
    x := aModelArea[cBL].x;
    y := aModelArea[cBL].y;
  end;
  with p[1] do begin
    u := NoFlipQuadUV[ tci^[1] ].x;
    v := NoFlipQuadUV[ tci^[1] ].y;
    x := aModelArea[cTL].x;
    y := aModelArea[cTL].y;
  end;
  with p[2] do begin
    u := NoFlipQuadUV[ tci^[2] ].x;
    v := NoFlipQuadUV[ tci^[2] ].y;
    x := aModelArea[cBR].x;
    y := aModelArea[cBR].y;
  end;
  with p[3] do begin
    u := NoFlipQuadUV[ tci^[3] ].x;
    v := NoFlipQuadUV[ tci^[3] ].y;
    x := aModelArea[cTR].x;
    y := aModelArea[cTR].y;
  end;
end;

{ TOGLCSpriteClouds }

function TOGLCSpriteClouds.GetFragmentation: single;
begin
  Result := FPArams.Fragmentation;
end;

function TOGLCSpriteClouds.GetFadeBottom: single;
begin
  Result := FParams.ThresholdTopBottomRightLeft[1] - 0.0001;
end;

function TOGLCSpriteClouds.GetFadeLeft: single;
begin
  Result := FParams.ThresholdTopBottomRightLeft[3] - 0.0001;
end;

function TOGLCSpriteClouds.GetFadeRight: single;
begin
  Result := FParams.ThresholdTopBottomRightLeft[2] - 0.0001;
end;

function TOGLCSpriteClouds.GetTopFade: single;
begin
  Result := FParams.ThresholdTopBottomRightLeft[0] - 0.0001;
end;

function TOGLCSpriteClouds.GetTransformation: single;
begin
  Result := FParams.Transformation;
end;

function TOGLCSpriteClouds.GetColor: TBGRAPixel;
begin
  Result := FParams.ColorF.ToBGRA;
end;

function TOGLCSpriteClouds.GetDensity: single;
begin
  Result := FParams.Density;
end;

function TOGLCSpriteClouds.GetRelief: boolean;
begin
  Result := FParams.Relief;
end;

function TOGLCSpriteClouds.GetTranslationSpeed: single;
begin
  Result := FParams.TranslationSpeed;
end;

procedure TOGLCSpriteClouds.SetFadeBottom(AValue: single);
begin
  FParams.ThresholdTopBottomRightLeft[1] := EnsureRange(AValue, 0.0, 1.0) + 0.0001;
end;

procedure TOGLCSpriteClouds.SetFadeLeft(AValue: single);
begin
  FParams.ThresholdTopBottomRightLeft[3] := EnsureRange(AValue, 0.0, 1.0) + 0.0001;
end;

procedure TOGLCSpriteClouds.SetFadeRight(AValue: single);
begin
  FParams.ThresholdTopBottomRightLeft[2] := EnsureRange(AValue, 0.0, 1.0) + 0.0001;
end;

procedure TOGLCSpriteClouds.SetFragmentation(AValue: single);
begin
  FParams.Fragmentation := EnsureRange(AValue, 1.0, 20.0);
end;

procedure TOGLCSpriteClouds.SetTopFade(AValue: single);
begin
  FParams.ThresholdTopBottomRightLeft[0] := EnsureRange(AValue, 0.0, 1.0) + 0.0001;
end;

procedure TOGLCSpriteClouds.SetTransformation(AValue: single);
begin
  FParams.Transformation := EnsureRange(AValue, 0.0, 1.0);
end;

procedure TOGLCSpriteClouds.SetColor(AValue: TBGRAPixel);
begin
  FParams.ColorF.InitFromBGRA(AValue);
end;

procedure TOGLCSpriteClouds.SetDensity(AValue: single);
begin
  FParams.Density := AValue;
end;

procedure TOGLCSpriteClouds.SetRelief(AValue: boolean);
begin
  FParams.Relief := AValue;
end;

procedure TOGLCSpriteClouds.SetTranslationSpeed(AValue: single);
begin
  FParams.TranslationSpeed := AValue;
end;

function TOGLCSpriteClouds.GetWidth: integer;
begin
  Result := FWidth;
end;

function TOGLCSpriteClouds.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TOGLCSpriteClouds.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  with FParams do
    TimeAccu := TimeAccu + aElapsedTime;
end;

procedure TOGLCSpriteClouds.DoDraw;
begin
  FParams.Opacity := FComputedOpacity;
  FCloudsRenderer.Prepare(ptTriangleStrip, FParentScene.MVPMatrix, FBlendMode, FParams);
  FCloudsRenderer.PushQuad(FModelArea, FlipToIndex);
end;

constructor TOGLCSpriteClouds.Create(aParentScene: TOGLCScene; aPlanetRenderer: TOGLCCloudsRenderer);
begin
  inherited Create;
  FParentScene := aParentScene;
  FCloudsRenderer := aPlanetRenderer;

  // default parameters
  FParams.InitDefault;
  SetSize(100, 100);
end;

procedure TOGLCSpriteClouds.SetSize(aWidth, aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
  FModelArea.SetAsRectangle(aWidth, aHeight);
end;

procedure TOGLCSpriteClouds.LoadParamsFromString(const s: string);
begin
  FParams.InitDefault;
  FParams.LoadParamsFromString(s);
end;

function TOGLCSpriteClouds.SaveParamsToString: string;
begin
  Result := FParams.SaveParamsToString;
end;

end.

