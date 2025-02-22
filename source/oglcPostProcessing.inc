{$ifdef oglcINTERFACE}
type

{ TOGLCTexturedTriangleRendererWithFX }

TOGLCTexturedTriangleRendererWithFX = class(specialize TOGLCGenericPrimitiveRenderer<Txyuv>)
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

  FRAGMENT_SHADER =
   '#version 330 core'#10+
   '  out vec4 FragColor;'#10+
   '  in vec2 TexCoords;'#10+
   '  uniform sampler2D uTexUnit;'#10+
   '  uniform vec4 uTintColor;'#10+
   '  uniform float uOpacity;'#10+
   '  uniform int uFXFlag;'#10+
   '  uniform int uTexWidth, uTexHeight;'#10+
   '  uniform float uTime;'#10+
   '  uniform float uUnderWaterAmplitude = 0.01;'#10+ //   original 0.01     acceptable range: 0.1 to 0.001
   '  uniform float uUnderWaterDensity = 25.0;'#10+  // original 25    range is [1..100]
   '  uniform float uUnderWaterSpeed = 1.0;'#10+ // slow 0.1  1.0 normal   +++
   '  uniform int uBlurAmount = 3;'#10+         // 1, 2, 3, 4
   '  uniform float uPixelizeAmount = 0.5;'#10+  // 0.01 to 1.0
   '  uniform vec2 uShockWaveCenter=vec2(0.5,0.5);'#10+ // center of the shock
   '  uniform vec4 uShockWaveParams=vec4(10.0, 0.8, 0.1, 0.3);'#10+ // 10.0, 0.8, 0.1(ring width),  1.0(timeMultiplicator)
   '  uniform vec4 uShockWaveColor=vec4(1.0,0.0,0.8,0.2);'#10+
   ' float Random1f(float x){'#10+
   '    return fract(sin(x)*1.0);'#10+
   ' }'#10+
   ' float random(vec2 st) {'#10+
   '   return fract(sin(dot(st.xy,'#10+
   '                        vec2(12.9898,78.233)))*'#10+
   '       43758.5453123);'#10+
   '}'#10+
   'void main()'#10+
   '{'#10+
   '  vec4 col = texture(uTexUnit, TexCoords);'#10+
   '  int flags = uFXFlag;'#10+
   '  vec2 texCoords = TexCoords;'#10+
   // ShockWave from https://www.geeks3d.com/20091116/shader-library-2d-shockwave-post-processing-filter-glsl/
   // modified with color and time adjustment
   ' if (flags >= 0x200){'#10+
   '   flags -= 0x200;'#10+
   '   vec2 uv = texCoords;'#10+
   '   float halfTime = uTime*uShockWaveParams.w;'#10+
   '   float distance = distance(texCoords, uShockWaveCenter);'#10+
   '   if ( (distance <= (halfTime + uShockWaveParams.z)) &&'#10+
   '       (distance >= (halfTime - uShockWaveParams.z)) )'#10+
   '   {'#10+
   '    float diff = (distance - halfTime);'#10+
   '    float powDiff = 1.0 - pow(abs(diff*uShockWaveParams.x),'#10+
   '                                uShockWaveParams.y);'#10+
   '    float diffTime = diff  * powDiff;'#10+
   '    vec2 diffUV = normalize(texCoords - uShockWaveCenter);'#10+
   '    texCoords = texCoords + (diffUV * diffTime);'#10+
   '    col = texture2D(uTexUnit, texCoords);'#10+
   '    col += vec4(uShockWaveColor.rgb,uShockWaveColor.a-distance);'#10+
   '   }'#10+
   ' }'#10+
   // Dream Vision from https://www.geeks3d.com/20091112/shader-library-dream-vision-post-processing-filter-glsl/
   ' if (flags >= 0x100){'#10+
   '   flags -= 0x100;'#10+
   '   vec4 c = col;'#10+
   '   c += texture2D(uTexUnit, texCoords+0.001);'#10+
   '   c += texture2D(uTexUnit, texCoords+0.003);'#10+
   '   c += texture2D(uTexUnit, texCoords+0.005);'#10+
   '   c += texture2D(uTexUnit, texCoords+0.007);'#10+
   '   c += texture2D(uTexUnit, texCoords+0.009);'#10+
   '   c += texture2D(uTexUnit, texCoords+0.011);'#10+
   '   c += texture2D(uTexUnit, texCoords-0.001);'#10+
   '   c += texture2D(uTexUnit, texCoords-0.003);'#10+
   '   c += texture2D(uTexUnit, texCoords-0.005);'#10+
   '   c += texture2D(uTexUnit, texCoords-0.007);'#10+
   '   c += texture2D(uTexUnit, texCoords-0.009);'#10+
   '   c += texture2D(uTexUnit, texCoords-0.011);'#10+
   '   c.rgb = vec3((c.r+c.g+c.b)/3.0);'#10+
   '   c = c / 9.5;'#10+
   '   col = c;'#10+
   ' }'#10+
   // pixelize
   ' if (flags >= 0x80){'#10+
   '   flags -= 0x80;'#10+
   '   vec2 pixelizeDelta = vec2(uPixelizeAmount*uTexWidth, uPixelizeAmount*uTexHeight);'#10+
   '   vec2 texCoords = round(texCoords * pixelizeDelta) / pixelizeDelta;'#10+
   '   float dx = 1.0f/(pixelizeDelta.x*5.0f);'#10+
   '   float dy = 1.0f/(pixelizeDelta.y*5.0f);'#10+
   '   texCoords -= vec2(dx*2.5, dy*2.5);'#10+      // avoid image shift
   '   vec4 sum1 = vec4(0,0,0,0);'#10+
   '   for(int i = 0; i < 5; i++)'#10+
   '	   for(int j = 0; j < 5; j++)'#10+
   '		sum1 += texture2D(uTexUnit, texCoords.xy + vec2(i * dx, j * dy));'#10+
   '   col = vec4(sum1/25);'#10+
   ' }'#10+
   // under water effect created by OmarShehata https://www.shadertoy.com/view/4tG3WR
   ' if (flags >= 0x40){'#10+
   '  flags -= 0x40;'#10+
   '  vec2 uv = TexCoords;'#10+
   '  float X = TexCoords.x*uUnderWaterDensity+uTime*uUnderWaterSpeed;'#10+   //25
   '  float Y = TexCoords.y*uUnderWaterDensity+uTime*uUnderWaterSpeed;'#10+
   '  uv.y += cos(X+Y)*uUnderWaterAmplitude*cos(Y);'#10+
   '  uv.x += sin(X-Y)*uUnderWaterAmplitude*sin(Y);'#10+
   '  col = texture(uTexUnit, uv);'#10+
   '  texCoords = uv;'#10+
   ' }'#10+
   // blur
   ' if (flags >= 0x20){'#10+
   '   flags -= 0x20;'#10+
   '   const int BlurDivisor[4] = int[](4, 18, 30, 60);'#10+
   '   float dx = 1.0f/uTexWidth;'#10+ //step on x
   '   float dy = 1.0f/uTexHeight;'#10+ //step on y
   '   vec3 sum = vec3(0,0,0);'#10+
   '   for(int i = -uBlurAmount; i < uBlurAmount; i++)'#10+
   '	   for(int j = -uBlurAmount;j < uBlurAmount; j++)'#10+
   '		sum += texture2D(uTexUnit, texCoords.xy + vec2(i * dx, j * dy)).rgb;'#10+
   '   col = vec4(vec3(sum/BlurDivisor[uBlurAmount-1]), col.a);'#10+
   ' }'#10+
   // sharpen
   ' if (flags >= 0x10){'#10+
   '   flags -= 0x10;'#10+
   '   float kernel[9] = float[]('#10+
   '   -1, -1, -1,'#10+
   '    -1,  9, -1,'#10+
   '   -1, -1, -1'#10+
   '   );'#10+
   '   float dx = 1.0f/uTexWidth;//step on x'#10+
   '   float dy = 1.0f/uTexHeight;//step on y'#10+
   '   vec2 offsets[9] = vec2[]('#10+
   '   vec2(-dx,  dy),'#10+ // top-left
   '   vec2( 0.0f,    dy),'#10+ // top-center
   '   vec2( dx,  dy),'#10+ // top-right
   '   vec2(-dx,  0.0f),'#10+   // center-left
   '   vec2( 0.0f,    0.0f),'#10+   // center-center
   '   vec2( dx,  0.0f),'#10+   // center-right
   '   vec2(-dx, -dy),'#10+ // bottom-left
   '   vec2( 0.0f,   -dy),'#10+ // bottom-center
   '   vec2( dx, -dy)'#10+  // bottom-right
   '   );'#10+
   '   vec3 sampleTex[9];'#10+
   '   for(int i = 0; i < 9; i++)'#10+
   '      sampleTex[i] = vec3(texture2D(uTexUnit, texCoords.xy + offsets[i]));'#10+
   '   sampleTex[4] = col.rgb;'#10+
   '   vec3 c = vec3(0.0);'#10+
   '   for(int i = 0; i < 9; i++)'#10+
   '      c += sampleTex[i] * kernel[i];'#10+
   '   col = vec4(c, col.a);'#10+
   ' }'#10+
   // sepia
   ' if (flags >= 0x08){'#10+
   '   flags -= 0x08;'#10+
   '   col = vec4(0.393*col.r + 0.769*col.g + 0.189*col.b,'#10+
   '              0.349*col.r + 0.686*col.g + 0.168*col.b,'#10+
   '              0.272*col.r + 0.534*col.g + 0.131*col.b, col.a);'#10+
   ' }'#10+
   // grayscale
   ' if (flags >= 0x04){'#10+
   '   flags -= 0x04;'#10+
   '   float average = 0.2126 * col.r + 0.7152 * col.g + 0.0722 * col.b;'#10+
   '   col = vec4(average, average, average, col.a);'#10+
   ' }'#10+
   // color inversion
   ' if (flags >= 0x02){'#10+
   '   flags -= 0x02;'#10+
   '   col = vec4(vec3(1.0 - col), col.a);'#10+
   ' }'#10+

   // Tint
   ' if (flags == 0x01){'#10+
   '  float tintAlphaX2 = uTintColor.a*2;'#10+
   '  if (uTintColor.a >= 0.5)'#10+
   '   {'#10+
   '     tintAlphaX2 = tintAlphaX2-1;'#10+
   '     col = mix(col, uTintColor, tintAlphaX2);'#10+  // replace color
   '   }'#10+
   '  else if (uTintColor.a > 0)'#10+
   '     col = col + uTintColor*tintAlphaX2;'#10+     // mix color
   ' }'#10+
   '  col.a = col.a*uOpacity;'#10+
   '  if (col.a == 0) discard;'#10+
   '  FragColor = col;'#10+
   '}' ;
private
  FLocMVP,
  FLocTextureUnit,
  FLocTintColor,
  FLocOpacity,
  FLocFXFlags,
  FLocTexWidth, FLocTexHeight,
  FLocTime,
  FLocUnderWaterAmplitude, FLocUnderWaterDensity, FLocUnderWaterSpeedCoeff,
  FLocBlurAmount,
  FLocPixelizeAmount,
  FLocShockWaveCenter, FLocShockWaveParams, FLocShockWaveColor: GLint;
  FMVP: TOGLCMatrix;
  FTintF: TColorF;
  FOpacity: single;
  FFXBitMask, FTexWidth, FTexHeight: GLuint;
  FUnderWaterAmplitude, FUnderWaterDensity, FUnderWaterTimeMultiplicator: single;
  FShockWaveCenterU, FShockWaveCenterV,
  FShockWaveParam1, FShockWaveParam2, FShockWaveParam3, FShockWaveTimeMultiplicator: single;
  FShockWaveColorF: TColorF;

  FBlurAmount: GLint;
  FPixelizeAmount: single;
  FTimeOrigin: QWord;
  procedure InitShaderCodeAndCallBack; override;
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; aTexture: PTexture; aTextureUnit: integer;
                    const aMVP: TOGLCMatrix; const aOpacity: single; aBlendMode: byte);

  procedure PushFrame(const aFrameIndex, aFlipIndex: integer; const aXPosOffset, aYPosOffset: single);

  procedure ResetTime;
  procedure SetFXBitMask(const aFXBitMask: GLuint);
  procedure SetPixelizeParams(aAmount: single);
  procedure SetUnderWaterParams(aAmplitude, aDensity, aTimeMultiplicator: single);
  procedure SetBlurParams(aAmount: integer);
  procedure SetShockWaveParams(aCenterU, aCenterV, aTimeMultiplicator: single; const aColor: TColorF);
  procedure SetTintValue(const aComputedTint: TColorF);
end;

{ TOGLCPostProcessingEngine }

TOGLCPostProcessingEngine = class
private
  FRenderToTexture: TOGLCRenderToTexture;
  FParentScene: TOGLCScene;
  FCurrentFXBitMask: GLuint;
  FComputedTint: TColorF;
  FUnderWaterAmplitude, FUnderWaterDensity, FUnderWaterTimeMultiplicator: single;
  FBlurAmount: GLint;
  FPixelizeAmount: single;
  FShockWaveCenterU, FShockWaveCenterV, FShockWaveTimeMultiplicator: single;
  FShockWaveColorF: TColorF;
  FIsActive: boolean;
  procedure CreateTargetTexture;
  procedure FreeTargetTexture;
  procedure Draw;
public
  Parameters: TOGLCPostProcessingParameters;
  constructor Create(aParentScene: TOGLCScene);
  destructor Destroy; override;

  procedure ApplyFX(aParams: POGLCPostProcessingParameters);
  procedure Flush;
end;

{ TOGLCPostProcessingUtils }
// this record groups a bunch of convenient procedures to ease the control of the post-processing effects
// from the TOGLCScene.PostProcessing property.
TOGLCPostProcessingUtils = record
private
  FParentScene: TOGLCScene;
public
  // You must call this method before using post-processing effect.
  // It reserve some memory in video ram for a temporary texture.
  // The best place to call this method is inside TScreenTemplate.CreateObjects
  procedure CreateTargetTexture;
  // Free the memory for temporary texture.
  // The best place to call this method is inside TScreenTemplate.FreeObjects
  procedure FreeTargetTexture;
  // Force the target texture to be rendered.
  procedure Flush;

  // Enable post-processing FX on one or several the layers.
  procedure EnableFXOnLayers(aFXs: TOGLCPostProcessingFX; const aLayers: TArrayOfInteger);
  // Enable post-processing FX on a range of layers.
  procedure EnableFXOnLayerRange(aFXs: TOGLCPostProcessingFX; aFirstLayer, aLastLayer: integer);
  // Enable post-processing FX on all layers.
  procedure EnableFXOnAllLayers(aFXs: TOGLCPostProcessingFX);

  // Disable post-processing FX on one or several the layers.
  procedure DisableFXOnLayers(aFXs: TOGLCPostProcessingFX; const aLayers: TArrayOfInteger);
  // Disable post-processing FX on a range of layers.
  procedure DisableFXOnLayerRange(aFXs: TOGLCPostProcessingFX; aFirstLayer, aLastLayer: integer);
  // Disable post-processing FX on all layers.
  procedure DisableFXOnAllLayers(aFXs: TOGLCPostProcessingFX);

  // Disable all post-processing FX on all layers.
  procedure DisableAllFXOnAllLayers;

  // aAmplitude: the waves amplitude [1..100]
  // aDensity: the waves density [1..100]
  // aTimeMultiplicator mini is 0.1 (slow)  -->  1.0 normal speed(default)  --->  greater than 1.0 (fast)
  procedure SetUnderWaterParamsOnLayers(aAmplitude, aDensity, aTimeMultiplicator: single; const aLayers: TArrayOfInteger);
  // aAmplitude: the waves amplitude [1..100]
  // aDensity: the waves density [1..100]
  // aTimeMultiplicator mini is 0.1 (slow)  -->  1.0 normal speed(default)  --->  greater than 1.0 (fast)
  procedure SetUnderWaterParamsOnLayerRange(aAmplitude, aDensity, aTimeMultiplicator: single; aFirstLayer, aLastLayer: integer);
  // aAmplitude: the waves amplitude [1..100]
  // aDensity: the waves density [1..100]
  // aTimeMultiplicator must be >0    <1 slow,  1=normal speed,  >1 fast,    Default is 1.0
  procedure SetUnderWaterParamsOnAllLayers(aAmplitude, aDensity, aTimeMultiplicator: single);

  // aAmount range is 1 soft   .....    4 full
  procedure SetBlurParamsOnLayers(aAmount: integer; const aLayers: TArrayOfInteger);
  // aAmount range is 1 soft   .....    4 full
  procedure SetBlurParamsOnLayerRange(aAmount: integer; aFirstLayer, aLastLayer: integer);
  // aAmount range is 1 soft   .....    4 full
  procedure SetBlurParamsOnAllLayers(aAmount: integer);

  // aCenterU, aCenterV are the coordinates of the center of the wave, expressed in range [0..1]  Default value is 0.5
  // aTimeMultiplicator must be >0    <1 slow,  1=normal speed,  >1 fast,    Default is 0.3
  // aColor is the tint color of the wave. Default is transparent
  procedure SetShockWaveParamsOnLayers(aCenterU, aCenterV, aTimeMultiplicator: single; const aColor: TBGRAPixel;
                                       const aLayers: TArrayOfInteger);
  // aCenterU, aCenterV are the coordinates of the center of the wave, expressed in range [0..1]  Default value is 0.5
  // aTimeMultiplicator must be >0    <1 slow,  1=normal speed,  >1 fast,    Default is 0.3
  // aColor is the tint color of the wave. Default is transparent
  procedure SetShockWaveParamsOnLayerRange(aCenterU, aCenterV, aTimeMultiplicator: single; const aColor: TBGRAPixel;
                                           aFirstLayer, aLastLayer: integer);
  // aCenterU, aCenterV are the coordinates of the center of the wave, expressed in range [0..1]  Default value is 0.5
  // aTimeMultiplicator must be >0    <1 slow,  1=normal speed,  >1 fast,    Default is 0.3
  // aColor is the tint color of the wave. Default is transparent
  procedure SetShockWaveParamsOnAllLayers(aCenterU, aCenterV, aTimeMultiplicator: single; const aColor: TBGRAPixel);

  // reset the time variable to 0 in the post-processing shader. Needed before starting the ShockWave effect.
  procedure ResetTimeVariable;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCPostProcessingUtils }

procedure TOGLCPostProcessingUtils.CreateTargetTexture;
begin
  FParentScene.FPostProcessingEngine.CreateTargetTexture;
end;

procedure TOGLCPostProcessingUtils.FreeTargetTexture;
begin
  FParentScene.FPostProcessingEngine.FreeTargetTexture;
end;

procedure TOGLCPostProcessingUtils.Flush;
begin
  FParentScene.FPostProcessingEngine.Flush;
end;

procedure TOGLCPostProcessingUtils.EnableFXOnLayers(aFXs: TOGLCPostProcessingFX; const aLayers: TArrayOfInteger);
var i: integer;
begin
  if Length(aLayers) = 0 then exit;
  for i in aLayers do
    FParentScene.Layer[i].PostProcessing.Enable(aFXs);
end;

procedure TOGLCPostProcessingUtils.EnableFXOnLayerRange(aFXs: TOGLCPostProcessingFX; aFirstLayer, aLastLayer: integer);
var i: integer;
begin
  if aLastLayer < aFirstLayer then begin
    i := aLastLayer;
    aLastLayer := aFirstLayer;
    aFirstLayer := i;
  end;
  for i:=aFirstLayer to aLastLayer do
    FParentScene.Layer[i].PostProcessing.Enable(aFXs);
end;

procedure TOGLCPostProcessingUtils.EnableFXOnAllLayers(aFXs: TOGLCPostProcessingFX);
var i: integer;
begin
  for i:=0 to FParentScene.LayerCount-1 do
    FParentScene.Layer[i].PostProcessing.Enable(aFXs);
end;

procedure TOGLCPostProcessingUtils.DisableFXOnLayers(aFXs: TOGLCPostProcessingFX; const aLayers: TArrayOfInteger);
var i: integer;
begin
  if Length(aLayers) = 0 then exit;
  for i in aLayers do
    FParentScene.Layer[i].PostProcessing.Disable(aFXs);
end;

procedure TOGLCPostProcessingUtils.DisableFXOnLayerRange(aFXs: TOGLCPostProcessingFX; aFirstLayer, aLastLayer: integer);
var i: integer;
begin
  if aLastLayer < aFirstLayer then begin
    i := aLastLayer;
    aLastLayer := aFirstLayer;
    aFirstLayer := i;
  end;
  for i:=aFirstLayer to aLastLayer do
    FParentScene.Layer[i].PostProcessing.Disable(aFXs);
end;

procedure TOGLCPostProcessingUtils.DisableFXOnAllLayers(aFXs: TOGLCPostProcessingFX);
var i: integer;
begin
  for i:=0 to FParentScene.LayerCount-1 do
    FParentScene.Layer[i].PostProcessing.Disable(aFXs);
end;

procedure TOGLCPostProcessingUtils.DisableAllFXOnAllLayers;
var i: integer;
begin
  for i:=0 to FParentScene.LayerCount-1 do
    FParentScene.Layer[i].PostProcessing.DisableAll;
end;

procedure TOGLCPostProcessingUtils.SetUnderWaterParamsOnLayers(aAmplitude,
  aDensity, aTimeMultiplicator: single; const aLayers: TArrayOfInteger);
var i: integer;
begin
  if Length(aLayers) = 0 then exit;
  for i in aLayers do
    FParentScene.Layer[i].PostProcessing.SetUnderWaterParams(aAmplitude, aDensity, aTimeMultiplicator);
end;

procedure TOGLCPostProcessingUtils.SetUnderWaterParamsOnLayerRange(aAmplitude,
  aDensity, aTimeMultiplicator: single; aFirstLayer, aLastLayer: integer);
var i: integer;
begin
  if aLastLayer < aFirstLayer then begin
    i := aLastLayer;
    aLastLayer := aFirstLayer;
    aFirstLayer := i;
  end;
  for i:=aFirstLayer to aLastLayer do
    FParentScene.Layer[i].PostProcessing.SetUnderWaterParams(aAmplitude, aDensity, aTimeMultiplicator);
end;

procedure TOGLCPostProcessingUtils.SetUnderWaterParamsOnAllLayers(aAmplitude,
  aDensity, aTimeMultiplicator: single);
var i: integer;
begin
  for i:=0 to FParentScene.LayerCount-1 do
    FParentScene.Layer[i].PostProcessing.SetUnderWaterParams(aAmplitude, aDensity, aTimeMultiplicator);
end;

procedure TOGLCPostProcessingUtils.SetBlurParamsOnLayers(aAmount: integer; const aLayers: TArrayOfInteger);
var i: integer;
begin
  if Length(aLayers) = 0 then exit;
  for i in aLayers do
    FParentScene.Layer[i].PostProcessing.SetBlurParams(aAmount);
end;

procedure TOGLCPostProcessingUtils.SetBlurParamsOnLayerRange(aAmount: integer;
  aFirstLayer, aLastLayer: integer);
var i: integer;
begin
  if aLastLayer < aFirstLayer then begin
    i := aLastLayer;
    aLastLayer := aFirstLayer;
    aFirstLayer := i;
  end;
  for i:=aFirstLayer to aLastLayer do
    FParentScene.Layer[i].PostProcessing.SetBlurParams(aAmount);
end;

procedure TOGLCPostProcessingUtils.SetBlurParamsOnAllLayers(aAmount: integer);
var i: integer;
begin
  for i:=0 to FParentScene.LayerCount-1 do
    FParentScene.Layer[i].PostProcessing.SetBlurParams(aAmount);
end;

procedure TOGLCPostProcessingUtils.SetShockWaveParamsOnLayers(aCenterU,
  aCenterV, aTimeMultiplicator: single; const aColor: TBGRAPixel;
  const aLayers: TArrayOfInteger);
var i: integer;
begin
  if Length(aLayers) = 0 then exit;
  for i in aLayers do
    FParentScene.Layer[i].PostProcessing.SetShockWaveParams(aCenterU, aCenterV, aTimeMultiplicator, aColor);
end;

procedure TOGLCPostProcessingUtils.SetShockWaveParamsOnLayerRange(aCenterU,
  aCenterV, aTimeMultiplicator: single; const aColor: TBGRAPixel; aFirstLayer,
  aLastLayer: integer);
var i: integer;
begin
  if aLastLayer < aFirstLayer then begin
    i := aLastLayer;
    aLastLayer := aFirstLayer;
    aFirstLayer := i;
  end;
  for i:=aFirstLayer to aLastLayer do
    FParentScene.Layer[i].PostProcessing.SetShockWaveParams(aCenterU, aCenterV, aTimeMultiplicator, aColor);
end;

procedure TOGLCPostProcessingUtils.SetShockWaveParamsOnAllLayers(aCenterU,
  aCenterV, aTimeMultiplicator: single; const aColor: TBGRAPixel);
var i: integer;
begin
  for i:=0 to FParentScene.LayerCount-1 do
    FParentScene.Layer[i].PostProcessing.SetShockWaveParams(aCenterU, aCenterV, aTimeMultiplicator, aColor);
end;

procedure TOGLCPostProcessingUtils.ResetTimeVariable;
begin
  FParentScene.TexturedTriangleRendererWithFX.ResetTime;
end;

{ TOGLCTexturedTriangleRendererWithFX }

procedure TOGLCTexturedTriangleRendererWithFX.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;

  ResetTime;
end;

procedure TOGLCTexturedTriangleRendererWithFX.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(0);
end;

procedure TOGLCTexturedTriangleRendererWithFX.GetUniformLocation;
begin
 with FShader do
 begin
   Use;
   FLocMVP := GetUniform('uMVP');
   FLocTextureUnit := GetUniform('uTexUnit');
   FLocTintColor := GetUniform('uTintColor');
   FLocOpacity := GetUniform('uOpacity');
   FLocFXFlags := GetUniform('uFXFlag');
   FLocTexWidth := GetUniform('uTexWidth');
   FLocTexHeight := GetUniform('uTexHeight');
   FLocTime := GetUniform('uTime');
   FLocUnderWaterAmplitude := GetUniform('uUnderWaterAmplitude');
   FLocUnderWaterDensity := GetUniform('uUnderWaterDensity');
   FLocUnderWaterSpeedCoeff := GetUniform('uUnderWaterSpeed');
   FLocBlurAmount := GetUniform('uBlurAmount');
   FLocPixelizeAmount := GetUniform('uPixelizeAmount');
   FLocShockWaveCenter := GetUniform('uShockWaveCenter');
   FLocShockWaveParams := GetUniform('uShockWaveParams');
   FLocShockWaveColor := GetUniform('uShockWaveColor');

   Release;
 end;
end;

procedure TOGLCTexturedTriangleRendererWithFX.SetUniformValuesAndTexture;
begin
  glUniform1i(FLocTextureUnit, FBatch^.CurrentTextureUnit);
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
  glUniform4fv(FLocTintColor, 1, @FTintF);
  glUniform1f(FLocOpacity, FOpacity);
  glUniform1i(FLocFXFlags, FFXBitMask);
  glUniform1i(FLocTexWidth, FTexWidth);
  glUniform1i(FLocTexHeight, FTexHeight);
  glUniform1f(FLocTime, (GetTickCount64-FTimeOrigin)*0.001);
  glUniform1f(FLocUnderWaterAmplitude, FUnderWaterAmplitude);
  glUniform1f(FLocUnderWaterDensity, FUnderWaterDensity);
  glUniform1f(FLocUnderWaterSpeedCoeff, FUnderWaterTimeMultiplicator);
  glUniform1i(FLocBlurAmount, FBlurAmount);
  glUniform1f(FLocPixelizeAmount, FPixelizeAmount);
  glUniform2f(FLocShockWaveCenter, FShockWaveCenterU, FShockWaveCenterV);
  glUniform4f(FLocShockWaveParams, FShockWaveParam1, FShockWaveParam2, FShockWaveParam3, FShockWaveTimeMultiplicator);
  glUniform4fv(FLocShockWaveColor, 1, @FShockWaveColorF);
  FParentScene.TexMan.Bind(FBatch^.CurrentTexture, FBatch^.CurrentTextureUnit);
end;

procedure TOGLCTexturedTriangleRendererWithFX.Prepare(aTriangleType: TTriangleType;
  aTexture: PTexture; aTextureUnit: integer; const aMVP: TOGLCMatrix;
  const aOpacity: single; aBlendMode: byte);
var forceFlush: boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP) or
                (FOpacity <> aOpacity);
  if aTexture <> NIL then
    forceFlush := forceFlush or (aTexture^.TextureWidth <> FTexWidth)
                             or (aTexture^.TextureHeight <> FTexHeight);

  Batch_CheckIfNeedFlush(Self, aTriangleType, aTexture, aTextureUnit, aBlendMode, forceFlush);

  FBatch^.CurrentTextureUnit := aTextureUnit;
  FMVP.CopyFrom(aMVP);
  FOpacity := aOpacity;
  if aTexture <> NIL then begin
    FTexWidth := aTexture^.TextureWidth;
    FTexHeight := aTexture^.TextureHeight;
  end;
end;

procedure TOGLCTexturedTriangleRendererWithFX.PushFrame(const aFrameIndex,
  aFlipIndex: integer; const aXPosOffset, aYPosOffset: single);
var w, h: single;
    p: Pxyuv;
    pIndex: PVertexIndex;
    tci: PQuadCornerIndexes;
    texCoord: PQuadCoor;
    fi: Integer;
    currentIndex: Int64;
    tex: PTexture;
begin
  tex := FBatch^.CurrentTexture;
  fi := EnsureRange(aFrameIndex, 0, tex^.FrameCount-1);
  texCoord := @tex^.FramesCoord[fi];
  w := tex^.FrameWidth;
  h := tex^.FrameHeight;
  currentIndex := FIndexInAttribsArray;
  case FBatch^.CurrentPrimitiveType of
    ptTriangleStrip: begin
      tci := @FLIP_INDEXES[aFlipIndex];
      p := QueryVertex(4);
      with p[0] do begin             // BD
        u := texCoord^[ tci^[0] ].x; // AC
        v := texCoord^[ tci^[0] ].y;
        x := aXPosOffset;
        y := h + aYPosOffset;
      end;
      with p[1] do begin
        u := texCoord^[ tci^[1] ].x;
        v := texCoord^[ tci^[1] ].y;
        x := aXPosOffset;
        y := aYPosOffset;
      end;
      with p[2] do begin
        u := texCoord^[ tci^[2] ].x;
        v := texCoord^[ tci^[2] ].y;
        x := w + aXPosOffset;
        y := h + aYPosOffset;
      end;
      with p[3] do begin
        u := texCoord^[ tci^[3] ].x;
        v := texCoord^[ tci^[3] ].y;
        x := w + aXPosOffset;
        y := aYPosOffset;
      end;
    end;

    ptTriangles: begin
      tci := @FLIP_INDEXES[aFlipIndex];
      p := QueryVertex(6);
      with p[0] do begin  // A           // B   BD
        u := texCoord^[ tci^[0] ].x;     // AC  C
        v := texCoord^[ tci^[0] ].y;
        x := aXPosOffset;
        y := h + aYPosOffset;
      end;
      with p[1] do begin  // B
        u := texCoord^[ tci^[1] ].x;
        v := texCoord^[ tci^[1] ].y;
        x := aXPosOffset;
        y := aYPosOffset;
      end;
      with p[2] do begin  // C
        u := texCoord^[ tci^[2] ].x;
        v := texCoord^[ tci^[2] ].y;
        x := w + aXPosOffset;
        y := h + aYPosOffset;
      end;
      Move(p[2], p[3], SizeOf(Txyuv)); // C
      Move(p[1], p[4], SizeOf(Txyuv)); // B
      with p[5] do begin               // D
        u := texCoord^[ tci^[3] ].x;
        v := texCoord^[ tci^[3] ].y;
        x := w + aXPosOffset;
        y := aYPosOffset;
      end;
    end;
  end;
  // indices
  case FBatch^.CurrentPrimitiveType of
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
end;

procedure TOGLCTexturedTriangleRendererWithFX.ResetTime;
begin
  FTimeOrigin := GetTickCount64;
end;

procedure TOGLCTexturedTriangleRendererWithFX.SetFXBitMask(const aFXBitMask: GLuint);
begin
  FFXBitMask := aFXBitMask;
end;

procedure TOGLCTexturedTriangleRendererWithFX.SetPixelizeParams(aAmount: single);
begin
  aAmount := 1-(aAmount*0.7+0.2);
  FPixelizeAmount := aAmount*aAmount*aAmount;
end;

procedure TOGLCTexturedTriangleRendererWithFX.SetUnderWaterParams(aAmplitude, aDensity, aTimeMultiplicator: single);
begin
  FUnderWaterAmplitude := aAmplitude;
  FUnderWaterDensity := aDensity;
  FUnderWaterTimeMultiplicator := aTimeMultiplicator;
end;

procedure TOGLCTexturedTriangleRendererWithFX.SetBlurParams(aAmount: integer);
begin
  FBlurAmount := aAmount;
end;

procedure TOGLCTexturedTriangleRendererWithFX.SetShockWaveParams(aCenterU, aCenterV, aTimeMultiplicator: single; const aColor: TColorF);
begin
  FShockWaveCenterU := aCenterU;
  FShockWaveCenterV := aCenterV;
  FShockWaveTimeMultiplicator := aTimeMultiplicator;
  FShockWaveColorF.CopyFrom(aColor);
  FShockWaveParam1 := 10.0;
  FShockWaveParam2 := 0.8;
  FShockWaveParam3 := 0.1;      // ring width (?)
end;

procedure TOGLCTexturedTriangleRendererWithFX.SetTintValue(const aComputedTint: TColorF);
begin
  FTintF.CopyFrom(aComputedTint);
end;

{ TOGLCPostProcessingEngine }

procedure TOGLCPostProcessingEngine.CreateTargetTexture;
begin
  if FRenderToTexture = NIL then begin
    FRenderToTexture := TOGLCRenderToTexture.Create(FParentScene, FParentScene.Width, FParentScene.Height, 0, True);
    if not FRenderToTexture.Ready
      then FParentScene.LogError('TOGLCPostProcessingEngine: RenderToTexture is not ready !...')
      else FParentScene.LogInfo('TOGLCPostProcessingEngine: RenderToTexture is ready');
  end;
end;

procedure TOGLCPostProcessingEngine.FreeTargetTexture;
begin
  FRenderToTexture.Free;
  FRenderToTexture := NIL;
end;

procedure TOGLCPostProcessingEngine.Draw;
begin
  FRenderToTexture.Unbind;
  glViewport(0, 0, FParentScene.Width, FParentScene.Height);

  FParentScene.TexturedTriangleRendererWithFX.Prepare(ptTriangleStrip,
                                  FRenderToTexture.RenderedTexture, 0,
                                  FParentScene.ProjectionMatrix, 1.0, FX_BLEND_NORMAL);
  FParentScene.TexturedTriangleRendererWithFX.PushFrame(0, 0, 0, 0);
  FParentScene.TexturedTriangleRendererWithFX.Batch_Flush;
end;

constructor TOGLCPostProcessingEngine.Create(aParentScene: TOGLCScene);
begin
  FParentScene := aParentScene;
  Parameters.InitDefault;
  FCurrentFXBitMask := 0;
  FComputedTint.InitFromTint(tmReplaceColor, BGRA(0,0,0,0));

  FUnderWaterAmplitude := 0.01;
  FUnderWaterDensity := 25;
  FUnderWaterTimeMultiplicator := 1.0;
  FBlurAmount := 4;
  FPixelizeAmount := 0.5;
end;

destructor TOGLCPostProcessingEngine.Destroy;
begin
  FreeTargetTexture;
  inherited Destroy;
end;

procedure TOGLCPostProcessingEngine.ApplyFX(aParams: POGLCPostProcessingParameters);
var newBitMask: GLuint;
    paramChange: boolean;
    ct: TColorF;
    procedure CopyAndSendParamsToRenderer;
    begin
      FCurrentFXBitMask := newBitMask;
      FComputedTint.InitFromTint(aParams^.TintMode, aParams^.Tint);
      FUnderWaterAmplitude := aParams^.FUnderWaterAmplitude;
      FUnderWaterTimeMultiplicator := aParams^.FUnderWaterTimeMultiplicator;
      FUnderWaterDensity := aParams^.FUnderWaterDensity;
      FBlurAmount := aParams^.FBlurAmount;
      FPixelizeAmount := aParams^.FPixelizeAmount;
      FShockWaveCenterU := aParams^.FShockWaveCenterU;
      FShockWaveCenterV := aParams^.FShockWaveCenterV;
      FShockWaveTimeMultiplicator := aParams^.FShockWaveTimeMultiplicator;
      FShockWaveColorF.CopyFrom(aParams^.FShockWaveColorF);

      with FParentScene.TexturedTriangleRendererWithFX do begin
        SetFXBitMask(FCurrentFXBitMask);
        SetUnderWaterParams(Self.FUnderWaterAmplitude, Self.FUnderWaterDensity, Self.FUnderWaterTimeMultiplicator);
        SetBlurParams(Self.FBlurAmount);
        SetPixelizeParams(Self.FPixelizeAmount);
        SetTintValue(Self.FComputedTint);
        SetShockWaveParams(Self.FShockWaveCenterU, Self.FShockWaveCenterV, Self.FShockWaveTimeMultiplicator, Self.FShockWaveColorF);
      end;
    end;

begin
  if (FRenderToTexture = NIL) or not FRenderToTexture.Ready then exit;

  newBitMask := aParams^.FXBitMask;

  with aParams^ do begin
    paramChange := (FXBitMask and fxbmPixelize = fxbmPixelize) and
                   (FPixelizeAmount <> Self.FPixelizeAmount)
                   or
                   (FXBitMask and fxbmUnderWater = fxbmUnderWater) and
                   ((FUnderWaterAmplitude <> Self.FUnderWaterAmplitude) or
                    (FUnderWaterDensity <> Self.FUnderWaterDensity) or
                    (FUnderWaterTimeMultiplicator <> Self.FUnderWaterTimeMultiplicator))
                   or
                   ((FXBitMask and fxbmBlur = fxbmBlur) and
                    (FBlurAmount <> Self.FBlurAmount));
    if FXBitMask and fxbmTint = fxbmTint then begin
      with aParams^ do ct.InitFromTint(TintMode, Tint);
      paramChange := paramChange or
                     not FComputedTint.EqualTo(ct);
    end;
  end;

  if FIsActive then begin
    // we stops/draw/restart the rendering into a texture only if the FX or parameters changes.
    if (FCurrentFXBitMask <> newBitMask) or
       ((FCurrentFXBitMask = newBitMask) and paramChange) then begin
      // flush an eventual remains in the batch renderer process
      FParentScene.FTexturedMVTriangleRenderer.Batch_Flush;
      // stops the rendering into a texture and draw it
      Draw;
      // restart the rendering into a texture
      CopyAndSendParamsToRenderer;
      FRenderToTexture.Bind;
      FRenderToTexture.ClearBuffers;
      FIsActive := True;
    end;
  end else begin
    if newBitMask <> 0 then begin
      // flush an eventual remains in the batch renderer process
      FParentScene.FTexturedMVTriangleRenderer.Batch_Flush;
      // start the rendering into a texture
      CopyAndSendParamsToRenderer;
      FRenderToTexture.Bind;
      FRenderToTexture.ClearBuffers;
      FIsActive := True;
    end;
  end;
end;

procedure TOGLCPostProcessingEngine.Flush;
begin
  if not FIsActive then exit;
  // flush an eventual remains in the batch renderer process
  FParentScene.FTexturedMVTriangleRenderer.Batch_Flush;
  // stop the rendering into a texture
  // and draw the obtained texture with the appropriate effects
  Draw;
  FCurrentFXBitMask := 0;
  FIsActive := False;
end;

{$endif oglcIMPLEMENTATION}

