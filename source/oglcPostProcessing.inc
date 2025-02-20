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
   '  uniform int uBlurAmount = 3;'#10+         // 1, 2,  3
//   '  uniform int uBlurDivisor = 30;'#10+       // 4, 18, 30
   'void main()'#10+
   '{'#10+
   '  vec4 col = texture(uTexUnit, TexCoords);'#10+
   '  float a = col.a*uOpacity;'#10+
//   '  if (a == 0) discard;'#10+
'  if (a == 0) a=a+0.0001;'#10+
   '  int flags = uFXFlag;'#10+
   '  vec2 texCoords = TexCoords;'#10+
   // light blur
   ' if (flags >= 0x80){'#10+
   '  flags -= 0x80;'#10+
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
   '  a = col.a*uOpacity;'#10+
//   '  if (a == 0) discard;'#10+
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
//   '     col = col * (1-tintAlphaX2) + uTintColor * tintAlphaX2;'#10+  // replace color
   '     col = mix(col, uTintColor, tintAlphaX2);'#10+  // replace color
   '   }'#10+
   '  else if (uTintColor.a > 0)'#10+
   '     col = col + uTintColor*tintAlphaX2;'#10+     // mix color
   ' }'#10+

//   '  col.a = a;'#10+
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
  FLocBlurAmount: GLint;
  FMVP: TOGLCMatrix;
  FTintF: TColorF;
  FOpacity: single;
  FFXBitMask, FTexWidth, FTexHeight: GLuint;
  FUnderWaterAmplitude, FUnderWaterDensity, FUnderWaterTimeMultiplicator: single;
  FBlurAmount: GLint;
  procedure InitShaderCodeAndCallBack; override;
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; aTexture: PTexture; aTextureUnit: integer;
                    const aMVP: TOGLCMatrix; const aOpacity: single; aBlendMode: byte);

  procedure PushFrame(const aFrameIndex, aFlipIndex: integer; const aXPosOffset, aYPosOffset: single);

  procedure SetFXBitMask(const aFXBitMask: GLuint);
  procedure SetUnderWaterParams(aAmplitude, aDensity, aTimeMultiplicator: single);
  procedure SetBlurParams(aAmount: integer);
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
// this record groups a bunch of convenient procedure to ease the control of the post-processing effects
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
  procedure EnableFXOnLayers(aFXs: TOGLCPostProcessingFX; const aLayers: TIntegerArray);
  // Enable post-processing FX on a range of layers.
  procedure EnableFXOnLayerRange(aFXs: TOGLCPostProcessingFX; aFirstLayer, aLastLayer: integer);
  // Enable post-processing FX on all layers.
  procedure EnableFXOnAllLayers(aFXs: TOGLCPostProcessingFX);

  // Disable post-processing FX on one or several the layers.
  procedure DisableFXOnLayers(aFXs: TOGLCPostProcessingFX; const aLayers: TIntegerArray);
  // Disable post-processing FX on a range of layers.
  procedure DisableFXOnLayerRange(aFXs: TOGLCPostProcessingFX; aFirstLayer, aLastLayer: integer);
  // Disable post-processing FX on all layers.
  procedure DisableFXOnAllLayers(aFXs: TOGLCPostProcessingFX);

  // Disable all post-processing FX on all layers.
  procedure DisableAllFXOnAllLayers;

  // aAmplitude: the waves amplitude [1..100]
  // aDensity: the waves density [1..100]
  // aTimeMultiplicator mini is 0.1 (slow)  -->  1.0 normal speed(default)  --->  greater than 1.0 (fast)
  procedure SetUnderWaterParamsOnLayers(aAmplitude, aDensity, aTimeMultiplicator: single; const aLayers: TIntegerArray);
  // aAmplitude: the waves amplitude [1..100]
  // aDensity: the waves density [1..100]
  // aTimeMultiplicator mini is 0.1 (slow)  -->  1.0 normal speed(default)  --->  greater than 1.0 (fast)
  procedure SetUnderWaterParamsOnLayerRange(aAmplitude, aDensity, aTimeMultiplicator: single; aFirstLayer, aLastLayer: integer);
  // aAmplitude: the waves amplitude [1..100]
  // aDensity: the waves density [1..100]
  // aTimeMultiplicator mini is 0.1 (slow)  -->  1.0 normal speed(default)  --->  greater than 1.0 (fast)
  procedure SetUnderWaterParamsOnAllLayers(aAmplitude, aDensity, aTimeMultiplicator: single);

  // aAmount range is 1 soft   .....    4 full
  procedure SetBlurParamsOnLayers(aAmount: integer; const aLayers: TIntegerArray);
  // aAmount range is 1 soft   .....    4 full
  procedure SetBlurParamsOnLayerRange(aAmount: integer; aFirstLayer, aLastLayer: integer);
  // aAmount range is 1 soft   .....    4 full
  procedure SetBlurParamsOnAllLayers(aAmount: integer);


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

procedure TOGLCPostProcessingUtils.EnableFXOnLayers(aFXs: TOGLCPostProcessingFX; const aLayers: TIntegerArray);
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

procedure TOGLCPostProcessingUtils.DisableFXOnLayers(aFXs: TOGLCPostProcessingFX; const aLayers: TIntegerArray);
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
  aDensity, aTimeMultiplicator: single; const aLayers: TIntegerArray);
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

procedure TOGLCPostProcessingUtils.SetBlurParamsOnLayers(aAmount: integer; const aLayers: TIntegerArray);
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

{ TOGLCTexturedTriangleRendererWithFX }

procedure TOGLCTexturedTriangleRendererWithFX.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
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
  glUniform1f(FLocTime, GetTickCount64*0.001);
  glUniform1f(FLocUnderWaterAmplitude, FUnderWaterAmplitude);
  glUniform1f(FLocUnderWaterDensity, FUnderWaterDensity);
  glUniform1f(FLocUnderWaterSpeedCoeff, FUnderWaterTimeMultiplicator);
  glUniform1i(FLocBlurAmount, FBlurAmount);
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

procedure TOGLCTexturedTriangleRendererWithFX.SetFXBitMask(const aFXBitMask: GLuint);
begin
  FFXBitMask := aFXBitMask;
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

      with FParentScene.TexturedTriangleRendererWithFX do begin
        SetFXBitMask(FCurrentFXBitMask);
        SetUnderWaterParams(Self.FUnderWaterAmplitude, Self.FUnderWaterDensity, Self.FUnderWaterTimeMultiplicator);
        SetBlurParams(Self.FBlurAmount);
        SetTintValue(Self.FComputedTint);
      end;
    end;

begin
  if (FRenderToTexture = NIL) or not FRenderToTexture.Ready then exit;

  newBitMask := aParams^.FXBitMask;

  with aParams^ do begin
    paramChange := (FXBitMask and fxbmUnderWater = fxbmUnderWater) and
                   ((FUnderWaterAmplitude <> Self.FUnderWaterAmplitude) or
                    (FUnderWaterDensity <> Self.FUnderWaterDensity) or
                    (FUnderWaterTimeMultiplicator <> Self.FUnderWaterTimeMultiplicator));
    paramChange := paramChange or
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

