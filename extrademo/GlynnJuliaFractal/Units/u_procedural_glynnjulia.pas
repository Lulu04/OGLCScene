unit u_procedural_glynnjulia;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, glcorearb, u_common;

type
// the color palette is build in an image with size (2048, 2) and converted in texture

TGlynnJuliaColorMode = (gjcmRGB,
                        gjcmHSLRedConstant,
                        gjcmHSLBlueConstant,
                        gjcmHSLGreenConstant);

{ TGlynnJuliaParam }

TGlynnJuliaParam = record
private
  FCoeffR: Double;
  FCoeffG: Double;
  FCoeffB: Double;
  FmyLightness: Double;
  FIteration: integer;
  FMinHueOffset: integer;
  FColorMode: TGlynnJuliaColorMode;
  FAlpha: byte;
  FAlphaGlowSoftness: integer;
  FTexture: PTexture;
  function HSLToRGB(Hue, Saturation, Lightness: Double): TColor;
  procedure BuildPaletteIntoTexture;
  procedure SetCoeffR(AValue: Double);
  procedure SetCoeffG(AValue: Double);
  procedure SetCoeffB(AValue: Double);
  procedure SetmyLightness(AValue: Double);
  procedure SetAlpha(AValue: byte);
  procedure SetAlphaGlowSoftness(AValue: integer);
  procedure SetColorMode(AValue: TGlynnJuliaColorMode);
  procedure SetIteration(AValue: integer);
  procedure SetMinHueOffset(AValue: integer);
public
  procedure InitDefault;
  // 10..1000
  property Iteration: integer read FIteration write SetIteration;
  // 10..200
  property MinHueOffset: integer read FMinHueOffset write SetMinHueOffset;
  property ColorMode: TGlynnJuliaColorMode read FColorMode write SetColorMode;
  // 0..255
  property Alpha: byte read FAlpha write SetAlpha;
  property CoeffR: Double read FCoeffR write SetCoeffR;
  property CoeffG: Double read FCoeffG write SetCoeffG;
  property CoeffB: Double read FCoeffB write SetCoeffB;
  property myLightness: Double read FmyLightness write SetmyLightness;
  // 1..5
  property AlphaGlowSoftness: integer read FAlphaGlowSoftness write SetAlphaGlowSoftness;
end;


{ TGlynnJuliaRenderer }

TGlynnJuliaRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyuv>)
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
  'layout(location = 0) out vec4 FragColor;'#10+
  'in vec2 TexCoords;'#10+
  'uniform vec4 uSize_Time_Iteration;'#10+   // x=width, y=height, z=time, w=iterations
  'uniform sampler2D uTexUnit;'#10+
  '#define xMin 0.06'#10+
  '#define xMax 0.42'#10+
  '#define yMin 0.36'#10+
  '#define yMax 0.63'#10+
  'void main()'#10+
  '{'#10+
  '  vec2 UV = TexCoords;'#10+
  '  float zx = xMin + UV.x * (xMax - xMin);'#10+
  '  float zy = yMin + UV.y * (yMax - yMin);'#10+
  '  float i = uSize_Time_Iteration.w;'#10+
  '  while (zx*zx + zy*zy < 4.0 && i > 1.0)'#10+
  '  {'#10+
  '    float r = sqrt(zx*zx + zy*zy);'#10+
  '    float theta = atan(zy, zx);'#10+
  '    float r15 = pow(r, 1.5);'#10+
  '    float tmp = r15 * cos(1.5*theta) - 0.2;'#10+
  '    zy = r15 * sin(1.5*theta);'#10+
  '    zx = tmp;'#10+
  '    i = i - 1.0;'#10+
  '  }'#10+
  ''#10+
  '  // Linear normalized iteration for smooth palette sampling'#10+
  '  float t = i / uSize_Time_Iteration.w;'#10+
  ''#10+
  '  // Optional subtle cyclic offset to reduce banding (linear)'#10+
  '  // float cyclic = 0.003 * fract(UV.x * 37.0 + UV.y * 23.0);'#10+
  '  // t = clamp(t + cyclic, 0.0, 1.0);'#10+
  ''#10+
  '  vec4 col = texture(uTexUnit, vec2(clamp(t, 0.0, 1.0), 0.0));'#10+
  '  FragColor = col;'#10+
  '}';

private
  FLocMVP,
  FLocSize_Time_Iteration,
  FLocTextureUnit: glint;

  FMVP: TOGLCMatrix;
  FOpacity, FTimeAccu, FWidth, FHeight: single;
protected
  procedure InitShaderCodeAndCallBack; override;
private
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  Params: TGlynnJuliaParam;
  destructor Destroy; override;
  procedure Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix;
                    const aOpacity: single; aBlendMode: byte;
                    aTimeAccu: single; aWidth, aHeight: single);
  procedure PushQuad(aFlipIndex: integer);
end;

{ TGlynnJulia }

TGlynnJulia = class(TSimpleSurfaceWithEffect)
private
  FWidth, FHeight: integer;
  FCustomRenderer: TGlynnJuliaRenderer;
  FTimeAccu: single;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
public
  constructor Create(aParentScene: TOGLCScene; aCustomRenderer: TGlynnJuliaRenderer);

  procedure SetSize(aWidth, aHeight: integer);
end;

implementation
uses Math;

{ TGlynnJuliaParam }

procedure TGlynnJuliaParam.BuildPaletteIntoTexture;
var
  ima: TBGRABitmap;
  c: TColor = clBlack;
  hue: Double;
  i: Integer;
  R, G, B: Integer;
  maxIter: Integer;
  sat: Double;
begin
  if FTexture <> NIL then
    FScene.TexMan.Delete(FTexture);

  maxIter := Iteration;

  // Create 1D palette texture
  ima := TBGRABitmap.Create(Min(maxIter + 1, 8192), 1);

  for i := 0 to maxIter do
  begin
    case ColorMode of

      gjcmRGB:
        begin
          R := (i * 7 + FMinHueOffset) mod 256;
          G := (i * 5 + FMinHueOffset*2) mod 256;
          B := (i * 3 + FMinHueOffset*3) mod 256;
          c := RGBToColor(R, G, B);
        end;

      gjcmHSLRedConstant:
        begin
          hue := (0 + FMinHueOffset + (i / maxIter) * 360) mod 360;
          sat := (hue * FCoeffR);
          c := HSLToRGB(hue, sat, FmyLightness);
        end;

      gjcmHSLGreenConstant:
        begin
          hue := (120 + FMinHueOffset + (i / maxIter) * 360) mod 360;
          sat := (hue * FCoeffG);
          c := HSLToRGB(hue, sat, FmyLightness);
        end;

      gjcmHSLBlueConstant:
        begin
          hue := (240 + FMinHueOffset + (i / maxIter) * 360) mod 360;
          sat := (hue * FCoeffB);
          c := HSLToRGB(hue, sat, FmyLightness);
        end;

    end;

    if i < ima.Width then
      ima.DrawPixel(i, 0, ColorToBGRA(c, Alpha));
  end;

  FTexture := FScene.TexMan.Add(ima, ima.Width, ima.Height);
  ima.Free;
end;

procedure TGlynnJuliaParam.SetAlpha(AValue: byte);
begin
  if FAlpha = AValue then Exit;
  FAlpha := AValue;
  BuildPaletteIntoTexture;
end;

procedure TGlynnJuliaParam.SetCoeffR(AValue: Double);
begin
  AValue := EnsureRange(AValue, 0.1, 0.9);
  if FCoeffR = AValue then Exit;
  FCoeffR := AValue;
  BuildPaletteIntoTexture;
end;

procedure TGlynnJuliaParam.SetCoeffG(AValue: Double);
begin
  AValue := EnsureRange(AValue, 0.1, 0.9);
  if FCoeffG = AValue then Exit;
  FCoeffG := AValue;
  BuildPaletteIntoTexture;
end;

procedure TGlynnJuliaParam.SetCoeffB(AValue: Double);
begin
  AValue := EnsureRange(AValue, 0.1, 0.9);
  if FCoeffB = AValue then Exit;
  FCoeffB := AValue;
  BuildPaletteIntoTexture;
end;

procedure TGlynnJuliaParam.SetmyLightness(AValue: Double);
begin
  AValue := EnsureRange(AValue, 0.1, 0.9);
  if FmyLightness = AValue then Exit;
  FmyLightness := AValue;
  BuildPaletteIntoTexture;
end;

procedure TGlynnJuliaParam.SetAlphaGlowSoftness(AValue: integer);
begin
  AValue := EnsureRange(AValue, 1, 5);
  if FAlphaGlowSoftness = AValue then Exit;
  FAlphaGlowSoftness := AValue;
  BuildPaletteIntoTexture;
end;

procedure TGlynnJuliaParam.SetColorMode(AValue: TGlynnJuliaColorMode);
begin
  if FColorMode = AValue then Exit;
  FColorMode := AValue;
  BuildPaletteIntoTexture;
end;

procedure TGlynnJuliaParam.SetIteration(AValue: integer);
begin
  AValue := EnsureRange(AValue, 10, 2047);
  if FIteration = AValue then Exit;
  FIteration := AValue;
  BuildPaletteIntoTexture;
end;

procedure TGlynnJuliaParam.SetMinHueOffset(AValue: integer);
begin
  AValue := EnsureRange(AValue, 10, 200);
  if FMinHueOffset = AValue then Exit;
  FMinHueOffset := AValue;
  BuildPaletteIntoTexture;
end;

function TGlynnJuliaParam.HSLToRGB(Hue, Saturation, Lightness: Double): TColor;
var
  C, X, M: Double;
  R, G, B: Byte;
begin
  if Saturation = 0 then
  begin
    R := Round(Lightness * 255);
    G := R;
    B := R;
  end
  else
  begin
    if Lightness < 0.5 then
      C := (1 + Saturation) * Lightness
    else
      C := Lightness + Saturation - Lightness * Saturation;

    M := 2 * Lightness - C;
    X := Abs(Frac(Hue / 60) - 1);

    if Hue < 60 then
    begin
      R := Round((C + M) * 255) and $ff;
      G := Round((X * C + M) * 255) and $ff;
      B := Round((M) * 255) and $ff;
    end
    else if Hue < 120 then
    begin
      R := Round((X * C + M) * 255) and $ff;
      G := Round((C + M) * 255) and $ff;
      B := Round((M) * 255) and $ff;
    end
    else if Hue < 180 then
    begin
      R := Round((M) * 255) and $ff;
      G := Round((C + M) * 255) and $ff;
      B := Round((X * C + M) * 255) and $ff;
    end
    else if Hue < 240 then
    begin
      R := Round((M) * 255) and $ff;
      G := Round((X * C + M) * 255) and $ff;
      B := Round((C + M) * 255) and $ff;
    end
    else if Hue < 300 then
    begin
      R := Round((X * C + M) * 255) and $ff;
      G := Round((M) * 255) and $ff;
      B := Round((C + M) * 255) and $ff;
    end
    else
    begin
      R := Round((C + M) * 255) and $ff;
      G := Round((M) * 255) and $ff;
      B := Round((X * C + M) * 255) and $ff;
    end;
  end;

  Result := RGBToColor(R, G, B);
end;

procedure TGlynnJuliaParam.InitDefault;
begin
  Iteration := 197;
  MinHueOffset := 132;
  ColorMode := gjcmRGB;
  Alpha := 255;
  AlphaGlowSoftness := 1;

  CoeffR := 0.6;
  CoeffG := 0.6;
  CoeffB := 0.6;
  myLightness := 0.52;

  BuildPaletteIntoTexture;
end;

{ TGlynnJuliaRenderer }

procedure TGlynnJuliaRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;

  Params.InitDefault;
end;

procedure TGlynnJuliaRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(0);
end;

procedure TGlynnJuliaRenderer.GetUniformLocation;
begin
  with Shader do begin
    FLocMVP := GetUniform('uMVP');
    FLocSize_Time_Iteration := GetUniform('uSize_Time_Iteration');
    FLocTextureUnit := GetUniform('uTexUnit');
  end;
end;

procedure TGlynnJuliaRenderer.SetUniformValuesAndTexture;
begin
  glUniform1i(FLocTextureUnit, Batch^.CurrentTextureUnit);
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
  glUniform4f(FLocSize_Time_Iteration, FWidth, FHeight, FTimeAccu, single(Params.Iteration));
//  glGetError();

  //ParentScene.TexMan.Bind(Params.FTexture);
  ParentScene.TexMan.Bind(Batch^.CurrentTexture, Batch^.CurrentTextureUnit);

end;

destructor TGlynnJuliaRenderer.Destroy;
begin
  FScene.TexMan.Delete(Params.FTexture);
  inherited Destroy;
end;

procedure TGlynnJuliaRenderer.Prepare(aTriangleType: TTriangleType;
  const aMVP: TOGLCMatrix; const aOpacity: single; aBlendMode: byte;
  aTimeAccu: single; aWidth, aHeight: single);
var forceFlush: Boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP) or
                (FOpacity <> aOpacity) or
                (aTimeAccu <> FTimeAccu) or
                (aWidth <> FWidth) or
                (aHeight <> FHeight);
  Batch_CheckIfNeedFlush(Self, aTriangleType, NIL, 0, aBlendMode, forceFlush);
  FMVP.CopyFrom(aMVP);
  FOpacity := aOpacity;
  FTimeAccu := aTimeAccu;
  FWidth := aWidth;
  FHeight := aHeight;
end;

procedure TGlynnJuliaRenderer.PushQuad(aFlipIndex: integer);
var area, texCoords: TQuadF;
  tci: PQuadCornerIndexes;
    p: Pxyuv;
    pIndex: PVertexIndex;
    currentIndex: TVertexIndex;
begin
  area[cBL].x := 0;
  area[cBL].y := FHeight;
  area[cTL].x := 0;
  area[cTL].y := 0;
  area[cBR].x := FWidth;
  area[cBR].y := FHeight;
  area[cTR].x := FWidth;
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
    u := texCoords[ tci^[0] ].x;
    v := texCoords[ tci^[0] ].y;
    x := area[cBL].x;
    y := area[cBL].y;
  end;
  with p[1] do begin
    u := texCoords[ tci^[1] ].x;
    v := texCoords[ tci^[1] ].y;
    x := area[cTL].x;
    y := area[cTL].y;
  end;
  with p[2] do begin
    u := texCoords[ tci^[2] ].x;
    v := texCoords[ tci^[2] ].y;
    x := area[cBR].x;
    y := area[cBR].y;
  end;
  with p[3] do begin
    u := texCoords[ tci^[3] ].x;
    v := texCoords[ tci^[3] ].y;
    x := area[cTR].x;
    y := area[cTR].y;
  end;
end;

{ TGlynnJulia }

function TGlynnJulia.GetWidth: integer;
begin
  Result := FWidth;
end;

function TGlynnJulia.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TGlynnJulia.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  FTimeAccu := FTimeAccu + aElapsedTime;
end;

procedure TGlynnJulia.DoDraw;
begin
  FCustomRenderer.Prepare(ptTriangleStrip, FParentScene.MVPMatrix, FComputedOpacity, FBlendMode,
                          FTimeAccu, Width, Height);
  FCustomRenderer.PushQuad(FlipToIndex);
end;

constructor TGlynnJulia.Create(aParentScene: TOGLCScene; aCustomRenderer: TGlynnJuliaRenderer);
begin
  inherited Create;
  FParentScene := aParentScene;
  FCustomRenderer := aCustomRenderer;

  FWidth := 100;
  FHeight := 100;
end;

procedure TGlynnJulia.SetSize(aWidth, aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
end;

end.
