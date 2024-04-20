{$ifdef oglcINTERFACE}
type

{ TOGLCGlowRenderer }

TOGLCGlowRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyattenuationcolor>)
private const
GLOW_VERTEX_SHADER_CORE =
  '#version 330 core'#10+
  '  layout(location = 0) in vec3 aVertexCoor_Attenuation;'#10+
  '  layout(location = 1) in vec4 aVertexColor;'#10+
  '  uniform mat4 uMVP;'#10+
  '  out vec4 VertexColor;'#10+
  '  flat out float Attenuation;'#10+
  'void main() {'#10+
  '  gl_Position = uMVP*vec4(aVertexCoor_Attenuation.xy, 0.0, 1.0);'#10+
  '  VertexColor = aVertexColor;'#10+
  '  Attenuation = aVertexCoor_Attenuation.z;'#10+
  '}';
GLOW_FRAGMENT_SHADER_CORE =
 '#version 330 core'#10+
 '  out vec4 FragColor;'#10+
 '  in vec4 VertexColor;'#10+
 '  flat in float Attenuation;'#10+
 '  uniform float uOpacity;'#10+
 'void main() {'#10+
 '  float a = sqrt(pow(VertexColor.a, Attenuation))*uOpacity;'#10+  //modified easeInCirc  https://easings.net/
 '  if (a == 0.0) discard;'#10+
   'FragColor = vec4(VertexColor.rgb, a);'#10+
 '}';
private
  FLocMVP,
  FLocOpacity: GLint;
  FMVP: TOGLCMatrix;
  FOpacity: single;
  procedure InitShaderCodeAndCallBack; override;
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix; const aOpacity: single; aBlendMode: byte);
end;


type
{ TOGLCGlow }

TOGLCGlow = class(TSimpleSurfaceWithEffect)
private const
  MIN_ATTENUATION_VALUE = 0.1;
  MAX_ATTENUATION_VALUE = 100.0;
  DEFAULT_ATTENUATION_VALUE = 0.97;
private
  FFillVertex: array of Txyattenuationcolor;
  FRadius,
  FPreviousPowerValue: single;
  FPreviousTintValue: TBGRAPixel;
  function GetAttenuationValue: single;
  procedure PrepareVertexArray;
  procedure UpdateVertexArrayVariables;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
public
  // range [0..1]
  Power: TBoundedFParam;
  // use Tint to change color at run time
  Constructor Create(aParentScene: TOGLCScene; aRadius: single; aColor: TBGRAPixel; aBlendMode: byte=FX_BLEND_ADD);
  Destructor Destroy; override;
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
  // xy attenuation = 3 float
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Txyattenuationcolor), NIL);
  glEnableVertexAttribArray(0);
  // color = 4 float
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(Txyattenuationcolor), PChar(sizeof(single)*3));
  glEnableVertexAttribArray(1);
end;

procedure TOGLCGlowRenderer.GetUniformLocation;
begin
  with FShader do
  begin
    FLocMVP := GetUniform('uMVP');
    FLocOpacity := GetUniform('uOpacity');
  end;
end;

procedure TOGLCGlowRenderer.SetUniformValuesAndTexture;
begin
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
  glUniform1f(FLocOpacity, FOpacity);
  FParentScene.TexMan.UnbindTexture;
end;

procedure TOGLCGlowRenderer.Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix; const aOpacity: single; aBlendMode: byte);
var forceFlush: Boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP) or
                (FOpacity <> aOpacity);
  Batch_CheckIfNeedFlush(Self, aTriangleType, NIL, 0, aBlendMode, forceFlush);
  FMVP.CopyFrom(aMVP);
  FOpacity := aOpacity;
end;

{ TOGLCGlow }

function TOGLCGlow.GetAttenuationValue: single;
begin
  Result := (1-Power.Value)*(MAX_ATTENUATION_VALUE-MIN_ATTENUATION_VALUE)+MIN_ATTENUATION_VALUE;
end;

procedure TOGLCGlow.PrepareVertexArray;
var p: Pxyattenuationcolor;
  i: Integer;
  col, colTransparent: TColorF;
  atte: single;
  path: ArrayOfTPointF;
begin
  path := ComputeEllipse(FRadius, FRadius, FRadius, FRadius, 0.05); // we don't need high precision

  col.InitFromBGRA(Tint.Value);
  colTransparent.CopyFrom(col);
  colTransparent.a := 0;
  atte := GetAttenuationValue;
  FFillVertex := NIL;
  SetLength(FFillVertex, Length(path)+2); // TRIANGLE_FAN: center + points + first point
  p := @FFillVertex[0];
  // center
  p^.x := FRadius;
  p^.y := FRadius;
  p^.attenuation := atte;
  p^.c.CopyFrom(col);
  // points
  for i:=0 to High(path) do
  begin
    inc(p);
    p^.x := path[i].x;
    p^.y := path[i].y;
    p^.attenuation := atte;
    p^.c.CopyFrom(colTransparent);
  end;
  // first point to close the shape
  inc(p);
  p^.x := path[0].x;
  p^.y := path[0].y;
  p^.attenuation := atte;
  p^.c.CopyFrom(colTransparent);
end;

procedure TOGLCGlow.UpdateVertexArrayVariables;
var col: TColorF;
  atte: single;
  i: integer;
  p: Pxyattenuationcolor;
begin
  col.InitFromBGRA(Tint.Value);
  atte := GetAttenuationValue;
  // center
  p := @FFillVertex[0];
  p^.attenuation := atte;
  p^.c.CopyFrom(col);
  inc(p);

  col.a := 0;
  for i:=1 to High(FFillVertex) do
  begin
    p^.attenuation := atte;
    p^.c.CopyFrom(col);
    inc(p);
  end;
end;

function TOGLCGlow.GetWidth: integer;
begin
  Result := Round(FRadius* 2);
end;

function TOGLCGlow.GetHeight: integer;
begin
  Result := GetWidth;
end;

procedure TOGLCGlow.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if Freeze then exit;

  Power.OnElapse(aElapsedTime);
  if (FPreviousPowerValue <> Power.Value) or (FPreviousTintValue <> Tint.Value) then
    UpdateVertexArrayVariables;
  FPreviousPowerValue := Power.Value;
  FPreviousTintValue := Tint.Value;
end;

procedure TOGLCGlow.DoDraw;
var p: Pxyattenuationcolor;
begin
  // render with triangle fan
  FParentScene.GlowRenderer.Prepare(ptTriangleFan, FParentScene.MVPMatrix, FComputedOpacity, FBlendMode);
  p := FParentScene.GlowRenderer.QueryVertex(Length(FFillVertex));
  Move(FFillVertex[0], p^, SizeOf(Txyattenuationcolor)*Length(FFillVertex));
end;

constructor TOGLCGlow.Create(aParentScene: TOGLCScene; aRadius: single; aColor: TBGRAPixel; aBlendMode: byte);
begin
  inherited Create;
  FParentScene := aParentScene;
  FRadius := aRadius;
  Power := CreateBoundedFParam(0.0, 1.0);
  Power.Value := DEFAULT_ATTENUATION_VALUE;
  FPreviousPowerValue := Power.Value;
  Tint.Value := aColor;
  FPreviousTintValue := aColor;
  FBlendMode := aBlendMode;

  PrepareVertexArray;
end;

destructor TOGLCGlow.Destroy;
begin
  FreeAndNil(Power);
  inherited Destroy;
end;

{$endif oglcIMPLEMENTATION}

