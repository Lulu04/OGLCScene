unit u_procedural_starnest;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmapTypes,
  OGLCScene, glcorearb;

type

{ TStarNestParameter }

TStarNestParameter = record
private
  FVolSteps, // 20
  FIterations: integer; // 17
  FStepSize, // 0.1
  FZoom, //   2.800     0.800
  FTile, //   0.850
  FBrightness, // 0.0015
  FDarkmatter, // 0.800  0.300
  FDistFading, // 0.730    730
  FSaturation: single; // 0.850
  FChanged: boolean;
  function GetChanged: boolean;
  procedure SetBrightness(AValue: single);
  procedure SetDarkmatter(AValue: single);
  procedure SetDistFading(AValue: single);
  procedure SetIterations(AValue: integer);
  procedure SetSaturation(AValue: single);
  procedure SetStepSize(AValue: single);
  procedure SetTile(AValue: single);
  procedure SetVolSteps(AValue: integer);
  procedure SetZoom(AValue: single);
public
  procedure InitDefault;
  // Load the planet's parameters from a string constant
  procedure LoadParamsFromString(const s: string);
  // Pack the planet's parameters into a string
  function SaveParamsToString: string;
  function CheckString(const s: string): boolean;

  property VolSteps: integer read FVolSteps write SetVolSteps;
  property Iterations: integer read FIterations write SetIterations;
  property StepSize: single read FStepsize write SetStepSize;
  property Zoom: single read FZoom write SetZoom;
  property Tile: single read FTile write SetTile;
  property Brightness: single read FBrightness write SetBrightness;
  property Darkmatter: single read FDarkmatter write SetDarkmatter;
  property DistFading: single read FDistFading write SetDistFading;
  property Saturation: single read FSaturation write SetSaturation;

  property Changed: boolean read GetChanged;
end;
PStarNestParameter= ^TStarNestParameter;


{ TStarNestRenderer }

TStarNestRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyuv>)
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
    '  layout(location = 0) out vec4 FragColor;'#10+
    '  in vec2 TexCoords;'#10+
    '  uniform vec2 uIterations_VolSteps;'#10+
    '  uniform vec4 uSizexy_Opacity_Time;'#10+
    '  uniform vec3 stepsize_zoom_tile;'#10+
    '  uniform vec4 brightness_darkmatter_distfading_saturation;'#10+
    '  uniform vec2 uScrollingAngle_OpacityTheshold;'#10+

    // Star Nest by Pablo Roman Andrioli
    // This fragment shader is under the MIT License.
    // Source : https://www.shadertoy.com/view/XlfGRj
    '#define formuparam 0.53'#10+
     'void main()'#10+
    '{'#10+
    //get coords and direction
    'vec2 uv = gl_FragCoord.xy / uSizexy_Opacity_Time.xy - .5;'#10+
    'uv.y *= uSizexy_Opacity_Time.y / uSizexy_Opacity_Time.x;'#10+

    // rotation
    'mat2 rot1 = mat2(cos(uScrollingAngle_OpacityTheshold.x),sin(uScrollingAngle_OpacityTheshold.x),-sin(uScrollingAngle_OpacityTheshold.x),cos(uScrollingAngle_OpacityTheshold.x));'#10+
    'uv *=rot1;'#10+

    'vec3 direc = vec3(vec2(uv*stepsize_zoom_tile.y),1.);'#10+
    'float time = uSizexy_Opacity_Time.w+.25;'#10+

    'vec3 from = vec3(time+1.0, 0.5, 0.5);'#10+

    //volumetric rendering
    'float s = 0.1;'#10+
    'float fade = 1.0;'#10+
    'vec3 v = vec3(0.0);'#10+
    'for (int r=0; r<int(uIterations_VolSteps.y); r++) {'#10+      // volsteps
    '    vec3 p = from + s*direc*.5;'#10+
    '    p = abs(vec3(stepsize_zoom_tile.z)-mod(p,vec3(stepsize_zoom_tile.z*2.)));'#10+ // tiling fold
    //'    p=abs(p)/max(dot(p,p),0.0016)-formuparam;'#10+ // the magic formula
    '    float pa = 0.0;'#10+
    '    float a = 0.0;'#10+
    '    for (int i=0; i<int(uIterations_VolSteps.x); i++) {'#10+     //iterations
    '        p = abs(p)/dot(p,p) - formuparam;'#10+ // the magic formula
    '        a += abs(length(p) - pa);'#10+ // absolute sum of average change
    '        pa = length(p);'#10+
    '    }'#10+
    '    float dm = max(0., brightness_darkmatter_distfading_saturation.y-a*a*.001);'#10+ //dark matter
    '    a *= a*a;'#10+ // add contrast
    '    if (r > 6) fade *= 1.-dm;'#10+ // dark matter, don't render near
    '    //v+=vec3(dm,dm*.5,0.);'#10+
    '    v += fade;'#10+
    '    v += vec3(s,s*s,s*s*s*s)*a*brightness_darkmatter_distfading_saturation.x*fade;'#10+ // coloring based on distance
    '    fade *= brightness_darkmatter_distfading_saturation.z;'#10+ // distance fading
    '    s += stepsize_zoom_tile.x;'#10+
    '}'#10+
    'v = mix(vec3(length(v)), v, brightness_darkmatter_distfading_saturation.w);'#10+ //color adjust
    // compute alpha value: stars becomes transparent to the bottom
    'vec3 col = v*.02;'#10+
    //'if ( all( lessThan(col, vec3(0.08)) )) discard;'#10+
    'float alpha = 1.0;'#10+
    'if (TexCoords.y > uScrollingAngle_OpacityTheshold.y)'#10+
    '   alpha = 1.0-smoothstep(uScrollingAngle_OpacityTheshold.y, 1.0, TexCoords.y);'#10+
    'FragColor = vec4(col, alpha*uSizexy_Opacity_Time.z);'#10+
   '}';
private
  FLocMVP,
  FLocIterations_VolSteps,
  FLocSizexy_Opacity_Time,
  FLocstepsize_zoom_tile,
  FLocbrightness_darkmatter_distfading_saturation,
  FLocScrollingAngle_OpacityTheshold: glint;

  FMVP: TOGLCMatrix;
  FOpacity, FTimeAccu, FWidth, FHeight, FScrollingAngle, FOpacityThreshold: single;
protected
  procedure InitShaderCodeAndCallBack; override;
private
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  Params: TStarNestParameter;
  procedure Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix;
                    const aOpacity: single; aBlendMode: byte;
                    aTimeAccu: single; aWidth, aHeight, aScrollingAngleDeg, aOpacityThreshold: single);
  procedure PushQuad(aFlipIndex: integer);
end;


{ TStarNest }

TStarNest = class(TSimpleSurfaceWithEffect)
private
  FWidth, FHeight: integer;
  FRenderer: TStarNestRenderer;
  FTimeAccu: single;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
public
  ScrollingSpeed: TFParam;
  ScrollingAngle: TBoundedFParam;
  OpacityThreshold: single; // control the size of transparent part on the top. default is 0.4
  constructor Create(aParentScene: TOGLCScene; aRenderer: TStarNestRenderer);
  destructor Destroy; override;

  procedure SetSize(aWidth, aHeight: integer);

  // load the stars parameters from a string constant
  procedure LoadParamsFromString(const s: string);
  // Pack the stars parameters into a string
  function SaveParamsToString: string;

  function Params: PStarNestParameter;
end;

implementation

{ TStarNestParameter }

procedure TStarNestParameter.SetBrightness(AValue: single);
begin
  if FBrightness = AValue then Exit;
  FBrightness := AValue;
  FChanged := True;
end;

function TStarNestParameter.GetChanged: boolean;
begin
  Result := FChanged;
  FChanged := False;
end;

procedure TStarNestParameter.SetDarkmatter(AValue: single);
begin
  if FDarkmatter=AValue then Exit;
  FDarkmatter:=AValue;
  FChanged := True;
end;

procedure TStarNestParameter.SetDistFading(AValue: single);
begin
  if FDistFading=AValue then Exit;
  FDistFading:=AValue;
  FChanged := True;
end;

procedure TStarNestParameter.SetIterations(AValue: integer);
begin
  if FIterations=AValue then Exit;
  FIterations:=AValue;
  FChanged := True;
end;

procedure TStarNestParameter.SetSaturation(AValue: single);
begin
  if FSaturation=AValue then Exit;
  FSaturation:=AValue;
  FChanged := True;
end;

procedure TStarNestParameter.SetStepSize(AValue: single);
begin
  if FStepsize=AValue then Exit;
  FStepsize:=AValue;
  FChanged := True;
end;

procedure TStarNestParameter.SetZoom(AValue: single);
begin
  if FZoom=AValue then Exit;
  FZoom:=AValue;
  FChanged := True;
end;

procedure TStarNestParameter.SetTile(AValue: single);
begin
  if FTile=AValue then Exit;
  FTile:=AValue;
  FChanged := True;
end;

procedure TStarNestParameter.SetVolSteps(AValue: integer);
begin
  if FVolSteps=AValue then Exit;
  FVolSteps:=AValue;
  FChanged := True;
end;

procedure TStarNestParameter.InitDefault;
begin
  FVolSteps := 20;
  FIterations := 17;
  FStepSize := 0.1;
  FZoom := 0.800;
  FTile := 0.850;
  FBrightness := 0.0015;
  FDarkmatter := 0.300;
  FDistFading := 0.730;
  FSaturation := 0.850;
  FChanged := True;
end;

procedure TStarNestParameter.LoadParamsFromString(const s: string);
var prop: TProperties;
begin
  InitDefault;
  prop.Split(s, '|');
  prop.IntegerValueOf('volsteps', FVolSteps, FVolSteps);
  prop.IntegerValueOf('iterations', FIterations, FIterations);
  prop.SingleValueOf('stepsize', FStepSize, FStepSize);
  prop.SingleValueOf('zoom', FZoom, FZoom);
  prop.SingleValueOf('tile', FTile, FTile);
  prop.SingleValueOf('brightness', FBrightness, FBrightness);
  prop.SingleValueOf('darkmatter', FDarkmatter, FDarkmatter);
  prop.SingleValueOf('distfading', FDistFading, FDistFading);
  prop.SingleValueOf('saturation', FSaturation, FSaturation);
end;

function TStarNestParameter.SaveParamsToString: string;
var prop: TProperties;
begin
  prop.Init('|');
  prop.Add('volsteps', FVolSteps);
  prop.Add('iterations', FIterations);
  prop.Add('stepsize', FStepSize);
  prop.Add('zoom', FZoom);
  prop.Add('tile', FTile);
  prop.Add('brightness', FBrightness);
  prop.Add('darkmatter', FDarkmatter);
  prop.Add('distfading', FDistFading);
  prop.Add('saturation', FSaturation);
  Result := prop.PackedProperty;
end;

function TStarNestParameter.CheckString(const s: string): boolean;
var p: TProperties;
begin
  p.Split(s, '|');
  Result := p.CheckProperties(['volsteps', 'iterations', 'stepsize', 'zoom', 'tile', 'brightness',
       'darkmatter', 'distfading', 'saturation']);
end;

{ TStarNestRenderer }

procedure TStarNestRenderer.InitShaderCodeAndCallBack;
begin
  FShaderName := 'TStarNestRenderer';
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
  Params.InitDefault;
end;

procedure TStarNestRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(0);
end;

procedure TStarNestRenderer.GetUniformLocation;
begin
  with Shader do begin
    FLocMVP := GetUniform('uMVP');
    FLocIterations_VolSteps := GetUniform('uIterations_VolSteps');
    FLocSizexy_Opacity_Time := GetUniform('uSizexy_Opacity_Time');
    FLocstepsize_zoom_tile := GetUniform('stepsize_zoom_tile');
    FLocbrightness_darkmatter_distfading_saturation := GetUniform('brightness_darkmatter_distfading_saturation');
    FLocScrollingAngle_OpacityTheshold := GetUniform('uScrollingAngle_OpacityTheshold');
  end;
end;

procedure TStarNestRenderer.SetUniformValuesAndTexture;
begin
    glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
    if Params.Changed then begin
      glUniform2f(FLocIterations_VolSteps, single(Params.Iterations), single(Params.VolSteps));
      glUniform3f(FLocstepsize_zoom_tile, Params.StepSize, Params.Zoom, Params.Tile);
      glUniform4f(FLocbrightness_darkmatter_distfading_saturation, Params.Brightness, Params.Darkmatter, Params.DistFading, Params.Saturation);
    end;
    glUniform4f(FLocSizexy_Opacity_Time, FWidth, FHeight, FOpacity, FTimeAccu);
    glUniform2f(FLocScrollingAngle_OpacityTheshold, FScrollingAngle*Deg2Rad, FOpacityThreshold);
    //  glGetError();

  ParentScene.TexMan.UnbindTexture;
end;

procedure TStarNestRenderer.Prepare(aTriangleType: TTriangleType;
  const aMVP: TOGLCMatrix; const aOpacity: single; aBlendMode: byte;
  aTimeAccu: single; aWidth, aHeight, aScrollingAngleDeg,
  aOpacityThreshold: single);
var forceFlush: Boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP) or
                (FOpacity <> aOpacity) or
                (aTimeAccu <> FTimeAccu) or
                (aWidth <> FWidth) or
                (aHeight <> FHeight) or
                (FScrollingAngle <> aScrollingAngleDeg) or
                (FOpacityThreshold <> aOpacityThreshold);
  Batch_CheckIfNeedFlush(Self, aTriangleType, NIL, 0, aBlendMode, forceFlush);
  FMVP.CopyFrom(aMVP);
  FOpacity := aOpacity;
  FTimeAccu := aTimeAccu;
  FWidth := aWidth;
  FHeight := aHeight;
  FScrollingAngle := aScrollingAngleDeg;
  FOpacityThreshold := aOpacityThreshold;
end;

procedure TStarNestRenderer.PushQuad(aFlipIndex: integer);
var area, texCoords: TQuadCoor;
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

{ TStarNest }

function TStarNest.GetWidth: integer;
begin
  Result := FWidth;
end;

function TStarNest.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TStarNest.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  ScrollingSpeed.OnElapse(aElapsedTime);
  ScrollingAngle.OnElapse(aElapsedTime);

  FTimeAccu := FTimeAccu + aElapsedTime * ScrollingSpeed.Value;
end;

procedure TStarNest.DoDraw;
begin
  FRenderer.Prepare(ptTriangleStrip, FParentScene.MVPMatrix, FComputedOpacity, FBlendMode,
                    FTimeAccu, Width, Height, ScrollingAngle.Value, OpacityThreshold);
  FRenderer.PushQuad(FlipToIndex);
end;

constructor TStarNest.Create(aParentScene: TOGLCScene; aRenderer: TStarNestRenderer);
begin
  inherited Create;
  FParentScene := aParentScene;
  FRenderer := aRenderer;

  ScrollingSpeed := TFParam.Create;
  ScrollingAngle := CreateBoundedFParam(0, 359.99, True);

  FWidth := 100;
  FHeight := 100;
  OpacityThreshold := 0.4;
end;

destructor TStarNest.Destroy;
begin
  ScrollingSpeed.Free;
  ScrollingAngle.Free;
  inherited Destroy;
end;

procedure TStarNest.SetSize(aWidth, aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
end;

procedure TStarNest.LoadParamsFromString(const s: string);
begin
  FRenderer.Params.LoadParamsFromString(s);
end;

function TStarNest.SaveParamsToString: string;
begin
  Result := FRenderer.Params.SaveParamsToString;
end;

function TStarNest.Params: PStarNestParameter;
begin
  Result := @FRenderer.Params;
end;

end.

