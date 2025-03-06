unit u_MyPostProcessingRenderer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmapTypes,
  OGLCScene, glcorearb;

type

{
  Post-processing consists to redirect the content of one or several layers to
  a target texture instead of the screen.
  This texture is then passed to a renderer capable to draw it
  on the screen with visual effects.

  A post-processing renderer contains a vertex and fragment shader, and VAO/VBO/indices buffers.

  The class TOGLCBaseRendererForPostProcessing contains the code to manage
  all the stuff about the target texture, the vertex shader, and the VAO/VBO/indices buffers.

  To write your own custom post-processing effect you only have to derivate
  a class from TOGLCBaseRendererForPostProcessing with:
     - a private string constant named FRAGMENT_SHADER with the code of your fragment shader.
       It must have the following lines:
             '  in vec2 TexCoords;'#10+
             '  uniform sampler2D uTexUnit;'#10+
             '  uniform vec2 uTexSize;'#10+
             '  uniform float uTime;'#10+
     - a procedure GetFragmentUniformLocation to get the location of your fragment shader uniforms
     - a procedure SetFragmentUniformValues to initialize the values of your fragment shader uniforms
     - override the procedure InitShaderCodeAndCallBack. It must have the following lines:
             FFragmentShaderCode := PChar(FRAGMENT_SHADER);
             FGetFragmentUniformLocation := @GetFragmentUniformLocation;
             FSetFragmentUniformValues := @SetFragmentUniformValues;

 See the example below.
}

{ TMyPostProcessingRenderer }

TMyPostProcessingRenderer = class(TOGLCBaseRendererForPostProcessing)
private const
  FRAGMENT_SHADER =
   '#version 330 core'#10+
   // here are the uniforms required and managed in TOGLCBaseRendererForPostProcessing
   '  in vec2 TexCoords;'#10+           // the texture coordinates (uv)
   '  uniform sampler2D uTexUnit;'#10+  // the texture to read
   '  uniform vec2 uTexSize;'#10+       // the texture size
   '  uniform float uTime;'#10+         // the time in seconds

   // here are the uniforms specific to TMyPostProcessingRenderer
   '  uniform vec4 uWaterColor = vec4(0.0, 0.0, 1.0, 0.8);'#10+
   '  uniform float uThreshold = 0.3;'#10+
   '  uniform float uTimeMultiplicator = 3.0;'#10+
   '  uniform vec2 uAmplitude = vec2(1.0, 1.0);'#10+    // 0.8 0.15
   '  out vec4 FragColor;'#10+

   'void main()'#10+
   '{'#10+
   '  float time = uTimeMultiplicator * uTime;'#10+
   '  float sepoffset = ((cos(TexCoords.x*uTexSize.x*0.05)+1.15) * (sin(time)*0.5+1.15))*0.001;'#10+
   '  if (TexCoords.y > uThreshold + sepoffset)'#10+
   '  {'#10+
        // no  effect
   '    FragColor = texture2D(uTexUnit, TexCoords);'#10+
   '  }'#10+
   '  else'#10+
   '  {'#10+
   '    vec2 uv = TexCoords;'#10+
        // revert uv.y for mirror effect
   '    uv.y = uThreshold + (uThreshold - uv.y);'#10+

   '    float quantity = (uThreshold - uv.y)/uThreshold;'#10+  //
   '    vec2 offset;'#10+
   '    offset.x = (1.0-quantity)*0.5 * 0.005*cos(time+200.0*uv.y);'#10+
   '    offset.y = quantity * 0.005*(1.0+cos(time+50.0*uv.y));'#10+

   '    vec4 color = texture(uTexUnit, uv+offset*uAmplitude);'#10+
   '    color.rgb = mix(color.rgb, uWaterColor.rgb, uWaterColor.a);'#10+
   '    FragColor = color;'#10+
   '  }'#10+
   '}';
private
  FLocWaterColor, FLocThreshold, FLocTimeMultiplicator, FLocAmplitude: GLint;
  procedure GetFragmentUniformLocation;
  procedure SetFragmentUniformValues;
private
  FWaterColorF: TColorF;
  FThreshold, FTimeMultiplicator, FAmplitudeX, FAmplitudeY: single;
  function getWaterColor: TBGRAPixel;
  procedure SetWaterColor(AValue: TBGRAPixel);
protected
  procedure InitShaderCodeAndCallBack; override;
public
  // aWaterColor is the color of the water. Alpha value is the amount to apply.
  // aYMirror is the scene y coord where the mirror effect starts.
  // aAmplitudeX, aAmplitudeY is the amplitude of the wave on both axis
  // aTimeMultiplicator allow to control the wave speed: ]0..1[=slower  1=normal   >1=faster
  procedure SetParams(const aWaterColor: TBGRAPixel;
                      aYMirror: single;
                      aAmplitudeX: single=1.0;
                      aAmplitudeY: single=1.0;
                      aTimeMultiplicator: single=3.0);
  property WaterColor: TBGRAPixel read getWaterColor write SetWaterColor;
end;

implementation
uses Math;

{ TMyPostProcessingRenderer }

procedure TMyPostProcessingRenderer.GetFragmentUniformLocation;
begin
  with Shader do begin
    FLocWaterColor := GetUniform('uWaterColor');
    FLocThreshold := GetUniform('uThreshold');
    FLocTimeMultiplicator := GetUniform('uTimeMultiplicator');
    FLocAmplitude := GetUniform('uAmplitude');
  end;
end;

procedure TMyPostProcessingRenderer.SetFragmentUniformValues;
begin
  glUniform4fv(FLocWaterColor, 1, @FWaterColorF);
  glUniform1f(FLocThreshold, FThreshold);
  glUniform1f(FLocTimeMultiplicator, FTimeMultiplicator);
  glUniform2f(FLocAmplitude, FAmplitudeX, FAmplitudeY);
end;

function TMyPostProcessingRenderer.getWaterColor: TBGRAPixel;
begin
  Result := FWaterColorF.ToBGRA;
end;

procedure TMyPostProcessingRenderer.SetWaterColor(AValue: TBGRAPixel);
begin
  FWaterColorF.InitFromBGRA(AValue);
end;

procedure TMyPostProcessingRenderer.InitShaderCodeAndCallBack;
begin
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FGetFragmentUniformLocation := @GetFragmentUniformLocation;
  FSetFragmentUniformValues := @SetFragmentUniformValues;

  // initialize default values
  FWaterColorF.InitFromBGRA(BGRA(0,75,100,90));
  FThreshold := 0.3;
  FTimeMultiplicator := 3.0;
  FAmplitudeX := 1.0;
  FAmplitudeY := 1.0;
end;

procedure TMyPostProcessingRenderer.SetParams(const aWaterColor: TBGRAPixel;
    aYMirror: single; aAmplitudeX: single; aAmplitudeY: single; aTimeMultiplicator: single);
begin
  FWaterColorF.InitFromBGRA(aWaterColor);
  FThreshold := 1.0 - (aYMirror/ParentScene.Height);

  if aAmplitudeX <= 0 then aAmplitudeX := 0.001;
  FAmplitudeX := aAmplitudeX;

  if aAmplitudeY <= 0 then aAmplitudeY := 0.001;
  FAmplitudeY := aAmplitudeY;

  FTimeMultiplicator := Max(aTimeMultiplicator, 0.0);
end;

end.

