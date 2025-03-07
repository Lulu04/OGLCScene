unit u_MySurfaceWithGradient;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmapTypes,
  OGLCScene, glcorearb;

type


{ TMySurfaceWithGradient }

TMySurfaceWithGradient = class(TOGLCBaseSurfaceWithProceduralShader)
private const
  VERTEX_SHADER =
    '#version 330 core'#10+
    '  layout(location = 0) in vec4 aVertexAndUVCoor;'#10+
    '  uniform mat4 uMVP;'#10+
    ' out vec2 UV;'#10+
    'void main()'#10+
    '{'#10+
    '  gl_Position = uMVP*vec4(aVertexAndUVCoor.xy, 0.0, 1.0);'#10+
    '  UV = aVertexAndUVCoor.zw;'#10+
    '}';

  FRAGMENT_SHADER =
    '#version 330 core'#10+
    '  layout(location = 0) out vec4 FragColor;'#10+
    '  in vec2 UV;'#10+
    'void main()'#10+
    '{'#10+
    //'  vec2 UV = gl_FragCoord.xy / uTexSize.xy;'#10+
    '  FragColor = vec4(UV.y, UV.x, 1.0-UV.x*UV.y, 1.0);'#10+
    '}';

private
  FLocMVP: glint;
  procedure DoWriteUniformValues;
public
  constructor Create(aParentScene: TOGLCScene;
                     aWidth, aHeight: integer;
                     aColorAttachmentIndex: integer=0);
end;

implementation

{ TMySurfaceWithGradient }

procedure TMySurfaceWithGradient.DoWriteUniformValues;
begin
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @ProjectionMatrix.Matrix[0,0]);
//  glGetError();
end;

constructor TMySurfaceWithGradient.Create(aParentScene: TOGLCScene; aWidth,
  aHeight: integer; aColorAttachmentIndex: integer);
begin
  inherited Create(aParentScene, aWidth, aHeight, VERTEX_SHADER, FRAGMENT_SHADER, aColorAttachmentIndex);

  if Ready then
    with Shader do begin
      Use;
      FLocMVP := GetUniform('uMVP');
      Release;
    end;

  OnWriteUniformValues := @DoWriteUniformValues;
end;

end.

