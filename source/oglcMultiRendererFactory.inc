{$ifdef oglcINTERFACE}
const
  INITIAL_BUFFER_CAPACITY = 128;

type

TRendererBase = class;

TRendererBatchDescriptor = record
  CurrentRenderer: TRendererBase;
  CurrentTexture: PTexture;
  CurrentTextureUnit: integer;
  CurrentRendererBlendMode: byte;
  CurrentPrimitiveType: TPrimitiveType;
end;
PRendererBatchDescriptor = ^TRendererBatchDescriptor;

{ TRendererBase }

TRendererBase = class
private
  FParentScene: TOGLCScene;
  FBatch: PRendererBatchDescriptor;
  procedure Batch_CheckIfNeedFlush(aNewRenderer: TRendererBase; aPrimitiveType: TPrimitiveType;
                                   aNewTexture: PTexture; aTextureUnit: integer; aNewBlendMode: byte; aExtraTest: boolean);
  procedure Batch_Flush;
  procedure AddPrimitiveRestart; virtual; abstract;
public
  procedure Draw(aEmptyBufferWhenDone: boolean=True); virtual; abstract;
end;


{ TOGLCGenericPrimitiveRenderer }

generic TOGLCGenericPrimitiveRenderer<T> = class(TRendererBase)
private type PT = ^T;
private
  FShader: TOGLCShader;
  FError: GLenum;
private
  FUseIndicesBuffer: boolean;
  FVBO, FVAO, FVIO: GLuint;
  FAttribsArray: array of T;
  FIndexInAttribsArray: ptrint;
private
  FIndexesArray: TArrayOfVertexIndex;
  FIndexInIndexArray: ptrint;
  procedure SetIndexBufferCapacity(aCount: ptrint);
  procedure AddPrimitiveRestart; override;
  procedure AddPrimitiveRestartIfNeeded;
private
  procedure CreateID;
  procedure FreeID;
  procedure SetCapacity(aCount: ptrint);
  procedure ResetBufferState;
protected
  FVertexShaderCode,
  FFragmentShaderCode: PChar;
  FDefineVertexAttribPointer,
  FGetUniformLocation,
  FSetUniformValuesAndTexture: TProcOfObject;
  procedure InitShaderCodeAndCallBack; virtual; abstract;
public
  // aInitialBufferCapacity is the initial memory space to reserve for the vertex buffer
  // expressed in vertex count.
  constructor Create(aParentScene: TOGLCScene; aUseIndicesBuffer: boolean);
  destructor Destroy; override;

  // Reserve space in the vertex buffer for the specified number of vertices and return a pointer to it.
  function QueryVertex(aCount: ptrint): PT;

  // Reserve space in the index buffer for the specified number of indexes and return a pointer to it.
  function QueryIndex(aCount: ptrint): PVertexIndex;

  // If needed shift the value of the indices passed in parameter and push them to the renderer index buffer.
  procedure PushIndices(const aIndices: TArrayOfVertexIndex);

  // Reserve space for 4 indexes in the index buffer and fill them as usual for triangle strip pattern.
  procedure QueryAndFillIndexForQuadTriangleStrip;

  // Draw all primitives in the buffer and empty it.
  procedure Draw(aResetBufferStateWhenDone: boolean=True); override;

  property VertexCount: ptrint read FIndexInAttribsArray;
  property ErrorStatus: GLenum read FError;
end;


{
                                         ...d
                                      ...   ---C
1) overlaps                        ...   ---   ...e
 AB is full color               ...   ---   ...
 abcd opacity=0              ...   ---   ...
                          ...   ---   ...
a-....................b...   ---   ...
|                     |   ---   ...
A---------------------B---   ...
|                     |   ...
d.....................c...

                                         ...C
                                      ...   ---d
2) inside                          ...   ---   ...e
                                ...   ---   ...
                             ...   ---   ...
                          ...   ---   ...
A---------------------B...   ---   ...
|                     |   ---   ...
a.....................b---   ...
|                     |   ...
d.....................c...



A B C are 3 consecutive points in a predefined path.
To draw antialiased lines, we draw 2 rectangles (or 6 triangles)
a b c d coordinates are computed from the normal of the segment AB and the desired line width.
we calculate the normals of the segment AB to find the coordinates of abcd
we calculate the normals of the segment BC to find the coordinates of bdec
}


{ TOGLCSmoothLineRenderer }

TOGLCSmoothLineRenderer = class(specialize TOGLCGenericPrimitiveRenderer<TLineVertexAttributes>)
private const
  VERTEX_SHADER =
   '#version 330 core'#10+
   '  layout(location = 0) in vec4 aVertexCoor_Distance_Threshold;'#10+
   '  layout(location = 1) in vec4 aVertexColor;'#10+
   '  uniform mat4 uMVP;'#10+
   '  out vec4 VertexColor;'#10+
   '  out float Distance;'#10+
   '  flat out float Threshold;'#10+
   'void main()'#10+
   '{'#10+
   '  gl_Position = uMVP*vec4(aVertexCoor_Distance_Threshold.xy, 0.0, 1.0);'#10+
   '  VertexColor = aVertexColor;'#10+
   '  Distance = aVertexCoor_Distance_Threshold.z;'#10+
   '  Threshold = aVertexCoor_Distance_Threshold.w;'#10+
   '}';

  FRAGMENT_SHADER =
   '#version 330 core'#10+
   '  out vec4 FragColor;'#10+
   '  uniform vec4 uTintColor;'#10+
   '  uniform float uOpacity;'#10+
   '  in vec4 VertexColor;'#10+
   '  in float Distance;'#10+
   '  flat in float Threshold;'#10+
   'void main()'#10+
   '{'#10+
     'float coefDir = 1/(Threshold-1);'#10+
     'float a;'#10+

     //'a = mix(VertexColor.a, VertexColor.a*(coefDir*Distance - coefDir), step(Distance, Threshold));'#10+
     'if (Distance < Threshold) {'#10+
     '   a = VertexColor.a;'#10+
     '} else {'#10+
     '   a = VertexColor.a*(coefDir*Distance - coefDir);'#10+ // equation of the downward slope is F(x) = a*x-a
     '}'#10+                                                  // with a = 1 / (Threshold-1)
     'vec4 col = vec4(VertexColor.rgb, a);'#10+
     // Tint
     'a = col.a*uOpacity;'#10+
     'float tintAlphaX2 = uTintColor.a*2;'#10+
     'if (uTintColor.a > 0.5)'#10+
     ' {'#10+
     '   tintAlphaX2 = tintAlphaX2-1;'#10+
     //'   col = col * (1-tintAlphaX2) + uTintColor * tintAlphaX2;'#10+  // replace mode
     '     col = mix(col, uTintColor, tintAlphaX2);'#10+ // replace mode
     ' }'#10+
     'else if (uTintColor.a > 0)'#10+
     '   col = col + uTintColor*tintAlphaX2;'#10+     // mix mode

     'col.a = a;'#10+
     'FragColor = col;'#10+
   '}';
private
  FLocMVP,
  FLocTintColor,
  FLocOpacity: GLint;
  FMVP: TOGLCMatrix;
  FTintF: TColorF;
  FOpacity: single;
  procedure InitShaderCodeAndCallBack; override;
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix; const aOpacity: single;
                    const aComputedTint: TColorF; aBlendMode: byte);

  // Don't call method Prepare, it's done inside with ptTriangles.
  // This method push the border vertices and indexes in the renderer buffers then draw the border.
  // At the end, the renderer buffers are emptied.
  procedure DrawBorder(const aBorder: TOGLCBorder; aOpacity: single; aComputedTint: TColorF);

  // Don't call method Prepare, it is done inside with ptTriangleStrip.
  procedure PushPath(const aPath: TOGLCPath; aLinePosition: TOGLCLinePosition; const aColor: TBGRAPixel;
                     const w, aOpacity: single; aForceDraw: boolean);

  // Don't call method Prepare, it is done inside with ptTriangles.
  procedure PushPath2(const aPath: TOGLCPath; aLinePosition: TOGLCLinePosition; const aColor: TBGRAPixel;
                     const w, aOpacity: single; aForceDraw: boolean);

  // Don't call method Prepare, it's done inside with ptTriangles.
  // Push several paths at once in the renderer buffers.
  procedure PushPaths(const aPaths: TArrayOfOGLCPath; aLinePosition: TOGLCLinePosition;
                      const aColor: TBGRAPixel; const w: single; const aComputedTint: TColorF;
                      const aOpacity: single; aForceDraw: boolean);
end;


{ TOGLCFastLineRenderer }

TOGLCFastLineRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txycolor>)
private const
  VERTEX_SHADER =
   '#version 330 core'#10+
   '  layout(location = 0) in vec2 aVertexCoor;'#10+
   '  layout(location = 1) in vec4 aVertexColor;'#10+
   '  uniform mat4 uMVP;'#10+
   '  out vec4 VertexColor;'#10+
   'void main()'#10+
   '{'#10+
   '  gl_Position = uMVP*vec4(aVertexCoor.xy, 0.0, 1.0);'#10+
   '  VertexColor = aVertexColor;'#10+
   '}';

  FRAGMENT_SHADER =
   '#version 330 core'#10+
   '  in vec4 VertexColor;'#10+
   '  out vec4 FragColor;'#10+
   'void main()'#10+
   '{'#10+
   '  FragColor = VertexColor;'#10+
   '}';
private
  FLocMVP: GLuint;
  FMVP: TOGLCMatrix;
  procedure InitShaderCodeAndCallBack; override;
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
private
  procedure InternalPushQuad(const aQuad: TQuadF; const aColor: TColorF);
public
  procedure Prepare(const aMVP: TOGLCMatrix; aBlendMode: byte);

  procedure PushLine(pt1, pt2: TPointF; const aColor: TBGRAPixel; w: single; aLinePosition: TOGLCLinePosition);
  procedure PushPath(aPath: TOGLCPath; const aColor: TBGRAPixel; w: single; aLinePosition: TOGLCLinePosition);
end;




{ TOGLCThreeColorTriangleRenderer }

TOGLCThreeColorTriangleRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txycolor>)
private const
  VERTEX_SHADER =
   '#version 330 core'#10+
   '  layout(location = 0) in vec2 aVertexCoor;'#10+
   '  layout(location = 1) in vec4 aVertexColor;'#10+
   '  uniform mat4 uMVP;'#10+
   '  out vec4 VertexColor;'#10+
   'void main()'#10+
   '{'#10+
   '  gl_Position = uMVP*vec4(aVertexCoor.xy, 0.0, 1.0);'#10+
   '  VertexColor = aVertexColor;'#10+
   '}';

  FRAGMENT_SHADER =
   '#version 330 core'#10+
   '  out vec4 FragColor;'#10+
   '  in vec4 VertexColor;'#10+
   '  uniform vec4 uTintColor;'#10+
   '  uniform float uOpacity;'#10+
   'void main()'#10+
   '{'#10+
     'vec4 col = VertexColor;'#10+
     // Tint
     'float a = col.a*uOpacity;'#10+
   '  float tintAlphaX2 = uTintColor.a*2;'#10+
   '  if (uTintColor.a >= 0.5)'#10+
   '   {'#10+
   '     tintAlphaX2 = tintAlphaX2-1;'#10+
   //'     col = col * (1-tintAlphaX2) + uTintColor * tintAlphaX2;'#10+  // replace color
   '     col = mix(col, uTintColor, tintAlphaX2);'#10+ // replace mode
   '   }'#10+
   '  else if (uTintColor.a > 0)'#10+
   '     col = col + uTintColor*tintAlphaX2;'#10+     // mix color

//   '  if (a == 0.0) discard;'#10+
   '  col.a = a;'#10+
   '  FragColor = col;'#10+
   '}';
private
  FLocMVP,
  FLocTintColor,
  FLocOpacity: GLint;
  FMVP: TOGLCMatrix;
  FTintF: TColorF;
  FOpacity: single;
  procedure InitShaderCodeAndCallBack; override;
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix; const aOpacity: single;
                    const aComputedTint: TColorF; aBlendMode: byte);

  procedure PushRectangle(const aX, aY, aWidth, aHeight: single; const aFillColor: TBGRAPixel);
  procedure PushGradient(aGradient: PGradientDescriptor);

  // Don't call Prepare, it is done inside the method with ptTriangleFan and current scene MVP.
  procedure PushFill(aFill: POGLCFill; const aOpacity: single; const aComputedTint: TColorF; aForceDraw: boolean);
end;



{ TOGLCThreeColorMVTriangleRenderer }

TOGLCThreeColorMVTriangleRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyopacitycolortintmatrix>)
private const
  VERTEX_SHADER =
   '#version 330 core'#10+
   '  layout(location = 0) in vec3 aVertexCoorAndOpacity;'#10+
   '  layout(location = 1) in vec4 aVertexColor;'#10+
   '  layout(location = 2) in vec4 aTint;'#10+
   '  layout(location = 3) in mat4 aModelViewMatrix;'#10+
   '  uniform mat4 uProjectionMatrix;'#10+
   '  out vec4 VertexColor;'#10+
   '  flat out float Opacity;'#10+
   '  flat out vec4 Tint;'#10+
   'void main()'#10+
   '{'#10+
   '  gl_Position = uProjectionMatrix*aModelViewMatrix*vec4(aVertexCoorAndOpacity.xy, 0.0, 1.0);'#10+
   '  VertexColor = aVertexColor;'#10+
   '  Opacity = aVertexCoorAndOpacity.z;'#10+
   '  Tint = aTint;'#10+
   '}';

  FRAGMENT_SHADER =
   '#version 330 core'#10+
   '  out vec4 FragColor;'#10+
   '  in vec4 VertexColor;'#10+
   '  flat in float Opacity;'#10+
   '  flat in vec4 Tint;'#10+
   'void main()'#10+
   '{'#10+
   '  vec4 col = VertexColor;'#10+
   // Tint and opacity
   '  float a = col.a*Opacity;'#10+
   '  float tintAlphaX2 = Tint.a*2;'#10+
   '  if (Tint.a >= 0.5)'#10+
   '   {'#10+
   '     tintAlphaX2 = tintAlphaX2-1;'#10+
   //'     col = col * (1-tintAlphaX2) + Tint * tintAlphaX2;'#10+  // replace mode
   '     col = mix(col, Tint, tintAlphaX2);'#10+ // replace mode
   '   }'#10+
   '  else if (Tint.a > 0)'#10+
   '     col = col + Tint*tintAlphaX2;'#10+     // mix mode

   //'  col = col * (1-Tint.a) + Tint * Tint.a;'#10+
   '  col.a = a;'#10+
//   '  if (a == 0.0) discard;'#10+
   '  FragColor = col;'#10+
   '}' ;
private
  FLocProjectionMatrix: GLint;
  FProjectionMatrix: TOGLCMatrix;
  procedure InitShaderCodeAndCallBack; override;
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; const aProjectionMatrix: TOGLCMatrix; aBlendMode: byte);

  procedure PushQuad(const aModelArea: TQuadCoor; const aColors: TQuadColorF; aFlipIndex: integer;
                     aModelViewMatrix: POGLCMatrix44; const aOpacity: single; const aComputedTint: TColorF);
end;




{ TOGLCTexturedTriangleRenderer }

TOGLCTexturedTriangleRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyuv>)
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
   'void main()'#10+
   '{'#10+
   '  vec4 col = texture(uTexUnit, TexCoords);'#10+
   // Tint
   '  float a = col.a*uOpacity;'#10+
   '  if (a == 0) discard;'#10+
   '  float tintAlphaX2 = uTintColor.a*2;'#10+
   '  if (uTintColor.a >= 0.5)'#10+
   '   {'#10+
   '     tintAlphaX2 = tintAlphaX2-1;'#10+
   //'     col = col * (1-tintAlphaX2) + uTintColor * tintAlphaX2;'#10+  // replace mode
   '     col = mix(col, uTintColor, tintAlphaX2);'#10+ // replace mode
   '   }'#10+
   '  else if (uTintColor.a > 0)'#10+
   '     col = col + uTintColor*tintAlphaX2;'#10+     // mix mode
   '  col.a = a;'#10+
   '  FragColor = col;'#10+
   '}' ;
private
  FLocMVP,
  FLocTextureUnit,
  FLocTintColor,
  FLocOpacity: GLint;
  FMVP: TOGLCMatrix;
  FTintF: TColorF;
  FOpacity: single;
  procedure InitShaderCodeAndCallBack; override;
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; aTexture: PTexture; aTextureUnit: integer;
                    const aMVP: TOGLCMatrix; const aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);

  procedure PushFrame(const aFrameIndex, aFlipIndex: integer; const aXPosOffset, aYPosOffset: single);
  procedure PushFrameWithCornerOffset(const aFrameIndex, aFlipIndex: integer; const aXPosOffset, aYPosOffset: single;
                                      aCornerOffset: TQuadParamF);
  procedure PushFrameClipped(const aFrameIndex, aFlipIndex: integer; const aXPosOffset, aYPosOffset: single;
                             const aClipRect: TRectF);
  procedure PushQuad(const aTexCoord: TQuadCoor; aFlipIndex: integer; const aRect: TRectF);
end;




{ TOGLCTexturedMVTriangleRenderer }

TOGLCTexturedMVTriangleRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyuvtintopacitymatrix>)
private const
  VERTEX_SHADER =
   '#version 330 core'#10+
   '  layout(location = 0) in vec4 aVertexAndTextureCoor;'#10+
   '  layout(location = 1) in vec4 aVertexTint;'#10+
   '  layout(location = 2) in float aOpacity;'#10+
   '  layout(location = 3) in mat4 aModelViewMatrix;'#10+
   '  uniform mat4 uProjectionMatrix;'#10+
   '  out vec2 TexCoords;'#10+
   '  out vec4 VertexTint;'#10+
   '  flat out float Opacity;'#10+
   'void main()'#10+
   '{'#10+
   '  gl_Position = uProjectionMatrix*aModelViewMatrix*vec4(aVertexAndTextureCoor.xy, 0.0, 1.0);'#10+
   '  TexCoords = aVertexAndTextureCoor.zw;'#10+
   '  VertexTint = aVertexTint;'#10+
   '  Opacity = aOpacity;'#10+
   '}';

  FRAGMENT_SHADER =
   '#version 330 core'#10+
   '  out vec4 FragColor;'#10+
   '  in vec2 TexCoords;'#10+
   '  in vec4 VertexTint;'#10+
   '  uniform sampler2D uTexUnit;'#10+
   '  flat in float Opacity;'#10+
   'void main()'#10+
   '{'#10+
   '  vec4 col = texture(uTexUnit, TexCoords);'#10+
   // Tint and opacity
   '  float a = col.a*Opacity;'#10+
   '  if (a == 0) discard;'#10+
   '  float tintAlphaX2 = VertexTint.a*2;'#10+
   '  if (VertexTint.a > 0.5)'#10+
   '   {'#10+
   '     tintAlphaX2 = tintAlphaX2-1;'#10+
   //'     col = col * (1-tintAlphaX2) + VertexTint * tintAlphaX2;'#10+  // replace mode
   '     col = mix(col, VertexTint, tintAlphaX2);'#10+ // replace mode
   '   }'#10+
   '  else if (VertexTint.a > 0)'#10+
   '     col = col + VertexTint*tintAlphaX2;'#10+     // mix mode

   '  col.a = a;'#10+
   '  FragColor = col;'#10+
   '}' ;
private
  FLocProjectionMatrix,
  FLocTextureUnit: GLint;
  procedure InitShaderCodeAndCallBack; override;
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  // procedure Draw use parent scene MVP matrix
  procedure Prepare(aTriangleType: TTriangleType; aTexture: PTexture; aTextureUnit: integer; aBlendMode: byte);


  procedure PushQuad(const aFrameIndex, aFlipIndex: integer; const aModelViewMatrix: POGLCMatrix44;
                     const aXPosOffset, aYPosOffset, aOpacity: single; const aComputedTint: TColorF); overload;

  // Set aModelViewMatrix to NIL to use the parent scene model view matrix
  procedure PushQuad2(const aModelArea, aTexCoor: TQuadCoor; aFlipIndex: integer; aModelViewMatrix: POGLCMatrix44;
                     const aOpacity: single; const aComputedTint: TColorF); overload;

  // Set aModelViewMatrix to NIL to use the parent scene model view matrix
  procedure PushQuad3(const aModelArea: TRect; const aTexCoor: TQuadCoor; aFlipIndex: integer;
                      aModelViewMatrix: POGLCMatrix44; const aOpacity: single; const aComputedTint: TColorF); overload;
end;


{ TOGLCNoFilterTextureMVTriangleRenderer }

TOGLCNoFilterTextureMVTriangleRenderer = class(TOGLCTexturedMVTriangleRenderer)
private
  procedure InitShaderCodeAndCallBack; override;
  procedure SetUniformValuesAndTextureCustomized;
public
  procedure Draw(aResetBufferStateWhenDone: boolean=True); override;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TRendererBase }

procedure TRendererBase.Batch_CheckIfNeedFlush(aNewRenderer: TRendererBase; aPrimitiveType: TPrimitiveType;
                   aNewTexture: PTexture; aTextureUnit: integer; aNewBlendMode: byte; aExtraTest: boolean);
var needFlush: boolean;
begin
  needFlush := True;
  if FBatch^.CurrentRenderer <> NIL then
    if aExtraTest then Batch_Flush
    else begin
      needFlush := (FBatch^.CurrentRenderer <> aNewRenderer) or
                   (FBatch^.CurrentRendererBlendMode <> aNewBlendMode) or
                   (FBatch^.CurrentPrimitiveType <> aPrimitiveType) or
                   (FBatch^.CurrentTextureUnit <> aTextureUnit);
      if aNewTexture = NIL then needFlush := needFlush or (FBatch^.CurrentTexture <> NIL)
      else if FBatch^.CurrentTexture = NIL then needFlush := needFlush or (aNewTexture <> NIL)
      else needFlush := needFlush or (aNewTexture^.ID <> FBatch^.CurrentTexture^.ID);

      if needFlush then Batch_Flush;
    end;

  // insert a primitive restart if needed
  if not aExtraTest and not needFlush and
    ((aPrimitiveType = ptTriangleStrip) or (aPrimitiveType = ptTriangleFan)) then AddPrimitiveRestart;

  FBatch^.CurrentRenderer := aNewRenderer;
  FBatch^.CurrentPrimitiveType := aPrimitiveType;
  FBatch^.CurrentRendererBlendMode := aNewBlendMode;
  FBatch^.CurrentTexture := aNewTexture;
  FBatch^.CurrentTextureUnit := aTextureUnit;
end;

procedure TRendererBase.Batch_Flush;
begin
  if FBatch^.CurrentRenderer <> NIL then begin
    FParentScene.BlendMode := FBatch^.CurrentRendererBlendMode;
    FBatch^.CurrentRenderer.Draw(True);
    FBatch^.CurrentRenderer := NIL;
  end;
end;

{ TOGLCGenericPrimitiveRenderer }

procedure TOGLCGenericPrimitiveRenderer.CreateID;
var err: GLenum;
begin
  glGenVertexArrays(1, @FVAO);
  glGenBuffers(1, @FVBO);
  if FUseIndicesBuffer then glGenBuffers(1, @FVIO);

  err := glGetError();
  if (err <> GL_NO_ERROR) and (FError = GL_NO_ERROR) then begin
    FError := err;
    FParentScene.LogError(ClassName+'.CreateID: GL ERROR $'+IntToHex(err, 4)+' '+GLErrorToString(err));
  end;
end;

procedure TOGLCGenericPrimitiveRenderer.FreeID;
begin
  glDeleteBuffers(1, @FVBO);
  if FUseIndicesBuffer then glDeleteBuffers(1, @FVIO);
  glDeleteVertexArrays(1, @FVAO);
end;

procedure TOGLCGenericPrimitiveRenderer.SetIndexBufferCapacity(aCount: ptrint);
begin
  if not FUseIndicesBuffer then exit;
  if Length(FIndexesArray) = aCount then exit;

  SetLength(FIndexesArray, aCount);

  glBindVertexArray(FVAO);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FVIO);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(TVertexIndex)*aCount, nil, GL_DYNAMIC_DRAW);
  FError := glGetError();
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
end;

procedure TOGLCGenericPrimitiveRenderer.AddPrimitiveRestart;
var p: PVertexIndex;
begin
  p := QueryIndex(1);
  p^ := PRIMITIVE_INDEX_RESTART_VALUE;
end;

procedure TOGLCGenericPrimitiveRenderer.AddPrimitiveRestartIfNeeded;
begin
  if (FIndexInIndexArray > 0) and
     (FBatch^.CurrentPrimitiveType in [ptTriangleStrip, ptTriangleFan]) then AddPrimitiveRestart;
end;

procedure TOGLCGenericPrimitiveRenderer.SetCapacity(aCount: ptrint);
var err: GLenum;
begin
  if Length(FAttribsArray) = aCount then exit;
  SetLength(FAttribsArray, aCount);

  glBindVertexArray(FVAO);
  // sets the size of the VBO
  glBindBuffer(GL_ARRAY_BUFFER, FVBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(T)*aCount, nil, GL_DYNAMIC_DRAW);

  FDefineVertexAttribPointer();
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glBindVertexArray(0);

  err := glGetError();
  if (err <> GL_NO_ERROR) and (FError = GL_NO_ERROR) then begin
    FError := err;
    FParentScene.LogError(ClassName+'.SetCapacity: GL ERROR $'+IntToHex(err, 4)+' '+GLErrorToString(err));
  end else FParentScene.LogInfo(ClassName+': increase vertex array capacity to '+aCount.ToString+
              ' ('+(sizeof(T)*aCount).ToString+' bytes)');
end;

procedure TOGLCGenericPrimitiveRenderer.ResetBufferState;
begin
  FBatch^.CurrentPrimitiveType := ptNone;
  FIndexInAttribsArray := 0;
  FIndexInIndexArray := 0;
end;

constructor TOGLCGenericPrimitiveRenderer.Create(aParentScene: TOGLCScene; aUseIndicesBuffer: boolean);
begin
  InitShaderCodeAndCallBack;
  FParentScene := aParentScene;
  FUseIndicesBuffer := aUseIndicesBuffer;
  FBatch := @FParentScene.FRendererBatch;
  FShader.Compile(FParentScene, FVertexShaderCode, FFragmentShaderCode);
  FGetUniformLocation();
  CreateID;
  SetCapacity(INITIAL_BUFFER_CAPACITY);
  if aUseIndicesBuffer then SetIndexBufferCapacity(INITIAL_BUFFER_CAPACITY);
  ResetBufferState;
end;

destructor TOGLCGenericPrimitiveRenderer.Destroy;
begin
  FreeID;
  FShader.FreeID;
  FParentScene := NIL;
  inherited Destroy;
end;

function TOGLCGenericPrimitiveRenderer.QueryVertex(aCount: ptrint): PT;
var idx, newCapacity: PtrInt;
begin
  if aCount = 0 then begin
    Result := @FAttribsArray[FIndexInAttribsArray];
    exit;
  end;

  idx := FIndexInAttribsArray;
  inc(FIndexInAttribsArray, aCount);
  if FIndexInAttribsArray >= Length(FAttribsArray) then
  begin
    newCapacity := Length(FAttribsArray);
    repeat
      newCapacity := newCapacity*2;
    until FIndexInAttribsArray < newCapacity;
    SetCapacity(newCapacity);
  end;
  Result := @FAttribsArray[idx];
end;

function TOGLCGenericPrimitiveRenderer.QueryIndex(aCount: ptrint): PVertexIndex;
var idx, newCapacity: PtrInt;
begin
  if aCount = 0 then begin
    Result := @FIndexesArray[FIndexInIndexArray];
    exit;
  end;

  idx := FIndexInIndexArray;
  inc(FIndexInIndexArray, aCount);
  if FIndexInIndexArray >= Length(FIndexesArray) then
  begin
    newCapacity := Length(FIndexesArray);
    repeat
      newCapacity := newCapacity*2;
    until FIndexInIndexArray < newCapacity;
    SetIndexBufferCapacity(newCapacity);
  end;
  Result := @FIndexesArray[idx];
end;

procedure TOGLCGenericPrimitiveRenderer.PushIndices(const aIndices: TArrayOfVertexIndex);
var i: integer;
  pIndex: PVertexIndex;
  currentIndex: TVertexIndex;
begin
  if (Length(aIndices) = 0) then exit;
  currentIndex := FIndexInAttribsArray;
  pIndex := QueryIndex(Length(aIndices));
  for i:=0 to High(aIndices) do
    pIndex[i] := aIndices[i] + currentIndex;
end;

procedure TOGLCGenericPrimitiveRenderer.QueryAndFillIndexForQuadTriangleStrip;
var currentIndex: PtrInt;
    pIndex: PVertexIndex;
begin
  currentIndex := FIndexInAttribsArray;
  pIndex := QueryIndex(4);
  pIndex[0] := currentIndex;
  pIndex[1] := currentIndex+1;
  pIndex[2] := currentIndex+2;
  pIndex[3] := currentIndex+3;
end;

procedure TOGLCGenericPrimitiveRenderer.Draw(aResetBufferStateWhenDone: boolean=True);
var err: GLenum;
begin
  if (FIndexInAttribsArray > 0) and (FBatch^.CurrentPrimitiveType <> ptNone) then
  begin
    // update buffer
    glBindBuffer(GL_ARRAY_BUFFER, FVBO);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(T)*FIndexInAttribsArray, @FAttribsArray[0]);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    FShader.Use;
    FSetUniformValuesAndTexture();

    glBindVertexArray(FVAO);

    if not FUseIndicesBuffer then
      glDrawArrays(GLenum(FBatch^.CurrentPrimitiveType), 0, FIndexInAttribsArray)
    else begin
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FVIO);
      glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(TVertexIndex)*FIndexInIndexArray, @FIndexesArray[0], GL_DYNAMIC_DRAW);
      glDrawElements(GLenum(FBatch^.CurrentPrimitiveType){mode}, FIndexInIndexArray{count}, OPENGL_TYPE_USED_FOR_INDICE{type}, PChar(0)); // element array buffer offset
    end;
    glBindVertexArray(0);

    err := glGetError();
    if (err <> GL_NO_ERROR) and (FError = GL_NO_ERROR) then begin
      FError := err;
      FParentScene.LogError(ClassName+'.Draw(): GL ERROR $'+IntToHex(err, 4)+' '+GLErrorToString(err));
      FParentScene.LogMess('vertex count '+(FIndexInAttribsArray+1).ToString+
                           ' index count '+(FIndexInIndexArray+1).ToString, 1);
      FParentScene.LogMess('FBatch^.CurrentPrimitiveType='+integer(FBatch^.CurrentPrimitiveType).ToString, 1);
    end;
  end;

  if aResetBufferStateWhenDone then ResetBufferState;
end;

{ TOGLCSmoothLineRenderer }

procedure TOGLCSmoothLineRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCSmoothLineRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(TLineVertexAttributes), NIL); // x y distance threshold
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(TLineVertexAttributes), PChar(SizeOf(single)*4)); // color
  glEnableVertexAttribArray(1);
end;

procedure TOGLCSmoothLineRenderer.GetUniformLocation;
begin
  with FShader do
  begin
   // Use;
    FLocMVP := GetUniform('uMVP');
    FLocTintColor := GetUniform('uTintColor');
    FLocOpacity := GetUniform('uOpacity');
  //  Release;
  end;
end;

procedure TOGLCSmoothLineRenderer.SetUniformValuesAndTexture;
begin
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
  glUniform4fv(FLocTintColor, 1, @FTintF);
  glUniform1f(FLocOpacity, FOpacity);
  FParentScene.TexMan.UnbindTexture;
end;

procedure TOGLCSmoothLineRenderer.Prepare(aTriangleType: TTriangleType;
    const aMVP: TOGLCMatrix; const aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);
var forceFlush: boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP) or
                not FTintF.EqualTo(aComputedTint) or
                (FOpacity <> aOpacity);
  Batch_CheckIfNeedFlush(Self, aTriangleType, NIL, 0, aBlendMode, forceFlush);
  FMVP.CopyFrom(aMVP);
  FTintF.CopyFrom(aComputedTint);
  FOpacity := aOpacity;
end;

procedure TOGLCSmoothLineRenderer.DrawBorder(const aBorder: TOGLCBorder; aOpacity: single; aComputedTint: TColorF);
var p: pointer;
begin
  if aBorder.VertexCount = 0 then exit;
  Prepare(ptTriangles, FParentScene.MVPMatrix, aOpacity, aComputedTint, aBorder.BlendMode);
  //push index before query vertex because QueryVertex() change the value of FIndexInAttribsArray
  PushIndices(aBorder.Indices);
  p := QueryVertex(aBorder.VertexCount);
  aBorder.CopyVertexTo(p);
  Draw(True);
end;

procedure TOGLCSmoothLineRenderer.PushPath(const aPath: TOGLCPath; aLinePosition: TOGLCLinePosition; const aColor: TBGRAPixel;
  const w, aOpacity: single; aForceDraw: boolean);
var border: TOGLCBorder;
  pVertex: Pointer;
  computedTint: TColorF;
begin
  if Length(aPath) = 0 then exit;
  border.Color := aColor;
  border.LinePosition := aLinePosition;
  border.InitFromPath(aPath, w);
  computedTint.SetAsTransparent;
  Prepare(ptTriangleStrip, FParentScene.MVPMatrix, aOpacity, computedTint, 0);
  // if needed push primitive restart
  if FIndexInIndexArray > 0 then AddPrimitiveRestart;
  //push index before query vertex because QueryVertex() change the value of FIndexInAttribsArray
  PushIndices(border.Indices);
  pVertex := QueryVertex(border.VertexCount);
  border.CopyVertexTo(pVertex);

  if aForceDraw then Draw(True);
end;

procedure TOGLCSmoothLineRenderer.PushPath2(const aPath: TOGLCPath; aLinePosition: TOGLCLinePosition; const aColor: TBGRAPixel; const w,
  aOpacity: single; aForceDraw: boolean);
var border: TOGLCBorder;
  pVertex: Pointer;
  computedTint: TColorF;
begin
  if Length(aPath) = 0 then exit;
  border.Color := aColor;
  border.LinePosition := aLinePosition;
  border.InitFromPath(aPath, w);
  computedTint.SetAsTransparent;
  Prepare(ptTriangles, FParentScene.MVPMatrix, aOpacity, computedTint, 0);
  // if needed push primitive restart
  if FIndexInIndexArray > 0 then AddPrimitiveRestart;
  //push index before query vertex because QueryVertex() change the value of FIndexInAttribsArray
  PushIndices(border.Indices);
  pVertex := QueryVertex(border.VertexCount);
  border.CopyVertexTo(pVertex);

  if aForceDraw then Draw(True);
end;

procedure TOGLCSmoothLineRenderer.PushPaths(const aPaths: TArrayOfOGLCPath; aLinePosition: TOGLCLinePosition;
  const aColor: TBGRAPixel; const w: single; const aComputedTint: TColorF; const aOpacity: single; aForceDraw: boolean);
var border: TOGLCBorder;
  pVertex: Pointer;
  i: integer;
begin
  if Length(aPaths) = 0 then exit;
  border.Color := aColor;
  border.LinePosition := aLinePosition;
  Prepare(ptTriangles, FParentScene.MVPMatrix, aOpacity, aComputedTint, 0);

  for i:=0 to High(aPaths) do
    if Length(aPaths[i]) > 0 then begin
      border.InitFromPath(aPaths[i], w);
      //push index before query vertex because QueryVertex() change the value of FIndexInAttribsArray
      PushIndices(border.Indices);

      pVertex := QueryVertex(border.VertexCount);
      border.CopyVertexTo(pVertex);
    end;
  if aForceDraw then Draw(True);
end;

{ TOGLCFastLineRenderer }

procedure TOGLCFastLineRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCFastLineRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(Txycolor), NIL);   // x y
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(Txycolor), PChar(SizeOf(single)*2)); // color
  glEnableVertexAttribArray(1);
end;

procedure TOGLCFastLineRenderer.GetUniformLocation;
begin
  with FShader do
  begin
    FLocMVP := GetUniform('uMVP');
  end;
end;

procedure TOGLCFastLineRenderer.SetUniformValuesAndTexture;
begin
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
  FParentScene.TexMan.UnbindTexture;
end;

procedure TOGLCFastLineRenderer.InternalPushQuad(const aQuad: TQuadF; const aColor: TColorF);
var p: Pxycolor;
    pIndex: PVertexIndex;
    currentIndex: TVertexIndex;
begin
  //push index before query vertex because QueryVertex() change the value of FIndexInAttribsArray
  currentIndex := FIndexInAttribsArray;
  pIndex := QueryIndex(6);      // B   BD
  pIndex[0] := currentIndex;    // AC  C
  pIndex[1] := currentIndex+1;
  pIndex[2] := currentIndex+2;
  pIndex[3] := currentIndex+2;
  pIndex[4] := currentIndex+1;
  pIndex[5] := currentIndex+3;

  // push the 4 vertex        // vertex coord      24
  p := QueryVertex(4);        //                   13
  with p[0] do begin
    x := aQuad[3].x;
    y := aQuad[3].y;
    c.CopyFrom(aColor);
  end;
  with p[1] do begin
    x := aQuad[0].x;
    y := aQuad[0].y;
    c.CopyFrom(aColor);
  end;
  with p[2] do begin
    x := aQuad[2].x;
    y := aQuad[2].y;
    c.CopyFrom(aColor);
  end;
  with p[3] do begin
    x := aQuad[1].x;
    y := aQuad[1].y;
    c.CopyFrom(aColor);
  end;
end;

procedure TOGLCFastLineRenderer.Prepare(const aMVP: TOGLCMatrix; aBlendMode: byte);
var forceFlush: boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP);
  Batch_CheckIfNeedFlush(Self, ptTriangles, NIL, 0, aBlendMode, forceFlush);

  FMVP.CopyFrom(aMVP);
end;

procedure TOGLCFastLineRenderer.PushLine(pt1, pt2: TPointF; const aColor: TBGRAPixel; w: single; aLinePosition: TOGLCLinePosition);
var quad: TQuadF;
  cf: TColorF;
begin
  case aLinePosition of
    lpMiddle: quad := LineSegmentToQuadF_PosMiddle(pt1, pt2, w*0.5, w*0.5);
    lpInside: begin
      quad := LineSegmentToQuadF_PosInside(pt1, pt2, w*0.5, w*0.5);
      quad[0] := pt1;
      quad[1] := pt2;
    end;
  end;
  cf.InitFromBGRA(aColor);
  InternalPushQuad(quad, cf);
end;

procedure TOGLCFastLineRenderer.PushPath(aPath: TOGLCPath; const aColor: TBGRAPixel; w: single; aLinePosition: TOGLCLinePosition);
var quad1, quad2: TQuadF;
  cf: TColorF;
  i: integer;
  width1, width2: single;
    function DoLineSegmentToQuadF(const pt1, pt2: TPointF): TQuadF;
    begin
      case aLinePosition of
        lpMiddle: Result := LineSegmentToQuadF_PosMiddle(pt1, pt2, width1, width2);
        lpInside: Result := LineSegmentToQuadF_PosInside(pt1, pt2, width1, width2);
        else raise exception.create('you forgot to implement this !');
      end;
    end;
begin
  if Length(aPath) < 2 then exit;
  cf.InitFromBGRA(aColor);

  width1 := w * 0.5;
  width2 := width1;

  quad1 := DoLineSegmentToQuadF(aPath[0], aPath[1]);

  for i:=1 to High(aPath)-1 do begin
    quad2 := DoLineSegmentToQuadF(aPath[i], aPath[i+1]);
    AdjustCornerOnConsecutiveQuadF(quad1, quad2);
    InternalPushQuad(quad1, cf);
    Move(quad2, quad1, SizeOf(TQuadF));
  end;

  InternalPushQuad(quad1, cf);
end;


{ TOGLCThreeColorTriangleRenderer }

procedure TOGLCThreeColorTriangleRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCThreeColorTriangleRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(Txycolor), NIL);   // x y
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(Txycolor), PChar(SizeOf(single)*2)); // color
  glEnableVertexAttribArray(1);
end;

procedure TOGLCThreeColorTriangleRenderer.GetUniformLocation;
begin
  with FShader do
  begin
    //Use;
    FLocMVP := GetUniform('uMVP');
    FLocTintColor := GetUniform('uTintColor');
    FLocOpacity := GetUniform('uOpacity');
    //Release;
  end;
end;

procedure TOGLCThreeColorTriangleRenderer.SetUniformValuesAndTexture;
begin
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
  glUniform4fv(FLocTintColor, 1, @FTintF);
  glUniform1f(FLocOpacity, FOpacity);
  FParentScene.TexMan.UnbindTexture;
end;

procedure TOGLCThreeColorTriangleRenderer.Prepare(aTriangleType: TTriangleType;
    const aMVP: TOGLCMatrix; const aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);
var forceFlush: boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP) or
                (FOpacity <> aOpacity) or
                FTintF.EqualTo(aComputedTint);

  Batch_CheckIfNeedFlush(Self, aTriangleType, NIL, 0, aBlendMode, forceFlush);
  FMVP.CopyFrom(aMVP);
  FTintF.CopyFrom(aComputedTint);
  FOpacity := aOpacity;
end;

procedure TOGLCThreeColorTriangleRenderer.PushRectangle(const aX, aY, aWidth, aHeight: single; const aFillColor: TBGRAPixel);
var p: Pxycolor;
  cf: TColorF;
begin
  cf.InitFromBGRA(aFillColor);
  QueryAndFillIndexForQuadTriangleStrip;
  p := QueryVertex(4);
  with p^ do begin
    x := aX;
    y := aY+aHeight;
    c.CopyFrom(cf);
  end; inc(p);
  with p^ do begin
    x := aX;
    y := aY;
    c.CopyFrom(cf);
  end; inc(p);
  with p^ do begin
    x := aX+aWidth;
    y := aY+aHeight;
    c.CopyFrom(cf);
  end; inc(p);
  with p^ do begin
    x := aX+aWidth;
    y := aY;
    c.CopyFrom(cf);
  end; inc(p);
end;

procedure TOGLCThreeColorTriangleRenderer.PushGradient(aGradient: PGradientDescriptor);
var p: Pxycolor;
begin
  if aGradient = NIL then exit;
  if (Length(aGradient^.Vertices) = 0) or (Length(aGradient^.Indices) = 0) then exit;

  // if needed push primitive restart
  AddPrimitiveRestartIfNeeded;

  //push index before query vertex because QueryVertex() change the value of FIndexInAttribsArray
  PushIndices(aGradient^.Indices);

  p := QueryVertex(Length(aGradient^.Vertices));
  Move(aGradient^.Vertices[0], p^, SizeOf(Txycolor)*Length(aGradient^.Vertices));
end;

procedure TOGLCThreeColorTriangleRenderer.PushFill(aFill: POGLCFill;
    const aOpacity: single; const aComputedTint: TColorF; aForceDraw: boolean);
var p: pointer;
begin
  Prepare(ptTriangleFan, FParentScene.MVPMatrix, aOpacity, aComputedTint, aFill^.FBlendMode);
  AddPrimitiveRestartIfNeeded;
  PushIndices(aFill^.FIndices);
  p := QueryVertex(aFill^.VertexCount);
  aFill^.CopyVertexTo(p);
  if aForceDraw then Draw(True);
end;

{ TOGLCThreeColorMVTriangleRenderer }

procedure TOGLCThreeColorMVTriangleRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCThreeColorMVTriangleRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Txyopacitycolortintmatrix), NIL); // xy - opacity
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(Txyopacitycolortintmatrix), PChar(SizeOf(single)*3));  // color
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, sizeof(Txyopacitycolortintmatrix), PChar(SizeOf(single)*7));  // tint
  glEnableVertexAttribArray(2);

  glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, sizeof(Txyopacitycolortintmatrix), PChar(SizeOf(single)*11));  // model view matrix
  glEnableVertexAttribArray(3);
  glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, sizeof(Txyopacitycolortintmatrix), PChar(SizeOf(single)*15));
  glEnableVertexAttribArray(4);
  glVertexAttribPointer(5, 4, GL_FLOAT, GL_FALSE, sizeof(Txyopacitycolortintmatrix), PChar(SizeOf(single)*19));
  glEnableVertexAttribArray(5);
  glVertexAttribPointer(6, 4, GL_FLOAT, GL_FALSE, sizeof(Txyopacitycolortintmatrix), PChar(SizeOf(single)*23));
  glEnableVertexAttribArray(6);
end;

procedure TOGLCThreeColorMVTriangleRenderer.GetUniformLocation;
begin
  with FShader do
  begin
    Use;
    FLocProjectionMatrix := GetUniform('uProjectionMatrix');
    Release;
  end;
end;

procedure TOGLCThreeColorMVTriangleRenderer.SetUniformValuesAndTexture;
begin
  glUniformMatrix4fv(FLocProjectionMatrix, 1, GL_FALSE, @FProjectionMatrix.Matrix[0,0]);
end;

procedure TOGLCThreeColorMVTriangleRenderer.Prepare(aTriangleType: TTriangleType; const aProjectionMatrix: TOGLCMatrix;
    aBlendMode: byte);
var forceFlush: boolean;
begin
  forceFlush := not FProjectionMatrix.EqualTo(aProjectionMatrix);
  Batch_CheckIfNeedFlush(Self, aTriangleType, NIL, 0, aBlendMode, forceFlush);
  FProjectionMatrix.CopyFrom(aProjectionMatrix);
end;

procedure TOGLCThreeColorMVTriangleRenderer.PushQuad(const aModelArea: TQuadCoor; const aColors: TQuadColorF; aFlipIndex: integer;
  aModelViewMatrix: POGLCMatrix44; const aOpacity: single; const aComputedTint: TColorF);
var tci: PQuadCornerIndexes;
    p: Pxyopacitycolortintmatrix;
    pIndex: PVertexIndex;
    currentIndex: TVertexIndex;
begin
  tci := @FLIP_INDEXES[aFlipIndex];

  if aModelViewMatrix = NIL then
    aModelViewMatrix := @FParentScene.ModelViewMatrix.Matrix;


  //push index before query vertex because QueryVertex() change the value of FIndexInAttribsArray
  currentIndex := FIndexInAttribsArray;
  case FBatch^.CurrentPrimitiveType of
    ptTriangleStrip: begin           // 24
      AddPrimitiveRestartIfNeeded;
      pIndex := QueryIndex(4);       // 13
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
    x := aModelArea[cBL].x;
    y := aModelArea[cBL].y;
    opacity := aOpacity;
    c.CopyFrom(aColors[Ord(tci^[0])]);
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
  end;
  with p[1] do begin
    x := aModelArea[cTL].x;
    y := aModelArea[cTL].y;
    opacity := aOpacity;
    c.CopyFrom(aColors[Ord(tci^[1])]);
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
  end;
  with p[2] do begin
    x := aModelArea[cBR].x;
    y := aModelArea[cBR].y;
    opacity := aOpacity;
    c.CopyFrom(aColors[Ord(tci^[2])]);
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
  end;
  with p[3] do begin
    x := aModelArea[cTR].x;
    y := aModelArea[cTR].y;
    opacity := aOpacity;
    c.CopyFrom(aColors[Ord(tci^[3])]);
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
  end;
end;

{ TOGLCTexturedTriangleRenderer }

procedure TOGLCTexturedTriangleRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCTexturedTriangleRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(0);
end;

procedure TOGLCTexturedTriangleRenderer.GetUniformLocation;
begin
  with FShader do
  begin
    Use;
    FLocMVP := GetUniform('uMVP');
    FLocTextureUnit := GetUniform('uTexUnit');
    FLocTintColor := GetUniform('uTintColor');
    FLocOpacity := GetUniform('uOpacity');
    Release;
  end;
end;

procedure TOGLCTexturedTriangleRenderer.SetUniformValuesAndTexture;
begin
  glUniform1i(FLocTextureUnit, FBatch^.CurrentTextureUnit);
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
  glUniform4fv(FLocTintColor, 1, @FTintF);
  glUniform1f(FLocOpacity, FOpacity);
  FParentScene.TexMan.Bind(FBatch^.CurrentTexture, FBatch^.CurrentTextureUnit);
end;

procedure TOGLCTexturedTriangleRenderer.Prepare(aTriangleType: TTriangleType;
  aTexture: PTexture; aTextureUnit: integer; const aMVP: TOGLCMatrix;
  const aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);
var forceFlush: boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP) or
                (FOpacity <> aOpacity) or
                not FTintF.EqualTo(aComputedTint);

  Batch_CheckIfNeedFlush(Self, aTriangleType, aTexture, aTextureUnit, aBlendMode, forceFlush);
  FBatch^.CurrentTextureUnit := aTextureUnit;
  FMVP.CopyFrom(aMVP);
  FTintF.CopyFrom(aComputedTint);
  FOpacity := aOpacity;
end;

procedure TOGLCTexturedTriangleRenderer.PushFrame(const aFrameIndex,
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

procedure TOGLCTexturedTriangleRenderer.PushFrameWithCornerOffset(const aFrameIndex, aFlipIndex: integer;
  const aXPosOffset, aYPosOffset: single; aCornerOffset: TQuadParamF);
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
      p := QueryVertex(4);
      tci := @FLIP_INDEXES[aFlipIndex];

      with p[0] do begin
        u := texCoord^[ tci^[0] ].x;
        v := texCoord^[ tci^[0] ].y;
        x := aXPosOffset + aCornerOffset.BottomLeft.Value.x;
        y := h + aYPosOffset + aCornerOffset.BottomLeft.Value.y;
      end;
      with p[1] do begin
        u := texCoord^[ tci^[1] ].x;
        v := texCoord^[ tci^[1] ].y;
        x := aXPosOffset + aCornerOffset.TopLeft.Value.x;
        y := aYPosOffset + aCornerOffset.TopLeft.Value.y;
      end;
      with p[2] do begin
        u := texCoord^[ tci^[2] ].x;
        v := texCoord^[ tci^[2] ].y;
        x := w + aXPosOffset + aCornerOffset.BottomRight.Value.x;
        y := h + aYPosOffset + aCornerOffset.BottomRight.Value.y;
      end;
      with p[3] do begin
        u := texCoord^[ tci^[3] ].x;
        v := texCoord^[ tci^[3] ].y;
        x := w + aXPosOffset + aCornerOffset.TopRight.Value.x;
        y := aYPosOffset + aCornerOffset.TopRight.Value.y;
      end;
    end;
    ptTriangles: begin
      tci := @FLIP_INDEXES[aFlipIndex];
      p := QueryVertex(6);
      with p[0] do begin  // A           // B   BD
        u := texCoord^[ tci^[0] ].x;     // AC  C
        v := texCoord^[ tci^[0] ].y;
        x := aXPosOffset + aCornerOffset.BottomLeft.Value.x;
        y := h + aYPosOffset + aCornerOffset.BottomLeft.Value.y;
      end;
      with p[1] do begin  // B
        u := texCoord^[ tci^[1] ].x;
        v := texCoord^[ tci^[1] ].y;
        x := aXPosOffset + aCornerOffset.TopLeft.Value.x;
        y := aYPosOffset + aCornerOffset.TopLeft.Value.y;
      end;
      with p[2] do begin  // C
        u := texCoord^[ tci^[2] ].x;
        v := texCoord^[ tci^[2] ].y;
        x := w + aXPosOffset + aCornerOffset.BottomRight.Value.x;
        y := h + aYPosOffset + aCornerOffset.BottomRight.Value.y;
      end;
      Move(p[2], p[3], SizeOf(Txyuv)); // C
      Move(p[1], p[4], SizeOf(Txyuv)); // B
      with p[5] do begin               // D
        u := texCoord^[ tci^[3] ].x;
        v := texCoord^[ tci^[3] ].y;
        x := w + aXPosOffset + aCornerOffset.TopRight.Value.x;
        y := aYPosOffset + aCornerOffset.TopRight.Value.y;
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

procedure TOGLCTexturedTriangleRenderer.PushFrameClipped(const aFrameIndex,
  aFlipIndex: integer; const aXPosOffset, aYPosOffset: single; const aClipRect: TRectF);
var p: Pxyuv;
    pIndex: PVertexIndex;
    tci: PQuadCornerIndexes;
    texCoord: TQuadCoor;
    fi: Integer;
    clippedRect: TRectF;
    tex: PTexture;
    currentIndex: Int64;
begin
  tex := FBatch^.CurrentTexture;
  fi := EnsureRange(aFrameIndex, 0, tex^.FrameCount-1);
  texCoord.CopyFrom(tex^.FramesCoord[fi]);
  clippedRect := RectF(aXPosOffset, aYPosOffset, aXPosOffset+tex^.FrameWidth, aYPosOffset+tex^.FrameHeight);
  if not CheckAndAdjustF(aClipRect, clippedRect, texCoord) then exit;
  currentIndex := FIndexInAttribsArray;

  case FBatch^.CurrentPrimitiveType of
    ptTriangleStrip: begin
      tci := @FLIP_INDEXES[aFlipIndex];
      p := QueryVertex(4);
      with p[0] do begin             // BD
        u := texCoord[ tci^[0] ].x; // AC
        v := texCoord[ tci^[0] ].y;
        x := clippedRect.Left;
        y := clippedRect.Bottom;
      end;
      with p[1] do begin
        u := texCoord[ tci^[1] ].x;
        v := texCoord[ tci^[1] ].y;
        x := clippedRect.Left;
        y := clippedRect.Top;
      end;
      with p[2] do begin
        u := texCoord[ tci^[2] ].x;
        v := texCoord[ tci^[2] ].y;
        x := clippedRect.Right;
        y := clippedRect.Bottom;
      end;
      with p[3] do begin
        u := texCoord[ tci^[3] ].x;
        v := texCoord[ tci^[3] ].y;
        x := clippedRect.Right;
        y := clippedRect.Top;
      end;
    end;

    ptTriangles: begin
      tci := @FLIP_INDEXES[aFlipIndex];
      p := QueryVertex(6);
      with p[0] do begin  // A           // B   BD
        u := texCoord[ tci^[0] ].x;     // AC  C
        v := texCoord[ tci^[0] ].y;
        x := clippedRect.Left;
        y := clippedRect.Bottom;
      end;
      with p[1] do begin  // B
        u := texCoord[ tci^[1] ].x;
        v := texCoord[ tci^[1] ].y;
        x := clippedRect.Left;
        y := clippedRect.Top;
      end;
      with p[2] do begin  // C
        u := texCoord[ tci^[2] ].x;
        v := texCoord[ tci^[2] ].y;
        x := clippedRect.Right;
        y := clippedRect.Bottom;
      end;
      Move(p[2], p[3], SizeOf(Txyuv)); // C
      Move(p[1], p[4], SizeOf(Txyuv)); // B
      with p[5] do begin               // D
        u := texCoord[ tci^[3] ].x;
        v := texCoord[ tci^[3] ].y;
        x := clippedRect.Right;
        y := clippedRect.Top;
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

procedure TOGLCTexturedTriangleRenderer.PushQuad(const aTexCoord: TQuadCoor;
  aFlipIndex: integer; const aRect: TRectF);
var p: Pxyuv;
    pIndex: PVertexIndex;
    tci: PQuadCornerIndexes;
    currentIndex: Int64;
begin
  currentIndex := FIndexInAttribsArray;
  case FBatch^.CurrentPrimitiveType of
    ptTriangleStrip: begin
      tci := @FLIP_INDEXES[aFlipIndex];
      p := QueryVertex(4);
      with p[0] do begin             // BD
        u := aTexCoord[ tci^[0] ].x; // AC
        v := aTexCoord[ tci^[0] ].y;
        x := aRect.Left;
        y := aRect.Bottom;
      end;
      with p[1] do begin
        u := aTexCoord[ tci^[1] ].x;
        v := aTexCoord[ tci^[1] ].y;
        x := aRect.Left;
        y := aRect.Top;
      end;
      with p[2] do begin
        u := aTexCoord[ tci^[2] ].x;
        v := aTexCoord[ tci^[2] ].y;
        x := aRect.Right;
        y := aRect.Bottom;
      end;
      with p[3] do begin
        u := aTexCoord[ tci^[3] ].x;
        v := aTexCoord[ tci^[3] ].y;
        x := aRect.Right;
        y := aRect.Top;
      end;
    end;

    ptTriangles: begin
      tci := @FLIP_INDEXES[aFlipIndex];
      p := QueryVertex(6);
      with p[0] do begin  // A           // B   BD
        u := aTexCoord[ tci^[0] ].x;     // AC  C
        v := aTexCoord[ tci^[0] ].y;
        x := aRect.Left;
        y := aRect.Bottom;
      end;
      with p[1] do begin  // B
        u := aTexCoord[ tci^[1] ].x;
        v := aTexCoord[ tci^[1] ].y;
        x := aRect.Left;
        y := aRect.Top;
      end;
      with p[2] do begin  // C
        u := aTexCoord[ tci^[2] ].x;
        v := aTexCoord[ tci^[2] ].y;
        x := aRect.Right;
        y := aRect.Bottom;
      end;
      Move(p[2], p[3], SizeOf(Txyuv)); // C
      Move(p[1], p[4], SizeOf(Txyuv)); // B
      with p[5] do begin               // D
        u := aTexCoord[ tci^[3] ].x;
        v := aTexCoord[ tci^[3] ].y;
        x := aRect.Right;
        y := aRect.Top;
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

{ TOGLCTexturedMVTriangleRenderer }

procedure TOGLCTexturedMVTriangleRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCTexturedMVTriangleRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), NIL); // xyuv
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*4));  // color
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(2, 1, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*8));  // opacity
  glEnableVertexAttribArray(2);

  glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*9));  // model view matrix
  glEnableVertexAttribArray(3);
  glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*13));
  glEnableVertexAttribArray(4);
  glVertexAttribPointer(5, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*17));
  glEnableVertexAttribArray(5);
  glVertexAttribPointer(6, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*21));
  glEnableVertexAttribArray(6);
end;

procedure TOGLCTexturedMVTriangleRenderer.GetUniformLocation;
begin
  with FShader do
  begin
    //Use;
    FLocProjectionMatrix := GetUniform('uProjectionMatrix');
    FLocTextureUnit := GetUniform('uTexUnit');
    //Release;
  end;
end;

procedure TOGLCTexturedMVTriangleRenderer.SetUniformValuesAndTexture;
begin
  glUniform1i(FLocTextureUnit, GLint(FBatch^.CurrentTextureUnit));
  glUniformMatrix4fv(FLocProjectionMatrix, 1, GL_FALSE, @FParentScene.ProjectionMatrix.Matrix[0,0]);
  FParentScene.TexMan.Bind(FBatch^.CurrentTexture, FBatch^.CurrentTextureUnit);
end;

procedure TOGLCTexturedMVTriangleRenderer.Prepare(aTriangleType: TTriangleType;
  aTexture: PTexture; aTextureUnit: integer; aBlendMode: byte);
begin
  Batch_CheckIfNeedFlush(Self, aTriangleType, aTexture, aTextureUnit, aBlendMode, False);
end;

procedure TOGLCTexturedMVTriangleRenderer.PushQuad(const aFrameIndex, aFlipIndex: integer;
  const aModelViewMatrix: POGLCMatrix44; const aXPosOffset, aYPosOffset, aOpacity: single; const aComputedTint: TColorF);
var w, h: single;
    fi: Integer;
    area: {TRect;} TQuadCoor;
    tex: PTexture;
begin
  tex := FBatch^.CurrentTexture;
  w := tex^.FrameWidth;
  h := tex^.FrameHeight;

  fi := EnsureRange(aFrameIndex, 0, tex^.FrameCount-1);

  area[cBL].x := aXPosOffset;
  area[cBL].y := h + aYPosOffset;
  area[cTL].x := aXPosOffset;
  area[cTL].y := aYPosOffset;
  area[cBR].x := w + aXPosOffset;
  area[cBR].y := h + aYPosOffset;
  area[cTR].x := w + aXPosOffset;
  area[cTR].y := aYPosOffset;

  PushQuad2(area, tex^.FramesCoord[fi], aFlipIndex, aModelViewMatrix, aOpacity, aComputedTint);
end;

procedure TOGLCTexturedMVTriangleRenderer.PushQuad2(const aModelArea, aTexCoor: TQuadCoor; aFlipIndex: integer;
  aModelViewMatrix: POGLCMatrix44; const aOpacity: single; const aComputedTint: TColorF);
var tci: PQuadCornerIndexes;
    p: Pxyuvtintopacitymatrix;
    pIndex: PVertexIndex;
    currentIndex: TVertexIndex;
begin
  tci := @FLIP_INDEXES[aFlipIndex];
  currentIndex := FIndexInAttribsArray;
  if aModelViewMatrix = NIL then
    aModelViewMatrix := @FParentScene.ModelViewMatrix.Matrix;

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

  // push the 4 vertex        // vertex coord      24
  p := QueryVertex(4);        //                   13
  with p[0] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := aTexCoor[ tci^[0] ].x;
    v := aTexCoor[ tci^[0] ].y;
    x := aModelArea[cBL].x;
    y := aModelArea[cBL].y;
    opacity := aOpacity;
  end;
  with p[1] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := aTexCoor[ tci^[1] ].x;
    v := aTexCoor[ tci^[1] ].y;
    x := aModelArea[cTL].x;
    y := aModelArea[cTL].y;
    opacity := aOpacity;
  end;
  with p[2] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := aTexCoor[ tci^[2] ].x;
    v := aTexCoor[ tci^[2] ].y;
    x := aModelArea[cBR].x;
    y := aModelArea[cBR].y;
    opacity := aOpacity;
  end;
  with p[3] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := aTexCoor[ tci^[3] ].x;
    v := aTexCoor[ tci^[3] ].y;
    x := aModelArea[cTR].x;
    y := aModelArea[cTR].y;
    opacity := aOpacity;
  end;
end;

procedure TOGLCTexturedMVTriangleRenderer.PushQuad3(const aModelArea: TRect; const aTexCoor: TQuadCoor; aFlipIndex: integer;
  aModelViewMatrix: POGLCMatrix44; const aOpacity: single; const aComputedTint: TColorF);
var tci: PQuadCornerIndexes;
    p: Pxyuvtintopacitymatrix;
    pIndex: PVertexIndex;
    currentIndex: TVertexIndex;
begin
  tci := @FLIP_INDEXES[aFlipIndex];
  if aModelViewMatrix = NIL then
    aModelViewMatrix := @FParentScene.ModelViewMatrix.Matrix;

  // push indexes
  currentIndex := FIndexInAttribsArray;
  case FBatch^.CurrentPrimitiveType of
    ptTriangleStrip: begin
      AddPrimitiveRestartIfNeeded;
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
  // push the 4 vertex          //                   13
  p := QueryVertex(4);          // vertex coord      24
  with p[0] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := aTexCoor[ tci^[0] ].x;
    v := aTexCoor[ tci^[0] ].y;
    x := aModelArea.Left;
    y := aModelArea.Bottom;
    opacity := aOpacity;
  end;
  with p[1] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := aTexCoor[ tci^[1] ].x;
    v := aTexCoor[ tci^[1] ].y;
    x := aModelArea.Left;
    y := aModelArea.Top;
    opacity := aOpacity;
  end;
  with p[2] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := aTexCoor[ tci^[2] ].x;
    v := aTexCoor[ tci^[2] ].y;
    x := aModelArea.Right;
    y := aModelArea.Bottom;
    opacity := aOpacity;
  end;
  with p[3] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := aTexCoor[ tci^[3] ].x;
    v := aTexCoor[ tci^[3] ].y;
    x := aModelArea.Right;
    y := aModelArea.Top;
    opacity := aOpacity;
  end;
end;

{ TOGLCNoFilterTextureMVTriangleRenderer }

procedure TOGLCNoFilterTextureMVTriangleRenderer.InitShaderCodeAndCallBack;
begin
  inherited InitShaderCodeAndCallBack;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTextureCustomized;
end;
procedure TOGLCNoFilterTextureMVTriangleRenderer.SetUniformValuesAndTextureCustomized;
begin
  glUniform1i(FLocTextureUnit, GLint(FBatch^.CurrentTextureUnit));
  glUniformMatrix4fv(FLocProjectionMatrix, 1, GL_FALSE, @FParentScene.ProjectionMatrix.Matrix[0,0]);
  FParentScene.TexMan.Bind(FBatch^.CurrentTexture, FBatch^.CurrentTextureUnit);

  // we don't want texture filtering
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
end;

procedure TOGLCNoFilterTextureMVTriangleRenderer.Draw(
  aResetBufferStateWhenDone: boolean);
begin
  inherited Draw(aResetBufferStateWhenDone);

  // set 'normal' filtering
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
end;

{$endif oglcIMPLEMENTATION}

