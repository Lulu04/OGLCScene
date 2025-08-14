{$ifdef oglcINTERFACE}
{
  Define a surface with a texture that can be scrolled both horizontally and vertically.
  limitation for this current version:
    - the texture must be isolated (not inside an atlas)
}

type

TScrollableTextureVertexAttribs = packed record
  x, y, u, v: single;    // vertex + current texture coordinates
  uleft, vbottom, uright, vtop: single;  // begin and end of uv coordinates
  tint: TColorF;   // tint
  offsetu, offsetv, opacity: single; // scrolling offset + global opacity
  mv: TOGLCMatrix; // model view matrix
end;
PTScrollableTextureVertexAttribs = ^TScrollableTextureVertexAttribs;

//

{ TOGLCScrollableTextureRenderer }

TOGLCScrollableTextureRenderer = class(specialize TOGLCGenericPrimitiveRenderer<TScrollableTextureVertexAttribs>)
private const
  VERTEX_SHADER =
   '#version 330 core'#10+
   '  layout(location = 0) in vec4 aVertexAndTextureCoor;'#10+
   '  layout(location = 1) in vec4 aLeftBottomRightTopUVCoor;'#10+
   '  layout(location = 2) in vec4 aVertexTint;'#10+
   '  layout(location = 3) in vec3 aUOffsetVOffsetOpacity;'#10+
   '  layout(location = 4) in mat4 aModelViewMatrix;'#10+
   '  uniform mat4 uProjectionMatrix;'#10+
   '  out vec2 TexCoords;'#10+
   '  out vec4 VertexTint;'#10+
   '  flat out float Opacity;'#10+
   '  flat out vec4  LeftRightBottomTopUVCoor;'#10+
   '  flat out vec2 UVOffset;'#10+
   'void main()'#10+
   '{'#10+
   '  gl_Position = uProjectionMatrix*aModelViewMatrix*vec4(aVertexAndTextureCoor.xy, 0.0, 1.0);'#10+
   '  TexCoords = aVertexAndTextureCoor.zw;'#10+
   '  LeftRightBottomTopUVCoor = aLeftBottomRightTopUVCoor;'#10+
   '  VertexTint = aVertexTint;'#10+
   '  UVOffset = aUOffsetVOffsetOpacity.xy;'#10+
   '  Opacity = aUOffsetVOffsetOpacity.z;'#10+
   '}';

  FRAGMENT_SHADER =
   '#version 330 core'#10+
   '  out vec4 FragColor;'#10+
   '  in vec2 TexCoords;'#10+
   '  in vec4 VertexTint;'#10+
   '  flat in float Opacity;'#10+
   '  flat in vec4  LeftBottomRightTopUVCoor;'#10+
   '  flat in vec2 UVOffset;'#10+
   '  uniform sampler2D uTexUnit;'#10+
   'void main()'#10+
   '{'#10+
   // adjust the uv coordinate
   '  vec2 uv = TexCoords + UVOffset;'#10+
{   '  if (uv.x >= LeftBottomRightTopUVCoor.z)'#10+
   '     uv.x = LeftBottomRightTopUVCoor.x + uv.x - LeftBottomRightTopUVCoor.z;'#10+
   '  if (uv.y >= LeftBottomRightTopUVCoor.w)'#10+
   '     uv.y = LeftBottomRightTopUVCoor.y + uv.y - LeftBottomRightTopUVCoor.w;'#10+   }

   'vec2 coeff = step(LeftBottomRightTopUVCoor.zw, uv);'#10+
   'uv = (vec2(1.0)-coeff)*uv + coeff*(LeftBottomRightTopUVCoor.xy+uv-LeftBottomRightTopUVCoor.zw);'#10+

   '  vec4 col = texture(uTexUnit, uv);'#10+
   // Tint and opacity
   '  float a = col.a*Opacity;'#10+
   '  if (a == 0) discard;'#10+
   '  float tintAlphaX2 = VertexTint.a*2;'#10+
   '  if (VertexTint.a > 0.5)'#10+
   '   {'#10+
   '     tintAlphaX2 = tintAlphaX2-1;'#10+
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
protected
  procedure InitShaderCodeAndCallBack; override;
private
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  // procedure Draw use parent scene MVP matrix
  procedure Prepare(aTriangleType: TTriangleType; aTexture: PTexture; aTextureUnit: integer; aBlendMode: byte);

  // Set aModelViewMatrix to NIL to use the parent scene model view matrix
  procedure PushQuad(const aModelArea, aTexBounds: TQuadCoor; aOffsetUV: TPointF;
                     aFlipIndex: integer; aModelViewMatrix: POGLCMatrix44;
                     const aOpacity: single; const aComputedTint: TColorF); overload;
end;


{ TScrollableSprite }

TScrollableSprite = class(TSprite)
private
  procedure Init; override;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
  destructor Destroy; override;
public
  // Offset in pixels. Default value is (0, 0)
  Offset: TBoundedPointFParam;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCScrollableTextureRenderer }

procedure TOGLCScrollableTextureRenderer.InitShaderCodeAndCallBack;
begin
  FShaderName := 'TOGLCScrollableTextureRenderer';
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCScrollableTextureRenderer.DefineVertexAttribPointer;
begin
  // xyuv
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(TScrollableTextureVertexAttribs), NIL);
  glEnableVertexAttribArray(0);
  // uv bounds
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(TScrollableTextureVertexAttribs), PChar(SizeOf(single)*4));
  glEnableVertexAttribArray(1);
  // color
  glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, sizeof(TScrollableTextureVertexAttribs), PChar(SizeOf(single)*8));
  glEnableVertexAttribArray(2);
  // UVOffset + opacity
  glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, sizeof(TScrollableTextureVertexAttribs), PChar(SizeOf(single)*12));
  glEnableVertexAttribArray(3);

  // model view matrix
  glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, sizeof(TScrollableTextureVertexAttribs), PChar(SizeOf(single)*15));
  glEnableVertexAttribArray(4);
  glVertexAttribPointer(5, 4, GL_FLOAT, GL_FALSE, sizeof(TScrollableTextureVertexAttribs), PChar(SizeOf(single)*19));
  glEnableVertexAttribArray(5);
  glVertexAttribPointer(6, 4, GL_FLOAT, GL_FALSE, sizeof(TScrollableTextureVertexAttribs), PChar(SizeOf(single)*23));
  glEnableVertexAttribArray(6);
  glVertexAttribPointer(7, 4, GL_FLOAT, GL_FALSE, sizeof(TScrollableTextureVertexAttribs), PChar(SizeOf(single)*27));
  glEnableVertexAttribArray(7);
end;

procedure TOGLCScrollableTextureRenderer.GetUniformLocation;
begin
  with FShader do begin
    FLocProjectionMatrix := GetUniform('uProjectionMatrix');
    FLocTextureUnit := GetUniform('uTexUnit');
  end;
end;

procedure TOGLCScrollableTextureRenderer.SetUniformValuesAndTexture;
begin
  glUniform1i(FLocTextureUnit, GLint(FBatch^.CurrentTextureUnit));
  glUniformMatrix4fv(FLocProjectionMatrix, 1, GL_FALSE, @FParentScene.ProjectionMatrix.Matrix[0,0]);
  FParentScene.TexMan.Bind(FBatch^.CurrentTexture, FBatch^.CurrentTextureUnit);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
end;

procedure TOGLCScrollableTextureRenderer.Prepare(aTriangleType: TTriangleType;
  aTexture: PTexture; aTextureUnit: integer; aBlendMode: byte);
begin
  Batch_CheckIfNeedFlush(Self, aTriangleType, aTexture, aTextureUnit, aBlendMode, False);
end;

procedure TOGLCScrollableTextureRenderer.PushQuad(const aModelArea,
  aTexBounds: TQuadCoor; aOffsetUV: TPointF; aFlipIndex: integer;
  aModelViewMatrix: POGLCMatrix44; const aOpacity: single;
  const aComputedTint: TColorF);
var tci: PQuadCornerIndexes;
    p: PTScrollableTextureVertexAttribs;
    pIndex: PVertexIndex;
    currentIndex: TVertexIndex;
    texCoor: TQuadCoor;
    deltaU, deltaV: Single;
debug:tpointf;
debug1: tquadcoor;
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

  texCoor.CopyFrom(aTexBounds);

  // push the 4 vertex        // vertex coord      24
  p := QueryVertex(4);        //                   13
  with p[0] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := texCoor[ tci^[0] ].x;
    v := texCoor[ tci^[0] ].y;
    x := aModelArea[cBL].x;
    y := aModelArea[cBL].y;
    opacity := aOpacity;
    uleft := aTexBounds[cBL].x;
    vbottom := aTexBounds[cBL].y;
    uright := aTexBounds[cTR].x;
    vtop := aTexBounds[cTR].y;
    offsetu := aOffsetUV.x;
    offsetv := aOffsetUV.y;
  end;
  with p[1] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := texCoor[ tci^[1] ].x;
    v := texCoor[ tci^[1] ].y;
    x := aModelArea[cTL].x;
    y := aModelArea[cTL].y;
    opacity := aOpacity;
    uleft := aTexBounds[cBL].x;
    vbottom := aTexBounds[cBL].y;
    uright := aTexBounds[cTR].x;
    vtop := aTexBounds[cTR].y;
    offsetu := aOffsetUV.x;
    offsetv := aOffsetUV.y;
  end;
  with p[2] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := texCoor[ tci^[2] ].x;
    v := texCoor[ tci^[2] ].y;
    x := aModelArea[cBR].x;
    y := aModelArea[cBR].y;
    opacity := aOpacity;
    uleft := aTexBounds[cBL].x;
    vbottom := aTexBounds[cBL].y;
    uright := aTexBounds[cTR].x;
    vtop := aTexBounds[cTR].y;
    offsetu := aOffsetUV.x;
    offsetv := aOffsetUV.y;
  end;
  with p[3] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := texCoor[ tci^[3] ].x;
    v := texCoor[ tci^[3] ].y;
    x := aModelArea[cTR].x;
    y := aModelArea[cTR].y;
    opacity := aOpacity;
    uleft := aTexBounds[cBL].x;
    vbottom := aTexBounds[cBL].y;
    uright := aTexBounds[cTR].x;
    vtop := aTexBounds[cTR].y;
    offsetu := aOffsetUV.x;
    offsetv := aOffsetUV.y;
  end;
end;

{ TScrollableSprite }

procedure TScrollableSprite.Init;
begin
  inherited Init;
  Offset := TBoundedPointFParam.Create;
  Offset.SetBoundary(PointF(0,0), PointF(Texture^.FrameWidth, Texture^.FrameHeight), True);
  Offset.Value := PointF(0, 0);

  SetShape(QuadCoor(PointF(0,0),
                    PointF(Texture^.FrameWidth, 0),
                    PointF(Texture^.FrameWidth, Texture^.FrameHeight),
                    PointF(0,Texture^.FrameHeight)));
end;

function TScrollableSprite.GetWidth: integer;
begin
  Result := Round(Abs(FModelArea[cBR].x - FModelArea[cBL].x));
end;

function TScrollableSprite.GetHeight: integer;
begin
  Result := Round(Abs(FModelArea[cTL].y - FModelArea[cBL].y));
end;

procedure TScrollableSprite.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if not Freeze then
    Offset.OnElapse(aElapsedTime);
end;

procedure TScrollableSprite.DoDraw;
var fi: integer;
    p: TPointF;
begin
  if FTexture = NIL then exit;

  fi := EnsureRange(Trunc(Frame), 0, High(FTexture^.FramesCoord));

  p.x := Offset.x.Value / FTexture^.FrameWidth;
  p.y := Offset.y.Value / FTexture^.FrameHeight;

  FParentScene.ScrollableTextureRenderer.Prepare(ptTriangleStrip, FTexture, 0, BlendMode);
  FParentScene.ScrollableTextureRenderer.PushQuad(FModelArea, FTexture^.FramesCoord[fi],
     p, FlipToIndex, NIL, FComputedOpacity, FComputedTint);
end;

destructor TScrollableSprite.Destroy;
begin
  Offset.Free;
  Offset := NIL;
  inherited Destroy;
end;

{$endif oglcIMPLEMENTATION}

