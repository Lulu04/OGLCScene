{$ifdef oglcINTERFACE}
type

{ TOGLCRenderToTexture }

TOGLCRenderToTexture = class
private
  FParentScene: TOGLCScene;
  FFBO, FDepthAndStencilName: GLuint;
  FRenderedTexture: PTexture;
  FReady, FIsBound: boolean;
  FColorAttachment,
  FColorAttachmentIndex: GLenum;
  FUseDepthAndStencil: boolean;
  FClearBitField: GLbitfield;
  FClearColorF: TColorF;
  function GetClearColor: TBGRAPixel;
  procedure SetClearColor(AValue: TBGRAPixel);
public
  // aColorAttachmentIndex is an array thats contains the index of the wanted ColorAttachment
  // IE: if you want to render in the ColorAttachment0 then set aColorAttachmentIndexes to 0
  // must be [0..n], n depending of the video driver.
  // Set aUseDepthAndStencil to True if you need a depth and/or stencil buffer.
  constructor Create(aParentScene: TOGLCScene; aWidth, aHeight, aColorAttachmentIndex: integer; aUseDepthAndStencil: boolean=False);
  destructor Destroy; override;

  // Enable render to texture
  procedure Bind;
  // Clear the opengl color buffer and eventually the depth/stencil buffer
  procedure ClearBuffers;
  // Disable render to texture.
  // If different, don't forget to re-initialize the size of the openGL viewport with glViewport().
  procedure Unbind;

 // TRUE if the RenderToTexture object is correctly created
 property Ready: boolean read FReady;
 // The color used to erase the texture. Default is transparent color (0,0,0,0)
 property ClearColor: TBGRAPixel read GetClearColor write SetClearColor;
 // The output texture
 property RenderedTexture: PTexture read FRenderedTexture;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

function TOGLCRenderToTexture.GetClearColor: TBGRAPixel;
begin
  Result := FClearColorF.ToBGRA;
end;

procedure TOGLCRenderToTexture.SetClearColor(AValue: TBGRAPixel);
begin
  FClearColorF.InitFromBGRA(AValue);
end;

constructor TOGLCRenderToTexture.Create(aParentScene: TOGLCScene; aWidth,
  aHeight, aColorAttachmentIndex: integer; aUseDepthAndStencil: boolean);
var err: GLenum;
begin
  FParentScene := aParentScene;

  if aColorAttachmentIndex >= ColorAttachmentCount
    then Raise Exception.Create ('TRenderToTexture.Create: you ask for a ColorAttachment='+
                        aColorAttachmentIndex.ToString+' but it must be between [0..'+(ColorAttachmentCount-1).ToString+'] !');
  FColorAttachment := GL_COLOR_ATTACHMENT0 + aColorAttachmentIndex;
  FColorAttachmentIndex := aColorAttachmentIndex;

  err := glGetError();
  glGenFramebuffers(1, @FFBO);
  glBindFramebuffer(GL_FRAMEBUFFER, FFBO);

  FRenderedTexture := FParentScene.TexMan.AddEmpty(aWidth, aHeight);
// TextureManager.BindTexture( FRenderedTexture );
// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  FParentScene.TexMan.UnbindTexture;

  glFramebufferTexture2D(GL_FRAMEBUFFER, FColorAttachment, GL_TEXTURE_2D, FRenderedTexture^.ID, 0);

  glDrawBuffers(1, @FColorAttachment);

  FUseDepthAndStencil := aUseDepthAndStencil;
  FClearBitField := GL_COLOR_BUFFER_BIT;
  if aUseDepthAndStencil then begin
    FClearBitField := FClearBitField or GL_STENCIL_BUFFER_BIT;
    glGenRenderbuffers(1, @FDepthAndStencilName);
    glBindRenderbuffer(GL_RENDERBUFFER, FDepthAndStencilName);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, aWidth, aHeight);
    glBindRenderbuffer(GL_RENDERBUFFER, 0);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, FDepthAndStencilName);
  end;

  FReady := glCheckFramebufferStatus(GL_FRAMEBUFFER) = GL_FRAMEBUFFER_COMPLETE;
  if not FReady then begin
    FParentScene.LogError('TOGLCRenderToTexture.Create('+aWidth.ToString+', '+aHeight.ToString+', '+aColorAttachmentIndex.ToString+
                          ') frame buffer not ready');
  end;
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  err := glGetError();
  if err <> GL_NO_ERROR then begin
    FParentScene.LogError('TOGLCRenderToTexture.Create generate a GL ERROR $'+IntToHex(err, 4)+' '+GLErrorToString(err));
  end;

  FClearColorF.InitFromBGRA(BGRA(0,0,0,0));
end;

destructor TOGLCRenderToTexture.Destroy;
begin
  glDeleteFramebuffers(1, @FFBO);
  if FUseDepthAndStencil then
    glDeleteRenderbuffers(1, @FDepthAndStencilName);
  FParentScene.TexMan.Delete(FRenderedTexture);
  inherited Destroy;
end;

procedure TOGLCRenderToTexture.Bind;
var err: GLenum;
begin
  if not Ready or FIsBound then exit;
  FIsBound := True;

  glBindFramebuffer(GL_FRAMEBUFFER, FFBO);
  glViewport(0, 0, FRenderedTexture^.TextureWidth, FRenderedTexture^.TextureHeight);

  FParentScene.TexMan.Bind(FRenderedTexture, FColorAttachmentIndex);

  err := glGetError();
  if err <> GL_NO_ERROR then
    FParentScene.LogError('TOGLCRenderToTexture.Bind generate a GL ERROR $'+IntToHex(err, 4)+' '+GLErrorToString(err));
end;

procedure TOGLCRenderToTexture.ClearBuffers;
begin
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(FClearBitField);
end;

procedure TOGLCRenderToTexture.Unbind;
var err: GLenum;
begin
  if not Ready or not FIsBound then exit;
  FIsBound := False;

  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  glActiveTexture(GL_TEXTURE0+FColorAttachmentIndex);
  glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE0);

  err := glGetError();
  if err <> GL_NO_ERROR then
    FParentScene.LogError('TOGLCRenderToTexture.Unbind generate a GL ERROR $'+IntToHex(err, 4)+' '+GLErrorToString(err));
end;

{$endif oglcIMPLEMENTATION}

