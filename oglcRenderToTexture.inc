{$ifdef oglcINTERFACE}
type

// TO DO

{ TRenderToTex }

TRenderToTex= class
private
  FFramebufferName: GLuint;
  FRenderedTexture: PTexture;
  FReady: boolean;
private
  //FShader: TShaderID;
public
  Constructor Create( aWidth, aHeight: integer );
  Destructor destroy; override;

//  procedure
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

constructor TRenderToTex.Create(aWidth, aHeight: integer);
var db: GLEnum;
begin
 glGenFramebuffers(1, @FFramebufferName);
 glBindFramebuffer(GL_FRAMEBUFFER, FFramebufferName);

 FRenderedTexture := TextureManager.AddEmpty( aWidth, aHeight );
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

 glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, FRenderedTexture^.ID, 0);

 db := (GL_COLOR_ATTACHMENT0);
 // Set the list of draw buffers.
// GLenum DrawBuffers[1] = {GL_COLOR_ATTACHMENT0};
 glDrawBuffers(1, @db);

 FReady := glCheckFramebufferStatus(GL_FRAMEBUFFER) = GL_FRAMEBUFFER_COMPLETE;

end;

destructor TRenderToTex.destroy;
begin
 glDeleteFramebuffers(1, @FFramebufferName);
 TextureManager.Delete( FRenderedTexture );
 inherited destroy;
end;







{$endif oglcIMPLEMENTATION}

