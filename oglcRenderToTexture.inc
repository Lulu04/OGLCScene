{$ifdef oglcINTERFACE}
type


{TOGLCFBOManager=class
  TOGLCFBODataToSave=record
    ViewPortSize: TSize;
    FBO: GLuint;
  end;

end;   }


{ TOGLCRenderToTexture }

TOGLCRenderToTexture=class
private type
  TOGLCDataToSave=record
    FBOIsValid: boolean;
    FBO: GLuint;
    ViewPortSize: TSize;
  end;
private
  class var FStack: TOGLCDataToSave;
  class var FCount: integer;
private
  FOldStack: TOGLCDataToSave;
  FFBO: GLuint;
  FRenderedTexture: PTexture;
  FReady: boolean;
  FColorAttachment: GLenum;
public
  // aColorAttachmentIndex is an array thats contains the index of the wanted ColorAttachment
  // IE: if you want to render in the ColorAttachment0 then set aColorAttachmentIndexes to 0
  // must be [0..15]
  Constructor Create( aParentScene: TOGLCScene; aWidth, aHeight, aColorAttachmentIndex: integer );
  Destructor destroy; override;

  // Enable the FBO
  procedure Bind;
  // Disable the FBO
  procedure Unbind;

 // TRUE if the RenderToTexture object has been correctly created
 property Ready: boolean read FReady;
 property RenderedTexture: PTexture read FRenderedTexture;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

constructor TOGLCRenderToTexture.Create(aParentScene: TOGLCScene; aWidth, aHeight, aColorAttachmentIndex: integer);
// var db: GLEnum;
begin
 if FCount=0 then begin
   FStack.ViewPortSize.cx := aParentScene.Width;
   FStack.ViewPortSize.cy := aParentScene.Height;
 end;
 inc(FCount);

 if aColorAttachmentIndex>=ColorAttachmentCount
   then Raise Exception.Create ('TRenderToTexture.Create: you ask for a ColorAttachment='+
                       aColorAttachmentIndex.ToString+' but it must be between [0..'+(ColorAttachmentCount-1).ToString+'] !');
 FColorAttachment:=IndexToColorAttachment(aColorAttachmentIndex);

 glGenFramebuffers(1, @FFBO);
 glBindFramebuffer(GL_FRAMEBUFFER, FFBO);

 FRenderedTexture := TextureManager.AddEmpty( aWidth, aHeight );
// TextureManager.BindTexture( FRenderedTexture );
// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
// glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
 TextureManager.DisableTextureUsage;

 glFramebufferTexture2D(GL_FRAMEBUFFER, FColorAttachment, GL_TEXTURE_2D, FRenderedTexture^.ID, 0);

// glFramebufferTexture(GL_FRAMEBUFFER, FColorAttachment, FRenderedTexture^.ID, 0);

// db := (GL_COLOR_ATTACHMENT0);
 // Set the list of draw buffers.
// GLenum DrawBuffers[1] = {GL_COLOR_ATTACHMENT0};
 glDrawBuffers(1, @FColorAttachment);

 FReady := glCheckFramebufferStatus(GL_FRAMEBUFFER) = GL_FRAMEBUFFER_COMPLETE;
 if not FReady then raise Exception.Create('Erreur de create de FBO');

 if FStack.FBOIsValid
   then glBindFramebuffer(GL_FRAMEBUFFER, FStack.FBO)
   else glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

destructor TOGLCRenderToTexture.destroy;
begin
 glDeleteFramebuffers(1, @FFBO);
 TextureManager.Delete( FRenderedTexture );
 inherited destroy;
end;

procedure TOGLCRenderToTexture.Bind;
begin
 FOldStack := FStack;
 glBindFramebuffer(GL_FRAMEBUFFER, FFBO);
 glViewport(0, 0, FRenderedTexture^.TextureWidth, FRenderedTexture^.TextureHeight);

 with FStack do begin
  FBOIsValid:=TRUE;
  FBO:=FFBO;
  ViewPortSize.cx:=FRenderedTexture^.TextureWidth;
  ViewPortSize.cy:=FRenderedTexture^.TextureHeight;
 end;
end;

procedure TOGLCRenderToTexture.Unbind;
begin
 if FOldStack.FBOIsValid
   then glBindFramebuffer(GL_FRAMEBUFFER, FOldStack.FBO)
   else glBindFramebuffer(GL_FRAMEBUFFER, 0);

 glViewport(0, 0, FOldStack.ViewPortSize.cx, FOldStack.ViewPortSize.cy);

 FStack:=FOldStack;
end;







{$endif oglcIMPLEMENTATION}

