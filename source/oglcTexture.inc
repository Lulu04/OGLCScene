{$ifdef oglcINTERFACE}
{
  Textures can be atlas that contains several sub-textures
  Sub-texture is a part of another texture, it share the same OpenGL ID than its parent.
  A texture or a sub-texture can be splitted in several frames.
  Frames are several images to represent an image animation.
}
type

{ TTexture }
PTexture = ^TTexture;
TTexture = record
private
  FParentScene: TOGLCScene;
  procedure ComputeOpenGLFramesCoord(aXFrameCount: Integer=1; aYFrameCount: Integer=1);
public
  ID: GLuint;
  IDOwner: boolean;
  Filename: string; // the filename without path. '' if the texture is not loaded from a file.
  TextureWidth: integer;
  TextureHeight: integer;
  FrameWidth: integer;
  FrameHeight: integer;
  FrameCount: integer; // = 1 minimum = length(FramesCoord)
  FramesCoord: array of TQuadCoor;  // [0] is whole texture or sub-texture, [1] is first frame, [2] is second frame, ...

  // convert a texture pixel coordinates into its UV equivalent
  // texture coordinates (0,0) is the top/left of the (sub-)texture. (local space)
  function FrameXYToUV(aPt: TPointF; aFrameIndex: integer=0): TPointF;
  // the returned point is relative to the Top/Left of the WHOLE texture
  function GetTopLeftCoor(aFrameIndex: integer): TPointF;
  function GetBottomRightCoor(aFrameIndex: integer): TPointF;
  // the returned rectangle is relative to the Top/Left of the WHOLE texture
  function GetRect(aFrameIndex: integer): TRect;

  procedure SetSingleFrame;
  procedure SetFrameSize(AFrameWidth, AFrameHeight: integer);
  function GetFrameCount: TSize;

  procedure TakePartOf(aTexture: PTexture; aRect: TRect); overload;
  procedure TakePartOf(aTexture: PTexture; aRect: TRect; AFrameWidth, AFrameHeight: integer); overload;

  procedure Bind(aTextureUnitIndex: integer=0);
  procedure Unbind;
end;

TArrayOfPTexture = array of PTexture;

{ TTexturedChar }

TTexturedChar = record
  Character: string;
  DecoratedCharSize,          // size of the character with outline and shadow
  OriginalCharSize: TSize;    // normal size of the character (as given by TBGRABitmap.TextSize())
  Texture: PTexture;
  function ModelArea: TRect; inline;
end;
PTexturedChar = ^TTexturedChar;

{ TTextureManager }

TTextureManager = class
private
  FParentScene: TOGLCScene;
  FCurrentActiveTextureID: GLuint;
  FCurrentTextureUnit: integer;
  FGL_TEXTURE_2DEnabled: boolean;
  FSrcBitmapDuplicated: TBGRABitmap;
  FCount: integer;
  function PrepareBitmap(aIma: TBGRABitmap; out aGL_Format: GLEnum): TBGRABitmap;
  procedure FinalizeBitmap;
  function CreateInstanceFromBitmap(aIma: TBGRABitmap): PTexture;
  function GetMaxTextureWidth: integer;
public
  Constructor Create(aParentScene: TOGLCScene);
  function Add(const aFilename: string): PTexture; overload;
  function Add(const aFilename: string; AFrameWidth, AFrameHeight: integer): PTexture; overload;
  function Add(aImage: TBGRABitmap): PTexture; overload;
  function Add(aImage: TBGRABitmap; AFrameWidth, AFrameHeight: integer): PTexture; overload;
  function AddEmpty(aWidth, aHeight: integer): PTexture;
  // create a texture from an svg file.
  // You can set aHeight=-1 to force TextureManager to calculate the right height to conserve aspect ratio.
  // Or you can set aWidth=-1 to force TextureManager to calculates the right width to conserve aspect ratio.
  // Both aWidth and aHeight can not be equal to -1.
  function AddFromSVG(const aSVGFilename: string; aWidth, aHeight: integer): PTexture; overload;
  function TextToTexture(aCaption: string; const aFont: TFontDescriptor; aFillTexture: TBGRABitmap=NIL): PTexture;


 // procedure Replace(ATexture: PTexture; const aFilename: string); overload;
  // Replace the texture image by a new one. Use only to replace the whole image.
  procedure Replace(ATexture: PTexture; aImage: TBGRABitmap); overload;

  procedure Delete(var aTexture: PTexture);

  // Select the OpenGL texture unit and bind the texture (only if necessary)
  procedure Bind(ATexture: PTexture; aTextureUnitIndex: integer=0);
  procedure UnbindTexture; // call 'glDisable( GL_TEXTURE_2D )' (only if necessary)

  property MaxTextureWidth: integer read GetMaxTextureWidth;
  property Count: integer read FCount;
  // return the ID of the texture actually active, 0 if none.
  property CurrentActiveTextureID: GLuint read FCurrentActiveTextureID;
  // return the texture unit selected with the current bounded texture.
  property CurrentTextureUnit: integer read FCurrentTextureUnit;
end;



{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TTexture }

procedure TTexture.SetSingleFrame;
begin
  SetFrameSize(TextureWidth, TextureHeight);
end;

procedure TTexture.SetFrameSize(AFrameWidth, AFrameHeight: integer);
begin
  if (TextureWidth = 0) or (TextureHeight = 0) then exit;
  if (AFrameWidth <= 0) or (AFrameHeight <= 0) or
     (AFrameWidth > TextureWidth) or
     (AFrameHeight > TextureHeight) then
  begin
    raise exception.Create('Invalid frame size');
    exit;
  end;

  ComputeOpenGLFramesCoord(TextureWidth div AFrameWidth, TextureHeight div AFrameHeight);
  FrameWidth := AFrameWidth;
  FrameHeight := AFrameHeight;
end;

procedure TTexture.ComputeOpenGLFramesCoord(aXFrameCount: Integer; aYFrameCount: Integer);
var U, V, tx, ty, fU, fV: single;
  ix, iy, i: integer;
begin
  U := 1;
  V := 1;

  if aXFrameCount < 1 then aXFrameCount := 1;
  if aYFrameCount < 1 then aYFrameCount := 1;
  FrameCount := aXFrameCount * aYFrameCount;

  if FrameCount > 1
    then FrameCount := FrameCount + 1; // because frame[0] is whole texture, frame index start at 1
  SetLength(FramesCoord, FrameCount);

  FramesCoord[ 0, cTL ].X := 0;
  FramesCoord[ 0, cTL ].Y := 1;
  FramesCoord[ 0, cTR ].X := U;
  FramesCoord[ 0, cTR ].Y := 1;
  FramesCoord[ 0, cBR ].X := U;
  FramesCoord[ 0, cBR ].Y := 1 - V;
  FramesCoord[ 0, cBL ].X := 0;
  FramesCoord[ 0, cBL ].Y := 1 - V;

  if FrameCount = 1 then exit;

  fU := U / aXFrameCount;
  fV := V / aYFrameCount;

  i := 1;
  for iy := 1 to aYFrameCount do
   for ix := 1 to aXFrameCount do
    begin
      ty := 1 - fV * ( iy - 1 );
      tx := ( ix - 1 ) * fU;

      FramesCoord[ i, cTL ].X := tx;
      FramesCoord[ i, cTL ].Y := ty;

      FramesCoord[ i, cTR ].X := tx + fU;
      FramesCoord[ i, cTR ].Y := ty;

      FramesCoord[ i, cBR ].X := tx + fU;
      FramesCoord[ i, cBR ].Y := ty - fV;

      FramesCoord[ i, cBL ].X := tx;
      FramesCoord[ i, cBL ].Y := ty - fV;

      inc( i );
    end;
end;

function TTexture.FrameXYToUV(aPt: TPointF; aFrameIndex: integer): TPointF;
begin
  Result.x := aPt.x / FrameWidth + FramesCoord[aFrameIndex,cBL].x;
  Result.y := aPt.y / FrameHeight + FramesCoord[aFrameIndex,cBL].y;
end;

function TTexture.GetTopLeftCoor(aFrameIndex: integer): TPointF;
begin
  Result.x := FramesCoord[aFrameIndex,cTL].x * TextureWidth;
  Result.y := (1.0 - FramesCoord[aFrameIndex,cTL].y) * TextureHeight;
end;

function TTexture.GetBottomRightCoor(aFrameIndex: integer): TPointF;
begin
  Result.x := FramesCoord[aFrameIndex,cBR].x * TextureWidth;
  Result.y := (1.0 - FramesCoord[aFrameIndex,cBR].y) * TextureHeight;
end;

function TTexture.GetRect(aFrameIndex: integer): TRect;
begin
  Result.TopLeft := GetTopLeftCoor(aFrameIndex).Round;
  Result.BottomRight := GetBottomRightCoor(aFrameIndex).Round;
end;

procedure TTexture.TakePartOf(aTexture: PTexture; aRect: TRect);
var fw, fh, originU, originV: single;
begin
  FParentScene := aTexture^.FParentScene;
  TextureWidth := aTexture^.TextureWidth;
  TextureHeight := aTexture^.TextureHeight;
  FrameWidth := aRect.Width;
  FrameHeight := aRect.Height;
  FrameCount := 1;
  ID := aTexture^.ID;
  IDOwner := FALSE;

  fw := aRect.Width / aTexture^.TextureWidth;
  fh := aRect.Height / aTexture^.TextureHeight;
  originU := aRect.Left / aTexture^.TextureWidth;
  originV := 1 - aRect.Top / aTexture^.TextureHeight;

  SetLength(FramesCoord, 1);

  FramesCoord[ 0, cTL ].X := originU;
  FramesCoord[ 0, cTL ].Y := originV;

  FramesCoord[ 0, cTR ].X := originU + fw;
  FramesCoord[ 0, cTR ].Y := originV;

  FramesCoord[ 0, cBR ].X := originU + fw;
  FramesCoord[ 0, cBR ].Y := originV - fh;

  FramesCoord[ 0, cBL ].X := originU;
  FramesCoord[ 0, cBL ].Y := originV - fh;
end;

procedure TTexture.TakePartOf(aTexture: PTexture; aRect: TRect; AFrameWidth, AFrameHeight: integer);
var originX, originY, fw, fh, fx, fy: single;
 fcx, fcy, i, ix, iy: integer;
begin
  if (aRect.Width mod AFrameWidth <> 0) or
     (aRect.Height mod AFrameHeight <> 0)
     then Raise Exception.Create('Rect size don''t match AFrame size');

  FParentScene := aTexture^.FParentScene;
  TextureWidth := aTexture^.TextureWidth;
  TextureHeight := aTexture^.TextureHeight;
  ID := aTexture^.ID;
  IDOwner := FALSE;

  FrameWidth := AFrameWidth;
  FrameHeight := AFrameHeight;

  fcx := aRect.Width div AFrameWidth;
  fcy := aRect.Height div AFrameHeight;
  FrameCount := fcx*fcy;
  if FrameCount > 1 then inc(FrameCount); // because frame[0] is whole texture
  SetLength(FramesCoord, FrameCount);

  fw := AFrameWidth / aTexture^.TextureWidth;
  fh := AFrameHeight / aTexture^.TextureHeight;
  originY := 1 - aRect.Top / aTexture^.TextureHeight;
  originX := aRect.Left / aTexture^.TextureWidth;

  FramesCoord[ 0, cTL ].X := originX;
  FramesCoord[ 0, cTL ].Y := originY;

  FramesCoord[ 0, cTR ].X := originX + fw*fcx;
  FramesCoord[ 0, cTR ].Y := originY;

  FramesCoord[ 0, cBR ].X := originX + fw*fcx;
  FramesCoord[ 0, cBR ].Y := originY - fh*fcy;

  FramesCoord[ 0, cBL ].X := originX;
  FramesCoord[ 0, cBL ].Y := originY - fh*fcy;
  if FrameCount = 1 then exit;

  i := 1;
  for iy := 0 to fcy-1 do
   for ix := 0 to fcx-1 do
    begin
      fy := originY - fh * iy;
      fx := originX + ix * fw;

      FramesCoord[ i, cTL ].X := fx;
      FramesCoord[ i, cTL ].Y := fy;

      FramesCoord[ i, cTR ].X := fx + fw;
      FramesCoord[ i, cTR ].Y := fy;

      FramesCoord[ i, cBR ].X := fx + fw;
      FramesCoord[ i, cBR ].Y := fy - fh;

      FramesCoord[ i, cBL ].X := fx;
      FramesCoord[ i, cBL ].Y := fy - fh;

      inc( i );
    end;
end;

procedure TTexture.Bind(aTextureUnitIndex: integer);
begin
  FParentScene.TexMan.Bind(@Self, aTextureUnitIndex);
end;

procedure TTexture.Unbind;
begin
  FParentScene.TexMan.UnbindTexture;
end;

function TTexture.GetFrameCount: TSize;
begin
  if (FrameWidth > 0) and (FrameHeight > 0) then begin
    Result.cx := TextureWidth div FrameWidth;
    Result.cy := TextureHeight div FrameHeight;
  end else begin
    Result.cx := 0;
    Result.cy := 0;
  end;
end;

{ TTexturedChar }

function TTexturedChar.ModelArea: TRect;
begin
  with Result do begin
    Left := 0;
    Top := 0;
    Right := Texture^.FrameWidth;
    Bottom := Texture^.FrameHeight;
  end;
end;

{ TTextureManager }

constructor TTextureManager.Create(aParentScene: TOGLCScene);
begin
  FParentScene := aParentScene;
  FCurrentActiveTextureID := 0;
  FGL_TEXTURE_2DEnabled := FALSE;
  FCurrentTextureUnit := -1;
end;

function TTextureManager.CreateInstanceFromBitmap(aIma: TBGRABitmap): PTexture;
var dup: TBGRABitmap;
  glFormat, err: GLenum;
begin
  dup := PrepareBitmap(aIma, glFormat);

  new(Result);
  Result^.FParentScene := FParentScene;
  with Result^ do begin
    TextureWidth := dup.Width;
    TextureHeight := dup.Height;
    FrameWidth := dup.Width;
    FrameHeight := dup.Height;
    FrameCount := 1;
    IDOwner := TRUE;

    glGenTextures(1, @ID);
  end;

  Bind(Result);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, dup.Width , dup.Height, 0,
               glFormat, GL_UNSIGNED_BYTE, dup.Data);

  err := glGetError();
  if err <> GL_NO_ERROR then begin
    FParentScene.LogError('TextureManager: error while assigning data to a texture. GL ERROR $'+IntToHex(err, 4)+' '+GLErrorToString(err));
    FParentScene.LogMess('texture glID: '+Result^.ID.ToString+' width: '+dup.Width.ToString+' height: '+dup.Height.ToString, 1);
  end;

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);   // GL_LINEAR      GL_NEAREST
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  FinalizeBitmap;
  inc(FCount);
end;

function TTextureManager.PrepareBitmap(aIma: TBGRABitmap; out aGL_Format: GLEnum): TBGRABitmap;
begin
  if aIma.LineOrder = riloTopToBottom then begin
    Result := aIma.Duplicate;
    Result.VerticalFlip;
    FSrcBitmapDuplicated := Result;
  end else Result := aIma;

  if TBGRAPixel_RGBAOrder then {%H-}aGL_Format := GL_RGBA
    else aGL_Format := GL_BGRA;
end;

procedure TTextureManager.FinalizeBitmap;
begin
  FSrcBitmapDuplicated.Free;
  FSrcBitmapDuplicated := NIL;
end;

function TTextureManager.GetMaxTextureWidth: integer;
var res: GLint;
begin
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @res);
  Result := res;
end;

function TTextureManager.Add(const aFilename: string): PTexture;
var ima: TBGRABitmap;
begin
  try
   ima := TBGRABitmap.Create(aFilename);
   Result := Add(ima);
   Result^.Filename := ExtractFilename(aFilename);
   ima.Free;
  except
   raise Exception.Create('Texture manager can''t create texture from file "'+aFilename+'"');
   Result := NIL;
  end;
end;

function TTextureManager.Add(const aFilename: string; AFrameWidth, AFrameHeight: integer): PTexture;
var ima: TBGRABitmap;
begin
  try
   ima := TBGRABitmap.Create(aFilename);
   Result := Add(ima, AFrameWidth, AFrameHeight);
   Result^.Filename := ExtractFilename(aFilename);
   ima.Free;
  except
   raise Exception.Create('Texture manager can''t create texture from file '+aFilename );
   Result := NIL;
  end;
end;

function TTextureManager.Add(aImage: TBGRABitmap): PTexture;
begin
  Result := CreateInstanceFromBitmap(aImage);
  Result^.SetSingleFrame;
  Result^.Filename := '';
end;

function TTextureManager.Add(aImage: TBGRABitmap; AFrameWidth, AFrameHeight: integer): PTexture;
begin
  Result := Add(aImage);
  Result^.SetFrameSize(AFrameWidth, AFrameHeight);
  Result^.Filename := '';
end;

function TTextureManager.AddEmpty(aWidth, aHeight: integer): PTexture;
begin
  New(Result);
  Result^.FParentScene := FParentScene;
  with Result^ do begin
    TextureWidth := aWidth;
    TextureHeight := aHeight;
    FrameWidth := aWidth;
    FrameHeight := aHeight;
    FrameCount := 1;
    IDOwner := TRUE;
    Filename := '';
    SetSingleFrame;
    glGenTextures(1, @ID);
    Self.Bind(Result);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, TextureWidth,
                 TextureHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, NIL);
  end;
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR); // Linear Filtering
  //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR); // Linear Filtering
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
end;

function TTextureManager.AddFromSVG(const aSVGFilename: string; aWidth, aHeight: integer): PTexture;
var ima: TBGRABitmap;
begin
  try
    ima := LoadBitmapFromSVG(aSVGFilename, aWidth, aHeight);
    Result := Add(ima);
    Result^.Filename := ExtractFilename(aSVGFilename);
    ima.Free;
  except
    raise Exception.Create('Texture manager can''t create texture from SVG file '+aSVGFilename );
    Result := NIL;
  end;
end;

function TTextureManager.TextToTexture(aCaption: string; const aFont: TFontDescriptor; aFillTexture: TBGRABitmap): PTexture;
var ima: TBGRABitmap;
begin
  ima := aFont.StringToBitmap(aCaption, aFillTexture);
  Result := Add(ima);
  Result^.Filename := '';
  ima.Free;
end;

procedure TTextureManager.Replace(ATexture: PTexture; aImage: TBGRABitmap);
var dup: TBGRABitmap;
  glFormat: GLenum;
begin
  if ATexture = NIL then exit;

  dup := PrepareBitmap(aImage, glFormat);
  Bind(ATexture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, dup.Width , dup.Height, 0,
               glFormat, GL_UNSIGNED_BYTE, dup.Data);
  FinalizeBitmap;
end;

procedure TTextureManager.Delete(var aTexture: PTexture);
begin
  if aTexture = NIL then exit;

  if aTexture^.IDOwner then begin
    glDeleteTextures(1, @aTexture^.ID);
    dec(FCount);
  end;
  SetLength(aTexture^.FramesCoord, 0);
  Dispose(aTexture);
  aTexture := NIL;
end;

procedure TTextureManager.Bind(ATexture: PTexture; aTextureUnitIndex: integer);
begin
  if ATexture = NIL then exit;

  if not FGL_TEXTURE_2DEnabled then begin
    FGL_TEXTURE_2DEnabled := TRUE;
    glEnable(GL_TEXTURE_2D);
  end;

  if (FCurrentActiveTextureID <> ATexture^.ID) or (FCurrentTextureUnit <> aTextureUnitIndex) then begin
    glActiveTexture(GL_TEXTURE0+aTextureUnitIndex);
    glBindTexture(GL_TEXTURE_2D, aTexture^.ID);
    FCurrentActiveTextureID := aTexture^.ID;
    FCurrentTextureUnit := aTextureUnitIndex;
  end;
end;

procedure TTextureManager.UnbindTexture;
begin
  if not FGL_TEXTURE_2DEnabled then exit;
  glDisable(GL_TEXTURE_2D);
  FGL_TEXTURE_2DEnabled := FALSE;
  FCurrentActiveTextureID := 0;
end;


{$endif oglcIMPLEMENTATION}


