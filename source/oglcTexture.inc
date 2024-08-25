{$ifdef oglcINTERFACE}
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
  FrameCount: integer;
  FramesCoord: array of TQuadCoor;  // [0] is whole texture, [1] is first frame, [2] is second frame, ...

  procedure SetSingleFrame;
  procedure SetFrameSize(AFrameWidth, AFrameHeight: integer);
  function GetFrameCount: TSize;

  procedure TakePartOf(aTexture: PTexture; aRect: TRect); overload;
  procedure TakePartOf(aTexture: PTexture; aRect: TRect; AFrameWidth, AFrameHeight: integer); overload;

  function GivePart(aRect: TRect): PTexture; overload;
  function GivePart(aRect: TRect; AFrameWidth, AFrameHeight: integer): PTexture; overload;

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

  // return a part of a texture.
  function GivePart(aTexture: PTexture; aRect: TRect): PTexture;
  // return part of a texture with multiple frames.
  function GivePart(aTexture: PTexture; aRect: TRect; AFrameWidth, AFrameHeight: integer): PTexture;

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

procedure TTexture.TakePartOf(aTexture: PTexture; aRect: TRect);
var fw, fh, originX, originY: single;
begin
  FParentScene := aTexture^.FParentScene;
  TextureWidth := aRect.Width;
  TextureHeight := aRect.Height;
  FrameWidth := aRect.Width;
  FrameHeight := aRect.Height;
  FrameCount := 1;
  ID := aTexture^.ID;
  IDOwner := FALSE;

  fw := aTexture^.FrameWidth/aTexture^.TextureWidth;
  fh := aTexture^.FrameHeight/aTexture^.TextureHeight;
  originY := 1-aRect.Top/aTexture^.TextureHeight;    // + 1-TexCoor[0]
  originX := aRect.Left/aTexture^.TextureWidth;      // + TexCoor[0]

  SetLength(FramesCoord, 1);

  FramesCoord[ 0, cTL ].X := originX;
  FramesCoord[ 0, cTL ].Y := originY;

  FramesCoord[ 0, cTR ].X := originX + fw;
  FramesCoord[ 0, cTR ].Y := originY;

  FramesCoord[ 0, cBR ].X := originX + fw;
  FramesCoord[ 0, cBR ].Y := originY - fh;

  FramesCoord[ 0, cBL ].X := originX;
  FramesCoord[ 0, cBL ].Y := originY - fh;
end;

function TTexture.GivePart(aRect: TRect): PTexture;
var fw, fh, originX, originY: single;
begin
  new(Result);

  Result^.FParentScene := FParentScene;
  Result^.TextureWidth := aRect.Width;
  Result^.TextureHeight := aRect.Height;
  Result^.FrameWidth := aRect.Width;
  Result^.FrameHeight := aRect.Height;
  Result^.FrameCount := 1;
  Result^.ID := ID;
  Result^.IDOwner := FALSE;
  Result^.Filename := '';

  fw := Self.FrameWidth/Result^.TextureWidth;
  fh := Self.FrameHeight/Result^.TextureHeight;
  originY := 1-aRect.Top/Result^.TextureHeight;    // + 1-TexCoor[0]
  originX := aRect.Left/Result^.TextureWidth;      // + TexCoor[0]

  SetLength(Result^.FramesCoord, 1);

  Result^.FramesCoord[ 0, cTL ].X := originX;
  Result^.FramesCoord[ 0, cTL ].Y := originY;

  Result^.FramesCoord[ 0, cTR ].X := originX + fw;
  Result^.FramesCoord[ 0, cTR ].Y := originY;

  Result^.FramesCoord[ 0, cBR ].X := originX + fw;
  Result^.FramesCoord[ 0, cBR ].Y := originY - fh;

  Result^.FramesCoord[ 0, cBL ].X := originX;
  Result^.FramesCoord[ 0, cBL ].Y := originY - fh;
end;

procedure TTexture.TakePartOf(aTexture: PTexture; aRect: TRect; AFrameWidth, AFrameHeight: integer);
var originX, originY, fw, fh, fx, fy: single;
 fcx, fcy, i, ix, iy: integer;
begin
  if (aRect.Width mod AFrameWidth <> 0) or
     (aRect.Height mod AFrameHeight <> 0)
     then Raise Exception.Create('Rect size don''t match AFrame size');

  FParentScene := aTexture^.FParentScene;
  TextureWidth := aRect.Width;
  TextureHeight := aRect.Height;
  ID := aTexture^.ID;
  IDOwner := FALSE;

  FrameWidth := AFrameWidth;
  FrameHeight := AFrameHeight;

  fcx := aRect.Width div AFrameWidth;
  fcy := aRect.Height div AFrameHeight;
  FrameCount := fcx*fcy;
  if FrameCount > 1 then inc(FrameCount); // because frame[0] is whole texture
  SetLength(FramesCoord, FrameCount);

  fw := AFrameWidth/aTexture^.TextureWidth;
  fh := AFrameHeight/aTexture^.TextureHeight;
  originY := 1-aRect.Top/aTexture^.TextureHeight;
  originX := aRect.Left/aTexture^.TextureWidth;

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

function TTexture.GivePart(aRect: TRect; AFrameWidth, AFrameHeight: integer): PTexture;
var originX, originY, fw, fh, fx, fy: single;
 fcx, fcy, i, ix, iy: integer;
begin
  if (aRect.Width mod AFrameWidth <> 0) or
     (aRect.Height mod AFrameHeight <> 0)
     then Raise Exception.Create('Rect size don''t match AFrame size');
  new(Result);

  Result^.FParentScene := FParentScene;
  Result^.TextureWidth := aRect.Width;
  Result^.TextureHeight := aRect.Height;
  Result^.ID := ID;
  Result^.IDOwner := FALSE;
  Result^.Filename := '';

  Result^.FrameWidth := AFrameWidth;
  Result^.FrameHeight := AFrameHeight;

  fcx := aRect.Width div AFrameWidth;
  fcy := aRect.Height div AFrameHeight;
  Result^.FrameCount := fcx*fcy;
  if Result^.FrameCount > 1 then inc(Result^.FrameCount); // because frame[0] is whole texture
  SetLength(Result^.FramesCoord, Result^.FrameCount);

  fw := AFrameWidth/Self.TextureWidth;
  fh := AFrameHeight/Self.TextureHeight;
  originY := 1-aRect.Top/Self.TextureHeight;
  originX := aRect.Left/Self.TextureWidth;

  Result^.FramesCoord[ 0, cTL ].X := originX;
  Result^.FramesCoord[ 0, cTL ].Y := originY;

  Result^.FramesCoord[ 0, cTR ].X := originX + fw;
  Result^.FramesCoord[ 0, cTR ].Y := originY;

  Result^.FramesCoord[ 0, cBR ].X := originX + fw;
  Result^.FramesCoord[ 0, cBR ].Y := originY - fh;

  Result^.FramesCoord[ 0, cBL ].X := originX;
  Result^.FramesCoord[ 0, cBL ].Y := originY - fh;
  if Result^.FrameCount = 1 then exit;

  i := 1;
  for iy := 0 to fcy-1 do
   for ix := 0 to fcx-1 do
    begin
      fy := originY - fh * iy;
      fx := originX + ix * fw;

      Result^.FramesCoord[ i, cTL ].X := fx;
      Result^.FramesCoord[ i, cTL ].Y := fy;

      Result^.FramesCoord[ i, cTR ].X := fx + fw;
      Result^.FramesCoord[ i, cTR ].Y := fy;

      Result^.FramesCoord[ i, cBR ].X := fx + fw;
      Result^.FramesCoord[ i, cBR ].Y := fy - fh;

      Result^.FramesCoord[ i, cBL ].X := fx;
      Result^.FramesCoord[ i, cBL ].Y := fy - fh;

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
   raise Exception.Create('Texture manager can''t create texture from file '+aFilename );
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

function TTextureManager.GivePart(aTexture: PTexture; aRect: TRect): PTexture;
begin
  if aTexture = NIL then Raise Exception.Create('aTexture can not be NIL');
  Result := aTexture^.GivePart(aRect);
end;

function TTextureManager.GivePart(aTexture: PTexture; aRect: TRect; AFrameWidth, AFrameHeight: integer): PTexture;
begin
  if (aRect.Width mod AFrameWidth <> 0) or
     (aRect.Height mod AFrameHeight <> 0)
     then Raise Exception.Create('Texture Manager: aRect size don''t match with (AFrameWidth,AFrameHeight)...');
  if aTexture = NIL then Raise Exception.Create('aTexture can not be NIL');

  Result := aTexture^.GivePart(aRect, AFrameWidth, AFrameHeight);
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


