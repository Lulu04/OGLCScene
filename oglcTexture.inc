{$ifdef oglcINTERFACE}
type
PTexCoor = ^TTexCoor;
TTexCoor = array[0..3] of TPointF;

{ TTexture }
PTexture = ^TTexture;
TTexture = record
            ID            : GLuint   ;
            Owner         : boolean  ;
            TextureWidth  : integer  ;
            TextureHeight : integer  ;
            ImageWidth    : integer  ;
            ImageHeight   : integer  ;
            FrameWidth    : integer  ;
            FrameHeight   : integer  ;
            FrameCount    : integer  ;
            FramesCoord   : array of TTexCoor;  // [0] is whole texture, [1] is first frame, [2] is second, ...
            Next          : PTexture ;
            Prev          : PTexture ;
           end;

{ TTextureManager }
TOGLCGTextureManagerSpec = specialize TOGLCGList<TTexture>;

TTextureManager = class( TOGLCGTextureManagerSpec )
 Constructor Create;
private
 FCurrentBindedTexture : PTexture;
 FGL_TEXTURE_2DEnabled : boolean;
 function InitFromBGRABitmap( aIma: TBGRABitmap ): PTexture;
 procedure SetFrameSize( ATexture: PTexture; AFrameWidth, AFrameHeight: integer );
 procedure ComputeOpenGLFramesCoord(ATexture: PTexture; FramesX: Integer=1; FramesY: Integer=1);
public
 function Add( const aFilename: string ): PTexture; overload;
 function Add( const aFilename: string; AFrameWidth, AFrameHeight: integer ): PTexture; overload;
 function Add( aImage: TBGRABitmap ): PTexture; overload;
 function Add( aImage: TBGRABitmap; AFrameWidth, AFrameHeight: integer ): PTexture; overload;
 function AddEmpty( aWidth, aHeight: integer ): PTexture;

 procedure Replace( ATexture: PTexture; const aFilename : string ); overload;
 procedure Replace( ATexture: PTexture; aImage : TBGRABitmap ); overload;

 procedure Delete ( var aTexture: PTexture );

 // return a part of a texture from an existing one
 function GivePart( aTexture: PTexture; aRect: TRect ): PTexture;
 // return a framed part of a texture
 function GivePart( aTexture: PTexture; aRect: TRect; AFrameWidth, AFrameHeight: integer ): PTexture;

 procedure BindTexture( ATexture: PTexture );  // call 'glEnable( GL_TEXTURE_2D )' and 'glBindTexture(...)' only if necessary
 procedure DisableTextureUsage;                // call 'glDisable( GL_TEXTURE_2D )' only if necessary

 function GetMaxTextureWidth: integer;

 function GetFrameCount( ATexture: PTexture ): TSize;
 property Count: integer read FCount;
end;

var
 TextureManager : TTextureManager;
{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}
{ TTextureManager }

constructor TTextureManager.Create;
begin
 inherited Create;
 FCurrentBindedTexture := NIL;
 FGL_TEXTURE_2DEnabled := FALSE;
end;

function TTextureManager.InitFromBGRABitmap(aIma: TBGRABitmap ): PTexture;
var dup:TBGRABitmap;
 glFormat: GLenum;
begin
 dup := aIma.Duplicate as TBGRABitmap;

 if TBGRAPixel_RGBAOrder then
   {%H-}glFormat := GL_RGBA
 else
   glFormat := GL_BGRA;

 if dup.LineOrder = riloTopToBottom then
   dup.VerticalFlip;

 Result := AddInstanceToList;
 Result^.ImageWidth := dup.Width;
 Result^.ImageHeight := dup.Height;

 Result^.TextureWidth := dup.Width;
 Result^.TextureHeight := dup.Height;

 Result^.FrameWidth := dup.Width;
 Result^.FrameHeight := dup.Height;

 Result^.FrameCount := 1;

 Result^.Owner:=TRUE;

 glGenTextures(1, @Result^.ID );
 BindTexture( Result );
 glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, dup.Width , dup.Height, 0, glFormat, GL_UNSIGNED_BYTE, dup.Data );

 dup.Free;

 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR); // Linear Filtering
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR); // Linear Filteringend;
end;

procedure TTextureManager.ComputeOpenGLFramesCoord(ATexture: PTexture; FramesX: Integer; FramesY: Integer);
var U,V: Single;
  tx, ty, fU, fV : single;
  ix,iy,i: integer;
begin
  with ATexture^ do
  begin
    U := ImageWidth/TextureWidth;
    V := ImageHeight/TextureHeight;

    if FramesX < 1 then FramesX := 1;
    if FramesY < 1 then FramesY := 1;
    FrameCount := FramesX * FramesY;

    if FrameCount > 1
      then FrameCount := FrameCount + 1; // because frame[0] fit whole texture, frame index start at 1
    SetLength( FramesCoord, FrameCount );

    FramesCoord[ 0, 0 ].X := 0;
    FramesCoord[ 0, 0 ].Y := 1;
    FramesCoord[ 0, 1 ].X := U;
    FramesCoord[ 0, 1 ].Y := 1;
    FramesCoord[ 0, 2 ].X := U;
    FramesCoord[ 0, 2 ].Y := 1 - V;
    FramesCoord[ 0, 3 ].X := 0;
    FramesCoord[ 0, 3 ].Y := 1 - V;

    if FrameCount = 1 then exit;

    fU := U / FramesX;
    fV := V / FramesY;

    i := 1;
    for iy := 1 to FramesY do
     for ix := 1 to FramesX do
      begin
        ty := 1 - fV * ( iy - 1 );
        tx := ( ix - 1 ) * fU;

        FramesCoord[ i, 0 ].X := tx;
        FramesCoord[ i, 0 ].Y := ty;

        FramesCoord[ i, 1 ].X := tx + fU;
        FramesCoord[ i, 1 ].Y := ty;

        FramesCoord[ i, 2 ].X := tx + fU;
        FramesCoord[ i, 2 ].Y := ty - fV;

        FramesCoord[ i, 3 ].X := tx;
        FramesCoord[ i, 3 ].Y := ty - fV;

        inc( i );
      end;
  end;
end;

function TTextureManager.Add(const aFilename: string): PTexture;
var ima : TBGRABitmap ;
begin
 try
  ima := TBGRABitmap.Create( aFilename ) ;
 except
  raise Exception.Create('Texture manager can''t create texture from file '+aFilename );
  Result := NIL;
  ima.Free;
  exit;
 end;
 Result := InitFromBGRABitmap( ima );
 SetFrameSize( Result, Result^.ImageWidth, Result^.ImageHeight );
 ima.Free ;
end;

function TTextureManager.Add(const aFilename: string; AFrameWidth,
  AFrameHeight: integer): PTexture;
begin
 Result := Add( aFilename );
 SetFrameSize( Result, AFrameWidth, AFrameHeight );
end;

function TTextureManager.Add(aImage: TBGRABitmap): PTexture;
begin
 Result := InitFromBGRABitmap( aImage ) ;
 SetFrameSize( Result, Result^.ImageWidth, Result^.ImageHeight );
end;

function TTextureManager.Add(aImage: TBGRABitmap; AFrameWidth,
  AFrameHeight: integer): PTexture;
begin
 Result := Add( aImage );
 SetFrameSize( Result, AFrameWidth, AFrameHeight );
end;

function TTextureManager.AddEmpty(aWidth, aHeight: integer): PTexture;
begin
 Result := AddInstanceToList;
 Result^.ImageWidth := aWidth;
 Result^.ImageHeight := aHeight;

 Result^.TextureWidth := aWidth; //ValueOfNextPowerOfTwo( aWidth );
 Result^.TextureHeight := aHeight; //ValueOfNextPowerOfTwo( aHeight );

 Result^.FrameWidth := aWidth;
 Result^.FrameHeight := aHeight;

 Result^.FrameCount := 1;

 Result^.Owner:=TRUE;

 glGenTextures(1, @Result^.ID );
 BindTexture( Result );
 glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA{0x1908}, Result^.TextureWidth , Result^.TextureHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, NIL );

 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
 //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR); // Linear Filtering
 //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR); // Linear Filtering
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
 SetFrameSize( Result, Result^.ImageWidth, Result^.ImageHeight );
end;

procedure TTextureManager.Replace(ATexture: PTexture; const aFilename: string);
var ima : TBGRABitmap ;
begin
 try
  ima := TBGRABitmap.Create( aFilename ) ;
 except
  ima := TBGRABitmap.Create( 5, 5, BGRAWhite );
 end;
 Replace( ATexture, ima );
 ima.Free;
end;

procedure TTextureManager.Replace(ATexture: PTexture; aImage: TBGRABitmap);
begin
 BindTexture( ATexture );
 glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, aImage.Width , aImage.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, aImage.Data );
end;

procedure TTextureManager.SetFrameSize( ATexture: PTexture; AFrameWidth, AFrameHeight: integer );
begin
 if ATexture=NIL then exit;
 if (ATexture^.ImageWidth = 0) or (ATexture^.ImageHeight = 0) then exit;
 if (AFrameWidth <= 0) or (AFrameHeight <= 0) or
    (AFrameWidth > ATexture^.ImageWidth) or (AFrameHeight > ATexture^.ImageHeight) then
 begin
   raise exception.Create('Invalid frame size');
   exit;
 end;
 ComputeOpenGLFramesCoord(ATexture, ATexture^.ImageWidth div AFrameWidth, ATexture^.ImageHeight div AFrameHeight);

 ATexture^.FrameWidth:= AFrameWidth;
 ATexture^.FrameHeight:= AFrameHeight;

end;

procedure TTextureManager.Delete(var aTexture: PTexture);
begin
 if aTexture=NIL then raise Exception.Create('TextureManager: Try to delete a texture=NIL...');
 if aTexture^.Owner then glDeleteTextures( 1, @aTexture^.ID );
 SetLength( aTexture^.FramesCoord, 0 );
 DoDelete( aTexture );
 aTexture := NIL;
end;

function TTextureManager.GivePart(aTexture: PTexture; aRect: TRect): PTexture;
var fw, fh, fx, fy: single;
begin
 Result := AddInstanceToList;
 Result^.ImageWidth := aRect.Width; // aTexture^.ImageWidth;
 Result^.ImageHeight := aRect.Height;// aTexture^.ImageHeight;

 Result^.TextureWidth := aTexture^.TextureWidth;
 Result^.TextureHeight := aTexture^.TextureHeight;

 Result^.FrameWidth := aRect.Width;
 Result^.FrameHeight := aRect.Height;

 Result^.FrameCount := 1;

 Result^.ID:=aTexture^.ID;
 Result^.Owner:=FALSE;

 with Result^ do begin
   fw := aRect.Width/TextureWidth;
   fh := aRect.Height/TextureHeight;
   fy := 1-aRect.Top/TextureHeight;
   fx := aRect.Left/TextureWidth;

   SetLength( FramesCoord, FrameCount );

   FramesCoord[ 0, 0 ].X := fx;
   FramesCoord[ 0, 0 ].Y := fy;

   FramesCoord[ 0, 1 ].X := fx + fw;
   FramesCoord[ 0, 1 ].Y := fy;

   FramesCoord[ 0, 2 ].X := fx + fw;
   FramesCoord[ 0, 2 ].Y := fy - fh;

   FramesCoord[ 0, 3 ].X := fx;
   FramesCoord[ 0, 3 ].Y := fy - fh;
 end;
end;

function TTextureManager.GivePart(aTexture: PTexture; aRect: TRect; AFrameWidth, AFrameHeight: integer): PTexture;
var originX, originY, fw, fh, fx, fy: single;
 fcx, fcy, i, ix, iy: integer;
begin
 if (aRect.Width mod AFrameWidth<>0) or
    (aRect.Height mod AFrameHeight<>0)
    then Raise Exception.Create('Texture Manager: aRect size don''t match with (AFrameWidth,AFrameHeight)...');

 Result := GivePart( aTexture, aRect );

 fcx := aRect.Width div AFrameWidth;
 fcy := aRect.Height div AFrameHeight;
 if fcx*fcy=1 then exit;

 with Result^ do begin

   FrameWidth := AFrameWidth;
   FrameHeight := AFrameHeight;
   FrameCount := fcx*fcy+1;

   SetLength( FramesCoord, fcx*fcy+1);

   fw := FrameWidth/TextureWidth;
   fh := FrameHeight/TextureHeight;
   originX := aRect.Left/TextureWidth;
   originY := 1-aRect.Top/TextureHeight;

   i := 1;
   for iy := 0 to fcy-1 do
    for ix := 0 to fcx-1 do
     begin
       fy := originY - fh * iy;
       fx := originX + ix * fw;

       FramesCoord[ i, 0 ].X := fx;
       FramesCoord[ i, 0 ].Y := fy;

       FramesCoord[ i, 1 ].X := fx + fw;
       FramesCoord[ i, 1 ].Y := fy;

       FramesCoord[ i, 2 ].X := fx + fw;
       FramesCoord[ i, 2 ].Y := fy - fh;

       FramesCoord[ i, 3 ].X := fx;
       FramesCoord[ i, 3 ].Y := fy - fh;

       inc( i );
     end;
 end;
end;
{
with ATexture^ do
begin
  U := 1;
  V := 1;

  if FramesX < 1 then FramesX := 1;
  if FramesY < 1 then FramesY := 1;
  FrameCount := FramesX * FramesY;

  if FrameCount > 1
    then FrameCount := FrameCount + 1; // because frame[0] fit whole texture, frame index start at 1
  SetLength( FramesCoord, FrameCount );

  FramesCoord[ 0, 0 ].X := 0;
  FramesCoord[ 0, 0 ].Y := 1;
  FramesCoord[ 0, 1 ].X := 1;
  FramesCoord[ 0, 1 ].Y := 1;
  FramesCoord[ 0, 2 ].X := 1;
  FramesCoord[ 0, 2 ].Y := 0;
  FramesCoord[ 0, 3 ].X := 0;
  FramesCoord[ 0, 3 ].Y := 0;

  if FrameCount = 1 then exit;

  fU := U / FramesX;
  fV := V / FramesY;

  i := 1;
  for iy := 1 to FramesY do
   for ix := 1 to FramesX do
    begin
      ty := 1 - fV * ( iy - 1 );
      tx := ( ix - 1 ) * fU;

      FramesCoord[ i, 0 ].X := tx;
      FramesCoord[ i, 0 ].Y := ty;

      FramesCoord[ i, 1 ].X := tx + fU;
      FramesCoord[ i, 1 ].Y := ty;

      FramesCoord[ i, 2 ].X := tx + fU;
      FramesCoord[ i, 2 ].Y := ty - fV;

      FramesCoord[ i, 3 ].X := tx;
      FramesCoord[ i, 3 ].Y := ty - fV;

      inc( i );
    end;
end;
}

procedure TTextureManager.BindTexture(ATexture: PTexture);
begin
 if ATexture = NIL then exit;
 if not FGL_TEXTURE_2DEnabled
   then begin
         FGL_TEXTURE_2DEnabled := TRUE;
         glEnable( GL_TEXTURE_2D );
   end;
 if FCurrentBindedTexture <> aTexture
  then begin
        glBindTexture( GL_TEXTURE_2D, aTexture^.ID );
        FCurrentBindedTexture := aTexture;
  end;
end;

procedure TTextureManager.DisableTextureUsage;
begin
 if not FGL_TEXTURE_2DEnabled then exit;
 glDisable( GL_TEXTURE_2D );
 FGL_TEXTURE_2DEnabled := FALSE;
end;

function TTextureManager.GetMaxTextureWidth: integer;
var v:GLInt;
begin
 glGetIntegerv( GL_MAX_TEXTURE_SIZE, @v);
 Result := v;
end;

function TTextureManager.GetFrameCount(ATexture: PTexture): TSize;
begin
 if ATexture = NIL
    then begin
          Result.cx := 0;
          Result.cy := 0;
    end else begin
          Result.cx := ATexture^.ImageWidth div ATexture^.FrameWidth;
          Result.cy := ATexture^.ImageHeight div ATexture^.FrameHeight;
    end;
end;

{$endif oglcIMPLEMENTATION}


