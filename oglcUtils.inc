{$ifdef oglcINTERFACE}

function BGRAPixelToHex(aColor: TBGRAPixel): string;
function HexToBGRAPixel(const s: string): TBGRAPixel;
function PercentColor( const aColor : TBGRAPixel ;aPercent:single ) :TBGRAPixel;


// return a TBGRABitmap from a text and font attributes
// aFillTexture is a texture used to fill the letters
// Fill prior use aFillTexture, then gradient, then font color
function TextToBitmap( aCaption: string; const aFont: TGuiFont;
                        aFillTexture: TBGRABitmap=NIL): TBGRABitmap;

// return a PTexture from a text and font attributes
function TextToTexture( aCaption: string; const aFont: TGuiFont;
                        aFillTexture: TBGRABitmap=NIL): PTexture;

// return a sprite from a text and font attributes
function TextToSprite( aCaption: string; const aFont: TGuiFont;
                        aFillTexture: TBGRABitmap=NIL): TSprite;


// return an array of sprite. One sprite for one character.
// UseChilds=TRUE  => Result[0] have all the others sprites as children. Children coordinate are initialized
// UseChilds=FALSE => all sprites are independent of each other.
function TextToLetterSprite( aCaption: string; aFont: TGuiFont;
                             aFillTexture: TBGRABitmap=NIL;
                             UseChilds: boolean=FALSE): TArrayOfSprite;


function SplitLineToStringArray( ALine: string; aCharSeparator: char ): ArrayOfString;

procedure DrawTexture( aTexture: PTexture; AFrameIndex: integer; X, Y, aAngle: single; aAlpha: byte; ATint: TBGRAPixel ); overload;
procedure DrawTexture( aTexture: PTexture; AFrameIndex: integer; X, Y: single; Param: TSimpleSurfaceWithEffect; ALayerPercentOpacity: single=1.0 ); overload;

procedure DrawBox(X, Y, Width, Height: single; const BorderColor: TBGRAPixel; w: single=1.0; aLayerPercentOpacity: single=1.0 ); overload;
procedure DrawBox(X, Y, Width, Height: single; const BorderColor, FillColor: TBGRAPixel; w: single=1.0; aLayerPercentOpacity: single=1.0 ); overload;

procedure FillBox(X, Y, Width, Height: single; FillColor: TBGRAPixel; const aLayerPercentOpacity: single=1.0 );
procedure FillBox(X, Y, Width, Height: single; const aTopLeftColor, aTopRightColor, aBottomRightColor, aBottomLeftColor: TBGRAPixel; const aLayerPercentOpacity: single=1.0 ); overload;

procedure DrawLine( x1, y1, x2, y2: single; Color: TBGRAPixel; w: single; const aLayerPercentOpacity: single=1.0 ); overload;
procedure DrawLine( P1, P2: TPointF; Color: TBGRAPixel; w: single; const aLayerPercentOpacity: single=1.0 ); overload;

procedure DrawPolygon( aPath:ArrayOfTPointF; Color: TBGRAPixel; w: single );

procedure DrawEllipse( aCenter: TPointF; aRadius1, aRadius2: single; Color: TBGRAPixel; w: single );

function ScaleValueToFitScene( aTexture: PTexture; AScene: TOGLCScene ): TPointF;
function ScaleValueToFitSurface( aTexture: PTexture; aSurface: TSimpleSurfaceWithEffect ): TPointF;

type
TBlendFuncSeparateProc = procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure SetBlendMode( ABlendMode: byte );

var
  _glBlendFuncSeparateInitialized: boolean = FALSE;
  _FglBlendIsEnabled: boolean = FALSE;
  _FglCurrentBlendMode: byte = $FF;

const
  deg2rad = 0.017453292;
  rad2deg = 57.29578049;


{$endif oglcINTERFACE}



{$ifdef oglcIMPLEMENTATION}



function BGRAPixelToHex(aColor: TBGRAPixel): string;
begin
 Result := inttoHex(aColor.red,2)+inttoHex(aColor.green,2)+inttoHex(aColor.blue,2)+inttoHex(aColor.alpha,2);
end;

function HexToBGRAPixel(const s: string): TBGRAPixel;
var ss: string;
begin
 ss := copy( s, 1, 2);
 Result.red:=Hex2Dec(ss);
 ss := copy( s, 3, 2);
 Result.green:=Hex2Dec(ss);
 ss := copy( s, 5, 2);
 Result.blue:=Hex2Dec(ss);
 ss := copy( s, 7, 2);
 Result.alpha:=Hex2Dec(ss);
end;

function PercentColor ( const aColor : TBGRAPixel ;aPercent:single ) :TBGRAPixel;
var b,g,r:integer ;
begin
 b := round(aColor.blue*aPercent); if b>255 then b:=255 else if b<0 then b:=0;
 g := round(aColor.green*aPercent); if g>255 then g:=255 else if g<0 then g:=0;
 r := round(aColor.red*aPercent); if r>255 then r:=255 else if r<0 then r:=0;
 Result.blue:=b;
 Result.green:=g;
 Result.red:=r;
 Result.alpha:=aColor.alpha;
end;

function GradientRecordToBGRAGradient( FG: TFontGradient ): TBGRAGradientScanner;
begin
 with FG do
 Result:=TBGRAGradientScanner.Create(c1, c2, GradientType, Origin, d1, gammaColorCorrection, Sinus);
end;


function TextToBitmap(aCaption: string; const aFont: TGuiFont; aFillTexture: TBGRABitmap): TBGRABitmap;
var xx,yy:single;
    renderer: TBGRATextEffectFontRenderer;
    ts: TSize;
    _ShadowOffsetX, _ShadowOffsetY, _ShadowRadius: integer;
    _OutLineWidth: single;
   grad: TBGRAGradientScanner;
begin
 renderer:= TBGRATextEffectFontRenderer.Create;
 with aFont do begin
    renderer.ShadowVisible := ShadowColor.alpha<>0;
    renderer.ShadowColor := ShadowColor;
    renderer.ShadowOffset.x := ShadowOffsetX;
    renderer.ShadowOffset.y := ShadowOffsetY;
    renderer.ShadowRadius := ShadowRadius;
    renderer.OutlineVisible := (OutLineColor.alpha>0) and (OutLineWidth>0);
    renderer.OutlineColor := OutLineColor;
    renderer.OuterOutlineOnly := True;
    renderer.OutlineWidth := OutLineWidth;

    Result := TBGRABitmap.Create( 1, 1 );
    Result.FontRenderer := renderer;
    Result.FontQuality:= fqFineAntialiasing;
    Result.FontName := FontName;
    Result.FontHeight := FontHeight;
    Result.FontStyle := Style;

    // force shadow param. to 0 if there is no shadow
    if renderer.ShadowVisible then begin
      _ShadowOffsetX:=ShadowOffsetX;
      _ShadowOffsetY:=ShadowOffsetY;
      _ShadowRadius:=ShadowRadius;
    end else begin
       _ShadowOffsetX:=0;
       _ShadowOffsetY:=0;
       _ShadowRadius:=0;
    end;
    // force outline param. to 0 if there is no outline
    if renderer.OutlineVisible
      then _OutLineWidth:=OutLineWidth
      else _OutLineWidth:=0;

    // size of the rendered font. try to take into account shadow and outline.
    ts := Result.TextSize(aCaption);

    Result.SetSize( ts.cx + round(_OutlineWidth)+abs( _ShadowOffsetX ){%H-}+_ShadowRadius*2,
                    ts.cy + round(_OutlineWidth)+abs( _ShadowOffsetY ){%H-}+_ShadowRadius*2 );

    xx := _OutlineWidth*0.5+_ShadowRadius;
    yy:= xx;
    if _ShadowOffsetX<0 then xx += Abs(_ShadowOffsetX);
    if _ShadowOffsetY<0 then yy += Abs(_ShadowOffsetY);

    Result.Fill( BGRAPixelTransparent );
    if aFillTexture<>NIL
      then Result.TextOut( xx, yy, aCaption, aFillTexture)
      else if UseGradient then begin
             grad:= GradientRecordToBGRAGradient( Gradient );
             Result.TextOut( xx, yy, aCaption, grad );
             grad.Free;
           end else Result.TextOut( xx, yy, aCaption, FontColor );

 end;
end;

function TextToTexture(aCaption: string; const aFont: TGuiFont;aFillTexture: TBGRABitmap=NIL): PTexture;
var ima: TBGRABitmap;
begin
 ima := TextToBitmap( aCaption, aFont, aFillTexture );
 Result := TextureManager.Add( ima );
 ima.Free;
end;

function TextToSprite(aCaption: string; const aFont: TGuiFont;
  aFillTexture: TBGRABitmap): TSprite;
var t: PTexture;
begin
 t := TextToTexture(aCaption, aFont, aFillTexture );
 Result := TSprite.Create( t, TRUE );
end;

function TextToLetterSprite(aCaption: string; aFont: TGuiFont;
  aFillTexture: TBGRABitmap; UseChilds: boolean): TArrayOfSprite;
var i: integer;
begin
 SetLength( Result, UTF8Length( aCaption ));

 for i:=0 to Length( Result )-1 do begin
   Result[i] := TextToSprite( UTF8Copy(aCaption, i+1, 1 ), aFont, aFillTexture );
   if UseChilds and (i>0) then begin
     Result[i].SetCoordinate( Result[i-1].Width, 0 );
     Result[0].AddChild( Result[i] );
   end;
 end;
end;

function SplitLineToStringArray(ALine: string; aCharSeparator: char): ArrayOfString;
var i, w: integer;
begin
 SetLength( Result, 0 );
 if Length( ALine ) = 0 then exit;

 if ALine[1] <> aCharSeparator
   then ALine := aCharSeparator + ALine;

 i := 1;
 w := -1;
 repeat
  if ALine[i] <> aCharSeparator
    then Result[w] := Result[w] + ALine[i]
    else begin
          inc ( w );
          SetLength( Result, w + 1 );
          Result[w] := '';
    end;
  inc ( i );
 until i > Length( ALine );
end;


procedure DrawTexture(aTexture: PTexture; AFrameIndex: integer; X, Y, aAngle: single; aAlpha: byte; ATint: TBGRAPixel);
var
  quad : array[ 0..3 ] of TPointF;
  TexCoord : TTexCoor;
  AlphaTint: byte;
  x1, x2 : Single;
  y1, y2 : Single;
  cX, cY : Single;
  c, s   : Single;
  aX, aY, aW, aH : Single;
  procedure DrawVertex;
   begin
    glBegin( GL_QUADS );
      glTexCoord2fv( @TexCoord[0] );  glVertex2fv( @quad[ 0 ] );
      glTexCoord2fv( @TexCoord[1] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @TexCoord[2] );  glVertex2fv( @quad[ 2 ] );
      glTexCoord2fv( @TexCoord[3] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
   end;
begin
 if aTexture = nil then exit;

 if AFrameIndex < 0
   then AFrameIndex := 0
   else if AFrameIndex >= aTexture^.FrameCount
          then AFrameIndex := aTexture^.FrameCount-1;

 TexCoord := aTexture^.FramesCoord[AFrameIndex];

 aX := X;
 aY := Y;
 if AFrameIndex > 0
   then begin
    aW := aTexture^.FrameWidth;
    aH := aTexture^.FrameHeight;
   end else begin
    aW := aTexture^.ImageWidth;
    aH := aTexture^.ImageHeight;
   end;

 if aAngle <> 0
   then begin
         x1 := -aW / 2;
         y1 := -aH / 2;
         x2 := -x1;
         y2 := -y1;
         cX :=  aX + x2;
         cY :=  aY + y2;
         sincos( aAngle * deg2rad, s, c );
         quad[ 0 ].X := x1 * c - y1 * s + cX; quad[ 0 ].Y := x1 * s + y1 * c + cY;
         quad[ 1 ].X := x2 * c - y1 * s + cX; quad[ 1 ].Y := x2 * s + y1 * c + cY;
         quad[ 2 ].X := x2 * c - y2 * s + cX; quad[ 2 ].Y := x2 * s + y2 * c + cY;
         quad[ 3 ].X := x1 * c - y2 * s + cX; quad[ 3 ].Y := x1 * s + y2 * c + cY;
        end
   else begin
         quad[ 0 ].X := aX; quad[ 0 ].Y := aY;
         quad[ 1 ].X := aX + aW; quad[ 1 ].Y := aY;
         quad[ 2 ].X := aX + aW; quad[ 2 ].Y := aY + aH;
         quad[ 3 ].X := aX; quad[ 3 ].Y := aY + aH;
        end;

// glEnable(GL_BLEND);
// glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

 TextureManager.BindTexture( aTexture );

 if ATint.alpha < 255
   then begin
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
         glColor4ub( 255, 255, 255, aAlpha );
         DrawVertex;
        end;

 if ATint.alpha <> 0
   then begin  //Apply Tint
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
         glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
         glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
         AlphaTint:=round( ATint.alpha * aAlpha / 255 );
         glColor4ub( ATint.red, ATint.Green, ATint.Blue, AlphaTint);
         DrawVertex;
        end;

 TextureManager.DisableTextureUsage;
 {$IFDEF DEBUG_MODE_ON}
   DrawBox(X, Y, aTexture^.TextureWidth, aTexture^.TextureHeight, BGRA(255,0,0) );
 {$ENDIF}
end;

procedure DrawTexture(aTexture: PTexture; AFrameIndex: integer; X, Y: single; Param: TSimpleSurfaceWithEffect; ALayerPercentOpacity: single);
var
  quad : array[ 0..3 ] of TPointF;
  tci  : PTextureCoordIndex;
  TexCoord : TTexCoor;

  x1, x2 : Single;
  y1, y2 : Single;
  cX, cY : Single;
  c, s   : Single;
  AlphaTint:byte;
  i:integer;
  aX, aY, aW, aH : Single ;
  procedure DrawVertex ;
   begin
    glBegin( GL_QUADS );
      glTexCoord2fv( @TexCoord[ tci^[ 0 ] ] );  glVertex2fv( @quad[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @TexCoord[ tci^[ 2 ] ] );  glVertex2fv( @quad[ 2 ] );
      glTexCoord2fv( @TexCoord[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
   end;
begin
 if aTexture = nil then exit;

 if AFrameIndex < 0
   then AFrameIndex := 0
   else if AFrameIndex >= aTexture^.FrameCount
          then AFrameIndex := aTexture^.FrameCount-1;

 TexCoord := aTexture^.FramesCoord[AFrameIndex];

 aX := X;
 aY := Y;

 with Param do
  begin
            if AFrameIndex>0
              then begin
                    aW := Width;
                    aH := Height;
              end else begin
                    aW := aTexture^.ImageWidth;
                    aH := aTexture^.ImageHeight;
              end;

            // Scale
            if (Scale.x.Value <> 1) or (Scale.y.Value <> 1) Then
              begin
                aX := aX + ( aW - aW * Scale.x.Value ) / 2;
                aY := aY + ( aH - aH * Scale.y.Value ) / 2;
                aW := aW * Scale.x.Value;
                aH := aH * Scale.y.Value;
              end;
            // Flip
            i:=0;
            if FFlipH then inc( i );
            if FFlipV then inc( i, 2 );
            tci := @FLIP_TEXCOORD[ i ];

            if Angle.Value <> 0
              then begin
                    x1 := -aW / 2;
                    y1 := -aH / 2;
                    x2 := -x1;
                    y2 := -y1;
                    cX :=  aX + x2;
                    cY :=  aY + y2;
                    sincos( Angle.Value * deg2rad, s, c );
                    quad[ 0 ].X := x1 * c - y1 * s + cX; quad[ 0 ].Y := x1 * s + y1 * c + cY;
                    quad[ 1 ].X := x2 * c - y1 * s + cX; quad[ 1 ].Y := x2 * s + y1 * c + cY;
                    quad[ 2 ].X := x2 * c - y2 * s + cX; quad[ 2 ].Y := x2 * s + y2 * c + cY;
                    quad[ 3 ].X := x1 * c - y2 * s + cX; quad[ 3 ].Y := x1 * s + y2 * c + cY;
                   end
              else begin
                    quad[ 0 ].X := aX; quad[ 0 ].Y := aY;
                    quad[ 1 ].X := aX + aW; quad[ 1 ].Y := aY;
                    quad[ 2 ].X := aX + aW; quad[ 2 ].Y := aY + aH;
                    quad[ 3 ].X := aX; quad[ 3 ].Y := aY + aH;
                   end;
            // Skew
            quad[0].X += Skew.x.Value;
            quad[0].Y += Skew.y.Value;
            quad[1].X += Skew.x.Value;
            quad[1].Y += Skew.y.Value;

     //       glEnable(GL_BLEND);
     //       glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

            TextureManager.BindTexture( aTexture );

            if Tint.alpha.Value < 255
              then begin
                    glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
                    glColor4ub(255,255,255,round(Opacity.Value * aLayerPercentOpacity));
                    DrawVertex;
                   end;

            if Tint.alpha.Value <> 0
              then begin  //Apply Tint
                    glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
                    glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
                    glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
                    AlphaTint:=round( Tint.alpha.Value * Opacity.Value / 255 * aLayerPercentOpacity );
                    glColor4ub( round(Tint.red.Value), round(Tint.Green.Value), round(Tint.Blue.Value), AlphaTint);
                    DrawVertex;
                   end;

      //      TextureManager.DisableTextureUsage;
            {$IFDEF DEBUG_MODE_ON}
              DrawBox(X.Value, Y.Value, Width, Height, BGRA(255,0,0));
            {$ENDIF}
  end;
end;


procedure DrawBox(X, Y, Width, Height: single; const BorderColor: TBGRAPixel; w: single; aLayerPercentOpacity: single);
begin
 X += 0.5;
 Y += 0.5;

 if (width=0) or (Height=0) then exit;

 glLineWidth( w );
 glColor4ub( BorderColor.red, BorderColor.green, BorderColor.blue, round(BorderColor.alpha*aLayerPercentOpacity) );
 glBegin( GL_LINES );
   glVertex2f( X, Y );
   glVertex2f( X + Width, Y );

   glVertex2f( X + Width, Y );
   glVertex2f( X + Width, Y + Height );

   glVertex2f( X + Width, Y + Height );
   glVertex2f( X, Y + Height );

   glVertex2f( X, Y + Height );
   glVertex2f( X, Y );
 glEnd;
end;

procedure DrawBox(X, Y, Width, Height: single; const BorderColor, FillColor: TBGRAPixel; w: single; aLayerPercentOpacity: single);
begin
 FillBox( X, Y, Width, Height, FillColor, aLayerPercentOpacity );
 DrawBox(X, Y, Width, Height, BorderColor, w, aLayerPercentOpacity );
end;

procedure FillBox(X, Y, Width, Height: single; FillColor: TBGRAPixel; const aLayerPercentOpacity : single);
begin
 if (width=0) or (Height=0) then exit;
 glColor4ub( FillColor.red, FillColor.green, FillColor.blue, round(FillColor.alpha*aLayerPercentOpacity) );
 glBegin( GL_QUADS );
   glVertex2f( X, Y );
   glVertex2f( X + Width, Y );
   glVertex2f( X + Width, Y + Height );
   glVertex2f( X, Y + Height );
 glEnd;
end;

procedure FillBox(X, Y, Width, Height: single; const aTopLeftColor, aTopRightColor, aBottomRightColor,
  aBottomLeftColor: TBGRAPixel; const aLayerPercentOpacity: single);
begin
 if (width=0) or (Height=0) then exit;
 glBegin( GL_QUADS );
   with aTopLeftColor do glColor4ub( red, green, blue, round(alpha*aLayerPercentOpacity) );
   glVertex2f( X, Y );
   with aTopRightColor do glColor4ub( red, green, blue, round(alpha*aLayerPercentOpacity) );
   glVertex2f( X + Width, Y );
   with aBottomRightColor do glColor4ub( red, green, blue, round(alpha*aLayerPercentOpacity) );
   glVertex2f( X + Width, Y + Height );
   with aBottomLeftColor do glColor4ub( red, green, blue, round(alpha*aLayerPercentOpacity) );
   glVertex2f( X, Y + Height );
 glEnd;
end;

procedure DrawLine(x1, y1, x2, y2: single; Color: TBGRAPixel; w: single; const aLayerPercentOpacity: single);
begin
 glLineWidth( w );
 glEnable( GL_LINE_SMOOTH );
 glColor4ub( Color.red, Color.green, Color.blue, round(Color.alpha*aLayerPercentOpacity) );
 glBegin( GL_LINES );
   glVertex2f( x1 + 0.5, y1 + 0.5 );
   glVertex2f( x2 + 0.5, y2 + 0.5 );
 glEnd;
 glDisable( GL_LINE_SMOOTH );
end;

procedure DrawLine(P1, P2: TPointF; Color: TBGRAPixel;
  w:single; const aLayerPercentOpacity: single);
begin
 DrawLine( P1.x, P1.y, P2.x, P2.y, Color, w, aLayerPercentOpacity);
end;


procedure SetBlendMode(ABlendMode: byte);
var src : LongWord;
    dst : LongWord;
    procedure EnableglBlend; inline;
    begin
     if not _FglBlendIsEnabled
       then glEnable( GL_BLEND );
     _FglBlendIsEnabled := TRUE;
    end;
    procedure DisableglBlend; inline;
    begin
     if _FglBlendIsEnabled
       then glDisable( GL_BLEND );
     _FglBlendIsEnabled := FALSE;
    end;
begin
 if _FglCurrentBlendMode = ABlendMode then exit;
 case ABlendMode of

   FX_BLEND_ADD: begin
       src := GL_SRC_ALPHA;
       dst := GL_ONE;
       EnableglBlend;
   end;

   FX_BLEND_MULT: begin
       src := GL_ZERO;
       dst := GL_SRC_COLOR;
       EnableglBlend;
   end;

   FX_NOBLEND: DisableglBlend

   else begin  // normal blend
      src := GL_SRC_ALPHA;
      dst := GL_ONE_MINUS_SRC_ALPHA;
      EnableglBlend;
   end;
 end;
 if not _glBlendFuncSeparateInitialized then begin
   glBlendFuncSeparate := TBlendFuncSeparateProc( wglGetProcAddress( 'glBlendFuncSeparate' ));
   _glBlendFuncSeparateInitialized := true;
 end;
 if Assigned( glBlendFuncSeparate )
   then glBlendFuncSeparate( {%H-}src, {%H-}dst, GL_ONE, GL_ONE_MINUS_SRC_ALPHA )
   else glBlendFunc( src, dst );
 _FglCurrentBlendMode := ABlendMode;
end;

procedure DrawPolygon(aPath: ArrayOfTPointF; Color: TBGRAPixel; w: single);
var i: integer;
begin
 for i:=1 to Length( aPath )-1 do
  DrawLine( aPath[i-1], aPath[i], Color, w, 1 );
end;

procedure DrawEllipse(aCenter: TPointF; aRadius1, aRadius2: single;
  Color: TBGRAPixel; w: single);
var path: ArrayOfTPointF;
begin
 path := ComputeEllipse( aCenter.x, aCenter.y, aRadius1, aRadius2 );
 DrawPolygon( path, Color, w );
end;

function ScaleValueToFitScene( aTexture: PTexture; AScene: TOGLCScene): TPointF;
var rl,rh,r: single;
begin
 rl := AScene.Width / aTexture^.ImageWidth;
 rh := AScene.Height / aTexture^.ImageHeight;
 if rl < rh then r := rl else r := rh;
 Result := PointF(r, r);
end;

function ScaleValueToFitSurface(aTexture: PTexture;
  aSurface: TSimpleSurfaceWithEffect): TPointF;
var rl,rh,r: single;
begin
 rl := aSurface.Width / aTexture^.ImageWidth;
 rh := aSurface.Height / aTexture^.ImageHeight;
 if rl < rh then r := rl else r := rh;
 Result := PointF(r, r);
end;

{$endif oglcIMPLEMENTATION}


