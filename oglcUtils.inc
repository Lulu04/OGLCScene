{$ifdef oglcINTERFACE}

function BGRAPixelToHex(aColor: TBGRAPixel): string;
function HexToBGRAPixel(const s: string): TBGRAPixel;
function PercentColor( const aColor : TBGRAPixel ;aPercent:single ) :TBGRAPixel;

// returns a time formated string HH:MM:SS.xx
// if aShowFractionalPart is set to FALSE the .xx part is removed
function SecondsToString( aTime: single; aShowFractionalPart: boolean ): string;


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

// splits each words separated by the specified separator.
function SplitLineToStringArray( ALine: string; aCharSeparator: char ): ArrayOfString;
// splits each words of a text. #10(line feed) is considered as a line break
function SplitTextToWordArray( aText: string; aCharSeparator: char=' ' ): ArrayOfString;

procedure DrawTexture( aTexture: PTexture; AFrameIndex: integer; X, Y, aAngle: single; aAlpha: byte; ATint: TBGRAPixel ); overload;
procedure DrawTexture( aTexture: PTexture; AFrameIndex: integer; X, Y: single; Param: TSimpleSurfaceWithEffect; ALayerPercentOpacity: single=1.0 ); overload;

procedure DrawTexturePart( aTexture: PTexture; aScreenCoor, aTexCoor: PTexCoor;
                           aFlipH, aFlipV: boolean; aTint: TBGRAPixel;
                           aPercentOpacity: single);

procedure DrawStretchedTexture( aTexture: PTexture; AFrameIndex: integer;
                                TopLeft, TopRight, BottomRight, BottomLeft: TPointF;
                                aFlipH, aFlipV: boolean; aTint: TBGRAPixel;
                                aPercentOpacity: single);

procedure DrawBox(X, Y, Width, Height: single; const BorderColor: TBGRAPixel; w: single=1.0; aLayerPercentOpacity: single=1.0 ); overload;
procedure DrawBox(X, Y, Width, Height: single; const BorderColor, FillColor: TBGRAPixel; w: single=1.0; aLayerPercentOpacity: single=1.0 ); overload;

procedure FillBox(X, Y, Width, Height: single; FillColor: TBGRAPixel; const aLayerPercentOpacity: single=1.0 );
procedure FillBox(X, Y, Width, Height: single; const aTopLeftColor, aTopRightColor, aBottomRightColor, aBottomLeftColor: TBGRAPixel; const aLayerPercentOpacity: single=1.0 ); overload;

procedure DrawLine( x1, y1, x2, y2: single; Color: TBGRAPixel; w: single; const aLayerPercentOpacity: single=1.0 ); overload;
procedure DrawLine( P1, P2: TPointF; Color: TBGRAPixel; w: single; const aLayerPercentOpacity: single=1.0 ); overload;

procedure DrawPolygon( aPath:ArrayOfTPointF; Color: TBGRAPixel; w: single );

procedure DrawEllipse( aCenter: TPointF; aRadius1, aRadius2: single; Color: TBGRAPixel; w: single );

function ScaleValueToFitScene( aTexture: PTexture; AScene: TOGLCScene ): TPointF;
function ScaleValueToFitSurface( aTexture: PTexture; aTargetSurface: TSimpleSurfaceWithEffect ): TPointF;
function ScaleValueToFitRect( aRect: TRectF; aTargetSurface: TSimpleSurfaceWithEffect ): TPointF;

function Distance(aPt1, aPt2: TPointF): single;

function PointFToRectF( p1, p2: TPointF ): TRectF;

// return the appropriate GL_COLOR_ATTACHMENTx
// ie: aIndex=0  returns GL_COLOR_ATTACHMENT0
//     aIndex=8  returns GL_COLOR_ATTACHMENT8
// aIndex [0..15]
function IndexToColorAttachment( aIndex: integer ): GLenum;
// return the total color attachment available on your system
// the max value will be the returned value minus 1.
function ColorAttachmentCount: GLint;

// Activate/Deactivate the screen vertical blank synchonization
procedure SetVSync( AValue: boolean );

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

function SecondsToString(aTime: single; aShowFractionalPart: boolean): string;
var t,h,m,s: integer;
begin
 t := trunc(aTime);
 h := t div 3600;
 t:=t-h*3600;
 m := t div 60;
 s:=t-m*60;
 t := trunc(Frac(aTime)*100);
 Result:='';
 if h>0 then Result:=inttostr(h)+':';

 if m>0 then
  if m>10
   then Result+=inttostr(m)+':'
   else Result+='0'+inttostr(m)+':';

 if s<10 then Result+='0';
 Result+=inttostr(s);

 if aShowFractionalPart
   then if t<10
     then Result+='.0'+inttostr(t)
     else Result+='.'+inttostr(t);
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
var k, i: integer;
  function EndOfString: boolean;
  begin
   Result := k>Length(ALine);
  end;
  procedure SkipSeparator;
  begin
   repeat
    if EndOfString
      then exit
      else if ALine[k]=aCharSeparator
            then inc(k)
            else exit;
   until false;
  end;
  procedure GetWord;
  begin
   inc(i);
   SetLength( Result, i+1 );
   Result[i] := '';
   repeat
    Result[i] := Result[i]+ALine[k];
    inc(k);
    if EndOfString then exit;
   until ALine[k]=aCharSeparator;
  end;
begin
 SetLength( Result{%H-}, 0 );
 if Length( ALine ) = 0 then exit;
 i := -1;
 k := 1;
 repeat
  SkipSeparator;
  if not EndOfString then GetWord;
 until EndOfString;
end;

function SplitTextToWordArray(aText: string; aCharSeparator: char
  ): ArrayOfString;
var i, w: integer;
   function IsLineFeed: boolean;
   begin
    Result := aText[i]=#10;
   end;
   procedure AddNewLine;
   begin
    inc(w);
    SetLength(Result, w+1);
    Result[w] := '';
   end;
begin
 SetLength( Result, 0 );
 if Length( aText ) = 0 then exit;

 // prepare the string
 if aText[1] <> aCharSeparator
   then aText := aCharSeparator {%H-}+ aText;

 i := 1;
 w := -1;
 repeat
  if IsLineFeed then begin
    if Result[w]<>'' then AddNewLine;
    Result[w] := aText[i];
    AddNewLine;
  end else if aText[i] <> aCharSeparator
             then Result[w] := Result[w] + aText[i]
             else AddNewLine;
  inc ( i );
 until i>Length( aText );
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

procedure DrawTexturePart(aTexture: PTexture; aScreenCoor, aTexCoor: PTexCoor;
  aFlipH, aFlipV: boolean; aTint: TBGRAPixel; aPercentOpacity: single);
var
  quad : array[ 0..3 ] of TPointF;
  tci  : PTextureCoordIndex;
  i: integer;
  procedure DrawVertex ;
   begin
    glBegin( GL_QUADS );
      glTexCoord2fv( @aTexCoor^[ tci^[ 0 ] ] );  glVertex2fv( @quad[ 0 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 2 ] ] );  glVertex2fv( @quad[ 2 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
   end;
begin
 if aTexture = nil then exit;
 TextureManager.BindTexture( aTexture );

 quad[0] := aScreenCoor^[0];
 quad[1] := aScreenCoor^[1];
 quad[2] := aScreenCoor^[2];
 quad[3] := aScreenCoor^[3];

 // Flip
 i:=0;
 if aFlipH then inc( i );
 if aFlipV then inc( i, 2 );
 tci := @FLIP_TEXCOORD[ i ];

 if aTint.alpha < 255
   then begin
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
         glColor4ub(255,255,255,round(255*aPercentOpacity));
         DrawVertex;
        end;

 if aTint.alpha <> 0
   then begin  //Apply Tint
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
         glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
         glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
         glColor4ub( aTint.red, aTint.Green, aTint.Blue, round(aTint.alpha*aPercentOpacity));
         DrawVertex;
        end;
end;

procedure DrawStretchedTexture(aTexture: PTexture; AFrameIndex: integer;
                          TopLeft, TopRight, BottomRight, BottomLeft: TPointF;
                          aFlipH, aFlipV: boolean; aTint: TBGRAPixel;
                          aPercentOpacity: single);
var
  quad : array[ 0..3 ] of TPointF;
  tci  : PTextureCoordIndex;
  i: integer;
  TexCoord : TTexCoor;
  procedure DrawVertex;
   begin
    glBegin( GL_QUADS );
      glTexCoord2fv( @TexCoord[ tci^[ 0 ] ] );  glVertex2fv( @quad[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @TexCoord[ tci^[ 2 ] ] );  glVertex2fv( @quad[ 2 ] );
      glTexCoord2fv( @TexCoord[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
   end;
begin
// if aTexture = nil then exit;
// if aPercentOpacity=0 then exit;
 TextureManager.BindTexture( aTexture );

 AFrameIndex := EnsureRange( AFrameIndex, 0, aTexture^.FrameCount-1);
 TexCoord := aTexture^.FramesCoord[AFrameIndex];

 quad[0] := TopLeft;
 quad[1] := TopRight;
 quad[2] := BottomRight;
 quad[3] := BottomLeft;

 // Flip
 i:=0;
 if aFlipH then inc( i );
 if aFlipV then inc( i, 2 );
 tci := @FLIP_TEXCOORD[ i ];

 if aTint.alpha < 255
   then begin
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
         glColor4ub(255,255,255,round(255*aPercentOpacity));
         DrawVertex;
        end;

 if aTint.alpha <> 0
   then begin  //Apply Tint
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
         glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
         glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
         glColor4ub( aTint.red, aTint.Green, aTint.Blue, round(aTint.alpha*aPercentOpacity));
         DrawVertex;
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

function Distance(aPt1, aPt2: TPointF): single;
begin
 Result := sqrt(sqr(aPt2.x-aPt1.x)+sqr(aPt2.y-aPt1.y));
end;

function PointFToRectF(p1, p2: TPointF): TRectF;
var topleft, bottomright: TPointF;
begin
 if p1.x<p2.x then topleft.x:=p1.x else topleft.x:=p2.x;
 if p1.y<p2.y then topleft.y:=p1.y else topleft.y:=p2.y;

 if p1.x<p2.x then bottomright.x:=p2.x else bottomright.x:=p1.x;
 if p1.y<p2.y then bottomright.y:=p2.y else bottomright.y:=p1.y;

 Result.TopLeft:=topleft;
 Result.BottomRight:=bottomright;
end;

function IndexToColorAttachment(aIndex: integer): GLenum;
begin
{ case aIndex of
   1: Result:=GL_COLOR_ATTACHMENT1;
   2: Result:=GL_COLOR_ATTACHMENT2;
   3: Result:=GL_COLOR_ATTACHMENT3;
   4: Result:=GL_COLOR_ATTACHMENT4;
   5: Result:=GL_COLOR_ATTACHMENT5;
   6: Result:=GL_COLOR_ATTACHMENT6;
   7: Result:=GL_COLOR_ATTACHMENT7;
   8: Result:=GL_COLOR_ATTACHMENT8;
   9: Result:=GL_COLOR_ATTACHMENT9;
   10: Result:=GL_COLOR_ATTACHMENT10;
   11: Result:=GL_COLOR_ATTACHMENT11;
   12: Result:=GL_COLOR_ATTACHMENT12;
   13: Result:=GL_COLOR_ATTACHMENT13;
   14: Result:=GL_COLOR_ATTACHMENT14;
   15: Result:=GL_COLOR_ATTACHMENT15;
   else Result:= GL_COLOR_ATTACHMENT0;
 end;   }
 Result:= GL_COLOR_ATTACHMENT0+aIndex;
end;

function ColorAttachmentCount: GLint;
var c: GLint;
begin
 glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, @c);
 Result:=c;
end;


procedure SetVSync( AValue: boolean );
const
{$IFDEF Windows}
opengllib = 'opengl32.dll';
{$ELSE}
{$IFDEF OS2}
opengllib = 'opengl.dll';
{$ELSE OS2}
{$ifdef darwin}
opengllib = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib';
{$ELSE}
{$ifdef haiku}
opengllib = 'libGL.so';
{$else}
opengllib = 'libGL.so.1';
{$endif}
{$endif}
{$ENDIF OS2}
{$ENDIF}

{$MACRO ON}
{$IFDEF Windows}
{$DEFINE extdecl := stdcall}
{$ELSE}
{$DEFINE extdecl := cdecl}
{$ENDIF}
var
  HLib: TLibHandle;
  wglSwapInterval: function(interval: GLint): LongBool; extdecl;
begin
 HLib := DynLibs.LoadLibrary( opengllib );
 if HLib <> DynLibs.NilHandle then begin
  Pointer(wglSwapInterval) := DynLibs.GetProcedureAddress( HLib, PChar('wglSwapInterval'));
  if wglSwapInterval<>NIL then wglSwapInterval( Integer( AValue ));
  DynLibs.UnloadLibrary(HLib);
 end;
end;

{
function dlopen ( lpLibFileName : PAnsiChar) : HMODULE; stdcall; external 'kernel32.dll' name 'LoadLibraryA';
function dlclose( hLibModule : HMODULE ) : Boolean; stdcall; external 'kernel32.dll' name 'FreeLibrary';
//function dlsym  ( hModule : HMODULE; lpProcName : PAnsiChar) : Pointer; stdcall; external 'kernel32.dll' name 'GetProcAddress';
function wglGetProcAddress(proc: PAnsiChar): Pointer; stdcall; external 'opengl32.dll';
procedure SetVSync( AValue: boolean );
type
  TSwapIntervalFunc = function( interval: GLInt ): LongBool ; stdcall;
var oglLibrary: HMODULE=0;
    wglSwapInterval: function(interval: GLint): LongBool; stdcall;
  function gl_GetProc( const Proc : UTF8String ) : Pointer;
  begin
    Result := wglGetProcAddress( PAnsiChar( Proc ) );
    if Result = nil Then
      Result := wglGetProcAddress( PAnsiChar( Proc + 'ARB' ) );
    if Result = nil Then
      Result := wglGetProcAddress( PAnsiChar( Proc + 'EXT' ) );
  end;
begin
 oglLibrary := dlopen( 'opengl32.dll' {$IFDEF UNIX}, $001 {$ENDIF} );
 wglSwapInterval := TSwapIntervalFunc( gl_GetProc( 'wglSwapInterval' ));
 if wglSwapInterval<>NIL then wglSwapInterval( Integer( AValue ));
 dlclose( oglLibrary );
end;
}

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
  aTargetSurface: TSimpleSurfaceWithEffect): TPointF;
var rl,rh,r: single;
begin
 rl := aTargetSurface.Width / aTexture^.ImageWidth;
 rh := aTargetSurface.Height / aTexture^.ImageHeight;
 if rl < rh then r := rl else r := rh;
 Result := PointF(r, r);
end;

function ScaleValueToFitRect(aRect: TRectF;
  aTargetSurface: TSimpleSurfaceWithEffect): TPointF;
var rl,rh,r: single;
begin
 rl := aTargetSurface.Width / aRect.Width;
 rh := aTargetSurface.Height / aRect.Height;
 if rl < rh then r := rl else r := rh;
 Result := PointF(r, r);
end;


{$endif oglcIMPLEMENTATION}


