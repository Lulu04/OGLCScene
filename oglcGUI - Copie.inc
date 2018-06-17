{$ifdef oglcINTERFACE}
type
{ TMouseManager }

TMouseManager = class
 Constructor Create ;
 Destructor Destroy; override ;
protected
 FTexture: PTexture;
 FSprite : TSprite;
 FParentScene : TOGLCScene;
 FMousePos : TPointF;
 procedure UpDate( AElapsedTime: single );
 procedure Draw;
public
 ClickPointOffset: TPointF; // (0;0)=Top/Left  (0.5;0.5)= Center  (0;1)=Bottom/Left ...
 LeftButton: boolean;
 RightButton: boolean;
 MiddleButton: boolean;
 procedure SetCursor( const aFilename : string ); overload;
 procedure SetCursor( const aFilename : string; aFrameWidth, aFrameHeight: integer ); overload;
 procedure SetCursor( aImage : TBGRABitmap ); overload;
 procedure SetCursor( aImage : TBGRABitmap; aFrameWidth, aFrameHeight: integer ); overload;
 procedure DeleteCursorSprite;
 property MouseSprite: TSprite read FSprite ; // for accessing all fx methods and property of TSprite
 property MousePosition: TPointF read FMousePos; // in scene coordinates
end;

var
   MouseManager : TMouseManager;

type
{ TGuiFont }

TGuiFont = record
 FontName: string;
 FontHeight: integer;
 Style: TFontStyles;
 FontColor,
 OutLineColor: TBGRAPixel;
 OutLineWidth: single;
 ShadowColor: TBGRAPixel;
 ShadowOffsetX,
 ShadowOffsetY,
 ShadowRadius: integer;
end;
PGuiFont = ^TGuiFont;

function GuiFont(aFontName:string; aFontHeight:integer; aStyle:TFontStyles;
                 aFontColor, aOutLineColor: TBGRAPixel; aOutLineWidth:single;
                 aShadowColor: TBGRAPixel; aShadowOffsetX, aShadowOffsetY, aShadowRadius: integer ): TGuiFont;
const
  DefaultFont: TGuiFont=( FontName:''; FontHeight:12; Style:[];FontColor:(blue:255;green:255;red:255;alpha:255);
                          OutLineColor:(blue:0;green:0;red:0;alpha:0); OutLineWidth: 1.0;
                          ShadowColor:(blue:0;green:0;red:0;alpha:0); ShadowOffsetX:0;
                          ShadowOffsetY:0; ShadowRadius:0);

type
TGuiEvent = procedure( aGUISurface:TSimpleSurfaceWithEffect ) of object;

{ TGuiClickableObject }

TGuiClickableObject = class( TSprite )
private
  FMouseEntered: boolean;
  FOnMouseLeave,
  FOnMouseEnter,
  FOnClick: TGuiEvent;
  procedure Init; override;
  procedure DoOnMouseEnter; virtual;
  procedure DoOnMouseLeave; virtual;
  procedure DoOnMouseClick; virtual;
private
  FFont: TGuiFont;
  FCaption: string;
  procedure SetCaption(AValue: string);
  procedure SetFont( AValue: TGuiFont );
protected
  procedure DoFontChange; virtual; // called when font change. Override to update the widget
  procedure DoCaptionChange; virtual; // called when caption change. Override to update the widget
public
  procedure Update( const aElapsedTime : single ); override;
public
 procedure Anim_OnMouseEnter; virtual; // override to animate the control when the mouse enters
 procedure Anim_OnMouseLeave; virtual; // override to animate the control when the mouse leaves
 procedure Anim_OnClick; virtual; // override to animate the control when the user clicks on it

 property OnMouseEnter : TGuiEvent read FOnMouseEnter write FOnMouseEnter;
 property OnMouseLeave : TGuiEvent read FOnMouseLeave write FOnMouseLeave;
 property OnClick      : TGuiEvent read FOnClick write FOnClick;

 property Font: TGuiFont read FFont write SetFont;
 property Caption: string read FCaption write SetCaption;
end;

{ TGuiButton }
TGuiButton = class( TGuiClickableObject )
protected
  procedure DoFontChange; override;
  procedure DoCaptionChange; override;
public
  procedure Anim_OnMouseEnter; override;
  procedure Anim_OnMouseLeave; override;
  procedure Anim_OnClick; override;
public
  Constructor Create( const aCaption: string; aFont: TGuiFont; aBackground: TBGRABitmap=NIL );
  Constructor Create( const aFilename : string );   // from file
  Constructor Create( aImage : TBGRABitmap );       // from BGRABitmap
  Constructor Create( const aCaption : string; aFont: TGuiFont; const aBackgroundFilename : string ); // text with background file
end;

{ TGuiLabel }
TGuiLabel = class ( TGuiClickableObject )
protected
  procedure DoFontChange; override;
  procedure DoCaptionChange; override;
public
  Constructor Create( aCaption: string; aFont: TGuiFont; aBackground: TBGRABitmap=NIL );
  property Caption: string read FCaption write SetCaption;
end;

{ TGuiCheck }
TGuiCheck = class ( TGuiButton )
protected
  procedure DoFontChange; override;
  procedure DoCaptionChange; override;
private
  FChecked: boolean;
  FOnChange: TGuiEvent;
  procedure SetChecked(AValue: boolean); virtual;
  procedure DoOnMouseClick; override;
public
  procedure Anim_OnMouseEnter; override;
  procedure Anim_OnMouseLeave; override;
  procedure Anim_OnClick; override;
  procedure Anim_OnChange; virtual;
public
  property Checked: boolean read FChecked write SetChecked;
  property OnChange: TGuiEvent read FOnChange write FOnChange;
end;

{ TGuiRadio }
TGuiRadio = class ( TGuiCheck )
protected
  procedure DoFontChange; override;
  procedure DoCaptionChange; override;
private
  procedure UncheckOtherOnParent;
  procedure SetChecked(AValue: boolean); override;
  procedure DoOnMouseClick; override;
end;


{ TGuiProgressBar }
TGuiProgressBarMode = ( sbmHorizontal, sbmVertical );

TGuiProgressBar = class( TColorBackground )
  Constructor Create( aX, aY: single; aWidth, aHeight : integer );
  Destructor Destroy; override;
private
  FMode: TGuiProgressBarMode;
  FPercent: single;
public
 procedure Update( const aElapsedTime : single ) ; override;
  procedure Draw( const aLayerPercentOpacity: single ); override;
public
  BackGroundColor,
  BorderColor: TBGRAParam;
  property Mode: TGuiProgressBarMode read FMode write FMode;
  property Percent: single read FPercent write FPercent; // [0..1]
end;



{ TGuiTextArea }

//TAlignment=( alLeft, alCenter, alRight, alJustify );
const oglcLineEnding=#13;
  type

TGuiTextArea = class( TGuiClickableObject )
  // word wrapped text in a rectangular area
  Constructor Create( const aText: string; aX, aY, aWidth, aHeight: integer;
                      aFont: TGuiFont; aAlignment : TAlignment=taCenter );
  Destructor Destroy; override;
private type
  TXYWord = record
             x          : integer ;
             y          : integer ;
             width      : integer ;
             IndexLine  : integer ;
            end;
private
  FImaTemp: TBGRABitmap;
  FWidth, FHeight: integer;
  FWord: TStringList;
  ArrayXYWord: array of TXYWord;
  FViewableLine: TStringList;
  FspaceWidth : integer;
  FAlign: TAlignment;
  procedure SplitCaptionInSeparateWords;
  procedure ProcessDrawing;
  procedure ProcessAlign;
  procedure PrepareImage;
  procedure ReleaseImage;

  procedure LeftAlign;
  procedure CenterAlign;
  procedure RightAlign;
  procedure JustifyAlign;
  procedure VerticalAlign;
  procedure FindWordIndex( aLine: integer; out iStart, iEnd: integer );
  procedure DrawText;
protected
  procedure DoFontChange; override;
  procedure DoCaptionChange; override;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

function GuiFont(aFontName: string; aFontHeight: integer;
  aStyle: TFontStyles; aFontColor, aOutLineColor: TBGRAPixel;
  aOutLineWidth: single; aShadowColor: TBGRAPixel; aShadowOffsetX, aShadowOffsetY,
  aShadowRadius: integer): TGuiFont;
begin
 with Result do begin
    FontName:= aFontName;
    FontHeight:=aFontHeight;
    Style:= aStyle;
    FontColor:=aFontColor;
    OutLineColor:=aOutLineColor;
    OutLineWidth:=aOutLineWidth;
    ShadowColor:=aShadowColor;
    ShadowOffsetX:=aShadowOffsetX;
    ShadowOffsetY:=aShadowOffsetY;
    ShadowRadius:=aShadowRadius;
 end;
end;

{ TGuiRadio }

procedure TGuiRadio.DoFontChange;
begin
 DoCaptionChange;
end;

procedure TGuiRadio.DoCaptionChange;
begin
 if FTexture<>NIL then TextureManager.Delete( FTexture );
 FTexture := TextToTexture( FCaption, FFont );
end;

procedure TGuiRadio.UncheckOtherOnParent;
var L, i: integer;
   o: TGuiRadio;
begin
 if FParentSurface<>NIL then begin
   // the control have a surface parent
   // -> we uncheck the other radio on this parent
   for i:=0 to FParentSurface.Childs.Count-1 do
    if TSimpleSurface(FParentSurface.Childs.Items[i]) is TGuiRadio then begin
      o := TSimpleSurface(FParentSurface.Childs.Items[i]) as TGuiRadio;
      if o.FChecked then begin
            o.FChecked:=FALSE;
            o.Anim_OnChange;
            //o.DoOnChange;
      end;
    end;
 end else begin
   // the control's parent is the Scene
   // -> we uncheck other radio without a parent
 for L:=0 to FParentScene.LayerCount-1 do
  for i:=0 to FParentScene.Layer[L].SurfaceCount-1 do
   if (FParentScene.Layer[L].Surface[i] is TGuiRadio) and
      (FParentScene.Layer[L].Surface[i].FParentSurface=NIL) and
      (o<>Self) then begin
     o := FParentScene.Layer[L].Surface[i] as TGuiRadio;
     if o.FChecked then begin
       o.FChecked:=FALSE;
       o.Anim_OnChange;
       //o.DoOnChange;
     end;
  end;
 end;
end;

procedure TGuiRadio.SetChecked(AValue: boolean);
begin
 if FChecked=AValue then Exit;
 if FChecked and not AValue then exit;
 UncheckOtherOnParent;

 FChecked:=AValue;
 Anim_OnChange;
end;

procedure TGuiRadio.DoOnMouseClick;
begin
 if FChecked then exit;
 UncheckOtherOnParent;

 FChecked := TRUE;
 Anim_OnClick;
 if FOnClick <> NIL then FOnClick( Self );
 Anim_OnChange;
 if FOnChange<>NIL then FOnChange( self );
end;

{ TGuiCheck }

procedure TGuiCheck.DoFontChange;
begin
 DoCaptionChange;
end;

procedure TGuiCheck.DoCaptionChange;
begin
 if FTexture<>NIL then TextureManager.Delete( FTexture );
 FTexture := TextToTexture( FCaption, FFont );
end;

procedure TGuiCheck.SetChecked(AValue: boolean);
begin
 if FChecked=AValue then Exit;
 FChecked:=AValue;
 Anim_OnChange;
end;

procedure TGuiCheck.DoOnMouseClick;
begin
 FChecked := not FChecked;
 inherited DoOnMouseClick;
 Anim_OnChange;
 if FOnChange<>NIL then FOnChange( self );
end;

procedure TGuiCheck.Anim_OnMouseEnter;
begin
 if not FChecked then begin
   Scale.ChangeTo( PointF(1.05,1.05), 0.3, idcStartFastEndSlow );
   Tint.Value := BGRA(255,255,255,60);
 end;
end;

procedure TGuiCheck.Anim_OnMouseLeave;
begin
 if not FChecked then begin
   Scale.ChangeTo( PointF(1,1), 0.3, idcStartFastEndSlow );
   Tint.Alpha.Value := 0;
 end;
end;

procedure TGuiCheck.Anim_OnClick;
begin
end;

procedure TGuiCheck.Anim_OnChange;
begin
 if FChecked then begin
   Scale.Value := PointF(0.97, 0.97);
   Scale.ChangeTo(PointF(1, 1), 0.3);
   Tint.Value := BGRA(255, 255, 150, 200)
 end else Tint.Alpha.ChangeTo(0, 0.3);
end;

{ TGuiProgressBar }

constructor TGuiProgressBar.Create(aX, aY: single; aWidth, aHeight: integer);
begin
 inherited Create( aX, aY, aWidth, aHeight );

 BackGroundColor := TBGRAParam.Create;
 BackGroundColor.Value := BGRA(10,5,3);

 BorderColor := TBGRAParam.Create;
 BorderColor.Value := BGRA(150,150,150);

 FMode := sbmHorizontal;
 FPercent := 0.0;
end;

destructor TGuiProgressBar.Destroy;
begin
 FreeAndNil( BackGroundColor );
 FreeAndNil( BorderColor );
 inherited Destroy;
end;

procedure TGuiProgressBar.Update(const aElapsedTime: single);
begin
 BackGroundColor.OnElapse( aElapsedTime );
 BorderColor.OnElapse( aElapsedTime );
 inherited Update(aElapsedTime);
end;

procedure TGuiProgressBar.Draw(const aLayerPercentOpacity: single);
var xx, yy, ww, hh: single;
begin
 if FMode = sbmHorizontal
   then begin
     yy := 0;
     hh := Height;
     if FFlipH
       then begin
        xx := 0;
        ww := Width * ( 1 - FPercent );
       end else begin
        xx := Width * FPercent;
        ww := Width - xx;
       end;
   end
   else begin
     xx := 0;
     ww := Width;
     if not FFlipV
       then begin
        yy := 0;
        hh := Height * ( 1 - FPercent );
       end else begin
        yy := Height * FPercent;
        hh := Height - yy;
       end;
   end;


 // Skew is ignored

 PushAndPrepareMatrix;
 SetBlendMode( FBlendMode );
 TextureManager.DisableTextureUsage;

 if Tint.alpha.Value < 255
   then begin
         FillBox( 0, 0, Width, Height, // fill entire bar
                  TopLeftColor.Value, TopRightColor.Value, BottomRightColor.Value,
                  BottomLeftColor.Value, aLayerPercentOpacity );
         FillBox( xx, yy, ww, hh, BackGroundColor.Value, aLayerPercentOpacity ); // background
         DrawBox( 0, 0, Width, Height, BorderColor.Value, aLayerPercentOpacity ); // border
   end;

 if Tint.alpha.Value <> 0
   then begin  //Apply Tint
         FillBox( 0, 0, Width, Height, Tint.Value, aLayerPercentOpacity * Opacity.Value / 255 );
        end;

 {$IFDEF DEBUG_MODE_ON}
   DrawBox(0, 0, Width, Height, BGRA(255,0,0));
 {$ENDIF}

 DrawChildSurfaces( aLayerPercentOpacity );
 PopMatrix;
end;

{ TGuiLabel }

procedure TGuiLabel.DoFontChange;
begin
 DoCaptionChange;
end;

procedure TGuiLabel.DoCaptionChange;
begin
 if FTexture<>NIL then TextureManager.Delete( FTexture );
 FTexture := TextToTexture( FCaption, FFont );
end;

constructor TGuiLabel.Create(aCaption: string; aFont: TGuiFont; aBackground: TBGRABitmap);
begin
 inherited Create( TextToTexture( aCaption, aFont, aBackground ), TRUE );
 FFont := aFont;
 FCaption := aCaption;
end;

{ TGuiButton }

procedure TGuiButton.DoFontChange;
begin
 DoCaptionChange;
end;

procedure TGuiButton.DoCaptionChange;
begin
 if FTexture<>NIL then TextureManager.Delete( FTexture );
 FTexture := TextToTexture( FCaption, FFont );
end;

procedure TGuiButton.Anim_OnMouseEnter;
begin
 Scale.ChangeTo( PointF(1.1,1.1), 0.3, idcStartFastEndSlow );
end;

procedure TGuiButton.Anim_OnMouseLeave;
begin
 Scale.ChangeTo( PointF(1,1), 0.3, idcStartFastEndSlow );
end;

procedure TGuiButton.Anim_OnClick;
begin
 Scale.Value := PointF(0.95, 0.95);
 Scale.ChangeTo(PointF(1.1, 1.1), 0.3);
 Tint.Value := BGRA(255, 255, 150, 200);
 Tint.Alpha.ChangeTo(0, 0.3);
end;

constructor TGuiButton.Create(const aCaption: string; aFont: TGuiFont;
  aBackground: TBGRABitmap);
begin
 inherited Create( TextToTexture( aCaption, aFont, aBackground), TRUE );
 FCaption := aCaption;
 FFont := aFont;
end;

constructor TGuiButton.Create(const aFilename: string);
var T: PTexture;
begin
 T := TextureManager.Add( aFilename );
 inherited Create ( T, TRUE );
end;

constructor TGuiButton.Create(aImage: TBGRABitmap);
var T: PTexture;
begin
 T := TextureManager.Add( aImage );
 inherited Create ( T );
end;

constructor TGuiButton.Create(const aCaption: string; aFont: TGuiFont; const aBackgroundFilename: string);
var ima, back: TBGRABitmap;
    T: PTexture;
begin
 if aBackgroundFilename=''
   then ima := TextToBitmap( aCaption, aFont )
   else begin
         back := TBGRABitmap.Create( aBackgroundFilename );
         ima := TextToBitmap( aCaption, aFont, back ) ;
         back.Free;
        end;
 T := TextureManager.Add( ima );
 inherited Create ( T, TRUE );
 ima.Free ;
 FCaption := aCaption;
 FFont := aFont;
end;

{ TGuiClickableObject }

procedure TGuiClickableObject.Init;
begin
 inherited Init;
 FMouseEntered := FALSE;
 FOnMouseEnter := NIL;
 FOnMouseLeave := NIL;
 FOnClick := NIL;
 FFont := DefaultFont;
end;

procedure TGuiClickableObject.DoOnMouseEnter;
begin
 Anim_OnMouseEnter;
 if FOnMouseEnter<>NIL then FOnMouseEnter ( Self );
end;

procedure TGuiClickableObject.DoOnMouseLeave;
begin
 Anim_OnMouseLeave;
 if FOnMouseLeave <> NIL then FOnMouseLeave( Self );
end;

procedure TGuiClickableObject.DoOnMouseClick;
begin
 Anim_OnClick;
 if FOnClick <> NIL then FOnClick( Self );
end;

procedure TGuiClickableObject.SetCaption(AValue: string);
begin
 if AValue=FCaption then exit;
 FCaption := AValue;
 DoCaptionChange;
end;

procedure TGuiClickableObject.SetFont(AValue: TGuiFont);
begin
 FFont := AValue;
 DoFontChange;
end;

procedure TGuiClickableObject.DoFontChange;
begin
end;

procedure TGuiClickableObject.DoCaptionChange;
begin
end;

procedure TGuiClickableObject.Update(const aElapsedTime: single);
begin
 inherited Update(aElapsedTime);

 if FFreeze or not FVisible then exit;
 if Opacity.Value=0 then exit;

 // Pool mouse activity
 if (MouseManager.FMousePos.x>=FSurfaceOrigin.x) and (MouseManager.FMousePos.x<=FSurfaceOrigin.x+Width) and
    (MouseManager.FMousePos.y>=FSurfaceOrigin.y) and (MouseManager.FMousePos.y<=FSurfaceOrigin.y+Height)
   then begin
         if not FMouseEntered
           then begin
                 FMouseEntered := TRUE;
                 DoOnMouseEnter;
                end
           else begin
                 if FParentScene.FFlagMouseLeftClicked
                   then begin
                         FParentScene.FFlagMouseLeftClicked := FALSE;
                         DoOnMouseClick;
                        end;
                end;
        end
   else begin
         if FMouseEntered
           then begin
                 FMouseEntered := FALSE;
                 DoOnMouseLeave;
           end;
        end;
end;

procedure TGuiClickableObject.Anim_OnMouseEnter;
begin
end;

procedure TGuiClickableObject.Anim_OnMouseLeave;
begin
end;

procedure TGuiClickableObject.Anim_OnClick;
begin
end;


{ TMouseManager }

constructor TMouseManager.Create;
begin
 FSprite := NIL;
 FTexture:= NIL;
end;

destructor TMouseManager.Destroy;
begin
 if FSprite <> NIL then FSprite.Free;
 if FTexture <> NIL then TextureManager.Delete( FTexture );
 inherited Destroy;
end;

procedure TMouseManager.DeleteCursorSprite;
begin
 if FTexture <> NIL then TextureManager.Delete( FTexture );
 FTexture := NIL;
 if FSprite <> NIL then FSprite.Free;
 FSprite := NIL;
 FParentScene.FOGLC.Cursor := crDefault;
end;

procedure TMouseManager.UpDate(AElapsedTime: single);
var p: TPoint;
begin
 p := FParentScene.FOGLC.ScreenToClient( Controls.Mouse.CursorPos );
 FMousePos.x := p.x;
 FMousePos.y := p.y;

 if FSprite = NIL then exit;
 FSprite.Update( AElapsedTime );
 FSprite.X.Value := FMousePos.x-ClickPointOffset.x*FSprite.Width;
 FSprite.Y.Value := FMousePos.y-ClickPointOffset.y*FSprite.Height;
end;

procedure TMouseManager.Draw;
begin
 if FSprite=NIL then exit;
 FSprite.Draw(1.0);
end;

procedure TMouseManager.SetCursor(const aFilename: string);
begin
 DeleteCursorSprite;

 FTexture := TextureManager.Add( aFilename );
 FSprite := TSprite.Create( FTexture );

 FParentScene.FOGLC.Cursor := crNone ;
end;

procedure TMouseManager.SetCursor(const aFilename: string; aFrameWidth, aFrameHeight: integer);
begin
 DeleteCursorSprite;

 FTexture := TextureManager.Add( aFilename, aFrameWidth, aFrameHeight );
 FSprite := TSprite.Create( FTexture );

 FParentScene.FOGLC.Cursor := crNone ;
end;

procedure TMouseManager.SetCursor(aImage: TBGRABitmap);
begin
 DeleteCursorSprite;

 FTexture := TextureManager.Add( aImage );
 FSprite := TSprite.Create( FTexture );

 FParentScene.FOGLC.Cursor := crNone ;
end;

procedure TMouseManager.SetCursor(aImage: TBGRABitmap; aFrameWidth, aFrameHeight: integer);
begin
 DeleteCursorSprite;

 FTexture := TextureManager.Add( aImage, aFrameWidth, aFrameHeight );
 FSprite := TSprite.Create( FTexture );

 FParentScene.FOGLC.Cursor := crNone ;
end;

{ TGuiTextArea }

constructor TGuiTextArea.Create(const aText: string; aX, aY, aWidth, aHeight: integer;
  aFont: TGuiFont; aAlignment: TAlignment);
begin
 FCaption := aText;
 FFont := aFont;
 FWidth := aWidth;
 FHeight := aHeight;
 FAlign := aAlignment;
 FWord := TStringList.Create;
 FViewableLine := TStringList.Create;

 SplitCaptionInSeparateWords;
 SetLength ( ArrayXYWord , FWord.Count );

 PrepareImage;
 ProcessDrawing;
 ProcessAlign;
 DrawText;

 inherited Create( TextureManager.Add(FImaTemp), TRUE );
 ReleaseImage;
 SetCoordinate ( aX, aY );
end;

procedure TGuiTextArea.ProcessDrawing;
var
  i  : integer;
  xx , yy : integer;
  wordWidth : integer;
  line : string;
  indexline : integer;
  TotalHeight, TotalWidth: integer;
begin
 FspaceWidth := FImaTemp.TextSize(' ').cx;

 FViewableLine.Clear;
 TotalHeight := 0;
 TotalWidth := 0;
 if FWord.Count = 0 then exit;

 line := '';
 xx := -FspaceWidth;
 yy := 0;
 wordWidth:=0;
 indexline := 1;
 for i:=1 to FWord.Count do
  begin
   if TotalWidth < xx then TotalWidth := xx;
   if FWord.Strings[i-1]=oglcLineEnding then begin
      // on passe à la ligne suivante
      if TotalWidth < xx then TotalWidth := xx;
      yy := yy + FFont.FontHeight;
      inc( indexline );

      ArrayXYWord[i-1].x := 0;
      ArrayXYWord[i-1].y := yy;
      ArrayXYWord[i-1].IndexLine := indexline;
      ArrayXYWord[i-1].Width := wordWidth;

      xx := wordWidth;
      TotalHeight := TotalHeight + FFont.FontHeight;
      FViewableLine.Add( line );
      line := '';
   end else begin
    wordWidth := FImaTemp.TextSize( SysToUTF8(FWord.Strings[i-1]) ).cx;
    if xx+FspaceWidth+wordWidth <= FWidth
      then begin // on peut rester sur la même ligne

            ArrayXYWord[i-1].x := xx + FspaceWidth;
            ArrayXYWord[i-1].y := yy;
            ArrayXYWord[i-1].IndexLine := indexline;
            ArrayXYWord[i-1].Width := wordWidth;

            xx := xx + FspaceWidth + wordWidth;
            if line='' then line := FWord.Strings[i-1]
                       else line := line + ' ' + FWord.Strings[i-1];
           end
      else begin  // on passe à la ligne suivante
            if TotalWidth < xx then TotalWidth := xx ;
            yy := yy + FFont.FontHeight;//+ FImaTemp.TextSize( SysToUTF8(FWord.Strings[i-1]) ).cy ;
            inc( indexline );
            ArrayXYWord[i-1].x := 0 ; ArrayXYWord[i-1].y := yy ; ArrayXYWord[i-1].IndexLine := indexline ; ArrayXYWord[i-1].Width := wordWidth ;
            xx := wordWidth ;
            TotalHeight := TotalHeight + FFont.FontHeight;//+ FImaTemp.TextSize( SysToUTF8(FWord.Strings[i-1]) ).cy ;
            FViewableLine.Add( line );
            line := FWord.Strings[i-1] ;
           end;
   end;
  end;
 if line <> ''
   then begin
         FViewableLine.Add ( line ) ;
         TotalHeight := TotalHeight + FFont.FontHeight;//+ FImaTemp.TextSize( SysToUTF8(line) ).cy ;
         if TotalWidth < xx then TotalWidth := xx ;
        end;
end;

procedure TGuiTextArea.ProcessAlign;
begin
 // word align
 case FAlign of
  taLeftJustify: LeftAlign;
  taCenter: CenterAlign;
  taRightJustify: RightAlign;
//  alJustify: JustifyAlign;
 end;//case aAlignement
// VerticalAlign;
end;

procedure TGuiTextArea.PrepareImage;
begin
 FImaTemp:=TBGRABitmap.Create(FWidth, FHeight, BGRAPixelTransparent);
 FImaTemp.FontName := FFont.FontName;
 FImaTemp.FontHeight := FFont.FontHeight;
 FImaTemp.FontStyle := FFont.Style;
end;

procedure TGuiTextArea.ReleaseImage;
begin
 FreeAndNil(FImaTemp);
end;


destructor TGuiTextArea.Destroy;
begin
 FWord.Free ;
 FViewableLine.Free ;
 SetLength ( ArrayXYWord, 0 ) ;
 inherited Destroy;
end;

procedure TGuiTextArea.SplitCaptionInSeparateWords;
var
 i : integer ;
 t : string ;
const aSeparator=' ';
begin
 FWord.Clear;
 if length( FCaption ) = 0 then exit;
 i := 1;
 t := '';
 repeat
  if FCaption[i]=oglcLineEnding then begin
   if t<>'' then FWord.Add(t);
   t:='';
   FWord.Add(oglcLineEnding);
  end else if FCaption[i] <> aSeparator
             then t := t + FCaption[i]
             else if (FCaption[i] = aSeparator) and (length(t) > 0) then begin
                    FWord.Add( t );
                    t := '';
                  end;
  inc (i);
 until i > length ( FCaption );
 if t <> '' then FWord.Add ( t );
end;

{
 procedure TGuiTextArea.SeparateTheWordsOfTheText(aTxt: string; temp: TStringList);
 var
  i : integer ;
  t : string ;
 const aSeparator=' ';
 begin
  temp.Clear;
  if length ( aTxt ) = 0 then exit;
  i := 1;
  t := '';
  repeat
   if aTxt[i] <> aSeparator then t := t + aTxt[i];
   if ( aTxt[i] = aSeparator ) and ( length ( t ) > 0  )
     then begin
           temp.Add ( t );
           t := '';
          end;
   inc (i);
  until i > length ( aTxt );
  if t <> '' then temp.Add ( t );
 end;
}

procedure TGuiTextArea.LeftAlign;
var i, j, w: integer;
  istart, iend: integer;
begin
 for i:=1 to FViewableLine.Count do
  begin
   FindWordIndex( i, istart, iend );
   w := 0 ;
   for j:=istart to iend do
    begin
     ArrayXYWord[j].x := w ;
     w := w + ArrayXYWord[j].Width + FspaceWidth;
    end;
  end;
end;

procedure TGuiTextArea.CenterAlign;
var i , j : integer ;
  w : integer ;
  istart , iend : integer ;
begin
 for i:=1 to FViewableLine.Count do
  begin
   FindWordIndex ( i , istart , iend ) ;
   w := ArrayXYWord[iend].x + ArrayXYWord[iend].Width - ArrayXYWord[istart].x ; // largeur totale de la ligne
   w := ( FWidth - w ) div 2 ;
   for j:=istart to iend do
    begin
     ArrayXYWord[j].x := w ;
     w := w + ArrayXYWord[j].Width + FspaceWidth;//FImaTemp.TextSize ( ' ' ).cx ;
    end;
  end;
end;

procedure TGuiTextArea.RightAlign;
var i , j : integer ;
  w : integer ;
  istart , iend : integer ;
begin
 for i:=1 to FViewableLine.Count do
  begin
   FindWordIndex ( i , istart , iend ) ;
   w := FWidth + FspaceWidth;//FImaTemp.TextSize ( ' ' ).cx ;
   for j:=iend downto istart do
    begin
     w := w - ArrayXYWord[j].Width - FspaceWidth;//FImaTemp.TextSize ( ' ' ).cx ;
     ArrayXYWord[j].x := w ;
    end;
  end;
end;

procedure TGuiTextArea.JustifyAlign;
begin

end;

procedure TGuiTextArea.VerticalAlign;
var i , j : integer ;
  istart , iend : integer ;
  h : integer ;
  yy : integer ;
begin
 h := 0 ;
 for i:=1 to FViewableLine.Count do h := h + FFont.FontHeight;
 yy := ( FHeight - h ) div 2 ;
 for i:=1 to FViewableLine.Count do
  begin
   FindWordIndex ( i , istart , iend ) ;
   for j:=istart to iend do ArrayXYWord[j].y := yy ;
   yy := yy + FFont.FontHeight;
  end;
end;

procedure TGuiTextArea.FindWordIndex(aLine: integer; out iStart, iEnd: integer);
var i : integer;
begin
 iStart := -1;
 iEnd := -1;
 for i:=low(ArrayXYWord) to high(ArrayXYWord) do
  begin
   if ArrayXYWord[i].IndexLine = aLine
     then begin
           if iStart = -1 then iStart := i;
           iEnd := i;
          end
     else if iStart <> -1 then exit;
  end;
end;

procedure TGuiTextArea.DrawText;
var i : integer ;
  xx , yy : single ;
  txt : string ;
  renderer: TBGRATextEffectFontRenderer;
begin
 renderer := TBGRATextEffectFontRenderer.Create;
 renderer.ShadowVisible := FFont.ShadowColor.alpha<>0;
 renderer.ShadowColor := FFont.ShadowColor;
 renderer.ShadowOffset.x := FFont.ShadowOffsetX;
 renderer.ShadowOffset.y := FFont.ShadowOffsetY;
 renderer.ShadowRadius := FFont.ShadowRadius;
 renderer.OutlineVisible := FFont.OutLineColor.alpha<>0;
 renderer.OutlineColor := FFont.OutLineColor;
 renderer.OuterOutlineOnly := True;
 renderer.OutlineWidth := FFont.OutLineWidth;

 FImaTemp.FontRenderer := renderer;
 FImaTemp.FontQuality:= fqFineAntialiasing;
 FImaTemp.FontHeight := FFont.FontHeight ;
 FImaTemp.FontName := FFont.FontName;
 FImaTemp.FontStyle := FFont.Style;
{ for i:=low(ArrayXYWord) to high(ArrayXYWord) do
  begin
   txt := FWord.Strings[i]; //SysToUTF8( FWord.Strings[i] );
   xx := ArrayXYWord[i].x;
   yy := ArrayXYWord[i].y;
   FImaTemp.TextOut ( xx, yy, txt, FFont.FontColor );
  end;  }
 FImaTemp.TextRect(Rect(0, 0, FWidth, FHeight),
                   FCaption, FAlign, tlBottom, FFont.FontColor);
end;

procedure TGuiTextArea.DoFontChange;
begin
 DoCaptionChange;
end;

procedure TGuiTextArea.DoCaptionChange;
begin
 SplitCaptionInSeparateWords;
 SetLength ( ArrayXYWord , FWord.Count );

 PrepareImage;
 ProcessDrawing;
 ProcessAlign;
 DrawText;

 if FTexture<>NIL then TextureManager.Delete(FTexture);
 FTexture := TextureManager.Add(FImaTemp);
 ReleaseImage;
end;





{$endif oglcIMPLEMENTATION}


