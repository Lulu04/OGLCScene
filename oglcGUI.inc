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
TFontGradient = record
 c1,
 c2: TBGRAPixel;
 GradientType: TGradientType;
 Origin, d1: TPointF;
 gammaColorCorrection,
 Sinus: boolean;
end;

{ TGuiFont }

TGuiFont = record
 FontName: string;
 FontHeight: integer;
 Style: TFontStyles;
 OutLineColor: TBGRAPixel;
 OutLineWidth: single;
 ShadowColor: TBGRAPixel;
 ShadowOffsetX,
 ShadowOffsetY,
 ShadowRadius: integer;
 UseGradient: boolean;
 FontColor: TBGRAPixel;
 Gradient: TFontGradient;
end;
PGuiFont = ^TGuiFont;

// Give a filled TFontGradient record
function FontGradient(c1, c2: TBGRAPixel; AGradientType: TGradientType; AOrigin, d1: TPointF;
                      gammaColorCorrection: boolean = True; Sinus: Boolean=False): TFontGradient;
// Give a TGuiFont record (color version)
function GuiFont(aFontName:string; aFontHeight:integer; aStyle:TFontStyles;
                 aFontColor, aOutLineColor: TBGRAPixel; aOutLineWidth:single;
                 aShadowColor: TBGRAPixel; aShadowOffsetX, aShadowOffsetY, aShadowRadius: integer ): TGuiFont; overload;
// Give a TGuiFont record (gradient version)
function GuiFont(aFontName:string; aFontHeight:integer; aStyle:TFontStyles;
                 aFontGradient: TFontGradient;
                 aOutLineColor: TBGRAPixel; aOutLineWidth:single;
                 aShadowColor: TBGRAPixel; aShadowOffsetX, aShadowOffsetY, aShadowRadius: integer ): TGuiFont; overload;
// Give a default font (arial, black)
function DefaultFont: TGuiFont;

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

TGuiPanel = class( TColorBackground );

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
  Constructor Create( const aCaption: string; aFont: TGuiFont; aBackground: TBGRABitmap=NIL ); // from text
  Constructor Create( const aFilename : string );   // from file
  Constructor Create( aImage : TBGRABitmap );       // from BGRABitmap
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
TGuiProgressBarMode = ( pbmHorizontal, pbmVertical );

TGuiProgressBar = class( TColorBackground )
  Constructor Create( aX, aY: single; aWidth, aHeight : integer );
  Destructor Destroy; override;
private
  FMode: TGuiProgressBarMode;
  FPercent,
  FBorderWidth: single;
  procedure SetPercent(AValue: single);
public
 procedure Update( const aElapsedTime : single ) ; override;
  procedure Draw( const aLayerPercentOpacity: single ); override;
public
  BackGroundColor,
  BorderColor: TBGRAParam;
  property BorderWidth: single read FBorderWidth write FBorderWidth;
  property Mode: TGuiProgressBarMode read FMode write FMode;
  property Percent: single read FPercent write SetPercent; // [0..1]
end;


{ TGuiRadialProgressBar }

TGuiRadialProgressBar=class(TOGLCPathDrawing)
private
 FCircularPath: TOGLCPath;
 FMax: single;
 FPosition: single;
 procedure SetPosition(AValue: single);
 procedure UpdateDrawing;
public
 constructor Create( aCenter: TPointF; aRadius: single );
 destructor Destroy; override;
 // return TRUE if Position >= Max
 function Completed: boolean;
 property Max: single read FMax write FMax;
 property Position: single read FPosition write SetPosition;
end;




{ TGuiTextArea }

TGuiTextArea = class( TGuiClickableObject )
  // word wrapped text in a rectangular area (done by TBGRABitmap.TextRect method)
  Constructor Create( const aText: string; aX, aY, aWidth, aHeight: integer;
                      aFont: TGuiFont; aHorizAlign: TAlignment=taCenter;
                      aVerticalAlign: TTextLayout=tlCenter);
private
  FImaTemp: TBGRABitmap;
  FWidth, FHeight: integer;
  FHAlign: TAlignment;
  FVAlign: TTextLayout;
  procedure PrepareImage;
  procedure ReleaseImage;
protected
  procedure DoFontChange; override;
  procedure DoCaptionChange; override;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

function FontGradient(c1, c2: TBGRAPixel; AGradientType: TGradientType;
  AOrigin, d1: TPointF; gammaColorCorrection: boolean; Sinus: Boolean
  ): TFontGradient;
begin
 Result.c1:=c1;
 Result.c2:=c2;
 Result.GradientType:=AGradientType;
 Result.Origin:=AOrigin;
 Result.d1:=d1;
 Result.gammaColorCorrection:=gammaColorCorrection;
 Result.Sinus:=Sinus;
end;

function GuiFont(aFontName: string; aFontHeight: integer;
  aStyle: TFontStyles; aFontColor, aOutLineColor: TBGRAPixel;
  aOutLineWidth: single; aShadowColor: TBGRAPixel; aShadowOffsetX, aShadowOffsetY,
  aShadowRadius: integer): TGuiFont;
begin
 if aFontName='' then aFontName:='Arial';
 with Result do begin
    FontName:= aFontName;
    FontHeight:=aFontHeight;
    Style:= aStyle;
    FontColor:=aFontColor;
    UseGradient:=FALSE;
    OutLineColor:=aOutLineColor;
    OutLineWidth:=aOutLineWidth;
    ShadowColor:=aShadowColor;
    ShadowOffsetX:=aShadowOffsetX;
    ShadowOffsetY:=aShadowOffsetY;
    ShadowRadius:=aShadowRadius;
 end;
end;

function GuiFont(aFontName: string; aFontHeight: integer; aStyle: TFontStyles;
  aFontGradient: TFontGradient; aOutLineColor: TBGRAPixel;
  aOutLineWidth: single; aShadowColor: TBGRAPixel; aShadowOffsetX,
  aShadowOffsetY, aShadowRadius: integer): TGuiFont;
begin
 if aFontName='' then aFontName:='Arial';
 with Result do begin
    FontName:=aFontName;
    FontHeight:=aFontHeight;
    Style:= aStyle;
    FontColor:=BGRAPixelTransparent;
    Gradient:=aFontGradient;
    UseGradient:=TRUE;
    OutLineColor:=aOutLineColor;
    OutLineWidth:=aOutLineWidth;
    ShadowColor:=aShadowColor;
    ShadowOffsetX:=aShadowOffsetX;
    ShadowOffsetY:=aShadowOffsetY;
    ShadowRadius:=aShadowRadius;
 end;
end;

function DefaultFont: TGuiFont;
begin
 Result.FontName:='';
 Result.FontHeight:=12;
 Result.Style:=[];
 Result.FontColor:=BGRAWhite;
 Result.OutLineColor:=BGRABlack;
 Result.OutLineWidth:= 1.0;
 Result.ShadowColor:=BGRABlack;
 Result.ShadowOffsetX:=0;
 Result.ShadowOffsetY:=0;
 Result.ShadowRadius:=0;
 Result.UseGradient:=FALSE;
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
   for i:=0 to FParentSurface.ChildCount-1 do
    if FParentSurface.Child[i] is TGuiRadio then begin
      o := FParentSurface.Child[i] as TGuiRadio;
      if o.FChecked and (o<>Self) then begin
            o.FChecked:=FALSE;
            o.Anim_OnChange;
            //o.DoOnChange;
      end;
    end;
 end else begin
   // the control's parent is the Scene
   // -> we uncheck other radio without a parent
 for L:=0 to FParentScene.LayerCount-1 do
  for i:=0 to FParentScene.Layer[L].SurfaceCount-1 do begin
   o := FParentSurface.Child[i] as TGuiRadio;
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

 FMode := pbmHorizontal;
 FPercent := 0.0;
 FBorderWidth := 1.5;
end;

destructor TGuiProgressBar.Destroy;
begin
 FreeAndNil( BackGroundColor );
 FreeAndNil( BorderColor );
 inherited Destroy;
end;

procedure TGuiProgressBar.SetPercent(AValue: single);
begin
 if AValue>1.0 then AValue:=1.0;
 if AValue<0 then AValue:=0;
 FPercent:=AValue;
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
 if FMode = pbmHorizontal
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
         DrawBox( 0, 0, Width, Height, BorderColor.Value, FBorderWidth, aLayerPercentOpacity ); // border
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


{ TGuiRadialProgressBar }

procedure TGuiRadialProgressBar.SetPosition(AValue: single);
begin
 if FPosition=AValue then Exit;
 FPosition:=AValue;
 UpdateDrawing;
end;

procedure TGuiRadialProgressBar.UpdateDrawing;
var f: single;
begin
 if FMax=0
   then f:=0
   else f := FPosition/FMax;
 DistanceTraveled.Value := Path.PathLength*f;
end;

constructor TGuiRadialProgressBar.Create(aCenter: TPointF; aRadius: single);
begin
 inherited Create;
 FCircularPath := TOGLCPath.Create;
 FCircularPath.Pts := ComputeEllipse(aCenter.x, aCenter.y, aRadius, aRadius );
 AssignPath( FCircularPath );
end;

destructor TGuiRadialProgressBar.Destroy;
begin
 FCircularPath.Free;
 inherited Destroy;
end;

function TGuiRadialProgressBar.Completed: boolean;
begin
 result:= FPosition>=Max;
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

 if not FGuiEnabled then exit;

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
  aFont: TGuiFont; aHorizAlign: TAlignment; aVerticalAlign: TTextLayout);
begin
 FCaption := aText;
 FFont := aFont;
 FWidth := aWidth;
 FHeight := aHeight;
 FHAlign := aHorizAlign;
 FVAlign := aVerticalAlign;

 PrepareImage;
 inherited Create( TextureManager.Add(FImaTemp), TRUE );
 FFont := aFont; // because 'inherited Create' change it to font default
 ReleaseImage;

 SetCoordinate ( aX, aY );
end;

procedure TGuiTextArea.PrepareImage;
var renderer: TBGRATextEffectFontRenderer;
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

 FImaTemp:=TBGRABitmap.Create(FWidth, FHeight, BGRAPixelTransparent);
 FImaTemp.FontRenderer := renderer;
 FImaTemp.FontQuality:= fqFineAntialiasing;
 FImaTemp.FontHeight := FFont.FontHeight ;
 FImaTemp.FontName := FFont.FontName;
 FImaTemp.FontStyle := FFont.Style;

 FImaTemp.TextRect(Rect(0, 0, FWidth, FHeight), FCaption,
                   FHAlign, FVAlign, FFont.FontColor);
end;

procedure TGuiTextArea.ReleaseImage;
begin
 FreeAndNil(FImaTemp);
end;

procedure TGuiTextArea.DoFontChange;
begin
 DoCaptionChange;
end;

procedure TGuiTextArea.DoCaptionChange;
begin
 PrepareImage;
 if FTexture<>NIL then TextureManager.Delete(FTexture);
 FTexture := TextureManager.Add(FImaTemp);
 ReleaseImage;
end;





{$endif oglcIMPLEMENTATION}


