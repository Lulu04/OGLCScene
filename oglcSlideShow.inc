{$ifdef oglcINTERFACE}
type
  { TSlideShow }
   TSlideFX = ( sfxNormal, sfxGray, sfxSphere, sfxSmooth, sfxContour ); // BGRABitmap effect
   TSlideTransition = ( stNone,  // no fade
                        stFadeToColor,   // not implemented
                        stFadeToNextImage, // next image appear from opacity=0 to 255
                        stFadeToNextImageFrom100 ); // next image appear from opacity=100 to 255

   TSlideStep = class
     Filename: string;
     Effect: TSlideFX;
     Tex: PTexture;
     Sprite: TSprite;
     StayTime: single;
     TransitionType: TSlideTransition;
     TransitionTime: single;
   end;
   TSlideState = ( stEmpty,
                   stPlaying,
                   stPaused,
                   stStopped );


  TSlideShow= class( TSimpleSurfaceWithEffect )
  private
    FWidth, FHeight: integer;
    FList: TList;
    FCurrentIndex: integer;
    FCurrentTexture: PTexture;
    FCurrentSprite: TSprite;
    FCurrentStayTime: single;
    FLoop: boolean;
    FSlideState: TSlideState;
    function GetCount: integer;
    function GetCurrentIndex: integer;
    function GetSlideStep( aIndex: integer ): TSlideStep;
    procedure SetCurrentStep;
    procedure UnsetCurrentStep( ATransition: TSlideTransition; ATransitionTime: single );
  protected
    function GetWidth: integer; override;
    function GetHeight: integer; override;
  public
    constructor Create;
    Destructor Destroy; override;
    procedure Update( const aElapsedTime: single ); override;
  public
    // use to stretch the time between each image
    // 0.0= time stopped   0.5= half time     1.0= normal time  2.0= accelerate double time  etc...
    StretchStayTime: TFParam;
    procedure SetSize( aW, aH: integer );
    procedure AddImage( AFilename: string; AEffect: TSlideFX;
                        AStayTime: single;
                        ATransition: TSlideTransition; ATransitionTime: single );
    procedure Clear;
    // Use this method to go manually to the next image
    procedure GotoNextStep;
    // Automatic slide
    procedure Play( AFromBegin: boolean=TRUE );
    procedure Pause;
    procedure Stop;

    property Loop: boolean read FLoop write FLoop; // TRUE= loop to first image
    property Count: integer read GetCount;
    property CurrentIndex: integer read GetCurrentIndex;
    property State: TSlideState read FSlideState;
  end;
{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}
{ TSlideShow }

function TSlideShow.GetSlideStep(aIndex: integer): TSlideStep;
begin
 Result := TSlideStep( FList.Items[aIndex] );
end;

function TSlideShow.GetCount: integer;
begin
 Result := FList.Count;
end;

function TSlideShow.GetCurrentIndex: integer;
begin
 Result := FCurrentIndex;
end;

procedure TSlideShow.GotoNextStep;
var i: integer;
    s: TSlideStep;
begin
 if FSlideState = stEmpty then exit;

// if ( FCurrentIndex = GetCount-1 ) and not FLoop then exit;

 // take next step index
 i := FCurrentIndex;
 inc(i);
 if i >= GetCount then begin
   if FLoop then i := 0
            else begin
              i := GetCount-1;
              FSlideState := stStopped;
              exit;
            end;
 end;

 if i = FCurrentIndex then exit;
 s := GetSlideStep( i );

 UnsetCurrentStep(s.TransitionType, s.TransitionTime );
 FCurrentIndex := i;
 SetCurrentStep;
end;

procedure TSlideShow.SetCurrentStep;
var s: TSlideStep;
    ima, temp: TBGRABitmap;
    procedure SetScaleValue;
    var p: TPointF;
    begin
     p := ScaleValueToFitSurface(FCurrentTexture, self);
     p.x := p.x * Scale.Value.x;
     p.y := p.y * Scale.Value.y;
     FCurrentSprite.Scale.Value := p;
    end;

begin
 s := GetSlideStep( FCurrentIndex );

 if s.Effect = sfxNormal
   then FCurrentTexture := TextureManager.Add( s.Filename )
   else begin
    ima := TBGRABitmap.Create( s.Filename );
    case s.Effect of
     sfxGray: temp := ima.FilterGrayscale as TBGRABitmap;
     sfxSphere: temp := ima.FilterSphere as TBGRABitmap;
     sfxSmooth: temp :=ima.FilterSmooth as TBGRABitmap;
     sfxContour: temp := ima.FilterContour as TBGRABitmap;
   end;
   FCurrentTexture := TextureManager.Add( {%H-}temp );
   ima.Free;
   temp.Free;
 end;

 FCurrentSprite := TSprite.Create( FCurrentTexture, TRUE );
 FParentLayer.Add( FCurrentSprite );

 case s.TransitionType of
  stNone: begin
    FCurrentSprite.CenterOnSurface( self );
    SetScaleValue;
  end;

  stFadeToColor:begin
    FCurrentSprite.CenterOnSurface( self );
    SetScaleValue;
  end;

  stFadeToNextImage: begin
    FCurrentSprite.CenterOnSurface( self );
    SetScaleValue;
    FCurrentSprite.Opacity.Value:=0;
    FCurrentSprite.Opacity.ChangeTo( 255, s.TransitionTime );
  end;
  stFadeToNextImageFrom100: begin
    FCurrentSprite.CenterOnSurface( self );
    SetScaleValue;
    FCurrentSprite.Opacity.Value:=100;
    FCurrentSprite.Opacity.ChangeTo( 255, s.TransitionTime );
  end;
 end;//case

 FCurrentStayTime := s.StayTime;
end;

procedure TSlideShow.UnsetCurrentStep( ATransition: TSlideTransition; ATransitionTime: single );
begin
 if FCurrentSprite=NIL then exit;

 case ATransition of

  stNone: FCurrentSprite.Kill;

  stFadeToColor:;

  stFadeToNextImage,
  stFadeToNextImageFrom100: begin
    FCurrentSprite.Opacity.ChangeTo( 0, ATransitionTime );
    FCurrentSprite.KillDefered( ATransitionTime );
  end;

 end;//case

 FCurrentSprite := NIL;
end;

function TSlideShow.GetWidth: integer;
begin
 Result := FWidth;
end;

function TSlideShow.GetHeight: integer;
begin
 Result := FHeight;
end;

constructor TSlideShow.Create;
begin
 inherited Create;
 FList:= TList.Create;
 FCurrentTexture:=NIL;
 FCurrentSprite:=NIL;
 FCurrentIndex:=-1;
 FLoop:=FALSE;
 FSlideState:=stEmpty;
 StretchStayTime:= TFParam.Create;
 StretchStayTime.Value:=1.0;
end;

destructor TSlideShow.Destroy;
begin
 Clear;
 FList.Free;
 StretchStayTime.Free;
 inherited Destroy;
end;

procedure TSlideShow.Update(const aElapsedTime: single);
begin
 inherited Update(aElapsedTime);
 StretchStayTime.OnElapse( aElapsedTime );
 if FSlideState = stEmpty then exit;


 if FCurrentSprite <> NIL then begin
   FCurrentSprite.Update( aElapsedTime );
   FCurrentSprite.Tint.Value := Tint.Value;
 end;

 if FSlideState = stPlaying then begin
     if FCurrentStayTime > 0.0
     then FCurrentStayTime -= aElapsedTime*StretchStayTime.Value
     else GotoNextStep;
 end;
end;

procedure TSlideShow.SetSize(aW, aH: integer);
begin
 FWidth := aW;
 FHeight := aH;
end;

procedure TSlideShow.AddImage(AFilename: string; AEffect: TSlideFX;
  AStayTime: single; ATransition: TSlideTransition; ATransitionTime: single);
var o:TSlideStep;
begin
 o := TSlideStep.Create;
 o.Filename:=AFilename;
 o.Effect:=AEffect;
 o.StayTime:=AStayTime;
 o.TransitionType:=ATransition;
 o.TransitionTime:=ATransitionTime;

 FList.Add( o );

 if FSlideState = stEmpty
   then FSlideState := stStopped;
end;

procedure TSlideShow.Clear;
begin
 UnsetCurrentStep( stNone, 0 );

 while FList.Count > 0 do begin
   GetSlideStep(0).Free;
   FList.Delete(0);
 end;

 FCurrentIndex := -1;
 FSlideState := stEmpty;
end;

procedure TSlideShow.Play( AFromBegin: boolean );
begin
 if FSlideState = stEmpty then exit;

 if AFromBegin or ( FCurrentIndex = -1 ) then begin
   UnsetCurrentStep( stFadeToNextImage, 1 );
   FCurrentIndex := 0;
   SetCurrentStep;
 end;

 FSlideState := stPlaying;
end;

procedure TSlideShow.Pause;
begin
 if FSlideState = stEmpty then exit;
 if FSlideState = stStopped then exit;

 if FSlideState = stPlaying
   then FSlideState := stPaused
   else FSlideState := stPlaying;

end;

procedure TSlideShow.Stop;
begin
 if FSlideState = stEmpty then exit;
 FSlideState := stStopped;
end;
{$endif oglcIMPLEMENTATION}


