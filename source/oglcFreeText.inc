{$ifdef oglcINTERFACE}

{ TFreeText }

// A surface that draw text. The text can be multiline. Lines are separated by #10.
// Instead of TUILabel, TFreeText is not clipped and mouse can't interact with it.
TFreeText = class(TSimpleSurfaceWithEffect)
private
  FCharIndexes: array of TTexturedCharIndexes;
  FCaptions: TStringArray;
  FTexturedFont: TTexturedFont;
  FSize: TSize;
  procedure UpdateCharIndexesAndComputeSize;
  function GetCaption: string;
  procedure SetCaption(AValue: string); virtual;
  procedure SetTexturedFont(AValue: TTexturedFont); virtual;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure DoDraw; override;
public
  constructor Create(aParentScene: TOGLCScene);
  property Caption: string read GetCaption write SetCaption;
  property TexturedFont: TTexturedFont read FTexturedFont write SetTexturedFont;
end;


{ TFreeTextOnPath }
// a text that moves along a TOGLCPathToFollow instance
TFreeTextOnPath = class(TFreeText)
private
  FAutoRotate: boolean;
  FAngleAdjust,
  FCoeffPositionOnPath: single;
  procedure SetCoeffPositionOnLine(AValue: single);
public
  procedure Update(const aElapsedTime: single); override;
  procedure Draw(const aLayerPercentOpacity: single);  override;
  procedure DoDraw; override;
public
  // Distance traveled on the path
  DistanceTraveled: TFParam;

  // Use this constructor to create the text associated with a TOGLCPathToFollow instance.
  constructor CreateAsChildOf(aParentPathToFollow: TOGLCPathToFollow; aZOrder: integer=0);
  destructor Destroy; override;

  // 'TRUE' force the sprite to rotate on the right direction along the path. Default is TRUE.
  property AutoRotate: boolean read FAutoRotate write FAutoRotate;
  // Set this value to ajust the right orientation of the sprite. Default is '0'.
  property AngleAdjust: single read FAngleAdjust write FAngleAdjust;
  // Allow to control the position of the sprite on the path. Range is from 0 to 1. Default value is 0.5
  //   0 -> the sprite is above the path
  // 0.5 -> the sprite is centered on the path
  //   1 -> the sprite is below the path
  property CoeffPositionOnPath: single read FCoeffPositionOnPath write SetCoeffPositionOnLine;
end;

TFreeTextOnPathToFollow = TFreeTextOnPath deprecated 'use TFreeTextOnPath instead';


{ TFreeTextClock }
// A surface that can count/count down time and show the current time
TFreeTextClock = class(TFreeText)
private
  FOnCountdownDone: TNotifyEvent;
  FTime: single;
  FPaused,
  FCountdown,
  FShowFractionalPart: boolean;
  procedure SetShowFractionalPart(AValue: boolean);
  procedure SetTime(AValue: single);
  procedure UpdateCaption;
public
  // the TexturedFont used by the inherited TFreeText must have the characters 0123456789:. included
  // Set aPaused to TRUE to pause the count/countdown at the begining
  constructor Create(aParentScene: TOGLCScene; aPaused: boolean);
  procedure Update(const aElapsedTime: single); override;
  // pause the clock
  procedure Pause;
  // run the clock
  procedure Run;
  // gives/sets the current value to count in seconds
  property Time: single read FTime write SetTime;
  // set to TRUE if you want to time countdown. Default is FALSE (counting time)
  property Countdown: boolean read FCountdown write FCountdown;
  // set to TRUE force the tenths and hundredths of seconds to be drawned. default is TRUE
  property ShowFractionalPart: boolean read FShowFractionalPart write SetShowFractionalPart;
  // event callback for countdown mode. Fired when timer reach 0.
  property OnCountdownDone: TNotifyEvent read FOnCountdownDone write FOnCountdownDone;
end;
  TFreeTextClockLabel = TFreeTextClock deprecated 'use TFreeTextClock instead';


{ TFreeTextAligned }
// Draws a text in a rectangular area with horizontal align (Topleft, TopCenter or TopRight).
// The text can be multiline: use #10 to separate the lines.
// Instead of TUITextArea, the text is not clipped and mouse can't interact with it.
TFreeTextAligned = class(TSimpleSurfaceWithEffect)
private
  function GetCaption: string;
  procedure SetCaption(AValue: string);
  procedure SetTexturedFont(AValue: TTexturedFont);
  procedure SetAlign(AValue: TOGLCAlignment); virtual;
private
  FAlignedTextDescriptor: TAlignedTextDescriptor;
  function GetAlign: TOGLCAlignment;
  function GetDrawingRect: TRect;
  function GetTexturedFont: TTexturedFont;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure DoDraw; override;
public
  constructor Create(aParentScene: TOGLCScene; aFont: TTexturedFont; aWidth, aHeight: integer);
  destructor Destroy; override;

  // Redefine the size of the area where the text is displayed.
  procedure SetSize(aWidth, aHeight: integer);

  // Adjust the size of the surface to the real size of the formatted text
  // Call this method AFTER initializing the Caption, TexturedFont and Align
  procedure AdjustSize;

  property Caption: string read GetCaption write SetCaption;
  property TexturedFont: TTexturedFont read GetTexturedFont write SetTexturedFont;
  // The alignment of the text. Use only taTopLeft, taTopCenter and taTopRight.
  // default is taTopLeft
  property Align: TOGLCAlignment read GetAlign write SetAlign;
  // The real size of the used rectangle after formatted the text.
  property DrawingRect: TRect read GetDrawingRect;
end;

TAlignedText = TFreeTextAligned deprecated 'use TFreeTextAligned instead';

 {$endif oglcINTERFACE}
 {$ifdef oglcIMPLEMENTATION}

{ TFreeTextAligned }

procedure TFreeTextAligned.SetAlign(AValue: TOGLCAlignment);
begin
  if (AValue = taCenterLeft) or (AValue = taBottomLeft) then AValue := taTopLeft;
  if (AValue = taCenterCenter) or (AValue = taBottomCenter) then AValue := taTopCenter;
  if (AValue = taCenterRight) or (AValue = taBottomRight) then AValue := taTopRight;
  FAlignedTextDescriptor.Align := AValue;
end;

function TFreeTextAligned.GetAlign: TOGLCAlignment;
begin
  Result := FAlignedTextDescriptor.Align;
end;

function TFreeTextAligned.GetDrawingRect: TRect;
begin
  Result := FAlignedTextDescriptor.DrawingRect;
  Result.Offset(Round(X.Value), Round(Y.Value));
end;

function TFreeTextAligned.GetTexturedFont: TTexturedFont;
begin
  Result := FAlignedTextDescriptor.TexturedFont;
end;

procedure TFreeTextAligned.SetCaption(AValue: string);
begin
  FAlignedTextDescriptor.Caption := AValue;

end;

procedure TFreeTextAligned.SetTexturedFont(AValue: TTexturedFont);
begin
  FAlignedTextDescriptor.TexturedFont := AValue;
end;

function TFreeTextAligned.GetWidth: integer;
begin
  Result := FAlignedTextDescriptor.Area.Width; //  .DrawingSize.cx;
end;

function TFreeTextAligned.GetHeight: integer;
begin
  Result := FAlignedTextDescriptor.Area.Height; //  .DrawingSize.cy;
end;

procedure TFreeTextAligned.DoDraw;
var i, h: integer;
begin
  if not FAlignedTextDescriptor.ReadyToRender then exit;

  h := FAlignedTextDescriptor.DrawingRect.Height;
  with FAlignedTextDescriptor do
    for i:=0 to Length(Words)-1 do
      if Words[i].Location.y < h then
        FTexturedFont.DrawChars(Words[i].CharIndexes, Words[i].Location.x, Words[i].Location.y, 0, NIL, NIL,
                                FComputedOpacity, FComputedTint, FBlendMode);
end;

constructor TFreeTextAligned.Create(aParentScene: TOGLCScene; aFont: TTexturedFont; aWidth, aHeight: integer);
begin
  inherited Create;
  FParentScene := aParentScene;
  FAlignedTextDescriptor.InitDefault;
  FAlignedTextDescriptor.Align := taTopLeft;
  FAlignedTextDescriptor.TexturedFont := aFont;
  FAlignedTextDescriptor.Area := Rect(0, 0, aWidth, aHeight);
end;

destructor TFreeTextAligned.Destroy;
begin
  FAlignedTextDescriptor.FreeObjects;
  inherited Destroy;
end;

procedure TFreeTextAligned.SetSize(aWidth, aHeight: integer);
begin
  FAlignedTextDescriptor.Area := Rect(0, 0, aWidth, aHeight);
  FAlignedTextDescriptor.ForceDistributeWordsAndAlign;
end;

procedure TFreeTextAligned.AdjustSize;
begin
  SetSize(DrawingRect.Width, DrawingRect.Height);
end;

function TFreeTextAligned.GetCaption: string;
begin
  Result := FAlignedTextDescriptor.Caption;
end;


{ TFreeText }

procedure TFreeText.UpdateCharIndexesAndComputeSize;
var i: integer;
  s: TSize;
begin
  FSize.cx := 0;
  FSize.cy := 0;

  if (FTexturedFont = NIL) or (Length(FCaptions) = 0) then begin
    FCharIndexes := NIL;
    exit;
  end;

  SetLength(FCharIndexes, Length(FCaptions));
  for i:=0 to High(FCaptions) do begin
    FCharIndexes[i] := FTexturedFont.StringToCharIndexes(FCaptions[i]);

    s := FTexturedFont.CharIndexesToSize(FCharIndexes[i]);
    if FSize.cx < s.cx then FSize.cx := s.cx;
    FSize.cy := FSize.cy + s.cy;
  end;
end;

function TFreeText.GetCaption: string;
var i: integer;
begin
  Result := '';
  for i:=0 to High(FCaptions) do begin
    Result := Result + FCaptions[i];
    if i < High(FCaptions) then Result := Result + #10;
  end;
end;

procedure TFreeText.SetCaption(AValue: string);
begin
  if AValue = '' then FCaptions := NIL
    else FCaptions := AdjustLineEnding(AValue).Split([#10]);
  UpdateCharIndexesAndComputeSize;
end;

procedure TFreeText.SetTexturedFont(AValue: TTexturedFont);
begin
  if FTexturedFont = AValue then Exit;
  FTexturedFont := AValue;
  UpdateCharIndexesAndComputeSize;
end;

function TFreeText.GetWidth: integer;
begin
  Result := FSize.cx;
end;

function TFreeText.GetHeight: integer;
begin
  Result := FSize.cy;
end;

procedure TFreeText.DoDraw;
var i, yy: integer;
begin
  if (FSize.cx = 0) or (FTexturedFont = NIL) then exit;
  yy := 0;
  for i:=0 to High(FCaptions) do begin
    FTexturedFont.DrawChars(FCharIndexes[i], 0, yy, FlipToIndex, NIL,
                            NIL, FComputedOpacity, FComputedTint, FBlendMode);
    yy := yy + FTexturedFont.Font.FontHeight;
  end;
end;

constructor TFreeText.Create(aParentScene: TOGLCScene);
begin
  inherited Create;
  FParentScene := aParentScene;
end;

 { TFreeTextOnPath }

procedure TFreeTextOnPath.SetCoeffPositionOnLine(AValue: single);
begin
  FCoeffPositionOnPath := EnsureRange(AValue, 0.0, 1.0);
end;

procedure TFreeTextOnPath.Update(const aElapsedTime: single);
begin
   inherited Update(aElapsedTime);
   if not FFreeze then
     DistanceTraveled.OnElapse(aElapsedTime);
end;

procedure TFreeTextOnPath.Draw(const aLayerPercentOpacity: single);
var i, j, flipIndex, yOffset: integer;
  m: TOGLCMatrix;
  pos: TPointF;
  lineY, ang, dist, spacingCoef: single;
  ch: PTexturedChar;
begin
  if (FSize.cx = 0) or (FTexturedFont = NIL) then exit;

  FComputedOpacity := aLayerPercentOpacity*Opacity.Value*OneDiv255;
  if FComputedOpacity = 0 then exit;
  FComputedTint.InitFromTint(TintMode, Tint.Value);

  // render childs with negative ZOrder
  if (FChilds.Count > 0) and (TSimpleSurfaceWithEffect(FChilds[0]).FZOrderAsChild < 0) then begin
    PushAndPrepareSceneMV;
    if FChildsUseParentOpacity then DrawChildSurfacesWithNegativeZOrder(FComputedOpacity)
      else DrawChildSurfacesWithNegativeZOrder(aLayerPercentOpacity);
    PopSceneMV;
  end;

  // here model view matrix is set individually for each characters to draw.

  flipIndex := FlipToIndex;
  spacingCoef := FTexturedFont.CharSpacingCoeff.Value;

  yOffset := FTexturedFont.Font.FontHeight;
  yOffset := Round(yOffset - yOffset * FCoeffPositionOnPath);

  for i:=0 to High(FCharIndexes) do begin
    lineY := FTexturedFont.Font.FontHeight*i;
    dist := DistanceTraveled.Value;
    for j:=0 to High(FCharIndexes[i]) do begin
      ch := FTexturedFont.GetCharByIndex(FCharIndexes[i][j]);
      if ch <> NIL then begin
        TOGLCPathToFollow(FParentSurface).GetPosAndAngle(dist, pos, ang);
        if not AutoRotate then ang := Angle.Value
          else ang := ang + FAngleAdjust;
        m.CopyFrom(FParentScene.ModelViewMatrix);
        with m do begin
          Translate(pos.x-ch^.OriginalCharSize.cx*0.5, pos.y + lineY);
          Scale(Self.Scale.x.Value, Self.Scale.y.Value);
          Rotate(ang);
        end;

        FTexturedFont.DrawChar(FCharIndexes[i][j], 0, -yOffset, flipIndex, NIL, @m.Matrix, FComputedOpacity, FComputedTint, FBlendMode);
        dist := dist + ch^.OriginalCharSize.cx * spacingCoef * Scale.x.Value;
      end;
    end;
  end;

  // render childs with positive ZOrder
  if FChilds.Count > 0 then begin
    PushAndPrepareSceneMV;
    if FChildsUseParentOpacity then DrawChildSurfacesWithPositiveZOrder(FComputedOpacity)
      else DrawChildSurfacesWithPositiveZOrder(aLayerPercentOpacity);
    PopSceneMV;
  end;
end;

procedure TFreeTextOnPath.DoDraw;
begin
end;

constructor TFreeTextOnPath.CreateAsChildOf(aParentPathToFollow: TOGLCPathToFollow; aZOrder: integer);
begin
  inherited Create(aParentPathToFollow.ParentScene);
  DistanceTraveled := TFParam.Create;
  AutoRotate := TRUE;
  FCoeffPositionOnPath := 0.0;
  aParentPathToFollow.AddChild(Self, aZOrder);
end;

destructor TFreeTextOnPath.Destroy;
begin
  DistanceTraveled.Free;
  DistanceTraveled := NIL;
  inherited Destroy;
end;

 { TFreeTextClock }

procedure TFreeTextClock.SetTime(AValue: single);
begin
  //if FTime = AValue then Exit;
  FTime := AValue;
  UpdateCaption;
end;

procedure TFreeTextClock.SetShowFractionalPart(AValue: boolean);
begin
  if FShowFractionalPart = AValue then Exit;
  FShowFractionalPart := AValue;
  UpdateCaption;
end;

procedure TFreeTextClock.UpdateCaption;
begin
  if FShowFractionalPart then
    Caption := SecondsToString(FTime, FShowFractionalPart)
  else
    Caption := IntToStr(Ceil(FTime));
end;

constructor TFreeTextClock.Create(aParentScene: TOGLCScene; aPaused: boolean);
begin
  inherited Create(aParentScene);
  FPaused := aPaused;
  FShowFractionalPart := TRUE;
end;

procedure TFreeTextClock.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if FFreeze or FPaused then exit;

  if FCountdown then begin
    if FTime > 0 then begin
      FTime := FTime - aElapsedTime;
      if FTime < 0 then begin
        FTime := 0;
        if FOnCountdownDone <> NIL then FOnCountdownDone(self);
        FPaused := True;
      end;
    end;
  end else FTime := FTime + aElapsedTime;
  UpdateCaption;
end;

procedure TFreeTextClock.Pause;
begin
  FPaused := TRUE;
end;

procedure TFreeTextClock.Run;
begin
  FPaused := FALSE;
end;

{$endif oglcIMPLEMENTATION}

