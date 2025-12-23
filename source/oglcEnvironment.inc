{$ifdef oglcINTERFACE}

// The code for TWaterSurface below has been written by Circular, the creator
// and maintainer of BGRABitmap library.
// Thanks !
type

{ TWaterSurface }

TWaterSurface = class( TSimpleSurfaceWithEffect )
private type
  TWaterSurfaceValue = NativeInt;
  PWaterPixel = ^TWaterPixel;
  TWaterPixel = packed record
    h,                     //height (with average equal to zero)
    v: TWaterSurfaceValue; //speed (multiplied by precisionFactor)
  end;
private const
  watersizex = 640;
  watersizey = 480;
  precisionFactor = 256; //use power of two to improve compiler optimisation
  gravity = precisionFactor * 2 div 10;
  friction = precisionFactor * 1 div 256;
private
   w : packed array[0..watersizey, 0..watersizex] of TWaterPixel;
   FC: single;
   FRainRadius,
   FRainHeight: integer;
   FWaterIsTransparent: boolean;
   function clamp(AValue: TWaterSurfaceValue; AMin,AMax: TWaterSurfaceValue): TWaterSurfaceValue; inline;
private
   FBGRABuffer: TBGRABitmap;
   FTexture: PTexture;
   FWidth, FHeight: integer;
   FCornerOffset: TQuadParamF;
protected
   function GetWidth: integer; override;
   function GetHeight: integer; override;
public
   procedure Update(const aElapsedTime: single); override;
   procedure DoDraw; override;
public
   constructor Create(aParentScene: TOGLCScene);
   Destructor Destroy; override;
   procedure WaterReset;

   // The effect is drawn internally on a 640x480 texture, then this texture is
   // stretch drawn on the screen with the specified size.
   procedure SetSize(aWidth, aHeight: integer);
   procedure WaterDrop(aX, aY, r, dropHeight: TWaterSurfaceValue); overload;
   procedure WaterDrop(aX, aY: single); overload;
public
   DropAmount: TBoundedFParam;  // 0=no    10=max
   WaterColor: TBGRAParam;
   property WaterIsTransparent: boolean read FWaterIsTransparent write FWaterIsTransparent; // if true, draw only waterdrop and not static water
   property RainRadius: integer read FRainRadius write FRainRadius;
   property RainHeight: integer read FRainHeight write FRainHeight;
end;



{ TSnow }

TSnow = class(TSimpleSurfaceWithEffect)
private type
  PFlake = ^TStructFlake;
  TStructFlake = record
    fx,
    fy,
    speedx,
    speedy,
    anglex,
    amplitudex,
    xdraw: single;
  end;
  TArrayOfFlake = array of TStructFlake;
private
   FTexture: PTexture;
   FTextureOwner: boolean;
   FWidth, FHeight: integer;
   FFlakeArray: array[0..2] of TArrayOfFlake;
   FQuadOffset: TQuadParamF;
   function GetOneTypeFlakeCount: integer;
   procedure InitFlake(aPFlake: PFlake);
   procedure InitLittleFlake(aPFlake: PFlake);
   procedure InitMiddleFlake(aPFlake: PFlake);
   procedure InitBigFlake(aPFlake: PFlake);
protected
   function GetWidth: integer; override;
   function GetHeight: integer; override;
public
   constructor Create;
   Destructor Destroy; override;
public
   procedure Update(const aElapsedTime: single); override;
   procedure DoDraw; override;
public
   FlakeAmount: TBoundedFParam;  // 0=no snow    1000=max
   FlakeSpeed: TPointFParam;     // speed on both axis in pixels per second
   HAmplitude: TFParam;          // horizontal amplitude in pixel
   Gravity: TPointFParam;
   // Set the size of the rendered area
   procedure SetSize(aWidth, aHeight: integer);
   // Set the texture to use for the snow flakes
   procedure UseTexture(aTexture: PTexture; aOwner: boolean); overload;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TSnow }

function TSnow.GetOneTypeFlakeCount: integer;
begin
  Result := round(FlakeAmount.Value/3);
end;

procedure TSnow.InitFlake(aPFlake: PFlake);
begin
  aPFlake^.fx := random (Width);
  aPFlake^.fy := random(Height);
  aPFlake^.anglex := Random(360);
end;

procedure TSnow.InitLittleFlake(aPFlake: PFlake);
begin
  InitFlake(aPFlake);
  aPFlake^.speedx := FlakeSpeed.x.Value*0.4;
  aPFlake^.speedy := FlakeSpeed.y.Value*0.4 - FlakeSpeed.y.Value*0.2*Random;
  aPFlake^.amplitudex := HAmplitude.Value*0.4;
end;

procedure TSnow.InitMiddleFlake(aPFlake: PFlake);
begin
  InitFlake(aPFlake);
  aPFlake^.speedx := FlakeSpeed.x.Value * 0.6;
  aPFlake^.speedy := FlakeSpeed.y.Value*0.6 - FlakeSpeed.y.Value*0.3*Random;
  aPFlake^.amplitudex := HAmplitude.Value * 0.6;
end;

procedure TSnow.InitBigFlake(aPFlake: PFlake);
begin
  InitFlake(aPFlake);
  aPFlake^.speedx := FlakeSpeed.x.Value;
  aPFlake^.speedy := FlakeSpeed.y.Value - FlakeSpeed.y.Value*0.4*Random;
  aPFlake^.amplitudex := HAmplitude.Value;
end;

function TSnow.GetWidth: integer;
begin
  Result := FWidth;
end;

function TSnow.GetHeight: integer;
begin
  Result := FHeight;
end;

constructor TSnow.Create;
begin
  inherited Create;

  FlakeAmount := CreateBoundedFParam(0, 1000, FALSE);

  FlakeSpeed := TPointFParam.Create;
  FlakeSpeed.Value := PointF(10, 10);

  Gravity := TPointFParam.Create;

  HAmplitude := TFParam.Create;
  FQuadOffset := TQuadParamF.Create;
end;

destructor TSnow.Destroy;
begin
  FreeAndNil(FlakeAmount);
  FreeAndNil(FlakeSpeed);
  FreeAndNil(Gravity);
  FreeAndNil(HAmplitude);
  FreeAndNil(FQuadOffset);

  if FTextureOwner then FParentScene.TexMan.Delete(FTexture);
  inherited Destroy;
end;

procedure TSnow.Update(const aElapsedTime: single);
var i, j, z, fc: integer;
    wFlake: single;
begin
  if FFreeze then exit;
  inherited Update(aElapsedTime);

  FlakeAmount.OnElapse(aElapsedTime);
  FlakeSpeed.OnElapse(aElapsedTime);
  Gravity.OnElapse(aElapsedTime);
  HAmplitude.OnElapse(aElapsedTime);

  fc := GetOneTypeFlakeCount;

  // adjust array according flake's count
  for i:=0 to 2 do
   if Length(FFlakeArray[i]) < fc then
   begin
    z := Length(FFlakeArray[i]);
    SetLength(FFlakeArray[i], fc);
    for j:=z to fc-1 do
     case i of
       0: InitLittleFlake(@FFlakeArray[i][j]);
       1: InitMiddleFlake(@FFlakeArray[i][j]);
       2: InitBigFlake( @FFlakeArray[i][j]);
     end;
   end else
   if Length(FFlakeArray[i]) > fc then SetLength(FFlakeArray[i], fc);

  // update each flake
  wFlake := FTexture^.FrameWidth;
  for i:=0 to 2 do
   for j:=0 to Length(FFlakeArray[i])-1 do
    with FFlakeArray[i][j] do
    begin
      fx := fx + Gravity.x.Value * aElapsedTime;
      if fx < -wFlake then fx := fx + Width
      else if fx > Width+wFlake then fx := fx - Width-wFlake;

      fy := fy + (speedy + Gravity.y.Value)*aElapsedTime;
      if fy > Height then fy := fy - Height-FTexture^.FrameHeight;

      anglex := anglex + speedx*aElapsedTime;
      if anglex > 360 then anglex := anglex - 360;

      xdraw := fx + sin(anglex)*amplitudex;
    end;
end;

procedure TSnow.DoDraw;
var i: integer;
     w , h: single;
    procedure InitQuadOffset(aCoef: single);
    begin
     FQuadOffset.TopLeft.Value := PointF(w*aCoef, h*aCoef);
     FQuadOffset.TopRight.Value := PointF(-w*aCoef, h*aCoef);
     FQuadOffset.BottomRight.Value := PointF(-w*aCoef, -h*aCoef);
     FQuadOffset.BottomLeft.Value := PointF(w*aCoef, -h*aCoef);
    end;
begin
  if FTexture = NIL then exit;
  w := FTexture^.FrameWidth;
  h := FTexture^.FrameHeight;

  glScissor(round(X.Value), round(Y.Value), Width, Height);
  glEnable(GL_SCISSOR_TEST);

  FParentScene.TexturedTriangleRenderer.Prepare(ptTriangleStrip, FTexture, 0, FParentScene.MVPMatrix,
                                                FComputedOpacity, FComputedTint, FBlendMode);
  // small
  InitQuadOffset(0.8);
  for i:=0 to High(FFlakeArray[0]) do
   with FFlakeArray[0][i] do
     FParentScene.TexturedTriangleRenderer.PushFrameWithCornerOffset(0, 0, xdraw, fy, FQuadOffset);
  // middle
  InitQuadOffset(0.65);
  for i:=0 to High(FFlakeArray[1]) do
   with FFlakeArray[1][i] do
     FParentScene.TexturedTriangleRenderer.PushFrameWithCornerOffset(0, 0, xdraw, fy, FQuadOffset);
  // normal
  for i:=0 to High(FFlakeArray[2]) do
   with FFlakeArray[2][i] do
     FParentScene.TexturedTriangleRenderer.PushFrame(0, 0, xdraw, fy);
  FParentScene.TexturedTriangleRenderer.Draw;

  glDisable(GL_SCISSOR_TEST);
end;

procedure TSnow.SetSize(aWidth, aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
end;

procedure TSnow.UseTexture(aTexture: PTexture; aOwner: boolean);
begin
  FTexture := aTexture;
  FTextureOwner := aOwner;
end;

{ TWaterSurface }

function TWaterSurface.clamp(AValue: TWaterSurfaceValue; AMin, AMax: TWaterSurfaceValue): TWaterSurfaceValue;
begin
  if AValue <= AMin then result := AMin
    else if AValue >= AMax then result := AMax
     else result := AValue;
end;

function TWaterSurface.GetWidth: integer;
begin
  Result := FWidth;
end;

function TWaterSurface.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TWaterSurface.Update(const aElapsedTime: single);
var xx, yy, hgtSurr, hgtDiff, a: TWaterSurfaceValue;
    diagFactor, denom: TWaterSurfaceValue;
    pw: PWaterPixel;
    currentAverage: NativeInt;
    hxy: TWaterSurfaceValue;
    P: PBGRAPixel;
    c: TBGRAPixel;
begin
  inherited Update( aElapsedTime );
  if FFreeze then exit;

  DropAmount.OnElapse( aElapsedTime );
  if DropAmount.Value > 0.0 then
  begin
    FC += aElapsedTime;
    if FC >= (10 - DropAmount.Value)/10*3 then  // water drop fall every [0..3] seconds
    begin
      xx := random(FWidth - 10) + 5;
      yy := random(FHeight - 10) + 5;
      WaterDrop(xx, yy, FRainRadius, random(FRainHeight) + FRainHeight div 2);
      FC := 0;
    end;
  end;

  WaterColor.OnElapse(aElapsedTime);

  //use the border to reflect the values nearby
  //so that water bounces without loss of energy
  for xx := 1 to watersizex - 1 do
  begin
    w[0,xx] := w[1,xx];
    w[watersizey,xx] := w[watersizey-1,xx];
  end;
  for yy := 1 to watersizey - 1 do
  begin
    w[yy,0] := w[yy,1];
    w[yy,watersizex] := w[yy,watersizex-1];
  end;
  //same with corners
  w[0,0].h := (w[1,0].h+w[0,1].h) div 2;
  w[watersizey,0].h := (w[watersizey-1,0].h+w[watersizey,1].h) div 2;
  w[watersizey,watersizex].h := (w[watersizey-1,watersizex].h+w[watersizey,watersizex-1].h) div 2;
  w[0,watersizex].h := (w[1,watersizex].h+w[0,watersizex-1].h) div 2;
  //compute speeds
  diagFactor:= round(sqrt(2)/2*precisionFactor);
  denom := 4*precisionFactor+4*diagFactor;
  currentAverage := 0;
  for yy := 1 to watersizey - 1 do begin
    pw := @w[yy,1];
    for xx := 1 to watersizex - 1 do begin
      currentAverage += pw^.h;

      hgtSurr := (precisionFactor*(pw[-1].h + pw[1].h + pw[-(watersizex+1)].h + pw[watersizex+1].h)
      + diagFactor*(pw[-watersizex-2].h + pw[watersizex+2].h + pw[-watersizex].h + pw[watersizex].h)
      + denom div 2) //rounding of divison
      div denom; //average

  {   hgtSurr := (precisionFactor*((pw-1)^.h + (pw+1)^.h + (pw-(watersizex+1))^.h + (pw+(watersizex+1))^.h)
     + diagFactor*((pw-(watersizex+1)-1)^.h + (pw+(watersizex+1)+1)^.h + (pw-(watersizex+1)+1)^.h + (pw+(watersizex+1)-1)^.h)
     + denom div 2) //rounding of divison
     div denom; //average   }

      hgtDiff := hgtSurr - pw^.h;     //difference x,y and surrounding fields
      if hgtDiff = 0 then
      begin
        inc(pw);
        continue;
      end;

      a := hgtDiff * gravity; //multiply by gravity (this is premultiplied by precisionFactor)
      pw^.v := (pw^.v * (precisionFactor - friction) div precisionFactor + a);
      inc(pw);
    end;
  end;
  currentAverage:= currentAverage div ((watersizex-1)*(watersizey-1));
  //move water (needs to be separate to avoid interference and recursive effect on successive values)
  for yy := 1 to watersizey - 1 do begin
    pw := @w[yy,1];
    for xx := 1 to watersizex - 1 do begin
      pw^.h += pw^.v div precisionFactor - currentAverage;
      inc(pw);
    end;
  end;

  // draw in TBGRABitmap
  //draw the water heights except borders at 0 and watersize
  P := FBGRABuffer.Data;
  c := WaterColor.Value;
  for yy := 1 to watersizey-1 do begin
    p := FBGRABuffer.ScanLine[yy-1];
    pw := @w[yy,1];
    for xx := 1 to watersizex-1 do begin

      //compute slope
      hxy := (pw^.h-(pw-(watersizex+1)-1)^.h)*16 div precisionFactor;
      //reflect light for values between 0 and 300
      if hxy > 150 then hxy := 300-hxy;
      if hxy < 0 then hxy := 0;

      //variation also depending on height
      hxy += pw^.h div (precisionFactor*2);

      p^.blue := clamp(c.blue+hxy, 0, 255);
      p^.green := clamp(c.green+hxy, 0, 255);
      p^.red := clamp(c.red+hxy, 0, 255);
      if FWaterIsTransparent
        then p^.alpha := clamp(hxy*3, 0, c.alpha)
        else p^.alpha := c.alpha;

      Inc(p);
      inc(pw);
    end;
  end;
// BGRABuffer.InvalidateBitmap;
  // replace pixels in texture
  FParentScene.TexMan.Replace(FTexture, FBGRABuffer);
end;

procedure TWaterSurface.DoDraw;
var modelArea: TRect;
begin
  modelArea := Bounds(Round(X.Value), Round(Y.Value), Width, Height);
  FParentScene.TexturedMVTriangleRenderer.Prepare(ptTriangles, FTexture, 0, FBlendMode);
  FParentScene.TexturedMVTriangleRenderer.PushQuad3(modelArea, FTexture^.FramesCoord[0], FlipToIndex,
              NIL, FComputedOpacity, FComputedTint);
end;

constructor TWaterSurface.Create(aParentScene: TOGLCScene);
var ima: TBGRABitmap;
begin
  inherited Create;
  FParentScene := aParentScene;
  ima := TBGRABitmap.Create(watersizex-1, watersizey-1, BGRA(0,0,0));
  FTexture := FParentScene.TexMan.Add(ima);
  ima.Free;

  DropAmount:= CreateBoundedFParam(0, 10);
  DropAmount.Value := 0; // no rain
  FRainRadius := 5;
  FRainHeight := 50;
  FC := 10;
  FWaterIsTransparent := FALSE;
  WaterColor := TBGRAParam.Create;
  WaterColor.Value := BGRA(10,100,210);
  FBGRABuffer := TBGRABitmap.Create(watersizex-1, watersizey-1, BGRABlack);

  FCornerOffset := TQuadParamF.Create;
end;

destructor TWaterSurface.Destroy;
begin
  FParentScene.TexMan.Delete(FTexture);
  FreeAndNil(DropAmount);
  FreeAndNil(WaterColor);
  FreeAndNil(FBGRABuffer);
  FreeAndNil(FCornerOffset);
  inherited Destroy;
end;

procedure TWaterSurface.SetSize(aWidth, aHeight: integer);
var dx, dy: single;
begin
  FWidth := aWidth;
  FHeight := aHeight;
  if FWidth < 1 then FWidth := 1;
  if FHeight < 1 then FHeight := 1;


  dx := (FWidth-watersizex)*0.5;
  dy := (FHeight-watersizey)*0.5;
  FCornerOffset.TopLeft.Value := PointF(-dx, -dy);
  FCornerOffset.TopRight.Value := PointF(dx,-dy);
  FCornerOffset.BottomLeft.Value := PointF(-dx,dy);
  FCornerOffset.BottomRight.Value := PointF(dx,dy);
end;

procedure TWaterSurface.WaterReset;
var xx, yy: integer;
begin
  for yy:=0 to watersizey do
    for xx:=0 to watersizex do
    begin
      w[yy,xx].h := 0;
      w[yy,xx].v := 0;
    end;
end;

procedure TWaterSurface.WaterDrop(aX, aY, r, dropHeight: TWaterSurfaceValue);
var xb,yb, xx, yy: TWaterSurfaceValue;
    d,f: single;
begin
  xx := round( aX / FWidth * watersizex );
  yy := round( aY / FHeight * watersizey );

  f := Pi/r;
  for xb := xx-r to xx+r do
    for yb := yy-r to yy+r do
      if (xb >= 0) and (xb <= watersizex)
      and (yb >= 0) and (yb <= watersizey) then
      begin
        //distance to the center of the waterdrop
        d := sqrt(sqr(xb-xx)+sqr(yb-yy))/r;
        if d < 1 then
          w[yb,xb].h += round(cos((1-d)*f)   //wave form
                   *dropHeight*precisionFactor   //height
                   *(1-d));     //attenuation
      end;
end;

procedure TWaterSurface.WaterDrop(aX, aY: single);
begin
  WaterDrop(round(aX), round(aY), FRainRadius, random(FRainHeight) + FRainHeight div 2);
end;

{$endif oglcIMPLEMENTATION}

