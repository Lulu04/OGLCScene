{$ifdef oglcINTERFACE}
type

  TValue = NativeInt;
const
  watersizex = 640;
  watersizey = 480;
  precisionFactor = 256; //use power of two to improve compiler optimisation
  gravity = precisionFactor * 2 div 10;
  friction = precisionFactor * 1 div 256;
type
  PWaterPixel = ^TWaterPixel;
  TWaterPixel = packed record
    h,         //height (with average equal to zero)
    v: TValue; //speed (multiplied by precisionFactor)
  end;

{ TWaterSurface }

TWaterSurface = class( TSimpleSurfaceWithEffect )
private
   w : packed array[0..watersizey, 0..watersizex] of TWaterPixel;
   FC: single;
   FRainRadius,
   FRainHeight: integer;
   FWaterIsTransparent: boolean;
   function clamp(AValue: TValue; AMin,AMax: TValue): TValue; inline;
private
   FTexture: PTexture;
   FWidth, FHeight: integer;
protected
   function GetWidth: integer; override;
   function GetHeight: integer; override;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single );  override;
public
   constructor Create;
   Destructor Destroy; override;
   procedure WaterReset;

   procedure SetSize( aWidth, aHeight: integer );
   procedure WaterDrop(aX, aY, r, dropHeight: TValue); overload;
   procedure WaterDrop( aX, aY: single ); overload;
public
   RainAmount: TBoundedFParam;  // 0=no rain    10=max rain
   WaterColor: TBGRAParam;
   property WaterIsTransparent: boolean read FWaterIsTransparent write FWaterIsTransparent; // if true, draw only waterdrop and not static water
   property RainRadius: integer read FRainRadius write FRainRadius;
   property RainHeight: integer read FRainHeight write FRainHeight;
end;



{ TSnow }
const MAX_FLAKE = 1000;
type
PFlake = ^TStructFlake;
TStructFlake = record
  fx         : single ;
  fy         : single ;
  sx         : single ;
  sy         : single ;
  anglex     : single ;
  amplitudex : single ;
  xdraw      : single ;
 end;

TArrayOfFlake = array of TStructFlake;

TSnow = class( TSimpleSurfaceWithEffect )
private
   FTexture: array[0..2] of PTexture; // 3 sizes of flakes  0= smallest  1=middle  2=normal size
   FWidth, FHeight: integer;
   FFlakeArray: array[0..2] of TArrayOfFlake;
   function GetOneTypeFlakeCount: integer;
   procedure InitFlake( aPFlake: PFlake );
   procedure InitLittleFlake( aPFlake: PFlake );
   procedure InitMiddleFlake( aPFlake: PFlake );
   procedure InitBigFlake( aPFlake: PFlake );
protected
   function GetWidth: integer; override;
   function GetHeight: integer; override;
public
   constructor Create;
   Destructor Destroy; override;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single );  override;
public
   FlakeAmount: TBoundedFParam;  // 0=no snow    1000=max
   FlakeSpeed: TPointFParam;     // speed on both axis in pixels per second
   HAmplitude: TFParam;          // horizontal amplitude in pixel
   Gravity: TPointFParam;

   procedure SetSize( aWidth, aHeight: integer );
   // set the image flake from a file
   // if aSizeCoeff<1 reduce the image
   // aSizeCoeff=1 use the same size
   // aSizeCoeff>1 enlarge the image
   procedure UseTexture( aTextureFile: string; aSizeCoeff: single=1.0 ); overload;
   procedure UseTexture( aImage: TBGRABitmap ); overload;
end;


{
 TStrand = record
   tex      : PTexture;
   percent  : single; //
   min_scale: single;
   max_scale: single;
 end;
 PStrand = ^TStrand;
 TArrayOfStrand= array of TStrand;


 TGrassArea= class( TSimpleSurfaceWithEffect )
 private
  FStrandList: TArrayOfStrand;
  FSurfaceList: TSurfaceList;     // list of TSimpleSurfaceWithEffect;
   // Internal procedure wrapper
   procedure DoUpdate ( const aElapsedTime : double ) ;
   procedure DoDraw( const aLayerPercentOpacity : single ) ;
   procedure DoDestroy ;
 public
   Constructor Create( aX, aY: single; aWidth, aHeight: integer );
   Destructor Destroy; override;

   procedure AddGrassTexture( aFilename: string; aPercentage: single=100.0; aMinScale: single=1.0; aMaxScale: single= 1.0);  // example: one grass type 60%, and another grass type 40%
   procedure AddGrassTexture(aTex: PTexture; aPercentage: single; aMinScale: single; aMaxScale: single);
   procedure RandomizeGrass( aGrassCountPerLine, aLineCount: integer; aDuration: single ); // duration= time for one undulation

 end;

 TBubble= class
 end;
}

{
TOGLCStormLightningNode=record
  Pt: TPointF;
  Po: TPolarCoor;
  Alpha: single;
end;
POGLCStormLightningNode=^TOGLCStormLightningNode;
ArrayOfStormLightningNode = array of TOGLCStormLightningNode;

TOGLCStormLightningSecondaryPath=record
  FirstNode: POGLCStormLightningNode;
  Nodes: ArrayOfStormLightningNode;
  IsMain: boolean;
end;


{ ToglcStormLightning }

ToglcStormLightning = class(TSimpleSurfaceWithEffect)
private
  FWidth, FHeight: integer;
  FMainColor, FSecondaryColor: TBGRAPixel;
  FNodeAngleAperture,
  FMaxLineWidth: single;
  FMainTexture,
  FSecondaryTexture: PTexture;
  FMainNodes: ArrayOfStormLightningNode;
  FSecondaryNodes: array of ArrayOfOGLCStormLightningNode;
  procedure CreateTextures;
  procedure ComputeDrawing;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Update( const aElapsedTime: single ); override;
  procedure Draw( const aLayerPercentOpacity: single );  override;
public
  Constructor Create( aStartPt, // scene coordinates from where the lightning start
                      aTargetPt: TPointF; // scene coordinates where the lightning ends
                      aMainNodeCount,                // must be >2
                      aSecondaryNodeCount: integer;  // must be >=0
                      aMainColor, // color of the main lightning
                      aSecondaryColor: TBGRAPixel; // color of the secondary lightning
                      aNodeAngleAperture, // >=0
                      aMaxLineWidth: single); // must be >0
  Destructor Destroy; override;

end;
}
{$endif oglcINTERFACE}



{$ifdef oglcIMPLEMENTATION}

{ ToglcStormLightning }
{
procedure ToglcStormLightning.CreateTextures;
var ima: TBGRABitmap;
    procedure DoGradient(aColor: TBGRAPixel);
    var
      xL, xR: integer;
      opacityStep, accualpha: single;
      c: TBGRAPixel;
    begin
     ima.Fill(aColor);
     if ima.Width<2 then exit;

     xL:= round(ima.Width/2);
     xR:=xL;
     opacityDeltaStep:=aColor.alpha/(ima.Width-xR);
     c:=aColor;

     xR:= ima.Width-1;
     accualpha:= opacityDeltaStep;
     for xL:=0 to round(ima.Width/2)-1 do begin
      c.alpha := round(accualpha);
      ima.Pixels[xL,0]:=c;
      ima.Pixels[xR,0]:=c;
      accualpha+=opacityDeltaStep;
      dec(xR);
     end;
    end;

begin
 w:=round(FMaxLineWidth*10);
 if w>200 then w:=200;
 if w<1 then w:=1;

 ima:=TBGRABitmap.Create(w, 1,);
 DoGradient( FMainColor );
 FMainTexture:=TextureManager.Add(ima);

 DoGradient( FSecondaryColor );
 FSecondaryTexture:=TextureManager.Add(ima);
 ima.Free;
end;

procedure ToglcStormLightning.ComputeDrawing;
begin

end;

function ToglcStormLightning.GetWidth: integer;
begin
 Result:=FWidth;
end;

function ToglcStormLightning.GetHeight: integer;
begin
 Result:=FHeight;
end;

procedure ToglcStormLightning.Update(const aElapsedTime: single);
begin
 inherited Update(aElapsedTime);
end;

procedure ToglcStormLightning.Draw(const aLayerPercentOpacity: single);
begin
 if not FVisible then exit;
 if Opacity.Value=0 then exit;

 SetBlendMode( FBlendMode );
 PushAndPrepareMatrix;
 TextureManager.DisableTextureUsage;
 glLineWidth( 3.5 );    // 3.5

 c := FMainColor;
 c.alpha := round( c.alpha*aLayerPercentOpacity*Opacity.pcValue );

 p1 := PointF( FPtArray[0].CenterX, FPtArray[0].CenterY );
 if FShakerEnable then p1 := p1.Add(PointF(random(FShakerAmplitude),random(FShakerAmplitude)));
 i := 1;
 repeat
   p2 := PointF( FPtArray[i].CenterX, FPtArray[i].CenterY );
   if FShakerEnable then p2 := p2.Add(PointF(random(FShakerAmplitude),random(FShakerAmplitude)));
   if not FPtArray[i].IsJump then begin
     if (Mode = pdmAll) or
        ((Mode = pdmOnlyPositionned) and FPtArray[i].IsGoodPositionned)
       then DrawLine( p1, p2, c, FLineWidth, aLayerPercentOpacity);
   end;
   p1 := p2;
   inc( i );
 until i=Count;

 DrawChildSurfaces( aLayerPercentOpacity * Opacity.pcValue );
 PopMatrix;
end;

constructor ToglcStormLightning.Create(aStartPt, aTargetPt: TPointF;
  aMainNodeCount, aSecondaryNodeCount: integer; aMainColor,
  aSecondaryColor: TBGRAPixel; aNodeAngleAperture, aMaxLineWidth: single);
begin
 FMainColor := MainColor;
 FSecondaryColor:=aSecondaryColor;
 FNodeAngleAperture:=aNodeAngleAperture;

 if aMaxLineWidth<1 then aMaxLineWidth:=1;
 if aMaxLineWidth>80 then aMaxLineWidth:=80;
 FMaxLineWidth:=aMaxLineWidth;

 if aMainNodeCount<2 then aMainNodeCount:=2;
 if aSecondaryNodeCount<0 then aSecondaryNodeCount:=0;

 CreateTextures;
end;

destructor ToglcStormLightning.Destroy;
begin
 inherited Destroy;
end;
}
{ TSnow }

function TSnow.GetOneTypeFlakeCount: integer;
begin
 Result := round ( FlakeAmount.Value/3 );
end;

procedure TSnow.InitFlake(aPFlake: PFlake);
begin
 aPFlake^.fx := random ( Width );
 aPFlake^.fy := random( Height );
 aPFlake^.anglex := Random( 360 );
end;

procedure TSnow.InitLittleFlake(aPFlake: PFlake);
begin
 InitFlake( aPFlake );
 aPFlake^.sx := FlakeSpeed.x.Value * 0.4;
 aPFlake^.sy := Random ( 40 )*0.1 + FlakeSpeed.y.Value + 2;
 aPFlake^.amplitudex := HAmplitude.Value * 0.4;
end;

procedure TSnow.InitMiddleFlake(aPFlake: PFlake);
begin
 InitFlake( aPFlake );
 aPFlake^.sx := FlakeSpeed.x.Value * 0.6;
 aPFlake^.sy := Random ( 20 )*0.1 + FlakeSpeed.y.Value + 1;
 aPFlake^.amplitudex := HAmplitude.Value * 0.6;
end;

procedure TSnow.InitBigFlake(aPFlake: PFlake);
begin
 InitFlake( aPFlake );
 aPFlake^.sx := FlakeSpeed.x.Value;
 aPFlake^.sy := FlakeSpeed.y.Value;
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

 FlakeAmount := CreateBoundedFParam( 0, 1000, FALSE );

 FlakeSpeed := TPointFParam.Create;
 FlakeSpeed.Value := PointF(10, 10);

 Gravity := TPointFParam.Create;

 HAmplitude := TFParam.Create;
end;

destructor TSnow.Destroy;
begin
 FlakeAmount.Free;
 FlakeSpeed.Free;
 Gravity.Free;
 HAmplitude.Free;

 if Length(FTexture)=3 then begin
  TextureManager.Delete(FTexture[0]);
  TextureManager.Delete(FTexture[1]);
  TextureManager.Delete(FTexture[2]);
 end;

 inherited Destroy;
end;

procedure TSnow.Update(const aElapsedTime: single);
var i, j, z, fc: integer;
begin
 if FFreeze then exit;
 inherited Update(aElapsedTime);

 FlakeAmount.OnElapse( aElapsedTime );
 FlakeSpeed.OnElapse( aElapsedTime );
 Gravity.OnElapse( aElapsedTime );
 HAmplitude.OnElapse( aElapsedTime );

 fc := GetOneTypeFlakeCount;

 // adjust array according flake's count
 for i:=0 to 2 do
  if Length( FFlakeArray[i] ) < fc then
  begin
   z := Length( FFlakeArray[i] );
   SetLength( FFlakeArray[i], fc );
   for j:=z to fc-1 do
    case i of
      0: InitLittleFlake( @FFlakeArray[i][j] );
      1: InitMiddleFlake( @FFlakeArray[i][j] );
      2: InitBigFlake( @FFlakeArray[i][j] );
    end;
  end else
  if Length( FFlakeArray[i] ) > fc
    then SetLength( FFlakeArray[i], fc );

 // update each flake
 for i:=0 to 2 do
  for j:=0 to Length( FFlakeArray[i] )-1 do
   with FFlakeArray[i][j] do
     begin

      fx := fx + Gravity.x.Value * aElapsedTime;
      if fx < 0
        then fx := fx + Width
        else if fx > Width
               then fx := fx - Width;

      fy := fy + ( FlakeSpeed.y.Value + Gravity.y.Value ) * aElapsedTime;
      if fy > Height then fy := fy - Height;

      anglex := anglex + sx * aElapsedTime;
      if anglex > 360 then anglex := anglex - 360;

      xdraw := fx + sin( anglex ) * amplitudex;

     end;

end;

procedure TSnow.Draw(const aLayerPercentOpacity: single);
var i: integer;
    a: byte;
begin
 if not Visible then exit;
 PushAndPrepareMatrix;
 SetBlendMode( FBlendMode );

 a := round( Opacity.Value*aLayerPercentOpacity );

 for i:=0 to Length( FFlakeArray[0] )-1 do
  with FFlakeArray[0][i] do DrawTexture( FTexture[0], 0, xdraw,
                                         fy, 0, a, Tint.Value );

 for i:=0 to Length( FFlakeArray[1] )-1 do
  with FFlakeArray[1][i] do DrawTexture( FTexture[1], 0, xdraw,
                                         fy, 0, a, Tint.Value );

 for i:=0 to Length( FFlakeArray[2] )-1 do
  with FFlakeArray[2][i] do DrawTexture( FTexture[2], 0, xdraw,
                                         fy, 0, a, Tint.Value );
 PopMatrix;
end;

procedure TSnow.SetSize(aWidth, aHeight: integer);
begin
 FWidth := aWidth;
 FHeight := aHeight;
end;

procedure TSnow.UseTexture(aTextureFile: string; aSizeCoeff: single=1.0);
var ima,ima1: TBGRABitmap;
    i: integer;
begin
 if FTexture[0] <> NIL
   then for i:=0 to 2 do TextureManager.Delete( FTexture[i] );

 ima := TBGRABitmap.Create( aTextureFile );
 ima1:=ima.Resample(round(ima.Width*aSizeCoeff), round(ima.Height*aSizeCoeff)) as TBGRABitmap;
 UseTexture( ima1 );
 ima.Free;
 ima1.Free;
end;

procedure TSnow.UseTexture(aImage: TBGRABitmap);
var ima: TBGRABitmap;
begin
 FTexture[2] := TextureManager.Add( aImage );

 ima := TBGRABitmap( aImage.Resample( round(aImage.Width*0.6), round(aImage.Height*0.6) ));
 FTexture[1] := TextureManager.Add( ima );
 ima.Free;

 ima := TBGRABitmap( aImage.Resample( round(aImage.Width*0.4), round(aImage.Height*0.35) ));
 FTexture[0] := TextureManager.Add( ima );
 ima.Free;
end;

{ TWaterSurface }

function TWaterSurface.clamp(AValue: TValue; AMin, AMax: TValue): TValue;
begin
 if AValue <= AMin then
     result := AMin
 else if AValue >= AMax then
     result := AMax
 else
   result := AValue;
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
var xx, yy, hgtSurr, hgtDiff, a : TValue;
    diagFactor, denom: TValue;
    pw: PWaterPixel;
    currentAverage: NativeInt;
    hxy : TValue;
    BGRABuffer : TBGRABitmap;
    P : PBGRAPixel;

begin
 inherited Update( aElapsedTime );
 if FFreeze then exit;

 RainAmount.OnElapse( aElapsedTime );
 if RainAmount.Value > 0.0
   then begin
     FC += aElapsedTime;
     if FC >= ( 10 - RainAmount.Value ) / 10 * 3  // water drop fall every [0..3] seconds
      then begin
       xx := random( FWidth - 10 ) + 5;
       yy := random( FHeight - 10 ) + 5;
       WaterDrop( xx, yy, FRainRadius, random( FRainHeight ) + FRainHeight div 2 );
       FC := 0;
      end;
   end;

 WaterColor.OnElapse( aElapsedTime );

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

     hgtSurr := (precisionFactor*((pw-1)^.h + (pw+1)^.h + (pw-(watersizex+1))^.h + (pw+(watersizex+1))^.h)
     + diagFactor*((pw-(watersizex+1)-1)^.h + (pw+(watersizex+1)+1)^.h + (pw-(watersizex+1)+1)^.h + (pw+(watersizex+1)-1)^.h)
     + denom div 2) //rounding of divison
     div denom; //average

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
 BGRABuffer := TBGRABitmap.Create(watersizex-1, watersizey-1, BGRABlack);
 P := BGRABuffer.Data;
 for yy := 1 to watersizey-1 do begin
   p := BGRABuffer.ScanLine[yy-1];
   pw := @w[yy,1];
   for xx := 1 to watersizex-1 do begin

     //compute slope
     hxy := (pw^.h-(pw-(watersizex+1)-1)^.h)*16 div precisionFactor;
     //reflect light for values between 0 and 300
     if hxy > 150 then hxy := 300-hxy;
     if hxy < 0 then hxy := 0;

     //variation also depending on height
     hxy += pw^.h div (precisionFactor*2);

     p^.blue := clamp(round(WaterColor.blue.Value) +hxy,0,255);
     p^.green := clamp(round(WaterColor.green.Value)+hxy,0,255);
     p^.red := clamp(round(WaterColor.red.Value)+hxy,0,255);
     if FWaterIsTransparent
       then p^.alpha := clamp(hxy*3,0,round(WaterColor.alpha.Value))
       else p^.alpha := round(WaterColor.alpha.Value);

     Inc(p);
     inc(pw);
   end;
 end;
 BGRABuffer.InvalidateBitmap;
 // replace pixels in texture
 TextureManager.Replace( FTexture, BGRABuffer );
 BGRABuffer.Free;
end;

procedure TWaterSurface.Draw(const aLayerPercentOpacity: single);
begin
if Visible then DrawSurface( FTexture, 0, aLayerPercentOpacity );
end;

constructor TWaterSurface.Create;
var ima: TBGRABitmap;
begin
 inherited Create;
 ima := TBGRABitmap.Create( watersizex-1, watersizey-1, BGRA(0,0,0) );
 FTexture := TextureManager.Add( ima );
 ima.Free;

 WaterReset;
 RainAmount:= CreateBoundedFParam( 0, 10 );
 RainAmount.Value := 0; // no rain
 FRainRadius := 5;
 FRainHeight := 50;
 FC := 10;
 FWaterIsTransparent := FALSE;
 WaterColor  := TBGRAParam.Create;
 WaterColor.Value := BGRA(10,100,210);
end;

destructor TWaterSurface.Destroy;
begin
 TextureManager.Delete( FTexture );
 FreeAndNil( RainAmount );
 FreeAndNil( WaterColor );
 inherited Destroy;
end;

procedure TWaterSurface.SetSize( aWidth, aHeight: integer );
begin
 FWidth := aWidth;
 FHeight := aHeight;
 if FWidth < 1 then FWidth := 1;
 if FHeight < 1 then FHeight := 1;
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

procedure TWaterSurface.WaterDrop(aX, aY, r, dropHeight: TValue);
var xb,yb, xx, yy: TValue;
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
 WaterDrop( round( aX ), round( aY ), FRainRadius, random( FRainHeight ) + FRainHeight div 2 );
end;




{ TGrassArea }

{
 constructor TGrassArea.Create(aX, aY: single; aWidth, aHeight: integer);
 begin
   inherited Create;
   WrapperUpdate := @DoUpdate ;
   WrapperDraw := @DoDraw ;
   WrapperDestroy := @DoDestroy ;
   FX.Value := aX;
   FY.Value := aY;
   FWidth := aWidth;
   FHeight := aHeight;
   FSurfaceList:= TSurfaceList.Create;
 end;

 destructor TGrassArea.Destroy;
 begin
   inherited Destroy;
 end;

 procedure TGrassArea.DoUpdate(const aElapsedTime: double);
 var i: integer;
     s: TSimpleSurfaceWithEffect ;
 begin
  if FFreeze then exit;
  for i:=0 to FSurfaceList.SurfaceCount-1 do
   begin
    s := TSimpleSurfaceWithEffect( FSurfaceList[i] );
    s.DoUpdate( aElapsedTime );
   end;
 end;

 procedure TGrassArea.DoDraw(const aLayerPercentOpacity: single);
 var i: integer;
     s: TSimpleSurfaceWithEffect ;
 begin
  for i:=0 to FSurfaceList.SurfaceCount-1 do
   begin
    s := TSimpleSurfaceWithEffect( FSurfaceList[i] );
    s.DrawSurface( FStrandList[s.Tag1].tex ,aLayerPercentOpacity );
   end;
 end;

 procedure TGrassArea.DoDestroy;
 begin
  setLength( FStrandList, 0 ) ;
  FSurfaceList.Free;
  self.Free;
 end;

 procedure TGrassArea.AddGrassTexture(aFilename: string; aPercentage: single;
   aMinScale: single; aMaxScale: single);
 begin
  AddGrassTexture( TextureManager.Add( aFilename ), aPercentage, aMinScale, aMaxScale );
 end;

 procedure TGrassArea.AddGrassTexture(aTex: PTexture; aPercentage: single; aMinScale: single; aMaxScale: single);
 var i: integer;
 begin
  SetLength( FStrandList, Length(FStrandList)+1 );
  i := Length( FStrandList )-1;
  FStrandList[i].tex:= aTex;
  FStrandList[i].percent:= aPercentage/100;
  FStrandList[i].min_scale:= aMinScale;
  FStrandList[i].max_scale:= aMaxScale;
 end;

 procedure TGrassArea.RandomizeGrass(aGrassCountPerLine, aLineCount: integer; aDuration: single);
 var i,j,l,c: integer;
     xx, yy, sc,deltascale,invertsc,deltaSkew: single;
     s:TSimpleSurfaceWithEffect;
 begin
  yy := FY.Value;
  for l:=1 to aLineCount do
   begin
    for i:=0 to Length( FStrandList )-1 do
     begin
      c := round ( aGrassCountPerLine*FStrandList[i].percent );
      deltascale := FStrandList[i].max_scale - FStrandList[i].min_scale;
      for j:=1 to c do
       begin
        xx := random(FWidth*10000)/10000+FX.Value;
        if  deltascale <> 0
          then sc := FStrandList[i].min_scale+ (random( round(deltascale*10000) )/10000)
          else sc := FStrandList[i].min_scale;
        s := TSimpleSurfaceWithEffect.Create;
        s.SetCoordinate(xx, yy);
        s.FWidth:=FStrandList[i].tex^.TextureWidth;
        s.FHeight:=FStrandList[i].tex^.TextureHeight;
        s.Scale( sc, 0 );
        invertsc := 1-sc;
        s.Y := yy+invertsc*FStrandList[i].tex^.ImageHeight/2;//   s.FHeight;
        deltaSkew := 5 + 5*sc ;
        s.SkewRepeated(-deltaSkew,deltaSkew,0,0,rmGoAndBack,aDuration,mSinusoid1,mSinusoid1,0);
        s.Tag1:=i; // tag=index of texture in FStrandList[]
     //   s.WrapperDestroy:=;
        FSurfaceList.Add(s);
       end;
     end;
    yy+= FHeight/aLineCount;
   end;
 end;
}

{$endif oglcIMPLEMENTATION}

