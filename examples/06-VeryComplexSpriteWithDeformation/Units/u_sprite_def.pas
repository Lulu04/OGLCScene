unit u_sprite_def;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene, BGRABitmap, BGRABitmapTypes;
type


TBaseComplexSprite = class(TSprite)
  function CreateChildSprite(aTex: PTexture; aZOrder: integer): TSprite;
  function CreateChildPolar(aTex: PTexture; aZOrder: integer): TPolarSprite;
  function CreateChildDeformationGrid(aTex: PTexture; aZOrder: integer): TDeformationGrid;
end;

{ TBaseComplexContainer }
// add some functionalities to TSpriteContainer
TBaseComplexContainer = class(TSpriteContainer)
private
  FDeltaYToBottom, FDeltaYToTop: single;
  FBodyWidth, FBodyHeight: integer;
public
  function CreateChildSprite(aTex: PTexture; aZOrder: integer): TSprite;
  function CreateChildPolar(aTex: PTexture; aZOrder: integer): TPolarSprite;
  function CreateChildDeformationGrid(aTex: PTexture; aZOrder: integer): TDeformationGrid;
public
  function GetYTop: single;
  function GetYBottom: single;
  function CheckCollisionWith(aX, aY, aWidth, aHeight: single): boolean;
  // init in descendent classes
  property DeltaYToTop: single read FDeltaYToTop write FDeltaYToTop;
  property DeltaYToBottom: single read FDeltaYToBottom write FDeltaYToBottom;
  property BodyWidth: integer read FBodyWidth write FBodyWidth;
  property BodyHeight: integer read FBodyHeight write FBodyHeight;
end;


TLRFaceType = (lrfSmile, lrfHappy, lrfNotHappy, lrfWorry, lrfBroken, lrfVomit);

{ TLRBaseFace }

TLRBaseFace = class(TBaseComplexSprite)
private
  FOriginCenterCoor: TPointF;
  FFaceType: TLRFaceType;
  FEyeMaxDistance: single;
  function EyeCanBlink: boolean;
public
  LeftEye, RightEye: TPolarSprite;
  HairLock: TSprite;
  procedure SetFaceType(AValue: TLRFaceType); virtual;
  procedure SetDeformationOnHair(aHair: TDeformationGrid);
  procedure SetWindSpeedOnHair(aHair: TDeformationGrid; aValue: single);
public
  constructor Create(aTexture: PTexture);
  procedure ProcessMessage({%H-}UserValue: TUserMessageValue); override;
  property OriginCenterCoor: TPointF write FOriginCenterCoor;
  property FaceType: TLRFaceType read FFaceType write SetFaceType;
  property EyeMaxDistance: single read FEyeMaxDistance write FEyeMaxDistance;
end;


TLRFace = class(TLRBaseFace)
private
  Hair: TDeformationGrid;
  MouthNotHappy, MouthOpen, MouthSmile,
  WhiteBG: TSprite;
  protected
  procedure SetFlipH(AValue: boolean); override;
  procedure SetFlipV(AValue: boolean); override;
public
  procedure SetFaceType(AValue: TLRFaceType); override;
  constructor Create;
end;


{ TLRDress }

TLRDress = class(TDeformationGrid)
private
  procedure SetDeformation;
public
  constructor Create(aDressTexture: PTexture);
  procedure SetWindSpeed(AValue: single);
end;

{ TLRFrontView }

TLRFrontView = class(TBaseComplexContainer)
private
  FHood: TDeformationGrid;
  FDress: TLRDress;
  FBasket: TSprite;
  procedure SetDeformationOnHood;
protected
  procedure SetFlipH(AValue: boolean); override;
  procedure SetFlipV(AValue: boolean); override;
public
  Face: TLRFace;
  RightArm, LeftArm, RightLeg, LeftLeg: TSprite;
  constructor Create;
  procedure ProcessMessage({%H-}UserValue: TUserMessageValue); override;
  procedure ToogleFlipH;
  procedure ToogleFlipV;
  procedure HideBasket;
  procedure SetWindSpeed(AValue: single);
  procedure MoveArmsAsWinner;
  procedure SetCoordinateByFeet(aCenterX, aY: single);
end;

procedure LoadLRFaceTextures(aAtlas: TOGLCTextureAtlas);
procedure LoadLRFrontViewTextures(aAtlas: TOGLCTextureAtlas);

// The design of the character is done with Inkscape on a working page with size of 1024x768  (aspect ratio 4/3)
// To keep proportion on a screen with different resolution (or PPI) wa have to scale the size of our sprites.
function ScaleW(AValue: integer): integer;
function ScaleH(AValue: integer): integer;

var AdditionnalScale: single=1.0;

implementation
uses Forms, u_common;

var
// textures for Little Red Face
  texLRFace,
//  texLRFaceBroken,
  texLRFaceBGWhite,
  texLRFaceEye,
  texLRFaceHairLock,
  texLRFaceHair,
  texLRFaceMouthHurt,
  texLRFaceMouthOpen,
  texLRFaceMouthSmile: PTexture;

  texLRFrontViewHood,
  texLRFrontViewDress,
  texLRFrontViewLeftArm, texLRFrontViewRightArm,
  texLRFrontViewLeftLeg, texLRFrontViewRightLeg,
  texLRFrontViewBasket: PTexture;


procedure LoadLRFaceTextures(aAtlas: TOGLCTextureAtlas);
var path: string;
begin
  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator+'LittleRed'+DirectorySeparator;
  texLRFace := aAtlas.AddMultiFrameImageFromSVG([path+'LittleRedFaceEyeOpen.svg',
                                                 path+'LittleRedFaceEyeClose.svg',
                                                 path+'LittleRedFaceBroken.svg'], ScaleW(57), -1, 1, 3, 1);
  texLRFaceBGWhite := aAtlas.AddFromSVG(path+'LittleRedFaceBGWhite.svg', ScaleW(44), -1);
  texLRFaceEye := aAtlas.AddFromSVG(path+'LittleRedFaceEye.svg', ScaleW(11), -1);
  texLRFaceHair := aAtlas.AddFromSVG(path+'LittleRedHair.svg', ScaleW(57), -1);
  texLRFaceHairLock := aAtlas.AddFromSVG(path+'LittleRedFaceHairLock.svg', ScaleW(6), ScaleH(14));
  texLRFaceMouthHurt := aAtlas.AddFromSVG(path+'LittleRedFaceMouthHurt.svg', ScaleW(26), -1);
  texLRFaceMouthOpen :=aAtlas.AddFromSVG(path+'LittleRedFaceMouthOpen.svg', ScaleW(27), -1);
  texLRFaceMouthSmile := aAtlas.AddFromSVG(path+'LittleRedFaceMouthSmile.svg', ScaleW(33), -1);
end;

procedure LoadLRFrontViewTextures(aAtlas: TOGLCTextureAtlas);
var path: string;
begin
  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator+'LittleRed'+DirectorySeparator;
  texLRFrontViewHood := aAtlas.AddFromSVG(path+'Hood.svg', ScaleW(87), -1);
  texLRFrontViewDress := aAtlas.AddFromSVG(path+'Dress.svg', ScaleW(62), -1);
  texLRFrontViewLeftArm := aAtlas.AddFromSVG(path+'LeftArm.svg', ScaleW(23), -1);
  texLRFrontViewRightArm := aAtlas.AddFromSVG(path+'RightArm.svg', ScaleW(22), -1);
  texLRFrontViewLeftLeg := aAtlas.AddFromSVG(path+'LeftLeg.svg', ScaleW(20), -1);
  texLRFrontViewRightLeg := aAtlas.AddFromSVG(path+'RightLeg.svg', ScaleW(20), -1);
  texLRFrontViewBasket := aAtlas.AddFromSVG(path+'Basket.svg', ScaleW(26), -1);
end;

function ScaleW(AValue: integer): integer;
begin
  Result := Round(FScene.Width*AValue/1024*AdditionnalScale);
end;

function ScaleH(AValue: integer): integer;
begin
  Result := Round(FScene.Height*AValue/768*AdditionnalScale);
end;

{ TBaseComplexContainer }

function TBaseComplexContainer.CreateChildSprite(aTex: PTexture; aZOrder: integer): TSprite;
begin
  Result := TSprite.Create(aTex, False);
  AddChild(Result, aZOrder);
  Result.ApplySymmetryWhenFlip := True;
end;

function TBaseComplexContainer.CreateChildPolar(aTex: PTexture; aZOrder: integer): TPolarSprite;
begin
  Result := TPolarSprite.Create(aTex, False);
  AddChild(Result, aZOrder);
  Result.ApplySymmetryWhenFlip := True;
end;

function TBaseComplexContainer.CreateChildDeformationGrid(aTex: PTexture; aZOrder: integer): TDeformationGrid;
begin
  Result := TDeformationGrid.Create(aTex, False);
  AddChild(Result, aZOrder);
  Result.ApplySymmetryWhenFlip := True;
end;

function TBaseComplexContainer.GetYTop: single;
begin
  Result := Y.Value - FDeltaYToTop;
end;

function TBaseComplexContainer.GetYBottom: single;
begin
  Result := Y.Value + FDeltaYToBottom;
end;

function TBaseComplexContainer.CheckCollisionWith(aX, aY, aWidth, aHeight: single): boolean;
var xx, yy, w, h: single;
begin
  xx := X.Value;
  yy := Y.Value;
  w := BodyWidth * 0.5;
  h := BodyHeight * 0.5;
  Result := not((aX > xx+w) or (aX+aWidth < xx-w) or (aY > yy+h) or (aY+aHeight < yy-h));
end;

{ TBaseComplexSprite }

function TBaseComplexSprite.CreateChildSprite(aTex: PTexture; aZOrder: integer): TSprite;
begin
  Result := TSprite.Create(aTex, False);
  AddChild(Result, aZOrder);
  Result.ApplySymmetryWhenFlip := True;
end;

function TBaseComplexSprite.CreateChildPolar(aTex: PTexture; aZOrder: integer): TPolarSprite;
begin
  Result := TPolarSprite.Create(aTex, False);
  AddChild(Result, aZOrder);
  Result.ApplySymmetryWhenFlip := True;
end;

function TBaseComplexSprite.CreateChildDeformationGrid(aTex: PTexture; aZOrder: integer): TDeformationGrid;
begin
  Result := TDeformationGrid.Create(aTex, False);
  AddChild(Result, aZOrder);
  Result.ApplySymmetryWhenFlip := True;
end;

{ TLRBaseFace }

function TLRBaseFace.EyeCanBlink: boolean;
begin
  Result := FFaceType <> lrfBroken;
end;

procedure TLRBaseFace.SetFaceType(AValue: TLRFaceType);
begin
  if (FFaceType = lrfVomit) then Tint.Alpha.ChangeTo(0, 0.5);

  FFaceType := AValue;
  case AValue of
    lrfSmile, lrfHappy, lrfNotHappy, lrfVomit: if Frame = 3 then Frame := 1;
    lrfBroken: Frame := 3;
  end;

  if AValue = lrfVomit then Tint.ChangeTo(BGRA(0,255,0,100), 0.5);
end;

procedure TLRBaseFace.SetDeformationOnHair(aHair: TDeformationGrid);
begin
  aHair.SetGrid(5, 5);
  aHair.ApplyDeformation(dtWaveH);
  aHair.DeformationSpeed.Value := PointF(1.5,1.6);
  aHair.Amplitude.Value := PointF(0.3,0.2);
  aHair.SetDeformationAmountOnRow(0, 0.4);
  aHair.SetDeformationAmountOnRow(1, 0.6);
  aHair.SetDeformationAmountOnRow(1, 0.8);

  aHair.SetTimeMultiplicatorOnRow(5, 1.5);
//aHair.ShowGrid:=true;
end;

procedure TLRBaseFace.SetWindSpeedOnHair(aHair: TDeformationGrid; aValue: single);
begin
  aHair.Amplitude.Value := PointF(0.3*aValue,0.2*aValue);
end;

constructor TLRBaseFace.Create(aTexture: PTexture);
begin
  inherited Create(aTexture, False);
  ApplySymmetryWhenFlip := True;
end;

procedure TLRBaseFace.ProcessMessage(UserValue: TUserMessageValue);
const _DELTA=0.03;
var v: single;
begin
  case UserValue of
    //Face moving
    0: begin
      v := random+1;
      MoveCenterTo(FOriginCenterCoor+PointF(random*Width*_DELTA-Width*_DELTA*0.5,
                                            random*Height*_DELTA-Height*_DELTA*0.5), v, idcSinusoid);
      PostMessage(0, v+1*random);
    end;

    // eye blink
    100: begin
      if EyeCanBlink then Frame := 2;
      PostMessage(101, 0.1);
    end;
    101: begin
      if EyeCanBlink then Frame := 1;
      PostMessage(100, 1+random+random*2);
    end;

    // eye move idle for portrait and LR4direction right-left view
    200: begin
      LeftEye.Polar.Angle.ChangeTo(0, 1, idcSinusoid);
      LeftEye.Polar.Distance.ChangeTo(FEyeMaxDistance, 1, idcSinusoid);
      RightEye.Polar.Angle.ChangeTo(0, 1, idcSinusoid);
      RightEye.Polar.Distance.ChangeTo(FEyeMaxDistance, 1, idcSinusoid);
      PostMessage(201, 3);
    end;
    201: begin
      LeftEye.Polar.Angle.ChangeTo(45, 2, idcSinusoid);
      RightEye.Polar.Angle.ChangeTo(45, 2, idcSinusoid);
      PostMessage(200, 4);
    end;

    // eye move idle for LR4direction front view
    210: begin
      v := random(360);
      LeftEye.Polar.Angle.ChangeTo(v, 2, idcSinusoid);
      RightEye.Polar.Angle.ChangeTo(v, 2, idcSinusoid);
      LeftEye.Polar.Distance.ChangeTo(FEyeMaxDistance, 3, idcSinusoid);
      RightEye.Polar.Distance.ChangeTo(FEyeMaxDistance, 3, idcSinusoid);
      PostMessage(211, 4+Random(4));
    end;
    211: begin
      LeftEye.Polar.Distance.ChangeTo(0, 3, idcSinusoid);
      RightEye.Polar.Distance.ChangeTo(0, 3, idcSinusoid);
      PostMessage(210, 4+Random(4));
    end;

    // lock hair swing
    300: begin
      v := random(50)*0.01+0.5;
      HairLock.Angle.ChangeTo(v*10, v, idcSinusoid);
      PostMessage(301, v);
    end;
    301: begin
      v := random(50)*0.01+0.5;
      HairLock.Angle.ChangeTo(-v*10, v, idcSinusoid);
      PostMessage(300, v);
    end;
  end;
end;

{ TLRFrontView }

procedure TLRFrontView.SetDeformationOnHood;
begin
  FHood.SetGrid(5, 5);
  FHood.ApplyDeformation(dtWaveH);
  FHood.Amplitude.Value := PointF(0.3, 0.2);
  FHood.DeformationSpeed.Value := PointF(5,5);
  FHood.SetDeformationAmountOnRow(0, 0.4);
  FHood.SetDeformationAmountOnRow(1, 0.4);
  FHood.SetDeformationAmountOnRow(2, 0.2);
  FHood.SetDeformationAmountOnRow(3, 0);
  FHood.SetDeformationAmountOnRow(4, 0.5);
  FHood.SetDeformationAmountOnRow(5, 1.0);
end;

constructor TLRFrontView.Create;
begin
  inherited Create(FScene);

  FDress := TLRDress.Create(texLRFrontViewDress);
  AddChild(FDress, 0);
  FDress.X.Value := -FDress.Width*0.5;
  FDress.BottomY := 0;
  FDress.Pivot := PointF(0.5, 1.0);

  FHood := CreateChildDeformationGrid(texLRFrontViewHood, 1);
  FHood.X.Value := -FHood.Width*0.55;
  FHood.BottomY := -FHood.Height*0.07;
  SetDeformationOnHood;

  Face := TLRFace.Create;
  FHood.AddChild(Face, 0);
  Face.CenterX := FHood.Width*0.52;
  Face.CenterY := FHood.Height*0.43;
  Face.OriginCenterCoor := Face.Center;

  RightArm := TSprite.Create(texLRFrontViewRightArm, False);
  FDress.AddChild(RightArm, 1);
  RightArm.SetCoordinate(FDress.Width*0.1, FDress.Height*0.25);
  RightArm.Pivot := PointF(0.2, 0.1);
  RightArm.ApplySymmetryWhenFlip := True;

  LeftArm := TSprite.Create(texLRFrontViewLeftArm, False);
  FDress.AddChild(LeftArm, 1);
  LeftArm.SetCoordinate(FDress.Width*0.5, FDress.Height*0.22);
  LeftArm.Pivot := PointF(0.8, 0.1);
  LeftArm.ApplySymmetryWhenFlip := True;

  RightLeg := CreateChildSprite(texLRFrontViewRightLeg, -1);
  RightLeg.SetCoordinate(-RightLeg.Width, -RightLeg.Height*0.25);

  LeftLeg := CreateChildSprite(texLRFrontViewLeftLeg, -1);
  LeftLeg.SetCoordinate(0, -LeftLeg.Height*0.25);

  FBasket := TSprite.Create(texLRFrontViewBasket, False);
  FDress.AddChild(FBasket, 0);
  FBasket.SetCoordinate(FDress.Width*0.25, FDress.Height*0.6);
  FBasket.ApplySymmetryWhenFlip := True;

  PostMessage(0, 4.0); // start left arm anim
end;

procedure TLRFrontView.ProcessMessage(UserValue: TUserMessageValue);
begin
  case UserValue of
    0: begin
      LeftArm.Angle.ChangeTo(-Random(7)-1, 1.0, idcSinusoid);
      PostMessage(1, 2.0+Random*1);
    end;
    1: begin
      LeftArm.Angle.ChangeTo(0, 1.0, idcSinusoid);
      PostMessage(0, 2.0+Random*4);
    end;
  end;
end;

procedure TLRFrontView.ToogleFlipH;
begin
  SetFlipH(not FHood.FlipH);
end;

procedure TLRFrontView.SetFlipH(AValue: boolean);
begin
  inherited SetFlipH(AValue);
  FHood.FlipH := AValue;
  Face.SetFlipH(AValue);
  FDress.FlipH := AValue;
  RightArm.FlipH := AValue;
  LeftArm.FlipH := AValue;
  RightLeg.FlipH := AValue;
  LeftLeg.FlipH := AValue;
  FBasket.FlipH := AValue;
end;

procedure TLRFrontView.ToogleFlipV;
begin
  SetFlipV(not FHood.FlipV);
end;

procedure TLRFrontView.SetFlipV(AValue: boolean);
begin
  inherited SetFlipV(AValue);
  FHood.FlipV := AValue;
  Face.SetFlipV(AValue);
  FDress.FlipV := AValue;
  RightArm.FlipV := AValue;
  LeftArm.FlipV := AValue;
  RightLeg.FlipV := AValue;
  LeftLeg.FlipV := AValue;
  FBasket.FlipV := AValue;
end;

procedure TLRFrontView.HideBasket;
begin
  FBasket.Visible := False;
end;

procedure TLRFrontView.SetWindSpeed(AValue: single);
begin
  FDress.SetWindSpeed(AValue);
  FHood.Amplitude.Value := PointF(0.3*AValue, 0.2*AValue);
end;

procedure TLRFrontView.MoveArmsAsWinner;
begin
  RightArm.Angle.Value := 140;
  LeftArm.Angle.Value := -140;
end;

procedure TLRFrontView.SetCoordinateByFeet(aCenterX, aY: single);
begin
  CenterX := aCenterX;
  Y.Value := aY - RightLeg.Height*0.75;
end;

{ TLRDress }

procedure TLRDress.SetDeformation;
const _cellCount = 3;
var i: integer;
begin
  SetGrid(_cellCount, _cellCount);
  ApplyDeformation(dtWaveH);
  Amplitude.Value := PointF(0.3, 0.2);
  DeformationSpeed.Value := PointF(5,5);
  for i:=0 to _cellCount do
    SetDeformationAmountOnRow(i, i*(1/_cellCount));
end;

constructor TLRDress.Create(aDressTexture: PTexture);
begin
  inherited Create(aDressTexture, False);
  SetDeformation;
  ApplySymmetryWhenFlip := True;
end;

procedure TLRDress.SetWindSpeed(AValue: single);
begin
  Amplitude.Value := PointF(AValue*0.3, 0.2);
end;

{ TLRFace }

procedure TLRFace.SetFaceType(AValue: TLRFaceType);
begin
  inherited SetFaceType(AValue);
  MouthSmile.Visible := AValue = lrfSmile;
  MouthOpen.Visible := AValue = lrfHappy;
  MouthNotHappy.Visible := AValue = lrfNotHappy;
end;

constructor TLRFace.Create;
begin
  inherited Create(texLRFace);

  // white background behind the face
  WhiteBG := CreateChildSprite(texLRFaceBGWhite, -2);
  WhiteBG.SetCenterCoordinate(Width*0.55, Height*0.5);

  // left eye
  LeftEye := CreateChildPolar(texLRFaceEye, -1);
  LeftEye.Polar.Center.Value := PointF(Width*0.30, Height*0.54);

  // right eye
  RightEye := CreateChildPolar(texLRFaceEye, -1);
  RightEye.Polar.Center.Value := PointF(Width*0.77, Height*0.50);

  FEyeMaxDistance := LeftEye.Width*0.30;

  // hair lock
  HairLock := CreateChildSprite(texLRFaceHairLock, -1);
  HairLock.SetCoordinate(Width*0.87, Height*0.6);
  HairLock.Pivot := PointF(0.5, 0);

  // hair
  Hair := CreateChildDeformationGrid(texLRFaceHair, 0);
  Hair.SetCenterCoordinate(Width*0.55, Height*0.40);
  SetDeformationOnHair(Hair);

  // mouth hurt
  MouthNotHappy := CreateChildSprite(texLRFaceMouthHurt, 0);
  MouthNotHappy.SetCenterCoordinate(Width*0.5, Height*0.90);

  // mouth open
  MouthOpen := CreateChildSprite(texLRFaceMouthOpen, 0);
  MouthOpen.SetCenterCoordinate(Width*0.55, Height*0.88);

  // mouth smile
  MouthSmile := CreateChildSprite(texLRFaceMouthSmile, 0);
  MouthSmile.SetCenterCoordinate(Width*0.55, Height*0.85);

  FaceType := lrfSmile;
  PostMessage(0);
  PostMessage(100);
  PostMessage(200);
  PostMessage(300);
end;

procedure TLRFace.SetFlipH(AValue: boolean);
begin
  inherited SetFlipH(AValue);
  LeftEye.FlipH := AValue;
  RightEye.FlipH := AValue;
  Hair.FlipH := AValue;
  HairLock.FlipH := AValue;
  MouthNotHappy.FlipH := AValue;
  MouthOpen.FlipH := AValue;
  MouthSmile.FlipH := AValue;
  WhiteBG.FlipH := AValue;
end;

procedure TLRFace.SetFlipV(AValue: boolean);
begin
  inherited SetFlipV(AValue);
  LeftEye.FlipV := AValue;
  RightEye.FlipV := AValue;
  Hair.FlipV := AValue;
  HairLock.FlipV := AValue;
  MouthNotHappy.FlipV := AValue;
  MouthOpen.FlipV := AValue;
  MouthSmile.FlipV := AValue;
  WhiteBG.FlipV := AValue;
end;

end.

