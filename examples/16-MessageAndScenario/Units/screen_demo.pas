unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_sprite_def;

const  // message posted by TShipArm instance
  MESS_ARM_RETRACTED  = 0;
  MESS_ARM_DEPLOYED   = 1;
  MESS_HARVESTING_ON  = 2;
  MESS_HARVESTING_OFF = 3;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to pack all images in a single texture
  FtexStar,      // the texture for the star image
  FtexShip,      // the texture for the ship
  FtexShipRadar, // texture for its radar
  FtexShipArm,   // texture for its arm
  FtexShipPliers, // texture for its pliers
  FtexMeteor: PTexture; // texture for the meteor

  FtexFont: TTexturedFont; // the font used to show text

  FShip: TSprite;   // the sprite for the ship

  FShipRadar: TShipRadar; // the animated sprite for the radar

  FShipArm: TShipArm;    // the animated sprite for the 3 parts of the arm and the pliers

  FMeteor: TSprite;  // the sprite for the meteor

  FPanelStatus: TPanelStatus; // a panel to show informations to the player
  FLabel: TFreeText;
private
  FTimeAccu: single;
  procedure CreateStars;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;
  procedure ProcessMessage({%H-}UserValue: TUserMessageValue); override;

  property Ship: TSprite read FShip;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, LCLType;


{ TScreenDemo }

procedure TScreenDemo.CreateStars;
var star: TSprite;
  zoom: single;
  b: byte;
  i: integer;
begin
  // stars creation
  for i:=0 to 400 do begin
    star := TSprite.Create(FtexStar, False);  // creation of the sprite. False because texture is owned by the atlas.
    FScene.Add(star, LAYER_BACK);   // add the sprite instance to the scene
    with star do begin
        X.Value := Random(FScene.Width-star.Width);  // set a random coordinates
        Y.Value := Random(FScene.Height-star.Height);
        zoom := 0.5 + Random*0.8;
        Scale.Value := PointF(zoom, zoom);          // scale the sprite
        b := Random(192)+64;
        Tint.Value := BGRA(b,b,50);                 // set the color
        Angle.AddConstant(Random(180));             // add a rotation effect
      end;

  end;
end;

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  coef: single;
  fd: TFontDescriptor;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;
  FtexStar := FAtlas.AddFromSVG(path+'SpaceStar.svg', -1, Round(FScene.Height/100));

  coef := 1;
  FtexShip := FAtlas.AddFromSVG(path+'SpaceShip.svg', Round(294*coef), -1);
  FtexShipRadar := FAtlas.AddFromSVG(path+'SpaceShipRadar.svg', Round(50*coef), -1);
  FtexShipArm := FAtlas.AddFromSVG(path+'SpaceShipArm.svg', Round(24*coef), -1);
  FtexShipPliers := FAtlas.AddFromSVG(path+'SpaceShipPliers.svg', Round(55*coef), -1);
  FtexMeteor := FAtlas.AddFromSVG(path+'SpaceMeteor.svg', Round(327*coef), -1);

  fd.Create('Arial', FScene.ScaleDesignToScene(18), [], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, SIMPLELATIN_CHARSET+ASCII_SYMBOL_CHARSET);

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  // stars creation
  CreateStars;

  // ship creation
  FShip := TSprite.Create(FtexShip, False);
  FScene.Add(FShip, LAYER_TOP);
  FShip.CenterOnScene;

  // add the radar to the chip as child
  FShipRadar := TShipRadar.Create(FtexShipRadar);
  FShip.AddChild(FShipRadar, 0);
  FShipRadar.CenterX := FShip.Width*0.5;
  FShipRadar.CenterY := FShip.Height*0.9;

  // add the arm as child
  FShipArm := TShipArm.Create(FtexShipArm, FtexShipPliers);
  FShip.AddChild(FShipArm, 1);
  FShipArm.CenterX := FShip.Width*0.515;
  FShipArm.BottomY := FShip.Height*0.7;

  // meteor creation
  FMeteor := TSprite.Create(FtexMeteor, False);
  FScene.Add(FMeteor, LAYER_MIDDLE);
  FMeteor.X.Value := FShip.RightX;
  FMeteor.CenterY := FShip.Y.Value;
  FMeteor.Angle.AddConstant(2.5);

  // panel creation
  FPanelStatus := TPanelStatus.Create(FtexFont);

  // explanation label
  FLabel := TFreeText.Create(FScene);
  FScene.Add(FLabel, LAYER_TOP);
  FLabel.TexturedFont := FtexFont;
  FLabel.Caption := 'Press SPACE to deploy/retract the arm to harvest the meteor';
  FLabel.Tint.Value := BGRA(255,0,255);
  FLabel.Opacity.Value := 200;
  FLabel.CenterX := FScene.Center.x;
  FLabel.BottomY := FScene.Height - FLabel.Height;
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);

  // check if player press space to deploy/retract the arm
  if FScene.KeyState[VK_SPACE] and not FShipArm.Busy then begin
    if FShipArm.ArmIsRetracted then FShipArm.DeployArm
      else FShipArm.RetractArm;
  end;

  FTimeAccu := FTimeAccu + AElapsedTime;
  if FTimeAccu > 0.01 then begin
    FTimeAccu := 0.0;
  end;

end;

procedure TScreenDemo.ProcessMessage(UserValue: TUserMessageValue);
begin
  case UserValue of

    MESS_ARM_RETRACTED: FPanelStatus.ArmIsRetracted := True;

    MESS_ARM_DEPLOYED: FPanelStatus.ArmIsRetracted := False;

    MESS_HARVESTING_ON: FPanelStatus.ArmIsHarvesting := True;

    MESS_HARVESTING_OFF: FPanelStatus.ArmIsHarvesting := False;

  end;
end;


end.

