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
  MESS_TRANSFERTOCARGO_ON = 4;

type

TGameState=(gsWaitNearMeteor, gsHarvesting, gsShipTravelToFreighter, gsShipTransfersToCargo,
            gsShipTravelToMeteor);

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to pack all images in a single texture
  FtexStar,      // the texture for the star image
  FtexShip,      // the texture for the ship
  FtexShipRadar, // texture for its radar
  FtexShipArm,   // texture for its arm
  FtexShipPliers, // texture for its pliers
  FtexMeteor, // texture for the meteor
  FtexFreighter: PTexture; // texture for the freighter

  FtexFont: TTexturedFont; // the font used to show text

  FShip: TSprite;   // the sprite for the ship
  FShipPE: TParticleEmitter;  // the paricle emitter for the ship engine

  FShipRadar: TShipRadar; // the animated sprite for the radar

  FShipArm: TShipArm;    // the animated sprite for the 3 parts of the arm and the pliers

  FMeteor: TSprite;  // the sprite for the meteor

  FFreighter: TSprite; // the sprite for the freighter

  FPanelStatus: TPanelStatus; // a panel to show informations to the player
  FExplanation: TFreeTextAligned; // to contains the explanation text

  FCamera: TOGLCCamera;
  FGameState: TGameState;
private
  procedure CreateStars;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;
  procedure ProcessMessage({%H-}UserValue: TUserMessageValue); override;

  property GameState: TGameState read FGameState write FGameState;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, LCLType;


{ TScreenDemo }

procedure TScreenDemo.CreateStars;
var star: TSprite;
  zoom: single;
  b: byte;
  i, xrange, yrange: integer;
begin
  // stars creation
  xrange := Trunc(Abs(FFreighter.X.Value-FFreighter.Width))+FScene.Width;
  yrange := Trunc(Abs(FFreighter.Y.Value-FFreighter.Height))+FScene.Height;
  for i:=0 to 2000 do begin
    star := TSprite.Create(FtexStar, False);  // creation of the sprite. False because texture is owned by the atlas.
    FScene.Add(star, LAYER_BACK);   // add the sprite instance to the scene
    with star do begin
        X.Value := Random(xrange)+(FFreighter.X.Value-FFreighter.Width); // Random(FScene.Width-star.Width);  // set a random coordinates
        Y.Value := Random(yrange)+(FFreighter.Y.Value-FFreighter.Height);  // Random(FScene.Height-star.Height);
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
  FtexFreighter := FAtlas.AddFromSVG(path+'SpaceFreighter.svg', Round(1092*coef), -1);

  // used by the particle emitter for the ship engine.
  FAtlas.Add(path+'Dust_particle.png');

  fd.Create('Arial', FScene.ScaleDesignToScene(18), [], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, FScene.Charsets.SIMPLELATIN + FScene.Charsets.ASCII_SYMBOL);

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  // freighter creation ( before create the ship to have the freighter under the ship )
  FFreighter := TSprite.Create(FtexFreighter, False);
  FScene.Add(FFreighter, LAYER_SHIP);
  FFreighter.SetCoordinate(-2000, -1000);

  // stars creation
  CreateStars;

  // ship creation
  FShip := TSprite.Create(FtexShip, False);
  FScene.Add(FShip, LAYER_SHIP);
  FShip.CenterOnScene;

  // ship engine particle emitter as child of the chip
  FShipPE := TParticleEmitter.Create(FScene);
  FShip.AddChild(FShipPE, -1);
  // Load the particle emitter configuration: the file '.par' are generated by the ParticleEditor tool.
  // Because:
  //         1) the file .par have a reference to an image file for the texture used by the particle emitter
  //         2) this texture is already loaded in our atlas
  // -> we specify the atlas instance where the load method will search the relevant file.
  FShipPE.LoadFromFile(path+'SpaceShipEngine.par', FAtlas);
  FShipPE.ParticlesPosRelativeToEmitterPos := False;
  FShipPE.SetCoordinate(FShip.Width*0.5, FShip.Height);
  FShipPE.ParticlesToEmit.Value := 0; // ship engine don't emit particles


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
  FScene.Add(FMeteor, LAYER_BACK);
  FMeteor.X.Value := FShip.RightX;
  FMeteor.CenterY := FShip.Y.Value;
  FMeteor.Angle.AddConstant(2.5);

  // panel creation
  FPanelStatus := TPanelStatus.Create(FtexFont);

  // explanation text
  FExplanation := TFreeTextAligned.Create(FScene, FtexFont, Round(FScene.Width*0.8), FScene.Height div 3);
  FScene.Add(FExplanation, LAYER_GUI);
  FExplanation.Align := taTopCenter;
  FExplanation.Caption := 'Press ''SPACE'' to deploy/retract the arm to harvest the meteor.'#10+
                          'Press ''C'' to reach the freighter to transfert the cargo.'#10+
                          'The camera follow the ship and zoom to show the other objects.'#10;
  FExplanation.AdjustSize;
  FExplanation.Tint.Value := BGRA(255,255,255);
  FExplanation.Opacity.Value := 200;
  FExplanation.CenterX := FScene.Center.x;
  FExplanation.BottomY := FScene.Height;

  // we create a camera that affects only the layer LAYER_SHIP and LAYER_BACK
  FCamera := FScene.CreateCamera;
  FCamera.AssignToLayers([LAYER_SHIP, LAYER_BACK]);

  FGameState := gsWaitNearMeteor;
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);
  case GameState of
    gsWaitNearMeteor: begin
      FShipPE.ParticlesToEmit.Value := 0; // ship engine don't emit particles
      // check if player press space to deploy/retract the arm
      if FScene.KeyState[VK_SPACE] and not FShipArm.Busy then begin
        FShipArm.DeployArm;
        GameState := gsHarvesting;
      end
      else
      // check if player press C to reach the freighter
      if FScene.KeyState[VK_C] and not FShipArm.Busy and FShipArm.ArmIsRetracted then begin
        FShipPE.ParticlesToEmit.Value := 100; // ship engine emits particles
        FShip.MoveCenterTo(FFreighter.Center, 10, idcSinusoid);
        FShip.Angle.Value := 0;
        FShip.Angle.ChangeTo(-90, 8, idcSinusoid);
        FCamera.Scale.ChangeTo(PointF(0.6,0.6), 3.0, idcSinusoid); // zoom out
        GameState := gsShipTravelToFreighter;
      end;
    end;

    gsHarvesting: begin
      // check if player press space to deploy/retract the arm
      if FScene.KeyState[VK_SPACE] and not FShipArm.Busy then begin
        FShipArm.RetractArm;
        GameState := gsWaitNearMeteor;
      end;
    end;

    gsShipTravelToFreighter: begin
      // ship reach the freighter ?
      if (FShip.X.State = psNO_CHANGE) and (FShip.Y.State = psNO_CHANGE) then begin
        GameState := gsShipTransfersToCargo;
        FShipPE.ParticlesToEmit.Value := 0; // ship engine don't emit particles
        FCamera.Scale.ChangeTo(PointF(1,1), 3.0, idcSinusoid); // no zoom
      end;
    end;

    gsShipTransfersToCargo: begin
      FPanelStatus.Gauge.Percent := FPanelStatus.Gauge.Percent - AElapsedTime/5;
      if FPanelStatus.Gauge.Percent = 0 then begin
        FShipPE.ParticlesToEmit.Value := 100; // ship engine emits particles
        FShip.MoveCenterTo(FScene.Center, 10, idcSinusoid);
        FShip.Angle.ChangeTo(-360, 10, idcSinusoid);
        FCamera.Scale.ChangeTo(PointF(0.6,0.6), 3.0, idcSinusoid);  // zoom out
        GameState := gsShipTravelToMeteor;
      end;
    end;

    gsShipTravelToMeteor: begin
      // ship reach the meteor ?
      if (FShip.X.State = psNO_CHANGE) and (FShip.Y.State = psNO_CHANGE) then begin
        FCamera.Scale.ChangeTo(PointF(1,1), 3.0, idcSinusoid); // no zoom
        GameState := gsWaitNearMeteor;
      end;
    end;

  end;//case

  // we update the camera position to follow the ship
  FCamera.MoveTo(FShip.Center);
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

