unit screen_1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common;

{
  About game screen, keep in mind this concept:
   - there is one screen for one part of the game, for example: screen_title, screen_game,
     screen_halloffame, etc... each ones in a separate unit and encapsulated in a class
     descendant of TScreenTemplate.
}

type
{ TScreen1 }

TScreen1 = class(TScreenTemplate)
private
  // declare a texture atlas here if you need one atlas per screen.
  // In case you need only a single atlas for the whole game shared by different screens,
  // its better to declare it as global variable in 'u_common', initialize it in FormMain.LoadCommonData and
  // free it in FormMain.FreeCommonData
  FAtlas: TOGLCTextureAtlas;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;
  // If you don't use message in this screen, you can remove this method.
  procedure ProcessMessage(UserValue: TUserMessageValue); override;
end;

var Screen1: TScreen1 = NIL;

implementation
uses Forms;

{ TScreen1 }

procedure TScreen1.CreateObjects;
var ima: TBGRABitmap;
begin
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  // Add your images to the atlas here
  // example:
      ima := TBGRABitmap.Create(100,100,BGRA(255,255,0));
      FAtlas.Add(ima, 'MyYellowRectangle'); // don't freed 'ima' because FAtlas become its owner !
  // ...

  FAtlas.TryToPack;
  FAtlas.Build;
  // for debug purpose, we save the packed image just to see if all is fine.
  ima := FAtlas.GetPackedImage(False, False);
  ima.SaveToFile(Application.Location+'atlas.png');
  ima.Free;
  FAtlas.FreeItemImages; // free some memory because we no longer need individual images

  // Creates your surfaces here
  // ...
end;

procedure TScreen1.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreen1.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);

  // Put here the logic to manage this screen. This method is called every frames.
  // ...
end;

procedure TScreen1.ProcessMessage(UserValue: TUserMessageValue);
begin
  // uncomment and handle your messages here
{
  case UserValue of
  ...
  end;//case
}
end;


end.

