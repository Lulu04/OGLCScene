unit screen_1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common;

type

{ TScreen1 }

TScreen1 = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas;

  // if you use UI elements that can be clicked (buttons...) redirects their OnClick event to this method.
  // If you don't have any UI in your game, you can remove this method.
  procedure ProcessUIClick(Sender: TSimpleSurfaceWithEffect);
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

procedure TScreen1.ProcessUIClick(Sender: TSimpleSurfaceWithEffect);
begin
  // Manage here the click on your UI elements
  // ...
end;

procedure TScreen1.CreateObjects;
var ima: TBGRABitmap;
begin
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  // Add your images to the atlas here
  // example:
      ima := TBGRABitmap.Create(100,100,BGRA(255,255,0));
      FAtlas.Add(ima); // don't freed 'ima' because FAtlas become its owner !
  // ...

  FAtlas.TryToPack;
  FAtlas.Build;
  // for debug purpose, we save the packed image just to see if all is fine.
  ima := FAtlas.GetPackedImage(True, True);
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
var truck: TSprite;
begin
  inherited Update(AElapsedTime);

  // Put here the code to manage this screen. This method is called every frames.
  // ...
end;

procedure TScreen1.ProcessMessage(UserValue: TUserMessageValue);
begin
  // uncomment and handle your messages here
{
  case UserValue of
    YourFirstUserValue: begin

    end;
  ...
  end;//case
}
end;


end.

