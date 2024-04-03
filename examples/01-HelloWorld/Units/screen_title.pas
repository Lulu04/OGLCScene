unit screen_title;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common;

type

{ TScreenTitle }

TScreenTitle = class(TScreenTemplate)
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure ProcessMessage({%H-}UserValue: TUserMessageValue); override;
end;

var ScreenTitle: TScreenTitle = NIL;

implementation

{ TScreenTitle }

procedure TScreenTitle.CreateObjects;
var o: TSprite;
  fd: TFontDescriptor;
begin
  fd.Create('Arial', 32, [], BGRA(255,0,200), BGRA(200,0,150), 1, BGRA(0,0,0,0), 0, 0, 0);

  o := TSprite.Create(FScene, fd, 'Hello world');
  FScene.Add(o);
  o.CenterOnScene;
end;

procedure TScreenTitle.FreeObjects;
begin
  FScene.ClearAllLayer;
end;

procedure TScreenTitle.ProcessMessage(UserValue: TUserMessageValue);
begin
  inherited ProcessMessage(UserValue); // keep this line please
end;


end.

