unit u_screen_title;

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
private
  FAtlas: TOGLCTextureAtlas;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
end;

var ScreenTitle: TScreenTitle;

implementation
uses Forms;

{ TScreenTitle }

procedure TScreenTitle.CreateObjects;
begin
end;

procedure TScreenTitle.FreeObjects;
begin
  FScene.ClearAllLayer;
end;


end.

