unit u_screen_fontbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, u_screen_template, u_common, u_surface_list, u_texture_list;

type

{ TScreenFontBank }

TScreenFontBank = class(TCustomScreenTemplate)
private
  FFontAtlas: TAtlas;
  FTexturedFont: TTexturedFont;
  FTextArea: TUITextArea;
public
  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseWheel({%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean); override;
  procedure ProcessOnKeyUp(var Key: Word; {%H-}Shift: TShiftState); override;

  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  procedure UpdatePreview(const aFD: TFontDescriptor; aCharset: string; aFillTexture: TBGRABitmap);
  procedure ClearView;

  function Surfaces: TSurfaceList; override;
  function Textures: TTextureList; override;
end;

var ScreenFontBank: TScreenFontBank;

implementation

{ TScreenFontBank }

procedure TScreenFontBank.ProcessMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited ProcessMouseUp(Button, Shift, X, Y);
end;

procedure TScreenFontBank.ProcessMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited ProcessMouseDown(Button, Shift, X, Y);
end;

procedure TScreenFontBank.ProcessMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited ProcessMouseMove(Shift, X, Y);
end;

procedure TScreenFontBank.ProcessMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);
end;

procedure TScreenFontBank.ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited ProcessOnKeyUp(Key, Shift);
end;

procedure TScreenFontBank.CreateObjects;
begin
  ShowLayers([LAYER_FONT, LAYER_UI, LAYER_TOP]);
  ShowSceneBounds;
  CreateCamera([LAYER_FONT, LAYER_SCENEBOUNDS]);
  ZoomOnScene;
end;

procedure TScreenFontBank.FreeObjects;
begin
  FScene.Layer[LAYER_FONT].Clear;
  FTextArea := NIL;
  FTexturedFont := NIL;

  if Assigned(FFontAtlas) then FFontAtlas.Free;
  FFontAtlas := NIL;

  HideSceneBounds;
  FreeCamera;
end;

procedure TScreenFontBank.Initialize;
begin

end;

procedure TScreenFontBank.Finalize;
begin

end;

procedure TScreenFontBank.UpdatePreview(const aFD: TFontDescriptor; aCharset: string; aFillTexture: TBGRABitmap);
begin
  FScene.MakeCurrent;

  if Assigned(FFontAtlas) then FFontAtlas.Free;

  FFontAtlas := FScene.CreateAtlas;
  FFontAtlas.Spacing := 2;
  FTexturedFont := FFontAtlas.AddTexturedFont(aFD, aCharset, aFillTexture);
  FFontAtlas.TryToPack;
  FFontAtlas.Build();

  if Assigned(FTextArea) then FTextArea.Kill;
  FTextArea := TUITextArea.Create(FScene);
  FTextArea.BodyShape.SetShapeRectangle(FScene.Width*8 div 10, FScene.Height*8 div 10, 0.0);
  FTextArea.BodyShape.Fill.Visible := False;
  FTextArea.BodyShape.Border.Visible := False;
  FScene.Add(FTextArea, LAYER_FONT);
  FTextArea.CenterOnScene;
  FTextArea.Text.Align := taTopCenter;
  FTextArea.Text.TexturedFont := FTexturedFont;
  FTextArea.Text.Caption := aCharset;
  FTextArea.Text.Tint.Value := BGRA(0,0,0,0);
  FTextArea.MouseInteractionEnabled := False;
  //FTextArea.ChildClippingEnabled := False;
end;

procedure TScreenFontBank.ClearView;
begin
  FScene.Layer[LAYER_FONT].Clear;
  FTextArea := NIL;
  FTexturedFont := NIL;

  if Assigned(FFontAtlas) then FFontAtlas.Free;
  FFontAtlas := NIL;
end;

function TScreenFontBank.Surfaces: TSurfaceList;
begin
  Result := NIL;
end;

function TScreenFontBank.Textures: TTextureList;
begin
  Result := NIL;
end;

end.

