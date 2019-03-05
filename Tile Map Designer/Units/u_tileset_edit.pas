unit u_tileset_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, VelocityCurve,
  common;

type

{ TTilesetEdit }

TTilesetEdit=class
private
  FIsActive: boolean;
  FTileEngineTileSetEdit,
  FOldWorkingTileEngine: TTileEngine;
  FEditTilesetIndex: integer;
  procedure FillTileEngineWithCurrentTileSet;
public
  constructor Create;
  destructor Destroy; override;

  procedure EnterModeEditTileset;
  procedure ExitModeEditTileset;
  procedure ShowNextTileset;
  procedure ShowPreviousTileset;
  procedure SetTileEngineCoordinates( aX, aY: single );

  property IsActive: boolean read FIsActive;
  property CurrentTilesetIndex: integer read FEditTilesetIndex write FEditTilesetIndex;
end;


var TilesetEdit: TTilesetEdit;

implementation
uses umaps;

{ TTilesetEdit }

procedure TTilesetEdit.FillTileEngineWithCurrentTileSet;
var t: PTexture;
    xTileCount, yTileCount: integer;
    ro, co: integer;
begin
 // link to the main map
 FTileEngineTileSetEdit.SetTextureListFromAnotherTileEngine( MapList.MainMap.TileEngine );

 // set MapSize of the tileengine to fit the current tileset
 t := MapList.MainMap.TileEngine.GetTexture( FEditTilesetIndex );
 xTileCount:= t^.TextureWidth div t^.FrameWidth;
 yTileCount:= t^.TextureHeight div t^.FrameHeight;

 FTileEngineTileSetEdit.SetTileSize( MapList.MainMap.TileEngine.TileSize.cx, MapList.MainMap.TileEngine.TileSize.cy);
 FTileEngineTileSetEdit.SetMapTileCount( yTileCount, xTileCount);
 FTileEngineTileSetEdit.SetViewSize(FTileEngineTileSetEdit.TileSize.cx*xTileCount, FTileEngineTileSetEdit.TileSize.cy*yTileCount);

 for ro:=0 to yTileCount-1 do
  for co:=0 to xTileCount-1 do
   begin
    FTileEngineTileSetEdit.SetCell( ro, co, FEditTilesetIndex, co, ro );
    FTileEngineTileSetEdit.SetUserEventValue( ro, co, -1 );   // no user event
   end;
end;

constructor TTilesetEdit.Create;
begin
 FTileEngineTileSetEdit:= TTileEngine.Create;
 FScene.Add( FTileEngineTileSetEdit, Layer_WorkTileSet );
 FTileEngineTileSetEdit.SetTextureListFromAnotherTileEngine( MapList.MainMap.TileEngine );
 FTileEngineTileSetEdit.ForceTheDrawingOfAllTheTiles:=TRUE;
 FTileEngineTileSetEdit.MapHoleColor.Value := BGRA(0,0,0,0);
end;

destructor TTilesetEdit.Destroy;
begin
 inherited Destroy;
end;

procedure TTilesetEdit.EnterModeEditTileset;
begin
 if MapList.MainMap.TileEngine.GetTextureCount=0 then exit;
  FScene.Layer[Layer_WorkMap].Visible:=FALSE;
//  FScene.Layer[Layer_InfoMap].Visible:=FALSE;
  FScene.Layer[Layer_WorkTileSet].Visible:=TRUE;
  FillTileEngineWithCurrentTileSet;
  FIsActive:=TRUE;

  FOldWorkingTileEngine:=FWorkingTileEngine;
  FWorkingTileEngine:=FTileEngineTileSetEdit;
end;

procedure TTilesetEdit.ExitModeEditTileset;
begin
  FScene.Layer[Layer_WorkMap].Visible:=TRUE;
//  FScene.Layer[Layer_InfoMap].Visible:=TRUE;
  FScene.Layer[Layer_WorkTileSet].Visible:=FALSE;
  FIsActive:=FALSE;
  FWorkingTileEngine := FOldWorkingTileEngine;
end;

procedure TTilesetEdit.ShowNextTileset;
begin
 if FEditTilesetIndex=MapList.MainMap.TileEngine.GetTextureCount-1 then exit;
 inc(FEditTilesetIndex);
 FillTileEngineWithCurrentTileSet;
end;

procedure TTilesetEdit.ShowPreviousTileset;
begin
 if FEditTilesetIndex=0 then exit;
 dec(FEditTilesetIndex);
 FillTileEngineWithCurrentTileSet;
end;

procedure TTilesetEdit.SetTileEngineCoordinates(aX, aY: single);
begin
 FTileEngineTileSetEdit.SetCoordinate( aX, aY );
end;

end.

