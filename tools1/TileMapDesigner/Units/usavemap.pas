unit usavemap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene,
  BGRABitmap, BGRABitmapTypes;


procedure DoSave( aTileEngine: TTileEngine; const aFilename: string );

implementation
uses tileset_manager,

     u_tool_window,
     uAskGroundType,
     uAskEventValue;

procedure DoSave( aTileEngine: TTileEngine; const aFilename: string );
var FMapFile: TStringList;
    i, ro, co: integer;
    itex, ixfr, iyfr: integer;
    s: string;
begin
  FMapFile := TStringList.Create;

  // save map's textures
  FMapFile.Add('TEXTURES');
  FMapFile.Add( inttostr( TileSetManager.Count ));
  for i:=0 to TileSetManager.Count-1 do
   FMapFile.Add( TileSetManager.TileSet[i].Name + '|' +
                 inttostr(TileSetManager.TileSet[i].TileWidth) + '|' +
                 inttostr(TileSetManager.TileSet[i].TileHeight) );

  // draw size
  FMapFile.Add('DRAW_SIZE');
  if Form_Tools.CB4.ItemIndex=0
    then FMapFile.Add( inttostr(Form_Tools.SE3.Value) + '|' + inttostr(Form_Tools.SE4.Value) )
    else FMapFile.Add( inttostr(Form_Tools.SE3.Value * aTileEngine.TileSize.cx) + '|' +
                       inttostr(Form_Tools.SE4.Value * aTileEngine.TileSize.cy) );

  // tile size
  FMapFile.Add('TILE_SIZE');
  FMapFile.Add( inttostr(aTileEngine.TileSize.cx) + '|' + inttostr(aTileEngine.TileSize.cy) );

  // map tile count
  FMapFile.Add('MAP_SIZE');
  FMapFile.Add( inttostr(aTileEngine.MapTileCount.cy) + '|' + inttostr(aTileEngine.MapTileCount.cx) );

  // scroll enable
  FMapFile.Add('SCROLL_ENABLE');
  s := '';
  if aTileEngine.HScrollEnable then s += 'H';
  if aTileEngine.VScrollEnable then s += 'V';
  FMapFile.Add( s );

  // scroll loop mode
  FMapFile.Add('SCROLL_LOOP_MODE');
  s := '';
  if aTileEngine.HLoopMode then s += 'H';
  if aTileEngine.VLoopMode then s += 'V';
  FMapFile.Add( s );

  // start tile
  FMapFile.Add('START_TILE');
  FMapFile.Add( Form_Tools.Label10.Caption + '|' + Form_Tools.Label11.Caption );

  // hole color
  FMapFile.Add('HOLE_COLOR');
  FMapFile.Add( BGRAPixelToHex( aTileEngine.MapHoleColor.Value ));

  // Event names
  if Form_AskEvent.LB.Count > 0 then
  begin
    FMapFile.Add('EVENT_NAMES');
    for i:=0 to Form_AskEvent.LB.Count-1 do
      if i=0 then s := Form_AskEvent.LB.Items.Strings[i]
             else s+= '|' + Form_AskEvent.LB.Items.Strings[i];
    FMapFile.Add( s );
  end;

  // Ground names
  if Form_AskGroundType.LB.Count > 0 then
  begin
    FMapFile.Add('GROUND_NAMES');
    for i:=0 to Form_AskGroundType.LB.Count-1 do
      if i=0 then s := Form_AskGroundType.LB.Items.Strings[i]
             else s+= '|' + Form_AskGroundType.LB.Items.Strings[i];
    FMapFile.Add( s );
  end;

  //'FRAMES_TYPE'
  FMapFile.Add('FRAMES_TYPE');
  FMapFile.Add( inttostr( aTileEngine.GetTextureCount ));   // texture (tileset) count
  for itex:=0 to aTileEngine.GetTextureCount-1 do
  begin
    s := inttostr( TileSetManager.TileSet[itex].XTileCount ) + '|' + inttostr( TileSetManager.TileSet[itex].YTileCount );

    for iyfr:=0 to TileSetManager.TileSet[itex].YTileCount-1 do
      for ixfr:=0 to TileSetManager.TileSet[itex].XTileCount-1 do
       s += '|' + inttostr( aTileEngine.GetGroundType( itex, ixfr, iyfr ));  // each frame type for each texture

    FMapFile.Add( s );
  end;

  // map data
  FMapFile.Add('DATA');
  for ro:=0 to aTileEngine.MapTileCount.cy-1 do begin
   s := '';
   for co:=0 to aTileEngine.MapTileCount.cx-1 do begin
    with aTileEngine.GetPTile( ro, co )^ do
      begin
       if co > 0 then s += ' ';
       s += 'T' + inttostr( TextureIndex ) + ',' + inttostr( ixFrame ) + ',' + inttostr( iyFrame ) + ',' + inttostr( UserEvent );
      end;
   end;
   FMapFile.Add( s );
  end;

  FMapFile.SaveToFile( aFileName );
  FMapFile.Free;
end;


end.

