unit usavemap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes;


procedure DoSave( aFilename: string );

implementation
uses common,
     tileset_manager,
     OGLCScene,
     Main,
     uAskGroundType,
     uAskEventValue;

procedure DoSave( aFilename: string );
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
 if Form_Principale.CB4.ItemIndex=0
   then FMapFile.Add( inttostr(Form_Principale.SE3.Value) + '|' + inttostr(Form_Principale.SE4.Value) )
   else FMapFile.Add( inttostr(Form_Principale.SE4.Value * FTileEngine.TileSize.cx) + '|' + inttostr(Form_Principale.SE3.Value * FTileEngine.TileSize.cy) );

 // tile size
 FMapFile.Add('TILE_SIZE');
 FMapFile.Add( inttostr(FTileEngine.TileSize.cx) + '|' + inttostr(FTileEngine.TileSize.cy) );

 // map tile count
 FMapFile.Add('MAP_SIZE');
 FMapFile.Add( inttostr(FTileEngine.MapTileCount.cy) + '|' + inttostr(FTileEngine.MapTileCount.cx) );

 // scroll enable
 FMapFile.Add('SCROLL_ENABLE');
 s := '';
 if Form_Principale.CB5.Checked then s += 'H';
 if Form_Principale.CB6.Checked then s += 'V';
 FMapFile.Add( s );

 // scroll loop mode
 FMapFile.Add('SCROLL_LOOP_MODE');
 s := '';
 if Form_Principale.CB5.Checked and Form_Principale.CB2.Checked then s += 'H';
 if Form_Principale.CB6.Checked and Form_Principale.CB3.Checked then s += 'V';
 FMapFile.Add( s );

 // start tile
 FMapFile.Add('START_TILE');
 FMapFile.Add( Form_Principale.Label10.Caption + '|' + Form_Principale.Label11.Caption );

 // hole color
 FMapFile.Add('HOLE_COLOR');
 FMapFile.Add( BGRAPixelToHex( FTileEngine.MapHoleColor.Value ));

 // Event names
 if Form_AskEvent.LB.Count > 0
   then begin
     FMapFile.Add('EVENT_NAMES');
     for i:=0 to Form_AskEvent.LB.Count-1 do
       if i=0 then s := Form_AskEvent.LB.Items.Strings[i]
              else s+= '|' + Form_AskEvent.LB.Items.Strings[i];
     FMapFile.Add( s );
   end;

 // Ground names
 if Form_AskGroundType.LB.Count > 0
   then begin
     FMapFile.Add('GROUND_NAMES');
     for i:=0 to Form_AskGroundType.LB.Count-1 do
       if i=0 then s := Form_AskGroundType.LB.Items.Strings[i]
              else s+= '|' + Form_AskGroundType.LB.Items.Strings[i];
     FMapFile.Add( s );
   end;

 //'FRAMES_TYPE'
 FMapFile.Add('FRAMES_TYPE');
 FMapFile.Add( inttostr( FTileEngine.GetTextureCount ));   // texture (tileset) count
 for itex:=0 to FTileEngine.GetTextureCount-1 do
  begin
   s := inttostr( TileSetManager.TileSet[itex].XTileCount ) + '|' + inttostr( TileSetManager.TileSet[itex].YTileCount );

   for iyfr:=0 to TileSetManager.TileSet[itex].YTileCount-1 do
     for ixfr:=0 to TileSetManager.TileSet[itex].XTileCount-1 do
      s += '|' + inttostr( FTileEngine.GetGroundType( itex, ixfr, iyfr ));  // each frame type for each texture

   FMapFile.Add( s );
  end;

 // map data
 FMapFile.Add('DATA');
 for ro:=0 to FTileEngine.MapTileCount.cy-1 do begin
  s := '';
  for co:=0 to FTileEngine.MapTileCount.cx-1 do begin
   with FTileEngine.GetPTile( ro, co )^ do
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

