{$ifdef oglcINTERFACE}
{ TTileEngine }
const
VALUE_NO_EVENT = -1;

type
// on map, tiles have a reference on a texture, indexes (ix,iy) for each tile on this texture (frame)
// and an user value for trigger an event
TTile = record
         TextureIndex  : integer; // index of texture in TTilemap.FTextureList
         ixFrame       : integer; // frame x index on texture
         iyFrame       : integer; // frame y index
         UserEvent     : integer;
end;
PTile = ^TTile;

// map is an array of array of TTile
TArrayOfArrayOfTile = array of array of TTile;

TArrayOfArrayOfInteger = array of array of integer;
// each texture (or tileset) have an array value that allows to identify ground type for each tile
TTileTexInfo = record
     Texture  : PTexture;
     TileType : TArrayOfArrayOfInteger;  // each type by frame column and row  [x][y]
end;
PTileTexInfo = ^TTileTexInfo;
TArrayOfTileTexInfo = array of TTileTexInfo;


TTileEvent = procedure ( Sender: TTileEngine; const TileTopLeftCoor: TPointF; EventValue: integer ) of object;

TTileEngine = class( TSimpleSurfaceWithEffect )
 Constructor Create;
 Destructor Destroy ; override ;
private
 FTextureList      : TArrayOfTileTexInfo;
 FMap              : TArrayOfArrayOfTile; // [row,column]
 FMapSize          : TSize;     // total map size in pixel
 FMapTileCount     : TSize;
 FWidth, FHeight   : integer;   // screen surface size
 FTileSize         : TSize;
 FVScrollEnable,
 FHScrollEnable,
 FHLoopMode,
 FVLoopMode        : boolean;
 FColumnIndexFirstTile : integer;
 FRowIndexFirstTile : integer;
 FXOffsetFirstTile: single;
 FYOffsetFirstTile: single;
 FOnTileEvent : TTileEvent ; // callback for event
 function GetTileSize: TSize;
 function GetMapSize: TSize;
 procedure EnsurePositionInMap( var APos: TPointF );
 procedure ComputeIndexes( APosition: TPointF );
 procedure SetTileSize( AHSize, AVSize: integer );
 function IsValidRow( ARow: integer ): boolean; inline;
 function IsValidColumn( AColumn: integer ): boolean; inline;
 function IsValidTextureIndex( AITex: integer ): boolean;
 function IsValidFrameIndex( ATextureIndex, ixFrame, iyFrame: integer ): boolean;
 function TileEngineIndexFrameToTextureIndexFrame( iTexture, ixFrame, iyFrame: integer): integer;
 procedure Clear;
protected
 function GetWidth: integer; override;
 function GetHeight: integer; override;
public
 procedure Update( const aElapsedTime: single ); override;
 procedure Draw( const aLayerPercentOpacity: single ); override;
public
 // methods to construct map. Used by 'Tile Map Designer' utility
 TileMapDesignerModeEnable: boolean;
 function GetFirstTileColumnIndex: integer;
 function GetFirstTileRowIndex: integer;
 function AddTexture( AFilename: string; AFrameWidth, AFrameHeight: integer ): integer; // return index of created texture
 procedure DeleteTexture( AIndex: integer );
 function GetTexture( AIndex: integer ): PTexture;
 function GetTextureCount: integer;
 procedure SetMapTileCount( ARowCount, AColumnCount: integer );
 procedure InsertRow( ARowIndex, ACount: integer );
 procedure InsertColumn( AColumnIndex, ACount: integer );
 procedure ResetMap;
 function GetPTile( ARow, AColumn: integer ): PTile;
 function GetPTileTexInfo( ARow, AColumn: integer ): PTileTexInfo;
 procedure SetUserEventValue( ARow, AColumn, AUserEvent: integer );
 function GetUserEventValue( ARow, AColumn: integer ): integer; overload;
 procedure SetCell( ARow, AColumn, ATextureIndex, ixFrame, iyFrame: integer );
// procedure GetCellTextureAndFrameIndex( ARow, AColumn: integer; out TextureIndex, ixFrame, iyFrame: integer );
 procedure SetGroundType( ATextureIndex, ixFrame, iyFrame, AGroundType: integer );
 function GetGroundType( ATextureIndex, ixFrame, iyFrame: integer ): integer; overload;
 procedure ClearCell( ARow, AColumn: integer );
public
 // set to TRUE if you use a camera to move on the map instead of using PositionOnMap property
 // slowly because all the tiles are drawn even if they are not visible
 ForceTheDrawingOfAllTheTiles: boolean;

 // this is virtual position of the top left corner of the view
 // why virtual ? Because when HLoopMode or VLoopMode is set to TRUE, the map repeats itself indefinitely
 // and position can be greater than view size, or even negative
 // for example, your map is 100x100 pixels sized and you set HLoopMode to TRUE, virtual position (15000,80) is valid
 // because map repeat itself on horizontal axis
 PositionOnMap : TPointFParam;

 // scrolling speed in pixel per seconds for both axis x y
 ScrollSpeed   : TPointFParam;

 // color for the 'holes' in the map
 MapHoleColor  : TBGRAParam;

 // give real position on map pointed by 'VirtualPositionOnMap'
 function GetBoundedPositionOnMap: TPointF;

 // load map previously created with 'Tile Map Designer' tool
 procedure LoadMapFile( const aFilename : string );

 // Give the type of the tile pointed by specified position on the scene (and not on map). Can be out of the view, but still on map
 function GetGroundType( aSceneCoor: TPointF ): integer; overload;

 // Give event's value associed at the tile pointed by specified position on the scene
 function GetUserEventValue( aSceneCoor: TPointF ): integer; overload;

 // Return TRUE if aSceneCoor point is in map (can be out of view, but on map)
 // if specified, at the end of the function, aPSurfaceCoor contain surface coordinates translated from aSceneCoor
 function SceneCoorIsInMap( aSceneCoor: TPointF;  aPSurfaceCoor: PPointF=NIL ): boolean;

// function SceneCoorToMapPosition( aSceneCoor: TPointF ): TPointF; inline;
// function MapPositionToSceneCoor( aMapPos: TPointF ): TPointF; inline;

 // set the size in pixels of the surface on which the map will be drawn in the scene. You can retrieve it with property Width and Height
 procedure SetViewSize( AWidth, AHeight: integer );

 property MapSize: TSize read GetMapSize; // give total map size in pixel
 property TileSize: TSize read GetTileSize; // give tile size in pixel
 property MapTileCount: TSize read FMapTileCount; // cx = column count     cy = row count
 property HScrollEnable: boolean read FHScrollEnable write FHScrollEnable; // TRUE to enable horizontal scrolling
 property HLoopMode: boolean read FHLoopMode write FHLoopMode;    // set TRUE to repeat the map horizontally
 property VScrollEnable: boolean read FVScrollEnable write FVScrollEnable; // TRUE to enable vertical scrolling
 property VLoopMode: boolean read FVLoopMode write FVLoopMode;    // set TRUE to repeat the map vertically

 property OnTileEvent: TTileEvent read FOnTileEvent write FOnTileEvent;

 property TextureList: TArrayOfTileTexInfo read FTextureList;
end;
{$endif oglcINTERFACE}

{$ifdef oglcIMPLEMENTATION}
{ TTileEngine }

constructor TTileEngine.Create;
begin
 inherited Create;
 ScrollSpeed := TPointFParam.Create;
 PositionOnMap := TPointFParam.Create;
 MapHoleColor := TBGRAParam.Create;
 FOnTileEvent := NIL;

 FVScrollEnable := TRUE;
 FHScrollEnable := TRUE;

 TileMapDesignerModeEnable := FALSE;

 Clear;
end;

destructor TTileEngine.Destroy;
begin
 Clear;
 FreeAndNil( ScrollSpeed );
 FreeAndNil( PositionOnMap );
 FreeAndNil( MapHoleColor );
 inherited Destroy;
end;

procedure TTileEngine.Update(const aElapsedTime: single);
begin
 if FFreeze then exit;

 inherited Update( aElapsedTime );

 ScrollSpeed.OnElapse( aElapsedTime );
  if ScrollSpeed.x.Value <> 0.000
    then PositionOnMap.x.Value := PositionOnMap.x.Value + aElapsedTime * ScrollSpeed.x.Value
    else PositionOnMap.x.OnElapse( aElapsedTime );
  if ScrollSpeed.y.Value <> 0.000
    then PositionOnMap.y.Value := PositionOnMap.y.Value - aElapsedTime * ScrollSpeed.y.Value
    else PositionOnMap.y.OnElapse( aElapsedTime );

  MapHoleColor.OnElapse( aElapsedTime );
end;

procedure TTileEngine.EnsurePositionInMap(var APos: TPointF);
begin
 // ensure position on map is ok according horizontal and vertical loop mode
 if ( APos.x < 0 )
  then begin
        if not FHScrollEnable
         then APos.x := 0
         else if FHLoopMode
                then begin
                      repeat
                       APos.x := APos.x + FMapSize.cx ;
                      until APos.x > 0 ;
                     end;
       end
  else if APos.x >= FMapSize.cx - FWidth
         then begin
               if not FHScrollEnable
                then APos.x := FMapSize.cx - Width
                else if FHLoopMode
                       then begin
                             if APos.x >= FMapSize.cx
                               then repeat
                                     APos.x := APos.x - FMapSize.cx ;
                                    until APos.x < FMapSize.cx ;
                            end
              end;

 if APos.y < 0
  then begin
        if not FVScrollEnable
         then APos.y := 0
         else if FVLoopMode
               then begin
                     repeat
                      APos.y := APos.y + FMapSize.cy;
                     until APos.y > 0 ;
                    end;
       end
  else if APos.y >= FMapSize.cy - FHeight
         then begin
               if not FVScrollEnable
                 then APos.y := FMapSize.cy - Height
                 else if FVLoopMode
                        then if APos.y >= FMapSize.cy
                               then begin
                                     repeat
                                      APos.y := APos.y - FMapSize.cy;
                                     until APos.y < FMapSize.cy ;
                                    end;
              end;
end;

procedure TTileEngine.ComputeIndexes(APosition: TPointF);
begin
 EnsurePositionInMap( {%H-}APosition );

 FColumnIndexFirstTile := trunc( APosition.x / FTileSize.cx );
 if APosition.x < 0 then dec( FColumnIndexFirstTile );
 FXOffsetFirstTile := - ( APosition.x / FTileSize.cx - FColumnIndexFirstTile ) * FTileSize.cx ;

 FRowIndexFirstTile := trunc( APosition.y / FTileSize.cy ) ;
 if APosition.y < 0 then dec( FRowIndexFirstTile );
 FYOffsetFirstTile := - ( APosition.y / FTileSize.cy - FRowIndexFirstTile ) * FTileSize.cy ;
end;

procedure TTileEngine.Draw(const aLayerPercentOpacity: single);
var ixtile, iytile : integer;
    indexTex, indexFrame: integer;
    xx, yy, xxx, yyy: single;
    c: TBGRAPixel;
    p: TPointF;

    procedure DrawColoredSquare; inline;
    begin
     if c.alpha = 0 then exit;
     FillBox( xx + x.Value, yy + y.Value, FTileSize.cx, FTileSize.cy, c );
    end;

begin
 if not Visible then exit;

 c := MapHoleColor.Value;
 c.alpha := round( Opacity.Value * aLayerPercentOpacity * c.alpha/255 );

// PushAndPrepareMatrix;          <- issue with glScissor
 SetBlendMode( FBlendMode );

 if ( Length( FMap ) = 0 ) or ( GetTextureCount = 0 ) then begin
  FillBox( x.Value, y.Value, Width-1, Height-1, c );
 end else begin
  // compute first tile indexes and its offset
  p := PositionOnMap.Value;
  ComputeIndexes( p );

  if not (TileMapDesignerModeEnable or ForceTheDrawingOfAllTheTiles) then
  begin
   glScissor( round(x.Value), round(y.Value), Width, Height );
   glEnable( GL_SCISSOR_TEST );
  end;

  yy := FYOffsetFirstTile;
  iytile := FRowIndexFirstTile;

  repeat
    xx := FXOffsetFirstTile;
    ixtile := FColumnIndexFirstTile;

    repeat
      xxx := xx + x.Value;
      yyy := yy + y.Value;
      if not( ( xxx > FParentScene.Width ) or ( xxx+TileSize.cx < 0 ) or
              ( yyy > FParentScene.Height) or ( yyy+TileSize.cy < 0 ) ) or
         ForceTheDrawingOfAllTheTiles
       then begin  // draw only non clipped tile
             if ( ixtile < 0 ) or ( ixtile >= FMapTileCount.cx ) or
                ( iytile < 0 ) or ( iytile >= FMapTileCount.cy )
               then DrawColoredSquare
               else begin
                     if ( FMap[iytile,ixtile].TextureIndex > -1 )
                      then begin
                            indexTex := FMap[iytile,ixtile].TextureIndex;
                            indexFrame := TileEngineIndexFrameToTextureIndexFrame( indexTex, FMap[iytile,ixtile].ixFrame, FMap[iytile,ixtile].iyFrame );

                            TextureManager.BindTexture(FTextureList[indexTex].Texture);
                            glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);//GL_LINEAR);
                            glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);//GL_LINEAR);


                            DrawTexture( FTextureList[indexTex].Texture, indexFrame, xxx, yyy, 0, round( Opacity.Value * aLayerPercentOpacity ), Tint.Value );
                           end
                      else DrawColoredSquare;
                     // callback event
                     if ( FOnTileEvent <> NIL ) and (( FMap[iytile][ixtile].UserEvent <> VALUE_NO_EVENT ) or TileMapDesignerModeEnable )
                       then FOnTileEvent( self, PointF(xxx,yyy), FMap[iytile][ixtile].UserEvent );
               end;
       end;

      xx += FTileSize.cx;
      inc(ixtile);
      if FHLoopMode and ( ixtile >= FMapTileCount.cx ) then ixtile := 0;
    until xx >= FWidth;

    yy += FTileSize.cy;
    inc(iytile);
    if FVLoopMode and ( iytile >= FMapTileCount.cy ) then iytile := 0;
  until yy >= FHeight;

  glDisable( GL_SCISSOR_TEST );
 end;

 DrawChildSurfaces( aLayerPercentOpacity );
// PopMatrix;
end;

function TTileEngine.GetWidth: integer;
begin
 Result := FWidth;
end;

function TTileEngine.GetHeight: integer;
begin
 Result := FHeight;
end;

function TTileEngine.AddTexture(AFilename: string; AFrameWidth, AFrameHeight: integer): integer;
var ix, iy: integer;
    fc: TSize;
begin
 Result := Length( FTextureList );
 SetLength( FTextureList, Result + 1 );
 FTextureList[Result].Texture := TextureManager.Add( AFilename );
 TextureManager.SetFrameSize( FTextureList[Result].Texture, AFrameWidth, AFrameHeight );

 SetTileSize( AFrameWidth, AFrameHeight );

 TextureManager.BindTexture( FTextureList[Result].Texture );
 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP ); //GL_CLAMP_TO_EDGE );
 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP ); //GL_CLAMP_TO_EDGE );
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST); // nearest Filtering
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST); // nearest Filtering

 // tile type to default (neutral=1)
 fc := TextureManager.GetFrameCount( FTextureList[Result].Texture );
 SetLength( FTextureList[Result].TileType, fc.cx, fc.cy );

 for ix := 0 to fc.cx-1 do
  for iy := 0 to fc.cy-1 do
   FTextureList[Result].TileType[ix][iy] := 1;  // neutral ground type by default
end;

procedure TTileEngine.DeleteTexture(AIndex: integer);
var i, co, ro: integer;
    p: PTile;
begin
 if not IsValidTextureIndex( AIndex ) then exit;

 for ro:=0 to FMapTileCount.cy-1 do
  for co:=0 to FMapTileCount.cx-1 do begin
    p := GetPTile( ro, co );
    if p^.TextureIndex = AIndex then SetCell( ro, co, -1, 0, 0 );
  end;

 for i:= AIndex to Length( FTextureList ) - 2 do
  FTextureList[i] := FTextureList[i+1];
 SetLength( FTextureList, Length( FTextureList ) - 1 );
end;

function TTileEngine.GetTexture(AIndex: integer): PTexture;
begin
 if (AIndex >= 0) and (AIndex<Length(FTextureList))
   then Result := FTextureList[AIndex].Texture
   else Result := NIL;
end;

function TTileEngine.GetTextureCount: integer;
begin
 Result := Length( FTextureList );
end;

procedure TTileEngine.SetMapTileCount(ARowCount, AColumnCount: integer);
var ro, co, oldRowCount, oldColumnCount: integer;
begin
 oldRowCount := MapTileCount.cy;
 oldColumnCount := MapTileCount.cx;

 SetLength( FMap, ARowCount, AColumnCount );
 FMapTileCount.cx := AColumnCount;
 FMapTileCount.cy := ARowCount;

 FMapSize.cx := FMapTileCount.cx * FTileSize.cx;
 FMapSize.cy := FMapTileCount.cy * FTileSize.cy;

 if oldColumnCount < AColumnCount then
   for ro:=0 to ARowCount-1 do
    for co:=oldColumnCount to AColumnCount-1 do
     begin
      FMap[ro][co].TextureIndex := -1;
      FMap[ro][co].UserEvent := VALUE_NO_EVENT;
     end;

 if oldRowCount < ARowCount then
  for ro:=oldRowCount to ARowCount-1 do
   for co:=0 to AColumnCount-1 do
    begin
     FMap[ro][co].TextureIndex := -1;
     FMap[ro][co].UserEvent := VALUE_NO_EVENT;
    end;
end;

procedure TTileEngine.InsertRow(ARowIndex, ACount: integer);
var ro, co, oldrowcount: integer;
begin
 if ACount <= 0 then exit;
 if not IsValidRow( ARowIndex ) then exit;

 oldrowcount := MapTileCount.cy;
 SetMapTileCount( MapTileCount.cy + ACount, MapTileCount.cx );

 if ARowIndex <= oldrowcount-1 then
 for ro:=FMapTileCount.cy-1 downto ARowIndex+ACount do
  for co:=0 to FMapTileCount.cx-1 do begin
    FMap[ro][co].TextureIndex := FMap[ro-ACount][co].TextureIndex;
    FMap[ro][co].ixFrame := FMap[ro-ACount][co].ixFrame;
    FMap[ro][co].iyFrame := FMap[ro-ACount][co].iyFrame;
    FMap[ro][co].UserEvent := FMap[ro-ACount][co].UserEvent;
  end;

 for ro:=ARowIndex to ARowIndex+ACount-1 do
  for co:=0 to FMapTileCount.cx-1 do
   begin
    FMap[ro][co].TextureIndex := -1;
    FMap[ro][co].UserEvent := VALUE_NO_EVENT;
   end;
end;

procedure TTileEngine.InsertColumn(AColumnIndex, ACount: integer);
var ro, co: integer;
begin
 if ACount <= 0 then exit;
 if not IsValidColumn( AColumnIndex ) then exit;

 SetMapTileCount( MapTileCount.cy , MapTileCount.cx + ACount );

 for ro:=0 to FMapTileCount.cy-1 do
  for co:=FMapTileCount.cx-1 downto AColumnIndex+ACount do begin
    FMap[ro][co].TextureIndex := FMap[ro][co-ACount].TextureIndex;
    FMap[ro][co].ixFrame := FMap[ro-ACount][co].ixFrame;
    FMap[ro][co].iyFrame := FMap[ro][co-ACount].iyFrame;
    FMap[ro][co].UserEvent := FMap[ro][co-ACount].UserEvent;
  end;

 for ro:=0 to FMapTileCount.cy-1 do
  for co:=AColumnIndex to AColumnIndex+ACount-1 do
   begin
    FMap[ro][co].TextureIndex := -1;
    FMap[ro][co].UserEvent := VALUE_NO_EVENT;
   end;
end;

procedure TTileEngine.ResetMap;
var co, ro: integer;
begin
 for ro:=0 to Length(FMap)-1 do
  for co:=0 to Length(FMap[ro])-1 do
   begin
    SetCell( ro, co, -1, 0, 0 );
    SetUserEventValue( ro, co, VALUE_NO_EVENT );
   end;
end;

procedure TTileEngine.SetTileSize(AHSize, AVSize: integer);
begin
 FTileSize.cx := AHSize;
 FTileSize.cy := AVSize;
end;

function TTileEngine.IsValidRow(ARow: integer): boolean;
begin
 Result :=  ( ARow >= 0 ) and ( ARow < MapTileCount.cy );
end;

function TTileEngine.IsValidColumn(AColumn: integer): boolean;
begin
 Result := ( AColumn >= 0 ) and ( AColumn < MapTileCount.cx );
end;

function TTileEngine.IsValidTextureIndex(AITex: integer): boolean;
begin
 Result := ( AITex >= 0 ) and ( AITex < Length( FTextureList ));
end;

function TTileEngine.IsValidFrameIndex(ATextureIndex, ixFrame, iyFrame: integer ): boolean;
var fc: TSize;
begin
 Result := IsValidTextureIndex( ATextureIndex );
 if Result then begin
  fc := TextureManager.GetFrameCount( FTextureList[ATextureIndex].Texture );
  Result := ( ixFrame > -1 ) and ( ixFrame < fc.cx ) and
            ( iyFrame > -1 ) and ( iyFrame < fc.cy );
 end;
end;

function TTileEngine.TileEngineIndexFrameToTextureIndexFrame(iTexture, ixFrame, iyFrame: integer ): integer;
var p: PTexture;
begin
 p := GetTexture( iTexture );
 if p^.FrameCount = 1
   then Result := 0
   else Result := iyFrame * TextureManager.GetFrameCount( p ).cx + ixFrame + 1;  //[ix,iy]=[0,0]=> FrameIndex := 1
end;

procedure TTileEngine.SetCell(ARow, AColumn, ATextureIndex, ixFrame, iyFrame: integer);
var p: PTile;
    fc: TSize;
begin
 p := GetPTile( ARow, AColumn );
 if p = NIL then exit;

 p^.TextureIndex := ATextureIndex;
 p^.ixFrame := ixFrame;
 p^.iyFrame := iyFrame;

 if ATextureIndex = -1 then exit;

 fc:= TextureManager.GetFrameCount( FTextureList[ATextureIndex].Texture );
 if p^.ixFrame >= fc.cx then  p^.ixFrame := fc.cx-1;
 if p^.ixFrame < 0 then  p^.ixFrame := 0;

 if p^.iyFrame >= fc.cy then  p^.iyFrame := fc.cy-1;
 if p^.iyFrame < 0 then  p^.iyFrame := 0;
end;

procedure TTileEngine.SetUserEventValue(ARow, AColumn, AUserEvent: integer);
var p: PTile;
begin
 p := GetPTile( ARow, AColumn );
 if p <> NIL then p^.UserEvent := AUserEvent;
end;

function TTileEngine.GetUserEventValue(ARow, AColumn: integer): integer;
var p: PTile;
begin
 p := GetPTile( ARow, AColumn );
 if p <> NIL
   then Result := p^.UserEvent
   else Result := VALUE_NO_EVENT;
end;

function TTileEngine.GetPTile(ARow, AColumn: integer): PTile;
begin
 if IsValidRow( ARow ) and IsValidColumn( AColumn )
   then Result := @FMap[ARow][AColumn]
   else Result := NIL;
end;

function TTileEngine.GetPTileTexInfo(ARow, AColumn: integer): PTileTexInfo;
var p: PTile;
begin
 p := GetPTile( ARow, AColumn );
 if p = NIL
   then Result := NIL
   else Result := @FTextureList[p^.TextureIndex];
end;

procedure TTileEngine.SetGroundType(ATextureIndex, ixFrame, iyFrame, AGroundType: integer);
begin
 if not IsValidFrameIndex( ATextureIndex, ixFrame, iyFrame ) then exit;
 FTextureList[ATextureIndex].TileType[ixFrame][iyFrame] := AGroundType;
end;

function TTileEngine.GetGroundType(ATextureIndex, ixFrame, iyFrame: integer ): integer;
begin
 if not IsValidFrameIndex( ATextureIndex, ixFrame, iyFrame )
   then Result := 0    // value for ground 'hole'
   else Result := FTextureList[ATextureIndex].TileType[ixFrame][iyFrame];
end;

procedure TTileEngine.ClearCell(ARow, AColumn: integer);
begin
 if ( AColumn < 0 ) or ( AColumn > MapTileCount.cx-1 ) then exit;
 if ( ARow < 0 ) or ( ARow > MapTileCount.cy-1 ) then exit;

 FMap[ARow][AColumn].TextureIndex := -1;
end;

function TTileEngine.GetBoundedPositionOnMap: TPointF;
begin
 Result := PositionOnMap.Value;
 EnsurePositionInMap( Result );
end;

procedure TTileEngine.Clear;
var i : integer;
begin
 SetLength( FMap, 0 );

 for i:=0 to Length( FTextureList )-1 do
  TextureManager.Delete ( FTextureList[i].Texture );
 SetLength( FTextureList, 0 );

 FMapSize.cx := 0;
 FMapSize.cy :=0;
 FMapTileCount.cx := 0;
 FMapTileCount.cy := 0;
// FPositionOnMap := PointF( 0, 0 );
// ScrollSpeed.Value := PointF( 0, 0 );
// MapHoleColor.Value := BGRA(0,0,0,0);
end;

procedure TTileEngine.SetViewSize(AWidth, AHeight: integer);
begin
 if AWidth >= 0 then FWidth := AWidth;
 if AHeight >= 0 then FHeight := AHeight;
end;

function TTileEngine.GetFirstTileColumnIndex: integer;
begin
 Result := FColumnIndexFirstTile;
end;

function TTileEngine.GetFirstTileRowIndex: integer;
begin
 Result := FRowIndexFirstTile;
end;

function TTileEngine.GetGroundType(aSceneCoor: TPointF): integer;
var p: PTile;
    sc: TPointF;
    ro, co: integer;
begin
 Result := 1;    // Neutral ground by default
 if ( TileSize.cx = 0 ) or ( TileSize.cy = 0 ) then exit;
 if not SceneCoorIsInMap( aSceneCoor, @sc ) then exit;

 co := FColumnIndexFirstTile + trunc((sc.x-FXOffsetFirstTile)/FTileSize.cx);
 ro := FRowIndexFirstTile + trunc((sc.y-FYOffsetFirstTile)/FTileSize.cy);

 p := GetPTile( ro, co );
 if p <> NIL
   then Result := GetGroundType( p^.TextureIndex, p^.ixFrame, p^.iyFrame );
end;

function TTileEngine.GetUserEventValue(aSceneCoor: TPointF): integer;
var p: PTile;
    sc: TPointF;
    ro, co: integer;
begin
 Result := VALUE_NO_EVENT;
 if ( TileSize.cx = 0 ) or ( TileSize.cy = 0 ) then exit;
 if not SceneCoorIsInMap( aSceneCoor, @sc ) then exit;

 co := FColumnIndexFirstTile + trunc((sc.x-FXOffsetFirstTile)/FTileSize.cx);
 ro := FRowIndexFirstTile + trunc((sc.y-FYOffsetFirstTile)/FTileSize.cy);

 p := GetPTile( ro, co );
 if p <> NIL then Result := p^.UserEvent;
end;

function TTileEngine.SceneCoorIsInMap(aSceneCoor: TPointF;  aPSurfaceCoor: PPointF): boolean;
var f1, f2: boolean;
    P: TPointF;
begin
 P := SceneToSurface( aSceneCoor );
 f1 := ( P.x >= 0 ) and ( P.x < FMapSize.cx );
 f1 := f1 OR FHLoopMode;

 f2 := ( P.y >= 0 ) and ( P.y < FMapSize.cy );
 f2 := f2 OR FVLoopMode;

 Result := f1 and f2 ;

 if aPSurfaceCoor <> NIL then aPSurfaceCoor^ := p;
end;

function TTileEngine.GetTileSize: TSize;
begin
 Result := FTileSize;
end;

function TTileEngine.GetMapSize: TSize;
begin
 Result := FMapSize;
end;

procedure TTileEngine.LoadMapFile(const aFilename: string);
var FMapFile: TStringList;
    path: string;
    i, j, k, z, Count, ix, iy : integer;
    txt:string;
    txtsplited, temp: ArrayOfString;
begin
 Clear ;
 FMapFile := TStringList.Create;
 FMapFile.LoadFromFile( aFilename );
 path := ExtractFilePath( aFileName );

 // load map's textures
 k := FMapFile.IndexOf('TEXTURES');
 if k=-1 then raise exception.Create('Tile engine - LoadMap error : can''t retrieve texture list');
 inc(k);
 Count := strtoint( FMapFile.Strings[k] );
 inc(k);
 for i:=0 to Count-1 do
  begin
   txtsplited := SplitLineToStringArray( FMapFile.Strings[k], '|' );
   if Length( txtsplited ) <> 3
     then raise exception.Create('Tile engine - LoadMap error : can''t retrieve texture name and frame size');
   AddTexture( path + txtsplited[0], strtoint( txtsplited[1] ), strtoint( txtsplited[2] ));
   inc(k);
  end;

 // load Frame Type
 //  FRAMES_TYPE
 //  texture_count
 //  ixCount|iyCount|values_first_line|values_second_line|...        <- texture 1
 //  ixCount|iyCount|values_first_line|values_second_line|...        <- texture 2
 //  ....
 k := FMapFile.IndexOf('FRAMES_TYPE');
 if k <> -1 then begin
  inc (k);
  Count := strtoint( FMapFile.Strings[k] );  // texture count
  inc(k);
  for i:=0 to Count-1 do
   begin
    z := 0;
    txtsplited := SplitLineToStringArray( FMapFile.Strings[k], '|' );
    for iy:=0 to strtoint(txtsplited[1])-1 do
     for ix:=0 to strtoint(txtsplited[0])-1 do
      begin
       SetGroundType( i, ix, iy, strtoint( txtsplited[2+z{ix}] ));
       inc(z);
      end;
    inc (k);
   end;
 end;

 // Draw Size
 k := FMapFile.IndexOf('DRAW_SIZE');
 if k <> -1
   then begin
         inc(k);
         txtsplited := SplitLineToStringArray( FMapFile.Strings[k], '|' );
         if Length( txtsplited ) <> 2
           then raise exception.Create('Tile engine - LoadMap error : can''t retrieve draw size');
         SetViewSize( strtoint( txtsplited[0]), strtoint( txtsplited[1] ));
         if FParentScene<>NIL then begin // center on scene by default
           x.Value := ( FParentScene.Width - FWidth ) / 2;
           y.Value := ( FParentScene.Height - FHeight ) / 2;
         end;
   end;

 // Tile Size
 k := FMapFile.IndexOf('TILE_SIZE');
 if k = -1
   then raise exception.Create('Tile engine - LoadMap error : can''t retrieve tile size');
 inc(k);
 txtsplited := SplitLineToStringArray( FMapFile.Strings[k], '|' );
 if Length( txtsplited ) <> 2
   then raise exception.Create('Tile engine - LoadMap error : can''t retrieve tile size');
 SetTileSize( strtoint( txtsplited[0] ), strtoint( txtsplited[1] ) );

 // Map tile count
 k := FMapFile.IndexOf('MAP_SIZE');
 if k = -1
   then raise exception.Create('Tile engine - LoadMap error : can''t retrieve map tile count');
 inc(k);
 txtsplited := SplitLineToStringArray( FMapFile.Strings[k], '|' );
 if Length( txtsplited ) <> 2
   then raise exception.Create('Tile engine - LoadMap error : can''t retrieve map size');
 SetMapTileCount( strtoint( txtsplited[0] ), strtoint( txtsplited[1] ) );

 // Scroll enable
 FHScrollEnable := FALSE;
 FVScrollEnable := FALSE;
 k := FMapFile.IndexOf('SCROLL_ENABLE');
 if k <> -1 then begin
   txt := UpperCase(FMapFile.Strings[k+1]);
   FHScrollEnable := ( txt='H' ) or ( txt='VH' ) or (  txt='HV' );
   FVScrollEnable := ( txt='V' ) or ( txt='VH' ) or (  txt='HV' );
 end;
 // Scroll Loop Mode
 FHLoopMode := FALSE;
 FVLoopMode := FALSE;
 k := FMapFile.IndexOf('SCROLL_LOOP_MODE');
 if k <> -1 then begin
   txt := UpperCase(FMapFile.Strings[k+1]);
   FHLoopMode := ( txt='H' ) or ( txt='VH' ) or (  txt='HV' );
   FVLoopMode := ( txt='V' ) or ( txt='VH' ) or (  txt='HV' );
 end;

 // Start tile
 PositionOnMap.Value := PointF( 0, 0);
 k := FMapFile.IndexOf('START_TILE');
 if k <> -1 then begin
   txtsplited := SplitLineToStringArray( FMapFile.Strings[k+1], '|' );
   if Length( txtsplited ) = 2 then begin
     PositionOnMap.x.Value := strtoint( txtsplited[0] ) * FTileSize.cx;
     PositionOnMap.y.Value := strtoint( txtsplited[1] ) * FTileSize.cy;
   end;
 end;

 // hole color
 MapHoleColor.Value := BGRA(0,0,0,255);
 k := FMapFile.IndexOf('HOLE_COLOR');
 if k <> -1 then MapHoleColor.Value := HexToBGRAPixel( FMapFile.Strings[k+1] );

 // Build map from Data
 k := FMapFile.IndexOf('DATA');
 if k = -1 then raise exception.Create('Tile engine - LoadMap error : can''t retrieve map data');
 inc(k);

 for i:=0 to FMapTileCount.cy-1 do
  begin
   j := 0;
   temp := SplitLineToStringArray( FMapFile.Strings[k], ' ' );
   for z:=0 to Length( temp )-1 do
    begin
     txtsplited := SplitLineToStringArray( temp[z], ',' );

     case txtsplited[0][1] of
      'T' : begin
             SetCell( i, j, strtoint( copy(txtsplited[0], 2, length( txtsplited[0] ) - 1 )),
                            strtoint( txtsplited[1] ),
                            strtoint( txtsplited[2] ));
             SetUserEventValue( i, j, strtoint( txtsplited[3] ));
             inc(j);
            end;
     end;//case

    end;
   inc(k);
  end;

 FMapFile.Free;
 ScrollSpeed.Value := PointF( 0, 0 );
end;
{$endif oglcIMPLEMENTATION}

