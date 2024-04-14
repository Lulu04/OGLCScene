{$ifdef oglcINTERFACE}
type


{ TOGLCTileRenderer }
// render in color attachment -> texture

TOGLCTileRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyuv>)
private const
VERTEX_SHADER =
 '#version 330 core'#10+
 '  layout(location = 0) in vec4 aVertexAndTextureCoor;'#10+
 '  uniform mat4 uMVP;'#10+
 '  out vec2 TexCoords;'#10+
 'void main()'#10+
 '{'#10+
 '  gl_Position = uMVP * vec4(aVertexAndTextureCoor.xy, 0.0, 1.0);'#10+ // no transformation
 '  TexCoords = aVertexAndTextureCoor.zw;'#10+
 '}';

FRAGMENT_SHADER =
 '#version 330 core'#10+
 '  in vec2 TexCoords;'#10+
 '  uniform sampler2D uTexUnit;'#10+
 '  uniform vec4 uTintColor;'#10+
 '  uniform float uOpacity;'#10+
// '  layout(location = 1) out vec4 FragColor;'#10+  // location = 1 means output in GL_COLOR_ATTACHMENT1
 '  layout(location = 0) out vec4 FragColor;'#10+
 'void main()'#10+
 '{'#10+
 '  vec4 col = texture(uTexUnit, TexCoords);'#10+
 '  if (col.a == 0.0) discard;'#10+
 // Tint
 '  float a = col.a*uOpacity;'#10+
 '  float tintAlphaX2 = uTintColor.a*2;'#10+
 '  if (uTintColor.a >= 0.5)'#10+
 '   {'#10+
 '     tintAlphaX2 = tintAlphaX2-1;'#10+
 '     col = col * (1-tintAlphaX2) + uTintColor * tintAlphaX2;'#10+  // replace color
 '   }'#10+
 '  else if (uTintColor.a > 0)'#10+
 '     col = col + uTintColor*tintAlphaX2;'#10+     // mix color

 '  col.a = a;'#10+
 '  FragColor = col;'#10+
 '}' ;
private
  FLocMVP,
  FLocTextureUnit,
  FLocTintColor,
  FLocOpacity: GLint;
  FMVP: TOGLCMatrix;
  FTexture: PTexture;
  FTintF: TColorF;
  FOpacity: single;
  procedure InitShaderCodeAndCallBack; override;
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix; aTexture: PTexture; aTextureUnit: integer;
    const aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);
  procedure PushQuad(const aFrameIndex, aFlipIndex: integer; const aXPosOffset, aYPosOffset: single);
end;


{ TTileEngine }
const
VALUE_NO_EVENT = -1;

type
// a tilemap can have several textures.
// each tile have a texture index, (ix,iy) indexes to reference the frame in this texture
// and an user value to trigger an event when the tile is drawn.
TTile = record
         TextureIndex,       // index of texture in TTilemap.FTextureList
         ixFrame,            // frame x index on texture
         iyFrame,            // frame y index
         UserEvent: integer; // when the tile is drawn, TTileEngine.OnTileEvent is called with this parameter
end;
PTile = ^TTile;

// 2D map is an array of array of TTile
TArrayOfArrayOfTile = array of array of TTile;

TArrayOfArrayOfInteger = array of array of integer;
// each texture (or tileset) have an array value that allows to identify the type for each tile
// ground, water, wall,...
TTileTexInfo = record
  Texture: PTexture;
  TileType: TArrayOfArrayOfInteger;  // each ground type by frame column and row  [x][y]
end;
PTileTexInfo = ^TTileTexInfo;
TArrayOfTileTexInfo = array of TTileTexInfo;
PArrayOfTileTexInfo = ^TArrayOfTileTexInfo;


TTileEvent = procedure (Sender: TTileEngine; const TileTopLeftCoor: TPointF; EventValue: integer) of object;

TTileEngine = class(TSimpleSurfaceWithEffect)
private
 FRenderToTexture: TOGLCRenderToTexture;
 FClipArea: TUIClipArea;
 FTextureList: TArrayOfTileTexInfo;
 FTextureListOwner: boolean;
 FPTextureList: PArrayOfTileTexInfo;
 FMap: TArrayOfArrayOfTile; // [row,column]
 FMapSize: TSize;     // total map size in pixel
 FMapTileCount: TSize;
 FWidth, FHeight: integer;   // screen surface size
 FTileSize: TSize;
 FVScrollEnable,
 FHScrollEnable,
 FHLoopMode,
 FVLoopMode: boolean;
 FColumnIndexFirstTile: integer;
 FRowIndexFirstTile: integer;
 FXOffsetFirstTile: single;
 FYOffsetFirstTile: single;
 FOnTileEvent: TTileEvent ; // callback for event
 function GetTextureList: TArrayOfTileTexInfo;
 function GetTileSize: TSize;
 function GetMapSize: TSize;
 procedure EnsurePositionInMap(var APos: TPointF);
 procedure ComputeIndexes(APosition: TPointF);
 function IsValidRow(ARow: integer): boolean;
 function IsValidColumn(AColumn: integer): boolean;
 function IsValidTextureIndex(AITex: integer): boolean;
 function IsValidFrameIndex(ATextureIndex, ixFrame, iyFrame: integer): boolean;
 function TileEngineIndexFrameToTextureIndexFrame(iTexture, ixFrame, iyFrame: integer): integer;
 procedure Clear;
private
  procedure DoLoadTextureFromStringList(t: TStringList; const aPathToTexImage: string);
  procedure DoLoadFramesTypeFromStringList(t: TStringList);
  procedure DoLoadDrawSizeFromStringList(t: TStringList);
  procedure DoLoadTileSizeFromStringList(t: TStringList);
  procedure DoLoadMapSizeFromStringList(t: TStringList);
  procedure DoLoadScrollInfoFromStringList(t: TStringList);
  procedure DoLoadStartTileFromStringList(t: TStringList);
  procedure DoLoadHoleColorFromStringList(t: TStringList);
  procedure DoLoadMapDataFromStringList(t: TStringList);
protected
 function GetWidth: integer; override;
 function GetHeight: integer; override;
public
 procedure Update( const aElapsedTime: single ); override;
 procedure Draw( const aLayerPercentOpacity: single ); override;
 procedure DoDraw; override;
public
 Constructor Create(aParentScene: TOGLCScene);
 Destructor Destroy; override;
public
 // methods to construct map. Used by 'Tile Map Designer' utility
 TileMapDesignerModeEnable: boolean;
 procedure SetTileSize(AHSize, AVSize: integer);
 function GetFirstTileColumnIndex: integer;
 function GetFirstTileRowIndex: integer;
 function AddTexture(AFilename: string; AFrameWidth, AFrameHeight: integer): integer; // return the index of created texture
 function AddTexture(aTexture: PTexture): integer;
 procedure DeleteTexture(AIndex: integer);
 function GetPTextureList: PArrayOfTileTexInfo;
 procedure SetTextureListFromAnotherTileEngine(aTileEngine: TTileEngine);
 function GetTexture(AIndex: integer): PTexture;
 function GetTextureCount: integer;
 procedure SetMapTileCount(ARowCount, AColumnCount: integer);
 procedure InsertRow(ARowIndex, ACount: integer);
 procedure InsertColumn(AColumnIndex, ACount: integer);
 procedure ResetMap;
 function GetPTile(ARow, AColumn: integer): PTile;
 function GetPTileTexInfo(ARow, AColumn: integer): PTileTexInfo;
 procedure SetUserEventValue(ARow, AColumn, AUserEvent: integer);
 function GetUserEventValue(ARow, AColumn: integer): integer; overload;
 procedure SetCell(ARow, AColumn, ATextureIndex, ixFrame, iyFrame: integer);
// procedure GetCellTextureAndFrameIndex( ARow, AColumn: integer; out TextureIndex, ixFrame, iyFrame: integer );
 procedure SetGroundType(ATextureIndex, ixFrame, iyFrame, AGroundType: integer);
 function GetGroundType(ATextureIndex, ixFrame, iyFrame: integer): integer; overload;
 procedure ClearCell(ARow, AColumn: integer);
public
 // set to TRUE if you use a camera to move on the map instead of using PositionOnMap property
 // slowly because all the tiles are drawn even if they are not visible
 ForceTheDrawingOfAllTheTiles: boolean;

 // this is virtual position of the top left corner of the view
 // why virtual ? Because when HLoopMode or VLoopMode is set to TRUE, the map repeats itself indefinitely
 // and position can be greater than view size, or even negative
 // for example, your map size is 100x100 pixels and you set HLoopMode to TRUE, virtual position (15000,80)
 // is valid because map repeat itself on horizontal axis
 PositionOnMap: TPointFParam;

 // scrolling speed in pixel per seconds for both axis x y
 ScrollSpeed: TPointFParam;

 // color for the 'holes' in the map
 MapHoleColor: TBGRAParam;

 // give real position on map pointed by 'PositionOnMap'
 function GetBoundedPositionOnMap: TPointF;

 // load map previously created with 'Tile Map Designer' tool
 // aTileEngineToCopy is an existing TileEngine wich have already all the tilesets loading in.
 // Use it if several maps have the same tileset than another
 // example:
 //     TileEngineONE.LoadFromFile('filemapONE.map');
 //     TileEngineTWO.LoadFromFile('filemapTWO.map', TileEngineONE);
 procedure LoadMapFile(const aFilename: string; aTileEngineToCopy: TTileEngine=NIL);

  // load map previously created with 'Tile Map Designer' tool and replace the
  // textures used by the specified ones.
  // aTextures count must be the same as those specified in the map file.
  procedure LoadMapFile(const aFilename: string; const aTextures: array of PTexture);

 // Give the type of the tile pointed by specified position on the scene (and not on map). Can be out of the view, but still on map
 function GetGroundType( aSceneCoor: TPointF ): integer; overload;

 // Give event's value associed at the tile pointed by specified position on the scene
 function GetUserEventValue(aSceneCoor: TPointF): integer; overload;

 // Return TRUE if aSceneCoor point is in map (can be out of view, but on map)
 // if specified, at the end of the function, aPSurfaceCoor contain surface coordinates translated from aSceneCoor
 function SceneCoorIsInMap(aSceneCoor: TPointF;  aPSurfaceCoor: PPointF=NIL): boolean;

// function SceneCoorToMapPosition( aSceneCoor: TPointF ): TPointF; inline;
// function MapPositionToSceneCoor( aMapPos: TPointF ): TPointF; inline;

 // set the size in pixels of the surface on which the map will be drawn in the scene. You can retrieve it with property Width and Height
 procedure SetViewSize(AWidth, AHeight: integer);

 property MapSize: TSize read GetMapSize; // give total map size in pixel
 property TileSize: TSize read GetTileSize; // give tile size in pixel
 property MapTileCount: TSize read FMapTileCount; // cx = column count     cy = row count
 property HScrollEnable: boolean read FHScrollEnable write FHScrollEnable; // TRUE to enable horizontal scrolling
 property HLoopMode: boolean read FHLoopMode write FHLoopMode;    // set TRUE to repeat the map horizontally
 property VScrollEnable: boolean read FVScrollEnable write FVScrollEnable; // TRUE to enable vertical scrolling
 property VLoopMode: boolean read FVLoopMode write FVLoopMode;    // set TRUE to repeat the map vertically

 property OnTileEvent: TTileEvent read FOnTileEvent write FOnTileEvent;

 property TextureList: TArrayOfTileTexInfo read GetTextureList;
 property TexturesOwner: boolean read FTextureListOwner write FTextureListOwner; // don't use please
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCTileRenderer }

procedure TOGLCTileRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCTileRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuv), PChar(0));
  glEnableVertexAttribArray(0);
end;

procedure TOGLCTileRenderer.GetUniformLocation;
begin
  with FShader do
  begin
    //Use;
    FLocMVP := GetUniform('uMVP');
    FLocTextureUnit := GetUniform('uTexUnit');
    FLocOpacity := GetUniform('uOpacity');
    FLocTintColor := GetUniform('uTintColor');
   // Release;
  end;
end;

procedure TOGLCTileRenderer.SetUniformValuesAndTexture;
begin
  glUniform1i(FLocTextureUnit, GL_TEXTURE0+FTextureUnit);
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
  glUniform4fv(FLocTintColor, 4, @FTintF);
  glUniform1f(FLocOpacity, FOpacity);
  FParentScene.TexMan.Bind(FTexture, FTextureUnit);
end;

procedure TOGLCTileRenderer.Prepare(aTriangleType: TTriangleType; const aMVP: TOGLCMatrix; aTexture: PTexture;
  aTextureUnit: integer; const aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);
var forceFlush: boolean;
begin
  forceFlush := not FMVP.EqualTo(aMVP);
  Batch_CheckIfNeedFlush(Self, aTriangleType, aTexture, aTextureUnit, aBlendMode, forceFlush);

  FMVP.CopyFrom(aMVP);
  FTexture := aTexture;
  FTextureUnit := aTextureUnit;
  FOpacity := aOpacity;
  FTintF.CopyFrom(aComputedTint);
end;

procedure TOGLCTileRenderer.PushQuad(const aFrameIndex, aFlipIndex: integer; const aXPosOffset, aYPosOffset: single);
var w, h: single;
    p: Pxyuv;
    tci: PQuadCornerIndexes;
    texCoord: PQuadCoor;
    fi: Integer;
begin
  fi := EnsureRange(aFrameIndex, 0, FTexture^.FrameCount-1);
  texCoord := @FTexture^.FramesCoord[fi];
  w := FTexture^.FrameWidth;
  h := FTexture^.FrameHeight;
  case FCurrentPrimitiveType of
    ptTriangleStrip: begin
      tci := @FLIP_INDEXES[aFlipIndex];
      p := QueryVertex(4);
      with p[0] do begin             // BD
        u := texCoord^[ tci^[0] ].x; // AC
        v := texCoord^[ tci^[0] ].y;
        x := aXPosOffset;
        y := h + aYPosOffset;
      end;
      with p[1] do begin
        u := texCoord^[ tci^[1] ].x;
        v := texCoord^[ tci^[1] ].y;
        x := aXPosOffset;
        y := aYPosOffset;
      end;
      with p[2] do begin
        u := texCoord^[ tci^[2] ].x;
        v := texCoord^[ tci^[2] ].y;
        x := w + aXPosOffset;
        y := h + aYPosOffset;
      end;
      with p[3] do begin
        u := texCoord^[ tci^[3] ].x;
        v := texCoord^[ tci^[3] ].y;
        x := w + aXPosOffset;
        y := aYPosOffset;
      end;
    end;

    ptTriangles: begin
      tci := @FLIP_INDEXES[aFlipIndex];
      p := QueryVertex(6);
      with p[0] do begin  // A           // B   BD
        u := texCoord^[ tci^[0] ].x;     // AC  C
        v := texCoord^[ tci^[0] ].y;
        x := aXPosOffset;
        y := h + aYPosOffset;
      end;
      with p[1] do begin  // B
        u := texCoord^[ tci^[1] ].x;
        v := texCoord^[ tci^[1] ].y;
        x := aXPosOffset;
        y := aYPosOffset;
      end;
      with p[2] do begin  // C
        u := texCoord^[ tci^[2] ].x;
        v := texCoord^[ tci^[2] ].y;
        x := w + aXPosOffset;
        y := h + aYPosOffset;
      end;
      Move(p[2], p[3], SizeOf(Txyuv)); // C
      Move(p[1], p[4], SizeOf(Txyuv)); // B
      with p[5] do begin               // D
        u := texCoord^[ tci^[3] ].x;
        v := texCoord^[ tci^[3] ].y;
        x := w + aXPosOffset;
        y := aYPosOffset;
      end;
    end;
  end;
end;

{ TTileEngine }

constructor TTileEngine.Create(aParentScene: TOGLCScene);
begin
 inherited Create;
 FParentScene := aParentScene;
 ScrollSpeed := TPointFParam.Create;
 PositionOnMap := TPointFParam.Create;
 MapHoleColor := TBGRAParam.Create;
 FOnTileEvent := NIL;

 FVScrollEnable := TRUE;
 FHScrollEnable := TRUE;

 TileMapDesignerModeEnable := FALSE;

 Clear;

 // create the renderer
{ if aParentScene.FTileRenderer = NIL then
   aParentScene.FTileRenderer := TOGLCTileRenderer.Create(aParentScene);   }
end;

destructor TTileEngine.Destroy;
begin
  Clear;
  FreeAndNil(ScrollSpeed);
  FreeAndNil(PositionOnMap);
  FreeAndNil(MapHoleColor);
  FreeAndNil(FRenderToTexture);
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
var v: single;
begin
  EnsurePositionInMap( {%H-}APosition );

  v := APosition.x / FTileSize.cx;
  FColumnIndexFirstTile := Trunc(v);
  if APosition.x < 0 then dec(FColumnIndexFirstTile);
  FXOffsetFirstTile := - (v - FColumnIndexFirstTile) * FTileSize.cx;

  v := APosition.y / FTileSize.cy;
  FRowIndexFirstTile := Trunc(v);
  if APosition.y < 0 then dec(FRowIndexFirstTile);
  FYOffsetFirstTile := - (v - FRowIndexFirstTile) * FTileSize.cy;
end;


procedure TTileEngine.Draw(const aLayerPercentOpacity: single);
var ixtile, iytile, indexTex, indexFrame: integer;
    ptrTile: PTile;
    xx, yy, xxx, yyy: single;
    c: TBGRAPixel;
    p: TPointF;
    t, currentTexture: PTexture;
    tileRenderer: TOGLCTileRenderer;
begin
  FComputedOpacity := Opacity.Value*OneDiv255*aLayerPercentOpacity;
  if FComputedOpacity = 0 then exit;
  FComputedTint.InitFromTint(TintMode, Tint.Value);

  tileRenderer := FParentScene.TileRenderer;

  // if the data map are empty, we draw a colored square with the hole color
  if (Length(FMap) = 0) or (GetTextureCount = 0) then
  begin
    PushAndPrepareSceneMV;
    FParentScene.BlendMode := FBlendMode;
    FillBox(FParentScene, 0, 0, Width-1, Height-1, MapHoleColor.Value, FComputedOpacity);
    DrawChildSurfacesWithPositiveZOrder(FComputedOpacity);
    PopSceneMV;
    exit;
  end;

  // compute first tile indexes and its offset
  p := PositionOnMap.Value;
  ComputeIndexes(p);

  FClipArea.Create(Point(Trunc(X.Value), Trunc(Y.Value)), FWidth, FHeight);


{  // try to render with the frame buffer object
  if (FRenderToTexture <> NIL) and FRenderToTexture.Ready then
  begin
    // save the current MV matrix and reset it
    FPreviousModelViewMatrix.CopyFrom(FParentScene.ModelViewMatrix);
    FParentScene.ModelViewMatrix.LoadIdentity;
    SetBlendMode(FX_BLEND_NORMAL);

    currentTexture := NIL;
    FRenderToTexture.Bind;

    yy := FYOffsetFirstTile;
    iytile := FRowIndexFirstTile;
    repeat
      xx := FXOffsetFirstTile;
      ixtile := FColumnIndexFirstTile;

      repeat
        xxx := xx + x.Value;
        yyy := yy + y.Value;
        // draw only non clipped tile
        if not((xxx > FParentScene.Width) or (xxx+TileSize.cx < 0) or
                (yyy > FParentScene.Height) or (yyy+TileSize.cy < 0)) or
           ForceTheDrawingOfAllTheTiles then
        begin
          if (ixtile >= 0) or (ixtile < FMapTileCount.cx) or
             (iytile >= 0) or (iytile < FMapTileCount.cy) then
          begin
            ptrTile := @FMap[iytile,ixtile];
            if ptrTile^.TextureIndex > -1 then
            begin
              indexTex := ptrTile^.TextureIndex;
              indexFrame := TileEngineIndexFrameToTextureIndexFrame( indexTex, ptrTile^.ixFrame, ptrTile^.iyFrame );

              t := GetTexture(indexTex);
              if currentTexture <> t then begin
                tileRenderer.Prepare(ptTriangles, FParentScene.MVPMatrix, t, 0);
                currentTexture := t;
              end;
              //t^.Bind;

              tileRenderer.PushQuad(indexFrame, 0, xxx, yyy);
//              FParentScene.TexturedQuadRenderer.DrawTexture(t, indexFrame, 0, xxx, yyy, 0, FComputedOpacity, Tint.Value);
            end
            else FillBox( xx + x.Value, yy + y.Value, FTileSize.cx, FTileSize.cy, c );
            // callback event
            if (FOnTileEvent <> NIL) and ((ptrTile^.UserEvent <> VALUE_NO_EVENT) or TileMapDesignerModeEnable)
              then FOnTileEvent(self, PointF(xxx,yyy), ptrTile^.UserEvent);
          end;
        end;

        xx += FTileSize.cx;
        inc(ixtile);
        if FHLoopMode and (ixtile >= FMapTileCount.cx) then ixtile := 0;
      until xx >= FWidth;

      yy += FTileSize.cy;
      inc(iytile);
      if FVLoopMode and (iytile >= FMapTileCount.cy) then iytile := 0;
    until yy >= FHeight;

    tileRenderer.Draw(True);

    FRenderToTexture.Unbind;
//    FParentScene.TextMan.UnbindTexture;

    // restore the previous MV matrix and draw the rendered texture
    FParentScene.ModelViewMatrix.CopyFrom(FPreviousModelViewMatrix);
    PushAndPrepareSceneMV;
    SetBlendMode(FBlendMode);
    FParentScene.TexturedQuadRenderer.DrawTexture(FRenderToTexture.RenderedTexture, 0, FlipToIndex,
                                                  FParentScene.MVPMatrix, 0, 0, computedOpacity, Tint.Value);
    PopSceneMV;

    exit;
  end;       }










  c := MapHoleColor.Value;
  c.alpha := Round(FComputedopacity*c.alpha);

// PushAndPrepareMatrix;          <- issue with glScissor
//  SetBlendMode(FBlendMode);

  if not (TileMapDesignerModeEnable or ForceTheDrawingOfAllTheTiles) then
  begin
   glScissor(round(x.Value), round(y.Value), Width, Height);
   glEnable(GL_SCISSOR_TEST);
  end;

    yy := FYOffsetFirstTile;
    iytile := FRowIndexFirstTile;

    repeat
      xx := FXOffsetFirstTile;
      ixtile := FColumnIndexFirstTile;

      repeat
        xxx := xx + x.Value;
        yyy := yy + y.Value;
        if not((xxx > FParentScene.Width) or (xxx+TileSize.cx < 0) or
                (yyy > FParentScene.Height) or (yyy+TileSize.cy < 0)) or
           ForceTheDrawingOfAllTheTiles then
        begin
          // draw only non clipped tile
          if (ixtile < 0) or (ixtile >= FMapTileCount.cx) or
             (iytile < 0) or (iytile >= FMapTileCount.cy) then
            FillBox(FParentScene, xx + x.Value, yy + y.Value, FTileSize.cx, FTileSize.cy, c)
          else
          begin
            ptrTile := @FMap[iytile,ixtile];
            if ptrTile^.TextureIndex > -1 then
            begin
              indexTex := ptrTile^.TextureIndex;
              indexFrame := TileEngineIndexFrameToTextureIndexFrame(indexTex, ptrTile^.ixFrame, ptrTile^.iyFrame);

              t := GetTexture(indexTex);
              tileRenderer.Prepare(ptTriangles, FParentScene.MVPMatrix, t, 0, FComputedOpacity, FComputedTint, BlendMode);
              //t^.Bind;
              //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);//GL_LINEAR);
              //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);//GL_LINEAR);
              tileRenderer.PushQuad(indexFrame, 0, xxx, yyy);
//tileRenderer.Batch_Flush;
//              FParentScene.TexturedQuadRenderer.DrawTexture(t, indexFrame, 0, xxx, yyy, 0, FComputedOpacity, computedTint);
            end
            else FillBox(FParentScene, xx + x.Value, yy + y.Value, FTileSize.cx, FTileSize.cy, c );
            // callback event
            if (FOnTileEvent <> NIL) and ((ptrTile^.UserEvent <> VALUE_NO_EVENT) or TileMapDesignerModeEnable)
              then FOnTileEvent(self, PointF(xxx,yyy), ptrTile^.UserEvent);
          end;
        end;

        xx += FTileSize.cx;
        inc(ixtile);
        if FHLoopMode and (ixtile >= FMapTileCount.cx) then ixtile := 0;
      until xx >= FWidth;

      yy += FTileSize.cy;
      inc(iytile);
      if FVLoopMode and (iytile >= FMapTileCount.cy) then iytile := 0;
    until yy >= FHeight;

    glDisable(GL_SCISSOR_TEST);

  if ChildsUseParentOpacity then DrawChildSurfacesWithPositiveZOrder(FComputedOpacity)
    else DrawChildSurfacesWithPositiveZOrder(aLayerPercentOpacity);
// PopMatrix;
end;

procedure TTileEngine.DoDraw;
begin
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
var tex: PTexture;
begin
  tex := FParentScene.TexMan.Add(AFilename);
  tex^.SetFrameSize(AFrameWidth, AFrameHeight);
  Result := AddTexture(tex);

{ Result := Length(FTextureList);
 SetLength(FTextureList, Result + 1);
 FTextureList[Result].Texture := TextureManager.Add(AFilename);
 FTextureList[Result].Texture^.SetFrameSize(AFrameWidth, AFrameHeight);

 SetTileSize(AFrameWidth, AFrameHeight);

 TextureManager.BindTexture(FTextureList[Result].Texture);
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP); //GL_CLAMP_TO_EDGE );
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP); //GL_CLAMP_TO_EDGE );
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST); // nearest Filtering
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); // nearest Filtering

 // tile type to default (neutral=1)
 fc := TextureManager.GetFrameCount(FTextureList[Result].Texture);
 SetLength(FTextureList[Result].TileType, fc.cx, fc.cy);

 for ix := 0 to fc.cx-1 do
  for iy := 0 to fc.cy-1 do
   FTextureList[Result].TileType[ix][iy] := 1;  // neutral ground type by default   }
end;

function TTileEngine.AddTexture(aTexture: PTexture): integer;
var ix, iy: integer;
    fc: TSize;
begin
  Result := Length(FTextureList);
  SetLength(FTextureList, Result + 1);
  FTextureList[Result].Texture := aTexture;

  SetTileSize(aTexture^.FrameWidth, aTexture^.FrameHeight);

  aTexture^.Bind;
//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_MIRRORED_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_MIRRORED_REPEAT);
//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP); //GL_CLAMP_TO_EDGE );
//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP); //GL_CLAMP_TO_EDGE );

//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
//  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST); // nearest Filtering
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); // nearest Filtering

  // tile type to default (neutral=1)
  fc := aTexture^.GetFrameCount;
  SetLength(FTextureList[Result].TileType, fc.cx, fc.cy);

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

function TTileEngine.GetPTextureList: PArrayOfTileTexInfo;
begin
 if FPTextureList = NIL
   then Result := @FTextureList
   else Result := FPTextureList;
end;

procedure TTileEngine.SetTextureListFromAnotherTileEngine(aTileEngine: TTileEngine);
begin
 FTextureListOwner := FALSE;
 FPTextureList := aTileEngine.GetPTextureList;
end;

function TTileEngine.GetTexture(AIndex: integer): PTexture;
begin
 if FPTextureList = NIL then begin
   if (AIndex >= 0) and (AIndex<Length(FTextureList))
     then Result := FTextureList[AIndex].Texture
     else Result := NIL;
 end else begin
   if (AIndex >= 0) and (AIndex<Length(FPTextureList^))
     then Result := FPTextureList^[AIndex].Texture
     else Result := NIL;
 end;
end;
function TTileEngine.GetTextureCount: integer;
begin
 if FPTextureList = NIL
   then Result := Length(FTextureList)
   else Result := Length(FPTextureList^);
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
 Result := ( AITex >= 0 ) and ( AITex < self.GetTextureCount);
end;

function TTileEngine.IsValidFrameIndex(ATextureIndex, ixFrame, iyFrame: integer ): boolean;
var fc: TSize;
begin
 Result := IsValidTextureIndex( ATextureIndex );
 if Result then begin
   fc := GetTexture(ATextureIndex)^.GetFrameCount;
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
   else Result := iyFrame * p^.GetFrameCount.cx + ixFrame + 1;  //[ix,iy]=[0,0]=> FrameIndex := 1
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

 fc:= GetTexture(ATextureIndex)^.GetFrameCount;
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
   else begin
     if FPTextureList = NIL
       then Result := @FTextureList[p^.TextureIndex]
       else Result := @FPTextureList^[p^.TextureIndex];
   end;
end;

procedure TTileEngine.SetGroundType(ATextureIndex, ixFrame, iyFrame, AGroundType: integer);
begin
 if not IsValidFrameIndex( ATextureIndex, ixFrame, iyFrame ) then exit;
 if FPTextureList = NIL
   then FTextureList[ATextureIndex].TileType[ixFrame][iyFrame] := AGroundType
   else FPTextureList^[ATextureIndex].TileType[ixFrame][iyFrame] := AGroundType;
end;

function TTileEngine.GetGroundType(ATextureIndex, ixFrame, iyFrame: integer ): integer;
begin
 if not IsValidFrameIndex( ATextureIndex, ixFrame, iyFrame )
   then Result := 0    // value for ground 'hole'
   else begin
    if FPTextureList = NIL
      then Result := FTextureList[ATextureIndex].TileType[ixFrame][iyFrame]
      else Result := FPTextureList^[ATextureIndex].TileType[ixFrame][iyFrame];
   end;
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
  EnsurePositionInMap(Result);
end;

procedure TTileEngine.Clear;
var i: integer;
begin
  SetLength(FMap, 0);

  if (FPTextureList = NIL) and FTextureListOwner then
  begin
    for i:=0 to GetTextureCount-1 do
     FParentScene.TexMan.Delete(FTextureList[i].Texture);
  end;
  SetLength(FTextureList, 0);

  FMapSize.cx := 0;
  FMapSize.cy :=0;
  FMapTileCount.cx := 0;
  FMapTileCount.cy := 0;
// FPositionOnMap := PointF( 0, 0 );
// ScrollSpeed.Value := PointF( 0, 0 );
// MapHoleColor.Value := BGRA(0,0,0,0);
end;

procedure TTileEngine.DoLoadTextureFromStringList(t: TStringList; const aPathToTexImage: string);
var k, i, c: Integer;
  txtsplited: TStringArray;
begin
  // load map's textures
  k := t.IndexOf('TEXTURES');
  if k = -1 then raise exception.Create('Tile engine - LoadMap error : can''t retrieve texture list');
  inc(k);
  c := t.Strings[k].ToInteger;
  inc(k);
  for i:=0 to c-1 do
  begin
    txtsplited := t.Strings[k].Split(['|']);
    if Length(txtsplited) <> 3 then
      raise exception.Create('Tile engine - LoadMap error : can''t retrieve texture name and frame size');
    AddTexture(IncludeTrailingPathDelimiter(aPathToTexImage) + txtsplited[0],
               txtsplited[1].ToInteger, txtsplited[2].ToInteger);
    inc(k);
  end;
end;

procedure TTileEngine.DoLoadFramesTypeFromStringList(t: TStringList);
var i, k, z, c, ix, iy : integer;
    txtsplited: TStringArray;
begin
  k := t.IndexOf('FRAMES_TYPE');
  if k = -1 then exit;
  inc(k);
  c := t.Strings[k].ToInteger;  // texture count
  inc(k);
  for i:=0 to c-1 do
  begin
    z := 0;
    txtsplited := t.Strings[k].Split(['|']);
    for iy:=0 to txtsplited[1].ToInteger-1 do
     for ix:=0 to txtsplited[0].ToInteger-1 do
     begin
       SetGroundType(i, ix, iy, txtsplited[2+z].ToInteger);
       inc(z);
     end;
    inc (k);
  end;
end;

procedure TTileEngine.DoLoadDrawSizeFromStringList(t: TStringList);
var k: Integer;
  txtsplited: TStringArray;
begin
  k := t.IndexOf('DRAW_SIZE');
  if k = -1 then exit;
  inc(k);
  txtsplited := t.Strings[k].Split(['|']);
  if Length( txtsplited ) <> 2 then
    raise exception.Create('Tile engine - LoadMap error : can''t retrieve draw size');
  SetViewSize(txtsplited[0].ToInteger, txtsplited[1].ToInteger);
  if FParentScene<>NIL then
  begin // center on scene by default
    x.Value := (FParentScene.Width - FWidth) * 0.5;
    y.Value := (FParentScene.Height - FHeight) * 0.5;
  end;
end;

procedure TTileEngine.DoLoadTileSizeFromStringList(t: TStringList);
var k: Integer;
  txtsplited: TStringArray;
begin
  k := t.IndexOf('TILE_SIZE');
  if k = -1 then
    raise exception.Create('Tile engine - LoadMap error : can''t retrieve tile size');
  inc(k);
  txtsplited := t.Strings[k].Split(['|']);
  if Length(txtsplited) <> 2 then
    raise exception.Create('Tile engine - LoadMap error : can''t retrieve tile size');
  SetTileSize(txtsplited[0].ToInteger, txtsplited[1].ToInteger);
end;

procedure TTileEngine.DoLoadMapSizeFromStringList(t: TStringList);
var k: Integer;
  txtsplited: TStringArray;
begin
  k := t.IndexOf('MAP_SIZE');
  if k = -1 then
    raise exception.Create('Tile engine - LoadMap error : can''t retrieve map tile count');
  inc(k);
  txtsplited := t.Strings[k].Split(['|']);
  if Length(txtsplited) <> 2 then
    raise exception.Create('Tile engine - LoadMap error : can''t retrieve map size');
  SetMapTileCount(txtsplited[0].ToInteger, txtsplited[1].ToInteger);
end;

procedure TTileEngine.DoLoadScrollInfoFromStringList(t: TStringList);
var k: Integer;
  txt: String;
begin
  FHScrollEnable := FALSE;
  FVScrollEnable := FALSE;
  k := t.IndexOf('SCROLL_ENABLE');
  if k <> -1 then
  begin
    txt := UpperCase(t.Strings[k+1]);
    FHScrollEnable := txt.Contains('H');
    FVScrollEnable := txt.Contains('V');
  end;

  FHLoopMode := FALSE;
  FVLoopMode := FALSE;
  k := t.IndexOf('SCROLL_LOOP_MODE');
  if k <> -1 then
  begin
    txt := UpperCase(t.Strings[k+1]);
    FHLoopMode := txt.Contains('H');
    FVLoopMode := txt.Contains('V');
  end;
end;

procedure TTileEngine.DoLoadStartTileFromStringList(t: TStringList);
var k: Integer;
  txtsplited: TStringArray;
begin
  PositionOnMap.Value := PointF( 0, 0);
  k := t.IndexOf('START_TILE');
  if k <> -1 then
  begin
    txtsplited := t.Strings[k+1].Split(['|']);
    if Length(txtsplited) = 2 then
    begin
      PositionOnMap.x.Value := txtsplited[0].ToInteger * FTileSize.cx;
      PositionOnMap.y.Value := txtsplited[1].ToInteger * FTileSize.cy;
    end;
  end;
end;

procedure TTileEngine.DoLoadHoleColorFromStringList(t: TStringList);
var k: Integer;
begin
  k := t.IndexOf('HOLE_COLOR');
  if k <> -1 then MapHoleColor.Value := HexToBGRAPixel(t.Strings[k+1])
  else MapHoleColor.Value := BGRA(0,0,0);
end;

procedure TTileEngine.DoLoadMapDataFromStringList(t: TStringList);
var k, i, j, z: Integer;
  temp, txtsplited: TStringArray;
begin
  k := t.IndexOf('DATA');
  if k = -1 then raise exception.Create('Tile engine - LoadMap error : can''t retrieve map data');
  inc(k);

  for i:=0 to FMapTileCount.cy-1 do
  begin
    j := 0;
    temp := t.Strings[k].Split([' ']);
    for z:=0 to High(temp) do
    begin
      txtsplited := temp[z].Split([',']);
      case txtsplited[0][1] of
       'T' : begin
              SetCell( i, j, copy(txtsplited[0], 2, length( txtsplited[0] ) - 1 ).ToInteger,
                             txtsplited[1].ToInteger,
                             txtsplited[2].ToInteger);
              SetUserEventValue(i, j, txtsplited[3].ToInteger);
              inc(j);
             end;
      end;//case
    end;
    inc(k);
   end;
end;

procedure TTileEngine.SetViewSize(AWidth, AHeight: integer);
begin
  FWidth := Max(1, AWidth);
  FHeight := Max(1, AHeight);

  if FRenderToTexture <> NIL then FreeAndNil(FRenderToTexture);
  //FRenderToTexture := TOGLCRenderToTexture.Create(FParentScene, FWidth, FHeight, 1);
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
  if (TileSize.cx = 0) or (TileSize.cy = 0) then exit;
  if not SceneCoorIsInMap(aSceneCoor, @sc) then exit;

  co := FColumnIndexFirstTile + trunc((sc.x-FXOffsetFirstTile)/FTileSize.cx);
  ro := FRowIndexFirstTile + trunc((sc.y-FYOffsetFirstTile)/FTileSize.cy);

  p := GetPTile(ro, co);
  if p <> NIL then Result := GetGroundType(p^.TextureIndex, p^.ixFrame, p^.iyFrame);
end;

function TTileEngine.GetUserEventValue(aSceneCoor: TPointF): integer;
var p: PTile;
    sc: TPointF;
    ro, co: integer;
begin
  Result := VALUE_NO_EVENT;
  if (TileSize.cx = 0) or (TileSize.cy = 0) then exit;
  if not SceneCoorIsInMap(aSceneCoor, @sc) then exit;

  co := FColumnIndexFirstTile + trunc((sc.x-FXOffsetFirstTile)/FTileSize.cx);
  ro := FRowIndexFirstTile + trunc((sc.y-FYOffsetFirstTile)/FTileSize.cy);

  p := GetPTile(ro, co);
  if p <> NIL then Result := p^.UserEvent;
end;

function TTileEngine.SceneCoorIsInMap(aSceneCoor: TPointF;  aPSurfaceCoor: PPointF): boolean;
var f1, f2: boolean;
    P: TPointF;
begin
  P := SceneToSurface(aSceneCoor);
  f1 := (P.x >= 0) and (P.x < FMapSize.cx);
  f1 := f1 OR FHLoopMode;

  f2 := (P.y >= 0) and (P.y < FMapSize.cy);
  f2 := f2 OR FVLoopMode;

  Result := f1 and f2;

  if aPSurfaceCoor <> NIL then aPSurfaceCoor^ := p;
end;

function TTileEngine.GetTileSize: TSize;
begin
  Result := FTileSize;
end;

function TTileEngine.GetTextureList: TArrayOfTileTexInfo;
begin
  if FPTextureList = NIL
    then Result := FTextureList
    else Result := FPTextureList^;
end;

function TTileEngine.GetMapSize: TSize;
begin
  Result := FMapSize;
end;

procedure TTileEngine.LoadMapFile(const aFilename: string; aTileEngineToCopy: TTileEngine);
var t: TStringList;
begin
  Clear;
  t := TStringList.Create;
  t.LoadFromFile(aFilename);

  if aTileEngineToCopy = NIL then
  begin
    DoLoadTextureFromStringList(t, ExtractFilePath(aFileName));
    FTextureListOwner := TRUE;
    FPTextureList := NIL;
  end else begin
    FTextureListOwner := FALSE;
    FPTextureList := aTileEngineToCopy.GetPTextureList;
  end;

  // Frame Type
  if aTileEngineToCopy = NIL then DoLoadFramesTypeFromStringList(t);
  // Draw Size
  DoLoadDrawSizeFromStringList(t);
  // Tile Size
  if aTileEngineToCopy = NIL then DoLoadTileSizeFromStringList(t)
   else SetTileSize(aTileEngineToCopy.TileSize.cx, aTileEngineToCopy.TileSize.cy);
  // Map size
  if aTileEngineToCopy = NIL then DoLoadMapSizeFromStringList(t)
   else SetMapTileCount(aTileEngineToCopy.MapTileCount.cy, aTileEngineToCopy.MapTileCount.cx);
  // Scroll info
  DoLoadScrollInfoFromStringList(t);
  // Start tile
  DoLoadStartTileFromStringList(t);
  // hole color
  if aTileEngineToCopy = NIL then DoLoadHoleColorFromStringList(t)
   else MapHoleColor.Value := aTileEngineToCopy.MapHoleColor.Value;
  // Build map from Data
  DoLoadMapDataFromStringList(t);

  t.Free;
  ScrollSpeed.Value := PointF(0, 0);
end;

procedure TTileEngine.LoadMapFile(const aFilename: string; const aTextures: array of PTexture);
var t: TStringList;
    i: integer;
begin
  Clear;
  t := TStringList.Create;
  t.LoadFromFile(aFilename);

  if Length(aTextures) = 0 then
    raise exception.Create('TileEngine: texture array is empty');
  for i:=0 to High(aTextures) do
    AddTexture(aTextures[i]);
  FTextureListOwner := False;
  FPTextureList := NIL;

  // Frame Type
  DoLoadFramesTypeFromStringList(t);
  // Draw Size
  DoLoadDrawSizeFromStringList(t);
  // Tile Size
  DoLoadTileSizeFromStringList(t);
  // Map size
  DoLoadMapSizeFromStringList(t);
  // Scroll info
  DoLoadScrollInfoFromStringList(t);
  // Start tile
  DoLoadStartTileFromStringList(t);
  // hole color
  DoLoadHoleColorFromStringList(t);
  // Build map from Data
  DoLoadMapDataFromStringList(t);
  t.Free;
  ScrollSpeed.Value := PointF(0, 0);
end;

{$endif oglcIMPLEMENTATION}

