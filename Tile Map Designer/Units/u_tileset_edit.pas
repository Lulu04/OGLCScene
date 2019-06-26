unit u_tileset_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, VelocityCurve,
  common;

type

TCellPattern= record
  tilesetIndex: integer;    // index of the tileset (texture)
  TileIndex: TPoint;        // row(cy), column(cx) of the tile in the texture
  Pos: TPoint;              // TopLeft screen position of the tile
end;
PCellPattern=^TCellPattern;

{ TTiledPattern }

TTiledPattern=class    // container for TCellPattern
private
  FList: TList;
  function GetCount: integer;
  function GetTileCellByIndex( index: integer): PCellPattern;
  procedure Delete( index: integer);
  function IndexOfByClientPos( aClientPos: TPoint ): integer;
  function CellPatternToString( aCell: PCellPattern ): string;
  procedure StringToCellPattern(aCell: PCellPattern; const s: string );
public
  constructor Create;
  destructor Destroy; override;

  function SaveToString: string;
  procedure LoadFromString( const s: string );

  procedure DrawBoxAroundTiles;

  procedure Clear;
  procedure AddOrRemoveTileUnderMouse;
  procedure AddOrRemoveTile( aTilesetIndex: integer; aTileIndex: TPoint; aScreenPos: TPoint); // tile is removed if it is already selected.

  procedure PutOn( aTileEngine: TTileEngine; aPos: TPoint ); // copy the pattern on the specified TileEngine and position (in tile)

  property TileCount: integer read GetCount;
end;


{ TPatternList }

TPatternList=class
private
  FList: TList;
  FWorkingPattern: TTiledPattern;
  FIsActive: boolean;
  FTileEngine: TTileEngine;
  function GetCount: integer;
  function GetPatternByIndex( index: integer): TTiledPattern;
  procedure Delete(index: integer);
  procedure FillTileEngineWithPatterns;
public
  constructor Create;
  destructor Destroy; override;

  procedure LoadFrom( aTemp: TStringList );
  procedure SaveTo( aTemp: TStringList );

  procedure EnterModeEditPattern;
  procedure ExitModeEditPattern;

  procedure Clear;              // clear all pattern list
  procedure AddWorkingPattern;  // create a new pattern with the current selected tiles
  procedure Add( aPattern: TTiledPattern ); // add a pattern to the list

  procedure SetTileEngineCoordinates( aX, aY: single );

  property Count: integer read GetCount;
  property WorkingPattern: TTiledPattern read FWorkingPattern;
  property IsActive: boolean read FIsActive;
  property TileEngine: TTileEngine read FTileEngine;
end;


{ TTilesetEdit }

TTilesetEdit=class
private
  FIsActive: boolean;
  FTileEngine: TTileEngine;
private
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
  property TileEngine: TTileEngine read FTileEngine;
end;

function WorkingPattern:TTiledPattern;


var TilesetEdit: TTilesetEdit;
    PatternList: TPatternList;


function ViewIsOnLayer: boolean;
procedure SetViewOnLayer;
procedure SetViewOnTileSetEdit;
procedure SetViewOnPatternList;

implementation
uses umaps, u_main, Controls,gl;

function WorkingPattern: TTiledPattern;
begin
 Result := PatternList.WorkingPattern;
end;

function ViewIsOnLayer: boolean;
begin
 Result := not( TilesetEdit.IsActive or PatternList.IsActive);
end;

procedure SetViewOnLayer;
begin
 if TilesetEdit.IsActive then TilesetEdit.ExitModeEditTileset;
 if PatternList.IsActive then PatternList.ExitModeEditPattern;
 FWorkingTileEngine := MapList.SelectedTileEngine;
end;

procedure SetViewOnTileSetEdit;
begin
 SetViewOnLayer;
 TilesetEdit.EnterModeEditTileset;
// FWorkingTileEngine := TilesetEdit.TileEngine;
end;

procedure SetViewOnPatternList;
begin
 SetViewOnLayer;
 PatternList.EnterModeEditPattern;
// FWorkingTileEngine := PatternList.TileEngine;
end;

{ TPatternList }

function TPatternList.GetCount: integer;
begin
 Result:=FList.Count;
end;

function TPatternList.GetPatternByIndex(index: integer): TTiledPattern;
begin
 Result := TTiledPattern(FList.Items[index]);
end;

procedure TPatternList.Delete(index: integer);
begin
 TTiledPattern(FList.Items[index]).Free;
 FList.Delete(index);
end;

procedure TPatternList.FillTileEngineWithPatterns;
var o: TGuiLabel;
    p: TPoint;
begin
 if Count=0 then begin
   o:= FScene.Add_GuiLabel('Pattern''s list is empty', FTitleFont, NIL, Layer_Pattern);
   o.CenterOnScene;
   exit;
 end;

 FTileEngine.SetTextureListFromAnotherTileEngine( MapList.MainMap.TileEngine );
 FTileEngine.SetTileSize( MapList.MainMap.TileEngine.TileSize.cx, MapList.MainMap.TileEngine.TileSize.cy);
 FTileEngine.SetMapTileCount( 30,30);
 GetPatternByIndex(0).PutOn( FTileEngine, Point(0,0));
end;

constructor TPatternList.Create;
begin
 FTileEngine:= TTileEngine.Create;
 FScene.Add( FTileEngine, Layer_Pattern );
// FTileEngine.SetTextureListFromAnotherTileEngine( MapList.MainMap.TileEngine );
 FTileEngine.ForceTheDrawingOfAllTheTiles:=TRUE;
 FTileEngine.MapHoleColor.Value := BGRA(0,0,0,0);

 FList:= TList.Create;
 FWorkingPattern:= TTiledPattern.Create;
end;

destructor TPatternList.Destroy;
begin
 Clear;
 FWorkingPattern.Free;
 FList.Free;
 inherited Destroy;
end;

procedure TPatternList.LoadFrom(aTemp: TStringList);
var o: TTiledPattern;
    i,k,c: integer;
begin
 Clear;
 k := aTemp.IndexOf('[PATTERN]');
 if k=-1 then exit;
 inc(k);
 c := strtoint(aTemp.Strings[k]);
 for i:=0 to c-1 do begin
   inc(k);
   o:= TTiledPattern.Create;
   o.LoadFromString(aTemp.Strings[k]);
   Add(o);
 end;
end;

procedure TPatternList.SaveTo(aTemp: TStringList);
var i: integer;
begin
 aTemp.Add('[PATTERN]');
 aTemp.Add(inttostr(Count));
 for i:=0 to Count-1 do aTemp.Add( GetPatternByIndex(i).SaveToString );
end;

procedure TPatternList.EnterModeEditPattern;
begin
 SetViewOnLayer;

// FScene.Layer[Layer_Pattern].KillAll;
 OnlyThisLayerVisible( Layer_Pattern );

  FIsActive:=TRUE;
  FWorkingTileEngine:=FTileEngine;
  FillTileEngineWithPatterns;
end;

procedure TPatternList.ExitModeEditPattern;
begin
// FScene.Layer[Layer_Pattern].KillAll;

 FScene.Layer[Layer_WorkMap].Visible:=TRUE;
 FScene.Layer[Layer_Pattern].Visible:=FALSE;

 FIsActive:=FALSE;
end;

procedure TPatternList.Clear;
begin
 while FList.Count>0 do Delete(0);
end;

procedure TPatternList.AddWorkingPattern;
begin
 if Self.WorkingPattern.GetCount>0 then Add( Self.WorkingPattern );
end;

procedure TPatternList.Add(aPattern: TTiledPattern);
begin
 FList.Add( aPattern );
end;

procedure TPatternList.SetTileEngineCoordinates(aX, aY: single);
begin
 FTileEngine.SetCoordinate( aX, aY );
end;

{ TTiledPattern }

function TTiledPattern.GetTileCellByIndex(index: integer): PCellPattern;
begin
 Result := PCellPattern(FList.Items[index]);
end;

function TTiledPattern.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TTiledPattern.Delete(index: integer);
begin
 Dispose(PCellPattern(FList.Items[index]));
 FList.Delete(index);
end;

function TTiledPattern.IndexOfByClientPos(aClientPos: TPoint): integer;
var i: integer;
    o: PCellPattern;
begin
 Result :=-1;
 for i:=0 to FList.Count-1 do begin
  o:=GetTileCellByIndex(i);
  if o^.Pos=aClientPos then begin
    Result:=i;
    exit;
  end;
 end;
end;

function TTiledPattern.CellPatternToString(aCell: PCellPattern): string;
begin
 with aCell^ do begin
  Result := inttostr(tilesetIndex)+';'+
            inttostr(TileIndex.x)+';'+inttostr(TileIndex.y)+';'+
            inttostr(Pos.x)+';'+inttostr(Pos.y);
 end;
end;

procedure TTiledPattern.StringToCellPattern(aCell: PCellPattern; const s: string );
var SplittedText: ArrayOfString;
begin
 SplittedText:= SplitLineToStringArray(s,';');
 with aCell^ do begin
   tilesetIndex := strtoint(SplittedText[0]);
   TileIndex.x := strtoint(SplittedText[1]);
   TileIndex.y := strtoint(SplittedText[2]);
   Pos.x := strtoint(SplittedText[3]);
   Pos.y := strtoint(SplittedText[4]);
 end;
end;

constructor TTiledPattern.Create;
begin
 FList:= TList.Create;
end;

destructor TTiledPattern.Destroy;
begin
 Clear;
 FList.Free;
 inherited Destroy;
end;



function TTiledPattern.SaveToString: string;
var o: PCellPattern;
    i: integer;
begin
 Result:=inttostr(GetCount);
 for i:=0 to GetCount-1 do begin
  o := GetTileCellByIndex(i);
  Result+='|'+CellPatternToString( o );
 end;
end;

procedure TTiledPattern.LoadFromString(const s: string);
var o: PCellPattern;
    i,k: integer;
    SplittedText: ArrayOfString;
begin
 Clear;
 SplittedText:=SplitLineToStringArray(s,'|');
 k:=1;
 for i:=0 to strtoint(SplittedText[0])-1 do begin
  New(o);
  StringToCellPattern( o, SplittedText[k]);
  FList.Add(o);
  inc(k);
 end;

end;

procedure TTiledPattern.DrawBoxAroundTiles;
var i, w, h: integer;
    p:TPoint;
begin
 if GetCount=0 then exit;

 w := MapList.MainMap.TileEngine.TileSize.cx;
 h := MapList.MainMap.TileEngine.TileSize.cy;

// DrawBox(pOrigin.x, pOrigin.y, w, h, BGRA(255,255,0), 1.5 );

 glEnable(GL_COLOR_LOGIC_OP);
 glLogicOp(GL_INVERT);
 for i:=0 to GetCount-1 do begin
  p:= GetTileCellByIndex(i)^.Pos;
  DrawBox(p.x, p.y, w, h, BGRA(255,255,255,0), BGRA(255,255,255,100), 1.5 );
 end;
 glDisable(GL_COLOR_LOGIC_OP);
end;


procedure TTiledPattern.Clear;
begin
 while FList.Count>0 do Delete(0);
end;

procedure TTiledPattern.AddOrRemoveTileUnderMouse;
var FMousePos: TPoint;
    p1: TPoint;
    p: PTile;
begin
 FMousePos := Form_Main.OpenGlControl1.ScreenToClient( Mouse.CursorPos );
 if not Form_Main.XYCoorIsInMap( FMousePos.x, FMousePos.y ) then exit;

 FMousePos := Form_Main.ClientCoorToTileTopLeftCoor( FMousePos );

 // take the info of the target tile
 p1 := Form_Main.ClientPosToTileIndex( FMousePos );
 p := FWorkingTileEngine.GetPTile( p1.y, p1.x);

 AddOrRemoveTile( p^.TextureIndex, Point(p^.ixFrame,p^.iyFrame), FMousePos );
end;

procedure TTiledPattern.AddOrRemoveTile(aTilesetIndex: integer; aTileIndex: TPoint; aScreenPos: TPoint);
var o: PCellPattern;
    i: integer;
begin
 i := IndexOfByClientPos( aScreenPos );
 if i=-1 then begin  // add to the list
   New(o);
   o^.tilesetIndex:=aTilesetIndex;
   o^.TileIndex:=aTileIndex;
  // if FList.Count=0
   {  then} o^.Pos:=aScreenPos;
    // else o^.Pos:=aScreenPos-GetTileCellByIndex(0)^.Pos;
   FList.Add(o);
 end else Delete(i); // remove the tile from the list
end;

procedure TTiledPattern.PutOn(aTileEngine: TTileEngine; aPos: TPoint);
var i: integer;
    p, posCell0: TPoint;
    o: PCellPattern;
begin
 if GetCount=0 then exit;

 posCell0:=GetTileCellByIndex(0)^.Pos;

 for i:=0 to FList.Count-1 do begin
  o:=GetTileCellByIndex(i);
  p:= o^.Pos - posCell0 + aPos;
  aTileEngine.SetCell(p.y, p.x, o^.tilesetIndex, o^.TileIndex.x, o^.TileIndex.y);
 end;
end;

{ TTilesetEdit }

procedure TTilesetEdit.FillTileEngineWithCurrentTileSet;
var t: PTexture;
    xTileCount, yTileCount: integer;
    ro, co: integer;
begin
 // link to the main map
 FTileEngine.SetTextureListFromAnotherTileEngine( MapList.MainMap.TileEngine );

 // set MapSize of the tileengine to fit the current tileset
 t := MapList.MainMap.TileEngine.GetTexture( FEditTilesetIndex );
 xTileCount:= t^.TextureWidth div t^.FrameWidth;
 yTileCount:= t^.TextureHeight div t^.FrameHeight;

 FTileEngine.SetTileSize( MapList.MainMap.TileEngine.TileSize.cx, MapList.MainMap.TileEngine.TileSize.cy);
 FTileEngine.SetMapTileCount( yTileCount, xTileCount);
 FTileEngine.SetViewSize(FTileEngine.TileSize.cx*xTileCount, FTileEngine.TileSize.cy*yTileCount);

 for ro:=0 to yTileCount-1 do
  for co:=0 to xTileCount-1 do
   begin
    FTileEngine.SetCell( ro, co, FEditTilesetIndex, co, ro );
    FTileEngine.SetUserEventValue( ro, co, -1 );   // no user event
   end;
end;

constructor TTilesetEdit.Create;
begin
 FTileEngine:= TTileEngine.Create;
 FScene.Add( FTileEngine, Layer_WorkTileSet );
 FTileEngine.SetTextureListFromAnotherTileEngine( MapList.MainMap.TileEngine );
 FTileEngine.ForceTheDrawingOfAllTheTiles:=TRUE;
 FTileEngine.MapHoleColor.Value := BGRA(0,0,0,0);
end;

destructor TTilesetEdit.Destroy;
begin
 inherited Destroy;
end;

procedure TTilesetEdit.EnterModeEditTileset;
begin
 if MapList.MainMap.TileEngine.GetTextureCount=0 then exit;
 SetViewOnLayer;
 FScene.Layer[Layer_WorkMap].Visible:=FALSE;
 FScene.Layer[Layer_WorkTileSet].Visible:=TRUE;

 FillTileEngineWithCurrentTileSet;
 FIsActive:=TRUE;
 FWorkingTileEngine:=FTileEngine;
end;

procedure TTilesetEdit.ExitModeEditTileset;
begin
 FScene.Layer[Layer_WorkMap].Visible:=TRUE;
 FScene.Layer[Layer_WorkTileSet].Visible:=FALSE;
 FIsActive:=FALSE;
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
 FTileEngine.SetCoordinate( aX, aY );
end;

end.

