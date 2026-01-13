unit umaps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Controls, StdCtrls,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  common;

type

{ TMapInfo }

TMapInfo=class
  LayerName: string;
  TileEngine: TTileEngine;
  IsMainMap: boolean;
  IsDeletable: boolean;
  constructor Create;
  destructor Destroy; override;
end;

{ TMapList }

TMapList=class
private
  FMapList: TList;
  FMainMap: TMapInfo;
  function GetCount: integer;
  function GetMainMap: TMapInfo;
  function GetMapByIndex(aIndex: integer):TMapInfo;
  function GetSelectedLayerName: string;
  function GetSelectedMap: TMapInfo;
  function GetSelectedTileEngine: TTileEngine;
  function GetTileEngine(index: integer): TTileEngine;
  function NoMapSelected: boolean;
  procedure RemoveTileEngineFromSceneLayer;
  procedure PutTileEngineToSceneLayer;
  procedure ClearList;
  procedure SetAsMainMap(aMapInfo: TMapInfo);
  procedure SetAsNormalMap(aMapInfo: TMapInfo);
public
  constructor Create;
  destructor Destroy; override;

  procedure SaveSession;
  procedure LoadSession;

  procedure NewMap;
  procedure DeleteMap;
  procedure RenameMap;

  procedure SetSize(aRow, aColumns: integer);
  procedure SetTileEngineCoordinates(aX, aY: single);
  procedure MoveTileEngineTo(aX, aY: single);
  procedure ResetMaps; // clear all maps, keep all layers.
  procedure InsertRow(aRowIndex, aCount: integer);
  procedure InsertColumn(aColumnIndex, aCount: integer);

  procedure ShiftMapUp;
  procedure ShiftMapDown;
  procedure SetWorkingTileEngine;

  property MainMap: TMapInfo read GetMainMap;
  property Count: integer read GetCount;
  property TileEngine[index:integer]: TTileEngine read GetTileEngine;
  property SelectedTileEngine: TTileEngine read GetSelectedTileEngine;
  property SelectedLayerName: string read GetSelectedLayerName;
end;


var MapList: TMapList;

implementation
uses form_tools,
  form_asknewlayermapinfo,
  form_main,
  tileset_manager,
  usavemap, form_askrenamemap,
  u_tileset_edit, Math;

{ TMapInfo }

constructor TMapInfo.Create;
begin
  TileEngine := TTileEngine.Create(FScene);
  //TileEngine.TileMapDesignerModeEnable := TRUE;
  TileEngine.OnTileEvent := @FormMain.ProcessTileEngineEvent;
  TileEngine.SetViewSize(FScene.Width, FScene.Height);
end;

destructor TMapInfo.Destroy;
begin
  TileEngine.Kill;
  inherited Destroy;
end;

{ TMapList }

function TMapList.GetMainMap: TMapInfo;
begin
  Result := FMainMap;
end;

function TMapList.GetCount: integer;
begin
  Result:=FMapList.Count;
end;

function TMapList.GetMapByIndex(aIndex: integer): TMapInfo;
begin
  Result := TMapInfo(FMapList.Items[aIndex]);
end;

function TMapList.GetSelectedLayerName: string;
begin
  if FormTools.CLBLayer.ItemIndex=-1
    then Result := ''
    else Result := GetMapByIndex(FormTools.CLBLayer.ItemIndex).LayerName;
end;

function TMapList.GetSelectedMap: TMapInfo;
begin
  if FormTools.CLBLayer.ItemIndex=-1
    then Result := NIL
    else Result := GetMapByIndex(FormTools.CLBLayer.ItemIndex);
end;

function TMapList.GetSelectedTileEngine: TTileEngine;
begin
  if GetSelectedMap<>NIL
    then Result := GetSelectedMap.TileEngine
    else Result:=NIL;
end;

function TMapList.GetTileEngine(index: integer): TTileEngine;
begin
  Result := GetMapByIndex(index).TileEngine;
end;

function TMapList.NoMapSelected: boolean;
begin
  Result := FormTools.CLBLayer.ItemIndex=-1;
end;

procedure TMapList.RemoveTileEngineFromSceneLayer;
var i: integer;
begin
  for i:=0 to FMapList.Count-1 do
   FScene.Layer[Layer_WorkMap].Remove( GetMapByIndex(i).TileEngine );
end;

procedure TMapList.PutTileEngineToSceneLayer;
var i: integer;
begin
  for i:=0 to FMapList.Count-1 do
   FScene.Add( GetMapByIndex(i).TileEngine, Layer_WorkMap );
end;

procedure TMapList.ClearList;
begin
  while FMapList.Count<>0 do begin
    GetMapByIndex(0).Free;
    FMapList.Delete(0);
  end;
  TileSetManager.Clear;
end;

procedure TMapList.SetAsMainMap(aMapInfo: TMapInfo);
begin
  aMapInfo.IsDeletable:=FALSE;
  aMapInfo.IsMainMap:=TRUE;
  aMapInfo.TileEngine.TexturesOwner:=TRUE;
  FMainMap := aMapInfo;
end;

procedure TMapList.SetAsNormalMap(aMapInfo: TMapInfo);
begin
  aMapInfo.IsMainMap:=FALSE;
  aMapInfo.IsDeletable:=TRUE;
  aMapInfo.TileEngine.TexturesOwner:=FALSE;
end;

constructor TMapList.Create;
begin
  FMapList := TList.Create;

  // create main map by default
  FMainMap:= TMapInfo.Create;
  FMainMap.LayerName:='Main_Map';
  FMapList.Add( FMainMap );
  SetAsMainMap( FMainMap );
  PutTileEngineToSceneLayer;
  FWorkingTileEngine:=FMainMap.TileEngine;

  FormTools.CLBLayer.Items.Add( FMainMap.LayerName );
  FormTools.CLBLayer.Checked[0]:=TRUE;
// Form_Tools.CLBLayer.ItemIndex:=0;

end;

destructor TMapList.Destroy;
begin
  ClearList;
  FreeAndNil(FMapList);
  inherited Destroy;
end;

procedure TMapList.SaveSession;
var temp: TStringList;
  i: integer;
  map_path: string;
begin
  if not FormTools.SD2.Execute then exit;

  // save all the maps individualy in the file: layer_name.map
  map_path := ExtractFilePath( FormTools.SD2.FileName );
  FormMain.SetWindowsTitle(ExtractFileName(FormTools.SD2.FileName));

  for i:=0 to Count-1 do with GetMapByIndex(i) do
    DoSave( TileEngine, map_path+LayerName+'.map');

  // and construct the session list and save it with the name of the main_layer_name.mapsession
  temp:= TStringList.Create;
  temp.Add('[MAP_SESSION]');
  temp.Add(inttostr(FMapList.Count)); // map count
  for i:=0 to FMapList.Count-1 do begin
   temp.Add(GetMapByIndex(i).LayerName+'|'+ BoolToStr(GetMapByIndex(i).IsMainMap,'MAIN',' ')+
            '|'+BoolToStr(GetMapByIndex(i).TileEngine.Visible,'VISIBLE','NOTVISIBLE')); // name|MAIN or ' '|VISIBLE or NOTVISIBLE
  end;

  temp.Add('[ACTIVE_MAP]');
  temp.Add(inttostr(FormTools.CLBLayer.ItemIndex));

  PatternList.SaveTo( temp );

  temp.SaveToFile( FormTools.SD2.FileName );

  temp.Free;
  FProjectIsModified:=FALSE;
end;

procedure TMapList.LoadSession;
var temp: TStringList;
  k, c, i: integer;
  m: TMapInfo;
  map_path: string;
  splittedText: TStringArray;

begin
  if not FormTools.OD3.Execute then exit;
  FormTools.SD2.FileName:=FormTools.OD3.FileName;
  FormMain.SetWindowsTitle(ExtractFileName(FormTools.OD3.FileName));

  temp:= TStringList.Create;
  temp.LoadFromFile( FormTools.OD3.FileName );

  k:=temp.IndexOf('[MAP_SESSION]');
  if k=-1 then begin
    raise Exception.Create('File is not a map session...');
    temp.Free;
    exit;
  end;
  inc(k);
  c:=strtoint(temp.Strings[k]);

  ClearList;
  FormTools.CLBLayer.Clear;

  // fill the list without loading any map data, because FMainMap isn't yet initialized
  while c>0 do begin
   inc(k);
   splittedText := temp.Strings[k].Split(['|']);   // name|MAIN|VISIBLE
   m:=TMapInfo.Create;
   m.LayerName:=splittedText[0];

   if splittedText[1]='MAIN'
     then SetAsMainMap( m )
     else SetAsNormalMap( m );

   FMapList.Add( m );

   i:=FormTools.CLBLayer.Items.Add( m.LayerName );
   FormTools.CLBLayer.Checked[i] := splittedText[2]='VISIBLE';

   dec(c);
  end;

  k := temp.IndexOf('[ACTIVE_MAP]');
  if k <> -1 then begin
    inc(k);
    FormTools.CLBLayer.ItemIndex:=strtoint(temp.Strings[k]);
    FormMain.Label3.Caption:=SelectedLayerName;
  end;
  PatternList.LoadFrom( temp );

  temp.Free;


  // prepare path to map data
  map_path:= ExtractFilePath(FormTools.OD3.FileName);
  // load the main map data
  FMainMap.TileEngine.TexturesOwner:=TRUE;
  FMainMap.TileEngine.LoadMapFile( map_path+FMainMap.LayerName+'.map' );
  //with FMainMap.TileEngine do SetViewSize( TileSize.cx * MapTileCount.cx, TileSize.cy * MapTileCount.cy ); // set view to whole map
  FMainMap.TileEngine.SetViewSize(FScene.Width, FScene.Height);
  FMainMap.TileEngine.PositionOnMap.Value := PointF(0, 0);
  FMainMap.TileEngine.SetCoordinate(0, 0);

  // load tilesset from main map data
  TileSetManager.LoadTileSetFromMapFile( map_path+FMainMap.LayerName+'.map' );
  // update tileset's list
  FormTools.CB1.Clear;
  for i:=0 to TileSetManager.Count-1 do FormTools.CB1.Items.Add( TileSetManager.TileSet[i].Name );
  if FormTools.CB1.Items.Count > 0 then FormTools.CB1.ItemIndex := 0;
  // load all other maps
  for i:=0 to FMapList.Count-1 do
   if GetMapByIndex(i)<>FMainMap then
     with GetMapByIndex(i) do begin
      TileEngine.LoadMapFile(map_path+LayerName+'.map', FMainMap.TileEngine);
      // set tileengine view to fit whole map
      //TileEngine.SetViewSize(TileEngine. TileSize.cx * TileEngine.MapTileCount.cx, TileEngine.TileSize.cy * TileEngine.MapTileCount.cy );
      TileEngine.SetViewSize(FScene.Width, FScene.Height);
      TileEngine.PositionOnMap.Value := PointF(0, 0);
      TileEngine.SetCoordinate(0, 0);
     end;

  FormTools.PB1SetSizeAndPos;
  FormTools.UpdateMapParameterOnScreen;

  PutTileEngineToSceneLayer;

  FormTools.CLBLayer.ItemIndex := 0;
  SetWorkingTileEngine;;

  FProjectIsModified := FALSE;
end;

procedure TMapList.NewMap;
var o: TMapInfo;
begin
  if FormAskNewLayerMapInfo.ShowModal = mrOk then
  begin
   RemoveTileEngineFromSceneLayer;

   o:= TMapInfo.Create;
   o.LayerName := FormAskNewLayerMapInfo.Edit1.Text;

   if FMapList.Count = 0 then SetAsMainMap(o)
   else begin
     SetAsNormalMap(o);

     o.TileEngine.SetTextureListFromAnotherTileEngine( FMainMap.TileEngine ); // link the texture list to the main map
     o.TileEngine.SetViewSize(FMainMap.TileEngine.Width, FMainMap.TileEngine.Height);
     o.TileEngine.SetTileSize(FMainMap.TileEngine.TileSize.cx,FMainMap.TileEngine.TileSize.cy);
     o.TileEngine.SetMapTileCount( FMainMap.TileEngine.MapTileCount.cy, FMainMap.TileEngine.MapTileCount.cx);

     o.TileEngine.HScrollEnable := FMainMap.TileEngine.HScrollEnable;
     o.TileEngine.VScrollEnable := FMainMap.TileEngine.VScrollEnable;
     o.TileEngine.HLoopMode := FMainMap.TileEngine.HLoopMode;
     o.TileEngine.VLoopMode := FMainMap.TileEngine.VLoopMode;

     o.TileEngine.SetCoordinate( MainMap.TileEngine.GetXY );
   end;

   FMapList.Add(o);

   with FormTools.CLBLayer do begin
    ItemIndex:=Items.Add(o.LayerName);
    Checked[ItemIndex]:=TRUE;
   end;
   PutTileEngineToSceneLayer;
   SetProjectModified;
  end;

end;

procedure TMapList.DeleteMap;
var m: TMapInfo;
  i: integer;
begin
  m := GetSelectedMap;
  if m=NIL then exit;
  if m=MainMap then begin
    ShowMessage('MAIN MAP can not be deleted...');
    exit;
  end;

  FMapList.Remove(m);
  m.Free;
  i:=FormTools.CLBLayer.ItemIndex;
  if i=FormTools.CLBLayer.Count-1 then dec(i);
  FormTools.CLBLayer.Items.Delete(FormTools.CLBLayer.ItemIndex);
  FormTools.CLBLayer.ItemIndex:=i;

  SetProjectModified;
end;

procedure TMapList.RenameMap;
var m: TMapInfo;
begin
  m := GetSelectedMap;
  if m=NIL then exit;

  Form_RenameMap.Edit1.Text := m.LayerName;
  Form_RenameMap.Edit1.SelectAll;
// Form_RenameMap.Edit1.SetFocus;

  if Form_RenameMap.ShowModal=mrOk then begin
    m.LayerName:=Form_RenameMap.Edit1.Text;
    FormTools.CLBLayer.Items.Strings[FormTools.CLBLayer.ItemIndex]:=Form_RenameMap.Edit1.Text;
  end;
end;

procedure TMapList.SetSize(aRow, aColumns:integer);
var i: integer;
begin
  for i:=0 to FMapList.Count-1 do
   with GetMapByIndex(i).TileEngine do begin
     SetMapTileCount( aRow, aColumns );
     SetViewSize( TileSize.cx * MapTileCount.cx, TileSize.cy * MapTileCount.cy ); // set view to whole map
  end;

  SetProjectModified;
end;

procedure TMapList.SetTileEngineCoordinates(aX, aY: single);
var i: integer;
begin
  for i:=0 to FMapList.Count-1 do
   with GetMapByIndex(i).TileEngine do SetCoordinate(aX, aY);
end;

procedure TMapList.MoveTileEngineTo(aX, aY: single);
var i, delta: integer;
begin
  delta := (FScene.Width div MainMap.TileEngine.TileSize.cx) * MainMap.TileEngine.TileSize.cx;
  aX  := EnsureRange(aX, 0, MainMap.TileEngine.MapSize.cx - delta);
  delta := (FScene.Height div MainMap.TileEngine.TileSize.cy) * MainMap.TileEngine.TileSize.cy;
  aY  := EnsureRange(aY, 0, MainMap.TileEngine.MapSize.cy - delta);

  for i:=0 to FMapList.Count-1 do
   with GetMapByIndex(i).TileEngine do
     PositionOnMap.Value := PointF(aX, aY);
end;

procedure TMapList.ResetMaps;
var i: integer;
begin
  for i:=0 to FMapList.Count-1 do begin
    GetMapByIndex(i).TileEngine.ResetMap;
  end;
end;

procedure TMapList.InsertRow(aRowIndex, aCount: integer);
var i: integer;
  t: TTileEngine;
begin
  for i:=0 to FMapList.Count-1 do begin
    t:=GetMapByIndex(i).TileEngine;
    t.InsertRow( aRowIndex, aCount);
    // set tileengine view to see whole map
    t.SetViewSize( t.MapSize.cx, t.MapSize.cy );
    FormTools.UpdateMapParameterOnScreen;
  end;
end;

procedure TMapList.InsertColumn(aColumnIndex, aCount: integer);
var i: integer;
  t: TTileEngine;
begin
  for i:=0 to FMapList.Count-1 do begin
    t := GetMapByIndex(i).TileEngine;
    t.InsertColumn(aColumnIndex, aCount);
    // set tileengine view to see whole map
    t.SetViewSize(t.MapSize.cx, t.MapSize.cy);
    FormTools.UpdateMapParameterOnScreen;
  end;
end;

procedure TMapList.ShiftMapUp;
var i: integer;
begin
  i:= FormTools.CLBLayer.ItemIndex;
  if i<=0 then exit;

  RemoveTileEngineFromSceneLayer;
  FMapList.Exchange( i, i-1 );
  FormTools.CLBLayer.Items.Exchange( i, i-1);
  FormTools.CLBLayer.ItemIndex:=i-1;
  PutTileEngineToSceneLayer;

  SetProjectModified;
end;

procedure TMapList.ShiftMapDown;
var i: integer;
begin
  i := FormTools.CLBLayer.ItemIndex;
  if i = -1 then exit;
  if i = FormTools.CLBLayer.Count-1 then exit;

  RemoveTileEngineFromSceneLayer;
  FMapList.Exchange(i, i+1);
  FormTools.CLBLayer.Items.Exchange(i, i+1);
  FormTools.CLBLayer.ItemIndex := i+1;
  PutTileEngineToSceneLayer;

  SetProjectModified;
end;

procedure TMapList.SetWorkingTileEngine;
begin
  if FormTools.CLBLayer.ItemIndex = -1 then exit;
  FWorkingTileEngine := TileEngine[FormTools.CLBLayer.ItemIndex];
end;

end.

