unit u_layerlist;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, StdCtrls,
  OGLCScene;

type
{
  The first 10 layers in the list are reserved for the app GameHelper.
  Then, from the index 10, we have the layers created by the user.
}

{ TLayerList }

TLayerList = record
private
  FNames: TStringArray;
  function GetCount: integer;
  function GetName(index: integer): string;
  function GetUserCount: integer;
  function GetUserLayersToStringArray: TStringArray;
  procedure SetName(index: integer; AValue: string);
  function FormatName(const aName: string): string;
  procedure UpdateSceneLayerCount;
public
  procedure InitDefault;
  procedure AddDefaultLayersForUser;

  procedure MakeUserLayersVisible(aValue: boolean);
  function GetUserLayerIndexes: TArrayOfInteger;

  procedure InitWith(A: TStringArray);
  procedure Add(aName: string);
  procedure Delete(aLayerUserIndex: integer);
  procedure Exchange(aLayerUser1, aLayerUser2: integer);

  function UserIndexCanBeDecremented(aLayerUserIndex: integer): boolean;
  function UserIndexCanBeIncremented(aLayerUserIndex: integer): boolean;

  function UserLayerIsVisible(aLayerUserIndex: integer): boolean;
  procedure SetUserLayerVisible(aLayerUserIndex: integer; aValue: boolean);

  function UserLayerNameExists(const aName: string): boolean;

  function SaveToString: string;
  procedure LoadFromString(const s: string);

  procedure FillComboBox(aCB: TComboBox);
  procedure FillListBox(aLB: TListBox);

  // index is 0 based (user index)
  property Names[index:integer]: string read GetName write SetName;
  // return the number of layer for the user
  property UserCount: integer read GetUserCount;
  // return the number of layer (app + user)
  property Count: integer read GetCount;
  // retyurn an array of strin with the user layer names
  property UserLayersToStringArray: TStringArray read GetUserLayersToStringArray;
end;

var
  Layers: TLayerList;


type

TArrayOfDecorLoopMode = array of TOGLCDecorLoopMode;
PArrayOfDecorLoopMode = ^TArrayOfDecorLoopMode;

{ TArrayOfDecorLoopModeHelper }

TArrayOfDecorLoopModeHelper = type helper for TArrayOfDecorLoopMode
  procedure InitDefaultFromLayersCount;
  function SaveToString: string;
  procedure LoadFromString(const s: string);
end;

implementation

uses u_common;

{ TLayerList }

function TLayerList.GetCount: integer;
begin
  Result := Length(FNames);
end;

function TLayerList.GetName(index: integer): string;
begin
  Result := FNames[index+APP_LAYER_COUNT];
end;

function TLayerList.GetUserCount: integer;
begin
  Result := Length(FNames) - APP_LAYER_COUNT;
end;

function TLayerList.GetUserLayersToStringArray: TStringArray;
var i: integer;
begin
  Result := NIL;
  if GetUserCount = 0 then exit;
  SetLength(Result, GetUserCount);

  for i:=0 to High(Result) do
    Result[i] := Names[i];
end;

procedure TLayerList.SetName(index: integer; AValue: string);
begin
  FNames[index+APP_LAYER_COUNT] := FormatName(AValue);
end;

function TLayerList.FormatName(const aName: string): string;
begin
  Result := aName.Replace(' ', '_', [rfReplaceAll]);
end;

procedure TLayerList.UpdateSceneLayerCount;
begin
  FScene.LayerCount := Length(FNames);
end;

procedure TLayerList.InitDefault;
var i: integer;
begin
  FNames := NIL;
  SetLength(FNames, APP_LAYER_COUNT);
  for i:=0 to APP_LAYER_COUNT-1 do
    FNames[i] := 'RESERVED_FOR_GAMEHELPER_'+i.ToString;
  UpdateSceneLayerCount;
end;

procedure TLayerList.AddDefaultLayersForUser;
begin
  Add('LAYER_TOP');
  Add('LAYER_UI');
  Add('LAYER_PLAYER');
  Add('LAYER_ENEMY');
  Add('LAYER_FX');
  Add('LAYER_DECOR');
  Add('LAYER_BG1');
  Add('LAYER_BG2');
  Add('LAYER_BG3');
  UpdateSceneLayerCount;
end;

procedure TLayerList.MakeUserLayersVisible(aValue: boolean);
var i: integer;
begin
  for i:=APP_LAYER_COUNT to High(FNames) do
    FScene.Layer[i].Visible := aValue;
end;

function TLayerList.GetUserLayerIndexes: TArrayOfInteger;
var i: integer;
begin
  Result := NIL;
  if Length(FNames) <= APP_LAYER_COUNT then exit;
  SetLength(Result, Length(FNames)-APP_LAYER_COUNT);

  for i:=0 to High(Result) do
    Result[i] := APP_LAYER_COUNT + i;
end;

procedure TLayerList.InitWith(A: TStringArray);
var i, k: integer;
begin
  InitDefault;
  k := Length(FNames);
  SetLength(FNames, Length(FNames)+Length(A));
  for i:=0 to High(A) do
    FNames[k+i] := A[i];
  UpdateSceneLayerCount;
end;

procedure TLayerList.Add(aName: string);
begin
  SetLength(FNames, Length(FNames)+1);
  FNames[High(FNames)] := FormatName(aName);
  UpdateSceneLayerCount;
end;

procedure TLayerList.Delete(aLayerUserIndex: integer);
var i, j: integer;
begin
  aLayerUserIndex := aLayerUserIndex + APP_LAYER_COUNT;

  // shift the surfaces on their previous layer  <- necessary ?
  FScene.Layer[aLayerUserIndex].Clear;
  for i:=aLayerUserIndex+1 to High(FNames) do
    for j:=0 to FScene.Layer[i].SurfaceCount-1 do
      FScene.Layer[i].Surface[j].MoveToLayer(i-1);

  // delete the name in the array
  system.Delete(FNames, aLayerUserIndex, 1);

  UpdateSceneLayerCount;
end;

procedure TLayerList.Exchange(aLayerUser1, aLayerUser2: integer);
var temp: string;
begin
  aLayerUser1 := aLayerUser1 + APP_LAYER_COUNT;
  aLayerUser2 := aLayerUser2 + APP_LAYER_COUNT;
  // we don't modify the reserved layers
  if aLayerUser1 < APP_LAYER_COUNT then exit;
  if aLayerUser2 < APP_LAYER_COUNT then exit;

  temp := FNames[aLayerUser1];
  FNames[aLayerUser1] := FNames[aLayerUser2];
  FNames[aLayerUser2] := temp;
end;

function TLayerList.UserIndexCanBeDecremented(aLayerUserIndex: integer): boolean;
begin
  Result := aLayerUserIndex > APP_LAYER_COUNT;
end;

function TLayerList.UserIndexCanBeIncremented(aLayerUserIndex: integer): boolean;
begin
  Result := aLayerUserIndex < High(FNames);
end;

function TLayerList.UserLayerIsVisible(aLayerUserIndex: integer): boolean;
begin
  Result := FScene.Layer[aLayerUserIndex+APP_LAYER_COUNT].Visible;
end;

procedure TLayerList.SetUserLayerVisible(aLayerUserIndex: integer; aValue: boolean);
begin
  FScene.Layer[aLayerUserIndex+APP_LAYER_COUNT].Visible := aValue;
end;

function TLayerList.UserLayerNameExists(const aName: string): boolean;
var i: integer;
begin
  for i:= APP_LAYER_COUNT to High(FNames) do
    if FNames[i] = aName then exit(True);
  Result := False;
end;

function TLayerList.SaveToString: string;
var i: integer;
begin
  Result := '';
  for i:=APP_LAYER_COUNT to High(FNames) do begin
    Result := Result + FNames[i];
    if i < High(FNames) then Result := Result + ' ';
  end;
end;

procedure TLayerList.LoadFromString(const s: string);
var i: integer;
  A: TStringArray;
begin
  InitDefault;
  A := s.Split([' ']);

  if Length(A) = 0 then begin
    Add('LAYER_TOP');
    exit;
  end;

  for i:=0 to High(A) do
    Add(A[i]);
end;

procedure TLayerList.FillComboBox(aCB: TComboBox);
var i: integer;
begin
  if aCB.Items.Count <> Length(FNames)-APP_LAYER_COUNT then begin
    aCB.Clear;
    for i:=APP_LAYER_COUNT to High(FNames) do
      aCB.Items.Add(FNames[i]);
  end else begin
    for i:=APP_LAYER_COUNT to High(FNames) do
      aCB.Items.Strings[i-APP_LAYER_COUNT] := FNames[i];
  end;
end;

procedure TLayerList.FillListBox(aLB: TListBox);
var i: integer;
begin
  aLB.Clear;
  for i:=APP_LAYER_COUNT to High(FNames) do
    aLB.Items.Add(FNames[i]);
end;

{ TArrayOfDecorLoopModeHelper }

procedure TArrayOfDecorLoopModeHelper.InitDefaultFromLayersCount;
var i: integer;
begin
  Self := NIL;
  SetLength(Self, Layers.UserCount);
  for i:=0 to High(Self) do
    Self[i] := dlmNone;
end;

function TArrayOfDecorLoopModeHelper.SaveToString: string;
var i: integer;
begin
  Result := '';
  for i:=0 to High(Self) do begin
    Result := Result + Ord(Self[i]).ToString;
    if i < High(Self) then Result := Result + ' ';
  end;
end;

procedure TArrayOfDecorLoopModeHelper.LoadFromString(const s: string);
var i: integer;
  A: TStringArray;
begin
  Self := NIL;
  if s = '' then exit;

  A := s.Split([' ']);
  SetLength(Self, Length(A));
  for i:=0 to High(A) do
    Self[i] := TOGLCDecorLoopMode(A[i].ToInteger);
end;

end.

