unit u_layerlist;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

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
  procedure SetName(index: integer; AValue: string);
  function FormatName(const aName: string): string;
public
  procedure InitDefault;
  procedure AddDefaultLayersForUser;

  procedure MakeUserLayersVisible(aValue: boolean);
  function GetUserLayerIndexes: TArrayOfInteger;

  procedure InitWith(A: TStringArray);
  procedure Add(aName: string);
  procedure Delete(aUserIndex: integer);

  function UserIndexCanBeDecremented(aLayerUserIndex: integer): boolean;
  function UserIndexCanBeIncremented(aLayerUserIndex: integer): boolean;

  function UserLayerIsVisible(aLayerUserIndex: integer): boolean;
  procedure SetUserLayerVisible(aLayerUserIndex: integer; aValue: boolean);


  function SaveToString: string;
  procedure LoadFromString(const s: string);
  procedure FillComboBox(aCB: TComboBox);
  procedure FillListBox(aLB: TListBox);

  // index is 0 based (user index)
  property Names[index:integer]: string read GetName write SetName;
  // return the number of user layer.
  property Count: integer read GetCount;
end;

var
  Layers: TLayerList;

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

procedure TLayerList.SetName(index: integer; AValue: string);
begin
  FNames[index+APP_LAYER_COUNT] := FormatName(AValue);
end;

function TLayerList.FormatName(const aName: string): string;
begin
  Result := aName.Replace(' ', '_', [rfReplaceAll]);
end;

procedure TLayerList.InitDefault;
var i: integer;
begin
  FNames := NIL;
  SetLength(FNames, APP_LAYER_COUNT);
  for i:=0 to APP_LAYER_COUNT-1 do
    FNames[i] := 'RESERVED_FOR_GAMEHELPER_'+i.ToString;
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
end;

procedure TLayerList.MakeUserLayersVisible(aValue: boolean);
var i: integer;
begin
  for i:= APP_LAYER_COUNT to High(FNames) do
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
end;

procedure TLayerList.Add(aName: string);
begin
  SetLength(FNames, Length(FNames)+1);
  FNames[High(FNames)] := FormatName(aName);

  FScene.LayerCount := FScene.LayerCount + 1;
end;

procedure TLayerList.Delete(aUserIndex: integer);
var i, j: integer;
begin
  aUserIndex := aUserIndex + APP_LAYER_COUNT;
  // we don't erase the reserved layer
  if aUserIndex < APP_LAYER_COUNT then exit;

  // shift the surfaces on their previous layer
  FScene.Layer[aUserIndex].Clear;
  for i:=aUserIndex+1 to High(FNames) do
    for j:=0 to FScene.Layer[i].SurfaceCount-1 do
      FScene.Layer[i].Surface[j].MoveToLayer(i-1);

  SetLength(FNames, Length(FNames)-1);
  FScene.LayerCount := FScene.LayerCount - 1;
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
fscene.LogDebug('layer list loaded: count='+integer(Length(FNames)).tostring);
end;

procedure TLayerList.FillComboBox(aCB: TComboBox);
var i: integer;
begin
  aCB.Clear;
  for i:=APP_LAYER_COUNT to High(FNames) do
    aCB.Items.Add(FNames[i]);
end;

procedure TLayerList.FillListBox(aLB: TListBox);
var i: integer;
begin
  aLB.Clear;
  for i:=APP_LAYER_COUNT to High(FNames) do
    aLB.Items.Add(FNames[i]);
end;

end.

