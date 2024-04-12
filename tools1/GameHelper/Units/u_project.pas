unit u_project;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  project_util,
  OGLCScene, form_tools;

var
  LayerNames: TStringArray;
  FAtlas: TOGLCTextureAtlas;


  function AudioFolder: string;
  function ParticleFolder: string;
  function SpriteFolder: string;
  function SpriteCommonFolder: string;

  function ALSoundLibrariesSubFolder: string;

  function PPIScale(AValue: integer): integer;

type

{ TProject }

TProject = class(TCustomProject)
private
  FSceneProportion: single;
  FScreenList: string;
  procedure SetSceneProportion(AValue: single);
  procedure SetScreenList(AValue: string);
public
  constructor Create;
  destructor Destroy; override;
  function DoNew: boolean; override;
  procedure DoSave(const aFilename: string); override;
  function DoLoad(const aFilename: string): boolean; override;
  procedure DoClose; override;
  procedure OnModifiedChange( aState: boolean ); override;
  procedure OnProjectReadyChange( aState: boolean ); override;

  // default is 4:3 (3/4)
  property SceneProportion: single read FSceneProportion write SetSceneProportion;
  property ScreenList: string read FScreenList write SetScreenList;
end;

var Project: TProject;

implementation
uses Forms, Dialogs, u_common, form_main;

function PPIScale(AValue: integer): integer;
begin
  Result := FScene.ScaleDesignToScene(AValue);
end;

function AudioFolder: string;
begin
  Result := FScene.App.DataFolder+'Sounds'+DirectorySeparator;
end;

function ParticleFolder: string;
begin
  Result := FScene.App.DataFolder+'Particles'+DirectorySeparator;
end;

function SpriteFolder: string;
begin
  Result := FScene.App.DataFolder+'Sprites'+DirectorySeparator;
end;

function SpriteCommonFolder: string;
begin
  Result := SpriteFolder+'Common'+DirectorySeparator;
end;

function ALSoundLibrariesSubFolder: string;
begin
  Result := FScene.App.ALSoundLibrariesSubFolder;
end;

{ TProject }

procedure TProject.SetSceneProportion(AValue: single);
begin
  if FSceneProportion = AValue then Exit;
  FSceneProportion := AValue;
  Save;
end;

procedure TProject.SetScreenList(AValue: string);
begin
  if FScreenList = AValue then Exit;
  FScreenList := AValue;
  Save;
end;

constructor TProject.Create;
begin
  inherited Create('.screen');
  SetFormCaption(FormMain, 'Game Helper');
  AddFilterToDialogs('Screen files', '*.screen');
  AddFilterToDialogs('All file', '*.*');

  FSceneProportion := 3/4;
end;

destructor TProject.Destroy;
begin
  inherited Destroy;
end;

function TProject.DoNew: boolean;
begin
  //Save;
  //Result := IsReady;

  Result := true;
end;

procedure TProject.DoSave(const aFilename: string);
var t: TStringList;
  prop: TProperties;
begin
  t := TStringList.Create;
  try
    prop.Init('|');
    prop.Add('SceneProportion', SceneProportion);
    prop.Add('ScreenList', FScreenList);

    t.Add(prop.PackedProperty);
    t.SaveToFile(aFilename);
  finally
    t.Free;
  end;
end;

function TProject.DoLoad(const aFilename: string): boolean;
var t: TStringList;
  prop: TProperties;
  vs: single;
begin
  t := TStringList.Create;
  try
    try
      t.LoadFromFile(aFilename);
      if t.Count = 0 then exit(False);
      prop.Split(t.Strings[0], '|');
      vs := 3/4;
      prop.SingleValueOf('SceneProportion', vs, vs);
      SceneProportion := vs;

      prop.StringValueOf('ScreenList', FScreenList, '');

      Result := True;
    except
      Result := False;
    end;
  finally
    t.Free;
  end;
end;

procedure TProject.DoClose;
begin

end;

procedure TProject.OnModifiedChange(aState: boolean);
begin
end;

procedure TProject.OnProjectReadyChange(aState: boolean);
begin
  if IsReady then begin
    FormTools.Show;
    FormTools.LoadScreenList;
  end else begin
    FormTools.Hide;
  end;
end;

end.

