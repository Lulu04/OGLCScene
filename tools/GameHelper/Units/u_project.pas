unit u_project;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  project_util,
  OGLCScene, u_layerlist;

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

{ TProjectConfig }

TProjectConfig = record
  // scene
  SceneWidth, SceneHeight: integer;
  MaximizeScene: boolean;
  // level bank
  LevelBankExportClassName,
  LevelBankExportUnitName: string;
  procedure InitDefault;
  function SaveToString: string;
  procedure LoadFromString(const data: string);
  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);
end;

{ TProject }

TProject = class(TCustomProject)
private
public
  Config: TProjectConfig;
  constructor Create;
  function DoNew: boolean; override;
  procedure DoSave(const aFilename: string); override;
  function DoLoad(const aFilename: string): boolean; override;
  procedure DoClose; override;
  procedure OnModifiedChange(aState: boolean); override;
  procedure OnProjectReadyChange(aState: boolean); override;
end;

var Project: TProject;

implementation
uses Forms, Dialogs, u_common, form_main, u_texture_list, u_surface_list,
  u_spritebank, u_screen_spritebuilder, u_levelbank;

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

{ TProjectConfig }

procedure TProjectConfig.InitDefault;
begin
  Layers.InitDefault;
  Layers.AddDefaultLayersForUser;

  SceneWidth := 1024;
  SceneHeight := 768;
  MaximizeScene := True;

  LevelBankExportClassName := 'TLevels';
  LevelBankExportUnitName := 'u_levels.pas';
end;

function TProjectConfig.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('|');
  // scene
  prop.Add('SceneWidth', SceneWidth);
  prop.Add('SceneHeight', SceneHeight);
  prop.Add('MaximizeScene', MaximizeScene);
  // layers
  prop.Add('Layers', Layers.SaveToString);
  Result := prop.PackedProperty;
  //level bank
  prop.Add('LevelBankExportClassName', LevelBankExportClassName);
  prop.Add('LevelBankExportUnitName', LevelBankExportUnitName);

  Result := prop.PackedProperty;
end;

procedure TProjectConfig.LoadFromString(const data: string);
var prop: TProperties;
  s: string;
begin
  prop.Split(data, '|');
  s := '';
  // scene
  prop.IntegerValueOf('SceneWidth', SceneWidth, 1024);
  prop.IntegerValueOf('SceneHeight', SceneHeight, 768);
  prop.BooleanValueOf('MaximizeScene', MaximizeScene, True);
  // layers
  prop.StringValueOf('Layers', s, 'LAYER_TOP');
  Layers.LoadFromString(s);
  //level bank
  prop.StringValueOf('LevelBankExportClassName', LevelBankExportClassName, LevelBankExportClassName);
  prop.StringValueOf('LevelBankExportUnitName', LevelBankExportUnitName, LevelBankExportUnitName);
end;

procedure TProjectConfig.SaveTo(t: TStringList);
begin
  t.Add('[CONFIG]');
  t.Add(SaveToString);
end;

procedure TProjectConfig.LoadFrom(t: TStringList);
var k: integer;
begin
  Self := Default(TProjectConfig);
  k := t.IndexOf('[CONFIG]');
  if (k = -1) or (k = t.Count-1) then exit;
  LoadFromString(t.Strings[k+1]);
end;

{ TProject }

constructor TProject.Create;
begin
  inherited Create('.oglc');
  SetFormCaption(FormMain, 'Game Helper');
  AddFilterToDialogs('Game helper files', '*.oglc');
  AddFilterToDialogs('All file', '*.*');
end;

function TProject.DoNew: boolean;
begin
  Config.InitDefault;
  Result := true;
end;

procedure TProject.DoSave(const aFilename: string);
var t: TStringList;
begin
  t := TStringList.Create;
  try
    Config.SaveTo(t);
    SpriteBank.SaveTo(t);
    LevelBank.SaveTo(t);

    t.SaveToFile(aFilename);
  finally
    t.Free;
  end;
end;

function TProject.DoLoad(const aFilename: string): boolean;
var t: TStringList;
begin
  t := TStringList.Create;
  try
    try
      t.LoadFromFile(aFilename);
      if t.Count = 0 then exit(False);

      Config.LoadFrom(t);
      SpriteBank.LoadFrom(t);
      LevelBank.LoadFrom(t);

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
  ScreenSpriteBuilder.Textures.Clear;
  ScreenSpriteBuilder.Surfaces.Clear;
  ScreenSpriteBuilder.Bodies.Clear;
  SpriteBank.Clear;
  LevelBank.Clear;
  Config.InitDefault;
end;

procedure TProject.OnModifiedChange(aState: boolean);
begin
end;

procedure TProject.OnProjectReadyChange(aState: boolean);
begin
  if IsReady then begin
    FrameToolsSpriteBuilder.FillListBoxTextureNames;
    FormMain.ShowPageSpriteBank;
  end else begin
  end;
end;

end.

