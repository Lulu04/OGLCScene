unit u_project;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  project_util,
  OGLCScene;

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
  FSceneAspectRatio: single;
  procedure SetSceneAspectRatio(AValue: single);
public
  constructor Create;
  destructor Destroy; override;
  function DoNew: boolean; override;
  procedure DoSave(const aFilename: string); override;
  function DoLoad(const aFilename: string): boolean; override;
  procedure DoClose; override;
  procedure OnModifiedChange( aState: boolean ); override;
  procedure OnProjectReadyChange( aState: boolean ); override;

  // default is 4:3
  property SceneAspectRatio: single read FSceneAspectRatio write SetSceneAspectRatio;
end;

var Project: TProject;

implementation
uses Forms, Dialogs, u_common, form_main, u_texture_list, u_surface_list,
  u_spritebank, u_screen_spritebuilder;

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

procedure TProject.SetSceneAspectRatio(AValue: single);
begin
  if FSceneAspectRatio = AValue then Exit;
  FSceneAspectRatio := AValue;
  SetModified;
  FScene.ChangeAspectRatioTo(AValue);
end;

constructor TProject.Create;
begin
  inherited Create('.oglc');
  SetFormCaption(FormMain, 'Game Helper');
  AddFilterToDialogs('Game helper files', '*.oglc');
  AddFilterToDialogs('All file', '*.*');

  FSceneAspectRatio := 3/4;
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
begin
  t := TStringList.Create;
  try
    ScreenSpriteBuilder.Textures.SaveTo(t);
    ScreenSpriteBuilder.Surfaces.SaveTo(t);
    ScreenSpriteBuilder.Bodies.SaveTo(t);
    SpriteBank.SaveTo(t);

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

      ScreenSpriteBuilder.Textures.LoadFrom(t);
      ScreenSpriteBuilder.Surfaces.LoadFrom(t);
      ScreenSpriteBuilder.Bodies.LoadFrom(t);
      SpriteBank.LoadFrom(t);

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
  SpriteBank.Clear;
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

