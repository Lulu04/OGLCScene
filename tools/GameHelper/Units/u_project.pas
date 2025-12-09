unit u_project;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  project_util,
  OGLCScene, u_layerlist, u_target_lazarusproject;

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
  // level editor
  LevelEditorOverlap: single;
  // common
  CommonShowFlyingTxt: boolean;
  procedure InitDefault;
  function SaveToString: string;
  procedure LoadFromString(const data: string);
  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);
  procedure SaveToFile(const aFilename: string);
  function LoadFromFile(const aFilename: string): boolean;

public // utils to modify target lazarus project files
  TargetLazarusProject: TTargetLazarusProject;
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
uses Forms, Dialogs, Controls, u_common, form_main, u_spritebank, u_levelbank,
  form_newproject, u_app_pref, u_utils, u_connection_to_ide, u_ui_objectlist,
  utilitaire_fichier, LCLIntf;

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

  LevelEditorOverlap := 0.5;

  CommonShowFlyingTxt := True;
end;

function TProjectConfig.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('|');
  // scene
  prop.Add('SceneWidth', SceneWidth);
  prop.Add('SceneHeight', SceneHeight);
  prop.Add('MaximizeScene', MaximizeScene);
  // layers names NOT NECESSARY because they are read from u_common.pas
  //prop.Add('Layers', Layers.SaveToString);

  // level editor
  prop.Add('LevelEditorOverlap', LevelEditorOverlap);
  // common
  prop.Add('CommonShowFlyingTxt', CommonShowFlyingTxt);

  Result := prop.PackedProperty;
end;

procedure TProjectConfig.LoadFromString(const data: string);
var prop: TProperties;
begin
  prop.Split(data, '|');
  // scene
  prop.IntegerValueOf('SceneWidth', SceneWidth, 1024);
  prop.IntegerValueOf('SceneHeight', SceneHeight, 768);
  prop.BooleanValueOf('MaximizeScene', MaximizeScene, True);
  // layers
  //prop.StringValueOf('Layers', s, 'LAYER_TOP');
  //Layers.LoadFromString(s);
  // level editor
  prop.SingleValueOf('LevelEditorOverlap', LevelEditorOverlap, 0.5);
  // common
  prop.BooleanValueOf('CommonShowFlyingTxt', CommonShowFlyingTxt, True);
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

procedure TProjectConfig.SaveToFile(const aFilename: string);
var t: TStringList;
begin
  FScene.LogInfo('Try to save project "'+aFilename+'"');
  t := TStringList.Create;
  try
    try
      SaveTo(t);
      t.SaveToFile(aFilename);
      FScene.LogInfo('done', 1);
    except
      On E :Exception do begin
        FScene.LogError('TProjectConfig.Save: exception occur');
        FScene.logError(E.Message, 1);
     end;
    end;
  finally
    t.Free;
  end;
end;

function TProjectConfig.LoadFromFile(const aFilename: string): boolean;
var t: TStringList;
begin
  FScene.LogInfo('Try to load project "'+aFilename+'"');
  t := TStringList.Create;
  try
    try
      t.LoadFromFile(aFilename);
      LoadFrom(t);
      Result := True;
      FScene.LogInfo('done', 1);
    except
      On E :Exception do begin
        FScene.LogError('TProjectConfig.Load: exception occur', 1);
        FScene.logError(E.Message, 2);
        Result := False;
      end;
    end;
  finally
    t.Free;
  end;
end;

{ TProject }

constructor TProject.Create;
begin
  inherited Create('.oglc');
  if IdeConnect.Activated then SetFormCaption(FormMain, 'Game Helper (from IDE)')
    else SetFormCaption(FormMain, 'Game Helper');
  AddFilterToDialogs('Game helper files', '*.oglc');
  AddFilterToDialogs('All file', '*.*');
end;

function TProject.DoNew: boolean;
var folderProjectTemplate, lazProjectFolder, lazProjectName, lpiFilename: string;
  flagdone: boolean;
begin
  FormNewProject := TFormNewProject.Create(NIL);
  try
    if FormNewProject.ShowModal <> mrOk then exit(False);
    // retrieve the project template folder
    folderProjectTemplate := GetSourceProjectTemplateFolder;
    lazProjectFolder := FormNewProject.GetLazarusProjectFolder;
    lazProjectName := FormNewProject.GetLazarusProjectName;
  finally
    FormNewProject.Free;
  end;

  flagdone := True;
  FScene.LogInfo('Try to create a new project "'+lazProjectName+'" in directory "'+lazProjectFolder+'"');

  FScene.LogInfo('copy the lazarus project template to the new directory', 1);
  try
    CopyDirectoryContent(folderProjectTemplate, lazProjectFolder);
    FScene.LogInfo('done', 2);
  except
    On E :Exception do begin
      FScene.LogError('Exception '+E.Message, 2);
      flagdone := False;
    end;
  end;
  if flagdone then begin
    // rename project files
    FScene.LogInfo('Renaming file project_oglcscene.ico', 1);
    try
      flagdone := flagDone and RenommeFichier(lazProjectFolder+'project_oglcscene.ico',
                                              lazProjectFolder+lazProjectName+'.ico');
      if flagdone then FScene.LogInfo('done', 2)
        else FScene.LogInfo('FAIL', 2);
    except
      On E :Exception do begin
        FScene.LogError('Exception: '+E.Message, 2);
        flagdone := False;
      end;
    end;

    if flagdone then begin
      FScene.LogInfo('Renaming file project_oglcscene.lpi', 1);
      try
        flagdone := flagDone and RenommeFichier(lazProjectFolder+'project_oglcscene.lpi',
                                                lazProjectFolder+lazProjectName+'.lpi');
        if flagdone then FScene.LogInfo('done', 2)
          else FScene.LogInfo('FAIL', 2);
      except
        On E :Exception do begin
          FScene.LogError('Exception: '+E.Message, 2);
          flagdone := False;
        end;
      end;
      if flagdone then begin
        FScene.LogInfo('Renaming file project_oglcscene.lpr', 1);
        try
        flagdone := flagDone and RenommeFichier(lazProjectFolder+'project_oglcscene.lpr',
                                              lazProjectFolder+lazProjectName+'.lpr');
        if flagdone then FScene.LogInfo('done', 2)
          else FScene.LogInfo('FAIL', 2);
        except
          On E :Exception do begin
            FScene.LogError('Exception: '+E.Message, 2);
            flagdone := False;
          end;
        end;
        if flagdone then begin
          // replace project name in lazarus files
          FScene.LogInfo('replacing name in lpi and lpr file', 1);
          try
            ReplaceStringInFile(lazProjectFolder+lazProjectName+'.lpi', 'project_oglcscene', lazProjectName);
            ReplaceStringInFile(lazProjectFolder+lazProjectName+'.lpr', 'project_oglcscene', lazProjectName);
            FScene.LogInfo('done', 2);
          except
            On E :Exception do begin
              FScene.LogError('Exception: '+E.Message, 2);
              flagdone := False;
            end;
          end;
        end;
      end;
    end;
  end;

  FScene.LogEmptyLine;

  Config.InitDefault;

  SaveAs(lazProjectFolder + lazProjectName + '.oglc');

  if flagdone then begin
    lpiFilename := lazProjectFolder + ChangeFileExt(lazProjectName, '.lpi');
    if QuestionDlg('', 'Do you want to open Lazarus with the new project ?'+LineEnding+
       lpiFilename, mtConfirmation, [mrOK, 'Yes', mrCancel, 'No'], 0) = mrOk then
      OpenDocument(lpiFilename);
  end;

  Result := true;
end;

procedure TProject.DoSave(const aFilename: string);
//var pathGameHelperFiles: string;
begin
  Config.SaveToFile(aFilename);

  // do not save the banks because they are saved immediatly after a change

{  pathGameHelperFiles := IncludeTrailingPathDelimiter(ExtractFilePath(aFilename)) +
                         GAMEHELPERFILES_SUBFOLDER + PathDelim;
  SpriteBank.SaveToPath(pathGameHelperFiles);
  LevelBank.SaveToPath(pathGameHelperFiles);
  FontBank.Load;}

  AppPref.LastProjectFilename := aFilename;
end;

function TProject.DoLoad(const aFilename: string): boolean;
var pathGameHelperFiles: string;
begin
  Result := Config.LoadFromFile(aFilename);

  if Result then begin
    pathGameHelperFiles := IncludeTrailingPathDelimiter(ExtractFilePath(aFilename)) +
                           GAMEHELPERFILES_SUBFOLDER + PathDelim;
    SpriteBank.LoadFromPath(pathGameHelperFiles);
    LevelBank.LoadFromPath(pathGameHelperFiles);
    FontBank.Load;

    AppPref.LastProjectFilename := aFilename;
  end;
end;

procedure TProject.DoClose;
begin
  FScene.LogInfo('Closing current project');
  try
    SpriteBank.Clear;
    LevelBank.Clear;
    FontBank.Clear;
    Config.InitDefault;
    WorkingLevelGroup := NIL;
    FScene.LogInfo('done', 1);
  except
    On E :Exception do begin
      FScene.LogError('Exception: '+E.Message, 1);
    end;
  end;
end;

procedure TProject.OnModifiedChange(aState: boolean);
begin
  aState := aState;
  FormMain.UpdateWidgets;
end;

procedure TProject.OnProjectReadyChange(aState: boolean);
begin
  aState := aState;
  if IsReady then begin
    // read the layer list from unit u_common
    Layers.InitWith(Config.TargetLazarusProject.UCommonGetLayerNames);

    // Sprite Builder: fill the listbox with texture  names
    FrameToolsSpriteBuilder.FillListBoxTextureNames;

    if LevelBank.Size > 0 then FormMain.ShowPageLevelBank
      else FormMain.ShowPageSpriteBank;

    // retrieve layers from unit u_common.pas
    Layers.InitWith(Config.TargetLazarusProject.UCommonGetLayerNames);
  end else begin
  end;
end;

end.

