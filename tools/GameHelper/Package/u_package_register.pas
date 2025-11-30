unit u_package_register;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, LCLType, Forms,
  IDECommands, MenuIntf, SrcEditorIntf, IDEWindowIntf, LazIDEIntf,
  PackageIntf, LCLProc, ProjectIntf,
  IDEMsgIntf, IDEExternToolIntf, CodeToolManager, {CodeCache,}
  u_respons_commontype;

resourcestring
  sRunGameHelper='Run Game Helper';
  sGameHelperNotFoundInOrderTo='Game Helper executable not found!'+LineEnding+
         'In order to use it, first compile the project GameHelper.lpi in folder tools/GameHelper, package OGLCScenePackage';
  sNotFoundInTheIDE='not found in the IDE...';
  sEnableToLoadUnitInTheEditor='Enable to load unit in the editor';
//  sEnableToAddUnitxxxToProjectUsesClause='Enable to add unit %s to uses clause of project.lpr...';
  sUnitOpenedInSourceEditor='Unit %s opened in source editor and added to project';
  sEnableToRemoveUnitxxxFromProject='Enable to remove unit %s from project';
  sUnitxxxDeletedAndRemovedFromProject='Unit %s deleted and removed from project';

var
  RunOGLCSceneGameHelperCmd: TIDECommand;
//const
//  GameHelperConnexionWindowName = 'IDEOGLCWindow';

procedure Register;
{procedure CreateIDEWindowGameHelperConnexion(Sender: TObject; aFormName: string;
                          var AForm: TCustomForm; DoDisableAutoSizing: boolean); }

type

{ TConnectionUtils }

TConnectionUtils = record
  procedure DoProcessOGLCGameHelperMenuClick(Sender: TObject);
  procedure SaveAnEmptyOGLCProject(const aFilename: string);
  procedure BackupOGLCFiles;

  // aUrgency can be mluVerbose for simple hint, mluWarning for warning
  procedure ShowMessageInIDE(const aMess: string; aUrgency: TMessageLineUrgency=mluVerbose);
  procedure ShowMessageInForm(const aMess: string);

  function GetActiveLazarusProjectFilename: string;
  function GetActiveProjectPath: string;
  function GetFolderGameHelperFiles: string;
  function GetActiveProjectUnitsPath: string;
  function GetActiveProjectUnitsSpritesPath: string;
  function GetActiveProjectUnitsLevelsPath: string;
  function GetActiveProjectBinaryPath: string;
  function GetActiveProjectBinaryDataPath: string;
  function GetActiveProjectBinaryDataTexturesPath: string;
  // return an array with the short unit names of the units that are part of the current project
  // ex: MyGame.lpr, Units/u_common.pas, Units/Sprites/u_sprite_car.pas
  function GetProjectUnitShortFilenames: TStringArray;
  function CurrentLazarusProjectIsOGLCSceneProject: boolean;

  function UnitIsPartOfProject(aItem: TResponsItem): boolean; overload;
  function UnitIsPartOfProject(const aShortFilename: string): boolean; overload;
  function IndexOfUnitInFilesProject(aItem: TResponsItem): integer;
  procedure DoAddUnitToProject(aItem: TResponsItem);
  procedure DoRemoveUnitFromProject(aItem: TResponsItem);
  // return True if the message is "Game Helper terminate"
  function DecodeMessageFromGameHelper(const aMess: string): boolean;
end;
var ConnectionUtils: TConnectionUtils;

implementation

uses Dialogs, Controls, FileUtil, LazFileUtils,
  OGLCScene, form_gamehelper_connection, utilitaire_fichier;


procedure ProcessOGLCSceneGameHelperMenuClick(Sender: TObject);
begin
  ConnectionUtils.DoProcessOGLCGameHelperMenuClick(Sender);
end;

function TConnectionUtils.GetActiveLazarusProjectFilename: string;
begin
  Result := LazarusIDE.ActiveProject.MainFile.GetFullFilename;
end;

function TConnectionUtils.GetActiveProjectPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(LazarusIDE.ActiveProject.MainFile.GetFullFilename));
end;

function TConnectionUtils.GetFolderGameHelperFiles: string;
begin
  Result := GetActiveProjectPath + 'GameHelperFiles' + PathDelim;
end;

function TConnectionUtils.GetActiveProjectUnitsPath: string;
begin
  Result := GetActiveProjectPath + 'Units' + PathDelim;
end;

function TConnectionUtils.GetActiveProjectUnitsSpritesPath: string;
begin
  Result := GetActiveProjectUnitsPath + 'Sprites' + PathDelim;
end;

function TConnectionUtils.GetActiveProjectUnitsLevelsPath: string;
begin
  Result := GetActiveProjectUnitsPath + 'Levels' + PathDelim;
end;

function TConnectionUtils.GetActiveProjectBinaryPath: string;
begin
  Result := GetActiveProjectPath + 'Binary' + PathDelim;
end;

function TConnectionUtils.GetActiveProjectBinaryDataPath: string;
begin
  Result := GetActiveProjectBinaryPath + 'Data' + PathDelim;
end;

function TConnectionUtils.GetActiveProjectBinaryDataTexturesPath: string;
begin
  Result := GetActiveProjectBinaryDataPath + 'Textures' + PathDelim;
end;

//
function TConnectionUtils.GetProjectUnitShortFilenames: TStringArray;
var
  LazProject: TLazProject;
  i: Integer;
  LazFile: TLazProjectFile;
begin
  Result := NIL;
  LazProject := LazarusIDE.ActiveProject;
  if LazProject <> nil then begin
    SetLength(Result, LazProject.FileCount);
    for i:=0 to LazProject.FileCount-1 do
    begin
      LazFile := LazProject.Files[i];
      if LazFile.IsPartOfProject
      //and FilenameIsPascalUnit(LazFile.Filename)
      then
        Result[i] := LazFile.GetShortFilename(true); //.Filename;
    end;
  end;
end;

function TConnectionUtils.CurrentLazarusProjectIsOGLCSceneProject: boolean;
begin
  Result := // check package
            (PackageEditingInterface.FindPackageWithName('OGLCScenePackage') <> NIL) and
            // check directory structure
            DirectoryExistsUTF8(GetActiveProjectUnitsPath) and
            DirectoryExistsUTF8(GetFolderGameHelperFiles) and
            DirectoryExistsUTF8(GetActiveProjectUnitsSpritesPath) and
            DirectoryExistsUTF8(GetActiveProjectUnitsLevelsPath) and
            DirectoryExistsUTF8(GetActiveProjectBinaryPath) and
            DirectoryExistsUTF8(GetActiveProjectBinaryDataPath) and
            DirectoryExistsUTF8(GetActiveProjectBinaryDataTexturesPath) and
            // check some file names to avoid user to change their names because Game Helper can modified them
            UnitIsPartOfProject('Units'+PathDelim+'form_main.pas') and
            UnitIsPartOfProject('Units'+PathDelim+'project_config.cfg') and
            UnitIsPartOfProject('Units'+PathDelim+'u_common.pas');
end;

function TConnectionUtils.UnitIsPartOfProject(aItem: TResponsItem): boolean;
begin
  Result := IndexOfUnitInFilesProject(aItem) <> -1;
end;

function TConnectionUtils.UnitIsPartOfProject(const aShortFilename: string): boolean;
var A: TStringArray;
  i: Integer;
begin
  Result := False;

  A := GetProjectUnitShortFilenames;
  if Length(A) = 0 then exit;
  for i:=0 to High(A) do
    if CompareFilenames(A[i], aShortFilename) = 0 then
      exit(True);
end;

function TConnectionUtils.IndexOfUnitInFilesProject(aItem: TResponsItem): integer;
var LazProject: TLazProject;
  i: Integer;
  LazFile: TLazProjectFile;
  unitShortName: string;
begin
  Result := -1;
  LazProject := LazarusIDE.ActiveProject;
  unitShortName := aItem.ExpandToShortFilename;

  for i:=0 to LazProject.FileCount-1 do
  begin
    LazFile := LazProject.Files[i];
    if LazFile.IsPartOfProject and
       (CompareFilenames(LazFile.GetShortFilename(True), unitShortName) = 0) then exit(i);
  end;

end;

procedure TConnectionUtils.DoAddUnitToProject(aItem: TResponsItem);
var unitFilename: string;
  Editor: TSourceEditorInterface;
begin
  unitFilename := GetActiveProjectPath + aItem.ExpandToShortFilename;

  // open the unit in IDE editor
  if LazarusIDE.DoOpenEditorFile(unitFilename,-1,-1,[ofOnlyIfExists, ofAddToRecent, ofRevert, ofAddToProject]) <> mrOk then
    ShowMessageInForm(sEnableToLoadUnitInTheEditor+' '+unitFilename);

//  if UnitIsPartOfProject(aItem) then ShowMessageInIDE('unit '+aItem.UnitName+' is already part of the project')
//    else ShowMessageInIDE('Adding unit '+aItem.UnitName+' to the project');

  // add file lazarus project
  Editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(unitFilename);
  if Editor <> NIL then begin
    LazarusIDE.ActiveProject.AddFile(Editor.GetProjectFile, True);
    ShowMessageInForm(Format(sUnitOpenedInSourceEditor, [aItem.ExpandToUnitNameWithoutExt]));
  end else
    ShowMessageInForm(Format('Enable to add unit %s to the Lazarus project', [aItem.ExpandToUnitNameWithoutExt]));

{  Editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(unitFilename);
  if Editor <> NIL then begin
    LazarusIDE.DoAddUnitToProject(Editor);
    ShowMessageInIDE(Format(sUnitOpenedInSourceEditor, [aItem.UnitName]));
  end else begin
    ShowMessageInIDE(Format('Enable to add unit %s to your Lazarus project', [aItem.UnitName]), mluWarning);
  end;
  exit; }


//  if UnitIsPartOfProject(aItem) then exit;

{  // add the unit in projet .lpr
  Code := CodeToolBoss.LoadFile(GetActiveProjectFilename, False, False);
  if Code <> NIL then begin
    // save changes in source editor to codetools
    LazarusIDE.SaveSourceEditorChangesToCodeCache(nil); // commit all source editors
    // add unit to interface uses
    if not CodeToolBoss.AddUnitToMainUsesSectionIfNeeded(Code, aItem.UnitName, '') then begin
      ShowMessageInIDE(Format(sEnableToAddUnitxxxToProjectUsesClause, [aItem.UnitName]));
      LazarusIDE.DoJumpToCodeToolBossError;
      exit;
    end;

    ShowMessageInIDE(Format(sNewUnitOpenedInSourceEditor, [aItem.UnitName]));
  end    }
end;

procedure TConnectionUtils.DoRemoveUnitFromProject(aItem: TResponsItem);
var unitFilename: String;
  unitIndexInProjectfiles: integer;
  Editor: TSourceEditorInterface;
begin
  if not UnitIsPartOfProject(aItem) then exit;

  // remove unit from project
  unitIndexInProjectfiles := IndexOfUnitInFilesProject(aItem);
  if unitIndexInProjectfiles <> -1
    then LazarusIDE.ActiveProject.RemoveUnit(unitIndexInProjectfiles, True)
    else ShowMessageInForm(Format(sEnableToRemoveUnitxxxFromProject, [aItem.ExpandToUnitNameWithoutExt]));

  // close the source editor for the unit
  unitFilename := GetActiveProjectPath + aItem.ExpandToShortFilename;
  Editor:=SourceEditorManagerIntf.SourceEditorIntfWithFilename(unitFilename);
  if Editor <> NIL then
    LazarusIDE.DoCloseEditorFile(Editor, [cfSaveFirst]);

  ShowMessageInForm(Format(sUnitxxxDeletedAndRemovedFromProject, [aItem.ExpandToUnitNameWithoutExt]));

  // don't delete file on disk from the IDE because an exception appears
  // delete file on disk
  //if FileExistsUTF8(unitFilename) then
  //  DeleteFileUTF8(unitFilename);
end;

function TConnectionUtils.DecodeMessageFromGameHelper(const aMess: string): boolean;
var o: TResponsItem;
begin
  Result := False;
  try
    o.LoadFieldsFromString(aMess);
    case o.ResponsType of
      rtUnknown: ShowMessageInForm('Unknown message...');
      rtAddUnitToProject: DoAddUnitToProject(o);
      rtRemoveUnitFromProject: DoRemoveUnitFromProject(o);
      rtTerminated: Result := True;
    end;
    LazarusIDE.DoSaveAll([]);
    Application.ProcessMessages;
  except
  end;
end;

procedure TConnectionUtils.DoProcessOGLCGameHelperMenuClick(Sender: TObject);
var oglcPackage: TIDEPackage;
  oglcSourcePath, gamehelperBinaryFolder, gamehelperexecutable: string;
  currentLazarusProjectFilename, currentOGLCProjectFilename: string;

{  k: integer;
  Buffer, messageLine: string;
  ByteRead: byte;
  flagQuit: boolean; }
begin
  IDEMessagesWindow.Clear;

  // before opening Game Helper, we do some checks:
  // 1) check if there is currently a project opened in the IDE
  if LazarusIDE.ActiveProject = NIL then begin
    ShowMessage('There is no project currently opened in the IDE'+LineEnding+
     'Please, first open an OGLCScene project or create a new one');
    exit;
  end;

  // 2) check if this current Lazarus project is an OGLCScene project
  if not ConnectionUtils.CurrentLazarusProjectIsOGLCSceneProject then begin
    ShowMessage('The current IDE project is not an OGLCScene project.'+LineEnding+
        'Game Helper can not be started');
    exit;
  end;

  // retrieve the path to source of OGLCScene package
  oglcPackage := PackageEditingInterface.FindPackageWithName('OGLCScenePackage');
  if oglcPackage = NIL then begin
    ShowMessage('OGLCScenePackage '+sNotFoundInTheIDE);
    exit;
  end;
  oglcSourcePath := oglcPackage.DirectoryExpanded;
  // retrieve the Game Helper executable
  gamehelperBinaryFolder := CleanAndExpandDirectory(oglcSourcePath+'..'+PathDelim+'tools'+PathDelim+'GameHelper'+PathDelim+
                            'Binary'+PathDelim);
{$ifdef Windows}
  gamehelperexecutable := gamehelperBinaryFolder+'GameHelper.exe';
{$else}
  gamehelperexecutable := gamehelperBinaryFolder+'GameHelper';
{$endif}

  // 3) check if the Game Helper executable exists (in other words if Game Helper was compiled by user)
  if not FileExists(gamehelperexecutable) then begin
    ShowMessage(sGameHelperNotFoundInOrderTo);
    exit;
  end;

  // save all source editor to disk
  LazarusIDE.DoSaveAll([]);
  Application.ProcessMessages;

  // retrieve the path of the current project opened in the IDE
  currentLazarusProjectFilename := ConnectionUtils.GetActiveLazarusProjectFilename;

  // if there isn't a OGLC project, create an empty one
  currentOGLCProjectFilename := ChangeFileExt(currentLazarusProjectFilename, '.oglc');
  if not FileExistsUTF8(currentOGLCProjectFilename) then begin
    Showmessage('Project OGLC not found, creating an empty one');
    SaveAnEmptyOGLCProject(currentOGLCProjectFilename);
  end;

  // make a copy of the oglc files in the backup directory
  ConnectionUtils.BackupOGLCFiles;

  // open the modal window
  FormGameHelperConnexion := TFormGameHelperConnexion.Create(NIL);
  try
    FormGameHelperConnexion.SetParameters(gamehelperExecutable, currentLazarusProjectFilename);
    if FormGameHelperConnexion.ShowModal = mrOk
      then ConnectionUtils.ShowMessageInIDE('Game Helper exited')
      else ConnectionUtils.ShowMessageInIDE('Connection with Game Helper canceled', mluWarning);
  finally
    FormGameHelperConnexion.Free;
    FormGameHelperConnexion := NIL;
   end;
end;

procedure TConnectionUtils.SaveAnEmptyOGLCProject(const aFilename: string);
var t: TStringList;
begin
  t := TStringList.Create;
  try
    t.Add('[CONFIG]');
    t.Add('SceneWidth|1024|SceneHeight|768|MaximizeScene|true|LevelEditorOverlap|0.5000|CommonShowFlyingTxt|true');
    t.SaveToFile(aFilename);
  finally
    t.Free;
  end;
end;

procedure TConnectionUtils.BackupOGLCFiles;
var oglcProjectFilename, oglcPath, backupPath: string;
  t: TStringList;
  i: integer;
begin
  // backup the <project>.oglc
  oglcProjectFilename := ChangeFileExt(GetActiveLazarusProjectFilename, '.oglc');
  backupPath := GetActiveProjectPath + 'backup' + PathDelim;
  CopyFile(oglcProjectFilename, backupPath+ExtractFileName(oglcProjectFilename), [cffOverwriteFile]);

  // now backup all files found in sub-folder GameHelperFiles\
  oglcPath := GetFolderGameHelperFiles;
  backupPath := oglcPath + 'backup' + PathDelim;
  // if needed create the backup directory
  ForceDirectoriesUTF8(backupPath);
  // get only files
  t := GetDirectoryContent(oglcPath, [], True, False);
  // copy them in backup directory
  for i:=0 to t.Count-1 do
    CopyFile(oglcPath+t.Strings[i], backupPath+t.Strings[i], [cffOverwriteFile]);
end;

procedure TConnectionUtils.ShowMessageInIDE(const aMess: string; aUrgency: TMessageLineUrgency);
begin
  IDEMessagesWindow.AddCustomMessage(aUrgency, aMess);
end;

procedure TConnectionUtils.ShowMessageInForm(const aMess: string);
begin
  FormGameHelperConnexion.ShowMessage(aMess);
end;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
begin
  // register shortcut Alt+H to open game helper window
  Key := IDEShortCut(VK_H,[ssAlt],VK_UNKNOWN,[]);
  Cat := IDECommandList.FindCategoryByName(CommandCategoryToolMenuName); //    CommandCategoryTextEditingName
  RunOGLCSceneGameHelperCmd := RegisterIDECommand(Cat, 'Run OGLCScene Game Helper', sRunGameHelper, Key,
    NIL, @ProcessOGLCSceneGameHelperMenuClick);

  // add a menu item in the Tool Menu
  RegisterIDEMenuCommand(itmCustomTools, 'MIRunGameHelper',
    sRunGameHelper, nil, nil, RunOGLCSceneGameHelperCmd, '');

  // register the window creator
  //IDEWindowCreators.Add(GameHelperConnexionWindowName, @CreateIDEWindowGameHelperConnexion,
  //                      NIL, ''{'400'}, ''{'400'}, '', '');
end;

{procedure CreateIDEWindowGameHelperConnexion(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if CompareText(aFormName, GameHelperConnexionWindowName) <> 0 then exit;
  IDEWindowCreators.CreateForm(FormGameHelperConnexion, TFormGameHelperConnexion,
                               DoDisableAutoSizing, LazarusIDE.OwningComponent);
  AForm := FormGameHelperConnexion;
end; }


end.

