unit project_util;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Forms, Controls, FileUtil,
  utilitaire_fichier, LCLTranslator;


ResourceString
  SPromptSave='Before, do you want to save the changes you have made ?';
  SModified = 'modified';

// create and return a folder+sub-folder where you can save data for your application.
// This folder is located according to your OS (see GetAppConfigDirUTF8 documentation)
// Do nothing if it already exists.
// example: Your application name is 'RaceGame' and you call CreateAPPSaveFolder('LazTeam')
//      1 -> the folder 'LazTeam' is created in a appropriate place according to your OS (where you can save data)
//      2 -> the sub-folder 'RaceGame' is created in folder 'LazTeam'
//      3 -> return the absolut path of folder 'RaceGame' terminated by DirectorySeparator
function CreateAPPSaveFolder( const CompanyName: string ): string;


// create and return a folder where you can save data for your application.
// This folder is located according to your OS (see GetAppConfigDirUTF8 documentation)
// Do nothing if it already exists. No sub-folder in this version.
// The returned folder have a trailing path delimiter.
function CreateAppFolder(const aFolderName: string): string;

type

{ TCustomStorageFolder }
//
// this classe is used to easily store files in subfolder of project file path
// and retrieve their relative and absolute path
//  Use: Create( '\Audio Data' );
//  then initialize "AbsoluteBaseFolder" with project folder, and store user files.

TCustomStorageFolder = class
private
  FReady: boolean;
  FAbsoluteBaseFolder: string;
  FStorageSubFolderName: string; // sub-folder name where data will be saved        ex:  /Audio
  procedure SetAbsoluteBaseFolder(AValue: string);
 // Gives the absolute path of the storage folder
 function GetAbsoluteStorageFolder: string;
public
 // aStorageSubFolderName will be relative to 'AbsoluteBaseFolder'
 // ex: .Create( 'Audio' );
 //     .AbsoluteBaseFolder := 'C:\MyProg'; // will create (if not exists) the sub folder 'C:\MyProg\Audio'
 constructor Create( const aStorageSubFolderName: string );

 // Copy a file into the store folder. Overwrite it if already exist
 // aSourceFile is the file to copy with its absolute path
 // aCopiedFile returns the copied file with its absolute path
 // Result is FALSE when the copy fail.
 function StoreFile( const aSourceFile: string; out aCopiedFile: string ): boolean;

 // Copy a file into the store folder. Overwrite it if already exist
 // aSourceFile is the file to copy with its absolute path
 // aTargetFileName is the name of the copied file, without path.
 // aCopiedFile returns the copied file with its absolute path
 // Result is FALSE when the copy fail.
 // This version is usefull when source files contains invalid characters for your application
 function StoreFileExt( const aSourceFile: string; const aTargetFileName: string; out aCopiedFile: string ): boolean;

 // Delete (physically) a file previously stored.
 function DeleteFile( const aStoredFilename: string ): boolean;

 // Check if a file is already in storage folder/sub-folder
 // return TRUE if the file is found.
 function IsStored( const aFilename: string ): boolean;

 // convert absolut/relative path
 function AbsoluteToRelative( const aFilename: string ): string;
 function RelativeToAbsolute( const aFilename: string ): string;

 // folder where files are saved
 property AbsoluteBaseFolder: string read FAbsoluteBaseFolder write SetAbsoluteBaseFolder;

 // Gives the absolute storage folder: AbsoluteBaseFolder + SubFolderName (as in Create)
 property AbsoluteStorageFolder: string read GetAbsoluteStorageFolder;

 // True if the sub-folder is well created on the disk
 property Ready: boolean read FReady;
end;


type

{ TCustomProject }

TCustomProject = class
private
  FAttachedForm: TForm;
  FFormCaption: string;
  FFlagFormCaptionIsMarkedAsModified: boolean;
  FIsReady: boolean;
  FIsNamed: boolean;
  FFilename: string;
  FExtension: string;
  FOD: TOpenDialog;
  FSD: TSaveDialog;
  FHasBeenModified: boolean;
  function GetProjectNameOnly: string;
  function GetProjectNameWithExtension: string;
  procedure SetFileName(AValue: string);
  procedure SetHasBeenModified(AValue: boolean);
  procedure SetTitleForm;
public
  //  ex '.seq'
  Constructor Create( const aProjectFileExtension: string );
  Destructor Destroy; override;
  // here you can specify the main form of your project and its caption.
  // when the project is modified, the string ' - modified' is added to the caption of the main form
  procedure SetFormCaption( aAttachedForm: TForm; const aCaption: string );
  // add one filter to the Open/Save dialogs for your project
  // use as many time you want. bellow an example to add 2 filters: *.* and *.wav
  // AddFilterToDialogs('All file', '*.*');
  // AddFilterToDialogs('Audio files', '*.wav');
  procedure AddFilterToDialogs( aFileType, aFilter: string );

  // abstract method to declare in descendant to do the job
  function DoNew: boolean; virtual; abstract;
  procedure DoSave( const aFilename: string ); virtual; abstract;
  function DoLoad( const aFilename: string ): boolean; virtual; abstract;
  procedure DoClose; virtual; abstract;

  // This method is called when the modified state changed.
  // to update your widget or another job.
  // You can, for example, set SaveItemMenu.Enabled to the value of aState
  procedure OnModifiedChange( aState: boolean ); virtual; abstract;

  // This method is called when the presence of a valid project change:
  // It is called with aState=TRUE after a success on DoNew or DoLoad
  // and aState=FALSE if there is no current project.
  // You can, for example, set CloseItemMenu.Enabled to the value of aState
  procedure OnProjectReadyChange( aState: boolean ); virtual; abstract;

  // From your main program, call only this method
  procedure New;
  procedure Load;
  procedure Load( const aFileName: string );
  procedure Save;
  procedure SaveAs( const aFilename: string );
  // return TRUE if the project have been closed
  // after a save or not, depending of the user choice
  function Close: boolean;

  // If the project is in modified state, ask to the user if he/she want to save before continue.
  // Return FALSE if user has aborted operation, else TRUE if user clicked yes or no.
  function DoUserPromptToSaveProject: boolean;

  // Set the project state to modified (need to be saved before close)
  // override to update your MenuItem.Enabled, etc...
  procedure SetModified; virtual;

  property HasBeenModified: boolean read FHasBeenModified write SetHasBeenModified; // get/set project status
  // TRUE if a project is currently available (created or loaded) and named
  property IsReady: boolean read FIsReady;
  property Filename: string read FFilename write SetFileName; // absolut project path + filename
  property NameWithExtension: string read GetProjectNameWithExtension;
  property NameOnly: string read GetProjectNameOnly; // project filename without extension
end;


implementation
uses LazFileUtils;

var _comp_name_: string;

function MyAppName: string;
begin
 Result := _comp_name_;
end;

function CreateAPPSaveFolder( const CompanyName: string ): string;
begin
 _comp_name_ := CompanyName;
 OnGetApplicationName := @MyAppName;

{$IFDEF WINDOWS}
  Result := IncludeTrailingPathDelimiter( GetAppConfigDirUTF8(TRUE, TRUE) );
{$ELSE}
  Result := IncludeTrailingPathDelimiter( GetAppConfigDirUTF8(FALSE, TRUE) );
{$ENDIF}

 OnGetApplicationName := NIL;
 Result := IncludeTrailingPathDelimiter( ConcatPaths([Result, ApplicationName]));

 if not DirectoryExistsUTF8( Result )
   then CreateDirUTF8( Result );
end;

function CreateAppFolder(const aFolderName: string): string;
begin
 _comp_name_ := aFolderName;
 OnGetApplicationName := @MyAppName;

{$IFDEF WINDOWS}
  Result := IncludeTrailingPathDelimiter( GetAppConfigDirUTF8(TRUE, TRUE) );
{$ELSE}
  Result := IncludeTrailingPathDelimiter( GetAppConfigDirUTF8(FALSE, TRUE) );
{$ENDIF}

 OnGetApplicationName := NIL;
end;



{ TCustomProject }

function TCustomProject.GetProjectNameOnly: string;
begin
 Result := ChangeFileExt( GetProjectNameWithExtension ,'' );
end;

function TCustomProject.GetProjectNameWithExtension: string;
begin
 Result := ExtractFileName( FFileName );
end;

procedure TCustomProject.SetFileName(AValue: string);
begin
 if FFilename=AValue then Exit;
 FFilename:=AValue;
end;

function TCustomProject.DoUserPromptToSaveProject: boolean;
var mr: TModalResult;
begin
 mr := mrYes;
 if FHasBeenModified and FIsReady
  then begin
    mr := MessageDlg('',SPromptSave, mtWarning, [mbYes, mbNo, mbCancel],0);
    if mr = mrYes
     then begin
      if FIsNamed then SaveAs( FFilename )
                  else begin
                        if not FSD.Execute
                         then mr := mrCancel
                         else SaveAs( FSD.FileName );
                  end;
     end;
  end;
 Result := mr <> mrCancel;
 if Result then HasBeenModified:=FALSE;
end;

procedure TCustomProject.SetModified;
begin
 if not FHasBeenModified then begin
   HasBeenModified:=TRUE;
 end;
end;

procedure TCustomProject.SetHasBeenModified(AValue: boolean);
begin
 if FHasBeenModified=AValue then Exit;
 FHasBeenModified:=AValue;
 SetTitleForm;
 OnModifiedChange( AValue );
end;

procedure TCustomProject.SetTitleForm;
var s: string;
begin
 if FAttachedForm=NIL then exit;
 s := FFormCaption;
 if FIsReady then s+='   -   '+GetProjectNameWithExtension;
 if FHasBeenModified then s+='  ('+SModified+')';
 FAttachedForm.Caption:=s;
end;

constructor TCustomProject.Create(const aProjectFileExtension: string);
begin
 FIsReady:=FALSE;
 FIsNamed:=FALSE;
 FFilename:='';
 FExtension:=aProjectFileExtension;
 FAttachedForm := NIL;
 FFlagFormCaptionIsMarkedAsModified := FALSE;
 FOD:= TOpenDialog.Create(NIL);
 FSD:= TSaveDialog.Create(NIL);
 FSD.Options:=[ofOverwritePrompt,ofEnableSizing,ofViewDetail];
 FHasBeenModified:=FALSE;
end;

destructor TCustomProject.Destroy;
begin
 FOD.Free;
 FSD.Free;
 inherited Destroy;
end;

procedure TCustomProject.SetFormCaption(aAttachedForm: TForm; const aCaption: string);
begin
 FAttachedForm:=aAttachedForm;
 FFormCaption:=aCaption;
 SetTitleForm;
end;

procedure TCustomProject.AddFilterToDialogs(aFileType, aFilter: string);
var s: string;
begin
 s := FOD.Filter;
 if s<>'' then s+='|';
 s+= aFileType+'|'+ aFilter;
 FOD.Filter:=s;
 FSD.Filter:=s;
end;

procedure TCustomProject.New;
begin
 if not DoUserPromptToSaveProject then exit;

 if FIsReady then Close;
 FIsNamed := FALSE;
 FFilename:='';
 HasBeenModified:=FALSE;
 FIsReady:=DoNew;
 SetTitleForm;
 OnProjectReadyChange( FIsReady );
end;

procedure TCustomProject.Load;
begin
  if FIsReady then begin
   if not Close then exit;
  end;

  if not FOD.Execute then exit;
  FIsNamed := TRUE;
  FFilename := FOD.FileName;
  FIsReady := DoLoad( FOD.FileName );
  HasBeenModified:=FALSE;
  SetTitleForm;
  OnProjectReadyChange( FIsReady );
end;

procedure TCustomProject.Load(const aFileName: string);
begin
 if FIsReady
   then if not Close
     then exit;
 FIsNamed := TRUE;
 FFilename := aFileName;
 FIsReady := DoLoad( aFileName );
 HasBeenModified:=FALSE;
 SetTitleForm;
 OnProjectReadyChange( FIsReady );
end;

procedure TCustomProject.SaveAs(const aFilename: string);
begin
 FIsNamed := TRUE;
 FFilename:=ChangeFileExt( aFilename, FExtension );
 DoSave( FFilename );
 HasBeenModified:=FALSE;
 SetTitleForm;
end;

procedure TCustomProject.Save;
begin
 if FIsNamed
   then SaveAs( FFilename )
   else if FSD.Execute
          then SaveAs( FSD.FileName );
end;

function TCustomProject.Close: boolean;
begin
 if not DoUserPromptToSaveProject then begin
   Result := FALSE;
   exit;
 end;
 Result := TRUE;
 DoClose;
 FIsNamed := FALSE;
 FFilename:='';
 FIsReady:=FALSE;
 HasBeenModified:=FALSE;
 SetTitleForm;
 OnProjectReadyChange( FIsReady );
end;

{ TCustomStorageFolder }

function TCustomStorageFolder.GetAbsoluteStorageFolder: string;
begin
 Result := ConcatPaths( [FAbsoluteBaseFolder, FStorageSubFolderName] );
 Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TCustomStorageFolder.SetAbsoluteBaseFolder(AValue: string);
begin
  AValue := IncludeTrailingPathDelimiter(AValue);
  if FAbsoluteBaseFolder=AValue then Exit;
  FAbsoluteBaseFolder:=AValue;
  FReady := RepertoireExistant( GetAbsoluteStorageFolder );
  if not FReady
    then FReady:=CreerRepertoire( GetAbsoluteStorageFolder );
end;

constructor TCustomStorageFolder.Create(const aStorageSubFolderName: string);
begin
 inherited Create;
 FStorageSubFolderName := IncludeLeadingPathDelimiter(IncludeTrailingPathDelimiter(aStorageSubFolderName));
end;

function TCustomStorageFolder.StoreFile(const aSourceFile: string; out aCopiedFile: string): boolean;
begin
 if FReady then begin
   try
    aCopiedFile:= ExtractFileName( aSourceFile );
    aCopiedFile := RelativeToAbsolute(aCopiedFile);
    Result := CopieFichier( aSourceFile, aCopiedFile, TRUE );
   except
     Result := FALSE;
   end;
 end else begin
   aCopiedFile:='';
   Result:=FALSE;
 end;
end;

function TCustomStorageFolder.StoreFileExt(const aSourceFile: string;
  const aTargetFileName: string; out aCopiedFile: string): boolean;
begin
 if FReady then begin
   try
    aCopiedFile:=aTargetFileName;
    aCopiedFile:=RelativeToAbsolute(aCopiedFile);
    Result:=CopieFichier(aSourceFile, aCopiedFile, TRUE);
   except
     Result:=FALSE;
   end;
 end else begin
   aCopiedFile:='';
   Result:=FALSE;
 end;
end;

function TCustomStorageFolder.DeleteFile(const aStoredFilename: string): boolean;
var f: string;
begin
 if FReady then begin
   f:=ConcatPaths([AbsoluteStorageFolder, aStoredFilename]);
   Result := SysUtils.DeleteFile( f );
 end else Result:=FALSE;
end;

function TCustomStorageFolder.IsStored(const aFilename: string): boolean;
var n: string;
begin
 if FReady then begin
   n := ExtractFileName( aFilename );
   Result := ChercheUnFichier( GetAbsoluteStorageFolder, n ) <> '';
 end else Result:=FALSE;
end;

function TCustomStorageFolder.AbsoluteToRelative(const aFilename: string): string;
var s,s1: string;
begin
 if FReady then begin
   s := GetAbsoluteStorageFolder;
   s1 := ExtractRelativePath( s, aFilename );
   Result := s1;
 end else Result:='';
end;

function TCustomStorageFolder.RelativeToAbsolute(const aFilename: string): string;
begin
 if FReady
   then Result := ConcatPaths( [GetAbsoluteStorageFolder, aFilename] )
   else Result:='';
end;

end.

