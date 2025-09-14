unit utilitaire_fichier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ComCtrls,
  LazFileUtils,
  Zipper;
 // {$IFDEF Windows},Shlobj{$ENDIF};

//-------------------------------------
//             FICHIER
//-------------------------------------
function FichierExistant( const aFichier: string ): boolean;
// ne fonctionne pas !!!
function DeplaceFichier(const aSrc, aDest: string; aEcraserExistant : boolean=TRUE ): boolean;
function CopieFichier(const aSrc, aDest: string; aEcraserExistant: boolean=TRUE): boolean;
function SupprimeFichier(const aFichier: string): boolean;
function RenommeFichier(const aAncien, aNouveau: string): boolean;
// recherche un fichier dans un répertoire et ses sous-répertoire
// renvoi le fichier avec son chemin, ou '' si pas trouvé
function ChercheUnFichier(const aRepertoireDeBase, Fichier: TFileName): string;

// déplace un fichier vers la corbeille
function MoveFilesToTrash(const AFilenamesUTF8: array of string): boolean;

//-------------------------------------
//             REPERTOIRE
//-------------------------------------
function RepertoireExistant(const aRepertoire: string): boolean;
function CreerRepertoire(const aRepertoire: string): boolean;
function SupprimeRepertoire( const aRepertoire: string): boolean ;
function RenommerRepertoire(const aAncien, aNouveau: string): boolean;
// exemple: 'C:\Lazarus\Poire\'  renvoi 'Poire\'
function NomDuDernierSousRepertoire(const aRepertoire: string): string;
function RepertoireParent( const aRepertoire: string ): string;
function RepertoireEstVide(const aRepertoire: string): boolean;

// Renvoie true si 's' est un répertoire
function IsFolder(const s: string): boolean;
// Renvoie True si 's' est un fichier
function IsFile(const s: string): boolean;

// efface les fichiers contenus dans le répertoire
// aMaskFichier: '.exe' pour ne filtrer que les exécutables , '' pour tous
procedure EffaceLesFichiersDuRepertoire ( const aRepertoire , aMaskFichier : string ) ;

// supprimme les fichiers et les sous-répertoires d'un répertoire
procedure VideLeRepertoire ( const aRepertoire : string ) ;

// renvoi la liste des fichiers, répertoires et sous-répertoire, contenu dans un répertoire
function ContenuDuRepertoire ( aANSIRepertoire , aMaskFichier : string ; aListerLesRepertoires , aListerLesSousRepertoires : boolean ) : TStringList ;
function ContenuDuRepertoire(const aFolder: string;
                             const aMasksExt: TStringArray; //NIL pour tous les fichiers ou ['.doc', '.exe',...]
                             aListerLesRepertoires,
                             aListerLesSousRepertoires: boolean): TStringList;
// Gives the content of the given directory
function GetDirectoryContent(const aDirectoryPath: string;
                             const aMasksExt: TStringArray; //NIL for all files or ['.doc', '.exe',...]
                             aListSubFolder: boolean; // set to True to include sub-folder in the list
                                                      // if False, aListSubFolderUntilLevel is ignored
                             aListSubFolderUntilLevel: integer=0) // 0 means explore only 'aDirectoryPath'
                                                                  // 1 means explore one level of sub-folder of 'aDirectoryPath'
                                                                  // 2 means explore two levels of sub-folder. and so on...
                             : TStringList;

// copie de répertoire entier
// exemple: CopieRepertoire('C:\Lazarus\Poire\', 'D:\Cible',...) va créer le sous-répertoire D:\Cible\Poire
// puis y copier tous les fichiers/répertoires trouvés dans Poire.
// Le répertoire 'Cible' doit être existant.
procedure CopieRepertoire (const aSrc , aDest : string ; aCopierLesSousRepertoires , aEcraserLesFichiersExistants : boolean ) ;

// remplit un TREE VIEW avec le contenu d'un répertoire
// aFiltre est un tableau contenant tous les filtres des fichiers qu'on veut garder. ex : ['.exe','.txt']. [] pour tous les fichiers
// pour avoir tous les fichiers, mettre aFiltre = []
procedure RemplitTTreeViewAvecUnRepertoire(const aRepertoire: string;
                                           aTV: TTreeView;
                                           aFiltre: array of string;
                                           const aTVRootNodeName: string='Bibliothèque');

//---------------------------------------
//                CHEMIN
//---------------------------------------

// returns the parent path
// ex: from '.../Folder1/SubFolder'  returns  '.../Folder1/'
function ParentPath( aPath:string ) : string ;


{$IFDEF Windows}
// Accès aux répertoires spéciaux de windows  renvoi des ANSIString
function RepertoireWindowsAllUsersApplicationData: string;
function RepertoireDeWindows: string;
function RepertoireSysteme: string;
function ReperoireAllUsersMesDocuments: string;
function RepertoireUserMesDocuments: string;

function RepertoireSpecial(aWindowsCSID:longint): string;
{$ENDIF}




//-----------------------------------
//         ZIP
//-----------------------------------

// ZIPPE un répertoire dans une Archive
procedure ZipLeRepertoire(const aRepertoire, aFichierDeSortie: string; aZipperLesSousRepertoires: boolean);

// UNZIPPE une Archive dans un répertoire
// aRepertoire doit être existant
procedure UnzipLeFichier(const aFichierArchive, aRepertoire: string);



implementation
{$if Defined(Darwin) or Defined(Linux)}
uses Process,
{$endif}
{$ifdef Windows}
uses Shlobj, ShellApi, Windows,
{$endif}
 LazUTF8, ShellCtrls;


//-------------------------------------
//             FICHIER
//-------------------------------------

function FichierExistant(const aFichier: string): boolean;
begin
 Result := LazFileUtils.FileExistsUTF8( aFichier );
end;

function DeplaceFichier(const aSrc, aDest: string; aEcraserExistant: boolean ): boolean;
var s, f : string ;
begin
 // on rajoute le nom du fichier avec son extension à 'aDest'
 f := ExtractFileName(aSrc);
 if f = ExtractFileName(aDest)
   then s := aDest
   else s := ConcatPaths([aDest, f]);
 Result := CopieFichier(aSrc, s, aEcraserExistant);
 if Result then SupprimeFichier(aSrc);
end;

function CopieFichier(const aSrc, aDest: string; aEcraserExistant: boolean ): boolean;
var flag: TCopyFileFlags;
begin
  flag := [cffPreserveTime];
  if aEcraserExistant then
    flag += [cffOverwriteFile];
  Result := FileUtil.CopyFile( aSrc, aDest, Flag, FALSE );
end;

function SupprimeFichier(const aFichier: string): boolean;
begin
  Result := LazFileUtils.DeleteFileUTF8 ( aFichier );
end;

function RenommeFichier(const aAncien, aNouveau: string): boolean;
begin
  Result := LazFileUtils.RenameFileUTF8 ( aAncien , aNouveau );
end;

function ContenuDuRepertoire(aANSIRepertoire, aMaskFichier: string;
  aListerLesRepertoires, aListerLesSousRepertoires: boolean): TStringList;
var A: TStringArray;
begin
  A := NIL;
  if aMaskFichier <> '' then begin
   SetLength(A, 1);
   A[0] := aMaskFichier;
  end;

  Result := ContenuDuRepertoire(aANSIRepertoire, A, aListerLesRepertoires, aListerLesSousRepertoires);
end;

function ContenuDuRepertoire(const aFolder: string; const aMasksExt: TStringArray;
  aListerLesRepertoires, aListerLesSousRepertoires: boolean): TStringList;
  function FileHaveOneOfMaskExt(const afile: string): boolean;
  var i: integer;
  begin
   Result := True;
   if Length(aMasksExt) = 0 then exit;

   for i:=0 to High(aMasksExt) do
     if UpCase( ExtractFileExt( afile )) = UpCase(aMasksExt[i]) then exit;
   Result := False;
  end;

  procedure ScruteLeDossier(Dossier: string; aSousDossier: string; T: TStringList);
  var
    Sr: TSearchRec;
    sd: string;
  begin
   if LazFileUtils.FindFirstUTF8( Dossier + string(DIRECTORYSEPARATOR + '*') , faAnyFile , Sr ) = 0 then begin
     repeat
      if (Sr.Attr and faDirectory)=faDirectory then begin
        if ( not((Sr.Name = '.') or (Sr.Name = '..')) ) and
           aListerLesRepertoires then begin
          // on a trouvé un répertoire
          if aSousDossier = ''
            then sd := Sr.Name
            else sd := aSousDossier + DIRECTORYSEPARATOR + Sr.Name ;
          T.Add ( sd {+ DIRECTORYSEPARATOR} );
          if aListerLesSousRepertoires then
            ScruteLeDossier( Dossier + DIRECTORYSEPARATOR + Sr.Name, sd, T );
        end;
      end else if FileHaveOneOfMaskExt( Sr.Name ) then begin
                 // on a trouvé un fichier
                 if aSousDossier = ''
                   then T.Add(Sr.Name)
                   else T.Add(aSousDossier + DIRECTORYSEPARATOR + Sr.Name);
      end;
     until LazFileUtils.FindNextUTF8(Sr) <> 0;
   end;
   LazFileUtils.FindCloseUTF8( Sr );
  end;
begin
 Result := TStringList.Create;
 ScruteLeDossier(aFolder, '', Result); // on lance la recherche récursive
end;

function GetDirectoryContent(const aDirectoryPath: string;
  const aMasksExt: TStringArray; aListSubFolder: boolean;
  aListSubFolderUntilLevel: integer): TStringList;
var currentLevel: integer;
  function FileHaveOneOfMaskExt(const afile: string): boolean;
  var i: integer;
  begin
   Result := True;
   if Length(aMasksExt) = 0 then exit;

   for i:=0 to High(aMasksExt) do
     if UpCase( ExtractFileExt( afile )) = UpCase(aMasksExt[i]) then exit;
   Result := False;
  end;
  procedure ScanFolder(aFolder: string; aSubFolder: string; aSL: TStringList);
  var
    Sr: TSearchRec;
    sd: string;
  begin
   inc(currentLevel);
   if LazFileUtils.FindFirstUTF8(aFolder + string(DIRECTORYSEPARATOR + '*') , faAnyFile , Sr ) = 0 then begin
     repeat
      if (Sr.Attr and faDirectory)=faDirectory then begin
        if ( not((Sr.Name = '.') or (Sr.Name = '..')) ) and
           aListSubFolder then begin
          // we find a folder
          if aSubFolder = ''
            then sd := Sr.Name
            else sd := aSubFolder + DIRECTORYSEPARATOR + Sr.Name;
          aSL.Add( sd );
          if currentLevel <= aListSubFolderUntilLevel then
            ScanFolder(aFolder + DIRECTORYSEPARATOR + Sr.Name, sd, aSL);
        end;
      end else if FileHaveOneOfMaskExt( Sr.Name ) then begin
                 // we find a file
                 if aSubFolder = ''
                   then aSL.Add(Sr.Name)
                   else aSL.Add(aSubFolder + DIRECTORYSEPARATOR + Sr.Name);
      end;
     until LazFileUtils.FindNextUTF8(Sr) <> 0;
   end;
   LazFileUtils.FindCloseUTF8( Sr );
   dec(currentLevel);
  end;
begin
  if aListSubFolderUntilLevel < 0 then aListSubFolderUntilLevel := 0;
  currentLevel := 0;
  Result := TStringList.Create;
  ScanFolder(aDirectoryPath, '', Result); // start recursive search
end;



// copie de répertoire
procedure CopieRepertoire(const aSrc, aDest: string; aCopierLesSousRepertoires , aEcraserLesFichiersExistants : boolean);
var t : TStringList ;
  i : integer ;
  d : string ;
begin
 d := NomDuDernierSousRepertoire ( aSrc ) ;
 if not RepertoireExistant(aDest + d ) then CreerRepertoire( aDest + d);
 t := ContenuDuRepertoire ( aSrc , '' , aCopierLesSousRepertoires , aCopierLesSousRepertoires ) ;
 for i:=0 to t.Count-1 do
   begin
    if FichierExistant( aSrc+t.Strings[i] )
      then begin // on doit copier un fichier
            if aEcraserLesFichiersExistants
              then begin    // on écrase
                    CopieFichier( aSrc+t.Strings[i] , aDest+d+t.Strings[i], TRUE) ;
                   end
              else begin
                    if not FichierExistant( aDest+d+t.Strings[i] ) // on n'écrase pas
                      then CopieFichier( aSrc+t.Strings[i] , aDest+d+t.Strings[i], TRUE) ;
                   end;
           end
      else begin // on doit copier un répertoire
            if aEcraserLesFichiersExistants
              then begin    // on écrase
                    CreerRepertoire( aDest+d+t.Strings[i] ) ;
                   end
              else begin
                    if not RepertoireExistant( aDest+d+t.Strings[i] ) // on n'écrase pas
                      then CreerRepertoire( aDest+d+t.Strings[i] ) ;
                   end;
           end;
   end;
 t.Free ;
end;

procedure RemplitTTreeViewAvecUnRepertoire(const aRepertoire: string;
  aTV: TTreeView; aFiltre: array of string; const aTVRootNodeName: string);
  function ExtensionFichierOk( aExt: string ): boolean;
  var j:integer;
  begin
   Result := FALSE ;
   if Length( aFiltre ) = 0 then
     Result := TRUE
    else begin
      for j:=0 to Length(aFiltre)-1 do
        Result := (UpCase(aExt) = UpCase (aFiltre[j])) or Result;
    end;
  end;
  procedure ScruteLeDossier(Dossier: string; aNode: TTreeNode);
  var
    sr: TSearchRec;
    n : TTreeNode;
  begin
   if LazFileutils.FindFirstUTF8( Dossier + DirectorySeparator + '*' , faAnyFile , sr ) = 0
     then begin
           repeat
            if Sr.Attr and faDirectory > 0
              then begin
                    if not((Sr.Name = '.') or (Sr.Name = '..'))
                     then begin
                           // on a trouvé un répertoire
                           n := aTV.Items.AddChild(aNode, UpCase(Sr.Name));
                           // initialiser les icones d'un répertoire
                           n.SelectedIndex := 2;  // icone d'un dossier fermé
                           n.ImageIndex := 1;     // icone d'un dossier ouvert
                           n.MakeVisible;
                           n.Collapse(True);
                           ScruteLeDossier( Dossier + DirectorySeparator + Sr.Name, n );
                          end;
                   end
              else if ExtensionFichierOk(sr.Name)
                     then begin
                           // on a trouvé un fichier matériel
                           n := aTV.Items.AddChild(aNode, ChangeFileExt(Sr.Name,''));
                           // initialiser les icones d'un fichier
                           n.SelectedIndex := 3;
                           n.ImageIndex := 3;
                           n.MakeVisible;
                          end;
           until LazFileutils.FindNextUTF8(Sr) <> 0;
          end;
   LazFileutils.FindCloseUTF8(Sr);
  end;

begin
 aTV.BeginUpdate;
 aTV.Items.Clear; // on vide le treeview
 with aTV.Items.AddFirst(NIL, aTVRootNodeName) do
   begin
    ImageIndex := 0;
    SelectedIndex := 0;
   end;
 ScruteLeDossier(aRepertoire, aTV.Items.GetFirstNode); // on lance la recherche récursive
 aTV.EndUpdate;
end;

function ParentPath(aPath: string): string;
begin
 Result := IncludeTrailingPathDelimiter(ExtractFilePath( ExcludeTrailingPathDelimiter( aPath )));
end;


{$IFDEF Windows}
function RepertoireWindowsAllUsersApplicationData: string;
begin
 Result := RepertoireSpecial(CSIDL_COMMON_APPDATA);
end;

// répertoires de windows
function RepertoireDeWindows: string;
begin
 Result := RepertoireSpecial(CSIDL_WINDOWS);
end;

function RepertoireSysteme: string;
begin
 Result := RepertoireSpecial(CSIDL_SYSTEM);
end;

function ReperoireAllUsersMesDocuments: string;
begin
 Result := RepertoireSpecial(CSIDL_COMMON_DOCUMENTS);
end;

function RepertoireUserMesDocuments: string;
begin
 Result := RepertoireSpecial(CSIDL_PERSONAL);
end;

function RepertoireSpecial(aWindowsCSID: longint): string;
var SpecialPath: Array[0..MAX_PATH] Of Char;
begin
 SHGetFolderPath(0, aWindowsCSID or CSIDL_FLAG_CREATE, 0, 0, SpecialPath{%H-});
 Result := IncludeTrailingPathDelimiter(StrPas(SpecialPath));
end;
{$ENDIF}



// recherche un fichier dans un dossier et ses sous-dossiers
// renvoi le fichier avec son chemin, ou '' si pas trouvé
function ChercheUnFichier ( const aRepertoireDeBase, Fichier : TFileName ) : string ;
var
  sr: TSearchRec;
begin
 Result := '' ;
 if LazFileutils.FindFirstUTF8( aRepertoireDeBase + DIRECTORYSEPARATOR + '*'  , faAnyFile , sr ) = 0
   then begin
         repeat
          if Sr.Attr and faDirectory > 0
            then begin
                  if not ( ( Sr.Name = '.' ) or ( Sr.Name = '..' ) )
                   then begin
                         // répertoire
                         Result := ChercheUnFichier ( aRepertoireDeBase +  DIRECTORYSEPARATOR + Sr.name, Fichier ) ;
                         if Result <> '' then exit ;
                        end;
                 end
            else if sr.Name = Fichier
                   then begin
                         Result := aRepertoireDeBase + DIRECTORYSEPARATOR + sr.Name ;
                         exit ;
                        end;
         until LazFileutils.FindNextUTF8 ( Sr ) <> 0 ;
        end;
 LazFileutils.FindCloseUTF8 ( Sr ) ;
end;


// Thanks to Circular for the routine MoveToTrash on different platforms.
// from LazPaint - unit ufilesystem.pas

{$ifdef Windows}
type
  {$PUSH}{$PACKRECORDS C}
  SHFILEOPSTRUCTW = record
     hwnd : HWND;
     wFunc : UINT;
     pFrom : LPCWSTR;
     pTo : LPCWSTR;
     fFlags : FILEOP_FLAGS;
     fAnyOperationsAborted : WINBOOL;
     hNameMappings : LPVOID;
     lpszProgressTitle : LPCWSTR;
  end;
  {$POP}
function SHFileOperationW(Var para1: SHFILEOPSTRUCTW):longint; stdcall; external 'shell32' name 'SHFileOperationW';

function MoveFilesToTrash({AForm: TForm;} const AFilenamesUTF8: array of string): boolean;
const FOF_ALLOWUNDO = $40;
      FO_DELETE = 3;
var
  struct: SHFILEOPSTRUCTW;
  errorCode: longint;
  filenamesW: unicodestring;
  i: Integer;
begin
  filenamesW := '';
  for i := 0 to high(AFilenamesUTF8) do
    filenamesW += UTF8ToUTF16(AFilenamesUTF8[i]) + #0; //this is a list of filenames, it is double terminated

  struct.hwnd := 0;// AForm.Handle;

  struct.wFunc := FO_DELETE;
  struct.pFrom := PWideChar(filenamesW);
  struct.pTo := nil;
  struct.fFlags := FOF_ALLOWUNDO;
  struct.lpszProgressTitle := nil;

  struct.fAnyOperationsAborted:= false;
  struct.hNameMappings := nil;
  errorCode := SHFileOperationW(struct);
  if errorCode = 0 then
    result := not struct.fAnyOperationsAborted
  else
    result := false;
end;
{$endif}
{$IFDEF LINUX}
function MoveFilesToTrash(const AFilenamesUTF8: array of string): boolean;
const gvfsTrash = '/usr/bin/gvfs-trash';
      trashPut = '/usr/bin/trash-put';

  function DoTrash(prog: string): boolean;
  var p: TProcess;
    i: integer;
  begin
    result := false;
    try
      p := TProcess.Create(nil);
      p.Executable := prog;
      for i := 0 to high(AFilenamesUTF8) do
        p.Parameters.Add(AFilenamesUTF8[i]);
      p.Options := [poWaitOnExit];
      p.Execute;
      p.Free;
      result := true;
      for i := 0 to high(AFilenamesUTF8) do
        if FileExists(AFilenamesUTF8[i]) then result := false;
    except

    end;
  end;

begin
  if FileExists(gvfsTrash) then result := DoTrash(gvfsTrash)
  else if FileExists(trashPut) then result := DoTrash(trashPut)
  else
    result := false;
end;
{$ENDIF}

{$ifdef Darwin}
function RunAppleScriptLine(AScriptLine: string): boolean;
var
  p: TProcess;
begin
  p := nil;
  try
    p := TProcess.Create(nil);
    p.Executable := 'osascript';
    p.Parameters.Add('-e');
    p.Parameters.Add(AScriptLine);
    p.Options := [poWaitOnExit];
    p.Execute;
    result := true;
  except
    result := false;
  end;
  p.Free;
end;

function AppleScriptEscape(AText: string): string;
begin
  result := StringReplace(AText, '\', '\\', [rfReplaceAll]);
  result := StringReplace(result, '"', '\"', [rfReplaceAll]);
end;

function MoveFilesToTrash(const AFilenamesUTF8: array of string): boolean;
var
  appleScript: String;
  i: Integer;
begin
  if length(AFilenamesUTF8) = 0 then exit(true);
  appleScript := 'tell application "Finder" to delete {';
  for i := 0 to high(AFilenamesUTF8) do
  begin
    if i > 0 then appleScript += ', ';
    appleScript += 'POSIX file "' + AppleScriptEscape(AFilenamesUTF8[i]) + '"';
  end;
  appleScript += '}';
  result := RunAppleScriptLine(appleScript);
end;
{$endif}

function RepertoireExistant(const aRepertoire: string): boolean;
var rep: string;
begin
 rep := LazFileUtils.CleanAndExpandDirectory( aRepertoire );
 Result := LazFileUtils.DirectoryExistsUTF8( rep );
end;

function CreerRepertoire(const aRepertoire: string): boolean;
var rep: string;
begin
 rep := LazFileUtils.CleanAndExpandDirectory( aRepertoire );
 Result := LazFileUtils.CreateDirUTF8 ( rep ) ;
end;

function SupprimeRepertoire(const aRepertoire: string): boolean;
var rep: string;
begin
 rep := LazFileUtils.CleanAndExpandDirectory(aRepertoire);
 VideLeRepertoire( rep );
 Result := LazFileUtils.RemoveDirUTF8( rep );
end;

function IsFolder(const s: string): boolean;
var
  Attr: LongInt;
begin
  Result := RepertoireExistant(s);
  if Result then begin
    Attr := LazFileUtils.FileGetAttrUTF8(s);
    Result := Attr and faDirectory = faDirectory;
  end;
end;

function IsFile(const s: string): boolean;
begin
  Result := FichierExistant(s);
  if Result then
    Result := not IsFolder(s);
end;

// efface les fichiers contenus dans le répertoire
procedure EffaceLesFichiersDuRepertoire(const aRepertoire, aMaskFichier: string); // ex: '.exe' , '' pour tous
var temp: TStringList;
    rep: string;
  i: integer;
begin
 rep := LazFileUtils.CleanAndExpandDirectory(aRepertoire);
 temp := ContenuDuRepertoire(rep, aMaskFichier, FALSE, FALSE);
 for i:=0 to temp.Count-1 do LazFileUtils.DeleteFileUTF8(rep + temp.Strings[i]);
 temp.Free;
end;


// supprimme les fichiers et les sous-répertoires d'un répertoire
procedure VideLeRepertoire(const aRepertoire: string);
 var rep, f: string;
     t: TStringList;
     i: integer;
begin
 rep := LazFileUtils.CleanAndExpandDirectory(aRepertoire);
 t := ContenuDuRepertoire(rep, '', TRUE, TRUE);
 for i:=t.Count-1 downto 0 do    // à l'envers pour d'abord vider les répertoires avant de les supprimer
  begin
   f := rep + t.Strings[i];
   if IsFolder(f) then
   begin   // c'est un répertoire
     VideLeRepertoire(f);
     SupprimeRepertoire(f);
   end
   else SupprimeFichier(f);   // c'est un fichier

  end;
 t.Free ;
end;









// Zippe un répertoire
procedure ZipLeRepertoire(const aRepertoire, aFichierDeSortie: string; aZipperLesSousRepertoires: boolean);
var
 temp: TStringList;
 i: integer;
 zipper: TZipper;
 rep: string;
begin
 rep := CleanAndExpandDirectory(aRepertoire);
 // on créé la liste des fichiers contenu dans le répertoire
 temp := ContenuDuRepertoire(rep, '', aZipperLesSousRepertoires, aZipperLesSousRepertoires);
 zipper := TZipper.Create;
 try
   zipper.FileName := CleanAndExpandFilename(aFichierDeSortie);
   for i:=0 to temp.Count-1 do
     zipper.Entries.AddFileEntry(rep + temp.Strings[i], temp.Strings[i]);
   zipper.ZipAllFiles;
 finally
   zipper.Free;
 end;
 temp.Free;
end;



// UNZIPPE un fichier archive dans un répertoire
procedure UnzipLeFichier(const aFichierArchive, aRepertoire: string);
var
 UnZipper: TUnZipper;
begin
 UnZipper := TUnZipper.Create;
 try
   UnZipper.FileName := CleanAndExpandFilename(aFichierArchive);
   UnZipper.OutputPath := CleanAndExpandDirectory(aRepertoire);
   UnZipper.Examine;
   UnZipper.UnZipAllFiles;
 finally
   UnZipper.Free;
 end;
end;




//-----------------------------------
//         DIVERS
//-----------------------------------

function RenommerRepertoire(const aAncien, aNouveau: string): boolean;
begin
 Result := LazFileUtils.RenameFileUTF8(IncludeTrailingPathDelimiter(aAncien),
                                       IncludeTrailingPathDelimiter(aNouveau));
end;

function NomDuDernierSousRepertoire(const aRepertoire: string): string;
var i : integer ;
 flag : boolean ;
  rep : string;
begin
 rep := ExtractFilePath(aRepertoire);
 Result := '';
 i := Length(rep);
 if i = 0 then exit;
 flag := FALSE;
 repeat
  Result := rep[i] + Result;
  dec ( i ) ;
  if i = 0  then
    flag := TRUE
  else if rep[i] in ['/', '\'] then
    flag := TRUE ;
 until flag;
end;

function RepertoireParent(const aRepertoire: string): string;
begin
 Result := IncludeTrailingPathDelimiter(aRepertoire) + '..' + PathDelim;
 Result := ExpandFileName(Result);
end;

function RepertoireEstVide(const aRepertoire: string): boolean;
var
  t: TStringList;
begin
  t := ContenuDuRepertoire(aRepertoire, NIL, True, False);
  Result := t.Count = 0;
  t.Free;
end;






end.

