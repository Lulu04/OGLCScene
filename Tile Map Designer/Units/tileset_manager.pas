unit tileset_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stdctrls,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene;

type

{ TTileSet }

TTileSet = class
private
  FImage: TBGRABitmap;
  FTileHeight, FTileWidth: integer;
  FMaxIndex: integer;
  FName: string;
  function GetXTileCount: integer;
//  function GetTile(AIndex: integer): TBGRABitmap;
  function GetTileCount: integer;
  function GetYTileCount: integer;
public
  Constructor Create( AFilename: string; ATileWidth, ATileHeight: integer );
  destructor Destroy; override;

  property WholeImage: TBGRABitmap read FImage;
//  property Tile[index: integer]: TBGRABitmap read GetTile; default;
  property TileWidth: integer read FTileWidth;
  property TileHeight: integer read FTileHeight;
  property TileCount: integer read GetTileCount;
  property XTileCount: integer read GetXTileCount;
  property YTileCount: integer read GetYTileCount;
  property Name: string read FName;
end;

{ TTilesetManager }

TTilesetManager = class
private
  FList: TList;
  function GetCount: integer;
  function GetTileSet( AIndex: integer ): TTileSet;
public
  constructor Create;
  destructor Destroy; override;
  procedure LoadTileSetFromMapFile( const AFilename: string );

  procedure Add( AFilename: string; ATileWidth, ATileHeight: integer );
  procedure Remove( AIndex: integer );
  procedure Clear;
  procedure FillComboBox( Acb: TComboBox );
  function IsValidTileSetIndex( ATileSetIndex: integer ): boolean;

  property Count: integer read GetCount;
  property TileSet[index: integer]: TTileSet read GetTileSet;
end;

var TileSetManager: TTilesetManager;

implementation
uses uAskGroundType,
     uAskEventValue;

{ TTilesetManager }

function TTilesetManager.GetCount: integer;
begin
 Result := FList.Count;
end;

function TTilesetManager.GetTileSet(AIndex: integer): TTileSet;
begin
 if ( AIndex < 0 ) or ( AIndex > FList.Count-1 )
   then begin result:=nil;exit;end; //raise Exception.Create('invalid tile index...');
 Result := TTileSet( FList.Items[AIndex] );
end;

constructor TTilesetManager.Create;
begin
 FList := TList.Create;
end;

destructor TTilesetManager.Destroy;
begin
 Clear;
 FreeAndNil( FList );
 inherited Destroy;
end;

procedure TTilesetManager.LoadTileSetFromMapFile(const AFilename: string);
var temp : TStringList;
    path: string;
    i, k, TexCount : integer;
    txtsplited: ArrayOfString;
begin
 Clear;
 temp := TStringList.Create;
 temp.LoadFromFile( AFilename );
 path := ExtractFilePath( AFilename );

 // load map's textures
 k := temp.IndexOf('TEXTURES');
 if k<>-1 then begin
   inc(k);
   TexCount := strtoint( temp.Strings[k] );
   inc(k);
   for i:=0 to TexCount-1 do
    begin
     txtsplited := SplitLineToStringArray( temp.Strings[k], '|' );
     if Length( txtsplited ) <> 3
       then raise exception.Create('Tileset - Load error : can''t retrieve texture name and frame size');
     Add( path + txtsplited[0], strtoint( txtsplited[1] ), strtoint( txtsplited[2] ) );
     inc(k);
    end;
 end;

 // load ground types
 Form_AskGroundType.LB.Clear;
 k := temp.IndexOf('GROUND_NAMES');
 if k<>-1 then begin
   txtsplited := SplitLineToStringArray( temp.Strings[k+1], '|' );
   for i:=0 to Length( txtsplited )-1 do
    Form_AskGroundType.LB.Items.Add( txtsplited[i] );
 end;

 // load Events names
 Form_AskEvent.LB.Clear;
 k := temp.IndexOf('EVENT_NAMES');
 if k<>-1 then begin
   txtsplited := SplitLineToStringArray( temp.Strings[k+1], '|' );
   for i:=0 to Length( txtsplited )-1 do
    Form_AskEvent.LB.Items.Add( txtsplited[i] );
 end;

 temp.Free;
end;

procedure TTilesetManager.Add(AFilename: string; ATileWidth, ATileHeight: integer);
var o: TTileSet;
begin
 o := TTileSet.Create( AFilename, ATileWidth, ATileHeight );
 FList.Add( o );
end;

procedure TTilesetManager.Remove(AIndex: integer);
begin
 TileSet[AIndex].Free;
 FList.Delete(AIndex);
end;

procedure TTilesetManager.Clear;
begin
 while Count > 0 do Remove( 0 );
end;

procedure TTilesetManager.FillComboBox(Acb: TComboBox);
var i: integer;
begin
 Acb.Clear;
 for i:=0 to Count-1 do
  Acb.Items.Add( TileSet[i].Name );
end;

function TTilesetManager.IsValidTileSetIndex(ATileSetIndex: integer): boolean;
begin
 Result := ( ATileSetIndex > -1 ) and ( ATileSetIndex < FList.Count );
end;

{ TTileSet }

{
 function TTileSet.GetTile(AIndex: integer): TBGRABitmap;
 var xx, yy: integer;
 begin
  if (AIndex < 0) or (AIndex > FMaxIndex )
    then raise Exception.Create('invalid tile index...');

  xx := 0;
  yy := 0;
  while AIndex > 0 do begin
    inc( xx, FTileWidth );
    if xx >= FImage.Width then begin
      xx := 0;
      inc( yy, FTileHeight );
    end;
    dec( AIndex );
  end;

  Result := FImage.GetPart( Rect( xx, yy, xx+FTileWidth-1, yy+FTileHeight-1) ) as TBGRABitmap;
 end;
}

function TTileSet.GetTileCount: integer;
begin
 Result := GetXTileCount * GetYTileCount;
end;

function TTileSet.GetXTileCount: integer;
begin
 Result := FImage.Width div TileWidth;
end;

function TTileSet.GetYTileCount: integer;
begin
 Result := FImage.Height div TileHeight;
end;

constructor TTileSet.Create(AFilename: string; ATileWidth, ATileHeight: integer );
begin
 FImage := TBGRABitmap.Create( AFilename );
 FTileWidth := ATileWidth;
 FTileHeight := ATileHeight;
 FMaxIndex := ( FImage.Width div FTileWidth ) * ( FImage.Height div FTileHeight ) - 1;
 if ( FMaxIndex < 0 ) or
    ( FImage.Width mod FTileWidth <> 0 ) or
    ( FImage.Height mod FTileHeight <> 0 )
 then Raise Exception.Create('Error while loading tileset '+AFilename+'. Tile size not match with image size...');
 FName := ExtractFileName( AFilename );
end;

destructor TTileSet.Destroy;
begin
 FreeAndNil( FImage );
 inherited Destroy;
end;


end.

