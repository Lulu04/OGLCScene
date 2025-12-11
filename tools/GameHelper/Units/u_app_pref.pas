unit u_app_pref;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, OGLCScene;

function PPIScale(AValue: integer): integer;
function ScaleW(AValue: integer): integer;
function ScaleH(AValue: integer): integer;

function GetDataFolder: string;
function GetHandleFolder: string;
function GetCursorFolder: string;
function GetIconFolder: string;
function GetSourceProjectTemplateFolder: string;


type

{ TAppPref }

TAppPref = class(TOGLCSaveDirectory)
private
  FLastProjectFilename: string;

  procedure SetLastProjectFilename(AValue: string);
public
  procedure Save;
  procedure Load;

  property LastProjectFilename: string read FLastProjectFilename write SetLastProjectFilename;
end;

var
  AppPref: TAppPref;

implementation

uses u_common, Forms;

function PPIScale(AValue: integer): integer;
begin
  Result := FScene.ScaleDesignToScene(AValue);
  //Result := Round(FScene.ScaleDesignToScene(AValue)*0.8);
end;

function ScaleW(AValue: integer): integer;
begin
  Result := Round(FScene.Width*AValue/1024{*AdditionnalScale});
end;

function ScaleH(AValue: integer): integer;
begin
  Result := Round(FScene.Height*AValue/768{*AdditionnalScale});
end;

function GetDataFolder: string;
begin
  Result := Application.Location+DirectorySeparator+'Data'+DirectorySeparator;
end;

function GetHandleFolder: string;
begin
  Result := GetDataFolder+'Handle'+DirectorySeparator;
end;

function GetCursorFolder: string;
begin
  Result := GetDataFolder+'Cursor'+DirectorySeparator;
end;

function GetIconFolder: string;
begin
  Result := GetDataFolder+'Icon'+DirectorySeparator;
end;

function GetSourceProjectTemplateFolder: string;
begin
  Result := GetDataFolder+'ProjectTemplate'+DirectorySeparator;
end;

{ TAppPref }

procedure TAppPref.SetLastProjectFilename(AValue: string);
begin
  if FLastProjectFilename = AValue then Exit;
  FLastProjectFilename := AValue;
  Save;
end;

procedure TAppPref.Save;
var t: TStringList;
  prop: TProperties;
begin
  if not FolderCreated then exit;

  t := TStringList.Create;
  try
    prop.Init('|');
    prop.Add('LastProjectFilename', FLastProjectFilename);

    t.Add(prop.PackedProperty);
    t.SaveToFile(SaveFolder+'config.cfg');
  finally
    t.Free;
  end;
end;

procedure TAppPref.Load;
var t: TStringList;
  prop: TProperties;
  f: string;
begin
  if not FolderCreated then exit;
  f :=  SaveFolder+'config.cfg';
  if not FileExists(f) then exit;

  t := TStringList.Create;
  try
    t.LoadFromFile(SaveFolder+'config.cfg');
    if t.Count = 0 then exit;
    prop.Split(t.Strings[0], '|');

    prop.StringValueOf('LastProjectFilename', FLastProjectFilename, '');
  finally
    t.Free;
  end;
end;

end.

