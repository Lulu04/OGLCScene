unit Form_AskGroundType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,
  StrUtils,
  OGLCScene;

type

  { TFormAskGroundType }

  TFormAskGroundType = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    E1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LB: TListBox;
    OD2: TOpenDialog;
    SBHelp1: TSpeedButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SBHelp1Click(Sender: TObject);
  private
    function NameAlreadyExist: boolean;
  public
    { public declarations }
  end;

  function GroundTypeToString( AIndex: integer ): string;
var
  FormAskGroundType: TFormAskGroundType;

implementation
uses tileset_manager,
     common,
     form_tools,
     umaps;

function GroundTypeToString(AIndex: integer): string;
begin
 if ( AIndex < 0 ) or ( AIndex >= FormAskGroundType.LB.Count )
   then Result := '??'+inttostr(AIndex)
   else Result := FormAskGroundType.LB.Items.Strings[AIndex];
end;

{$R *.lfm}

{ TFormAskGroundType }

// delete ground type
procedure TFormAskGroundType.Button2Click(Sender: TObject);
var itx, ixfr, iyfr, gr: integer;
begin
 if LB.ItemIndex = -1 then exit;
 if ( LB.ItemIndex = 0 ) or ( LB.ItemIndex = 1 )
   then begin
     Showmessage('Predefined value... You can not delete this ground type');
     exit;
   end;

 for itx:=0 to TileSetManager.Count-1 do  // for all texture (tileset)
   for ixfr:=0 to TileSetManager.TileSet[itx].XTileCount-1 do
     for iyfr:=0 to TileSetManager.TileSet[itx].YTileCount-1 do
       begin
        gr := MapList.MainMap.TileEngine.GetGroundType( itx, ixfr, iyfr );
        if gr = LB.ItemIndex-1
          then MapList.MainMap.TileEngine.SetGroundType( itx, ixfr, iyfr, -1 )
          else if gr > LB.ItemIndex-1
                 then MapList.MainMap.TileEngine.SetGroundType( itx, ixfr, iyfr, gr-1 );
   end;

 LB.Items.Delete( LB.ItemIndex );

 FormTools.PB1.Invalidate;
 SetProjectModified;
end;

// load ground types from another map file
procedure TFormAskGroundType.Button3Click(Sender: TObject);
var temp : TStringList;
    i, k: integer;
    txtsplited: TStringArray;
begin
 if not OD2.Execute then exit;

 temp := TStringList.Create;
 temp.LoadFromFile( OD2.Filename );

 k := temp.IndexOf('GROUND_NAMES');
 if k <> -1 then
 begin
   LB.Clear;
   txtsplited := temp.Strings[k+1].Split(['|']);
   for i:=0 to Length( txtsplited )-1 do
    LB.Items.Add( txtsplited[i] );
 end;

 temp.Free;

 FormTools.PB1.Invalidate;
 SetProjectModified;
end;

// rename ground
procedure TFormAskGroundType.Button4Click(Sender: TObject);
begin
 if LB.ItemIndex = -1 then exit;

 E1.Text := DelSpace( E1.Text );
 if E1.Text = '' then exit;

 if NameAlreadyExist then exit;
 LB.Items.Strings[LB.ItemIndex] := E1.Text;

 SetProjectModified;
end;

procedure TFormAskGroundType.SBHelp1Click(Sender: TObject);
begin
 ShowMessage('You can specify a ground type (integer) for each tile in your tileset.'+lineending+
             'There are two predefined types:'+lineending+
             '     - HOLE ->  Returned if there is a hole in the map ( value = 0 ).'+lineending+
             '     - NEUTRAL -> Default ground type is equal to neutral (value = 1).'+lineending+
             'This mecanism allow you to test collision between your sprites and specific tile on your map.'+lineending+

             lineending+
             'Each ground type have a name. Please, avoid spaces and choose a short name, it''s better.'+lineending+
             lineending+
             'ADD -> First enter a new type name and click ''Add'' button.'+lineending+
             'DELETE -> Delete the selected name in the list.'+lineending+
             'RENAME -> Rename the selected name in the list.'+lineending+
             lineending+
             'LOAD FROM ANOTHER MAP -> You can load an existing ground type list from another map.'+lineending+
             lineending+
             'SET TYPE ON SELECTED TILE -> set the ground type selected in the list on the tile under the mouse'+lineending+
             'or in the whole selected area on render window.' );
end;

function TFormAskGroundType.NameAlreadyExist: boolean;
var i: integer;
begin
 Result := FALSE;
 for i:=0 to LB.Count-1 do
   if LB.Items.Strings[i] = E1.Text
     then begin
       Showmessage(''''+E1.Text+''' already exist !');
       E1.SelectAll;
       E1.SetFocus;
       Result := TRUE;
       exit;
     end;
end;

// add ground type
procedure TFormAskGroundType.Button1Click(Sender: TObject);
begin
 E1.Text := DelSpace( E1.Text );
 if E1.Text = '' then exit;

 if NameAlreadyExist then exit;

 LB.Items.Add( E1.Text );
 LB.ItemIndex := LB.Count -1;

 E1.Text := '';
 E1.SetFocus;
 SetProjectModified;
end;

procedure TFormAskGroundType.BitBtn1Click(Sender: TObject);
begin
 if LB.ItemIndex = -1
   then begin
     Showmessage('Please, choose a ground type...');
     ModalResult := mrNone;
   end;
 SetProjectModified;
end;


end.

