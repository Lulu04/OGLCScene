unit uAskEventValue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,
  StrUtils,
  OGLCScene;

type

  { TForm_AskEvent }

  TForm_AskEvent = class(TForm)
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
    function NameAlreadyExist( aName: string ): boolean;
  public
    { public declarations }
  end;

function GetStrEvent( AIndex: integer ): string;

var
  Form_AskEvent: TForm_AskEvent;

implementation
uses common;

function GetStrEvent(AIndex: integer): string;
begin
  if ( AIndex < 0 ) or ( AIndex >= Form_AskEvent.LB.Count )
    then Result := 'none'
    else Result := Form_AskEvent.LB.Items.Strings[AIndex];
end;

{$R *.lfm}

{ TForm_AskEvent }

// delete event
procedure TForm_AskEvent.Button2Click(Sender: TObject);
var ro, co, eve: integer;
begin
 if LB.ItemIndex = -1 then exit;
 if LB.GetSelectedText='None' then exit; // avoid to delete first event (none)

 // delete this selected event on the map
 for ro:=0 to FTileEngine.MapTileCount.cy-1 do
   for co:=0 to FTileEngine.MapTileCount.cx-1 do begin
     eve := FTileEngine.GetPTile( ro, co )^.UserEvent ;
     if eve = LB.ItemIndex
       then FTileEngine.GetPTile( ro, co )^.UserEvent := -1
       else if eve > LB.ItemIndex
              then FTileEngine.GetPTile( ro, co )^.UserEvent := FTileEngine.GetPTile( ro, co )^.UserEvent-1;
   end;

 LB.Items.Delete( LB.ItemIndex );
 SetProjectModified;
end;

// load event names from another map file
procedure TForm_AskEvent.Button3Click(Sender: TObject);
var temp : TStringList;
    i, k: integer;
    txtsplited: ArrayOfString;
begin
 if not OD2.Execute then exit;

 temp := TStringList.Create;
 temp.LoadFromFile( OD2.Filename );

 k := temp.IndexOf('EVENT_NAMES');
 if k <> -1 then
 begin
   LB.Clear;
   txtsplited := SplitLineToStringArray( temp.Strings[k+1], '|' );
   for i:=0 to Length( txtsplited )-1 do
    LB.Items.Add( txtsplited[i] );
 end;

 temp.Free;
 SetProjectModified;
end;

// rename event
procedure TForm_AskEvent.Button4Click(Sender: TObject);
begin
 if LB.ItemIndex = -1 then exit;

 E1.Text := DelSpace( E1.Text );
 if E1.Text = '' then exit;

 if NameAlreadyExist( E1.Text ) then exit;
 LB.Items.Strings[LB.ItemIndex] := E1.Text;

 SetProjectModified;
end;

procedure TForm_AskEvent.SBHelp1Click(Sender: TObject);
begin
 ShowMessage('For each tile in your map, you can associate an event value (integer).'+lineending+
             'Default event value is equal to -1 (means no event).'+lineending+
             'If you set an event on a tile, a callback procedure is called each time this tile is drawn.'+lineending+
             'The callback is called after the drawing.'+lineending+
             'To activate this mecanism, simply set the TTileEngine.OnTileEvent property with the adress of your callback procedure.'+lineending+
             '   procedure MyTileEngineEventCallBack( Sender: TTileEngine; const TileTopLeftCoor: TPointF; EventValue: integer );'+lineending+
             lineending+
             'Each event have a name. Please, choose a short name, it''s better.'+lineending+
             lineending+
             'ADD -> First enter a new event name and click ''Add'' button.'+lineending+
             'DELETE -> Delete the selected name in the list.'+lineending+
             'RENAME -> Rename the selected name in the list.'+lineending+
             lineending+
             'LOAD FROM ANOTHER MAP -> You can load an existing event list from another map.'+lineending+
             lineending+
             'SET EVENT ON SELECTED TILE -> set the event selected in the list on the tile under the mouse'+lineending+
             'or in the whole selected area on render window.' );
end;

function TForm_AskEvent.NameAlreadyExist( aName: string ): boolean;
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

// add new event
procedure TForm_AskEvent.Button1Click(Sender: TObject);
begin
 E1.Text := DelSpace( E1.Text );
 if E1.Text = '' then exit;

 if NameAlreadyExist( E1.Text ) then exit;

 LB.Items.Add( E1.Text );
 LB.ItemIndex := LB.Count -1;

 E1.Text := '';
 E1.SetFocus;
 SetProjectModified;
end;

procedure TForm_AskEvent.BitBtn1Click(Sender: TObject);
begin
 if LB.ItemIndex = -1
   then begin
     Showmessage('Please, choose an event...');
     ModalResult := mrNone;
   end;
 SetProjectModified;
end;

end.

