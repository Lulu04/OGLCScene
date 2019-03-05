{
  ****************************************************************************
 *                                                                          *
 *  This file is part of OGLCScene library which is distributed             *
 *  under the modified LGPL.                                                *
 *                                                                          *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,   *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This program is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
 ****************************************************************************

 TILE MAP DESIGNER  written by Lulu - 2017
 Using OGLCScene, this software allow to create map for game based on tile.
 It is a simple version that does not pretend to compete with those already
 existing.

}
unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, LCLIntf, ExtCtrls, LCLType, Menus, Buttons, Types,
  BGRABitmap, BGRABitmapTypes,
  common, OGLCScene,
  VelocityCurve,
  uinsertlinecolumn,
  uAskEventValue,
  uexporteventtype,
  screen_map,
  tileset_manager;

type

  TState = ( sNeutral, sMoveView, sSelecting, sSelectionDone );

  { TForm_Main }

  TForm_Main = class(TForm)
    MenuClear: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Menu_StartMap: TMenuItem;
    Menu_FillWithSelectedTile: TMenuItem;
    MenuItem2: TMenuItem;
    Menu_Paste: TMenuItem;
    Menu_Cut: TMenuItem;
    Menu_Copy: TMenuItem;
    MenuItem6: TMenuItem;
    Menu_InsertRow: TMenuItem;
    Menu_InsertColumn: TMenuItem;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SBHelp3: TSpeedButton;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuClearClick(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure Menu_CutClick(Sender: TObject);
    procedure Menu_FillWithSelectedTileClick(Sender: TObject);
    procedure Menu_PasteClick(Sender: TObject);
    procedure Menu_CopyClick(Sender: TObject);
    procedure Menu_InsertRowClick(Sender: TObject);
    procedure Menu_InsertColumnClick(Sender: TObject);
    procedure Menu_StartMapClick(Sender: TObject);
    procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure SBHelp3Click(Sender: TObject);
  private
    FCoorBeginLeftClick,
    FCoorEndLeftClick,
    FOriginTileEngine     : TPoint;
    FState: TState;
    procedure DoPaintMap;
    function ClientCoorToColumnRowIndex( aPoint: TPoint ): TPoint; // return column and row indexes of tile pointed by aPoint (client window coordinates)
    function ColumnRowIndexToClientCoor( aTile: TPoint ): TPoint;  // return coordinate from tile's (column,row) indexes
    function ClientCoorToTileTopLeftCoor( aP: TPoint ): TPoint;

  private
    // selection
    FTileArrayToPaste: TArrayOfArrayOfTile;
    function SelectionAvailable: boolean;
    function TileCountInSelection: TSize;
  private
    procedure LoadCommonRessource;
    procedure FreeCommonRessource;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  private
    FMousePos: TPoint;
    function XCoorToColumnIndex( AX: integer ): integer;
    function YCoorToRowIndex( AY: integer ): integer;
    function ColumnIndexToXCoord( aColumn: integer ): integer;
    function RowIndexToYCoord( aRow: integer ): integer;


    function ClientPosToTileIndex( APos: TPoint ): TPoint;
    function ScreenPosToTileIndex( APos: TPoint ): TPoint;

    function XYCoorIsInMap( AX, AY: integer ): boolean;
    procedure ShowActionText( aX, aY: single; aTxt: string );
  public

    procedure ProcessTileEngineEvent( Sender: TTileEngine; const SceneTileCoor: TPointF; Event: integer );

    procedure SetWindowsTitle( const s: string );


  end;

var
  Form_Main: TForm_Main;

implementation
uses u_tool_window,
     uAskGroundType,
     umaps, u_tileset_edit;
{$R *.lfm}

{ TForm_Main }

procedure TForm_Main.FormCreate(Sender: TObject);
begin
 FScene := TOGLCScene.Create ( OpenGLControl1 ) ;
 FScene.LayerCount := LAYER_COUNT;

 FScene.OnLoadCommonData := @LoadCommonRessource;
 FScene.OnFreeCommonData := @FreeCommonRessource;

 Application.OnIdle := @ProcessApplicationIdle;
 FScene.OnAfterPaint := @DoPaintMap;
 FState := sNeutral;

end;

procedure TForm_Main.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var mr: TModalResult;
begin
 CanClose := FALSE;
 if FProjectIsModified then begin
  mr := MessageDlg('', 'Before quit, do you want to save the current(s) map(s) ?', mtWarning, [mbYes, mbNo, mbCancel],0);
  if mr = mrCancel then exit;
  if mr = mrYes then MapList.SaveSession;
  FProjectIsModified := FALSE;
 end;
 CanClose:=TRUE;
end;

procedure TForm_Main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 FScene.ColorFadeIn(BGRABlack,0.5);
 FScene.ExecuteDuring(0.5);
end;

procedure TForm_Main.FormDestroy(Sender: TObject);
begin
 FScene.Free;
 FScene := NIL;
end;

procedure TForm_Main.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 EvalKey( Key );

 FMousePos := OpenGlControl1.ScreenToClient( Mouse.CursorPos );
 // shortcut key to copy, paste, cut, erase
 case Key of
  VK_SPACE: begin
    Menu_FillWithSelectedTileClick(self);
  end;

  VK_DELETE, VK_BACK: begin
    MenuClearClick(self);
  end;

  VK_X : if ssCtrl in Shift then begin
    Menu_CutClick( self );
  end;

  VK_C : if ssCtrl in Shift then begin
    Menu_CopyClick( self );
  end;

  VK_V : if ssCtrl in Shift then begin
    Menu_PasteClick( self );
  end;
 end;//case
end;

procedure TForm_Main.FormMouseEnter(Sender: TObject);
begin
 SetFocus;
end;

procedure TForm_Main.FormShow(Sender: TObject);
begin
 Form_Tools.Show;
end;

function TForm_Main.ClientPosToTileIndex( APos: TPoint ): TPoint;
begin
 Result.x := XCoorToColumnIndex( APos.x );
 Result.y := YCoorToRowIndex( APos.y );
end;

function TForm_Main.ScreenPosToTileIndex(APos: TPoint): TPoint;
begin
 Result := ClientPosToTileIndex( OpenGlControl1.ScreenToClient( APos ) );
end;

// popup Clear tile
procedure TForm_Main.MenuClearClick(Sender: TObject);
var ro, co: integer;
     s: TSize;
    p1: TPoint;
    t: string;
begin
 if TilesetEdit.IsActive then exit;

 if not SelectionAvailable then
   begin // erase tile under mouse
    if XYCoorIsInMap( FMousePos.x, FMousePos.y ) then begin
      p1 := ClientPosToTileIndex( FMousePos );
      FWorkingTileEngine.SetCell( p1.y, p1.x, -1, 0, 0 );
      FWorkingTileEngine.SetUserEventValue( p1.y, p1.x, -1 );
      ShowActionText( FMousePos.x, FMousePos.y, 'erase 1 tile');
      SetProjectModified;
    end;
   end else begin
     // erase selection
     s := TileCountInSelection;
     p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
     for ro:=p1.y to p1.y+s.cy-1 do
       for co:=p1.x to p1.x+s.cx-1 do
         begin
          FWorkingTileEngine.SetCell( ro, co, -1, 0, 0 );
          FWorkingTileEngine.SetUserEventValue( ro, co, -1 );
         end;
     t := 'erase ' + inttostr( s.cx * s.cy ) + ' tile';
     if s.cx * s.cy > 1 then t+='s';
     ShowActionText( FMousePos.x, FMousePos.y, t);
     SetProjectModified;
   end;
end;

// popup Set ground type...
procedure TForm_Main.MenuItem10Click(Sender: TObject);
var pt: PTile;
  p1: TPoint;
  s: TSize;
  ro, co: integer;
begin
 if not XYCoorIsInMap( FMousePos.x, FMousePos.y ) then exit;
 if Form_AskGroundType.ShowModal = mrCancel then exit;

 if not SelectionAvailable then begin  // set ground type on tile under the mouse
      p1 := ClientPosToTileIndex( FMousePos );
      pt:=FWorkingTileEngine.GetPTile(p1.y, p1.x);
      FWorkingTileEngine.SetGroundType( pt^.TextureIndex,pt^.ixFrame, pt^.iyFrame, Form_AskGroundType.LB.ItemIndex);
      SetProjectModified;
 end else begin                       // set ground type on the selected tiles
      s := TileCountInSelection;
      p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
      for ro:=p1.y to p1.y+s.cy-1 do
       for co:=p1.x to p1.x+s.cx-1 do begin
         pt:=FWorkingTileEngine.GetPTile(ro, co);
         FWorkingTileEngine.SetGroundType( pt^.TextureIndex, pt^.ixFrame, pt^.iyFrame, Form_AskGroundType.LB.ItemIndex);
       end;
      ShowActionText( FMousePos.x, FMousePos.y, 'set ground type' );
      SetProjectModified;
 end;
end;

// popup Set user event value
procedure TForm_Main.MenuItem3Click(Sender: TObject);
var ro, co: integer;
    s: TSize;
    p1: TPoint;
    t: string;
begin
 if Form_AskEvent.ShowModal <> mrOk then exit;

 if not SelectionAvailable then
   begin // set event on tile under mouse
    if XYCoorIsInMap( FMousePos.x, FMousePos.y ) then begin
      p1 := ClientPosToTileIndex( FMousePos );
      FWorkingTileEngine.SetUserEventValue( p1.y, p1.x, Form_AskEvent.LB.ItemIndex );
      ShowActionText( FMousePos.x, FMousePos.y, 'set 1 event');
      SetProjectModified;
    end;
   end else begin // set event on selection
     s := TileCountInSelection;
     p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
     for ro:=p1.y to p1.y+s.cy-1 do
      for co:=p1.x to p1.x+s.cx-1 do
        FWorkingTileEngine.SetUserEventValue( ro, co, Form_AskEvent.LB.ItemIndex );

     t := 'set '+inttostr(s.cx * s.cy)+' event';
     if s.cx * s.cy > 1 then t+='s';
     ShowActionText( FMousePos.x, FMousePos.y, t );
     SetProjectModified;
   end;
end;

// popup Delete event
procedure TForm_Main.MenuItem5Click(Sender: TObject);
var ro, co: integer;
    s: TSize;
    p1: TPoint;
    t: string;
begin
 if not SelectionAvailable then
   begin // delete event on tile under mouse
    if XYCoorIsInMap( FMousePos.x, FMousePos.y ) then begin
      p1 := ClientPosToTileIndex( FMousePos );
      FWorkingTileEngine.SetUserEventValue( p1.y, p1.x, -1 );
      ShowActionText( FMousePos.x, FMousePos.y, 'delete 1 event');
      SetProjectModified;
    end;
   end else begin // delete event on selection
     s := TileCountInSelection;
     p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
     for ro:=p1.y to p1.y+s.cy-1 do
      for co:=p1.x to p1.x+s.cx-1 do
        FWorkingTileEngine.SetUserEventValue( ro, co, -1 );

     t := 'delete '+inttostr(s.cx * s.cy)+' event';
     if s.cx * s.cy > 1 then t+='s';
     ShowActionText( FMousePos.x, FMousePos.y, t );
     SetProjectModified;
   end;
end;

procedure TForm_Main.MenuItem7Click(Sender: TObject);
begin
 Form_ExportEvent.ShowModal;
end;

// popup 'put all tiles in the current tileset'
procedure TForm_Main.MenuItem9Click(Sender: TObject);
var ro, co: integer;
    p1: TPoint;
    Ftl: TTileSet;
begin
 if Form_Tools.CB1.ItemIndex=-1 then exit;

 p1 := ClientPosToTileIndex( FMousePos );
 Ftl := TileSetManager.TileSet[Form_Tools.CB1.ItemIndex];
 for ro:=p1.y to p1.y+Ftl.YTileCount-1 do
  for co:=p1.x to p1.x+Ftl.XTileCount-1 do
   begin
    FWorkingTileEngine.SetCell( ro, co, Form_Tools.CB1.ItemIndex, co-p1.x, ro-p1.y );
    FWorkingTileEngine.SetUserEventValue( ro, co, -1 );
   end;

 ShowActionText( FMousePos.x, FMousePos.y, 'put whole tileset');
 SetProjectModified;
end;

// popup Cut
procedure TForm_Main.Menu_CutClick(Sender: TObject);
var ro, co: integer;
    s: TSize;
    p1: TPoint;
    t: string;
begin
 if TilesetEdit.IsActive then exit;

 if not SelectionAvailable then
   begin // cut tile under mouse
    if XYCoorIsInMap( FMousePos.x, FMousePos.y ) then begin
      Menu_CopyClick( Sender );
      p1 := ClientPosToTileIndex( FMousePos );
      FWorkingTileEngine.SetCell( p1.y, p1.x, -1, -1, -1 );
      FWorkingTileEngine.SetUserEventValue( p1.y, p1.x, -1 );
      ShowActionText( FMousePos.x, FMousePos.y, 'cut 1 tile');
      SetProjectModified;
    end;
   end else begin // cut selection
     // copy selected area
     Menu_CopyClick( Sender );
     // erase selected area
     s := TileCountInSelection;
     p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
     for ro:=p1.y to p1.y+s.cy-1 do
      for co:=p1.x to p1.x+s.cx-1 do
       begin
        FWorkingTileEngine.SetCell( ro, co, -1, -1, -1 );
        FWorkingTileEngine.SetUserEventValue( ro, co, -1 );
       end;

     t := 'cut ' + inttostr( s.cx * s.cy ) + ' tile';
     if s.cx * s.cy > 1 then t+='s';
     ShowActionText( FMousePos.x, FMousePos.y, t );
     SetProjectModified;
   end;
end;

// popup fill with selected tile
procedure TForm_Main.Menu_FillWithSelectedTileClick(Sender: TObject);
var ro, co, t, ixfr, iyfr : integer;
    s: TSize;
    p1: TPoint;
    tt: string;
begin
 if TilesetEdit.IsActive then exit;

 Form_Tools.GetTextureAndFrameindex( t, ixfr, iyfr );
 if not SelectionAvailable then
   begin // fill tile under mouse
    if XYCoorIsInMap( FMousePos.x, FMousePos.y ) then begin
      p1 := ClientPosToTileIndex( FMousePos );
      FWorkingTileEngine.SetCell( p1.y, p1.x, t, ixfr, iyfr );

      ShowActionText( FMousePos.x, FMousePos.y, 'fill 1 tile');
      SetProjectModified;
    end;
   end else begin // fill selection
     s := TileCountInSelection;
     p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
     for ro:=p1.y to p1.y+s.cy-1 do
       for co:=p1.x to p1.x+s.cx-1 do
        FWorkingTileEngine.SetCell( ro, co, t, ixfr, iyfr );

     tt := 'fill ' + inttostr( s.cx * s.cy ) + ' tile';
     if s.cx * s.cy > 1 then tt+='s';
     ShowActionText( FMousePos.x, FMousePos.y, tt);
     SetProjectModified;
   end;
end;

// popup Copy
// if there is no selection, copy only tile over mouse
// if there is selection, copy it
procedure TForm_Main.Menu_CopyClick(Sender: TObject);
var ro, co: integer;
    s: TSize;
    p1: TPoint;
    tt: string;
    p: PTile;
begin
 if not SelectionAvailable then
   begin // copy tile under mouse
    if XYCoorIsInMap( FMousePos.x, FMousePos.y ) then begin
      SetLength( FTileArrayToPaste, 1, 1 );
      p1 := ClientPosToTileIndex( FMousePos );

      p := FWorkingTileEngine.GetPTile( p1.y, p1.x);

      FTileArrayToPaste[0][0].TextureIndex := p^.TextureIndex;
      FTileArrayToPaste[0][0].ixFrame := p^.ixFrame;
      FTileArrayToPaste[0][0].iyFrame := p^.iyFrame;
      FTileArrayToPaste[0][0].UserEvent := p^.UserEvent;
      ShowActionText( FMousePos.x, FMousePos.y, 'copy 1 tile');
    end;
   end else begin // copy selection
     // prepare buffer space
     s := TileCountInSelection;
     SetLength( FTileArrayToPaste, s.cy, s.cx );
     // copy the selected tiles in buffer
     p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
     for ro:=p1.y to p1.y+s.cy-1 do
       for co:=p1.x to p1.x+s.cx-1 do
        begin
         p := FWorkingTileEngine.GetPTile( ro, co );
         FTileArrayToPaste[ro-p1.y][co-p1.x].TextureIndex := p^.TextureIndex;
         FTileArrayToPaste[ro-p1.y][co-p1.x].ixFrame := p^.ixFrame;
         FTileArrayToPaste[ro-p1.y][co-p1.x].iyFrame := p^.iyFrame;
         FTileArrayToPaste[ro-p1.y][co-p1.x].UserEvent := p^.UserEvent;
        end;

     tt := 'copy ' + inttostr( s.cx * s.cy ) + ' tile';
     if s.cx * s.cy > 1 then tt+='s';
     ShowActionText( FMousePos.x, FMousePos.y, tt);
   end;
end;

// popup Paste
procedure TForm_Main.Menu_PasteClick(Sender: TObject);
var p1: TPoint;
    p3: PTile;
    ro, co, i: integer;
    t: string;
begin
 if TilesetEdit.IsActive then exit;

 if Length( FTileArrayToPaste ) = 0 then exit;
 if not XYCoorIsInMap( FMousePos.x, FMousePos.y ) then exit;
 // copy tiles in buffer to map
 p1 := ClientPosToTileIndex( FMousePos );
 for ro:=p1.y to p1.y+Length( FTileArrayToPaste )-1 do
  for co:=p1.x to p1.x+Length( FTileArrayToPaste[0] )-1 do
   begin
    {
     p2 := FTileEngine.GetPTile( ro, co );
     if p2 <> NIL then
       then begin
             p3 := @FTileArrayToPaste[ro-p1.y][co-p1.x];
             p2^.TextureIndex := p3^.TextureIndex;
             p2^.ixFrame:= p3^.ixFrame;
             p2^.iyFrame := p3^.iyFrame;
             p2^.UserEvent := p3^.UserEvent;
       end;
    }
    p3 := @FTileArrayToPaste[ro-p1.y][co-p1.x];
    FWorkingTileEngine.SetCell( ro, co, p3^.TextureIndex, p3^.ixFrame, p3^.iyFrame );
    FWorkingTileEngine.SetUserEventValue( ro, co, p3^.UserEvent );
   end;

 i := Length( FTileArrayToPaste ) * Length( FTileArrayToPaste[0] );
 t := 'paste ' + inttostr( i ) + ' tile';
 if i > 1 then t+='s';
 ShowActionText( FMousePos.x, FMousePos.y, t);
 SetProjectModified;
end;


// popup insert row
procedure TForm_Main.Menu_InsertRowClick(Sender: TObject);
var it: TPoint;
    t: string;
begin
 if TilesetEdit.IsActive then exit;

 it := ClientPosToTileIndex( FMousePos );
 Form_InsertLineColumn.Label1.Caption := 'Row to insert :';
 if Form_InsertLineColumn.ShowModal = mrCancel then exit;

 MapList.InsertRow( it.y, Form_InsertLineColumn.SE1.Value );

 t := 'insert ' + inttostr( Form_InsertLineColumn.SE1.Value ) + ' row';
 if Form_InsertLineColumn.SE1.Value > 1 then t+='s';
 ShowActionText( FMousePos.x, FMousePos.y, t);

 Form_Tools.UpdateMapParameterOnScreen;

 SetProjectModified;
end;

// popup insert columns
procedure TForm_Main.Menu_InsertColumnClick(Sender: TObject);
var it: TPoint;
  t: string;
begin
 if TilesetEdit.IsActive then exit;

 it := ClientPosToTileIndex( FMousePos );
 Form_InsertLineColumn.Label1.Caption := 'Column to insert :';
 if Form_InsertLineColumn.ShowModal = mrCancel then exit;

 MapList.InsertColumn( it.x, Form_InsertLineColumn.SE1.Value );

 t := 'insert ' + inttostr( Form_InsertLineColumn.SE1.Value ) + ' column';
 if Form_InsertLineColumn.SE1.Value > 1 then t+='s';
 ShowActionText( FMousePos.x, FMousePos.y, t);
 SetProjectModified;
end;

// menu Start map on this tile
procedure TForm_Main.Menu_StartMapClick(Sender: TObject);
var it: TPoint;
begin
 it := ClientPosToTileIndex( FMousePos );

 Form_Tools.Label10.Caption := inttostr( it.x );
 Form_Tools.Label11.Caption := inttostr( it.y );
 ShowActionText( FMousePos.x, FMousePos.y, 'Define start map at this tile');
 SetProjectModified;
end;

procedure TForm_Main.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then begin
   FState := sNeutral;
   FCoorBeginLeftClick.x := X;
   FCoorBeginLeftClick.y := Y;
 end;

 if ( Button = mbLeft ) and ( ssShift in Shift )
    and not( ssCtrl in Shift ) and XYCoorIsInMap( X, Y ) then begin
   FState := sSelecting;
   FCoorEndLeftClick := FCoorBeginLeftClick;
 end;

 if ( Button = mbLeft ) and not ( ssShift in Shift ) then begin
   FState := sMoveView;
   FOriginTileEngine.x := round(FWorkingTileEngine.X.Value);
   FOriginTileEngine.y := round(FWorkingTileEngine.Y.Value);
 end;

 if ( Button = mbLeft ) and ( ssCtrl in Shift ) then begin
   //select tile under mouse (if any)
 end;
end;

procedure TForm_Main.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var it: TPoint;
    tc: TSize;
    i: integer;
    p:PTile;
    s: string;
begin
 if not FReady then exit;
 // update info on screen
 if XYCoorIsInMap( X, Y ) then begin

  FLabelMapPosition.Caption := 'Map position:  (' + inttostr(round(X-FWorkingTileEngine.X.Value)) +
                               ',' + inttostr(round(Y-FWorkingTileEngine.Y.Value)) + ')';

  it := ClientPosToTileIndex( Point(X,Y) );
  s := 'Tile Column: ' + inttostr(it.x+1)+'/'+inttostr(FWorkingTileEngine.MapTileCount.cx)+
       '  Row: '+inttostr(it.y+1)+'/'+inttostr(FWorkingTileEngine.MapTileCount.cy);
  if TilesetEdit.IsActive
     then s+='  Tileset: '+inttostr(TilesetEdit.CurrentTilesetIndex)
     else s+='  Layer: '+MapList.SelectedLayerName;
  FLabelTileIndexes.Caption := s;

  p := FWorkingTileEngine.GetPTile( it.y, it.x);
  i := FWorkingTileEngine.GetGroundType( PointF(X,Y) );
  FLabelGroundType.Caption := 'Ground: ' + GroundTypeToString( i )+' ('+inttostr(i)+')';
  FLabelEventName.Caption:='Event: ' + GetStrEvent( FWorkingTileEngine.GetUserEventValue( PointF(X,Y) ))+
                           '  ->  '+ inttostr(p^.UserEvent);
{
 ti := FTileEngine.GetPTileTexInfo( it.y, it.x );
 FLabelDebug.Caption:='Mouse('+inttostr(X)+','+inttostr(Y)+')  '+
                      'Tile('+inttostr(it.x)+')('+inttostr(it.y)+')   '+
                      'Tex '+inttostr(p^.TextureIndex)+'   '+
                      'ixFrame '+inttostr(p^.ixFrame)+'   '+
                      'iyFrame '+inttostr(p^.iyFrame)+'   '+
                      'TileType '+inttostr(ti^.TileType[p^.ixFrame][p^.iyFrame])+'   '+
                      'maxtype: x '+inttostr(length(ti^.TileType))+'  y '+inttostr(length(ti^.TileType[p^.ixFrame]));
}
 end else begin
  FLabelMapPosition.Caption := 'Map position:';
  FLabelTileIndexes.Caption := 'Tile';
  FLabelGroundType.Caption := 'Ground:';
  FLabelEventName.Caption:='Event:';
 end;

 FLabelEventName.Visible := not TileSetEdit.IsActive;

 case FState of
     sSelecting: begin
           if XYCoorIsInMap( X, Y ) then begin
             FCoorEndLeftClick.x := X;
             FCoorEndLeftClick.y := Y;

             tc := TileCountInSelection;
             FLabelSelectionInfo.Caption := 'Selection:  column ' + inttostr(tc.cx) + '   row ' + inttostr(tc.cy);
           end;
     end;
     sMoveView: begin
        if TilesetEdit.IsActive
           then TilesetEdit.SetTileEngineCoordinates( FOriginTileEngine.x + X - FCoorBeginLeftClick.x, FOriginTileEngine.y + Y - FCoorBeginLeftClick.y )
           else MapList.SetTileEngineCoordinates( FOriginTileEngine.x + X - FCoorBeginLeftClick.x, FOriginTileEngine.y + Y - FCoorBeginLeftClick.y );
         // FWorkingTileEngine.SetCoordinate( FOriginTileEngine.x + X - FCoorBeginLeftClick.x, FOriginTileEngine.y + Y - FCoorBeginLeftClick.y );
     end;
     sNeutral: begin
          if XYCoorIsInMap( X, Y) then begin
            FCoorBeginLeftClick.x := X;
            FCoorBeginLeftClick.y := Y;
            FCoorEndLeftClick :=FCoorBeginLeftClick;

            FLabelSelectionInfo.Caption := 'Selection:';
          end;
     end;
 end;
end;

procedure TForm_Main.OpenGLControl1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p: TPoint;
begin
 if Button = mbLeft then begin
   case FState of
    sSelecting: begin
         FCoorEndLeftClick.x := X;
         FCoorEndLeftClick.y := Y;
         FState := sSelectionDone;
    end;
    sMoveView: begin
         FState := sNeutral;
    end;
   end;
 end;

 if (Button = mbRight) and XYCoorIsInMap( X, Y) and
   ((FState = sSelectionDone) or (FState = sNeutral))then begin   // popup menu
   FMousePos := Point(X,Y);
   p := ClientToScreen( Point(X,Y) );

   PopupMenu1.PopUp( p.x, p.y);
 end;
end;


// click on help button
procedure TForm_Main.SBHelp3Click(Sender: TObject);
begin
 Showmessage('This is the RENDER WINDOW.'+lineending+
             lineending+
             'LEFT CLICK + MOUSE MOVE => drag the map.'+lineending+
             'LEFT CLICK + SHIFT + MOUSE MOVE => select tile area on map.'+lineending+
             lineending+
             'Shortcut key :'+lineending+
             '              SPACE -> Fill tile under mouse or in selected area with selected tile.'+lineending+
             ' LEFT-RIGHT-UP-DOWN -> change selected tile in current tileset.'+lineending+
             '             DELETE -> Replace tile under mouse or in selected area by hole color.'+lineending+
             '             CTRL X -> Cut tile under mouse or in selected area.'+lineending+
             '             CTRL C -> Copy tile under mouse or in selected area.'+lineending+
             '             CTRL V -> Paste tile(s) copied at mouse position.'+lineending+
             lineending+
             '        F1 -> Previous layer.'+lineending+
             '        F2 -> Next layer.'+lineending+
             '        F6 -> Show/Hide main window.'+lineending+
             lineending+
             'RIGHT CLIC -> Tile contextual menu'+lineending+
             '  - START GAME ON THIS TILE: when you will load the map in your game application,'+lineending+
             '    the position will be set automatically to draw this tile on top/left view.'+lineending+
             '  - EVENT... -> To set an event on the tile under the mouse or in selected area.'+lineending+
             '  - DELETE EVENT -> To remove an event on the tile under the mouse or in selected area.'+lineending+
             '  - COPY EVENT TO CLIPBOARD -> Copy the event list in clipboard, so you can'+lineending+
             '    paste them under Lazarus, in your game application.' );
end;


function TForm_Main.XYCoorIsInMap(AX, AY: integer): boolean;
begin
 with FWorkingTileEngine do
 Result := ( AX >= X.Value ) and ( AX < X.Value + Width ) and
           ( AY >= Y.Value ) and ( AY < Y.Value + Height );
end;

procedure TForm_Main.ShowActionText(aX, aY: single; aTxt: string);
var o : TFreeText;
  rx, ry, time: single;
begin
  o := TFreeText.Create;
  o.TexturedFont:=FHintFont;
  o.Caption:=aTxt;
  o.SetCenterCoordinate( aX, aY );
  FScene.Add( o, Layer_InfoMap );
  time := 2.0;
  o.Opacity.ChangeTo( 0, time, idcStartSlowEndFast );
  if aX > FScene.Width/2
    then rx := -100
    else rx := 100;
  if aY > FScene.Height/2
    then ry := -100
    else ry := 100;
  o.MoveRelative( rx, ry, time, idcSinusoid );
  o.KillDefered( time );
end;

procedure TForm_Main.ProcessTileEngineEvent(Sender: TTileEngine;
  const SceneTileCoor: TPointF; Event: integer);
var o: TFreeText;
    p: TPointF;
begin
 if not Form_Tools.CheckBox2.Checked or ( Event = -1 ) then exit;
 o := TFreeText.Create;
 o.TexturedFont:=FEventFont;
 p := SceneTileCoor + PointF( FWorkingTileEngine.TileSize.cx*0.5, FWorkingTileEngine.TileSize.cy*0.5 );
 o.SetCenterCoordinate( p.x, p.y );
 TextureManager.DisableTextureUsage;
 FillBox( o.X.Value-3, o.Y.Value-3, o.Width+6, o.Height+6, BGRA(1,0,0) );
 o.Draw( 1.0 );
 o.Free;
end;

procedure TForm_Main.SetWindowsTitle(const s: string);
begin
 Caption := 'TILE MAP DESIGNER - ' + s;
end;


function TForm_Main.XCoorToColumnIndex(AX: integer): integer;
begin
 Result := round(AX-FWorkingTileEngine.X.Value) div FWorkingTileEngine.TileSize.cx + FWorkingTileEngine.GetFirstTileColumnIndex;
 if Result < 0 then Result := 0;
 if Result > FWorkingTileEngine.MapTileCount.cx-1 then Result := FWorkingTileEngine.MapTileCount.cx-1;
end;

function TForm_Main.YCoorToRowIndex(AY: integer): integer;
begin
 Result := round(AY-FWorkingTileEngine.Y.Value) div FWorkingTileEngine.TileSize.cy + FWorkingTileEngine.GetFirstTileRowIndex;
 if Result < 0 then Result := 0;
 if Result > FWorkingTileEngine.MapTileCount.cy-1 then Result := FWorkingTileEngine.MapTileCount.cy-1;
end;

function TForm_Main.ColumnIndexToXCoord( aColumn: integer ): integer;
begin
 Result := ( aColumn - FWorkingTileEngine.GetFirstTileColumnIndex ) * FWorkingTileEngine.TileSize.cx {%H-}+ round( FWorkingTileEngine.Y.Value );
end;

function TForm_Main.RowIndexToYCoord( aRow: integer ): integer;
begin
 Result := ( aRow - FWorkingTileEngine.GetFirstTileRowIndex ) * FWorkingTileEngine.TileSize.cy {%H-}+ round( FWorkingTileEngine.Y.Value );
end;

function TForm_Main.ClientCoorToColumnRowIndex(aPoint: TPoint): TPoint;
begin
 Result.x := round( aPoint.x - FWorkingTileEngine.X.Value ) div FWorkingTileEngine.TileSize.cx + FWorkingTileEngine.GetFirstTileColumnIndex;
 if Result.x < 0 then Result.x := 0;
 if Result.x > FWorkingTileEngine.MapTileCount.cx-1 then Result.x := FWorkingTileEngine.MapTileCount.cx-1;

 Result.y := round( aPoint.y - FWorkingTileEngine.Y.Value ) div FWorkingTileEngine.TileSize.cy + FWorkingTileEngine.GetFirstTileRowIndex;
 if Result.y < 0 then Result.y := 0;
 if Result.y > FWorkingTileEngine.MapTileCount.cy-1 then Result.y := FWorkingTileEngine.MapTileCount.cy-1;
end;

function TForm_Main.ColumnRowIndexToClientCoor(aTile: TPoint): TPoint;
begin
 Result.x := ( aTile.x - FWorkingTileEngine.GetFirstTileColumnIndex ) * FWorkingTileEngine.TileSize.cx {%H-}+ round( FWorkingTileEngine.Y.Value );
 Result.y := ( aTile.y - FWorkingTileEngine.GetFirstTileRowIndex ) * FWorkingTileEngine.TileSize.cy {%H-}+ round( FWorkingTileEngine.Y.Value );
end;

function TForm_Main.ClientCoorToTileTopLeftCoor(aP: TPoint): TPoint;
var xx, yy: integer;
begin
 if ( FWorkingTileEngine.TileSize.cx = 0 ) or ( FWorkingTileEngine.TileSize.cy = 0 )
   then begin
     Result.x := 0;
     Result.y := 0;
   end
   else begin
     xx := round(FWorkingTileEngine.X.Value) mod FWorkingTileEngine.TileSize.cx;
     yy := round(FWorkingTileEngine.Y.Value) mod FWorkingTileEngine.TileSize.cy;

     Result.x := (aP.x - xx) div FWorkingTileEngine.TileSize.cx * FWorkingTileEngine.TileSize.cx + xx;
     Result.y := (aP.y - yy) div FWorkingTileEngine.TileSize.cy * FWorkingTileEngine.TileSize.cy + yy;
   end;
end;

function TForm_Main.SelectionAvailable: boolean;
begin
 Result := FState = sSelectionDone;
end;

function TForm_Main.TileCountInSelection: TSize;
var p1, p2: TPoint;
begin
 p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
 p2 := ClientCoorToColumnRowIndex( FCoorEndLeftClick );

 Result.cx := p2.x - p1.x + 1;
 Result.cy := p2.y - p1.y + 1;
end;

procedure TForm_Main.DoPaintMap;
var p1, p2: TPoint;
begin
 if MapList.Count = 0 then exit;
 if ( FWorkingTileEngine.TileSize.cx = 0 ) or ( FWorkingTileEngine.TileSize.cy = 0 ) then exit;

 TextureManager.DisableTextureUsage;
 SetBlendMode( FX_BLEND_NORMAL );

 if ( FState = sSelecting ) or ( FState = sSelectionDone )
   then begin  // rectangular selection area
     p1 := ClientCoorToTileTopLeftCoor( FCoorBeginLeftClick );
     p2 := ClientCoorToTileTopLeftCoor( FCoorEndLeftClick );
     p2.x := p2.x + FWorkingTileEngine.TileSize.cx - p1.x;
     p2.y := p2.y + FWorkingTileEngine.TileSize.cy - p1.y;

     DrawBox( p1.x, p1.y, p2.x, p2.y, BGRA(255,255,255,190), 1.5 );
 end
 else begin  // tile under the mouse
      p1 := Form_Main.ScreenToClient( Mouse.CursorPos );
      if XYCoorIsInMap( p1.x, p1.y )
        then begin
          p1 := ClientCoorToTileTopLeftCoor( p1 );
          DrawBox( p1.x, p1.y, FWorkingTileEngine.TileSize.cx, FWorkingTileEngine.TileSize.cy, BGRA(255,0,0), 1.5 );
        end;
 end;

 // draw a box around the map boundary
 DrawBox( FWorkingTileEngine.X.Value, FWorkingTileEngine.Y.Value,
          FWorkingTileEngine.Width, FWorkingTileEngine.Height,
          BGRA(50,50,50), 1);
end;

procedure TForm_Main.LoadCommonRessource;
var ima: TBGRABitmap;
begin

 ScreenMap := TScreenMap.Create;
 FScene.LaunchStage( ScreenMap );

 TileSetManager := TTilesetManager.Create;

 // white/gray image for grid
 ima := TBGRABitmap.Create( 8, 8, BGRA(220,220,220));
 FImageBackGround := TBGRABitmap.Create( 16, 16, BGRAWhite );
 FImageBackGround.PutImage( 8, 0, ima, dmSet, 255 );
 FImageBackGround.PutImage( 0, 8, ima, dmSet, 255 );
 ima.Free;

end;

procedure TForm_Main.FreeCommonRessource;
begin
 FImageBackGround.Free;
 MapList.Free;
 TileSetEdit.Free;
 TileSetManager.Free;

 ScreenMap.Free;

 Application.OnIdle := NIL;
end;

procedure TForm_Main.ProcessApplicationIdle(Sender: TObject;
  var Done: Boolean);
begin
 if FScene <> NIL then FScene.DoLoop;
 Done := FALSE;
end;

end.

