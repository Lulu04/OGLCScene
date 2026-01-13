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
unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, LCLIntf, ExtCtrls, LCLType, Menus, Buttons, ComCtrls, StdCtrls,
  Types, BGRABitmap, BGRABitmapTypes, common, OGLCScene,
  form_insertlinecolumn, form_askeventvalue, form_exporteventtype, screen_map,
  tileset_manager;

type

  TState = (sNeutral, sMoveView, sSelecting, sSelectionDone,
            sMovingSelection,
            sMovingView       // move view with middle mouse button
            );

  TEventLabelItem = record
    UserEvent: integer;
    CenterCoor: TPointF;
  end;
  TEventLabels = array of TEventLabelItem;


  { TFormMain }

  TFormMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MIClear: TMenuItem;
    MenuItem1: TMenuItem;
    MISetGroundType: TMenuItem;
    MenuItem11: TMenuItem;
    MIKeepAsPAttern: TMenuItem;
    MenuItem13: TMenuItem;
    MICopyMapPositionToClipboard: TMenuItem;
    MenuItem15: TMenuItem;
    MIFlipH: TMenuItem;
    MIFlipV: TMenuItem;
    MINoFlip: TMenuItem;
    MIMoveSelection: TMenuItem;
    MIMirrorH: TMenuItem;
    MIMirrorV: TMenuItem;
    MIEvent: TMenuItem;
    MenuItem4: TMenuItem;
    MIDeleteEvent: TMenuItem;
    MICopyEventToClipboard: TMenuItem;
    MenuItem8: TMenuItem;
    MIPutAllTileset: TMenuItem;
    Menu_StartMap: TMenuItem;
    Menu_FillWithSelectedTile: TMenuItem;
    MenuItem2: TMenuItem;
    MIPaste: TMenuItem;
    MICut: TMenuItem;
    MICopy: TMenuItem;
    MenuItem6: TMenuItem;
    MIInsertRow: TMenuItem;
    MIInsertColumn: TMenuItem;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    PopupMenu1: TPopupMenu;
    SBHelp3: TSpeedButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Shape1: TShape;
    Shape2: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton5: TSpeedButton;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure MIClearClick(Sender: TObject);
    procedure MISetGroundTypeClick(Sender: TObject);
    procedure MIKeepAsPAtternClick(Sender: TObject);
    procedure MICopyMapPositionToClipboardClick(Sender: TObject);
    procedure MIFlipHClick(Sender: TObject);
    procedure MIMirrorHClick(Sender: TObject);
    procedure MIEventClick(Sender: TObject);
    procedure MIDeleteEventClick(Sender: TObject);
    procedure MICopyEventToClipboardClick(Sender: TObject);
    procedure MIPutAllTilesetClick(Sender: TObject);
    procedure MICutClick(Sender: TObject);
    procedure Menu_FillWithSelectedTileClick(Sender: TObject);
    procedure MIPasteClick(Sender: TObject);
    procedure MICopyClick(Sender: TObject);
    procedure MIInsertRowClick(Sender: TObject);
    procedure MIInsertColumnClick(Sender: TObject);
    procedure Menu_StartMapClick(Sender: TObject);
    procedure MIMoveSelectionClick(Sender: TObject);
    procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure SBHelp3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
  private
    FCoorBeginLeftClick,
    FCoorEndLeftClick,
    FOriginTileEngine: TPoint;
    FState: TState;
    FEventLabels: TEventLabels;
    FEventLabelCount: integer;
    procedure ClearEventLabels;
    procedure AddEventLabel(aUserEvent: integer; aCenterCoor: TPointF);
    procedure ProcessSceneAfterPaint;
    function ClientCoorToColumnRowIndex(aPoint: TPoint): TPoint; // return column and row indexes of tile pointed by aPoint (client window coordinates)
    function ColumnRowIndexToClientCoor(aTile: TPoint): TPoint;  // return coordinate from tile's (column,row) indexes

  private // selection
    FTileArrayToPaste: TArrayOfArrayOfTile;
    function SelectionAvailable: boolean;
    function TileCountInSelection: TSize;
  private
    FClickedPosOrigin: TPoint;
    procedure DoLoopMoveSelection;
    procedure DoLoopMoveView;

  private
    procedure LoadCommonRessource;
    procedure FreeCommonRessource;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  private
    FMousePos: TPoint;
    function XCoorToColumnIndex( AX: integer ): integer;
    function YCoorToRowIndex( AY: integer ): integer;
    function ScreenPosToTileIndex(APos: TPoint): TPoint;

    procedure ShowActionText(aX, aY: single; aTxt: string);
  public
    function XYCoorIsInMap(AX, AY: integer): boolean;
    function ClientPosToTileIndex(APos: TPoint): TPoint;
    function ClientCoorToTileTopLeftCoor(aP: TPoint): TPoint;

    procedure ProcessTileEngineEvent(Sender: TTileEngine; const SceneTileCoor: TPointF; aTile: PTile);

    procedure SetWindowsTitle(const s: string);


  end;

var
  FormMain: TFormMain;

implementation
uses form_tools,
     form_askgroundtype,
     umaps, u_tileset_edit,
     u_tool_layer, u_tool_minimap,
     Clipbrd, Math, LazFileUtils;
{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FScene := TOGLCScene.Create (OpenGLControl1, -1);
  FScene.LayerCount := LAYER_COUNT;

  FScene.OnLoadCommonData := @LoadCommonRessource;
  FScene.OnFreeCommonData := @FreeCommonRessource;
  FScene.FontManager.ScanProjectFont(CleanAndExpandDirectory(Application.Location+PathDelim+'..'+PathDelim+'..'+
            PathDelim+'..'+PathDelim+'examples'+PathDelim+'Data'+PathDelim+'Fonts'+PathDelim));

  Application.OnIdle := @ProcessApplicationIdle;
  FScene.OnAfterPaint := @ProcessSceneAfterPaint;
  FState := sNeutral;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
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

// user change the view
procedure TFormMain.ComboBox1Change(Sender: TObject);
begin
 case ComboBox1.ItemIndex of
  0: SetViewOnLayer;
  1: SetViewOnTileSetEdit;
  2: SetViewOnPatternList;
 end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
 FScene.Free;
 FScene := NIL;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var delta: integer;
  p: TPointF;
begin
 EvalKey( Key );

 FMousePos := OpenGlControl1.ScreenToClient( Mouse.CursorPos );
 // shortcut key to copy, paste, cut, erase
 case Key of
  VK_SPACE: begin
    Menu_FillWithSelectedTileClick(self);
  end;

  VK_DELETE, VK_BACK: begin
    MIClearClick(self);
  end;

  VK_X : if ssCtrl in Shift then begin
    MICutClick( self );
  end;

  VK_C : if ssCtrl in Shift then begin
    MICopyClick( self );
  end;

  VK_V : if ssCtrl in Shift then begin
    MIPasteClick( self );
  end;

  VK_H: MIFlipHClick(MIFlipH);
  VK_J: MIFlipHClick(MIFlipV);

  VK_RETURN: if FState = sMovingSelection then
    FState := sNeutral;

  // move view with arrow keys
  VK_RIGHT: begin
    p := FWorkingTileEngine.PositionOnMap.Value;
    p.x := p.x + FWorkingTileEngine.TileSize.cx*2;
    delta := (FScene.Width div FWorkingTileEngine.TileSize.cx)*FWorkingTileEngine.TileSize.cx;
    if p.x > FWorkingTileEngine.MapSize.cx - delta then
       p.x := FWorkingTileEngine.MapSize.cx - delta;
    FWorkingTileEngine.PositionOnMap.Value := p;
  end;
   VK_LEFT: begin
    p := FWorkingTileEngine.PositionOnMap.Value;
    p.x := p.x - FWorkingTileEngine.TileSize.cx*2;
    if p.x < 0 then p.x := 0;
    FWorkingTileEngine.PositionOnMap.Value := p;
  end;
   VK_DOWN: begin
     p := FWorkingTileEngine.PositionOnMap.Value;
     p.y := p.y + FWorkingTileEngine.TileSize.cx*2;
     delta := (FScene.Height div FWorkingTileEngine.TileSize.cy)*FWorkingTileEngine.TileSize.cy;
     if p.y > FWorkingTileEngine.MapSize.cy - delta then
       p.y := FWorkingTileEngine.MapSize.cy - delta;
     FWorkingTileEngine.PositionOnMap.Value := p;
   end;
   VK_UP: begin
     p := FWorkingTileEngine.PositionOnMap.Value;
     p.y := p.y - FWorkingTileEngine.TileSize.cx*2;
     if p.y < 0 then p.y := 0;
     FWorkingTileEngine.PositionOnMap.Value := p;
   end;
 end;//case
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  // force Panel 2 to have the same larger than the window
  // we don't use Panel2.Align:=alTop because the clientRect is modified by LCL...
  Panel2.SetBounds(0, 0, ClientWidth, Panel2.Height);

  FormTools.Left := Width - FormTools.Width;
  FormTools.Top := Panel5.Height;
  FormTools.Show;
end;

function TFormMain.ClientPosToTileIndex(APos: TPoint): TPoint;
begin
  Result.x := XCoorToColumnIndex(APos.x);
  Result.y := YCoorToRowIndex(APos.y);
end;

function TFormMain.ScreenPosToTileIndex(APos: TPoint): TPoint;
begin
  Result := ClientPosToTileIndex(OpenGlControl1.ScreenToClient(APos));
end;

// popup Clear tile
procedure TFormMain.MIClearClick(Sender: TObject);
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
procedure TFormMain.MISetGroundTypeClick(Sender: TObject);
var pt: PTile;
  p1: TPoint;
  s: TSize;
  ro, co: integer;
begin
 if not XYCoorIsInMap( FMousePos.x, FMousePos.y ) then exit;
 if FormAskGroundType.ShowModal = mrCancel then exit;

 if not SelectionAvailable then begin  // set ground type on tile under the mouse
      p1 := ClientPosToTileIndex( FMousePos );
      pt:=FWorkingTileEngine.GetPTile(p1.y, p1.x);
      FWorkingTileEngine.SetGroundType( pt^.TextureIndex,pt^.ixFrame, pt^.iyFrame, FormAskGroundType.LB.ItemIndex);
      SetProjectModified;
 end else begin                       // set ground type on the selected tiles
      s := TileCountInSelection;
      p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
      for ro:=p1.y to p1.y+s.cy-1 do
       for co:=p1.x to p1.x+s.cx-1 do begin
         pt:=FWorkingTileEngine.GetPTile(ro, co);
         FWorkingTileEngine.SetGroundType( pt^.TextureIndex, pt^.ixFrame, pt^.iyFrame, FormAskGroundType.LB.ItemIndex);
       end;
      ShowActionText( FMousePos.x, FMousePos.y, 'set ground type' );
      SetProjectModified;
 end;
end;

// popup Keep as pattern
procedure TFormMain.MIKeepAsPAtternClick(Sender: TObject);
begin
 showmessage('Keep...');
 if WorkingPattern.TileCount>0 then begin
   PatternList.AddWorkingPattern;
   ShowActionText( FMousePos.x, FMousePos.y, '1 pattern added' );
   SetProjectModified;
 end;
end;

// popup Copy map position to ClipBoard
procedure TFormMain.MICopyMapPositionToClipboardClick(Sender: TObject);
var p: TPoint;
begin
 p:=OpenGLControl1.ScreenToClient( Mouse.CursorPos );
 Clipboard.AsText := inttostr(round(p.x-FWorkingTileEngine.X.Value)) + ',' + inttostr(round(p.y-FWorkingTileEngine.Y.Value));
 ShowActionText( p.x, p.y, 'Map position copied to clipboard');
end;

// popup flip H
procedure TFormMain.MIFlipHClick(Sender: TObject);
var p1: TPoint;
  s: TSize;
  t: string;
  ro, co: integer;
begin
  if Sender = MIFlipH then begin
   if not SelectionAvailable then begin
     // toggle flipH on tile under mouse
    if XYCoorIsInMap(FMousePos.x, FMousePos.y) then begin
      p1 := ClientPosToTileIndex(FMousePos);
      FWorkingTileEngine.ToggleTileFlipH(p1.y, p1.x);
      ShowActionText(FMousePos.x, FMousePos.y, 'toggle FLIPH');
      SetProjectModified;
    end;
   end else begin
     // toggle flipH on selection
     s := TileCountInSelection;
     p1 := ClientCoorToColumnRowIndex(FCoorBeginLeftClick);
     for ro:=p1.y to p1.y+s.cy-1 do
      for co:=p1.x to p1.x+s.cx-1 do
        FWorkingTileEngine.ToggleTileFlipH(ro, co);

     t := 'toggle '+inttostr(s.cx * s.cy)+' FLIPH';
     ShowActionText(FMousePos.x, FMousePos.y, t);
     SetProjectModified;
   end;
  end;

  if Sender = MIFlipV then begin
   if not SelectionAvailable then begin
     // toggle flipV on tile under mouse
    p1 := ClientPosToTileIndex(FMousePos);
    FWorkingTileEngine.ToggleTileFlipV(p1.y, p1.x);
    ShowActionText( FMousePos.x, FMousePos.y, 'toggle FLIPV');
    SetProjectModified;
   end else begin
     // toggle flipV on selection
     s := TileCountInSelection;
     p1 := ClientCoorToColumnRowIndex(FCoorBeginLeftClick);
     for ro:=p1.y to p1.y+s.cy-1 do
      for co:=p1.x to p1.x+s.cx-1 do
       FWorkingTileEngine.ToggleTileFlipV(ro, co);

     t := 'toggle '+inttostr(s.cx * s.cy)+' FLIPV';
     ShowActionText(FMousePos.x, FMousePos.y, t);
     SetProjectModified;
   end;
  end;

  if Sender = MINoFlip then begin
   if not SelectionAvailable then begin
     // toggle flipV on tile under mouse
    p1 := ClientPosToTileIndex(FMousePos);
    FWorkingTileEngine.SetTileFlipIndex(p1.y, p1.x, 0);
    ShowActionText( FMousePos.x, FMousePos.y, 'no FLIP');
    SetProjectModified;
   end else begin
     // toggle flipV on selection
     s := TileCountInSelection;
     p1 := ClientCoorToColumnRowIndex(FCoorBeginLeftClick);
     for ro:=p1.y to p1.y+s.cy-1 do
      for co:=p1.x to p1.x+s.cx-1 do
       FWorkingTileEngine.SetTileFlipIndex(ro, co, 0);

     t := 'no FLIP on '+inttostr(s.cx * s.cy)+' tiles';
     ShowActionText(FMousePos.x, FMousePos.y, t);
     SetProjectModified;
   end;
  end;
end;

procedure TFormMain.MIMirrorHClick(Sender: TObject);
var p1: TPoint;
  s: TSize;
  t: string;
  ro, coLeft, coRight: integer;
  roUp, roDown, co: integer;
begin
  if Sender = MIMirrorH then begin
   // mirrorH on selection
   s := TileCountInSelection;
   p1 := ClientCoorToColumnRowIndex(FCoorBeginLeftClick);
   for ro:=p1.y to p1.y+s.cy-1 do begin
     coLeft := p1.x;
     coRight := p1.x + s.cx - 1;
     repeat
       FWorkingTileEngine.ToggleTileFlipH(ro, coLeft);
       if coLeft < coRight then begin
         FWorkingTileEngine.ToggleTileFlipH(ro, coRight);
         FWorkingTileEngine.ExchangeCell(ro, coLeft, ro, coRight);
       end;
       inc(coLeft);
       dec(coRight);
     until coleft > coRight;
   end;
   t := 'mirrorH on '+inttostr(s.cx * s.cy)+' tiles';
   ShowActionText(FMousePos.x, FMousePos.y, t);
   SetProjectModified;
  end;

  if Sender = MIMirrorV then begin
    // mirrorV on selection
    s := TileCountInSelection;
    p1 := ClientCoorToColumnRowIndex(FCoorBeginLeftClick);
    for co:=p1.x to p1.x+s.cx-1 do begin
      roUp := p1.y;
      roDown := p1.y + s.cy - 1;
      repeat
        FWorkingTileEngine.ToggleTileFlipV(roUp, co);
        if roUp < roDown then begin
          FWorkingTileEngine.ToggleTileFlipV(roDown, co);
          FWorkingTileEngine.ExchangeCell(roUp, co, roDown, co);
        end;
        inc(roUp);
        dec(roDown);
      until roUp > roDown;
    end;
    t := 'mirrorV on '+inttostr(s.cx * s.cy)+' tiles';
    ShowActionText(FMousePos.x, FMousePos.y, t);
    SetProjectModified;
  end;
end;

// popup Set user event value
procedure TFormMain.MIEventClick(Sender: TObject);
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
procedure TFormMain.MIDeleteEventClick(Sender: TObject);
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

procedure TFormMain.MICopyEventToClipboardClick(Sender: TObject);
begin
 Form_ExportEvent.ShowModal;
end;

// popup 'put all tiles in the current tileset'
procedure TFormMain.MIPutAllTilesetClick(Sender: TObject);
var ro, co: integer;
    p1: TPoint;
    Ftl: TTileSet;
begin
 if FormTools.CB1.ItemIndex=-1 then exit;

 p1 := ClientPosToTileIndex( FMousePos );
 Ftl := TileSetManager.TileSet[FormTools.CB1.ItemIndex];
 for ro:=p1.y to p1.y+Ftl.YTileCount-1 do
  for co:=p1.x to p1.x+Ftl.XTileCount-1 do
   begin
    FWorkingTileEngine.SetCell( ro, co, FormTools.CB1.ItemIndex, co-p1.x, ro-p1.y );
    FWorkingTileEngine.SetUserEventValue( ro, co, -1 );
   end;

 ShowActionText( FMousePos.x, FMousePos.y, 'put whole tileset');
 SetProjectModified;
end;

// popup Cut
procedure TFormMain.MICutClick(Sender: TObject);
var ro, co: integer;
    s: TSize;
    p1: TPoint;
    t: string;
begin
 if TilesetEdit.IsActive then exit;

 if not SelectionAvailable then
   begin // cut tile under mouse
    if XYCoorIsInMap( FMousePos.x, FMousePos.y ) then begin
      MICopyClick( Sender );
      p1 := ClientPosToTileIndex( FMousePos );
      FWorkingTileEngine.SetCell( p1.y, p1.x, -1, -1, -1 );
      FWorkingTileEngine.SetUserEventValue( p1.y, p1.x, -1 );
      FWorkingTileEngine.SetTileFlipIndex(p1.y, p1.x, 0);
      ShowActionText( FMousePos.x, FMousePos.y, 'cut 1 tile');
      SetProjectModified;
    end;
   end else begin // cut selection
     // copy selected area
     MICopyClick( Sender );
     // erase selected area
     s := TileCountInSelection;
     p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
     for ro:=p1.y to p1.y+s.cy-1 do
      for co:=p1.x to p1.x+s.cx-1 do
       begin
        FWorkingTileEngine.SetCell( ro, co, -1, -1, -1 );
        FWorkingTileEngine.SetUserEventValue( ro, co, -1 );
        FWorkingTileEngine.SetTileFlipIndex(ro, co, 0);
       end;

     t := 'cut ' + inttostr( s.cx * s.cy ) + ' tile';
     if s.cx * s.cy > 1 then t+='s';
     ShowActionText( FMousePos.x, FMousePos.y, t );
     SetProjectModified;
   end;
end;

// popup fill with selected tile
procedure TFormMain.Menu_FillWithSelectedTileClick(Sender: TObject);
var ro, co, t, ixfr, iyfr : integer;
    s: TSize;
    p1: TPoint;
    tt: string;
begin
 if TilesetEdit.IsActive then exit;

 FormTools.GetTextureAndFrameindex( t, ixfr, iyfr );
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
procedure TFormMain.MICopyClick(Sender: TObject);
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
      FTileArrayToPaste[0][0].FlipIndex := p^.FlipIndex;
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
         FTileArrayToPaste[ro-p1.y][co-p1.x].FlipIndex := p^.FlipIndex;
        end;

     tt := 'copy ' + inttostr( s.cx * s.cy ) + ' tile';
     if s.cx * s.cy > 1 then tt+='s';
     ShowActionText( FMousePos.x, FMousePos.y, tt);
   end;
end;

// popup Paste
procedure TFormMain.MIPasteClick(Sender: TObject);
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
     p3 := @FTileArrayToPaste[ro-p1.y][co-p1.x];
     FWorkingTileEngine.SetCell( ro, co, p3^.TextureIndex, p3^.ixFrame, p3^.iyFrame );
     FWorkingTileEngine.SetUserEventValue( ro, co, p3^.UserEvent );
     FWorkingTileEngine.SetTileFlipIndex(ro, co, p3^.FlipIndex);
    end;

  i := Length( FTileArrayToPaste ) * Length( FTileArrayToPaste[0] );
  t := 'paste ' + inttostr( i ) + ' tile';
  if i > 1 then t+='s';
  ShowActionText( FMousePos.x, FMousePos.y, t);
  SetProjectModified;
end;


// popup insert row
procedure TFormMain.MIInsertRowClick(Sender: TObject);
var it: TPoint;
    t: string;
begin
  if TilesetEdit.IsActive then exit;

  it := ClientPosToTileIndex( FMousePos );
  FormInsertLineColumn.Label1.Caption := 'Row to insert :';
  if FormInsertLineColumn.ShowModal = mrCancel then exit;

  MapList.InsertRow( it.y, FormInsertLineColumn.SE1.Value );

  t := 'insert ' + inttostr( FormInsertLineColumn.SE1.Value ) + ' row';
  if FormInsertLineColumn.SE1.Value > 1 then t+='s';
  ShowActionText( FMousePos.x, FMousePos.y, t);

  FormTools.UpdateMapParameterOnScreen;

  SetProjectModified;
end;

// popup insert columns
procedure TFormMain.MIInsertColumnClick(Sender: TObject);
var it: TPoint;
  t: string;
begin
  if TilesetEdit.IsActive then exit;

  it := ClientPosToTileIndex( FMousePos );
  FormInsertLineColumn.Label1.Caption := 'Column to insert :';
  if FormInsertLineColumn.ShowModal = mrCancel then exit;

  MapList.InsertColumn( it.x, FormInsertLineColumn.SE1.Value );

  t := 'insert ' + inttostr( FormInsertLineColumn.SE1.Value ) + ' column';
  if FormInsertLineColumn.SE1.Value > 1 then t+='s';
  ShowActionText( FMousePos.x, FMousePos.y, t);
  SetProjectModified;
end;

// menu Start map on this tile
procedure TFormMain.Menu_StartMapClick(Sender: TObject);
var it: TPoint;
begin
  it := ClientPosToTileIndex( FMousePos );

  FormTools.Label10.Caption := inttostr( it.x );
  FormTools.Label11.Caption := inttostr( it.y );
  ShowActionText( FMousePos.x, FMousePos.y, 'Define start map at this tile');
  SetProjectModified;
end;

procedure TFormMain.MIMoveSelectionClick(Sender: TObject);
begin
  if SelectionAvailable then
    DoLoopMoveSelection;
end;

procedure TFormMain.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then begin    // left clic all version
   FState := sNeutral;
   FCoorBeginLeftClick.x := X;
   FCoorBeginLeftClick.y := Y;
 end;

 if ( Button = mbLeft ) and not   // left clic alone = start moving view and unselect previous selection
    (( ssShift in Shift )or( ssCtrl in Shift )or( ssAlt in Shift )) then begin
{   FState := sMoveView;
   FOriginTileEngine.x := round(FWorkingTileEngine.X.Value);
   FOriginTileEngine.y := round(FWorkingTileEngine.Y.Value);
   WorkingPattern.Clear;  }
   FState := sSelecting;
   FCoorEndLeftClick := FCoorBeginLeftClick;
 end;

{ if ( Button = mbLeft ) and ( ssShift in Shift )   // left clic + SHIFT = start a rectangular selection without clear the previous selection
    and not( ssCtrl in Shift ) and XYCoorIsInMap( X, Y ) then begin
   FState := sSelecting;
   FCoorEndLeftClick := FCoorBeginLeftClick;
 end;  }

 if ( Button = mbLeft ) and ( ssCtrl in Shift ) then begin  // left clic + CTRL = add a single tile to selection
   if XYCoorIsInMap( X, Y ) then begin
     WorkingPattern.AddOrRemoveTileUnderMouse;
   end;
 end;

  if Button = mbMiddle then begin
    FClickedPosOrigin := Point(X, Y);
    DoLoopMoveView;
  end;
end;

procedure TFormMain.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var it: TPoint;
    tc: TSize;
    i: integer;
    p:PTile;
    s: string;
begin
 if not FReady then exit;

 // update info on screen
 if XYCoorIsInMap( X, Y ) then begin

  FLabelMapPosition.Caption := 'Map position:  (' + inttostr(Round(X+FWorkingTileEngine.PositionOnMap.x.Value)) +
                               ',' + inttostr(round(Y+FWorkingTileEngine.PositionOnMap.y.Value)) + ')';

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
                           ' ('+ inttostr(p^.UserEvent)+')';
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
        else if PatternList.IsActive
          then PatternList.SetTileEngineCoordinates( FOriginTileEngine.x + X - FCoorBeginLeftClick.x, FOriginTileEngine.y + Y - FCoorBeginLeftClick.y )
        else MapList.MoveTileEngineTo(FOriginTileEngine.x + X - FCoorBeginLeftClick.x, FOriginTileEngine.y + Y - FCoorBeginLeftClick.y); //    .SetTileEngineCoordinates( FOriginTileEngine.x + X - FCoorBeginLeftClick.x, FOriginTileEngine.y + Y - FCoorBeginLeftClick.y );
         // FWorkingTileEngine.SetCoordinate( FOriginTileEngine.x + X - FCoorBeginLeftClick.x, FOriginTileEngine.y + Y - FCoorBeginLeftClick.y );
     end;
     sNeutral: begin
       if XYCoorIsInMap( X, Y) then begin
         FCoorBeginLeftClick := Point(X, Y);
         FCoorEndLeftClick :=FCoorBeginLeftClick;
         FLabelSelectionInfo.Caption := 'Selection:';
       end;
     end;
     sMovingSelection: begin


     end;
 end;
end;

procedure TFormMain.OpenGLControl1MouseUp(Sender: TObject;
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
     sMoveView: FState := sNeutral;
    end;
  end;

  if (Button = mbRight) and XYCoorIsInMap(X, Y) and
    ((FState = sSelectionDone) or (FState = sNeutral))then begin
    // popup menu
    FMousePos := Point(X,Y);
    p := ClientToScreen(Point(X,Y));

    PopupMenu1.PopUp( p.x, p.y);
  end;

  case Button of
   mbMiddle: begin
     if FState = sMovingView then
       FState := sNeutral;
   end;
  end;
end;

procedure TFormMain.PopupMenu1Popup(Sender: TObject);
begin
  MIMirrorH.Enabled := SelectionAvailable;
  MIMirrorV.Enabled := MIMirrorH.Enabled;
end;


// click on help button
procedure TFormMain.SBHelp3Click(Sender: TObject);
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
             'RIGHT CLICK -> Tile contextual menu'+lineending+
             '  - START GAME ON THIS TILE: when you will load the map in your game application,'+lineending+
             '    the position will be set automatically to draw this tile on top/left view.'+lineending+
             '  - EVENT... -> To set an event on the tile under the mouse or in selected area.'+lineending+
             '  - DELETE EVENT -> To remove an event on the tile under the mouse or in selected area.'+lineending+
             '  - COPY EVENT TO CLIPBOARD -> Copy the event list in clipboard, so you can'+lineending+
             '    paste them under Lazarus, in your game application.' );
end;

// load session
procedure TFormMain.SpeedButton1Click(Sender: TObject);
var mr: TModalResult;
begin
  if FProjectIsModified then begin
    mr := MessageDlg('', 'Before load, do you want to save the current(s) map(s) ?', mtWarning, [mbYes, mbNo, mbCancel],0);
    if mr = mrCancel then exit;
    if mr = mrYes then MapList.SaveSession;
    FProjectIsModified := FALSE;
  end;

  MapList.LoadSession;
end;

// save session
procedure TFormMain.SpeedButton2Click(Sender: TObject);
begin
  MapList.SaveSession;
end;

procedure TFormMain.SpeedButton3Click(Sender: TObject);
begin
  if Form_Minimap = NIL then exit;
  if Form_Minimap.Visible then Form_Minimap.Hide
    else Form_Minimap.Show;
end;

//
procedure TFormMain.SpeedButton5Click(Sender: TObject);
begin
  Form_ToolLayer.Show;
end;

procedure TFormMain.ClearEventLabels;
begin
  FEventLabelCount := 0;
end;

procedure TFormMain.AddEventLabel(aUserEvent: integer; aCenterCoor: TPointF);
begin
  inc(FEventLabelCount);
  if Length(FEventLabels) <= FEventLabelCount then
    SetLength(FEventLabels, Length(FEventLabels) + 10);

  FEventLabels[FEventLabelCount-1].UserEvent := aUserEvent;
  FEventLabels[FEventLabelCount-1].CenterCoor := aCenterCoor;
end;


function TFormMain.XYCoorIsInMap(AX, AY: integer): boolean;
var p: TPointF;
begin
  if FWorkingTileEngine = NIL then exit(False);

  p := FWorkingTileEngine.PositionOnMap.Value + PointF(AX, AY);

  Result := Inrange(p.x, 0, FWorkingTileEngine.MapSize.cx) and
            Inrange(p.y, 0, FWorkingTileEngine.MapSize.cy);


{  with FWorkingTileEngine do
  Result := ( AX >= X.Value ) and ( AX < X.Value + Width ) and
            ( AY >= Y.Value ) and ( AY < Y.Value + Height );  }
end;

procedure TFormMain.ShowActionText(aX, aY: single; aTxt: string);
var o : TFreeText;
  rx, ry, time: single;
begin
  o := TFreeText.Create(FScene);
  o.TexturedFont := FHintFont;
  o.Caption := aTxt;
  o.SetCenterCoordinate(aX, aY);
  FScene.Add(o, Layer_InfoMap);
  time := 2.0;
  o.Opacity.ChangeTo(0, time, idcStartSlowEndFast);
  if aX > FScene.Width/2
    then rx := -100
    else rx := 100;
  if aY > FScene.Height/2
    then ry := -100
    else ry := 100;
  o.MoveRelative(rx, ry, time, idcSinusoid);
  o.KillDefered(time);
end;

procedure TFormMain.ProcessTileEngineEvent(Sender: TTileEngine;
  const SceneTileCoor: TPointF; aTile: PTile);
var p: TPointF;
begin
  if aTile^.UserEvent = -1 then exit;

  p := SceneTileCoor + PointF(FWorkingTileEngine.TileSize.cx*0.5, FWorkingTileEngine.TileSize.cy*0.5);
  AddEventLabel(aTile^.UserEvent, p);
end;

procedure TFormMain.SetWindowsTitle(const s: string);
begin
 Caption := 'TILE MAP DESIGNER - ' + s;
end;


function TFormMain.XCoorToColumnIndex(AX: integer): integer;
begin
  Result := Round(AX + FWorkingTileEngine.PositionOnMap.x.Value) div FWorkingTileEngine.TileSize.cx;
//  Result := Trunc((AX + FWorkingTileEngine.PositionOnMap.x.Value) / FWorkingTileEngine.TileSize.cx);

  Result := EnsureRange(Result, 0, FWorkingTileEngine.MapTileCount.cx-1);
end;

function TFormMain.YCoorToRowIndex(AY: integer): integer;
begin
  Result := Round(AY + FWorkingTileEngine.PositionOnMap.y.Value) div FWorkingTileEngine.TileSize.cy;
  //Result := Trunc((AY + FWorkingTileEngine.PositionOnMap.y.Value) / FWorkingTileEngine.TileSize.cy);
  Result := EnsureRange(Result, 0, FWorkingTileEngine.MapTileCount.cy-1);
end;

function TFormMain.ClientCoorToColumnRowIndex(aPoint: TPoint): TPoint;
begin
  Result.x := XCoorToColumnIndex(aPoint.x);
  Result.y := YCoorToRowIndex(aPoint.y);
end;

function TFormMain.ColumnRowIndexToClientCoor(aTile: TPoint): TPoint;
begin
  Result.x := aTile.x * FWorkingTileEngine.TileSize.cx - Round(FWorkingTileEngine.PositionOnMap.x.Value);

  Result.y := aTile.y * FWorkingTileEngine.TileSize.cy - Round(FWorkingTileEngine.PositionOnMap.y.Value);
end;

function TFormMain.ClientCoorToTileTopLeftCoor(aP: TPoint): TPoint;
var p: TPoint;
begin
  if (FWorkingTileEngine.TileSize.cx = 0) or (FWorkingTileEngine.TileSize.cy = 0) then begin
    Result.x := 0;
    Result.y := 0;
  end else begin
    p := ClientCoorToColumnRowIndex(aP);
    p.x := p.x - FWorkingTileEngine.GetFirstTileColumnIndex;
    p.y := p.y - FWorkingTileEngine.GetFirstTileRowIndex;
    Result.x := p.x * FWorkingTileEngine.TileSize.cx;
    Result.y := p.y * FWorkingTileEngine.TileSize.cy;
  end;
end;

function TFormMain.SelectionAvailable: boolean;
begin
  Result := FState = sSelectionDone;
end;

function TFormMain.TileCountInSelection: TSize;
var p1, p2: TPoint;
begin
  p1 := ClientCoorToColumnRowIndex( FCoorBeginLeftClick );
  p2 := ClientCoorToColumnRowIndex( FCoorEndLeftClick );

  Result.cx := p2.x - p1.x + 1;
  Result.cy := p2.y - p1.y + 1;
end;

procedure TFormMain.DoLoopMoveSelection;
var pattern, bg: TArrayOfArrayOfTile;
  posOrigin, currentPos, previousTilePos, currentTilePos: TPoint;
  s: TSize;
  patternMoved: boolean;
begin
  if FState = sMovingSelection then exit;
  FState := sMovingSelection;

  pattern := NIL;
  bg := NIL;
  patternMoved := False;

  posOrigin := FCoorBeginLeftClick;
  s := TileCountInSelection;

  // retrieve the pattern to move
  previousTilePos := ClientCoorToColumnRowIndex(posOrigin);
  FWorkingTileEngine.CopyPartTo(pattern, previousTilePos, s);

  // save the existing background into the background buffer
  FWorkingTileEngine.CopyPartTo(bg, previousTilePos, s);

  repeat
    currentPos := OpenGLControl1.ScreenToClient(Mouse.CursorPos);
    if XYCoorIsInMap(currentPos.x, currentPos.y) and
       ((currentPos.x <> posOrigin.x) or (currentPos.y <> posOrigin.y)) then begin

      // copy background buffer into the old position
      previousTilePos := ClientCoorToColumnRowIndex(posOrigin);
      FWorkingTileEngine.CopyPartFrom(bg, previousTilePos);

      // save the tiles at the new position
      currentTilePos := ClientCoorToColumnRowIndex(currentPos);
      FWorkingTileEngine.CopyPartTo(bg, currentTilePos, s);

      // put the pattern at the new position
      FWorkingTileEngine.CopyPartFrom(pattern, currentTilePos);

      posOrigin := currentPos;
      patternMoved := True;
    end;
    Application.ProcessMessages;
    FScene.DoLoop;
    Sleep(1);
  until FState <> sMovingSelection;

  if patternMoved then begin
    ShowActionText(currentPos.x, currentPos.y, (s.cx*s.cy).ToString+' tiles moved');
    SetProjectModified;
  end;
end;

procedure TFormMain.DoLoopMoveView;
var currentPos, delta: TPoint;
  p: TPointF;
begin
  if FState = sMovingView then exit;
  FState := sMovingView;

  repeat
    currentPos := OpenGLControl1.ScreenToClient(Mouse.CursorPos);
    delta := currentPos - FClickedPosOrigin;
    if (delta.x <> 0) or (delta.y <> 0) then begin
      if delta.x > 0 then delta.x := FWorkingTileEngine.TileSize.cx
        else delta.x := -FWorkingTileEngine.TileSize.cx;
      if delta.y > 0 then delta.y := FWorkingTileEngine.TileSize.cy
        else delta.y := -FWorkingTileEngine.TileSize.cy;
      p := FWorkingTileEngine.PositionOnMap.Value + PointF(delta);
      p.x := EnsureRange(p.x, 0, FWorkingTileEngine.MapSize.cx-FScene.Width);
      p.y := EnsureRange(p.y, 0, FWorkingTileEngine.MapSize.cy-FScene.Height);
      FWorkingTileEngine.PositionOnMap.Value := p;
    end;
    Application.ProcessMessages;
    FScene.DoLoop;
    Sleep(80);
  until FState <> sMovingView;
end;

procedure TFormMain.ProcessSceneAfterPaint;
var p1, p2: TPoint;
  o: TFreeText;
  i: integer;
begin
  if MapList.Count = 0 then exit;
  if FWorkingTileEngine = NIL then exit;
  if (FWorkingTileEngine.TileSize.cx = 0) or (FWorkingTileEngine.TileSize.cy = 0) then exit;

  FScene.BlendMode := FX_BLEND_NORMAL;

  // draw the event labels
  if FEventLabelCount > 0 then begin
    o := TFreeText.Create(FScene);
    o.TexturedFont := FEventFont;
    for i:=0 to FEventLabelCount-1 do begin
      o.Caption := GetStrEvent(FEventLabels[i].UserEvent);
      o.SetCenterCoordinate(FEventLabels[i].CenterCoor);
     // FScene.FlushRenderer;
      FillBox(FScene, o.X.Value-3, o.Y.Value-3, o.Width+6, o.Height+6, BGRA(0,0,0,180));
     // FScene.FlushRenderer;
      o.Draw(1.0);
    end;
    o.Free;
  end;
  ClearEventLabels;

  if (FState = sSelecting) or (FState = sSelectionDone) then begin
    // rectangular selection area
    p1 := ClientCoorToTileTopLeftCoor(FCoorBeginLeftClick);
    p2 := ClientCoorToTileTopLeftCoor(FCoorEndLeftClick);
    p2.x := p2.x + FWorkingTileEngine.TileSize.cx - p1.x;
    p2.y := p2.y + FWorkingTileEngine.TileSize.cy - p1.y;

    DrawBox(FScene, p1.x, p1.y, p2.x, p2.y, BGRA(255,255,255,190), 1.5);
  end else begin  // tile under the mouse
    p1 := OpenGLControl1.ScreenToClient(Mouse.CursorPos);
    if XYCoorIsInMap(p1.x, p1.y) then begin
      //Result := Result - FWorkingTileEngine.GetFirstTileColumnIndex;
      p1 := ClientCoorToTileTopLeftCoor(p1);
      DrawBox(FScene, p1.x, p1.y, FWorkingTileEngine.TileSize.cx, FWorkingTileEngine.TileSize.cy, BGRA(255,0,0), 1.5);
    end;
  end;

  // draw a box around the map boundary
  DrawBox(FScene, FWorkingTileEngine.X.Value, FWorkingTileEngine.Y.Value,
          FWorkingTileEngine.Width, FWorkingTileEngine.Height,
          BGRA(50,50,50), 1);

  // draw a box around selection in WorkingPattern
  WorkingPattern.DrawBoxAroundTiles;
end;

procedure TFormMain.LoadCommonRessource;
var ima: TBGRABitmap;
    w: integer;
begin
  // white/gray image for grid
  w := ScaleDesignToForm(8);
  ima := TBGRABitmap.Create(w, w, BGRA(220,220,220));
  FImageBackGround := TBGRABitmap.Create(w*2, w*2, BGRAWhite);
  FImageBackGround.PutImage(w, 0, ima, dmSet, 255);
  FImageBackGround.PutImage(0, w, ima, dmSet, 255);
  ima.Free;

  ScreenMap := TScreenMap.Create;
  FScene.RunScreen(ScreenMap);

  TileSetManager := TTilesetManager.Create;
end;

procedure TFormMain.FreeCommonRessource;
begin
  FImageBackGround.Free;
  MapList.Free;
  TileSetEdit.Free;
  TileSetManager.Free;
  PatternList.Free;

  ScreenMap.Free;

  Application.OnIdle := NIL;
end;

procedure TFormMain.ProcessApplicationIdle(Sender: TObject;
  var Done: Boolean);
begin
  if FScene <> NIL then FScene.DoLoop;
  Done := FALSE;
end;

end.

