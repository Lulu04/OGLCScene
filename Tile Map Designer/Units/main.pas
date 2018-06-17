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
unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  LCLType, LCLIntf,
  StdCtrls, ExtCtrls, Menus, Spin, ComCtrls,
  BGRABitmap, BGRABitmapTypes,
  common,
  VelocityCurve,
  WindowsScene,
  tileset_manager,
  uAskTileSize,
  uabout,
  usavemap,
  uaskgroundtype,
  uAskMapSize,
  uexportgroundtype;

const
  MAX_PAINTBOX_SIZE = 320;

type

  { TForm_Principale }

  TForm_Principale = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CB1: TComboBox;
    CB10: TCheckBox;
    CB11: TCheckBox;
    CB2: TCheckBox;
    CB3: TCheckBox;
    CB4: TComboBox;
    CB5: TCheckBox;
    CB6: TCheckBox;
    CB7: TCheckBox;
    CB8: TCheckBox;
    CB9: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CB12: TCheckBox;
    ColorButton1: TColorButton;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LBJeux: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    Menu_ExportGroundType: TMenuItem;
    Menu_ClearAllMap: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    OD1: TOpenDialog;
    OD2: TOpenDialog;
    PageControl1: TPageControl;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PB2: TPaintBox;
    Panel1: TPanel;
    PB1: TPaintBox;
    PopupMenu1: TPopupMenu;
    RB1: TRadioButton;
    RB2: TRadioButton;
    SBHelp2: TSpeedButton;
    SBHelp3: TSpeedButton;
    SBV: TScrollBar;
    SBH: TScrollBar;
    SD1: TSaveDialog;
    pageMiniMap: TTabSheet;
    pageSetting: TTabSheet;
    SE3: TSpinEdit;
    SE4: TSpinEdit;
    pageOverlaying: TTabSheet;
    SE1: TSpinEdit;
    SE2: TSpinEdit;
    SE5: TSpinEdit;
    SE6: TSpinEdit;
    Shape1: TShape;
    Shape2: TShape;
    SBHelp1: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CB1Change(Sender: TObject);
    procedure CB4Change(Sender: TObject);
    procedure CB5Change(Sender: TObject);
    procedure CB7Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure Menu_ClearAllMapClick(Sender: TObject);
    procedure Menu_ExportGroundTypeClick(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure PB1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PB1Paint(Sender: TObject);
    procedure PB2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PB2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PB2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PB2Paint(Sender: TObject);
    procedure RB1Change(Sender: TObject);
    procedure SBHelp1Click(Sender: TObject);
    procedure SBHelp2Click(Sender: TObject);
    procedure SBHelp3Click(Sender: TObject);
    procedure SBVScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure SE1Change(Sender: TObject);
    procedure SE3Change(Sender: TObject);
    procedure SE6Change(Sender: TObject);
  private
    procedure LoadTextureToProject( AFilename: string; AFrameWidth, AFrameHeight: integer );
  private
    FSelectedCell: TPoint;
    FMiniMapTarget: TPoint;
  public
    procedure PB1SetSizeAndPos;
    procedure PB2SetSizeAndPos;

    procedure GetTextureAndFrameindex( out t, ixfr, iyfr: integer );
    procedure SetRelativeSelectedTile( aDeltaX, aDeltay: integer );

    procedure SetWindowsTitle( const s: string );
    procedure UpdateMapParameterOnScreen;

  end;

var
  Form_Principale: TForm_Principale;

implementation
{$R *.lfm}

{ TForm_Principale }

procedure TForm_Principale.FormCloseQuery(Sender: TObject; var CanClose: boolean );
var mr: TModalResult;
begin
 CanClose := FALSE;

 if FProjectIsModified then
 begin
  mr := MessageDlg('', 'Before quit, do you want to save the current map ?', mtWarning, [mbYes, mbNo, mbCancel],0);
  if mr = mrCancel then exit;
  if mr = mrYes then
  begin
    if not SD1.Execute then exit;
    DoSave( SD1.FileName );
  end;
  FProjectIsModified := FALSE;
 end;

 CanClose:=TRUE;
end;

procedure TForm_Principale.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
  VK_LEFT:;
  VK_RIGHT:;
 end;
end;

procedure TForm_Principale.FormMouseLeave(Sender: TObject);
begin
 Window_Scene.SetFocus;
end;

procedure TForm_Principale.FormShow(Sender: TObject);
begin
 Left := 0;
 Top := 0;
 Window_Scene.Show;
 Window_Scene.SetBounds( Left+Width, Top, Screen.Monitors[0].Width-Width, Height);
end;


// Open file map
procedure TForm_Principale.MenuItem3Click(Sender: TObject);
var i: integer;
   mr: TModalResult;
begin
 if FProjectIsModified then
 begin
  mr := MessageDlg('', 'Before load, do you want to save the current map ?', mtWarning, [mbYes, mbNo, mbCancel],0);
  if mr = mrCancel then exit;
  if mr = mrYes then
  begin
    if not SD1.Execute then exit;
    DoSave( SD1.FileName );
  end;
  FProjectIsModified := FALSE;
 end;

 if not OD2.Execute then exit;

 SD1.FileName := ExtractFileName( OD2.FileName );
 FTileEngine.LoadMapFile( OD2.FileName );

 FTileEngine.PositionOnMap.Value := PointF(0, 0);
 SBV.Position := 0;
 SBH.Position := 0;

 TileSetManager.LoadTileSetFromMapFile( OD2.FileName );

 // texture list
 CB1.Clear;
 for i:=0 to TileSetManager.Count-1 do CB1.Items.Add( TileSetManager.TileSet[i].Name );
 if CB1.Items.Count > 0 then CB1.ItemIndex := 0;
 PB1SetSizeAndPos;
 PB2SetSizeAndPos;

 UpdateMapParameterOnScreen;

 // set tileengine view to fit whole map
 FTileEngine.SetViewSize( FTileEngine.MapSize.cx, FTileEngine.MapSize.cy );

 SetWindowsTitle( ExtractFileName( OD2.FileName ) );
 FProjectIsModified := FALSE;
end;

procedure TForm_Principale.SetWindowsTitle( const s: string );
begin
 Caption := 'TILE MAP DESIGNER - ' + s;
end;

procedure TForm_Principale.UpdateMapParameterOnScreen;
var s: string;
begin
 // map tile count
 s := 'Column: ' + inttostr(FTileEngine.MapTileCount.cx) + ' tile';
 if FTileEngine.MapTileCount.cx > 1 then s+='s';
 Label5.Caption := s;
 s := 'Row: ' + inttostr(FTileEngine.MapTileCount.cy) + ' tile';
 if FTileEngine.MapTileCount.cx > 1 then s+='s';
 Label15.Caption := s;
 // Tile Size
 Label13.Caption := inttostr( FTileEngine.TileSize.cx );
 Label14.Caption := inttostr( FTileEngine.TileSize.cy );

 // Hole color
 ColorButton1.ButtonColor := BGRAToColor( FTileEngine.MapHoleColor.Value );
 SE6.Value := FTileEngine.MapHoleColor.Alpha.Value;

 // draw size
 if CB4.ItemIndex = 0 then
 begin
  SE3.Value := FTileEngine.Width;
  SE4.Value := FTileEngine.Height;
 end else
 begin
  SE3.Value := FTileEngine.MapTileCount.cx;
  SE4.Value := FTileEngine.MapTileCount.cy;
 end;

 // Scroll Loop Mode
 CB2.Checked := FTileEngine.HLoopMode;
 CB3.Checked := FTileEngine.VLoopMode;

 // Start tile
 Label10.Caption := inttostr( trunc( FTileEngine.PositionOnMap.x.Value / FTileEngine.TileSize.cx ));
 Label11.Caption := inttostr( trunc( FTileEngine.PositionOnMap.y.Value / FTileEngine.TileSize.cy ));
end;

// Save file map
procedure TForm_Principale.MenuItem4Click(Sender: TObject);
begin
 if not SD1.Execute then exit;
 DoSave( SD1.FileName );
 FProjectIsModified := FALSE;
 SetWindowsTitle( ExtractFileName( SD1.FileName ) );
end;

// set Paintbox size and pos
procedure TForm_Principale.PB1SetSizeAndPos;
var ll, tt, ww, hh: integer;
begin
 PB1.Invalidate;
 if CB1.ItemIndex = -1 then exit;
 with TileSetManager.TileSet[CB1.ItemIndex] do begin
  if MAX_PAINTBOX_SIZE div TileWidth > XTileCount then begin
    ww := XTileCount * TileWidth;
    ll := 5 + ( MAX_PAINTBOX_SIZE - ww ) div 2;
  end else begin
    ww := MAX_PAINTBOX_SIZE;
    ll := 5;
  end;

  if MAX_PAINTBOX_SIZE div TileHeight > YTileCount then begin
    hh := YTileCount * TileHeight;
    tt := 5;
  end else begin
   hh := MAX_PAINTBOX_SIZE;
   tt := 5;
  end;
 end;

 PB1.SetBounds( ll, tt, ww, hh );
end;

procedure TForm_Principale.PB2SetSizeAndPos;
var factor: single;
  ll, tt, ww, hh: integer;
begin
 if FTileEngine.TileSize.cx = 0 then exit;
 if FTileEngine.MapSize.cy = 0 then exit;
 if FTileEngine.MapSize.cx = 0 then exit;

 factor := FTileEngine.MapSize.cx / FTileEngine.MapSize.cy;
 if FTileEngine.MapSize.cx > FTileEngine.MapSize.cy
   then begin // map width > map height
     ll := 0;
     ww := Panel2.Width;
     hh := round( ww/factor );
     tt := round( (Panel2.Height-hh )/2 );
   end
   else begin
    tt := 0;
    hh := Panel2.Height;
    ww := round( hh*factor );
    ll := round( (Panel2.Width-ww)/2 );
   end;
 PB2.SetBounds( ll, tt, ww, hh );
 PB2.Invalidate;

 SBV.Max := FTileEngine.MapTileCount.cy;
 SBV.PageSize := round( FScene.Height / FTileEngine.TileSize.cy );
 SBV.LargeChange := SBV.PageSize;

 SBH.Max := FTileEngine.MapTileCount.cx;
 SBH.PageSize := round( FScene.Width / FTileEngine.TileSize.cx );
 SBH.LargeChange := SBH.PageSize;

end;




// delete Tileset
procedure TForm_Principale.Button2Click(Sender: TObject);
var i: integer;
begin
 if CB1.ItemIndex = -1 then exit;
 if MessageDlg ( '' , 'Remove this tileset from project ?'+lineending+CB1.Text , mtWarning , [mbYes,mbCancel] , 0 ) = mrCancel
   then exit;
 TileSetManager.Remove( CB1.ItemIndex );
 FTileEngine.DeleteTexture( CB1.ItemIndex );

 i := CB1.ItemIndex-1;
 CB1.Items.Delete( CB1.ItemIndex );
 if i > -1 then CB1.ItemIndex := i;

 PB1.Invalidate;
 SetProjectModified;
end;

// load overlay tileengine
procedure TForm_Principale.Button3Click(Sender: TObject);
begin
 if not OD2.Execute then exit;
 FTopOverlayTileEngine.LoadMapFile( OD2.FileName );
 FBelowOverlayTileEngine.LoadMapFile( OD2.FileName );

 FBelowOverlayTileEngine.SetCoordinate( FTileEngine.X.Value, FTileEngine.Y.Value );
 FBelowOverlayTileEngine.SetViewSize( FTileEngine.Width, FTileEngine.Height );

 FTopOverlayTileEngine.SetCoordinate( FTileEngine.X.Value, FTileEngine.Y.Value );
 FTopOverlayTileEngine.SetViewSize( FTileEngine.Width, FTileEngine.Height );

 // update parameter on screen
 with FTopOverlayTileEngine do
  begin
   CB8.Checked := HScrollEnable;
   CB10.Checked := HLoopMode;
   CB9.Checked := VScrollEnable;
   CB11.Checked := VLoopMode;
   Label44.Caption := inttostr( MapTileCount.cx );
   Label43.Caption := inttostr( MapTileCount.cy );
  end;

 SE1Change(Sender);
end;

// set map size
procedure TForm_Principale.Button4Click(Sender: TObject);
var s: string;
begin
 Form_AskMapSize.ShowModal;

 // map tile count
 s := 'Column: ' + inttostr(FTileEngine.MapTileCount.cx) + ' tile';
 if FTileEngine.MapTileCount.cx > 1 then s+='s';
 Label5.Caption := s;
 s := 'Row: ' + inttostr(FTileEngine.MapTileCount.cy) + ' tile';
 if FTileEngine.MapTileCount.cx > 1 then s+='s';
 Label15.Caption := s;

 PB2SetSizeAndPos;
end;

// tileset choice
procedure TForm_Principale.CB1Change(Sender: TObject);
begin
 PB1SetSizeAndPos;
 PB1.Invalidate;
end;

procedure TForm_Principale.CB4Change(Sender: TObject);
begin
 if CB4.ItemIndex = 1 then
 begin
   Label19.Caption := 'rows';
   Label21.Caption := 'columns';
   SE3.Value := SE3.Value div FTileEngine.TileSize.cx;
   SE4.Value := SE4.Value div FTileEngine.TileSize.cy;
 end else
 begin
  Label19.Caption := 'width';
  Label21.Caption := 'height';
  SE3.Value := SE3.Value * FTileEngine.TileSize.cx;
  SE4.Value := SE4.Value * FTileEngine.TileSize.cy;
 end;
end;


procedure TForm_Principale.CB5Change(Sender: TObject);
begin
 CB2.Enabled := CB5.Checked;
 Label16.Enabled := CB5.Checked;
 CB3.Enabled := CB6.Checked;
 Label17.Enabled := CB6.Checked;
 SetProjectModified;
end;

procedure TForm_Principale.CB7Change(Sender: TObject);
begin
 Panel5.Enabled := CB7.Checked;
 FTopOverlayTileEngine.Visible := CB7.Checked and RB1.Checked;
 FBelowOverlayTileEngine.Visible := CB7.Checked and RB2.Checked;
end;

// show Ground type
procedure TForm_Principale.CheckBox1Change(Sender: TObject);
begin
 PB1.Invalidate;
end;

// user have changed hole color
procedure TForm_Principale.ColorButton1ColorChanged(Sender: TObject);
var c: TBGRAPixel;
begin
 c := ColorToBGRA( ColorButton1.ButtonColor );
 FTileEngine.MapHoleColor.Red.Value := c.red;
 FTileEngine.MapHoleColor.Green.Value := c.green;
 FTileEngine.MapHoleColor.Blue.Value := c.blue;
 SetProjectModified;
end;

procedure TForm_Principale.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
 FScene.ColorFadeIn(BGRABlack,0.5);
 FScene.ExecuteDuring(0.5);
end;


// add Tileset
procedure TForm_Principale.Button1Click(Sender: TObject);
begin
 if not OD1.Execute then exit;
 Form_AskTileSize.ShowModal;
 if TilesetManager.Count > 0
   then if ( TilesetManager.TileSet[0].TileWidth <> strtoint( Form_AskTileSize.E1.Text )) and
           ( TileSetManager.TileSet[0].TileHeight <> strtoint (Form_AskTileSize.E2.Text ))
          then showmessage('Warning : this tileset does not have the same tile size as the first...');
 LoadTextureToProject( OD1.FileName, strtoint( Form_AskTileSize.E1.Text ), strtoint( Form_AskTileSize.E2.Text ) );
 SetProjectModified;
end;

// load texture
procedure TForm_Principale.LoadTextureToProject( AFilename: string; AFrameWidth, AFrameHeight: integer );
begin
 FTileEngine.AddTexture( AFilename, AFrameWidth, AFrameHeight );

 TileSetManager.Add( AFilename, AFrameWidth, AFrameHeight );

 CB1.Items.Add( TileSetManager.TileSet[TileSetManager.Count-1].Name );
 CB1.ItemIndex := CB1.Items.Count - 1;
 PB1SetSizeAndPos;

 Label13.Caption := inttostr( AFrameWidth );
 Label14.Caption := inttostr( AFrameHeight );
end;

// menu Quit
procedure TForm_Principale.MenuItem7Click(Sender: TObject);
begin
 Close;
end;

// menu About
procedure TForm_Principale.MenuItem9Click(Sender: TObject);
begin
 Form_About.ShowModal;
end;

// menu Map - ClearAll
procedure TForm_Principale.Menu_ClearAllMapClick(Sender: TObject);
begin
 if MessageDlg ( '' , 'Reset the map ?', mtWarning , [mbYes,mbCancel] , 0 ) = mrYes
   then FTileEngine.ResetMap;
end;

// popup menu tile - Export ground type
procedure TForm_Principale.Menu_ExportGroundTypeClick(Sender: TObject);
begin
 Form_ExportGroundType.ShowModal;
end;

procedure TForm_Principale.Panel2Click(Sender: TObject);
begin
 FTileEngine.ResetMap;
end;

procedure TForm_Principale.PB1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var delta: integer;
begin
 if CB1.ItemIndex = -1 then exit;

 with TileSetManager.TileSet[CB1.ItemIndex] do begin
  delta := PB1.Width div XTileCount;
  FSelectedCell.x :=  X div delta;

  delta := PB1.Height div YTileCount;
  FSelectedCell.y :=  Y div delta;
 end;

 PB1.Invalidate;
end;


// PaintBox paint
procedure TForm_Principale.PB1Paint(Sender: TObject);
var bg, ima: TBGRABitmap;
    i, xxx, yyy, ixfr, iyfr: integer;
  deltaH, deltaV, xx, yy: single;
  s: string;
begin
 if CB1.ItemIndex = -1
   then begin
     PB1.Canvas.Brush.Color := Color;
     PB1.Canvas.FillRect(0, 0, PB1.Width, PB1.Height);
     exit;
   end;

 // rectangles around frame
 with TileSetManager.TileSet[CB1.ItemIndex] do begin
  ima := WholeImage.Resample( PB1.Width, PB1.Height ) as TBGRABitmap;
  ima.FontHeight := 9;

  deltaH := ima.Width / XTileCount;
  for i:=1 to trunc(deltaH)-1 do
    ima.DrawLineAntialias( i * deltaH, 0, i * deltaH, ima.Height, BGRAWhite, 1 );

  deltaV := ima.Height / YTileCount;
  for i:=1 to trunc(deltaV)-1 do
    ima.DrawLineAntialias( 0, i * deltaV, ima.Width, i * deltaV, BGRAWhite, 1 );

  // ground type
  if CheckBox1.Checked then begin
    xx := deltaH / 2;
    yy := deltaV / 2;
    for iyfr:=0 to YTileCount-1 do
      for ixfr:=0 to XTileCount-1 do
        begin
         s := GroundTypeToString( FTileEngine.GetGroundType( CB1.ItemIndex, ixfr, iyfr ));
         xxx := round(xx-ima.TextSize(s).cx div 2);
         yyy := round(yy - ima.TextSize(s).cy div 2);
         ima.Rectangle( xxx, yyy, xxx + ima.TextSize(s).cx, yyy + ima.TextSize(s).cy, BGRABlack, BGRABlack, dmSet );
         ima.TextOut( xxx, yyy, s, BGRA(128,128,128));

         xx += deltaH;
         if xx > ima.Width then begin
           xx := deltaH / 2;
           yy += deltaV;
         end;
       end;
  end;
 end;

 // selected frame
 xxx := FSelectedCell.x * round( deltaH );
 yyy := FSelectedCell.y * round( deltaV );
 ima.Rectangle( xxx, yyy, round(xxx+deltaH), round(yyy+deltaV), BGRA(255,255,0), BGRAPixelTransparent, dmDrawWithTransparency );

 // black and gray square background
 bg := TBGRABitmap.Create( ima.Width, ima.Height );
 bg.Fill( FImageBackGround );
 bg.PutImage( 0, 0, ima, dmDrawWithTransparency );

 bg.Draw( PB1.Canvas, 0, 0 );

 ima.Free;
 bg.Free;
end;

// popup set ground type
procedure TForm_Principale.MenuItem5Click(Sender: TObject);
begin
 if CB1.ItemIndex = -1 then exit;
 if Form_AskGroundType.ShowModal = mrCancel then exit;

 FTileEngine.SetGroundType( CB1.ItemIndex, FSelectedCell.x, FSelectedCell.y, Form_AskGroundType.LB.ItemIndex );
 PB1.Invalidate;
end;

// mouse down on mini map
procedure TForm_Principale.PB2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then PB2.Tag:=1;
end;

// mouse move on mini map
procedure TForm_Principale.PB2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var xx, yy: single;
begin
 if PB2.Tag = 0 then exit;
 if X > PB2.Width then X := PB2.Width;
 if X < 0 then X := 0;
 if Y > PB2.Height then Y := PB2.Height;
 if Y < 0 then Y := 0;

 xx := FTileEngine.MapSize.cx * X / PB2.Width;
 yy := FTileEngine.MapSize.cy * Y / PB2.Height;

 SBV.Position := round( yy*SBV.Max/FTileEngine.MapSize.cy);
 SBH.Position := round( xx/SBH.Max/FTileEngine.MapSize.cx);

 if FTileEngine.MapSize.cx <= FScene.Width
   then xx := -( FScene.Width - FTileEngine.MapSize.cx )/2;
 if FTileEngine.MapSize.cy <= FScene.Height
   then yy := -( FScene.Height - FTileEngine.MapSize.cy )/2;

 FTileEngine.MoveTo( -xx, -yy, 0.2, idcSinusoid );

 FMiniMapTarget := Point( X, Y );
 PB2.Invalidate;
end;

// mouse up on mini map
procedure TForm_Principale.PB2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then PB2.Tag:=0;
end;

// draw mini map
procedure TForm_Principale.PB2Paint(Sender: TObject);
const TARGET_RADIUS=5;
var ima: TBGRABitmap;
begin
 ima := TBGRABitmap.Create( PB2.Width, PB2.Height, BGRA(0,0,0));
 ima.Rectangle(0, 0, ima.Width, ima.Height, BGRA(180,180,180), dmSet );

 with FMiniMapTarget do
  ima.FillEllipseLinearColorAntialias( x, y, TARGET_RADIUS, TARGET_RADIUS, BGRA(200,180,50), BGRA(200,180,80));

 ima.Draw( PB2.Canvas, 0, 0 );
 ima.Free;
end;

procedure TForm_Principale.RB1Change(Sender: TObject);
begin
 FTopOverlayTileEngine.Visible := CB7.Checked and RB1.Checked;
 FBelowOverlayTileEngine.Visible := CB7.Checked and RB2.Checked;
end;

// Help button click page 'Setting'
procedure TForm_Principale.SBHelp1Click(Sender: TObject);
begin
 Showmessage('MAP SIZE -> reflect the size in tile of your working map. Click ''Set size'' button to change it.'+lineending+
             lineending+
             'HOLE COLOR -> A colored square will be drawn if there isn''t specified tile in a cell of your map.'+lineending+
             '        You can set the color of your choice here.'+lineending+
             '        Set the alpha channel < 255 for transparency.'+lineending+
             'Use TTileEngine.MapHoleColor property to change hole color at running time in your game application.'+lineending+
             lineending+
             'VIEW SIZE IN GAME -> Here you can set the size of the rectangle where the tile engine will display'+lineending+
             '         the tiles. This size can be in tiles or in pixels count.'+lineending+
             'Use TTileEngine.SetViewSize method to change it at running time in your game application.'+lineending+
             lineending+
             'START GAME ON TILE -> This is the row and column indexes of the top left tile'+lineending+
             '        where your game application will start. You can set this by RIGHT CLICK'+lineending+
             '        on render window.'+lineending+
             lineending+
             'SCROLLING -> You can enable or not the horizontal and vertical scrolling.'+lineending+
             'LOOP -> Checked means your map repeat itself infinitely on both axis.'+lineending+
             ' If not checked, map position will be bounded by map size.'+lineending+
             ' If checked, map position can be greater than map size, and tiles will be repeated.'+lineending+
             ' Use TTileEngine.HScrollEnable, TTileEngine.VScrollEnable, TTileEngine.HLoopMode'+lineending+
             ' and TTileEngine.VLoopMode property to change these options at running time in your game application.');
end;

// Help button click Tileset
procedure TForm_Principale.SBHelp2Click(Sender: TObject);
begin
 Showmessage('A map use texture or tileset that contains each tile.'+lineending+
             'ADD TILESET -> Load a tileset and add it to your project.'+lineending+
             '    The program will ask you the size of the tiles in pixels.'+lineending+
             lineending+
             'REMOVE TILESET -> remove the selected tileset from your project.'+lineending+
             '    All tiles that use this tileset will be removed and filled with hole color.'+lineending+
             lineending+
             'SHOW TYPE -> this option allow you to show the ground type on each tile (see below).'+lineending+
             'SHOW EVENT -> this option allow you to show the events on the render window.'+lineending+
             lineending+
             'You can select a tile by mouse left click on it'+lineending+
             lineending+
             'RIGHT CLICK -> Tileset contextual menu'+lineending+
             '  - GROUND TYPE -> Open a window where you can manage the tile''s ground type.'+lineending+
             '  - COPY GROUND TYPE TO CLIPBOARD -> Copy the ground types list in clipboard, so you can'+lineending+
             '    paste them under Lazarus, in your Pascal game application.' );

end;

// Help button click page 'Second map'
procedure TForm_Principale.SBHelp3Click(Sender: TObject);
begin
 Showmessage('TILE ENGINE DESIGNER offer the possibility to visualize a second map at the same time.'+lineending+
             lineending+
             'It''s usefull if you need to create animation with two (or more) superimposed tile engine in your game application.'+lineending+
             lineending+
             'Tiles of the back map will be seen through the ''holes'' of the top map.'+lineending+
             'Of course, the hole color of top map must be transparent.'+lineending+
             lineending+
             'Check ''Enable second tile engine'', load the second map,'+lineending+
             'and place it on the top or on the back of your working map.'+lineending+
             'You can only modify your working map and not the second.');
end;

procedure TForm_Principale.SBVScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
 if Sender = SBV
   then FTileEngine.Y.ChangeTo( -ScrollPos/SBV.Max*FTileEngine.MapSize.cy, 0.2, idcSinusoid );
 if Sender = SBH
   then FTileEngine.X.ChangeTo( -ScrollPos/SBH.Max*FTileEngine.MapSize.cx, 0.2, idcSinusoid );
end;

// Second TileEngine update parameters
procedure TForm_Principale.SE1Change(Sender: TObject);
begin
 with FTopOverlayTileEngine do
  begin
   Opacity.Value := SE1.Value;
   ScrollSpeed.Value := PointF( SE2.Value, SE5.Value );
   HScrollEnable := CB8.Checked;
   HLoopMode := CB10.Checked;
   VScrollEnable := CB9.Checked;
   VLoopMode := CB11.Checked;
  end;
 with FBelowOverlayTileEngine do
  begin
   Opacity.Value := SE1.Value;
   ScrollSpeed.Value := PointF( SE2.Value, SE5.Value );
   HScrollEnable := CB8.Checked;
   HLoopMode := CB10.Checked;
   VScrollEnable := CB9.Checked;
   VLoopMode := CB11.Checked;
  end;
end;

// change map view size in game
procedure TForm_Principale.SE3Change(Sender: TObject);
begin
 SetProjectModified;
end;

// user change opacity on map hole color
procedure TForm_Principale.SE6Change(Sender: TObject);
begin
 FTileEngine.MapHoleColor.Alpha.Value := SE6.Value;
 SetProjectModified;
end;


procedure TForm_Principale.GetTextureAndFrameindex(out t, ixfr, iyfr: integer);
begin
 t := CB1.ItemIndex;
 if t = -1
   then begin
     ixfr := -1;
     iyfr := -1;
   end
   else begin
    ixfr := FSelectedCell.x;
    iyfr := FSelectedCell.y;
   end;
end;

procedure TForm_Principale.SetRelativeSelectedTile(aDeltaX, aDeltay: integer);
begin
 FSelectedCell.x := FSelectedCell.x + aDeltaX;
 FSelectedCell.y := FSelectedCell.y + aDeltay;
 if FSelectedCell.x < 0 then FSelectedCell.x := 0;
 if FSelectedCell.x >= TileSetManager.TileSet[CB1.ItemIndex].XTileCount
   then FSelectedCell.x := TileSetManager.TileSet[CB1.ItemIndex].XTileCount-1;

 if FSelectedCell.y < 0 then FSelectedCell.y :=0;
 if FSelectedCell.y >= TileSetManager.TileSet[CB1.ItemIndex].YTileCount
   then FSelectedCell.y := TileSetManager.TileSet[CB1.ItemIndex].YTileCount-1;
 PB1.Invalidate;
end;


end.

