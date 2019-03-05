unit u_tool_window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  LCLType, LCLIntf,
  StdCtrls, ExtCtrls, Menus, Spin, ComCtrls, CheckLst, Arrow,
  BGRABitmap, BGRABitmapTypes,
  common,
  VelocityCurve,
  tileset_manager,
  uAskTileSize,
  uabout,
  uaskgroundtype,
  uAskMapSize,
  uexportgroundtype,
  umaps;

const
  MAX_PAINTBOX_SIZE = 320;

type

  { TForm_Tools }

  TForm_Tools = class(TForm)
    Arrow1: TArrow;
    Arrow2: TArrow;
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CB1: TComboBox;
    CB2: TCheckBox;
    CB3: TCheckBox;
    CB4: TComboBox;
    CB5: TCheckBox;
    CB6: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CLBLayer: TCheckListBox;
    ColorButton1: TColorButton;
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
    Label3: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LBJeux: TListBox;
    MainMenu1: TMainMenu;
    MenuItem10: TMenuItem;
    Menu_ExportGroundType: TMenuItem;
    Menu_ClearAllMap: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem9: TMenuItem;
    OD1: TOpenDialog;
    OD2: TOpenDialog;
    OD3: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel2: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    PB2: TPaintBox;
    PB1: TPaintBox;
    PopupMenu1: TPopupMenu;
    SBHelp2: TSpeedButton;
    SBV: TScrollBar;
    SBH: TScrollBar;
    SD1: TSaveDialog;
    pageMiniMap: TTabSheet;
    SD2: TSaveDialog;
    SE3: TSpinEdit;
    SE4: TSpinEdit;
    SE6: TSpinEdit;
    SE7: TSpinEdit;
    SE8: TSpinEdit;
    Shape1: TShape;
    SBHelp1: TSpeedButton;
    TabSheet1: TTabSheet;
    pageLayer: TTabSheet;
    procedure Arrow1Click(Sender: TObject);
    procedure Arrow2Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CB1Change(Sender: TObject);
    procedure CB4Change(Sender: TObject);
    procedure CB5Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CLBLayerClickCheck(Sender: TObject);
    procedure CLBLayerSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure Menu_ClearAllMapClick(Sender: TObject);
    procedure Menu_ExportGroundTypeClick(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure PB1MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PB1Paint(Sender: TObject);
    procedure PB2MouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PB2MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PB2MouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PB2Paint(Sender: TObject);
    procedure SBHelp1Click(Sender: TObject);
    procedure SBHelp2Click(Sender: TObject);
    procedure SBVScroll(Sender: TObject; {%H-}ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure SE3Change(Sender: TObject);
    procedure SE6Change(Sender: TObject);
    procedure SE7Change(Sender: TObject);
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

    procedure UpdateMapParameterOnScreen;

  end;

var
  Form_Tools: TForm_Tools;

implementation
{$R *.lfm}
uses u_tileset_edit;

{ TForm_Tools }


procedure TForm_Tools.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 EvalKey( Key );
end;

procedure TForm_Tools.UpdateMapParameterOnScreen;
var s: string;
begin
 // map tile count
 s := 'Column: ' + inttostr(MapList.MainMap.TileEngine.MapTileCount.cx) + ' tile';
 if MapList.MainMap.TileEngine.MapTileCount.cx > 1 then s+='s';
 Label5.Caption := s;
 s := 'Row: ' + inttostr(MapList.MainMap.TileEngine.MapTileCount.cy) + ' tile';
 if MapList.MainMap.TileEngine.MapTileCount.cx > 1 then s+='s';
 Label15.Caption := s;
 // Tile Size
 Label13.Caption := inttostr( MapList.MainMap.TileEngine.TileSize.cx );
 Label14.Caption := inttostr( MapList.MainMap.TileEngine.TileSize.cy );

 // Hole color
 ColorButton1.ButtonColor := BGRAToColor( MapList.MainMap.TileEngine.MapHoleColor.Value );
 SE6.Value := MapList.MainMap.TileEngine.MapHoleColor.Alpha.Value;

 // draw size
 if CB4.ItemIndex = 0 then
 begin
  SE3.Value := MapList.MainMap.TileEngine.Width;
  SE4.Value := MapList.MainMap.TileEngine.Height;
 end else
 begin
  SE3.Value := MapList.MainMap.TileEngine.MapTileCount.cx;
  SE4.Value := MapList.MainMap.TileEngine.MapTileCount.cy;
 end;

 // Scroll Loop Mode
 CB2.Checked := MapList.MainMap.TileEngine.HLoopMode;
 CB3.Checked := MapList.MainMap.TileEngine.VLoopMode;

 // Scrolling enabled
 CB5.Checked:=MapList.MainMap.TileEngine.HScrollEnable;
 CB6.Checked:=MapList.MainMap.TileEngine.VScrollEnable;

 // Start tile
 Label10.Caption := inttostr( trunc( MapList.MainMap.TileEngine.PositionOnMap.x.Value / MapList.MainMap.TileEngine.TileSize.cx ));
 Label11.Caption := inttostr( trunc( MapList.MainMap.TileEngine.PositionOnMap.y.Value / MapList.MainMap.TileEngine.TileSize.cy ));
end;

// set Paintbox size and pos
procedure TForm_Tools.PB1SetSizeAndPos;
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

procedure TForm_Tools.PB2SetSizeAndPos;
var factor: single;
  ll, tt, ww, hh: integer;
begin
 if MapList.Count=0 then exit;
 if MapList.MainMap.TileEngine.TileSize.cx = 0 then exit;
 if MapList.MainMap.TileEngine.MapSize.cy = 0 then exit;
 if MapList.MainMap.TileEngine.MapSize.cx = 0 then exit;

 factor := MapList.MainMap.TileEngine.MapSize.cx / MapList.MainMap.TileEngine.MapSize.cy;
 if MapList.MainMap.TileEngine.MapSize.cx > MapList.MainMap.TileEngine.MapSize.cy
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

 SBV.Max := MapList.MainMap.TileEngine.MapTileCount.cy;
 SBV.PageSize := round( FScene.Height / MapList.MainMap.TileEngine.TileSize.cy );
 SBV.LargeChange := SBV.PageSize;

 SBH.Max := MapList.MainMap.TileEngine.MapTileCount.cx;
 SBH.PageSize := round( FScene.Width / MapList.MainMap.TileEngine.TileSize.cx );
 SBH.LargeChange := SBH.PageSize;

end;




// delete Tileset
procedure TForm_Tools.Button2Click(Sender: TObject);
var i: integer;
begin
 if CB1.ItemIndex = -1 then exit;
 if MessageDlg ( '' , 'Remove this tileset from project ?'+lineending+CB1.Text , mtWarning , [mbYes,mbCancel] , 0 ) = mrCancel
   then exit;
 TileSetManager.Remove( CB1.ItemIndex );
 MapList.MainMap.TileEngine.DeleteTexture( CB1.ItemIndex );

 i := CB1.ItemIndex-1;
 CB1.Items.Delete( CB1.ItemIndex );
 if i > -1 then CB1.ItemIndex := i;

 PB1.Invalidate;
 SetProjectModified;
end;

// set map size
procedure TForm_Tools.Button4Click(Sender: TObject);
var s: string;
begin
 Form_AskMapSize.ShowModal;

 // map tile count
 s := 'Column: ' + inttostr(MapList.MainMap.TileEngine.MapTileCount.cx) + ' tile';
 if MapList.MainMap.TileEngine.MapTileCount.cx > 1 then s+='s';
 Label5.Caption := s;
 s := 'Row: ' + inttostr(MapList.MainMap.TileEngine.MapTileCount.cy) + ' tile';
 if MapList.MainMap.TileEngine.MapTileCount.cx > 1 then s+='s';
 Label15.Caption := s;

 PB2SetSizeAndPos;
end;

// stop scroll on main map
procedure TForm_Tools.Button5Click(Sender: TObject);
begin
 SE7.Value:=0;
 SE8.Value:=0;
 if CLBLayer.ItemIndex=-1 then exit;
 MapList.SelectedTileEngine.ScrollSpeed.Value:=PointF(0,0);
end;

// go to top left on main map
procedure TForm_Tools.Button6Click(Sender: TObject);
begin
 if CLBLayer.ItemIndex=-1 then exit;
 MapList.SelectedTileEngine.PositionOnMap.Value:=PointF(0,0);
end;

// add map layer
procedure TForm_Tools.Button7Click(Sender: TObject);
begin
 MapList.NewMap;
end;

// delete map layer
procedure TForm_Tools.Button8Click(Sender: TObject);
begin
 MapList.DeleteMap;
end;

// load project
procedure TForm_Tools.Button9Click(Sender: TObject);
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

// tileset choice
procedure TForm_Tools.CB1Change(Sender: TObject);
begin
 PB1SetSizeAndPos;
 PB1.Invalidate;
end;

procedure TForm_Tools.CB4Change(Sender: TObject);
begin
 if CB4.ItemIndex = 1 then
 begin
   Label19.Caption := 'columns';
   Label21.Caption := 'rows';
   SE3.Value := SE3.Value div MapList.MainMap.TileEngine.TileSize.cx;
   SE4.Value := SE4.Value div MapList.MainMap.TileEngine.TileSize.cy;
 end else
 begin
  Label19.Caption := 'width';
  Label21.Caption := 'height';
  SE3.Value := SE3.Value * MapList.MainMap.TileEngine.TileSize.cx;
  SE4.Value := SE4.Value * MapList.MainMap.TileEngine.TileSize.cy;
 end;
end;


procedure TForm_Tools.CB5Change(Sender: TObject);
begin
 CB2.Enabled := CB5.Checked;
 Label16.Enabled := CB5.Checked;
 CB3.Enabled := CB6.Checked;
 Label17.Enabled := CB6.Checked;

 FWorkingTileEngine.HScrollEnable:=CB5.Checked;
 FWorkingTileEngine.HLoopMode:=CB2.Checked;
 FWorkingTileEngine.VScrollEnable:=CB6.Checked;
 FWorkingTileEngine.VLoopMode:=CB3.Checked;

 SetProjectModified;
end;

// show Ground type
procedure TForm_Tools.CheckBox1Change(Sender: TObject);
begin
 PB1.Invalidate;
end;

procedure TForm_Tools.CLBLayerClickCheck(Sender: TObject);
var i: integer;
begin
 for i:=0 to CLBLayer.Count-1 do
   MapList.TileEngine[i].Visible:=CLBLayer.Checked[i];
end;

// user have selected a map layer
procedure TForm_Tools.CLBLayerSelectionChange(Sender: TObject;
  User: boolean);
begin
 FWorkingTileEngine := MapList.TileEngine[CLBLayer.ItemIndex];

 CB5.Checked:=FWorkingTileEngine.HScrollEnable;
 CB2.Checked:=FWorkingTileEngine.HLoopMode;
 CB6.Checked:=FWorkingTileEngine.VScrollEnable;
 CB3.Checked:=FWorkingTileEngine.VLoopMode;
end;

// user have changed hole color
procedure TForm_Tools.ColorButton1ColorChanged(Sender: TObject);
var c: TBGRAPixel;
begin
 c := ColorToBGRA( ColorButton1.ButtonColor );
 MapList.SetTileEngineRGBHoleColor( c );
end;

procedure TForm_Tools.FormCreate(Sender: TObject);
begin
 MapList:= TMapList.Create;
 TileSetEdit:= TTilesetEdit.Create;
end;


// add Tileset
procedure TForm_Tools.Button1Click(Sender: TObject);
begin
 if MapList.Count=0 then exit;
 if not OD1.Execute then exit;
 Form_AskTileSize.ShowModal;
 if TilesetManager.Count > 0
   then if ( TilesetManager.TileSet[0].TileWidth <> strtoint( Form_AskTileSize.E1.Text )) and
           ( TileSetManager.TileSet[0].TileHeight <> strtoint (Form_AskTileSize.E2.Text ))
          then showmessage('Warning : this tileset does not have the same tile size as the first...');
 LoadTextureToProject( OD1.FileName, strtoint( Form_AskTileSize.E1.Text ), strtoint( Form_AskTileSize.E2.Text ) );
 SetProjectModified;
end;

// layer map shift up in the list
procedure TForm_Tools.Arrow1Click(Sender: TObject);
begin
 MapList.ShiftMapUp;
end;
// layer map shift down in the list
procedure TForm_Tools.Arrow2Click(Sender: TObject);
begin
 MapList.ShiftMapDown;
end;

// save project
procedure TForm_Tools.Button10Click(Sender: TObject);
begin
 MapList.SaveSession;
end;

// rename a map layer
procedure TForm_Tools.Button11Click(Sender: TObject);
begin
 MapList.RenameMap;
end;

// load texture
procedure TForm_Tools.LoadTextureToProject( AFilename: string; AFrameWidth, AFrameHeight: integer );
begin
 MapList.MainMap.TileEngine.AddTexture( AFilename, AFrameWidth, AFrameHeight );

 TileSetManager.Add( AFilename, AFrameWidth, AFrameHeight );

 CB1.Items.Add( TileSetManager.TileSet[TileSetManager.Count-1].Name );
 CB1.ItemIndex := CB1.Items.Count - 1;
 PB1SetSizeAndPos;

 Label13.Caption := inttostr( AFrameWidth );
 Label14.Caption := inttostr( AFrameHeight );
end;

// menu About
procedure TForm_Tools.MenuItem9Click(Sender: TObject);
begin
 Form_About.ShowModal;
end;

// menu Map - ClearAll
procedure TForm_Tools.Menu_ClearAllMapClick(Sender: TObject);
begin
 if MessageDlg ( '' , 'Reset the map ?', mtWarning , [mbYes,mbCancel] , 0 ) = mrYes
   then MapList.ResetMaps;
end;

// popup menu tile - Export ground type
procedure TForm_Tools.Menu_ExportGroundTypeClick(Sender: TObject);
begin
 Form_ExportGroundType.ShowModal;
end;

procedure TForm_Tools.Panel2Click(Sender: TObject);
begin
// MapList.MainMap.TileEngine.ResetMap;
end;

procedure TForm_Tools.PB1MouseDown(Sender: TObject; Button: TMouseButton;
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
procedure TForm_Tools.PB1Paint(Sender: TObject);
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
         s := GroundTypeToString( MapList.MainMap.TileEngine.GetGroundType( CB1.ItemIndex, ixfr, iyfr ));
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
procedure TForm_Tools.MenuItem5Click(Sender: TObject);
begin
 if CB1.ItemIndex = -1 then exit;
 if Form_AskGroundType.ShowModal = mrCancel then exit;

 MapList.MainMap.TileEngine.SetGroundType( CB1.ItemIndex, FSelectedCell.x, FSelectedCell.y, Form_AskGroundType.LB.ItemIndex );
 PB1.Invalidate;
end;

// mouse down on mini map
procedure TForm_Tools.PB2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then PB2.Tag:=1;
end;

// mouse move on mini map
procedure TForm_Tools.PB2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var xx, yy: single;
begin
 if PB2.Tag = 0 then exit;
 if X > PB2.Width then X := PB2.Width;
 if X < 0 then X := 0;
 if Y > PB2.Height then Y := PB2.Height;
 if Y < 0 then Y := 0;

 xx := MapList.MainMap.TileEngine.MapSize.cx * X / PB2.Width;
 yy := MapList.MainMap.TileEngine.MapSize.cy * Y / PB2.Height;

 SBV.Position := round( yy*SBV.Max/MapList.MainMap.TileEngine.MapSize.cy);
 SBH.Position := round( xx/SBH.Max/MapList.MainMap.TileEngine.MapSize.cx);

 if MapList.MainMap.TileEngine.MapSize.cx <= FScene.Width
   then xx := -( FScene.Width - MapList.MainMap.TileEngine.MapSize.cx )/2;
 if MapList.MainMap.TileEngine.MapSize.cy <= FScene.Height
   then yy := -( FScene.Height - MapList.MainMap.TileEngine.MapSize.cy )/2;

 MapList.MoveTileEngineTo(-xx, -yy);

 FMiniMapTarget := Point( X, Y );
 PB2.Invalidate;
end;

// mouse up on mini map
procedure TForm_Tools.PB2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then PB2.Tag:=0;
end;

// draw mini map
procedure TForm_Tools.PB2Paint(Sender: TObject);
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

// Help button click page 'Setting'
procedure TForm_Tools.SBHelp1Click(Sender: TObject);
begin
 Showmessage('Tile Map Designer offer the possibility to create maps with multiple layer.'+lineending+
             'A session file groups all the informations to organize the layers in your map.'+lineending+
             'Each layer data is saved in a separate file with the name of its layer plus ''.map'' extension.'+lineending+
             'It is at your charge to put all the tilesets used in the same folder.'+lineending+lineending+

             'MAP SIZE -> reflect the size in tile of your project. Click ''Set size'' button to change it.'+lineending+
             '            All layers will be affected.'+lineending+lineending+
             'HOLE COLOR -> A colored square will be drawn if there isn''t specified tile in a cell of your map.'+lineending+
             '        You can set the color of your choice here.'+lineending+
             '        Set the opacity less than 255 for transparency.'+lineending+
             '        All layers will be affected.'+lineending+lineending+
             'VIEW SIZE IN GAME -> Here you can set the size of the rectangular surface where the map will be display'+lineending+
             '                     This size can be in tiles or in pixels count.'+lineending+
             'Use TTileEngine.SetViewSize method to change it at running time in your game application.'+lineending+
             lineending+
             'START GAME ON TILE -> This is the row and column indexes of the top left tile'+lineending+
             '        where your game will start on. You can set this by RIGHT CLICK'+lineending+
             '        on render window.'+lineending+
             lineending+
             'SCROLLING -> You can enable or not the horizontal and vertical scrolling individualy for each map''s layer.'+lineending+
             'LOOP -> Checked means your map repeat itself infinitely on both axis.'+lineending+
             ' If not checked, map position will be bounded by map size.'+lineending+
             ' If checked, map position can be greater than map size, and tiles will be repeated from the beginning.'+lineending+
             ' Use TTileEngine.HScrollEnable, TTileEngine.VScrollEnable, TTileEngine.HLoopMode'+lineending+
             ' and TTileEngine.VLoopMode property to change these options at running time in your game application.');
end;

// Help button click Tileset
procedure TForm_Tools.SBHelp2Click(Sender: TObject);
begin
 Showmessage('A map use texture or tileset that contains each tile.'+lineending+
             'ADD TILESET -> Load a tileset and add it to your project.'+lineending+
             '    The program will ask you the size of the tiles in pixels.'+lineending+
             lineending+
             'REMOVE TILESET -> remove the selected tileset from your project.'+lineending+
             '    All tiles that use this tileset will be removed and filled with hole color.'+lineending+
             lineending+
             'SHOW TYPE -> this option allow you to show the ground type on each tile (see below).'+lineending+
             lineending+
             'You can select a tile by mouse left click on it'+lineending+
             lineending+
             'RIGHT CLICK -> Tileset contextual menu'+lineending+
             '  - GROUND TYPE -> Open a window where you can manage the tile''s ground type.'+lineending+
             '  - COPY GROUND TYPE TO CLIPBOARD -> Copy the ground types list in clipboard, so you can'+lineending+
             '    paste them under Lazarus, in your Pascal game application.' );

end;

procedure TForm_Tools.SBVScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
 if Sender = SBV
   then MapList.MainMap.TileEngine.Y.ChangeTo( -ScrollPos/SBV.Max*MapList.MainMap.TileEngine.MapSize.cy, 0.2, idcSinusoid );
 if Sender = SBH
   then MapList.MainMap.TileEngine.X.ChangeTo( -ScrollPos/SBH.Max*MapList.MainMap.TileEngine.MapSize.cx, 0.2, idcSinusoid );
end;

// change map view size in game
procedure TForm_Tools.SE3Change(Sender: TObject);
begin

 SetProjectModified;
end;

// user change opacity on map hole color
procedure TForm_Tools.SE6Change(Sender: TObject);
begin
 MapList.SetTileEngineOpacityHoleColor( SE6.Value );
end;

// scrolling speed (main map)
procedure TForm_Tools.SE7Change(Sender: TObject);
var i: integer;
begin
 i:= CLBLayer.ItemIndex;
 if i=-1 then exit;
 MapList.TileEngine[i].ScrollSpeed.Value := PointF( SE7.Value, SE8.Value );
end;


procedure TForm_Tools.GetTextureAndFrameindex(out t, ixfr, iyfr: integer);
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

procedure TForm_Tools.SetRelativeSelectedTile(aDeltaX, aDeltay: integer);
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

