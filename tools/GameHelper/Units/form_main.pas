unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, Menus, OpenGLContext,
  OGLCScene, u_common, Types,
  frame_tool_spritebuilder,
  frame_tool_spritebank,
  frame_tool_leveleditor,
  frame_tool_levelbank,
  frame_viewfontbank, u_ui_objectlist,
  frame_tool_panelbank,
  frame_tool_uipaneleditor, frame_pathbank, frame_patheditor;

{
 RIGHT MOUSE button: Move view
 LEFT MOUSE button: select item, move item
 Mouse wheel : shift view vertically
 Mouse wheel + shift : shift view horizontally
 Mouse wheel + control: zoom in/out
}

type
{  TMouseState = ( msIdle,
                  msMoveView
                );  }
  { TFormMain }

  TFormMain = class(TForm)
    BLevelBank: TSpeedButton;
    BFontBank: TSpeedButton;
    BLevelEditor: TSpeedButton;
    BScreenBank: TSpeedButton;
    BToolUIPanel: TSpeedButton;
    CBBank: TComboBox;
    Label1: TLabel;
    Notebook1: TNotebook;
    OGL: TOpenGLControl;
    PagePathEditor: TPage;
    PagePathBank: TPage;
    PagePanelBank: TPage;
    PageUIPanelEditor: TPage;
    PageFontBank: TPage;
    PageLevelEditor: TPage;
    PageLevelBank: TPage;
    PageSpriteBuilder: TPage;
    PageSpriteBank: TPage;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    BSpriteBank: TSpeedButton;
    Panel8: TPanel;
    ToolBarMain: TToolBar;
    BNewProject: TToolButton;
    BLoadProject: TToolButton;
    BSaveProject: TToolButton;
    ToolButton1: TToolButton;
    BProjectConfig: TToolButton;
    procedure BLoadProjectClick(Sender: TObject);
    procedure BNewProjectClick(Sender: TObject);
    procedure BProjectConfigClick(Sender: TObject);
    procedure BSaveProjectClick(Sender: TObject);
    procedure BSpriteBankClick(Sender: TObject);
    procedure CBBankSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure OGLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    procedure CreateAppTextureAtlas;
    procedure FreeAppTextureAtlas;
  private
    procedure LoadCommonData;
    procedure FreeCommonData;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  public
    procedure UpdateWidgets;

    procedure HideAllPages;

    procedure ShowPageSpriteBank;
    procedure ShowPageSpriteBuilder;
    procedure ShowPageLevelEditor;
    procedure ShowPageLevelBank;
    procedure ShowPageFontBank;
    procedure ShowPagePanelBank;
    procedure ShowPagePanelEditor;
    procedure ShowPagePathBank;
    procedure ShowPagePathEditor;

    procedure EditLevelInLevelBank(const aName: string);
  end;

var
  FormMain: TFormMain;

  FrameToolsSpriteBuilder: TFrameToolsSpriteBuilder;
  FrameToolsSpriteBank: TFrameToolSpriteBank;
  FrameToolLevelEditor: TFrameToolLevelEditor;
  FrameToolLevelBank: TFrameToolLevelBank;
  FrameViewFontBank: TFrameViewFontBank;
  FrameToolPanelBank: TFrameToolPanelBank;
  FrameToolUIPanelEditor: TFrameToolUIPanelEditor;
  FramePathBank: TFramePathBank;
  FramePathEditor: TFramePathEditor;


implementation
uses u_screen_spritebuilder, u_project, u_app_pref, u_screen_template,
  u_spritebank, u_ui_handle, u_screen_spritebank, u_screen_levelbank,
  u_screen_leveleditor, u_levelbank, form_projectconfig, BGRABitmap,
  BGRABitmapTypes, u_connection_to_ide, u_screen_fontbank,
  u_screen_uipaneleditor, u_screen_uipanelbank, u_screen_pathbank,
  u_screen_patheditor, LCLType;
{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FScene := TOGLCScene.Create(OGL, -1);
  FScene.DesignPPI := 96;
  FScene.LayerCount := APP_LAYER_COUNT;
  FScene.BackgroundColor := BGRA(40,40,40);
  FScene.ScreenFadeTime := 0;
  FScene.CreateLogFile(Application.Location+'scene.log', True);

  FScene.OnLoadCommonData := @LoadCommonData;
  FScene.OnFreeCommonData := @FreeCommonData;

  FScene.FontManager.ScanProjectFont(GetFontFolder);

  // check if the app was started from the IDE
  IdeConnect.CheckCommandLine;

  Project := TProject.Create;

  Application.OnIdle := @ProcessApplicationIdle;

  FrameToolsSpriteBuilder := TFrameToolsSpriteBuilder.Create(Self);
  FrameToolsSpriteBuilder.Parent := Panel8;
  FrameToolsSpriteBuilder.Align := alClient;

  FrameToolsSpriteBank := TFrameToolSpriteBank.Create(Self);
  FrameToolsSpriteBank.Parent := PageSpriteBank;
  FrameToolsSpriteBank.Align := alClient;

  FrameToolLevelEditor := TFrameToolLevelEditor.Create(Self);
  FrameToolLevelEditor.Parent := PageLevelEditor;
  FrameToolLevelEditor.Align := alClient;

  FrameToolLevelBank := TFrameToolLevelBank.Create(Self);
  FrameToolLevelBank.Parent := PageLevelBank;
  FrameToolLevelBank.Align := alClient;

  FrameViewFontBank := TFrameViewFontBank.Create(Self);
  FrameViewFontBank.Parent := PageFontBank;
  FrameViewFontBank.Align := alClient;
  FrameViewFontBank.IsEditable := True;
  FrameViewFontBank.CheckListBox1.Clear;
  FrameViewFontBank.CheckListBox1.Items.AddStrings(FScene.Charsets.GetListOfCharsetNames);


  FrameToolPanelBank := TFrameToolPanelBank.Create(Self);
  FrameToolPanelBank.Parent := PagePanelBank;
  FrameToolPanelBank.Align := alClient;
  FrameToolPanelBank.IsEditable := True;

  FrameToolUIPanelEditor := TFrameToolUIPanelEditor.Create(Self);
  FrameToolUIPanelEditor.Parent := PageUIPanelEditor;
  FrameToolUIPanelEditor.Align := alClient;

  FramePathBank := TFramePathBank.Create(Self);
  FramePathBank.Parent := PagePathBank;
  FramePathBank.Align := alClient;
  FramePathBank.IsEditable := True;

  FramePathEditor := TFramePathEditor.Create(Self);
  FramePathEditor.Parent := PagePathEditor;
  FramePathEditor.Align := alClient;

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FScene.Free;
  FScene := NIL;
  FreeAndNil(Project);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (Notebook1.PageIndex = Notebook1.IndexOf(PageSpriteBuilder)) and
     FrameToolsSpriteBuilder.Modified then begin
    CanClose := False;
    exit;
  end;

  if (Notebook1.PageIndex = Notebook1.IndexOf(PageLevelEditor)) and
     FrameToolLevelEditor.Modified then begin
    CanClose := False;
    exit;
  end;

  CanClose := Project.DoUserPromptToSaveProject;
end;

procedure TFormMain.BNewProjectClick(Sender: TObject);
begin
  Project.New;
end;

procedure TFormMain.BProjectConfigClick(Sender: TObject);
begin
  FormProjectConfig := TFormProjectConfig.Create(NIL);
  try
    FormProjectConfig.ShowModal;
  finally
    FormProjectConfig.Free;
  end;
end;

procedure TFormMain.BSaveProjectClick(Sender: TObject);
begin
  Project.Save;
end;

procedure TFormMain.BSpriteBankClick(Sender: TObject);
begin
  if Sender = BSpriteBank then begin
    FScene.RunScreen(ScreenSpriteBank);
    Notebook1.PageIndex := Notebook1.IndexOf(PageSpriteBank);
    FrameToolsSpriteBank.OnShow;
    UpdateWidgets;
    ToolBarMain.Visible := True;
  end;

  if Sender = BLevelBank then begin
    FScene.RunScreen(ScreenLevelBank);
    Notebook1.PageIndex := Notebook1.IndexOf(PageLevelBank);
    FrameToolLevelBank.OnShow;
    UpdateWidgets;
    ToolBarMain.Visible := True;
  end;

  if Sender = BFontBank then begin
    FScene.RunScreen(ScreenFontBank);
    Notebook1.PageIndex := Notebook1.IndexOf(PageFontBank);
    FrameViewFontBank.Fill;
    UpdateWidgets;
    ToolBarMain.Visible := False;
  end;

  if Sender = BToolUIPanel then begin
    FScene.RunScreen(ScreenUIPanelEditor);
    Notebook1.PageIndex := Notebook1.IndexOf(PageUIPanelEditor);
    FrameToolUIPanelEditor.OnShow;
    UpdateWidgets;
    ToolBarMain.Visible := False;
  end;
end;

procedure TFormMain.CBBankSelect(Sender: TObject);
begin
  case CBBank.Text of
    'SPRITE BANK': ShowPageSpriteBank;
    'LEVEL BANK': ShowPageLevelBank;
    'FONT BANK': ShowPageFontBank;
    'PANEL BANK': ShowPagePanelBank;
    'PATH BANK': ShowPagePathBank;
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := CloseAction;
  // if needed, save the respons file for the IDE and free the respons buffer
  //ResponsToIde.Send;
  IdeConnect.InformIDEThatGameHelperTerminate;
end;

procedure TFormMain.BLoadProjectClick(Sender: TObject);
begin
  Project.Load;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessOnKeyDown(Key, Shift);
  FScene.ProcessOnKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  case Key of
    VK_S: if (ssCtrl in Shift) and Project.IsReady and Project.HasBeenModified then begin
      Project.Save;
      UpdateWidgets;
    end;
  end;

  TCustomScreenTemplate(FScene.CurrentScreen).ProcessOnKeyUp(Key, Shift);
  FScene.ProcessOnKeyUp(Key, Shift);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FScene.OpenGLLibLoaded then begin
    ShowMessage('ERROR: OpenGL library could not be loaded...'+LineEnding+
                'Check if your system is compatible with OpenGL 3.3 core'+LineEnding+
                'and if the library is well installed on your computer');
    Close;
  end;
end;

procedure TFormMain.OGLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessMouseDown(Button, Shift, X, Y);
end;

procedure TFormMain.OGLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var p: TPoint;
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;

  p := TCustomScreenTemplate(FScene.CurrentScreen).TransformCoor(PointF(X, Y)).Truncate;
  Label1.Caption := 'X:'+p.x.ToString+'  Y:'+p.y.ToString;

  TCustomScreenTemplate(FScene.CurrentScreen).ProcessMouseMove(Shift, X, Y);
end;

procedure TFormMain.OGLMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessMouseUp(Button, Shift, X, Y);
end;

procedure TFormMain.OGLMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);
end;

procedure TFormMain.CreateAppTextureAtlas;
var fd: TFontDescriptor;
  path: String;
begin
  // create an atlas for the app with fonts and mouse cursors
  FAtlas := FScene.CreateAtlas;
  FAtlas.Spacing := 1;

  fd.Create('Roboto', 14, [], BGRA(255,255,100), BGRA(0,0,0,200), 3, BGRA(0,0,0,180), 3, 3, 5);
  FHintFont := FAtlas.AddTexturedFont(fd, FScene.Charsets.ASCII_SYMBOL+FScene.Charsets.SIMPLELATIN);

  fd.Create('Roboto', 16, [], BGRA(255,155,155), BGRA(0,0,0,200), 3);
  FErrorFont := FAtlas.AddTexturedFont(fd, FScene.Charsets.ASCII_SYMBOL+FScene.Charsets.SIMPLELATIN);

  path := GetHandleFolder;
  texHandlePivot := FAtlas.AddFromSVG(path+'Pivot.svg', PPIScale(12), -1);
  texHandleRotate := FAtlas.AddFromSVG(path+'Rotate.svg', PPIScale(19), -1);
  texArrowH := FAtlas.AddFromSVG(path+'ArrowH.svg', PPIScale(19), -1);

  texHandlePathNode := FAtlas.AddMultiFrameImageFromSVG([path+'PathNode.svg',
                                                      path+'PathNodeSelected.svg'],
                                                      PPIScale(12), -1, 2, 1, 2);
  texHandlePathNodeCircle := FAtlas.AddFromSVG(path+'PathNodeCircle.svg', FScene.ScaleDesignToScene(6)*2, -1);

  // mouse cursor
  path := GetCursorFolder;
  texMouseNormal := FAtlas.AddFromSVG(path+'Select.svg', PPIScale(32), -1);
  texSelectSurfaceByRect := FAtlas.AddFromSVG(path+'SelectSurfaceByRect.svg', PPIScale(32), -1);
  texMouseOverSurface := FAtlas.AddFromSVG(path+'OverSurface.svg', PPIScale(32), -1);
  texMouseOverPivot := FAtlas.AddFromSVG(path+'OverPivot.svg', PPIScale(32), -1);
  texMouseRotateSurface := FAtlas.AddFromSVG(path+'RotateSurface.svg', PPIScale(32), -1);
  texMouseScaleSurface := FAtlas.AddFromSVG(path+'ScaleSurface.svg', PPIScale(32), -1);

  texMouseOverNode := FAtlas.AddFromSVG(path+'OverNode.svg', PPIScale(32), -1);
  texMouseMovingNode := FAtlas.AddFromSVG(path+'MovingNode.svg', PPIScale(32), -1);
  texMouseAddNode := FAtlas.AddFromSVG(path+'AddNode.svg', PPIScale(32), -1);
  texmouseAddNodeCircle := FAtlas.AddFromSVG(path+'AddNodeCircle.svg', PPIScale(32), -1);
  texMouseInsertNodeCircle := FAtlas.AddFromSVG(path+'InsertNodeCircle.svg', PPIScale(32), -1);
  texMouseToolPoint := FAtlas.AddFromSVG(path+'Point.svg', PPIScale(32), -1);
  texMouseToolLine := FAtlas.AddFromSVG(path+'Line.svg', PPIScale(32), -1);
  texMouseToolCircle := FAtlas.AddFromSVG(path+'Circle.svg', PPIScale(32), -1);
  texMouseToolRectangle := FAtlas.AddFromSVG(path+'Rectangle.svg', PPIScale(32), -1);
  texMouseToolPolygon := FAtlas.AddFromSVG(path+'Polygon.svg', PPIScale(32), -1);


  FAtlas.TryToPack;
  FAtlas.Build;
  //FAtlas.FreeItemImages;
end;

procedure TFormMain.FreeAppTextureAtlas;
begin
  FAtlas.Free;
  FAtlas := NIL;
end;

procedure TFormMain.LoadCommonData;
begin
  CreateAppTextureAtlas;
  Project.Config.InitDefault;

  ScreenSpriteBuilder := TScreenSpriteBuilder.Create;
  ScreenSpriteBuilder.Initialize;

  ScreenSpriteBank := TScreenSpriteBank.Create;
  ScreenSpriteBank.Initialize;

  UIHandle.InitDefault;
  UIHandle.TargetLayer := LAYER_UI;

  SpriteBank := TSpriteBank.Create;
  LevelBank := TLevelBank.Create;
  FontBank:= TFontBank.Create;
  PanelBank := TPanelBank.Create;
  PathBank := TPathBank.Create;

  ScreenLevelEditor := TScreenLevelEditor.Create;
  ScreenLevelEditor.Initialize;
  // level bank
  ScreenLevelBank := TScreenLevelBank.Create;

  ScreenFontBank := TScreenFontBank.Create;
  ScreenFontBank.Initialize;

  ScreenUIPanelEditor := TScreenUIPanelEditor.Create;
  ScreenUIPanelEditor.Initialize;
  ScreenUIPanelBank := TScreenUIPanelBank.Create;
  ScreenUIPanelBank.Initialize;

  ScreenPathBank := TScreenPathBank.Create;
  ScreenPathBank.Initialize;
  ScreenPathEditor := TScreenPathEditor.Create;
  ScreenPathEditor.Initialize;


  // check if the program was launched by the IDE: if yes, load the appropriate oglc project
  if IdeConnect.Activated then
    Project.Load(IdeConnect.TargetOGLCProjectFilename)
  else
  // else load the last project
  if AppPref.LastProjectFilename <> '' then
    Project.Load(AppPref.LastProjectFilename);

  if LevelBank.Size > 0 then ShowPageLevelBank
    else ShowPageSpriteBank;
end;

procedure TFormMain.FreeCommonData;
begin
  FreeAppTextureAtlas;

  FScene.Mouse.DeleteCursorSprite;
  FScene.ClearAllLayer;

  SpriteBank.Free;
  SpriteBank := NIL;

  LevelBank.Free;
  LevelBank := NIL;

  FontBank.Free;
  FontBank := NIL;

  PanelBank.Free;
  PanelBank := NIL;

  PathBank.Free;
  PathBank := NIL;

  ScreenUIPanelBank.Finalize;
  FreeAndNil(ScreenUIPanelBank);

  ScreenSpriteBuilder.Finalize;
  FreeAndNil(ScreenSpriteBuilder);

  ScreenSpriteBank.Finalize;
  FreeAndNil(ScreenSpriteBank);

  ScreenLevelEditor.Finalize;
  FreeAndNil(ScreenLevelEditor);

  ScreenLevelBank.Finalize;
  FreeAndNil(ScreenLevelBank);

  ScreenFontBank.Finalize;
  FreeAndNil(ScreenFontBank);

  ScreenUIPanelEditor.Finalize;
  FreeAndNil(ScreenUIPanelEditor);

  ScreenPathBank.Finalize;
  FreeAndNil(ScreenPathBank);
  ScreenPathEditor.Finalize;
  FreeAndNil(ScreenPathEditor);
end;

procedure TFormMain.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FScene.DoLoop;
  Done := FALSE;
end;

procedure TFormMain.UpdateWidgets;
begin
  BSpriteBank.Enabled := Notebook1.PageIndex = Notebook1.IndexOf(PageLevelBank);
  BLevelBank.Enabled := Notebook1.PageIndex = Notebook1.IndexOf(PageSpriteBank);

  // project button relative to project disabled if Game Helper is started from the IDE
  BNewProject.Enabled := not IdeConnect.Activated;
  BLoadProject.Enabled := not IdeConnect.Activated;

  BSaveProject.Enabled := Project.IsReady and Project.HasBeenModified;
end;

procedure TFormMain.HideAllPages;
begin
  Notebook1.PageIndex := -1;
end;

procedure TFormMain.ShowPageSpriteBank;
begin
  Notebook1.PageIndex := Notebook1.IndexOf(PageSpriteBank);
  FrameToolsSpriteBank.OnShow;
  FScene.RunScreen(ScreenSpriteBank);
  UpdateWidgets;
  ToolBarMain.Visible := True;
  CBBank.Enabled := True;
end;

procedure TFormMain.ShowPageSpriteBuilder;
begin
  Notebook1.PageIndex := Notebook1.IndexOf(PageSpriteBuilder);
  FrameToolsSpriteBuilder.OnShow;
  FScene.RunScreen(ScreenSpriteBuilder);
  UpdateWidgets;
  ToolBarMain.Visible := False;
  CBBank.Enabled := False;
end;

procedure TFormMain.ShowPageLevelEditor;
begin
  Notebook1.PageIndex := Notebook1.IndexOf(PageLevelEditor);
  FrameToolLevelEditor.OnShow;
  FScene.RunScreen(ScreenLevelEditor);
  UpdateWidgets;
  ToolBarMain.Visible := False;
  CBBank.Enabled := False;
end;

procedure TFormMain.ShowPageLevelBank;
begin
  Notebook1.PageIndex := Notebook1.IndexOf(PageLevelBank);
  FrameToolLevelBank.OnShow;
  FScene.RunScreen(ScreenLevelBank);
  UpdateWidgets;
  ToolBarMain.Visible := True;
  CBBank.Enabled := True;
end;

procedure TFormMain.ShowPageFontBank;
begin
  FScene.RunScreen(ScreenFontBank);
  Notebook1.PageIndex := Notebook1.IndexOf(PageFontBank);
  FrameViewFontBank.Fill;
  UpdateWidgets;
  ToolBarMain.Visible := True;
  CBBank.Enabled := True;
end;

procedure TFormMain.ShowPagePanelBank;
begin
  FScene.RunScreen(ScreenUIPanelBank);
  Notebook1.PageIndex := Notebook1.IndexOf(PagePanelBank);
  FrameToolPanelBank.OnShow;
  UpdateWidgets;
  ToolBarMain.Visible := True;
  CBBank.Enabled := True;
end;

procedure TFormMain.ShowPagePanelEditor;
begin
  Notebook1.PageIndex := Notebook1.IndexOf(PageUIPanelEditor);
  FrameToolUIPanelEditor.OnShow;
  UpdateWidgets;
  ToolBarMain.Visible := False;
  CBBank.Enabled := False;
end;

procedure TFormMain.ShowPagePathBank;
begin
  FScene.RunScreen(ScreenPathBank);
  Notebook1.PageIndex := Notebook1.IndexOf(PagePathBank);
  FramePathBank.OnShow;
  UpdateWidgets;
  ToolBarMain.Visible := True;
  CBBank.Enabled := True;
end;

procedure TFormMain.ShowPagePathEditor;
begin
  Notebook1.PageIndex := Notebook1.IndexOf(PagePathEditor);
  FramePathEditor.OnShow;
  UpdateWidgets;
  ToolBarMain.Visible := False;
  CBBank.Enabled := False;
end;

procedure TFormMain.EditLevelInLevelBank(const aName: string);
begin
  FrameToolLevelEditor.EditLevelInLevelBank(aName);
  ShowPageLevelEditor;
  ToolBarMain.Visible := False;
end;


end.

