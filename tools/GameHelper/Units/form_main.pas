unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, Menus, OpenGLContext,
  OGLCScene, u_common, Types,
  frame_tool_spritebuilder,
  frame_tool_spritebank,
  frame_tool_leveleditor;

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
    BLevelEditor: TSpeedButton;
    Label1: TLabel;
    Label3: TLabel;
    Notebook1: TNotebook;
    OGL: TOpenGLControl;
    PageLevelEditor: TPage;
    PageLevelBank: TPage;
    PageSpriteBuilder: TPage;
    PageSpriteBank: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    BSpriteBank: TSpeedButton;
    BSpriteBuilder: TSpeedButton;
    Panel8: TPanel;
    Panel9: TPanel;
    ToolBar1: TToolBar;
    BNewScreen: TToolButton;
    BLoadScreen: TToolButton;
    BSaveScreen: TToolButton;
    ToolButton1: TToolButton;
    BScreenOptions: TToolButton;
    procedure BLoadScreenClick(Sender: TObject);
    procedure BNewScreenClick(Sender: TObject);
    procedure BSaveScreenClick(Sender: TObject);
    procedure BSpriteBankClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OGLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
  private
    procedure LoadCommonData;
    procedure FreeCommonData;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  public
    procedure UpdateWidgets;

    procedure ShowPageSpriteBank;
    procedure ShowPageSpriteBuilder;

    procedure EditSpriteInSpriteBank(const aName: string);
  end;

var
  FormMain: TFormMain;

  FrameToolsSpriteBuilder: TFrameToolsSpriteBuilder;
  FrameToolsSpriteBank: TFrameToolSpriteBank;
  FrameToolLevelEditor: TFrameToolLevelEditor;

implementation
uses u_screen_spritebuilder, u_project, u_app_pref, u_screen_template,
  u_spritebank, u_ui_handle, u_screen_spritebank, u_ui_atlas, u_datamodule,
  u_screen_levelbank, BGRABitmap, BGRABitmapTypes;
{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Project := TProject.Create;

  FScene := TOGLCScene.Create(OGL, -1);
  FScene.DesignPPI := 96;
  FScene.LayerCount := LAYER_COUNT;
  FScene.BackgroundColor := BGRA(40,40,40);
  FScene.ScreenFadeTime := 0;
  FScene.CreateLogFile(Application.Location+'scene.log', True);

  FScene.OnLoadCommonData := @LoadCommonData;
  FScene.OnFreeCommonData := @FreeCommonData;

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


end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FScene.Free;
  FScene := NIL;
  FreeAndNil(Project);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := Project.DoUserPromptToSaveProject;
end;

procedure TFormMain.BNewScreenClick(Sender: TObject);
begin
  Project.New;
end;

procedure TFormMain.BSaveScreenClick(Sender: TObject);
begin
  Project.Save;
  if Project.IsReady then AppPref.LastProjectFilename := Project.Filename;
end;

procedure TFormMain.BSpriteBankClick(Sender: TObject);
begin
  if Sender = BSpriteBank then begin
    FScene.RunScreen(ScreenSpriteBank);
    Notebook1.PageIndex := Notebook1.IndexOf(PageSpriteBank);
    FrameToolsSpriteBank.OnShow;
    UpdateWidgets;
  end;

  if Sender = BSpriteBuilder then begin
    FScene.RunScreen(ScreenSpriteBuilder);
    Notebook1.PageIndex := Notebook1.IndexOf(PageSpriteBuilder);
    FrameToolsSpriteBuilder.OnShow;
    UpdateWidgets;
  end;

  if Sender = BLevelBank then begin
    FScene.RunScreen(ScreenLevelBank);
    Notebook1.PageIndex := Notebook1.IndexOf(PageLevelBank);
    FrameToolLevelEditor.OnShow;
    UpdateWidgets;
  end;
end;

procedure TFormMain.BLoadScreenClick(Sender: TObject);
begin
  Project.Load;
  if Project.IsReady then AppPref.LastProjectFilename := Project.Filename;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  FScene.ProcessOnKeyDown(Key, Shift);
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessOnKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  FScene.ProcessOnKeyUp(Key, Shift);
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessOnKeyUp(Key, Shift);
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

procedure TFormMain.LoadCommonData;
begin
  UIAtlas.InitDefault;
  UIAtlas.CreateAtlas;

  ScreenSpriteBuilder := TScreenSpriteBuilder.Create;
  ScreenSpriteBuilder.Initialize;

  ScreenSpriteBank := TScreenSpriteBank.Create;
  ScreenSpriteBank.Initialize;

  UIHandle.InitDefault;
  UIHandle.TargetLayer := LAYER_UI;

  SpriteBank := TSpriteBank.Create;

  // level bank
  ScreenLevelBank := TScreenLevelBank.Create;

  // load last project
  if AppPref.LastProjectFilename <> ''
    then Project.Load(AppPref.LastProjectFilename);

  ShowPageSpriteBank;
end;

procedure TFormMain.FreeCommonData;
begin
  FScene.Mouse.DeleteCursorSprite;
  FScene.ClearAllLayer;
  UIAtlas.FreeAtlas;

  SpriteBank.Free;
  SpriteBank := NIL;

  ScreenSpriteBuilder.Finalize;
  FreeAndNil(ScreenSpriteBuilder);

  ScreenSpriteBank.Finalize;
  FreeAndNil(ScreenSpriteBank);

  ScreenLevelBank.Finalize;
  FreeAndNil(ScreenLevelBank);
end;

procedure TFormMain.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FScene.DoLoop;
  Done := FALSE;
end;

procedure TFormMain.UpdateWidgets;
begin
  BSpriteBank.Enabled := Notebook1.PageIndex = Notebook1.IndexOf(PageLevelBank);
//  BSpriteBuilder.Enabled := (Notebook1.PageIndex <> Notebook1.IndexOf(PageSpriteBuilder)) and
//                            (Notebook1.PageIndex <> Notebook1.IndexOf(PageLevelEditor));

  BLevelBank.Enabled := Notebook1.PageIndex = Notebook1.IndexOf(PageSpriteBank);

  BSpriteBuilder.Enabled := BSpriteBank.Enabled or BLevelBank.Enabled;
  BLevelEditor.Enabled := BSpriteBank.Enabled or BLevelBank.Enabled;
end;

procedure TFormMain.ShowPageSpriteBank;
begin
  Notebook1.PageIndex := Notebook1.IndexOf(PageSpriteBank);
  FrameToolsSpriteBank.OnShow;
  FScene.RunScreen(ScreenSpriteBank);
  UpdateWidgets;
end;

procedure TFormMain.ShowPageSpriteBuilder;
begin
  Notebook1.PageIndex := Notebook1.IndexOf(PageSpriteBuilder);
  FrameToolsSpriteBuilder.OnShow;
  FScene.RunScreen(ScreenSpriteBuilder);
  UpdateWidgets;
end;

procedure TFormMain.EditSpriteInSpriteBank(const aName: string);
begin
  FrameToolsSpriteBuilder.EditSpriteInSpriteBank(aName);
  ShowPageSpriteBuilder;
end;


end.

