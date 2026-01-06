unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, ExtCtrls,
  ComCtrls, BGRABitmap, BGRABitmapTypes, OpenGLContext, OGLCScene, u_common;

type

  { TFormMain }

  TFormMain = class(TForm)
    OpenGLControl1: TOpenGLControl;
    OpenGLControl2: TOpenGLControl;
    OpenGLControl3: TOpenGLControl;
    OpenGLControl4: TOpenGLControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Timer1: TTimer;
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure LoadCommonDataScene1;
    procedure FreeCommonDataScene1;
    procedure LoadCommonDataScene2;
    procedure FreeCommonDataScene2;
    procedure LoadCommonDataScene3;
    procedure FreeCommonDataScene3;
    procedure LoadCommonDataScene4;
    procedure FreeCommonDataScene4;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  public
  end;

var
  FormMain: TFormMain;

implementation
uses screen_scene1, screen_scene2, LazFileUtils;
{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  fontDirectory: String;
begin
  fontDirectory := CleanAndExpandDirectory(Application.Location+'..'+PathDelim+'Data'+PathDelim+'Fonts'+PathDelim);

  FScene1 := TOGLCScene.Create(OpenGLControl1, -1);  //1  4/3
  FScene1.DesignPPI := 96;
  FScene1.LayerCount := LAYER_COUNT_SCENE1;
  FScene1.CreateLogFile(Application.Location+'scene1.log', True);
  FScene1.OnLoadCommonData := @LoadCommonDataScene1;
  FScene1.OnFreeCommonData := @FreeCommonDataScene1;
  FScene1.FontManager.ScanProjectFont(fontDirectory);

  FScene2 := TOGLCScene.Create(OpenGLControl2, -1);
  FScene2.DesignPPI := 96;
  FScene2.LayerCount := LAYER_COUNT_SCENE2;
  FScene2.CreateLogFile(Application.Location+'scene2.log', True);
  FScene2.OnLoadCommonData := @LoadCommonDataScene2;
  FScene2.OnFreeCommonData := @FreeCommonDataScene2;
  FScene2.FontManager.ScanProjectFont(fontDirectory);

  FScene3 := TOGLCScene.Create(OpenGLControl3, -1);
  FScene3.DesignPPI := 96;
  FScene3.LayerCount := LAYER_COUNT_SCENE3;
  FScene3.CreateLogFile(Application.Location+'scene3.log', True);
  FScene3.OnLoadCommonData := @LoadCommonDataScene3;
  FScene3.OnFreeCommonData := @FreeCommonDataScene3;
  FScene3.FontManager.ScanProjectFont(fontDirectory);

  FScene4 := TOGLCScene.Create(OpenGLControl4, -1);
  FScene4.DesignPPI := 96;
  FScene4.LayerCount := LAYER_COUNT_SCENE4;
  FScene4.CreateLogFile(Application.Location+'scene4.log', True);
  FScene4.OnLoadCommonData := @LoadCommonDataScene4;
  FScene4.OnFreeCommonData := @FreeCommonDataScene4;
  FScene4.FontManager.ScanProjectFont(fontDirectory);

  Application.OnIdle := @ProcessApplicationIdle;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FScene1.Free;
  FScene1 := NIL;
  FScene2.Free;
  FScene2 := NIL;
  FScene3.Free;
  FScene3 := NIL;
  FScene4.Free;
  FScene4 := NIL;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Timer1.Enabled := FALSE;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene1.ProcessOnKeyDown(Key, Shift);
  FScene2.ProcessOnKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene1.ProcessOnKeyUp(Key, Shift);
  FScene2.ProcessOnKeyUp(Key, Shift);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FScene.OpenGLLibLoaded then
    ShowMessage('ERROR: OpenGL library could not be loaded...'+LineEnding+
                'Check if your system is compatible with OpenGL 3.3 core'+LineEnding+
                'and if the library is well installed on your computer');
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  Caption := Format('OGLCScene - Multi scenes   -   Scene1 (%d,%d) FPS %d surfaces %d |'+
                                                   'Scene2 (%d,%d) FPS %d surfaces %d |'+
                                                   'Scene3 (%d,%d) FPS %d surfaces %d |'+
                                                   'Scene4 (%d,%d) FPS %d surfaces %d |',
  [FScene1.Width, FScene1.Height, FScene1.FPS, FScene1.SurfaceCount,
   FScene2.Width, FScene2.Height, FScene2.FPS, FScene2.SurfaceCount,
   FScene3.Width, FScene3.Height, FScene3.FPS, FScene3.SurfaceCount,
   FScene4.Width, FScene4.Height, FScene4.FPS, FScene4.SurfaceCount]);
end;

procedure TFormMain.LoadCommonDataScene1;
begin
  // we create the screen for the scene1
  ScreenDemo1 := TScreenDemo1.Create;
  FScene1.RunScreen(ScreenDemo1, False);
end;

procedure TFormMain.FreeCommonDataScene1;
begin
  FreeAndNil(ScreenDemo1);
end;

procedure TFormMain.LoadCommonDataScene2;
begin
  // we create the screen for the scene2
  ScreenDemo2 := TScreenDemo2.Create;
  FScene2.RunScreen(ScreenDemo2, False);
end;

procedure TFormMain.FreeCommonDataScene2;
begin
  FreeAndNil(ScreenDemo2);
end;

procedure TFormMain.LoadCommonDataScene3;
var fontDescriptor: TFontDescriptor;
  o: TDeformationGrid;
begin
  // for the scene3, we create the object directly here
  fontDescriptor.Create('Roboto', Round(FScene3.Height/8), [], BGRA(255,60,97,200), BGRA(255,255,150), 8, BGRA(0,255,0,255), 20, 20, 15);
  o := TDeformationGrid.Create(FScene3, fontDescriptor, 'Deformation grid');
  FScene3.Add(o);
  with o do begin
    CenterOnScene;
    SetGrid(20,20);
    ApplyDeformation(dtTumultuousWater);
    DeformationSpeed.Value := PointF(1.5,1.6);
    Amplitude.Value := PointF(0.2,0.3);
  end;
  FScene3.BackgroundColor := BGRA(40,40,0);
end;

procedure TFormMain.FreeCommonDataScene3;
begin
  FScene3.ClearAllLayer;
end;

procedure TFormMain.LoadCommonDataScene4;
var o: TSprite;
  fontDescriptor: TFontDescriptor;
begin
  // for the scene4, we create the object directly here
  fontDescriptor.Create('Roboto', Round(FScene4.Height/8), [], BGRA(255,0,255));
  o :=  TSprite.Create(FScene4, fontDescriptor, 'HELLO WORLD !');
  FScene4.Add(o);
  o.CenterOnScene;
  o.Angle.AddConstant(50);
end;

procedure TFormMain.FreeCommonDataScene4;
begin
  FScene4.ClearAllLayer;
end;

procedure TFormMain.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FScene1.DoLoop;
  FScene2.DoLoop;
  FScene3.DoLoop;
  FScene4.DoLoop;
  Done := FALSE;
end;


end.

