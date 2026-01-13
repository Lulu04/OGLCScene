unit form_projectconfig;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin, ComCtrls, lcl_utils, OGLCScene, frame_viewlayerlist;

type

  { TFormProjectConfig }

  TFormProjectConfig = class(TForm)
    BEditors: TSpeedButton;
    BRenameLayer: TSpeedButton;
    BDeleteLayer: TSpeedButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Notebook1: TNotebook;
    PageEditors: TPage;
    PageLayers: TPage;
    PageScene: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    BScene: TSpeedButton;
    BOk: TSpeedButton;
    BLayers: TSpeedButton;
    BAddLayer: TSpeedButton;
    Panel6: TPanel;
    BEraseLevelBank: TSpeedButton;
    Panel7: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RBWindowed: TRadioButton;
    RBMaximizeSceneSize: TRadioButton;
    SE1: TSpinEdit;
    SE2: TSpinEdit;
    UpDown1: TUpDown;
    procedure BAddLayerClick(Sender: TObject);
    procedure BEraseLevelBankClick(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SE1Change(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    NoteBookManager: TNoteBookManager;
    FrameViewLayerList: TFrameViewLayerList;
    FModified, FInitializingWidget: boolean;
    function CheckIntegrity: boolean;
    procedure ConfigToWidget;
    procedure WidgetToConfig;
    procedure ProcessNoteBookPageSelection(Sender: TObject);
    procedure DoAddLayer;
    procedure DoDeleteLayer;
    procedure DoRenameLayer;
    procedure DoOnLayerNamesChanged(aExportFileGameLevel: boolean);
  public

  end;

var
  FormProjectConfig: TFormProjectConfig;

implementation

uses u_layerlist, u_project, u_levelbank, form_main;

{$R *.lfm}

{ TFormProjectConfig }

procedure TFormProjectConfig.FormCreate(Sender: TObject);
begin
  NoteBookManager := TNoteBookManager.Create(Notebook1);
  NoteBookManager.SetActivatedColors(clBtnFace, clBlack);
  NoteBookManager.SetDeactivatedColors(clBtnShadow, clBlack);
  NoteBookManager.LinkButtonToPage(BScene, PageScene);
  NoteBookManager.LinkButtonToPage(BLayers, PageLayers);
  NoteBookManager.LinkButtonToPage(BEditors, PageEditors);
  NoteBookManager.ActivePage(PageScene);
  NoteBookManager.OnSelectionChange := @ProcessNoteBookPageSelection;

  FrameViewLayerList := TFrameViewLayerList.Create(Self);
  FrameViewLayerList.Parent := Panel5;
  FrameViewLayerList.Align := alClient;
  FrameViewLayerList.ShowIconEye := False;
  FrameViewLayerList.ShowDecorLoopMode := False;

end;

procedure TFormProjectConfig.FormDestroy(Sender: TObject);
begin
  NoteBookManager.Free;
end;

procedure TFormProjectConfig.FormShow(Sender: TObject);
begin
  FModified := False;
  ConfigToWidget;
end;

procedure TFormProjectConfig.SE1Change(Sender: TObject);
begin
  FModified := True;
end;

procedure TFormProjectConfig.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  if FrameViewLayerList.MoveSelected(Button = btNext) then
    DoOnLayerNamesChanged(True);
end;

function TFormProjectConfig.CheckIntegrity: boolean;
begin
  Result := (FrameViewLayerList.Count > 0);
end;

procedure TFormProjectConfig.ConfigToWidget;
var A: TArrayOfInteger;
begin
  FInitializingWidget := True;

  // scene
  A := Project.Config.TargetLazarusProject.UCommonGetDesignValues;
  SE1.Value := A[0]; // Project.Config.SceneWidth;
  SE2.Value := A[1]; // Project.Config.SceneHeight;
  if Project.Config.TargetLazarusProject.ProjectConfig_MaximizeSceneOnMonitor
    then RBMaximizeSceneSize.Checked := True
    else RadioButton1.Checked := True;
  if Project.Config.TargetLazarusProject.ProjectConfig_WindowedMode
    then RBWindowed.Checked := True
    else RadioButton2.Checked := True;
  // layers
  FrameViewLayerList.Fill;
  // level editor
  Label4.Caption := LevelBank.Size.ToString;
  CheckBox1.Checked := Project.Config.CommonShowFlyingTxt;

  FInitializingWidget := False;
end;

procedure TFormProjectConfig.WidgetToConfig;
begin
  // scene
  Project.Config.TargetLazarusProject.UCommonSetDesignValues([SE1.Value, SE2.Value, Self.Monitor.PixelsPerInch]);
  Project.Config.SceneWidth := SE1.Value;
  Project.Config.SceneHeight := SE2.Value;
  Project.Config.TargetLazarusProject.ProjectConfig_MaximizeSceneOnMonitor := RBMaximizeSceneSize.Checked;
  Project.Config.TargetLazarusProject.ProjectConfig_WindowedMode := RBWindowed.Checked;

{  // layers
  FrameViewLayerList.SaveLayerConfigToLayerList;
  // set also the layer names in unit u_common.pas
  Project.Config.TargetLazarusProject.UCommonSetLayerNames(Layers.UserLayersToStringArray);  }

  // level editor
  Project.Config.CommonShowFlyingTxt := CheckBox1.Checked;
  Project.Save;
end;

procedure TFormProjectConfig.BOkClick(Sender: TObject);
begin

  if Sender = BOk then begin
    if not CheckIntegrity then exit;
    if FModified then begin
      WidgetToConfig;
      Project.SetModified;
      FModified := False;
    end;
    ModalResult := mrOk;
  end;
end;

procedure TFormProjectConfig.BAddLayerClick(Sender: TObject);
begin
  if Sender = BAddLayer then DoAddLayer;
  if Sender = BDeleteLayer then DoDeleteLayer;
  if Sender = BRenameLayer then DoRenameLayer;
end;

procedure TFormProjectConfig.BEraseLevelBankClick(Sender: TObject);
begin
  if LevelBank.Size = 0 then exit;

  if QuestionDlg('','Are you sure ?', mtWarning, [mrOk, 'Erase', mrCancel, 'Cancel'], 0) = mrCancel then exit;
  LevelBank.Clear;
  FrameToolLevelBank.ClearAll;
  Project.SetModified;
end;

procedure TFormProjectConfig.ProcessNoteBookPageSelection(Sender: TObject);
begin

end;

procedure TFormProjectConfig.DoAddLayer;
begin
  if FrameViewLayerList.AddLayer then
    DoOnLayerNamesChanged(True);
end;

procedure TFormProjectConfig.DoDeleteLayer;
begin
  if FrameViewLayerList.DeleteSelected then
    DoOnLayerNamesChanged(True);
end;

procedure TFormProjectConfig.DoRenameLayer;
begin
  if FrameViewLayerList.RenameSelected then
    DoOnLayerNamesChanged(False);
end;

procedure TFormProjectConfig.DoOnLayerNamesChanged(aExportFileGameLevel: boolean);
begin
  if LevelBank.Size > 0 then begin
    // save the level bank
    LevelBank.Save;
    // generate the level's unit
    if aExportFileGameLevel then
      LevelBank.ExportToFileGameLevel;
  end;
  // set the layer names in u_common
  Project.Config.TargetLazarusProject.UCommonSetLayerNames(Layers.UserLayersToStringArray);
end;

end.

