unit form_projectconfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, lcl_utils, frame_viewlayerlist;

type

  { TFormProjectConfig }

  TFormProjectConfig = class(TForm)
    BRenameLayer: TSpeedButton;
    BDeleteLayer: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Notebook1: TNotebook;
    PageLayers: TPage;
    PageScene: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    BScene: TSpeedButton;
    BCancel: TSpeedButton;
    BOk: TSpeedButton;
    BLayers: TSpeedButton;
    BAddLayer: TSpeedButton;
    procedure BAddLayerClick(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    NoteBookManager: TNoteBookManager;
    FrameViewLayerList: TFrameViewLayerList;
    FModified: boolean;
    function CheckIntegrity: boolean;
    procedure ConfigToWidget;
    procedure WidgetToConfig;
    procedure ProcessNoteBookPageSelection(Sender: TObject);
    procedure DoAddLayer;
    procedure DoDeleteLayer;
    procedure DoRenameLayer;
  public

  end;

var
  FormProjectConfig: TFormProjectConfig;

implementation

uses u_layerlist, u_project;

{$R *.lfm}

{ TFormProjectConfig }

procedure TFormProjectConfig.FormCreate(Sender: TObject);
begin
  NoteBookManager := TNoteBookManager.Create(Notebook1);
  NoteBookManager.SetActivatedColors(clBtnFace, clBlack);
  NoteBookManager.SetDeactivatedColors(clBtnShadow, clBlack);
  NoteBookManager.LinkButtonToPage(BScene, PageScene);
  NoteBookManager.LinkButtonToPage(BLayers, PageLayers);
  NoteBookManager.ActivePage(PageScene);
  NoteBookManager.OnSelectionChange := @ProcessNoteBookPageSelection;

  FrameViewLayerList := TFrameViewLayerList.Create(Self);
  FrameViewLayerList.Parent := Panel5;
  FrameViewLayerList.Align := alClient;
  FrameViewLayerList.ShowIconEye := False;

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

function TFormProjectConfig.CheckIntegrity: boolean;
begin
  Result := (FrameViewLayerList.Count > 0);
end;

procedure TFormProjectConfig.ConfigToWidget;
begin
  // layers
  FrameViewLayerList.Fill;
end;

procedure TFormProjectConfig.WidgetToConfig;
begin
  // layers
  FrameViewLayerList.SaveLayerConfigToLayerList;
end;

procedure TFormProjectConfig.BOkClick(Sender: TObject);
begin
  if Sender = BCancel then ModalResult := mrCancel;

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

procedure TFormProjectConfig.ProcessNoteBookPageSelection(Sender: TObject);
begin

end;

procedure TFormProjectConfig.DoAddLayer;
var layerName: string;
begin
  layerName := Trim(InputBox('', 'Enter a name for the new layer:', FrameViewLayerList.GetNewDefaultLayerName));
  FrameViewLayerList.AddLayer(layerName);
  FModified := True;
end;

procedure TFormProjectConfig.DoDeleteLayer;
begin
  if FrameViewLayerList.Count <= 1 then exit;
  if FrameViewLayerList.GetSelectedIndex = -1 then exit;
  FrameViewLayerList.DeleteLayer(FrameViewLayerList.GetSelectedIndex);
  FModified := True;
end;

procedure TFormProjectConfig.DoRenameLayer;
var i: integer;
  oldName, newName: String;
begin
  i := FrameViewLayerList.GetSelectedIndex;
  if i = -1 then exit;
  oldName := FrameViewLayerList.Names[i];
  newName := Trim(InputBox('', 'Enter the new name:', oldName));
  if (newName = oldName) or (newName = '') then exit;
  FrameViewLayerList.Names[i] := newName;
  FModified := True;
end;

end.

