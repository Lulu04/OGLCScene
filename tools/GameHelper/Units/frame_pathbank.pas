unit frame_pathbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, Buttons, StdCtrls,
  u_resourcestring, u_ui_objectlist, u_screen_pathbank, Types;

type

  { TFramePathBank }

  TFramePathBank = class(TFrame)
    BAdd: TSpeedButton;
    BDelete: TSpeedButton;
    BDuplicate: TSpeedButton;
    BEdit: TSpeedButton;
    BHelp: TSpeedButton;
    BRename: TSpeedButton;
    Label24: TLabel;
    Panel1: TPanel;
    Panel8: TPanel;
    PanelPath: TPanel;
    PanelRoot: TPanel;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure BAddClick(Sender: TObject);
    procedure TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TVMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TVMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TVResize(Sender: TObject);
    procedure TVSelectionChanged(Sender: TObject);
  private
    FIsEditable: boolean;
    procedure UpdatePreview;
    procedure FillTV;
    procedure HideToolPanels;
    procedure ShowToolPanels;
    function NodeIsRoot(aNode: TTreeNode): boolean;
    function NodeIsPath(aNode: TTreeNode): boolean;
    function SelectedIsRoot: boolean;
    function SelectedIsPath: boolean;
    procedure DoEditSelected;
    procedure DoRenameSelected;
    procedure DoDuplicateSelected;
    procedure DoDeleteSelected;
  public
    procedure OnShow;

    property IsEditable: boolean read FIsEditable write FIsEditable;
  end;

implementation

uses u_screen_patheditor, u_common, form_main, Graphics;

{$R *.lfm}

{ TFramePathBank }

procedure TFramePathBank.TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var r: TRect;
  p: TPoint;
  ft: TFontStyles;
begin
  Sender := Sender;
  if (Stage = cdPostPaint) then begin // all is painted
    ft := TV.Font.Style;
    r := Node.DisplayRect(True);
    With TV.Canvas do begin
      // background
      if cdsSelected in State then begin
        Brush.Color := TV.SelectionColor;
        TV.Font.Color := clWhite
      end else begin
        Brush.Color := TV.BackgroundColor;
        TV.Font.Color := RGBToColor(220,220,220);
      end;

      Brush.Style := bsSolid;
      Pen.Style := psClear;
      FillRect(r);
      //Pen.Style := psSolid;

      // hot track (mouse is over)
      p := TV.ScreenToClient(Mouse.CursorPos);
      if TV.GetNodeAt(p.x, p.y) = Node then
        TV.Font.Style := TV.Font.Style+[fsUnderline];

      // text
      Brush.Style := bsClear;
      if Node.Level = 1 then
        TV.Font.Style := TV.Font.Style+[fsBold];
      TextOut(r.Left+2, r.Top+(r.Height-TV.Font.Height) div 2, Node.Text);
      TV.Font.Style := ft;
    end;
    PaintImages := True;
    DefaultDraw := false;
  end else begin
    PaintImages := True;
    DefaultDraw := True;
  end;
end;

procedure TFramePathBank.BAddClick(Sender: TObject);
begin
  if Sender = BAdd then begin
    ScreenPathEditor.EditNewPath;
    FScene.RunScreen(ScreenPathEditor);
    FormMain.ShowPagePathEditor;
    FramePathEditor.EditNewPath;
  end;

  if Sender = BEdit then DoEditSelected;
  if Sender = BRename then DoRenameSelected;
  if Sender = BDuplicate then DoDuplicateSelected;
  if Sender = BDelete then DoDeleteSelected;
end;

procedure TFramePathBank.TVMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var n: TTreeNode;
begin
  Sender := Sender;
  Shift := Shift;
  n := TV.GetNodeAt(X, Y);
  if n = NIL then
    TV.Selected := NIL;

  if (Button = mbLeft) and (TV.Selected <> NIL) then
    ShowToolPanels;
end;

procedure TFramePathBank.TVMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Sender := Sender;
  Shift := Shift;
  WheelDelta := WheelDelta;
  MousePos := MousePos;
  Handled := Handled;
  HideToolPanels;
end;

procedure TFramePathBank.TVResize(Sender: TObject);
begin
  Sender := Sender;
  HideToolPanels;
end;

procedure TFramePathBank.TVSelectionChanged(Sender: TObject);
begin
  Sender := Sender;
  HideToolPanels;
  UpdatePreview;
  ShowToolPanels;
end;

procedure TFramePathBank.UpdatePreview;
begin
  ScreenPathBank.ClearView;
  if SelectedIsPath then ScreenPathBank.ShowPathFromBank(TV.Selected.Text);
end;

procedure TFramePathBank.FillTV;
var  root, node: TTreeNode;
  i: Integer;
begin
  TV.BeginUpdate;
  TV.Items.Clear;
  TV.Items.AddFirst(NIL, sPaths);
  root := TV.Items.GetFirstNode;
  if PathBank.Size > 0 then
    for i:=0 to PathBank.Size-1 do begin
      node := TV.Items.AddChild(root, PathBank.GetByIndex(i)._Name);
      node.SelectedIndex := 12;
      node.ImageIndex := 12;
      node.MakeVisible;
    end;
  TV.EndUpdate;
end;

procedure TFramePathBank.HideToolPanels;
begin
  PanelPath.Visible := False;
  PanelRoot.Visible := False;
end;

procedure TFramePathBank.ShowToolPanels;
var r, r1: TRect;
  procedure MakePanelVisible(aPanel: TPanel);
  var x: integer;
  begin
    x := r.Right+ScaleDesignToForm(10);
    if x+aPanel.Width > TV.ClientWidth then
      x := TV.ClientWidth-aPanel.Width;
    aPanel.Left := x;
    aPanel.Top := r.Top+r.Height div 2 - aPanel.Height div 2;
    aPanel.Visible := True;
  end;
begin
  HideToolPanels;
  if IsEditable then begin
    if TV.Selected = NIL then exit;
    r := TV.Selected.DisplayRect(True);
    r1.TopLeft := TV.ClientToParent(r.TopLeft);
    r1.BottomRight := TV.ClientToParent(r.BottomRight);
    r := r1;

    if SelectedIsRoot then MakePanelVisible(PanelRoot)
    else
    if SelectedIsPath then MakePanelVisible(PanelPath);
  end;
end;

function TFramePathBank.NodeIsRoot(aNode: TTreeNode): boolean;
begin
  if aNode = NIL then exit(False);
  Result := aNode.Level = 0;
end;

function TFramePathBank.NodeIsPath(aNode: TTreeNode): boolean;
begin
  if aNode = NIL then exit(False);
  Result := aNode.Level = 1;
end;

function TFramePathBank.SelectedIsRoot: boolean;
begin
  Result := NodeIsRoot(TV.Selected);
end;

function TFramePathBank.SelectedIsPath: boolean;
begin
  Result := NodeIsPath(TV.Selected);
end;

procedure TFramePathBank.DoEditSelected;
var
  item: TPathDescriptorItem;
begin
  HideToolPanels;
  if not SelectedIsPath then exit;

  item := PathBank.GetByName(TV.Selected.Text);
  if item = NIL then exit;

  ScreenPathEditor.EditPathFromBank(TV.Selected.Text);
  FScene.RunScreen(ScreenPathEditor);
end;

procedure TFramePathBank.DoRenameSelected;
var oldName, newName: string;
begin
  HideToolPanels;
  if not SelectedIsPath then exit;

  oldName := TV.Selected.Text;
  if not PathBank.DoRenameByName(oldName, newName, True) then exit;

  // change name in treeview
  TV.Selected.Text := newName;
end;

procedure TFramePathBank.DoDuplicateSelected;
var dstName: string;
begin
  HideToolPanels;
  if not SelectedIsPath then exit;

  if not PathBank.DoDuplicateByName(TV.Selected.Text, dstName, True) then exit;
  // add the new name in the treeview
  with TV.Items.AddChild(TV.Items.GetFirstNode, dstName) do begin
    ImageIndex := 12;
    SelectedIndex := 12;
    MakeVisible;
  end;
end;

procedure TFramePathBank.DoDeleteSelected;
begin
  HideToolPanels;
  if not SelectedIsPath then exit;

  if not PathBank.DoDeleteByName(TV.Selected.Text, True) then exit;
  // delete in treeview
  TV.Items.Delete(TV.Selected);

  UpdatePreview;
end;

procedure TFramePathBank.OnShow;
begin
  FillTV;
end;

end.

