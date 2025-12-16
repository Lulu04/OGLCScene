unit frame_tool_panelbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons, Types;

type

  { TFrameToolPanelBank }

  TFrameToolPanelBank = class(TFrame)
    BAddPanel: TSpeedButton;
    BCancel: TSpeedButton;
    BDelete: TSpeedButton;
    BDuplicate: TSpeedButton;
    BRename: TSpeedButton;
    Label24: TLabel;
    Panel1: TPanel;
    Panel8: TPanel;
    PanelPanel: TPanel;
    PanelRoot: TPanel;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure BAddPanelClick(Sender: TObject);
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
    procedure FillTV;
    procedure HideToolPanels;
    procedure ShowToolPanels;

    procedure UpdatePreview;
    function NodeIsRoot(aNode: TTreeNode): boolean;
    function NodeIsPanel(aNode: TTreeNode): boolean;
    function SelectedIsRoot: boolean;
    function SelectedIsPanel: boolean;
    procedure DoRenameSelected;
    procedure DoDuplicateSelected;
    procedure DoDeleteSelected;
  public

    procedure OnShow;

    property IsEditable: boolean read FIsEditable write FIsEditable;
  end;

implementation

uses u_ui_objectlist, u_resourcestring, u_screen_uipaneleditor, form_main,
  Graphics;

{$R *.lfm}

{ TFrameToolPanelBank }

procedure TFrameToolPanelBank.TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
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

procedure TFrameToolPanelBank.BAddPanelClick(Sender: TObject);
begin
  if Sender = BAddPanel then
    FormMain.ShowPagePanelEditor;

  if Sender = BRename then DoRenameSelected;
  if Sender = BDuplicate then DoDuplicateSelected;
  if Sender = BDelete then DoDeleteSelected;
end;

procedure TFrameToolPanelBank.TVMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TFrameToolPanelBank.TVMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Sender := Sender;
  Shift := Shift;
  WheelDelta := WheelDelta;
  MousePos := MousePos;
  Handled := Handled;
  HideToolPanels;
end;

procedure TFrameToolPanelBank.TVResize(Sender: TObject);
begin
  Sender := Sender;
  HideToolPanels;
end;

procedure TFrameToolPanelBank.TVSelectionChanged(Sender: TObject);
begin
  Sender := Sender;
  HideToolPanels;
  UpdatePreview;
  //if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);

  ShowToolPanels;
end;

procedure TFrameToolPanelBank.FillTV;
var  root, node: TTreeNode;
  i: Integer;
begin
  TV.BeginUpdate;
  TV.Items.Clear;
  TV.Items.AddFirst(NIL, sPanels);
  root := TV.Items.GetFirstNode;
  if PanelBank.Size > 0 then
    for i:=0 to PanelBank.Size-1 do begin
      node := TV.Items.AddChild(root, PanelBank.GetByIndex(i)._Name);
      node.SelectedIndex := 12;
      node.ImageIndex := 12;
      node.MakeVisible;
    end;
  TV.EndUpdate;
end;

procedure TFrameToolPanelBank.HideToolPanels;
begin

end;

procedure TFrameToolPanelBank.ShowToolPanels;
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
    if SelectedIsPanel then MakePanelVisible(PanelPanel);
  end;
end;

procedure TFrameToolPanelBank.UpdatePreview;
var item: TPanelDescriptorItem;
begin
  if (TV.Selected = NIL) or SelectedIsRoot then begin
    ScreenUIPanelEditor.UIPanel.Visible := False;
    exit;
  end;

  item := PanelBank.GetByName(TV.Selected.Text);
  ScreenUIPanelEditor.UIPanel.Visible := True;
  ScreenUIPanelEditor.UIPanel.BodyShape.LoadFromString(item.BodyShape);
  ScreenUIPanelEditor.UIPanel.BackGradient.LoadGradientDataFromString(item.BackGradient);
  ScreenUIPanelEditor.UIPanel.BackGradient.Visible := item.Usegradient;
  ScreenUIPanelEditor.UIPanel.BodyShape.Fill.Visible := not item.Usegradient;
  ScreenUIPanelEditor.UIPanel.BodyShape.ResizeCurrentShape(item.width, item.height, True);
  ScreenUIPanelEditor.UIPanel.SetCoordinate(item.x, item.y);
  if item.DarkenBG then ScreenUIPanelEditor.UIPanel.ActivateSceneDarken(item.DarknessColor);
  ScreenUIPanelEditor.UIPanel.Show(item.ScenarioOnShow);

end;

function TFrameToolPanelBank.NodeIsRoot(aNode: TTreeNode): boolean;
begin
  if aNode = NIL then exit(False);
  Result := aNode.Level = 0;
end;

function TFrameToolPanelBank.NodeIsPanel(aNode: TTreeNode): boolean;
begin
  if aNode = NIL then exit(False);
  Result := aNode.Level = 1;
end;

function TFrameToolPanelBank.SelectedIsRoot: boolean;
begin
  Result := NodeIsRoot(TV.Selected);
end;

function TFrameToolPanelBank.SelectedIsPanel: boolean;
begin
  Result := NodeIsPanel(TV.Selected);
end;

procedure TFrameToolPanelBank.DoRenameSelected;
var oldName, newName: string;
begin
  HideToolPanels;
  if not SelectedIsPanel then exit;

  oldName := TV.Selected.Text;
  if not PanelBank.DoRenameByName(oldName, newName, True) then exit;

  // change name in treeview
  TV.Selected.Text := newName;
end;

procedure TFrameToolPanelBank.DoDuplicateSelected;
var dstName: string;
begin
  HideToolPanels;
  if not SelectedIsPanel then exit;

  if not PanelBank.DoDuplicateByName(TV.Selected.Text, dstName, True) then exit;
  // add the new name in the treeview
  with TV.Items.AddChild(TV.Items.GetFirstNode, dstName) do begin
    ImageIndex := 12;
    SelectedIndex := 12;
    MakeVisible;
  end;
end;

procedure TFrameToolPanelBank.DoDeleteSelected;
begin
  HideToolPanels;
  if not SelectedIsPanel then exit;

  if not PanelBank.DoDeleteByName(TV.Selected.Text, True) then exit;
  // delete in treeview
  TV.Items.Delete(TV.Selected);

  UpdatePreview;
end;

procedure TFrameToolPanelBank.OnShow;
begin
  FillTV;
end;

end.

