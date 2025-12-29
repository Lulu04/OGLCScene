unit frame_tool_panelbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  Types,
  OGLCScene;

type

  { TFrameToolPanelBank }

  TFrameToolPanelBank = class(TFrame)
    BAddPanel: TSpeedButton;
    BHelp: TSpeedButton;
    BDelete: TSpeedButton;
    BDuplicate: TSpeedButton;
    BRename: TSpeedButton;
    BEdit: TSpeedButton;
    Label24: TLabel;
    Panel1: TPanel;
    Panel8: TPanel;
    PanelPanel: TPanel;
    PanelRoot: TPanel;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure BAddPanelClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
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
    procedure DoEditSelected;
    procedure DoRenameSelected;
    procedure DoDuplicateSelected;
    procedure DoDeleteSelected;
  public

    procedure OnShow;

    property IsEditable: boolean read FIsEditable write FIsEditable;
  end;

implementation

uses u_ui_objectlist, u_resourcestring, form_main, u_common,
  u_screen_uipanelbank, u_screen_uipaneleditor, form_showhelp, Graphics;

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
  if Sender = BAddPanel then begin
    FormMain.ShowPagePanelEditor;
    FrameToolUIPanelEditor.EditNewPanel;
  end;

  if Sender = BEdit then DoEditSelected;
  if Sender = BRename then DoRenameSelected;
  if Sender = BDuplicate then DoDuplicateSelected;
  if Sender = BDelete then DoDeleteSelected;
end;

procedure TFrameToolPanelBank.BHelpClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('The Panel Bank contains the panels that you have defined in your project.'#10#10+
  'CREATE A NEW PANEL:'#10+
  ' - click on Panels label then click the ''+'' button.'#10#10+
  'EDIT/RENAME/DUPLICATE/DELETE AN EXISTING PANEL:'#10+
  ' - select a panel in the list and click the appropriate button on the right.');
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
      node.SelectedIndex := 11;
      node.ImageIndex := 11;
      node.MakeVisible;
    end;
  TV.EndUpdate;
end;

procedure TFrameToolPanelBank.HideToolPanels;
begin
  PanelPanel.Visible := False;
  PanelRoot.Visible := False;
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
begin
  ScreenUIPanelBank.ClearView;
  if SelectedIsPanel then ScreenUIPanelBank.ShowPanelFromBank(TV.Selected.Text);

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

procedure TFrameToolPanelBank.DoEditSelected;
var
  item: TPanelDescriptorItem;
begin
  HideToolPanels;
  if not SelectedIsPanel then exit;

  item := PanelBank.GetByName(TV.Selected.Text);
  if item = NIL then exit;

  ScreenUIPanelEditor.EditPanelFromBank(TV.Selected.Text);
  FScene.RunScreen(ScreenUIPanelEditor);
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
    ImageIndex := 11;
    SelectedIndex := 11;
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

