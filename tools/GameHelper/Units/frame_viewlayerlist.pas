unit frame_viewlayerlist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Types,
  Graphics;

type

  { TFrameViewLayerList }

  TFrameViewLayerList = class(TFrame)
    LB: TListBox;
    Panel1: TPanel;
    procedure LBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure LBKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBMouseLeave(Sender: TObject);
    procedure LBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure LBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FMouseCanMoveItem: boolean;
    FMouseOrigin: TPoint;
    FLeftClickedIndex: integer;
    FItemIndexUnderMouse: integer;
    FShowIconEye: boolean;
    FTextStyleForTextRect: TTextStyle;
    function GetSelectedCount: integer;
    procedure SetShowIconEye(AValue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;

    procedure Fill;
    property SelectedCount: integer read GetSelectedCount;
    // default is true
    property ShowIconEye: boolean read FShowIconEye write SetShowIconEye;
  end;

implementation
uses LCLType, LCLHelper, u_project, u_utils, u_datamodule, u_common,
  u_layerlist, Math;

{$R *.lfm}

{ TFrameViewLayerList }

procedure TFrameViewLayerList.LBDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var p: TPoint;
 i, xx, yy: integer;
begin
  with LB.Canvas do
  begin
    if State >= [odSelected] then begin
      Brush.Color := RGBToColor(94,128,130);
      Font.Color := clWhite;
    end else begin
      if Index Mod 2 = 0 then Brush.Color := LB.Color
        else Brush.Color := PercentColor(LB.Color,0.07);
      Font.Color := clBlack;
    end;
    // render dot rectangle if mouse is over item
    if Index = FItemIndexUnderMouse then begin
      Pen.Style := psDot;
      Pen.Color := PercentColor(LB.Color,0.95);
      Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Bottom);
    end else FillRect(ARect);

    // render drag target position
    if FLeftClickedIndex <> -1 then begin
      p := LB.ScreenToClient(Mouse.CursorPos);
      p.y := EnsureRange(p.y, 0, LB.ClientHeight);
      i := LB.GetIndexAtY(p.y);
      if i = Index then begin
        Pen.Style := psClear;
        Brush.Color := clHighLight;
        if i > FLeftClickedIndex then
          Rectangle(ARect.Left-1, ARect.Bottom-3, ARect.Right+1, ARect.Bottom)
        else
          Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Top+3);
      end;
    end;
    Brush.Style := bsClear;

    // render text
    xx := ARect.Left;
    if FShowIconEye then yy := ARect.Right-Round(DataModule1.ILIconLayerList.Width*1.3)
      else yy := ARect.Right;
    TextRect(Rect(xx, ARect.Top,
                  yy,
                  ARect.Bottom),
                  ARect.Left, ARect.Top, LB.Items.Strings[Index], FTextStyleForTextRect);

    // render icon visible or not
    if FShowIconEye then begin
      xx := ARect.Right-DataModule1.ILIconLayerList.Width;
      yy := ARect.Top + (ARect.Height-DataModule1.ImageList1.Height) div 2;
      if Layers.UserLayerIsVisible(Index) then DataModule1.ILIconLayerList.Draw(LB.Canvas, xx, yy, 0)
        else DataModule1.ILIconLayerList.Draw(LB.Canvas, xx, yy, 1);
    end;
  end;
end;

procedure TFrameViewLayerList.LBKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then Key := VK_UNKNOWN;
end;

procedure TFrameViewLayerList.LBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  i := LB.GetIndexAtY(Y);

  if Button = mbLeft then begin
    if i = -1 then LB.ItemIndex := -1 // click on empty area = unselect all
    else begin
      // check for drag
      if (X < LB.ClientWidth-DataModule1.ILIconLayerList.Width) and
         FMouseCanMoveItem then begin
           FLeftClickedIndex := i;
           FMouseOrigin := LB.ClientToScreen(Point(X,Y));
      end else FLeftClickedIndex := -1;
    end;
  end;


{  if (Button = mbLeft) and (i = -1) then
    LB.ItemIndex := -1;

  // prepare for drag
  if (Button = mbLeft) and (i <> -1) and FMouseCanMoveItem and
     (LB.Count > 1) then begin
    FLeftClickedIndex := i;
    FMouseOrigin := LB.ClientToScreen(Point(X,Y));
  end else FLeftClickedIndex := -1;   }

{  // right click on item = select it (except if it is already selected)
  if (Button = mbRight) and (i <> -1) then
    if not LB.Selected[i] then
      LB.ItemIndex := i; }
end;

procedure TFrameViewLayerList.LBMouseLeave(Sender: TObject);
begin
  if FLeftClickedIndex <> -1 then exit;

  FItemIndexUnderMouse := -1;
  LB.Cursor := crDefault;
  LB.Invalidate;
end;

procedure TFrameViewLayerList.LBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  // dragging an item
  if (FLeftClickedIndex <> -1) and (LB.ItemIndex <> FLeftClickedIndex) then begin
    LB.ItemIndex := FLeftClickedIndex;
    LB.Invalidate;
    exit;
  end;

  // check if the mouse is over an item
  i := LB.GetIndexAtY(Y);
  if i <> FItemIndexUnderMouse then begin
    if i = -1 then
      LB.Cursor := crDefault
    else
      LB.Cursor := crHandPoint;
    FItemIndexUnderMouse := i;
    LB.Invalidate;
  end;
end;

procedure TFrameViewLayerList.LBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: integer;
 flagCanMove: boolean;
begin
  if Button = mbLeft then begin
    if FLeftClickedIndex <> -1 then begin
      Y := EnsureRange(Y, 0, LB.ClientHeight);
      i := LB.GetIndexAtY(Y);
      // user can moves item only if one is selected
      flagCanMove := (i <> -1) and (i <> FLeftClickedIndex);

      if flagCanMove then begin
        LB.MoveSelection(i-FLeftClickedIndex);
        Project.SetModified;
      end;
      FLeftClickedIndex := -1;
      LB.Invalidate;
    end else begin
      // click on eye icon
      if X >= LB.ClientWidth-DataModule1.ILIconLayerList.Width then begin
        i := LB.GetIndexAtXY(X, Y);
        if i <> -1 then begin
          Layers.SetUserLayerVisible(i, not Layers.UserLayerIsVisible(i));
          LB.Invalidate;
        end;
      end;
    end;
  end;
end;

function TFrameViewLayerList.GetSelectedCount: integer;
begin
  Result := LB.SelCount;
end;

procedure TFrameViewLayerList.SetShowIconEye(AValue: boolean);
begin
  if FShowIconEye = AValue then Exit;
  FShowIconEye := AValue;
  LB.Invalidate;
end;

constructor TFrameViewLayerList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItemIndexUnderMouse := -1;
  FLeftClickedIndex := -1;
  LB.ItemHeight := 20;
  FShowIconEye := True;

  FTextStyleForTextRect.Alignment := taLeftJustify;
  FTextStyleForTextRect.Layout := Graphics.tlCenter;
  FTextStyleForTextRect.SingleLine := FALSE;
  FTextStyleForTextRect.Wordbreak := TRUE;
  FTextStyleForTextRect.Clipping := TRUE;
  FTextStyleForTextRect.Opaque := FALSE;
  FTextStyleForTextRect.SystemFont := FALSE;
end;

procedure TFrameViewLayerList.Fill;
begin
  Layers.FillListBox(LB);
  LB.Invalidate;
end;

end.

