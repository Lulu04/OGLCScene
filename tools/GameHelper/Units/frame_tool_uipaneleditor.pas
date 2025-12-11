unit frame_tool_uipaneleditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls, Dialogs,
  Buttons, Spin,
  OGLCScene, BGRABitmap, BGRABitmapTypes,
  frame_edit_uibodyshape, frame_texturelist, u_texture_list;

type

  { TFrameToolUIPanelEditor }

  TFrameToolUIPanelEditor = class(TFrame)
    BCancel: TSpeedButton;
    BSave: TSpeedButton;
    CBTopLeft: TComboBox;
    CBTopRight: TComboBox;
    CBBottomLeft: TComboBox;
    CBBottomRight: TComboBox;
    CBDarkenBG: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    FSE1: TFloatSpinEdit;
    FSE2: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel8: TPanel;
    RBOptionNormal: TRadioButton;
    RBOptionModal: TRadioButton;
    RBEllipse: TRadioButton;
    RBRoundRect: TRadioButton;
    RBRectangle: TRadioButton;
    PageMainPanel: TTabSheet;
    PageTextures: TTabSheet;
    PageChilds: TTabSheet;
    procedure BCancelClick(Sender: TObject);
    procedure FSE1Change(Sender: TObject);
  private
    FrameEditUIBodyShape: TFrameEditUIBodyShape;
    FModified: boolean;
    FInitializing: boolean;
    FTargetUIPanel: TUIPanel;
    procedure ProcessSomethingChange(Sender: TObject);
    function CBToRoundRectOptions: TRoundRectangleOptions;
    procedure RoundRectOptionsToCB(aOptions: TRoundRectangleOptions);
    procedure SetTargetUIPanel(AValue: TUIPanel);
  private // textures
    procedure ProcessTextureListOnModified(Sender: TObject);
    procedure ProcessAskToDeleteTextureEvent(aTextureItem: PTextureItem; var aCanDelete: boolean);
    procedure ProcessOnTextureChangedEvent(aTextureItem: PTextureItem);
    function Textures: TTextureList;
  private
    procedure DoClearAll;
  public
    FrameTexturesList: TFrameTextureList;
    constructor Create(aOwner: TComponent); override;
    procedure OnShow;

    // initialized by ScreenUIPanelEditor
    property TargetUIPanel: TUIPanel read FTargetUIPanel write SetTargetUIPanel;
    property Modified: boolean read FModified write FModified;
  end;

implementation

uses form_main, u_screen_uipaneleditor, u_resourcestring;

{$R *.lfm}

{ TFrameToolUIPanelEditor }

procedure TFrameToolUIPanelEditor.BCancelClick(Sender: TObject);
begin
  if FModified then
    if QuestionDlg('', sIfYouLeaveChangeWillBeLost, mtWarning,
                   [mrOk, sLeaveWithoutSaving, mrCancel, sCancel], 0) = mrCancel then exit;
  DoClearAll;
  FormMain.ShowPageLevelBank;
end;

procedure TFrameToolUIPanelEditor.FSE1Change(Sender: TObject);
var pan: TUIPanel;
begin
  Label2.Enabled := RBRoundRect.Checked;
  FSE1.Enabled := RBRoundRect.Checked;
  Label3.Enabled := RBRoundRect.Checked;
  FSE2.Enabled := RBRoundRect.Checked;
  CBTopLeft.Enabled := RBRoundRect.Checked;
  CBTopRight.Enabled := RBRoundRect.Checked;
  CBBottomLeft.Enabled := RBRoundRect.Checked;
  CBBottomRight.Enabled := RBRoundRect.Checked;

  if FInitializing then exit;

  pan := ScreenUIPanelEditor.UIPanel;

  if {(Sender = RBRectangle) and} RBRectangle.Checked then
    pan.BodyShape.SetShapeRectangle(pan.Width, pan.Height, FrameEditUIBodyShape.BorderWidth);

  if {((Sender = RBRoundRect) or (Sender = FSE1) or (Sender = FSE2) or
      (Sender) and }RBRoundRect.Checked then
      pan.BodyShape.SetShapeRoundRect(pan.Width, pan.Height, FSE1.Value, FSE2.Value, FrameEditUIBodyShape.BorderWidth, CBToRoundRectOptions);

  if {(Sender = RBEllipse) and} (RBEllipse.Checked) then
    pan.BodyShape.SetShapeEllipse(pan.Width, pan.Height, FrameEditUIBodyShape.BorderWidth);

end;

procedure TFrameToolUIPanelEditor.ProcessSomethingChange(Sender: TObject);
begin
  if FInitializing then exit;
  FModified := True;
end;

function TFrameToolUIPanelEditor.CBToRoundRectOptions: TRoundRectangleOptions;
begin
  Result := [];
  case CBTopLeft.ItemIndex of
    0: Include(Result, rrTopLeftSquare);
    1:;
    2: Include(Result, rrTopLeftBevel);
  end;

  case CBTopRight.ItemIndex of
    0: Include(Result, rrTopRightSquare);
    1:;
    2: Include(Result, rrTopRightBevel);
  end;

  case CBBottomRight.ItemIndex of
    0: Include(Result, rrBottomRightSquare);
    1:;
    2: Include(Result, rrBottomRightBevel);
  end;

  case CBBottomLeft.ItemIndex of
    0: Include(Result, rrBottomLeftSquare);
    1:;
    2: Include(Result, rrBottomLeftBevel);
  end;
end;

procedure TFrameToolUIPanelEditor.RoundRectOptionsToCB(aOptions: TRoundRectangleOptions);
begin
  if rrTopLeftSquare in aOptions then CBTopLeft.ItemIndex := 0
  else if rrTopLeftBevel in aOptions then CBTopLeft.ItemIndex := 2;

  if rrTopRightSquare in aOptions then CBTopRight.ItemIndex := 0
  else if rrTopRightBevel in aOptions then CBTopRight.ItemIndex := 2;

  if rrBottomLeftSquare in aOptions then CBBottomLeft.ItemIndex := 0
  else if rrBottomLeftBevel in aOptions then CBBottomLeft.ItemIndex := 2;

  if rrBottomRightSquare in aOptions then CBBottomRight.ItemIndex := 0
  else if rrBottomRightBevel in aOptions then CBBottomRight.ItemIndex := 2;
end;

procedure TFrameToolUIPanelEditor.SetTargetUIPanel(AValue: TUIPanel);
begin
  FTargetUIPanel := AValue;
  FrameEditUIBodyShape.TargetUIPanel := AValue;
end;

procedure TFrameToolUIPanelEditor.ProcessTextureListOnModified(Sender: TObject);
begin
  FModified := True;
  //Textures.FillComboBox(CBTextures);
end;

procedure TFrameToolUIPanelEditor.ProcessAskToDeleteTextureEvent(
  aTextureItem: PTextureItem; var aCanDelete: boolean);
begin
{  if Length(Surfaces.GetItemsThatUseThisTexture(aTextureItem)) > 0 then begin
    aCanDelete := False;
    ShowMessage('This texture is used by a surface, you can not delete it');
  end;  }
end;

procedure TFrameToolUIPanelEditor.ProcessOnTextureChangedEvent(aTextureItem: PTextureItem);
begin
{  // recreate the surfaces that use this texture (if any)
  surf := Surfaces.GetItemsThatUseThisTexture(aTextureItem);
  if Length(surf) <> 0 then
    for i:=0 to High(surf) do
      surf[i]^.RecreateSurfaceBecauseTextureChanged; }
end;

function TFrameToolUIPanelEditor.Textures: TTextureList;
begin
  Result := ScreenUIPanelEditor.Textures;
end;

procedure TFrameToolUIPanelEditor.DoClearAll;
begin

end;

constructor TFrameToolUIPanelEditor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FrameEditUIBodyShape := TFrameEditUIBodyShape.Create(Self);
  FrameEditUIBodyShape.Parent := Panel2;
  FrameEditUIBodyShape.Align := alClient;
  FrameEditUIBodyShape.OnChange := @ProcessSomethingChange;

  FrameTexturesList := TFrameTextureList.Create(Self);
  FrameTexturesList.Parent := PageTextures;
  FrameTexturesList.Align := alClient;
  FrameTexturesList.OnModified := @ProcessTextureListOnModified;
  FrameTexturesList.OnAskToDeleteTexture := @ProcessAskToDeleteTextureEvent;
  FrameTexturesList.OnTextureChanged := @ProcessOnTextureChangedEvent;
  FrameTexturesList.OnGetTextureList := @Textures;
end;

procedure TFrameToolUIPanelEditor.OnShow;
begin
  FModified := False;

  FrameTexturesList.FillListBox;
end;

end.

