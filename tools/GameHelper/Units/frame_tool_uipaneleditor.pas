unit frame_tool_uipaneleditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls, Dialogs,
  Buttons,
  OGLCScene, BGRABitmap, BGRABitmapTypes,
  frame_edit_uibodyshape;

type

  { TFrameToolUIPanelEditor }

  TFrameToolUIPanelEditor = class(TFrame)
    BCancel: TSpeedButton;
    BSave: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label24: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel8: TPanel;
    RadioGroup1: TRadioGroup;
    procedure BCancelClick(Sender: TObject);
  private
    FrameEditUIBodyShape: TFrameEditUIBodyShape;
    FModified: boolean;
    FInitializing: boolean;
    FTargetUIPanel: TUIPanel;
    procedure ProcessSomethingChange(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    procedure OnShow;

    // initialized by
    property TargetUIPanel: TUIPanel read FTargetUIPanel write FTargetUIPanel;
  end;

implementation

uses form_main;

{$R *.lfm}

{ TFrameToolUIPanelEditor }

procedure TFrameToolUIPanelEditor.BCancelClick(Sender: TObject);
begin
  if FModified then exit;
  FormMain.ShowPageLevelBank;
end;

procedure TFrameToolUIPanelEditor.ProcessSomethingChange(Sender: TObject);
begin
  if FInitializing then exit;
  FModified := True;
end;

constructor TFrameToolUIPanelEditor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FrameEditUIBodyShape := TFrameEditUIBodyShape.Create(Self);
  FrameEditUIBodyShape.Parent := Panel2;
  FrameEditUIBodyShape.Align := alClient;
  FrameEditUIBodyShape.OnChange := @ProcessSomethingChange;
end;

procedure TFrameToolUIPanelEditor.OnShow;
begin
  FModified := False;
end;

end.

