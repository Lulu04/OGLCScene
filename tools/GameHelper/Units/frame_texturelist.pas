unit frame_texturelist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin,
  Dialogs, u_texture_list, u_surface_list, u_screen_spritebuilder;

type

TEventOnGetSpriteBuilderSurfaceList = function(): TSpriteBuilderSurfaceList of object;
TEventOnGetTextureList = function (): TTextureList of object;

  { TFrameTextureList }

  TFrameTextureList = class(TFrame)
    BAddTexture: TSpeedButton;
    BChooseImageFile: TSpeedButton;
    BDeleteTexture: TSpeedButton;
    BTextureRedo: TSpeedButton;
    BTextureUndo: TSpeedButton;
    BUpdateTexture: TSpeedButton;
    BUpdateTextureListbox: TSpeedButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label21: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LBTextureNames: TListBox;
    OD1: TOpenDialog;
    Panel1: TPanel;
    Panel7: TPanel;
    SE10: TSpinEdit;
    SE11: TSpinEdit;
    SE12: TSpinEdit;
    SE9: TSpinEdit;
    procedure BChooseImageFileClick(Sender: TObject);
    procedure LBTextureNamesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBTextureNamesSelectionChange(Sender: TObject; User: boolean);
  private
    FOnGetSpriteBuilderSurfaceList: TEventOnGetSpriteBuilderSurfaceList;
    FOnGetTextureList: TEventOnGetTextureList;
    FInitializingWidget: boolean;
    FOnModified: TNotifyEvent;
    function Textures: TTextureList;
    function SpriteBuilderSurfaces: TSurfaceList;

    function LBToTextureName: string;
    function CheckTextureWidgets: boolean;
    procedure DoAddTexture;
    procedure DoDeleteTexture;
    procedure DoUpdateTexture;
  public
    procedure Clear;
    procedure UpdateTextureWidgetState;
    procedure FillListBox;

    property OnGetTextureList: TEventOnGetTextureList read FOnGetTextureList write FOnGetTextureList;
    property OnGetSpriteBuilderSurfaceList: TEventOnGetSpriteBuilderSurfaceList read FOnGetSpriteBuilderSurfaceList write FOnGetSpriteBuilderSurfaceList;

    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

implementation
uses Types, u_utils, u_common;

{$R *.lfm}

{ TFrameTextureList }

procedure TFrameTextureList.BChooseImageFileClick(Sender: TObject);
begin
  if sender = BUpdateTextureListbox then
    Textures.FillListBox(LBTextureNames);

  if Sender = BChooseImageFile then begin
    if not OD1.Execute then exit;
    Label2.Caption := OD1.FileName;
    Edit1.Text := 'tex'+ChangeFileExt(ExtractFileName(OD1.FileName), '');
    if ExtractFileExt(OD1.FileName) = '.svg' then begin
      SE9.Value := -1;
      SE10.Value := -1;
    end;
    UpdateTextureWidgetState;
  end;

  if Sender = BUpdateTexture then
    DoUpdateTexture;

  if Sender = BAddTexture then begin
    DoAddTexture;
    UpdateTextureWidgetState;
  end;

  if Sender = BDeleteTexture then begin
    DoDeleteTexture;
    UpdateTextureWidgetState;
  end;

  if Sender = BTextureUndo then begin
    Textures.UndoRedoManager.Undo;
    UpdateTextureWidgetState;
  end;

  if Sender = BTextureRedo then begin
    Textures.UndoRedoManager.Redo;
    UpdateTextureWidgetState;
  end;
end;

procedure TFrameTextureList.LBTextureNamesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var lb: TListBox;
begin
  lb := Sender as TListBox;
  lb.ItemIndex := lb.GetIndexAtXY(X, Y);
end;

procedure TFrameTextureList.LBTextureNamesSelectionChange(Sender: TObject;
  User: boolean);
var i: integer;
  p: PTextureItem;
begin
  if FInitializingWidget then exit;
  FInitializingWidget := True;

  i := LBTextureNames.ItemIndex;
  if i = -1 then begin
    Label2.Caption := '';
    Edit1.Text := '';
    SE9.Value := 0;
    SE10.Value := 0;
    CheckBox1.Checked := False;
    SE11.Value := 0;
    SE12.Value := 0;
  end else begin
    p := Textures.GetItemByName(LBTextureNames.Items.Strings[i]); // Textures.Mutable[i];
    Label2.Caption := p^.filename;
    Edit1.Text := p^.name;
    SE9.Value := p^.width;
    SE10.Value := p^.height;
    CheckBox1.Checked := p^.isMultiFrame;
    SE11.Value := p^.frameWidth;
    SE12.Value := p^.frameHeight;
  end;
  FInitializingWidget := False;

  UpdateTextureWidgetState;
end;

function TFrameTextureList.Textures: TTextureList;
begin
  Result := FOnGetTextureList();
end;

function TFrameTextureList.SpriteBuilderSurfaces: TSurfaceList;
begin
  if FOnGetSpriteBuilderSurfaceList = NIL then Result := NIL
    else Result := FOnGetSpriteBuilderSurfaceList();
end;

procedure TFrameTextureList.UpdateTextureWidgetState;
var siz: TSize;
begin
  if not FileExists(Label2.Caption) then begin
    BUpdateTexture.Enabled := False;
    BAddTexture.Enabled := False;
  end
  else
  if Textures.GetItemByFilename(Label2.Caption) <> NIL then begin
    BUpdateTexture.Enabled := True;
    BAddTexture.Enabled := False;
  end
  else
  begin
    BUpdateTexture.Enabled := False;
    BAddTexture.Enabled := True;
  end;

  // width and height are read only for non svg file
  SE9.Enabled := ExtractFileExt(Label2.Caption) = '.svg';
  SE10.Enabled := SE9.Enabled;
  if not SE9.Enabled and FileExists(Label2.Caption) then begin
    siz := GetImageSize(Label2.Caption);
    SE9.Value := siz.cx;
    SE10.Value := siz.cy;
  end;

  Label7.Enabled := CheckBox1.Checked;
  Label8.Enabled := CheckBox1.Checked;
  SE11.Enabled := CheckBox1.Checked;
  SE12.Enabled := CheckBox1.Checked;

  BDeleteTexture.Enabled := LBTextureNames.ItemIndex <> -1;
  BTextureUndo.Enabled := Textures.UndoRedoManager.CanUndo;
  BTextureRedo.Enabled := Textures.UndoRedoManager.CanRedo;
end;

procedure TFrameTextureList.FillListBox;
begin
  Textures.FillListBox(LBTextureNames);
  Label2.Caption := '';
  SE9.Value := 0;
  SE10.Value := 0;
  CheckBox1.Checked := False;
  SE11.Value := 0;
  SE12.Value := 0;
end;

function TFrameTextureList.LBToTextureName: string;
var i: Integer;
begin
  Result := '';
  i := LBTextureNames.ItemIndex;
  if i = -1 then exit;
  Result := LBTextureNames.Items.Strings[i];
end;

function TFrameTextureList.CheckTextureWidgets: boolean;
begin
  Result := FileExists(Label2.Caption) and
            (Trim(Edit1.Text) <> '') and
            not Textures.NameAlreadyExists(Trim(Edit1.Text)) and
            (SE9.Value <> 0) and
            (SE10.Value <> 0);
  if CheckBox1.Checked then
    Result := Result and (SE11.Value <> 0) and (SE12.Value <> 0);
end;

procedure TFrameTextureList.DoAddTexture;
var texName: string;
//i:integer;
begin
  if not CheckTextureWidgets then exit;

  texName := Trim(Edit1.Text);

{FScene.LogDebug('TFrameToolsSpriteBuilder.DoAddTexture: BEFORE Textures.Size='+Textures.Size.tostring);
for i:=0 to Textures.Size-1 do
  FScene.LogDebug('  Textures index '+i.tostring+' name='+Textures.mutable[i]^.name); }

FScene.LogDebug('  ADDING '+Label2.Caption+'  '+texName);
  Textures.Add(Label2.Caption, texName, SE9.Value, SE10.Value,
               CheckBox1.Checked, SE11.Value, SE12.Value);

{FScene.LogDebug('TFrameToolsSpriteBuilder.DoAddTexture: AFTER Textures.Size='+Textures.Size.tostring);
for i:=0 to Textures.Size-1 do
  FScene.LogDebug('  Textures index '+i.tostring+' name='+Textures.mutable[i]^.name);  }

  FInitializingWidget := True;
  LBTextureNames.ItemIndex := LBTextureNames.Items.Add(texName);
  FInitializingWidget := False;
  UpdateTextureWidgetState;

  Textures.UndoRedoManager.AddActionAddTexture(texName);
  OnModified(Self);
end;

procedure TFrameTextureList.DoDeleteTexture;
var textureName: string;
begin
  textureName := LBToTextureName;
  if textureName = '' then exit;

  // check if the texture is used by a surface
  if SpriteBuilderSurfaces.TextureNameisUsedByASurface(textureName) then begin
    ShowMessage('This texture is used by a surface, you can not delete it');
    exit;
  end;
  Textures.UndoRedoManager.AddActionDeleteTexture(textureName);
  Textures.DeleteByName(textureName);
  LBTextureNames.Items.Delete(LBTextureNames.ItemIndex);

  OnModified(Self);

  FInitializingWidget := True;
  Label2.Caption := '';
  Edit1.Text := '';
  SE9.Value := 0;
  SE10.Value := 0;
  CheckBox1.Checked := False;
  SE11.Value := 0;
  SE12.Value := 0;
  FInitializingWidget := False;
end;

procedure TFrameTextureList.DoUpdateTexture;
var i, j: Integer;
  item: PTextureItem;
  surf: ArrayOfPSurfaceDescriptor;
begin
  i := LBTextureNames.ItemIndex;
  if i = -1 then exit;

  item := Textures.GetItemByName(LBTextureNames.Items.Strings[i]);
  if item = NIL then exit;

  Textures.UndoRedoManager.AddActionModifyTexture(item, Label2.Caption, Edit1.Text, SE9.Value, SE10.Value,
                  CheckBox1.Checked, SE11.Value, SE12.Value);
  Textures.Update(item, Label2.Caption, Edit1.Text, SE9.Value, SE10.Value,
                  CheckBox1.Checked, SE11.Value, SE12.Value);

  // recreate the surfaces that use this texture (if any)
  if SpriteBuilderSurfaces <> NIL then begin
    surf := SpriteBuilderSurfaces.GetItemsThatUseThisTexture(item);
    if Length(surf) <> 0 then
      for j:=0 to High(surf)do
        surf[j]^.RecreateSurfaceBecauseTextureChanged;
  end;

  Label2.Caption := '';
  UpdateTextureWidgetState;
  OnModified(Self);
end;

procedure TFrameTextureList.Clear;
begin
  Checkbox1.Checked := False;
  LBTextureNames.Clear;
  Label2.Caption := '';
  Edit1.Text := '';
end;

end.

