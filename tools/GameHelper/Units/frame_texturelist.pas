unit frame_texturelist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin,
  Dialogs, BGRABitmap, u_texture_list;

type

TEventOnAskToDeleteTexture = procedure(aTextureItem: PTextureItem; var aCanDelete: boolean) of object;
TEventOnTextureChanged = procedure(aTextureItem: PTextureItem) of object;
TEventOnGetTextureList = function (): TTextureList of object;

  { TFrameTextureList }

  TFrameTextureList = class(TFrame)
    BAddTexture: TSpeedButton;
    BChooseImageFile: TSpeedButton;
    BDeleteTexture: TSpeedButton;
    BTextureRedo: TSpeedButton;
    BHelp: TSpeedButton;
    BTextureUndo: TSpeedButton;
    BUpdateTexture: TSpeedButton;
    BUpdateTextureListbox: TSpeedButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label21: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LBTextureNames: TListBox;
    OD1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel7: TPanel;
    SE10: TSpinEdit;
    SE11: TSpinEdit;
    SE12: TSpinEdit;
    SE9: TSpinEdit;
    SE13: TSpinEdit;
    procedure BChooseImageFileClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure LBTextureNamesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBTextureNamesSelectionChange(Sender: TObject; User: boolean);
    procedure SE11Change(Sender: TObject);
  private
    FOnAskToDeleteTexture: TEventOnAskToDeleteTexture;
    FOnGetTextureList: TEventOnGetTextureList;
    FInitializingWidget: boolean;
    FOnModified: TNotifyEvent;
    FOnTextureChanged: TEventOnTextureChanged;
    function Textures: TTextureList;

    function LBToTextureName: string;
    function CheckTextureWidgets: boolean;
    procedure DoAddTexture;
    procedure DoDeleteTexture;
    procedure DoUpdateTexture;
    procedure CheckFrameWidthAndHeight;
  public
    procedure Clear;
    procedure UpdateTextureWidgetState;
    procedure FillListBox;

    property OnGetTextureList: TEventOnGetTextureList read FOnGetTextureList write FOnGetTextureList;

    property OnAskToDeleteTexture: TEventOnAskToDeleteTexture read FOnAskToDeleteTexture write FOnAskToDeleteTexture;
    property OnTextureChanged: TEventOnTextureChanged read FOnTextureChanged write FOnTextureChanged;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

implementation
uses Types, u_utils, form_showhelp, OGLCScene;

{$R *.lfm}

{ TFrameTextureList }

procedure TFrameTextureList.BChooseImageFileClick(Sender: TObject);
var s: TSize;
begin
  if sender = BUpdateTextureListbox then
    Textures.FillListBox(LBTextureNames);

  if Sender = BChooseImageFile then begin
    if not OD1.Execute then exit;
    Label2.Caption := OD1.FileName;
    Label2.Hint := OD1.FileName;
    Edit1.Text := 'tex'+ChangeFileExt(ExtractFileName(OD1.FileName), '');
    if LowerCase(ExtractFileExt(OD1.FileName)) = '.svg' then begin
      s := GetSVGImageSize(OD1.FileName);
      SE9.Value := s.cx;
      SE10.Value := s.cy;
    end else begin
      s := GetImageSize(Label2.Caption);
      SE9.Value := s.cx;
      SE10.Value := s.cy;
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

procedure TFrameTextureList.BHelpClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('Texture are the first thing you have to add.'#10#10+
  'ADD A TEXTURE:'#10+
  ' - click on the button with the landscape and the small plus.'#10+
  ' - choose an image file (svg, png, etc...) then click open.'#10+
  ' - if you''ve choosen an SVG file you have to enter the width or the height of the texture.'#10+
  '   enter the width and keep height to -1 if the texture width is greater than its height.'#10+
  '   enter the height and keep width to -1 if the texture height is greater than its width.'#10+
  '   -1 means that GameHelper compute the value to keep the aspect ratio.'#10+
  '   this is to keep precision for long and thin textures.'#10+
  '   Off course, you can enter both width and height.'#10+
  ' - if the texture is made of multiple frames, check ''Have multiple frames'' and enter the frame size.'#10+
  ' - click on ''+'' button to add the texture to the list.'#10#10+
  'In Level Editor, there is one single texture list common to all levels.'#10+
  'In Sprite Builder, each sprite have its own texture list.');
end;

procedure TFrameTextureList.CheckBox1Change(Sender: TObject);
begin
  Panel2.Enabled := CheckBox1.Checked;
  CheckFrameWidthAndHeight;
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
  texItem: PTextureItem;
begin
  if FInitializingWidget then exit;
  FInitializingWidget := True;

  i := LBTextureNames.ItemIndex;
  if i = -1 then begin
    Label2.Caption := '';
    Label2.Hint := '';
    Edit1.Text := '';
    SE9.Value := 0;
    SE10.Value := 0;
    CheckBox1.Checked := False;
    SE11.Value := 0;
    SE12.Value := 0;
  end else begin
    texItem := Textures.GetItemByName(LBTextureNames.Items.Strings[i]);
    Label2.Caption := texItem^.filename;
    Label2.Hint := texItem^.filename;
    Edit1.Text := texItem^.name;
    SE9.Value := texItem^.width;
    SE10.Value := texItem^.height;
    CheckBox1.Checked := texItem^.isMultiFrame;
    SE11.Value := texItem^.frameWidth;
    SE12.Value := texItem^.frameHeight;
    SE13.Value := texItem^.framecount;
  end;
  FInitializingWidget := False;

  UpdateTextureWidgetState;
end;

procedure TFrameTextureList.SE11Change(Sender: TObject);
begin
  CheckFrameWidthAndHeight;
end;

function TFrameTextureList.Textures: TTextureList;
begin
  Result := FOnGetTextureList();
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

  Panel2.Enabled := CheckBox1.Checked;

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
var ima: TBGRABitmap;
begin
  Result := FileExists(Label2.Caption) and
            (Trim(Edit1.Text) <> '') and
            not Textures.NameAlreadyExists(Trim(Edit1.Text)) and
            (SE9.Value <> 0) and
            (SE10.Value <> 0);

  if Result and CheckBox1.Checked then begin
    if (SE11.Value = 0) or (SE12.Value = 0) then exit(False);
    if LowerCase(ExtractFileExt(Label2.Caption)) = '.svg' then
      ima := LoadBitmapFromSVG(Label2.Caption, SE9.Value, SE10.Value)
    else
      ima := TBGRABitmap.Create(Label2.Caption);

    Result := (ima.Width mod SE11.Value = 0) and (ima.Height mod SE12.Value = 0) and
              (SE13.Value <= ima.Width div SE11.Value + ima.Height div SE12.Value);
    ima.Free;
  end;
end;

procedure TFrameTextureList.DoAddTexture;
var texName: string;
begin
  if not CheckTextureWidgets then exit;

  texName := Trim(Edit1.Text);
  Textures.Add(Label2.Caption, texName, SE9.Value, SE10.Value,
               CheckBox1.Checked, SE11.Value, SE12.Value, SE13.Value);

  FInitializingWidget := True;
  LBTextureNames.ItemIndex := LBTextureNames.Items.Add(texName);
  FInitializingWidget := False;
  UpdateTextureWidgetState;

  Textures.UndoRedoManager.AddActionAddTexture(texName);
  OnModified(Self);
end;

procedure TFrameTextureList.DoDeleteTexture;
var textureName: string;
  textureItem: PTextureItem;
  canDelete: boolean;
begin
  textureName := LBToTextureName;
  if textureName = '' then exit;
  textureItem := Textures.GetItemByName(textureName);
  if textureItem = NIL then exit;

  // check if we can delete the texture
  canDelete := True;
  FOnAskToDeleteTexture(textureItem, canDelete);
  if not canDelete then exit;

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
var i: Integer;
  item: PTextureItem;
begin
  i := LBTextureNames.ItemIndex;
  if i = -1 then exit;

  item := Textures.GetItemByName(LBTextureNames.Items.Strings[i]);
  if item = NIL then exit;

  Textures.UndoRedoManager.AddActionModifyTexture(item, Label2.Caption, Edit1.Text, SE9.Value, SE10.Value,
                  CheckBox1.Checked, SE11.Value, SE12.Value, SE13.Value);
  Textures.Update(item, Label2.Caption, Edit1.Text, SE9.Value, SE10.Value,
                  CheckBox1.Checked, SE11.Value, SE12.Value, SE13.Value);

  // notify the texture changed
  FOnTextureChanged(item);

  Label2.Caption := '';
  UpdateTextureWidgetState;
  OnModified(Self);
end;

procedure TFrameTextureList.CheckFrameWidthAndHeight;
begin
  Label3.Visible := CheckBox1.Checked;
  if Label3.Visible and (SE11.Value > 0) then Label3.Visible := SE9.Value mod SE11.Value <> 0;

  Label4.Visible := CheckBox1.Checked;
  if Label4.Visible and (SE12.Value > 0) then Label4.Visible := SE10.Value mod SE12.Value <> 0;
end;

procedure TFrameTextureList.Clear;
begin
  Checkbox1.Checked := False;
  LBTextureNames.Clear;
  Label2.Caption := '';
  Edit1.Text := '';
end;

end.

