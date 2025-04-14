unit form_tool_spritebuilder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Spin, BGRABitmapTypes,
  OGLCScene,
  u_surface_list, u_texture_list, Types,
  u_screen_spritebuilder;

type

  { TFormTools }

  TFormTools = class(TForm)
    CheckBox1: TCheckBox;
    CBChildType: TComboBox;
    CBTextures: TComboBox;
    CBChildParent: TComboBox;
    Edit1: TEdit;
    Edit5: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LBTextureNames: TListBox;
    OD1: TOpenDialog;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PC1: TPageControl;
    PageTextures: TTabSheet;
    PageChilds: TTabSheet;
    SE10: TSpinEdit;
    SE11: TSpinEdit;
    SE12: TSpinEdit;
    SE2: TSpinEdit;
    SE3: TFloatSpinEdit;
    SE4: TFloatSpinEdit;
    SE5: TFloatSpinEdit;
    SE6: TFloatSpinEdit;
    SE7: TFloatSpinEdit;
    BNewChild: TSpeedButton;
    BAddTexture: TSpeedButton;
    BChooseImageFile: TSpeedButton;
    BUpdateTexture: TSpeedButton;
    SE1: TSpinEdit;
    SE8: TSpinEdit;
    SE9: TSpinEdit;
    BDeleteTexture: TSpeedButton;
    BAddToSpriteBank: TSpeedButton;
    procedure BChooseImageFileClick(Sender: TObject);
    procedure CBChildParentDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CBChildTypeSelect(Sender: TObject);
    procedure CBTexturesSelect(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBChildsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBTextureNamesSelectionChange(Sender: TObject; User: boolean);
  private
    FInitializingWidget: boolean;
    procedure UpdateTextureWidgetState;
  private // childs
    procedure ClassTypeToCB(aClass: classOfSimpleSurfaceWithEffect);
    function CBToClassType: classOfSimpleSurfaceWithEffect;
    procedure TexturenameToCB(aName: string);
    function CBToTextureName: string;
    procedure ParentIDToCB(aID: integer);
    function CBToParentID: integer;
    function CheckChildWidgets: boolean;
    procedure DoAddNewChild;
    procedure DoUpdateChild;
  private // textures
    function LBToTextureName: string;
    function CheckTextureWidgets: boolean;
    procedure DoAddTexture;
    procedure DoDeleteTexture;
  private
    FWorkingChild: PSurfaceDescriptor;
    procedure UpdateValuesToWorkingSurface;
    function Textures: TTextureList;
    function Surfaces: TSpriteBuilderSurfaceList;
  public
    procedure FillListBoxTextureNames;

    procedure ShowSelectionData(aSelected: ArrayOfPSurfaceDescriptor);
  end;

var
  FormTools: TFormTools;

implementation

uses form_main, u_project, u_common, u_utils, LCLType;

{$R *.lfm}

{ TFormTools }

procedure TFormTools.UpdateTextureWidgetState;
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
end;

procedure TFormTools.ClassTypeToCB(aClass: classOfSimpleSurfaceWithEffect);
var i: integer;
begin
  i := -1;
  if aClass = TSpriteContainer then i := 0
  else
  if aClass = TSprite then i := 1
  else
  if aClass = TDeformationGrid then i := 2
  else
  raise exception.create('forgot to implement!');
  CBChildType.ItemIndex := i;
end;

function TFormTools.CBToClassType: classOfSimpleSurfaceWithEffect;
begin
  case CBChildType.ItemIndex of
    0: Result := TSpriteContainer;
    1: Result := TSprite;
    2: Result := TDeformationGrid;
    else raise exception.create('forgot to implement!');
  end;
end;

procedure TFormTools.TexturenameToCB(aName: string);
begin
  CBTextures.ItemIndex := Textures.GetItemIndexByName(aName);
end;

function TFormTools.CBToTextureName: string;
begin
  if CBTextures.ItemIndex = -1 then Result := ''
    else Result := CBTextures.Items.Strings[CBTextures.ItemIndex];
end;

procedure TFormTools.ParentIDToCB(aID: integer);
begin
  if aID = -1 then CBChildParent.ItemIndex := 0
    else CBChildParent.ItemIndex := CBChildParent.Items.IndexOf(aID.ToString);
end;

function TFormTools.CBToParentID: integer;
begin
  if CBChildParent.ItemIndex = 0 then Result := -1
    else Result := CBChildParent.Items.Strings[CBChildParent.ItemIndex].ToInteger;
end;

function TFormTools.CheckChildWidgets: boolean;
begin
  Result := (CBChildType.ItemIndex <> -1) and
            (CBTextures.ItemIndex <> -1) and
            (Trim(Edit5.Text) <> '') and
            (CBChildParent.ItemIndex <> -1);
end;

procedure TFormTools.DoAddNewChild;
begin
  if not CheckChildWidgets then exit;
  if Surfaces.NameExists(Trim(Edit5.Text)) then begin
    ShowMessage('Duplicate name "'+Trim(Edit5.Text)+'"');
    exit;
  end;

  FWorkingChild := Surfaces.AddEmpty;
  DoUpdateChild;

{  FWorkingChild^.classtype := CBToClassType;
  FWorkingChild^.textureName := CBToTextureName;
  FWorkingChild^.name := Trim(Edit5.Text);
  FWorkingChild^.parentID := CBToParentID;

  FWorkingChild^.CreateSurface;
  FWorkingChild^.SetChildDependency;

  UpdateValuesToWorkingSurface;  }

  CBChildParent.Items.Add(FWorkingChild^.ID.ToString);
  FWorkingChild := NIL;
  Project.SetModified;
end;

procedure TFormTools.DoUpdateChild;
var recreateSurface: Boolean;
begin
  if FWorkingChild = NIL then exit;

  recreateSurface := False;

  if not (FWorkingChild^.surface is CBToClassType) then
    recreateSurface := True;

  if FWorkingChild^.textureName <> CBToTextureName then
    recreateSurface := True;

    if FWorkingChild^.parentID <> CBToParentID then
      recreateSurface := True;

  if recreateSurface then begin
    // create a new surface
    FWorkingChild^.KillSurface;
    FWorkingChild^.classtype := CBToClassType;
    FWorkingChild^.textureName := CBToTextureName;
    FWorkingChild^.CreateSurface;
    // set child dependency
    FWorkingChild^.name := Trim(Edit5.Text);
    FWorkingChild^.parentID := CBToParentID;
    FWorkingChild^.zOrder := SE8.Value;
    FWorkingChild^.SetChildDependency;
  end;

  UpdateValuesToWorkingSurface;
end;

function TFormTools.LBToTextureName: string;
var i: Integer;
begin
  Result := '';
  i := LBTextureNames.ItemIndex;
  if i = -1 then exit;
  Result := LBTextureNames.Items.Strings[i];
end;

function TFormTools.CheckTextureWidgets: boolean;
begin
  Result := FileExists(Label2.Caption) and
            (Trim(Edit1.Text) <> '') and
            (SE9.Value <> 0) and
            (SE10.Value <> 0);
  if CheckBox1.Checked then
    Result := Result and (SE11.Value <> 0) and (SE12.Value <> 0);
end;

procedure TFormTools.DoAddTexture;
var texName: string;
begin
  if not CheckTextureWidgets then exit;

  texName := Trim(Edit1.Text);
  Textures.Add(Label2.Caption, texName, SE9.Value, SE10.Value,
               CheckBox1.Checked, SE11.Value, SE12.Value);
  LBTextureNames.ItemIndex := LBTextureNames.Items.Add(texName);
  UpdateTextureWidgetState;

  Textures.FillComboBox(CBTextures); // refresh the combobox in childs page
  Project.SetModified;
end;

procedure TFormTools.DoDeleteTexture;
var textureName: string;
begin
  textureName := LBToTextureName;
  if textureName = '' then exit;

  // check if the texture is used by a surface
  if Surfaces.TextureNameisUsedByASurface(textureName) then begin
    ShowMessage('This texture is used by a surface, you can not delete it');
    exit;
  end;

  Textures.DeleteByName(textureName);
  LBTextureNames.Items.Delete(LBTextureNames.ItemIndex);
end;

procedure TFormTools.UpdateValuesToWorkingSurface;
begin
  if FWorkingChild = NIL then exit;

  with FWorkingChild^.surface do begin
    X.Value := SE1.Value;
    Y.Value := SE2.Value;
    FWorkingChild^.Pivot := SurfaceToScene(PointF(SE3.Value*Width, SE4.Value*Height));
    Scale.x.Value := SE5.Value;
    Scale.y.Value := SE6.Value;
    Angle.Value := SE7.Value;
    SetZOrder(SE8.Value);
  end;
end;

function TFormTools.Textures: TTextureList;
begin
  Result := ScreenSpriteBuilder.Textures;
end;

function TFormTools.Surfaces: TSpriteBuilderSurfaceList;
begin
  Result := ScreenSpriteBuilder.Surfaces;
end;

procedure TFormTools.FillListBoxTextureNames;
begin
  Textures.FillListBox(LBTextureNames);
  Label2.Caption := '';
  SE9.Value := 0;
  SE10.Value := 0;
  CheckBox1.Checked := False;
  SE11.Value := 0;
  SE12.Value := 0;
end;

procedure TFormTools.ShowSelectionData(aSelected: ArrayOfPSurfaceDescriptor);
begin
  FInitializingWidget := True;
  FWorkingChild := NIL;

  if Length(aSelected) = 1 then begin
    // 1 selected -> we can edit its parameters
    Label20.Visible := False;
    FWorkingChild := aSelected[0];
    with FWorkingChild^ do begin
      ClassTypeToCB(classtype);
      TexturenameToCB(textureName);
      Edit5.Text := name;
      ParentIDToCB(parentID);
      SE1.Value := surface.X.Value;
      SE2.Value := surface.Y.Value;
      SE3.Value := surface.Pivot.x;
      SE4.Value := surface.Pivot.y;
      SE5.Value := surface.Scale.x.Value;
      SE6.Value := surface.Scale.y.Value;
      SE7.Value := surface.Angle.Value;
    end;
    CBChildType.Enabled := True;
    CBTextures.Enabled := True;
    Edit5.Enabled := True;
    CBChildParent.Enabled := True;
    Panel4.Visible := True;
    BNewChild.Enabled := False;
  end
  else
  if Length(aSelected) > 1 then begin
    // several selected -> we can not edit the parameters
    Label20.Visible := True;  // show 'multiple'
    CBChildType.Enabled := False;
    CBTextures.Enabled := False;
    Edit5.Enabled := False;
    CBChildParent.Enabled := False;
    CBChildType.ItemIndex := -1;
    CBTextures.ItemIndex := -1;
    Edit5.Text := '';
    CBChildParent.ItemIndex := -1;
    Panel4.Visible := False;
    BNewChild.Enabled := False;
  end
  else begin
    // 0 selected -> reset parameters, enable them and activate the button 'ADD'
    Label20.Visible := False;
    CBChildType.ItemIndex := -1;
    CBTextures.ItemIndex := -1;
    Edit5.Text := '';
    CBChildParent.ItemIndex := -1;
    Panel4.Visible := True;
    SE1.Value := 0;
    SE2.Value := 0;
    SE3.Value := 0.5;
    SE4.Value := 0.5;
    SE5.Value := 1.0;
    SE6.Value := 1.0;
    SE7.Value := 0.0;
    SE8.Value := 0;
    BNewChild.Enabled := True;
  end;

  FInitializingWidget := False;
end;

procedure TFormTools.BChooseImageFileClick(Sender: TObject);
begin
  if Sender = BChooseImageFile then begin
    if not OD1.Execute then exit;
    Label2.Caption := OD1.FileName;
    Edit1.Text := 'tex'+ChangeFileExt(ExtractFileName(OD1.FileName), '');
    UpdateTextureWidgetState;
  end;

  if Sender = BUpdateTexture then begin
    Textures.Update(Label2.Caption, Edit1.Text, SE9.Value, SE10.Value,
                    CheckBox1.Checked, SE11.Value, SE12.Value);
    Label2.Caption := '';
    UpdateTextureWidgetState;
    Project.SetModified;
  end;

  if Sender = BAddTexture then
    DoAddTexture;

  if Sender = BDeleteTexture then
    DoDeleteTexture;
end;

procedure TFormTools.CBChildParentDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var s: string;
begin
  with CBChildParent.Canvas do begin
    if odSelected in State then
      Brush.Color := clHighLight;
    Brush.Style := bsSolid;
    FillRect(ARect);

    Brush.Style := bsClear;
    if Index = 0 then s := 'root'
      else s := Surfaces.GetItemByID(CBChildParent.Items.Strings[Index].ToInteger)^.name;
    TextOut(ARect.Left, ARect.Top, s);
  end;
end;

procedure TFormTools.CBChildTypeSelect(Sender: TObject);
var texItem: PTextureItem;
begin
  if FInitializingWidget then exit;
  //if FWorkingChild = NIL then exit;

  if Sender = CBTextures then begin
    if Edit5.Text = '' then begin
      texItem := Textures.GetItemByName(CBTextures.Text);
      if texItem <> NIL then
        Edit5.Text := ChangeFileExt(ExtractFilename(texItem^.filename), '');
    end;
  end;

  if FWorkingChild <> NIL then DoUpdateChild;
end;

procedure TFormTools.CBTexturesSelect(Sender: TObject);
begin
  if Sender = BNewChild then begin
    DoAddNewChild;
    ShowSelectionData(NIL);
  end;
end;

procedure TFormTools.FormShow(Sender: TObject);
begin
  Left := FormMain.Width-Width;
  FillListBoxTextureNames;
  Surfaces.FillComboBox(CBChildParent);
  Textures.FillComboBox(CBTextures);
  ShowSelectionData(NIL);
end;

procedure TFormTools.LBChildsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var lb: TListBox;
begin
  lb := Sender as TListBox;
  lb.ItemIndex := lb.GetIndexAtXY(X, Y);
end;

procedure TFormTools.LBTextureNamesSelectionChange(Sender: TObject; User: boolean);
var i: integer;
  p: PTextureItem;
begin
  i := LBTextureNames.ItemIndex;
  if i = -1 then Label2.Caption := ''
  else begin
    p := Textures.Mutable[i];
    Label2.Caption := p^.filename;
    Edit1.Text := p^.name;
    SE9.Value := p^.width;
    SE10.Value := p^.height;
    CheckBox1.Checked := p^.isMultiFrame;
    SE11.Value := p^.frameWidth;
    SE12.Value := p^.frameHeight;
  end;
  UpdateTextureWidgetState;
end;

end.

