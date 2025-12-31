unit u_surface_list;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, StdCtrls,
  OGLCScene, gvector,
  BGRABitmap, BGRABitmapTypes,
  u_texture_list, u_ui_handle, u_undo_redo;

type

classOfSimpleSurfaceWithEffect = class of TSimpleSurfaceWithEffect;
TSurfaceList = class;
{ TSurfaceDescriptor }

PSurfaceDescriptor = ^TSurfaceDescriptor;
TSurfaceDescriptor = record
private
  HandleManager: TUIHandleManager;
  FAngleOrigin: single; // used to rotate the surface
  FScaleOrigin, FPosOrigin: TPointF;
  FSizeOrigin: TSize;
  function GetPivot: TPointF;
  function GetSelected: boolean;
  procedure SetPivot(AWorldCoor: TPointF);
  procedure SetSelected(AValue: boolean);
private
  FChildsStored: array of TSimpleSurfaceWithEffect;
public
  ParentList: TSurfaceList;
  id: integer;           // unique ID
  parentID: integer;     // parentID = -1 means the surface is the root.
  name: string;          // by default same as textureName
  surface: TSimpleSurfaceWithEffect;
  textureName: string;   // the subfolder+filename+extension without path
  classtype: classOfSimpleSurfaceWithEffect;
  // temporary variables used when loading. They keep safe the original values
  pivotX, pivotY, angle, x, y, scaleX, scaleY: single;
  zOrder: integer;
  flipH, flipV: boolean;
  opacity: single;
  tint: TBGRAPixel;
  tintMode: TTintMode;
  width, height, layerindex: integer;
  frameindex: single;
  blendmode: byte;
  // EXTRA properties
  // TSpriteWithElasticCorner
  TopLeftOffsetX, TopLeftOffsetY,
  TopRightOffsetX, TopRightOffsetY,
  BottomRightOffsetX, BottomRightOffsetY,
  BottomLeftOffsetX, BottomLeftOffsetY: single;
  // TQuad4Color
  TopLeftColor, TopRightColor, BottomRightColor,BottomLeftColor: TBGRAPixel;
  // TGradientRectangle
  GradientData: string;
  // TDeformationGrid
  DeformationGridData: string;
  // UI objects with bodyshape
  BodyShapeData: string;
  BackGradientData: string;
  // UI panels with effects
  BGDarkenColor: TBGRAPixel;
  OnShowScenario,
  OnHideScenario: string;
  // UI objects with text (textured font)
  FontDescriptorName, Caption: string;
  Atlas: TAtlas;   // to contain localy the textured font
  TexturedFont: TTexturedFont;  // the textured font created from Atlas with FontDescriptorName and Caption
  TextAlignment: TOGLCAlignment;
  AutoSize: boolean;
  // UI objects with check
  Checked: boolean;
  CheckShape: TUICheckShape;
  CheckFill: TUICheckFill;
  CheckColorChecked: TBGRAPixel;
  textureName2: string;
  CheckAdjustImageToFontHeight: boolean;
  // ScrollBar
  uiOrientation: TUIOrientation;
  scrollbarMin, scrollbarMax, scrollbarPageSize, scrollbarPosition: ptrInt;
//  scrollbarCursorBodyShapeData: string;
//  scrollbarCursorGradientData: string;
  // ProgressBar
  ProgressBarGradientData: string;
  ProgressBarReversed: boolean;
  ProgressBarPercent: single;
  // scrollbox
  scrollboxUseVScrollbar, scrollboxUseHScrollbar: boolean;
  // listbox
  listboxUnselectedTextColor: TBGRAPixel;
  listboxUnselectedGradientData: string;
  listboxSelectedTextColor: TBGRAPixel;
  listboxSelectedGradientData: string;
  listboxMultiSelect: boolean;
  listboxItemHeight: integer;
  // TextArea
  TextAreaText: string;
  TextAreaTextTint: TBGRAPixel;


  procedure InitDefault;
  procedure KillSurface;
  procedure CreateCollisionBody;
  procedure CreateSurface(aCreateUIHandle: boolean=True);
  procedure RecreateSurfaceBecauseTextureChanged;
  procedure SetChildDependency;
  function GetTextureFromTextureName: PTexture;
  function GetTexture2FromTexturename2: PTexture;

  procedure SaveAndRemoveChilds;
  procedure RestoreChilds;

  procedure SetValuesFromTemporaryVariables; // sets x, y, pivot,...
  procedure DuplicateValuesToTemporaryVariables;

  function IsRoot: boolean;
  function IsTextured: boolean;
  function HaveTextureWithMultipleFrame: boolean;
  function GetTextureFrameCount: integer;

  function IsOverScaleHandle(aWorldPt: TPointF; out aType: TScaleHandle): boolean;
  function IsOverRotateHandle(aWorldPt: TPointF): boolean;
  function IsOverPivotHandle(aWorldPt: TPointF): boolean;
  procedure AddOffsetToPivot(aOffset: TPointF); // in world coordinates
  procedure UpdateHandlePosition;
  procedure ToogleScaledAndRotatedHandle;
  procedure ShowHandle;
  procedure HideHandle;

  procedure SaveCurrentAngleBeforeRotation;
  procedure ComputeAngle(aPreviousReferencePointInWorld,
    aReferencePointInWorld: TPointF; aUseIncrement: boolean);
  procedure SaveCurrentScaleValueBeforeScaling;
  procedure ComputeScale(aType: TScaleHandle; aDelta: TPointF; aKeepAspectRatio: boolean);
  procedure SaveCurrentPosAndSizeBeforeResizing;
  procedure AddDeltaToSurfaceSize(aType: TScaleHandle; aDelta: TPoint; aKeepAspectRatio: boolean);
  function IsContainedBy(const r: TRectF): boolean;

  property Selected: boolean read GetSelected write SetSelected;
  property Pivot: TPointF read GetPivot write SetPivot;

  procedure DuplicateTo(aSurface: PSurfaceDescriptor);

  function GetSurfaceType: classOfSimpleSurfaceWithEffect;
  function GetWidth: integer;
  function GetHeight: integer;
  function GetX: single;
  function GetY: single;
  function GetLayerIndex: integer;
  function GetZOrder: integer;
  function GetPivotX: single;
  function GetPivotY: single;
  function GetAngle: single;
  function GetScaleX: single;
  function GetScaleY: single;
  function GetFlipH: boolean;
  function GetFlipV: boolean;
  function GetOpacity: single;
  function GetTint: TBGRAPixel;
  function GetTintMode: TTintMode;
  function GetFrame: single;
  function GetBlendmode: byte;

  procedure SetSurfaceLayerIndex(aIndex: integer);

  function SaveToString: string;
  procedure LoadFromString(const s: string);

  function ExportToPascalString(aTextureList: TTextureList): string;

end;
ArrayOfPSurfaceDescriptor = array of PSurfaceDescriptor;

{$define _INTERFACE}
{$I u_surface_undoredo.inc}
{$undef _INTERFACE}

TOnGetTextureEvent = function(): TTextureList of object;

{ TSurfaceList }

TSurfaceList = class(specialize TVector<TSurfaceDescriptor>)
private
  FID: integer;
  function NextID: integer;
  function GetSortedSurfaceFromNearestTofurthest: ArrayOfPSurfaceDescriptor;
private
  FUndoRedoManager: TSurfaceUndoRedoManager;
  FWorkingLayer: integer;
private
  FModeForLevelEditor: boolean;
  FTextures: TTextureList;
  FOnGetTexture: TOnGetTextureEvent;
  function GetTextures: TTextureList;
public
  constructor Create;
  destructor Destroy; override;
  procedure SetModeForLevelEditor;

  procedure Clear; reintroduce;
  function AddEmpty: PSurfaceDescriptor;

  function GetItemByID(aID: integer): PSurfaceDescriptor;
  function GetItemIndexByID(aID: integer): integer;
  function GetByIndex(aIndex: integer): PSurfaceDescriptor;
  function GetItemBySurface(aSurface: TSimpleSurfaceWithEffect): PSurfaceDescriptor;
  function GetItemsThatUseThisTexture(aTextureItem: PTextureItem): ArrayOfPSurfaceDescriptor;
  //procedure DeleteItemsThatUseThisTexture(aTextureItem: PTextureItem);

  function GetRootItem: PSurfaceDescriptor;
  function RootIsDefined: boolean;

  // Returns the items pointed by the specified position.
  // Items are sorted from nearest to furthest away.
  function GetItemsAt(aX, aY: integer): ArrayOfPSurfaceDescriptor;

  function NameExists(const aName: string): boolean;
  function TextureNameisUsedByASurface(const aTextureName: string): boolean;
  function UseTexture: boolean;

  procedure DeleteItemByID(aID: integer);
  function DuplicateItemsByID(aItems: ArrayOfPSurfaceDescriptor): ArrayOfPSurfaceDescriptor;
  function DuplicateAndShiftItemsByID(aItems: ArrayOfPSurfaceDescriptor;
    aCoeffLeft, aCoeffRight, aCoeffTop, aCoeffBottom, aOverlapValue: single): ArrayOfPSurfaceDescriptor;

  function ItemsDescriptorToArrayOfID(aItems: ArrayOfPSurfaceDescriptor): TArrayOfInteger;

  function GetItemsIndexesByIDSortedSmallToHigh(aItems: ArrayOfPSurfaceDescriptor): TArrayOfInteger;
  function GetIndexesForALayer(aLayer: TLayer): TArrayOfInteger;
  procedure MoveItemTo(aCurrentIndex, aNewindex: integer);
  procedure ShiftItemToTopOneStep(aItemIndex: integer);
  procedure ShiftItemToBackOneStep(aItemIndex: integer);

{  procedure ShiftItemsToTop(A: TArrayOfInteger);
  procedure ShiftItemsToTopOneStep(A: TArrayOfInteger);
  procedure ShiftItemsToBack(A: TArrayOfInteger);
  procedure ShiftItemsToBackOneStep(A: TArrayOfInteger); }

  procedure SelectNone;
  function GetItemsBounds: TRectF;

  procedure SetValuesFromTemporaryVariables; // sets x, y, pivot,... on all surfaces
  procedure CopySurfaceValuesToTemporaryVariables;

  function SaveToString: string;
  procedure LoadFromString(const s: string; aCreateSurfaces: boolean=True);
{  procedure SaveTo(t:TStringList);
  procedure LoadFrom(t:TStringList);  }

  // items are the ID of each TSurfaceDescriptor
  procedure FillComboBox(aCB: TComboBox);
  procedure ReplaceNameInComboBox(aCB: TComboBox);
  procedure FillListBox(aLB: TListBox);

  // to access the texture list with a direct property, either a callback
  property Textures: TTextureList read GetTextures write FTextures;
  // to access the texture list with a callback (use always property Textures
  property OnGetTexture: TOnGetTextureEvent read FOnGetTexture write FOnGetTexture;

  property WorkingLayer: integer read FWorkingLayer write FWorkingLayer;
  property UndoRedoManager: TSurfaceUndoRedoManager read FUndoRedoManager;

  property ModeForLevelEditor: boolean read FModeForLevelEditor;

public // Panel Editor
  function AddMainPanel(const aName: string; aWidth, aHeight: integer): TUIPanelWithEffects;
end;


implementation

uses u_common, u_screen_spritebuilder, u_utils, u_ui_objectlist, Math;

{$define _IMPLEMENTATION}
{$I u_surface_undoredo.inc}
{$undef _IMPLEMENTATION}

{ TSurfaceDescriptor }

function TSurfaceDescriptor.GetSelected: boolean;
begin
  Result := HandleManager.IsVisible;
end;

function TSurfaceDescriptor.GetPivot: TPointF;
begin
  Result := surface.Pivot;
end;

procedure TSurfaceDescriptor.SetPivot(AWorldCoor: TPointF);
var p :TPointF;
begin
  p := surface.SceneToSurface(AWorldCoor);
  p.x := p.x / surface.Width;
  p.y := p.y / surface.Height;
  surface.Pivot.x := p.x;
  surface.Pivot.y := p.y;
  HandleManager.UpdatePivotHandle(surface);

  pivotX := p.x;
  pivotY := p.y
end;

procedure TSurfaceDescriptor.SetSelected(AValue: boolean);
begin
  if AValue then HandleManager.ToogleScaledAndRotatedHandle(surface)
    else HandleManager.HideAll;
end;

procedure TSurfaceDescriptor.InitDefault;
begin
  Surface := NIL;
  id := -1;
  parentID := -1;
  textureName := '';
  pivotX := 0.5;
  pivotY := 0.5;
  scaleX := 1.0;
  scaleY := 1.0;
  angle := 0.0;
  tint := BGRA(0,0,0,0);
  tintmode := tmReplaceColor;
  opacity := 255;
  flipH := False;
  flipV := False;
  zOrder := 0;
  frameindex := 1.0;
  blendmode := FX_BLEND_NORMAL;

  // TSpriteWithElasticCorner
  TopLeftOffsetX := 0; TopLeftOffsetY := 0;
  TopRightOffsetX := 0; TopRightOffsetY := 0;
  BottomRightOffsetX := 0; BottomRightOffsetY := 0;
  BottomLeftOffsetX := 0; BottomLeftOffsetY := 0;
  //TQuad4Color
  TopLeftColor := BGRA(255,0,0);
  TopRightColor := BGRA(0,255,0);
  BottomRightColor := BGRA(0,0,255);
  BottomLeftColor := BGRA(255,255,0);
  // TGradientRectangle
  GradientData := DEFAULT_GRADIENT;
  // TDeformationGrid
  DeformationGridData := '';

  // UIObject
  BodyShapeData := '';
  BackGradientData := '';
  BGDarkenColor := BGRAPixelTransparent;
  OnShowScenario := '';
  OnHideScenario := '';
  Caption := '';
  AutoSize := True;
  FontDescriptorName := '';
  Atlas := NIL;
  TexturedFont := NIL;
  Checked := False;
  CheckShape := ctRectangle;
  CheckFill := cfColor;
  CheckColorChecked := BGRAWhite;
  textureName2 := '';
  CheckAdjustImageToFontHeight := True;
  uiOrientation := uioHorizontal;
  scrollbarMin := 0;
  scrollbarMax := 100;
  scrollbarPageSize := 10;
  scrollbarPosition := 0;
//  scrollbarCursorBodyShapeData := '';
//  scrollbarCursorGradientData := '';
  ProgressBarGradientData := '';
  ProgressBarReversed := False;
  ProgressBarPercent := 0.0;
  scrollboxUseVScrollbar := True;
  scrollboxUseHScrollbar := False;
  listboxUnselectedTextColor := BGRA(220,220,220);
  listboxUnselectedGradientData := '';
  listboxSelectedTextColor := BGRAWhite;
  listboxSelectedGradientData := '';
  listboxMultiSelect := False;
  listboxItemHeight := 24;
  TextAreaText := '';
  TextAreaTextTint := BGRA(220,220,220);

  classtype := TSimpleSurfaceWithEffect;
  HandleManager.InitDefault;
end;

procedure TSurfaceDescriptor.KillSurface;
begin
  if Atlas <> NIL then FreeAndNil(Atlas);
  TexturedFont := NIL;

  if surface <> NIL then surface.Kill;
  surface := NIL;
  HandleManager.KillSurfaces;
end;

procedure TSurfaceDescriptor.CreateCollisionBody;
begin
  surface.CollisionBody.RemoveAll;
  surface.CollisionBody.AddPolygon([PointF(0,0), PointF(surface.Width, 0),
                                    PointF(surface.Width, surface.Height), PointF(0, surface.Height)]);
end;

procedure TSurfaceDescriptor.CreateSurface(aCreateUIHandle: boolean=True);
var tex: PTexture;
begin
  tex := GetTextureFromTextureName;

  if (classType = TSprite) then begin                  // TSprite
    surface := TSprite.Create(tex, False);
    surface.Frame := frameindex;
    TSprite(surface).SetSize(width, height);
  end
  else
  if classType = TSpriteWithElasticCorner then begin   // TSpriteWithElasticCorner
    surface := TSpriteWithElasticCorner.Create(tex, False);
    surface.Frame := frameindex;
    TSpriteWithElasticCorner(surface).SetSize(width, height);
    with TSpriteWithElasticCorner(surface) do begin
      CornerOffset.TopLeft.x.Value := TopLeftOffsetX;
      CornerOffset.TopLeft.y.Value := TopLeftOffsetY;
      CornerOffset.TopRight.x.Value := TopRightOffsetX;
      CornerOffset.TopRight.y.Value := TopRightOffsetY;
      CornerOffset.BottomRight.x.Value := BottomRightOffsetX;
      CornerOffset.BottomRight.y.Value := BottomRightOffsetY;
      CornerOffset.BottomLeft.x.Value := BottomLeftOffsetX;
      CornerOffset.BottomLeft.y.Value := BottomLeftOffsetY;
    end;
  end else
  if classType = TTiledSprite then begin
    surface := TTiledSprite.Create(tex, False);
    surface.Frame := frameindex;
    TTiledSprite(surface).SetSize(width, height)
  end else
  if classType = TPolarSprite then begin
    surface := TPolarSprite.Create(tex, False);
    surface.Frame := frameindex;
    TPolarSprite(surface).SetSize(width, height);
  end else
  if classType = TScrollableSprite then begin
    surface := TScrollableSprite.Create(tex, False);
    surface.Frame := frameindex;
    TScrollableSprite(surface).SetSize(width, height);
  end else
  if classType = TShapeOutline then begin
    surface := TShapeOutline.Create(FScene)
  end else
  if classType = TGradientRectangle then begin         // TGradientRectangle
    surface := TGradientRectangle.Create(FScene);
    TGradientRectangle(surface).Gradient.LoadGradientDataFromString(GradientData);
    TGradientRectangle(surface).SetSize(width, height);
  end else
  if classType = TQuad4Color then begin                // TQuad4Color
    surface := TQuad4Color.Create(FScene);
    TQuad4Color(surface).SetSize(width, height);
    with TQuad4Color(surface) do begin
      TopLeftColor.Value := Self.TopLeftColor;
      TopRightColor.Value := Self.TopRightColor;
      BottomRightColor.Value := Self.BottomRightColor;
      BottomLeftColor.Value := Self.BottomLeftColor;
    end;
  end else
  if classType = TDeformationGrid then begin           // TDeformationGrid
    surface := TDeformationGrid.Create(tex, False);
    surface.Frame := frameindex;
    TDeformationGrid(surface).SetSize(width, height);
    if DeformationGridData <> '' then
      TDeformationGrid(surface).LoadDeformationDataFromString(DeformationGridData);
  end else
  if classType = TSpriteContainer then begin  // TSpriteContainer
    surface := TSpriteContainer.Create(FScene);
    TSpriteContainer(surface).ShowOrigin := True;
  end else
  if (classType = TUIPanel) {or (classtype = TUIPanelWithEffects)} then begin      // UIPanel
    surface := TUIPanel.Create(FScene);
    TUIPanel(surface).BodyShape.LoadFromString(BodyShapeData);
    TUIPanel(surface).BodyShape.ResizeCurrentShape(width, height, True);
    TUIPanel(surface).BackGradient.LoadGradientDataFromString(BackGradientData);
    TUIPanel(surface).BackGradient.ComputeVerticesAndIndices(width, height);
  end else
  if classtype = TUIPanelWithEffects then begin // TUIPanelWithEffects
    surface := TUIPanelWithEffects.Create(FScene);
    TUIPanelWithEffects(surface).BodyShape.LoadFromString(BodyShapeData);
    TUIPanel(surface).BodyShape.ResizeCurrentShape(width, height, True);
    TUIPanelWithEffects(surface).BackGradient.LoadGradientDataFromString(BackGradientData);
    TUIPanel(surface).BackGradient.ComputeVerticesAndIndices(width, height);
    if BGDarkenColor.alpha <> 0 then TUIPanelWithEffects(surface).ActivateSceneDarken(BGDarkenColor);
    TUIPanelWithEffects(surface).Show('');
  end else
  if classType = TFreeText then begin  // TFreeText
    surface := TFreeText.Create(FScene);
    FontBank.GetAtlasWithTexturedFont(FontDescriptorName, Caption, Atlas, TexturedFont, NIL);
    TFreeText(surface).Caption := Caption;
    TFreeText(surface).TexturedFont := TexturedFont;
  end else
  if classType = TUILabel then begin // UILabel
    surface := TUILabel.Create(FScene);
    FontBank.GetAtlasWithTexturedFont(FontDescriptorName, Caption, Atlas, TexturedFont, NIL);
    TUILabel(surface).CaptionDescriptor.Alignment := TextAlignment;
    TUILabel(surface).TexturedFont := TexturedFont;
    TUILabel(surface).Caption := Caption;
  end else
  if classType = TUIImage then begin  // TUIImage
    surface := TUIImage.Create(FScene, GetTextureFromTextureName, width, height);
  end else
  if classType = TUIButton then begin // TUIButton
    FontBank.GetAtlasWithTexturedFont(FontDescriptorName, Caption, Atlas, TexturedFont, NIL);
    surface := TUIButton.Create(FScene, Caption, TexturedFont, GetTextureFromTextureName);
    TUIButton(surface).MouseInteractionEnabled := False;
  end else
  if classType = TUICheck then begin // TUICheck
    FontBank.GetAtlasWithTexturedFont(FontDescriptorName, Caption, Atlas, TexturedFont, NIL);
    surface := TUICheck.Create(FScene, Caption, TexturedFont);
    TUICheck(surface).Checked := Checked;
    if CheckShape = ctUseTexture then begin
      TUICheck(surface).CustomizeCheckBox(GetTexture2FromTextureName2, GetTextureFromTextureName, CheckAdjustImageToFontHeight);
    end else begin
      TUICheck(surface).CustomizeCheckBox(CheckShape, CheckFill);
      TUICheck(surface).ColorChecked := CheckColorChecked;
    end;
    TUICheck(surface).MouseInteractionEnabled := False;
  end else
  if classType = TUIRadio then begin // TUIRadio
    FontBank.GetAtlasWithTexturedFont(FontDescriptorName, Caption, Atlas, TexturedFont, NIL);
    surface := TUIRadio.Create(FScene, Caption, TexturedFont);
    TUIRadio(surface).Checked := Checked;
    if CheckShape = ctUseTexture then begin
      TUIRadio(surface).CustomizeCheckBox(GetTexture2FromTextureName2, GetTextureFromTextureName, CheckAdjustImageToFontHeight);
    end else begin
      TUIRadio(surface).CustomizeCheckBox(CheckShape, CheckFill);
      TUIRadio(surface).ColorChecked := CheckColorChecked;
    end;
    TUIRadio(surface).MouseInteractionEnabled := False;
  end else
  if classType = TUIScrollBar then begin  // TUIScrollBar
    surface := TUIScrollBar.Create(FScene, uiOrientation);
    TUIScrollBar(surface).BodyShape.LoadFromString(BodyShapeData);
    TUIScrollBar(surface).BodyShape.ResizeCurrentShape(width, height, True);
    TUIScrollBar(surface).BackGradient.LoadGradientDataFromString(BackGradientData);
    TUIScrollBar(surface).BackGradient.ComputeVerticesAndIndices(width, height);
    TUIScrollBar(surface).SliderShape.LoadFromString(BodyShapeData);
    TUIScrollBar(surface).SetParams(scrollbarPosition, scrollbarMin, scrollbarMax, scrollbarPageSize);
    TUIScrollBar(surface).MouseInteractionEnabled := False;
  end else
  if classType = TUIProgressBar then begin // TUIProgressBar
    surface := TUIProgressBar.Create(FScene, uiOrientation);
    TUIProgressBar(surface).Reversed := progressbarReversed;
    TUIProgressBar(surface).BodyShape.LoadFromString(BodyShapeData);
    TUIProgressBar(surface).BodyShape.ResizeCurrentShape(width, height, True);
    TUIProgressBar(surface).Gradient.LoadGradientDataFromString(ProgressBarGradientData);
    TUIProgressBar(surface).BackGradient.ComputeVerticesAndIndices(width, height);
    TUIProgressBar(surface).Percent := ProgressBarPercent;
    TUIProgressBar(surface).MouseInteractionEnabled := False;
  end else
  if classType = TUIScrollBox then begin // TUIScrollBox
    surface := TUIScrollBox.Create(FScene, scrollboxUseVScrollbar, scrollboxUseHScrollbar);
    TUIScrollBox(surface).BodyShape.LoadFromString(BodyShapeData);
    TUIScrollBox(surface).BodyShape.ResizeCurrentShape(width, height, True);
    TUIScrollBox(surface).BackGradient.LoadGradientDataFromString(BackGradientData);
    TUIScrollBox(surface).BackGradient.ComputeVerticesAndIndices(width, height);
    TUIScrollBox(surface).MouseInteractionEnabled := False;
  end else
  if classType = TUIListBox then begin // TUIListBox
    FontBank.GetAtlasWithTexturedFont(FontDescriptorName, 'Item 123', Atlas, TexturedFont, NIL);
    surface := TUIListBox.Create(FScene, TexturedFont);
    TUIListBox(surface).BodyShape.LoadFromString(BodyShapeData);
    TUIListBox(surface).BodyShape.ResizeCurrentShape(width, height, True);
    TUIListBox(surface).ItemColor.ColorText := listboxUnselectedTextColor;
    TUIListBox(surface).ItemColor.GradientItem.LoadGradientDataFromString(listboxUnselectedGradientData);
    TUIListBox(surface).ItemColor.ColorSelectedText := listboxSelectedTextColor;
    TUIListBox(surface).ItemColor.GradientItemSelected.LoadGradientDataFromString(listboxSelectedGradientData);
    TUIListBox(surface).ItemHeight := listboxItemHeight;
    TUIListBox(surface).Add('Item 1');
    TUIListBox(surface).Add('Item 2');
    TUIListBox(surface).Add('Item 3');
    TUIListBox(surface).FirstSelectedIndex := 1;
    TUIListBox(surface).MultiSelect := listboxMultiSelect;
    TUIListBox(surface).MouseInteractionEnabled := False;
  end else
  if classType = TUITextArea then begin  // TUITextArea
    surface := TUITextArea.Create(FScene);
    TUITextArea(surface).BodyShape.LoadFromString(BodyShapeData);
    TUITextArea(surface).BodyShape.ResizeCurrentShape(width, height, True);
    TUITextArea(surface).BackGradient.LoadGradientDataFromString(BackGradientData);
    TUITextArea(surface).BackGradient.ComputeVerticesAndIndices(width, height);
    TUITextArea(surface).Text.Caption := TextAreaText;
    FontBank.GetAtlasWithTexturedFont(FontDescriptorName, TextAreaText, Atlas, TexturedFont, NIL);
    TUITextArea(surface).Text.TexturedFont := TexturedFont;
    TUITextArea(surface).Text.Align := TextAlignment;
    TUITextArea(surface).Text.Tint.Value := TextAreaTextTint;
    TUITextArea(surface).MouseInteractionEnabled := False;
  end

  else raise exception.create('forgot to implement '''+classType.ClassName+'''');

  // collision for selection
  CreateCollisionBody;

  if aCreateUIHandle then
    UIHandle.CreateUIHandles(@HandleManager);
end;

procedure TSurfaceDescriptor.RecreateSurfaceBecauseTextureChanged;
begin
  SaveAndRemoveChilds;
  KillSurface;
  CreateSurface;
  RestoreChilds;
  SetChildDependency;
end;

procedure TSurfaceDescriptor.SetChildDependency;
var parentItem: PSurfaceDescriptor;
begin
  if parentID = -1 then begin
    if ParentList.ModeForLevelEditor then FScene.Add(surface, layerindex)
      else begin
        FScene.Add(surface, ParentList.WorkingLayer);
        //if classtype = TUIPanelWithEffects then TUIPanelWithEffects(surface).Show('');
      end;
  end else begin
      parentItem := ParentList.GetItemByID(parentID);
      if parentItem = NIL
        then raise exception.create('parent with ID='+parentID.ToString+' not found !')
        else parentItem^.surface.AddChild(surface, zOrder);
  end;
end;

function TSurfaceDescriptor.GetTextureFromTextureName: PTexture;
var texItem: PTextureItem;
begin
  texItem := ParentList.Textures.GetItemByName(textureName);
  if texItem <> NIL then Result := texItem^.texture else Result := NIL;
end;

function TSurfaceDescriptor.GetTexture2FromTexturename2: PTexture;
var texItem: PTextureItem;
begin
  texItem := ParentList.Textures.GetItemByName(textureName2);
  if texItem <> NIL then Result := texItem^.texture else Result := NIL;
end;

procedure TSurfaceDescriptor.SaveAndRemoveChilds;
var i: Integer;
begin
  FChildsStored := NIL;
  if surface = NIl then exit;
  SetLength(FChildsStored, surface.ChildCount);
  for i:=surface.ChildCount-1 downto 0 do begin
    FChildsStored[i] := surface.Childs[i];
    surface.RemoveChild(surface.Childs[i]);
  end;
end;

procedure TSurfaceDescriptor.RestoreChilds;
var i: Integer;
begin
  if surface = NIl then exit;
  for i:=0 to High(FChildsStored) do
    FChildsStored[i].SetChildOf(surface, FChildsStored[i].ZOrderAsChild);
end;

procedure TSurfaceDescriptor.SetValuesFromTemporaryVariables;
begin
  surface.X.Value := x;
  surface.Y.Value := y;
  surface.Pivot.x := pivotX;
  surface.Pivot.y := pivotY;
  surface.Scale.x.Value := scaleX;
  surface.Scale.y.Value := scaleY;
  surface.Angle.Value := angle;
  surface.FlipH := flipH;
  surface.FlipV := flipV;
  surface.Opacity.Value := opacity;
  surface.Tint.Value := tint;
  surface.TintMode := tintmode;
  surface.Frame := frameindex;
  surface.BlendMode := blendmode;
  if ParentList.FModeForLevelEditor then
    TSprite(surface).SetSize(width, height);
end;

procedure TSurfaceDescriptor.DuplicateValuesToTemporaryVariables;
begin
  pivotX := surface.Pivot.x;
  pivotY := surface.Pivot.y;
  angle := surface.Angle.Value;
  x := surface.X.Value;
  y := surface.Y.Value;
  scaleX := surface.Scale.X.Value;
  scaleY := surface.Scale.Y.Value;
  zOrder := surface.ZOrderAsChild;
  flipH := surface.FlipH;
  flipV := surface.FlipV;
  opacity := surface.Opacity.Value;
  tint := surface.Tint.Value;
  tintmode := surface.TintMode;
  width := surface.Width;
  height := surface.Height;
  frameindex := Trunc(surface.Frame);
  if ParentList.ModeForLevelEditor then
    layerindex := FScene.LayerIndexOf(surface.ParentLayer);
end;

function TSurfaceDescriptor.IsRoot: boolean;
begin
  Result := parentID = -1;
end;

function TSurfaceDescriptor.IsTextured: boolean;
begin
  Result := (classtype = TSprite) or
            (classtype = TSpriteWithElasticCorner) or
            (classtype = TTiledSprite) or
            (classtype = TPolarSprite) or
            (classtype = TScrollableSprite) or
            (classtype = TDeformationGrid);
end;

function TSurfaceDescriptor.HaveTextureWithMultipleFrame: boolean;
var tex: PTexture;
begin
  Result := IsTextured;
  if not Result then exit;
  tex := GetTextureFromTextureName;
  if tex = NIL then Result := False
    else Result := tex^.FrameCount > 1;
end;

function TSurfaceDescriptor.GetTextureFrameCount: integer;
var tex: PTexture;
begin
  if not IsTextured then exit(1);

  tex := GetTextureFromTextureName;
  if tex = NIL then exit(1);

  Result := Max(tex^.FrameCount-1, 1);
end;

function TSurfaceDescriptor.IsOverScaleHandle(aWorldPt: TPointF; out aType: TScaleHandle): boolean;
begin
  Result := HandleManager.IsOverScaleHandle(aWorldPt, aType);
end;

function TSurfaceDescriptor.IsOverRotateHandle(aWorldPt: TPointF): boolean;
begin
  Result := HandleManager.IsOverRotateHandle(aWorldPt);
end;

function TSurfaceDescriptor.IsOverPivotHandle(aWorldPt: TPointF): boolean;
begin
  Result := HandleManager.IsOverPivotHandle(aWorldPt);
end;

procedure TSurfaceDescriptor.SaveCurrentAngleBeforeRotation;
begin
  FAngleOrigin := surface.Angle.Value;
end;

procedure TSurfaceDescriptor.ComputeAngle(aPreviousReferencePointInWorld, aReferencePointInWorld: TPointF;
  aUseIncrement: boolean);
var ppivot: TPointF;
  a: single;
begin
  with surface do
    ppivot := SurfaceToScene(PointF(Width*Pivot.x, Height*Pivot.y));

  a := FAngleOrigin{surface.Angle.Value} + CartesianToPolar(ppivot, aReferencePointInWorld).Angle -
                             CartesianToPolar(ppivot, aPreviousReferencePointInWorld).Angle;
  //if a < 0 then a := a + 360;
  if a > 180 then a := a - 360
  else if a < -180 then a := a + 360;


  if aUseIncrement then
    a := Trunc(a / 15) * 15;
  surface.Angle.Value := a;
end;

procedure TSurfaceDescriptor.SaveCurrentScaleValueBeforeScaling;
begin
  FScaleOrigin := surface.Scale.Value;
  FPosOrigin := surface.GetXY;
end;

procedure TSurfaceDescriptor.ComputeScale(aType: TScaleHandle; aDelta: TPointF;
  aKeepAspectRatio: boolean);
const MIN_SCALE = 0.1;
var zx, zy, deltaW, deltaH: single;
  procedure CheckZXNoKeepAspect;
  begin
    if FScaleOrigin.x + zx < MIN_SCALE then zx := MIN_SCALE - FScaleOrigin.x;
  end;
  procedure CheckZYNoKeepAspect;
  begin
    if FScaleOrigin.y + zy < MIN_SCALE then zy := MIN_SCALE - FScaleOrigin.y;
  end;
  procedure AdjustZXKeepAspect;
  begin
    zy := zx;
    if (FScaleOrigin.x + zx < MIN_SCALE) or (FScaleOrigin.y + zy < MIN_SCALE) then
      zx := Max(MIN_SCALE - FScaleOrigin.x, MIN_SCALE - FScaleOrigin.y);
    zy := zx;
  end;
  procedure AdjustZYKeepAspect;
  begin
    zx := zy;
    if (FScaleOrigin.x + zx < MIN_SCALE) or (FScaleOrigin.y + zy < MIN_SCALE) then
      zy := Max(MIN_SCALE - FScaleOrigin.x, MIN_SCALE - FScaleOrigin.y);
    zx := zy;
  end;
begin
  zx := 0;
  zy := 0;
  case aType of
    shTopLeft: begin   // ok
      zx := -aDelta.x / surface.Width;
      zy := -aDelta.y / surface.Height;
      if aKeepAspectRatio then begin
        if zx < zy then AdjustZXKeepAspect
          else AdjustZYKeepAspect;
      end else begin
        CheckZXNoKeepAspect;
        CheckZYNoKeepAspect;
      end;
      deltaH := zy * surface.Height * 0.5;
      deltaW := zx * surface.Width * 0.5;
      surface.Scale.Value := FScaleOrigin + PointF(zx, zy);
      surface.X.Value := FPosOrigin.x - deltaW;
      surface.Y.Value := FPosOrigin.y - deltaH;
    end;

    shTopCenter: begin  // ok
      zx := 0;
      zy := -aDelta.y / surface.Height;
      if aKeepAspectRatio then AdjustZYKeepAspect
          else CheckZYNoKeepAspect;
      deltaH := zy * surface.Height * 0.5;
      surface.Scale.Value := FScaleOrigin + PointF(zx, zy);
      surface.Y.Value := FPosOrigin.y - deltaH;
    end;

    shTopRight: begin  // ok
      zx := aDelta.x / surface.Width;
      zy := -aDelta.y / surface.Height;
      if aKeepAspectRatio then begin
        if zx > zy then AdjustZXKeepAspect
          else AdjustZYKeepAspect;
      end else begin
        CheckZXNoKeepAspect;
        CheckZYNoKeepAspect;
      end;
      deltaH := zy * surface.Height * 0.5;
      deltaW := zx * surface.Width * 0.5;
      surface.Scale.x.Value := FScaleOrigin.x + zx;
      surface.Scale.y.Value := FScaleOrigin.y + zy;
      surface.X.Value := FPosOrigin.x + deltaW;
      surface.Y.Value := FPosOrigin.y - deltaH;
    end;

    shCenterRight: begin  // ok
      zx := aDelta.x / surface.Width;
      zy := 0;
      if aKeepAspectRatio then AdjustZXKeepAspect
        else CheckZXNoKeepAspect;
      deltaW := zx * surface.Width * 0.5;
      surface.Scale.Value := FScaleOrigin + PointF(zx, zy);
      surface.X.Value := FPosOrigin.x + deltaW;
    end;

    shBottomRight: begin  // ok
      zx := aDelta.x / surface.Width;
      zy := aDelta.y / surface.Height;
      if aKeepAspectRatio then begin
        if zx < zy then AdjustZXKeepAspect
          else AdjustZYKeepAspect;
      end else begin
        CheckZXNoKeepAspect;
        CheckZYNoKeepAspect;
      end;
      deltaH := zy * surface.Height * 0.5;
      deltaW := zx * surface.Width * 0.5;
      surface.Scale.Value := FScaleOrigin + PointF(zx, zy);
      surface.X.Value := FPosOrigin.x + deltaW;
      surface.Y.Value := FPosOrigin.y + deltaH;
    end;

    shBottomCenter: begin // ok
      zx := 0;
      zy := aDelta.y / surface.Height;
      if aKeepAspectRatio then AdjustZYKeepAspect
        else CheckZYNoKeepAspect;
      deltaH := zy * surface.Height * 0.5;
      surface.Scale.Value := FScaleOrigin + PointF(zx, zy);
      surface.Y.Value := FPosOrigin.y + deltaH;
    end;

    shBottomLeft: begin  // ok
       zx := -aDelta.x / surface.Width;
       zy := aDelta.y / surface.Height;
       if aKeepAspectRatio then begin
         if zx < zy then AdjustZXKeepAspect
           else AdjustZYKeepAspect;
       end else begin
         CheckZXNoKeepAspect;
         CheckZYNoKeepAspect;
       end;
       surface.Scale.x.Value := FScaleOrigin.x + zx;
       surface.Scale.y.Value := FScaleOrigin.y + zy;
       deltaW := zx * surface.Width * 0.5;
       deltaH := zy * surface.Height * 0.5;
       surface.X.Value := FPosOrigin.x - deltaW;
       surface.Y.Value := FPosOrigin.y + deltaH;
    end;

    shCenterLeft: begin  // ok
      zx := -aDelta.x / surface.Width;
      zy := 0;
      if aKeepAspectRatio then AdjustZXKeepAspect
        else CheckZXNoKeepAspect;
      deltaW := zx * surface.Width * 0.5;
      surface.Scale.Value := FScaleOrigin + PointF(zx, zy);
      surface.X.Value := FPosOrigin.x - deltaW;
    end;
  end;
end;

procedure TSurfaceDescriptor.SaveCurrentPosAndSizeBeforeResizing;
begin
  FSizeOrigin.cx :=  surface.Width;
  FSizeOrigin.cy :=  surface.Height;
  FPosOrigin := surface.GetXY;
end;

procedure TSurfaceDescriptor.AddDeltaToSurfaceSize(aType: TScaleHandle;
  aDelta: TPoint; aKeepAspectRatio: boolean);
const MIN_VALUE = 1;
var dw, dh, newW, newH: integer;
  procedure CheckDWNoKeepAspect;
  begin
    if FSizeOrigin.cx + dw < MIN_VALUE then dw := MIN_VALUE - FSizeOrigin.cx;
  end;
  procedure CheckDHNoKeepAspect;
  begin
    if FSizeOrigin.cy + dh < MIN_VALUE then dh := MIN_VALUE - FSizeOrigin.cy;
  end;
  procedure AdjustDWKeepAspect;
  begin
    dh := dw;
    if (FSizeOrigin.cx + dw < MIN_VALUE) or (FSizeOrigin.cy + dh < MIN_VALUE) then
      dw := Max(MIN_VALUE - FSizeOrigin.cx, MIN_VALUE - FSizeOrigin.cy);
    dh := dw;
  end;
  procedure AdjustDHKeepAspect;
  begin
    dw := dh;
    if (FSizeOrigin.cx + dw < MIN_VALUE) or (FSizeOrigin.cy + dh < MIN_VALUE) then
      dh := Max(MIN_VALUE - FSizeOrigin.cx, MIN_VALUE - FSizeOrigin.cy);
    dw := dh;
  end;

begin
  dw := aDelta.x;
  dh := aDelta.y;
  case aType of
      shTopLeft: begin  // ok
        dw := -aDelta.x;
        dh := -aDelta.y;
        if aKeepAspectRatio then begin
          if dw < dh then AdjustDWKeepAspect
            else AdjustDHKeepAspect;
        end else begin
          CheckDWNoKeepAspect;
          CheckDHNoKeepAspect;
        end;
        surface.X.Value := FPosOrigin.x - dw;
        surface.Y.Value := FPosOrigin.y - dh;
      end;

      shTopCenter: begin // ok
        dw := 0;
        dh := -aDelta.y;
        if aKeepAspectRatio then AdjustDHKeepAspect
          else CheckDHNoKeepAspect;
        surface.Y.Value := FPosOrigin.y - dh;
      end;

      shTopRight: begin  // ok
        dw := aDelta.x;
        dh := -aDelta.y;
        if aKeepAspectRatio then begin
          if dw > dh then AdjustDWKeepAspect
            else AdjustDHKeepAspect;
        end else begin
          CheckDWNoKeepAspect;
          CheckDHNoKeepAspect;
        end;
        surface.Y.Value := FPosOrigin.y - dh;
      end;

      shCenterLeft: begin // ok
        dw := -aDelta.x;
        dh := 0;
        if aKeepAspectRatio then AdjustDWKeepAspect
          else CheckDWNoKeepAspect;
        surface.X.Value := FPosOrigin.x - dw;
      end;

      shCenterRight: begin // ok
        dw := aDelta.x;
        dh := 0;
        if aKeepAspectRatio then AdjustDWKeepAspect
          else CheckDWNoKeepAspect;
      end;

      shBottomLeft: begin  // ok
        dw := -aDelta.x;
        dh := aDelta.y;
        if aKeepAspectRatio then begin
          if dw < dh then AdjustDWKeepAspect
            else AdjustDHKeepAspect;
        end else begin
          CheckDWNoKeepAspect;
          CheckDHNoKeepAspect;
        end;
        surface.X.Value := FPosOrigin.x - dw;
      end;

      shBottomCenter: begin  // ok
        dw := 0;
        dh := aDelta.y;
        if aKeepAspectRatio then AdjustDHKeepAspect
          else CheckDHNoKeepAspect;
      end;

      shBottomRight: begin // ok
        dw := aDelta.x;
        dh := aDelta.y;
        if aKeepAspectRatio then begin
          if dw < dh then AdjustDWKeepAspect
            else AdjustDHKeepAspect;
        end else begin
          CheckDWNoKeepAspect;
          CheckDHNoKeepAspect;
        end;
      end;
  end;//case

  newW := FSizeOrigin.cx + dw;
  newH := FSizeOrigin.cy + dh;

  // apply new size of the surface
  case surface.ClassName of
    'TSprite': TSprite(surface).SetSize(newW, newH);
    'TUIPanel', 'TUIPanelWithEffects': TUIClickableWithBodyShape(surface).BodyShape.ResizeCurrentShape(newW, newH, True);
    'TUIImage': TUIImage(surface).SetSize(newW, newH);
    'TUILabel':;
    'TUIButton': if not TUIButton(surface).AutoSize then TUIButton(surface).BodyShape.ResizeCurrentShape(newW, newH, True);
    'TUICheck':;
    'TUIRadio':;
    'TUIScrollBar', 'TUIProgressBar',
    'TUIScrollBox', 'TUIListBox',
    'TUITextArea': TUIClickableWithBodyShape(surface).BodyShape.ResizeCurrentShape(newW, newH, True);
    else raise exception.create('forgot to implement!');
  end;

  CreateCollisionBody;
end;

function TSurfaceDescriptor.IsContainedBy(const r: TRectF): boolean;
begin
  Result := r.Contains(surface.SurfaceToScene(PointF(0, 0))) and
            r.Contains(surface.SurfaceToScene(PointF(surface.Width, 0))) and
            r.Contains(surface.SurfaceToScene(PointF(surface.Width, surface.Height))) and
            r.Contains(surface.SurfaceToScene(PointF(0, surface.Height)));
end;

procedure TSurfaceDescriptor.DuplicateTo(aSurface: PSurfaceDescriptor);
begin
  aSurface^.parentID := parentID;
  aSurface^.name := name;
  aSurface^.textureName := textureName;
  aSurface^.classtype := classtype;
  if ParentList.ModeForLevelEditor
    then aSurface^.layerindex := FScene.LayerIndexOf(surface.ParentLayer)
    else aSurface^.zOrder := surface.ZOrderAsChild;

  aSurface^.width := surface.Width;
  aSurface^.height := surface.Height;
  aSurface^.frameindex := frameindex;
  aSurface^.blendmode := blendmode;
  // EXTRA properties
  // TSpriteWithElasticCorner
  aSurface^.TopLeftOffsetX := TopLeftOffsetX;
  aSurface^.TopLeftOffsetY := TopLeftOffsetY;
  aSurface^.TopRightOffsetX := TopRightOffsetX;
  aSurface^.TopRightOffsetY := TopRightOffsetY;
  aSurface^.BottomRightOffsetX := BottomRightOffsetX;
  aSurface^.BottomRightOffsetY := BottomRightOffsetY;
  aSurface^.BottomLeftOffsetX := BottomLeftOffsetX;
  aSurface^.BottomLeftOffsetY := BottomLeftOffsetY;
  // TQuad4Color
  aSurface^.TopLeftColor := TopLeftColor;
  aSurface^.TopRightColor := TopRightColor;
  aSurface^.BottomRightColor := BottomRightColor;
  aSurface^.BottomLeftColor := BottomLeftColor;
  // TGradientRectangle
  aSurface^.GradientData := GradientData;
  // TDeformationGrid
  aSurface^.DeformationGridData := DeformationGridData;
  // UI objects
  aSurface^.BodyShapeData := BodyShapeData;
  aSurface^.BackGradientData := BackGradientData;
  aSurface^.BGDarkenColor := BGDarkenColor;
  aSurface^.OnShowScenario := OnShowScenario;
  aSurface^.OnHideScenario := OnHideScenario;
  // surfaces with text (textured font)
  aSurface^.FontDescriptorName := FontDescriptorName;
  aSurface^.Caption := Caption;
  aSurface^.TextAlignment := TextAlignment;
  aSurface^.Checked := Checked;
  aSurface^.CheckShape := CheckShape;
  aSurface^.CheckFill := CheckFill;
  aSurface^.CheckColorChecked :=  CheckColorChecked;
  aSurface^.textureName2 := textureName2;
  aSurface^.CheckAdjustImageToFontHeight := CheckAdjustImageToFontHeight;
  aSurface^.AutoSize := AutoSize;
  aSurface^.uiOrientation := uiOrientation;
  aSurface^.scrollbarMin := scrollbarMin;
  aSurface^.scrollbarMax := scrollbarMax;
  aSurface^.scrollbarPageSize := scrollbarPageSize;
  aSurface^.scrollbarPosition := scrollbarPosition;
//  aSurface^.scrollbarCursorBodyShapeData := scrollbarCursorBodyShapeData;
//  aSurface^.scrollbarCursorGradientData := scrollbarCursorGradientData;
  aSurface^.ProgressBarGradientData := ProgressBarGradientData;
  aSurface^.ProgressBarReversed := ProgressBarReversed;
  aSurface^.ProgressBarPercent := ProgressBarPercent;
  aSurface^.scrollboxUseVScrollbar := scrollboxUseVScrollbar;
  aSurface^.scrollboxUseHScrollbar := scrollboxUseHScrollbar;
  aSurface^.listboxUnselectedTextColor := listboxUnselectedTextColor;
  aSurface^.listboxUnselectedGradientData := listboxUnselectedGradientData;
  aSurface^.listboxSelectedTextColor := listboxSelectedTextColor;
  aSurface^.listboxSelectedGradientData := listboxSelectedGradientData;
  aSurface^.listboxMultiSelect := listboxMultiSelect;
  aSurface^.listboxItemHeight := listboxItemHeight;
  aSurface^.TextAreaText := TextAreaText;
  aSurface^.TextAreaTextTint := TextAreaTextTint;

  aSurface^.CreateSurface(True);
  aSurface^.SetChildDependency;
  aSurface^.surface.SetCoordinate(surface.GetXY);
  aSurface^.surface.Pivot := surface.Pivot;
  aSurface^.surface.Angle.Value := surface.Angle.Value;
  aSurface^.surface.Scale.Value := surface.Scale.Value;
  if surface is TSprite then TSprite(aSurface^.surface).SetSize(surface.Width, surface.Height);
  aSurface^.surface.FlipH := surface.FlipH;
  aSurface^.surface.FlipV := surface.FlipV;
  aSurface^.surface.Opacity.Value := surface.Opacity.Value;
  aSurface^.surface.Tint.Value := surface.Tint.Value;
  aSurface^.surface.TintMode := surface.TintMode;
  aSurface^.surface.Frame := surface.Frame;
  aSurface^.surface.BlendMode := surface.BlendMode;
end;

function TSurfaceDescriptor.GetSurfaceType: classOfSimpleSurfaceWithEffect;
begin
  if Assigned(surface) then Result := classOfSimpleSurfaceWithEffect(surface.ClassType)
    else Result := classtype;
end;

function TSurfaceDescriptor.GetWidth: integer;
begin
  if Assigned(surface) then Result := surface.Width
    else Result := width;
end;

function TSurfaceDescriptor.GetHeight: integer;
begin
  if Assigned(surface) then Result := surface.Height
    else Result := height;
end;

function TSurfaceDescriptor.GetX: single;
begin
  if Assigned(surface) then Result := surface.X.Value
    else Result := x;
end;

function TSurfaceDescriptor.GetY: single;
begin
  if Assigned(surface) then Result := surface.Y.Value
    else Result := y;
end;

function TSurfaceDescriptor.GetLayerIndex: integer;
begin
  if Assigned(surface) then Result := FScene.LayerIndexOf(surface.ParentLayer)
    else Result := layerindex;
end;

function TSurfaceDescriptor.GetZOrder: integer;
begin
  if Assigned(surface) then Result := surface.ZOrderAsChild
    else Result := zorder;
end;

function TSurfaceDescriptor.GetPivotX: single;
begin
  if Assigned(surface) then Result := surface.Pivot.x
    else Result := pivotx;
end;

function TSurfaceDescriptor.GetPivotY: single;
begin
  if Assigned(surface) then Result := surface.Pivot.y
    else Result := pivoty;
end;

function TSurfaceDescriptor.GetAngle: single;
begin
  if Assigned(surface) then Result := surface.Angle.Value
    else Result := angle;
end;

function TSurfaceDescriptor.GetScaleX: single;
begin
  if Assigned(surface) then Result := surface.Scale.X.Value
    else Result := scalex;
end;

function TSurfaceDescriptor.GetScaleY: single;
begin
  if Assigned(surface) then Result := surface.Scale.Y.Value
    else Result := scaley;
end;

function TSurfaceDescriptor.GetFlipH: boolean;
begin
  if Assigned(surface) then Result := surface.FlipH
    else Result := fliph;
end;

function TSurfaceDescriptor.GetFlipV: boolean;
begin
  if Assigned(surface) then Result := surface.FlipV
    else Result := flipv;
end;

function TSurfaceDescriptor.GetOpacity: single;
begin
  if Assigned(surface) then Result := surface.Opacity.Value
    else Result := opacity;
end;

function TSurfaceDescriptor.GetTint: TBGRAPixel;
begin
  if Assigned(surface) then Result := surface.Tint.Value
    else Result := tint;
end;

function TSurfaceDescriptor.GetTintMode: TTintMode;
begin
  if Assigned(surface) then Result := surface.TintMode
    else Result := tintmode;
end;

function TSurfaceDescriptor.GetFrame: single;
begin
  if Assigned(surface) then Result := surface.Frame
    else Result := frameindex;
end;

function TSurfaceDescriptor.GetBlendmode: byte;
begin
  if Assigned(surface) then Result := surface.BlendMode
    else Result := blendmode;
end;

procedure TSurfaceDescriptor.SetSurfaceLayerIndex(aIndex: integer);
begin
  surface.MoveToLayer(aIndex);
end;


procedure TSurfaceDescriptor.AddOffsetToPivot(aOffset: TPointF);
var ppivot: TPointF;
begin
  with surface do
    ppivot := SurfaceToScene(PointF(Width*Pivot.x, Height*Pivot.y));
  ppivot := ppivot + aOffset;

  Pivot := ppivot;
end;

procedure TSurfaceDescriptor.UpdateHandlePosition;
begin
  HandleManager.UpdateHandlePosition(surface);
end;

procedure TSurfaceDescriptor.ToogleScaledAndRotatedHandle;
begin
  HandleManager.ToogleScaledAndRotatedHandle(surface);
end;

procedure TSurfaceDescriptor.ShowHandle;
begin
  HandleManager.ShowScaleHandle(surface);
end;

procedure TSurfaceDescriptor.HideHandle;
begin
  HandleManager.HideAll;
end;

function TSurfaceDescriptor.SaveToString: string;
var prop: TProperties;
  _className: String;
  isUISurface: Boolean;
begin
  prop.Init('~');
  prop.Add('ID', id);
  if parentID <> -1 then prop.Add('ParentID', parentID);

  if not ParentList.FModeForLevelEditor then begin
    if GetZOrder <> 0 then prop.Add('ZOrder', GetZOrder);
  end else prop.Add('LayerIndex', GetLayerIndex);

  if not ParentList.FModeForLevelEditor then
    prop.Add('Name', name);

  if not ParentList.FModeForLevelEditor then
    if classtype <> TSprite then
      prop.Add('Classtype', classtype.ClassName);

  if textureName <> '' then
    prop.Add('TextureName', textureName);
  if textureName2 <> '' then
    prop.Add('TextureName2', textureName2);

  if GetPivotX <> 0.5 then prop.Add('PivotX', GetPivotX);
  if GetPivotY <> 0.5 then prop.Add('PivotY', GetPivotY);
  if GetAngle <> 0 then prop.Add('Angle', GetAngle);
  if GetX <> 0.0 then prop.Add('X', GetX);
  if GetY <> 0.0 then prop.Add('Y', GetY);

  if not ParentList.FModeForLevelEditor then begin
    if GetScaleX <> 1.0 then prop.Add('ScaleX', GetScaleX);
    if GetScaleY <> 1.0 then prop.Add('ScaleY', GetScaleY);
  end;

  prop.Add('Width', GetWidth);
  prop.Add('Height', GetHeight);

  if GetFlipH then prop.Add('FlipH', GetFlipH);
  if GetFlipV then prop.Add('FlipV', GetFlipV);
  if GetOpacity <> 255 then prop.Add('Opacity', GetOpacity);
  if GetTint <> BGRA(0,0,0,0) then prop.Add('Tint', GetTint);
  if GetTintMode <> tmReplaceColor then prop.Add('TintMode', Ord(GetTintMode));
  if GetFrame <> 1.0 then prop.Add('FrameIndex', GetFrame);
  if GetBlendMode <> FX_BLEND_NORMAL then prop.Add('BlendMode', GetBlendMode);

  // TSpriteWithElasticCorner
  if GetSurfaceType = TSpriteWithElasticCorner then begin
    if TopLeftOffsetX <> 0 then prop.Add('TopLeftOffsetX', TopLeftOffsetX);
    if TopLeftOffsetY <> 0 then prop.Add('TopLeftOffsetY', TopLeftOffsetY);
    if TopRightOffsetX <> 0 then prop.Add('TopRightOffsetX', TopRightOffsetX);
    if TopRightOffsetY <> 0 then prop.Add('TopRightOffsetY', TopRightOffsetY);
    if BottomRightOffsetX <> 0 then prop.Add('BottomRightOffsetX', BottomRightOffsetX);
    if BottomRightOffsetY <> 0 then prop.Add('BottomRightOffsetY', BottomRightOffsetY);
    if BottomLeftOffsetX <> 0 then prop.Add('BottomLeftOffsetX', BottomLeftOffsetX);
    if BottomLeftOffsetY <> 0 then prop.Add('BottomLeftOffsetY', BottomLeftOffsetY);
  end;

  // TQuad4Color
  if GetSurfaceType = TQuad4Color then begin
    prop.Add('TopLeftColor', TopLeftColor);
    prop.Add('TopRightColor', TopRightColor);
    prop.Add('BottomRightColor', BottomRightColor);
    prop.Add('BottomLeftColor', BottomLeftColor);
  end;

  // TGradientRectangle
  if GetSurfaceType = TGradientRectangle then
    prop.Add('GradientData', GradientData);

  // TDeformationGrid
  if GetSurfaceType = TDeformationGrid then
    prop.Add('DeformationGridData', DeformationGridData);

  _className := GetSurfaceType.ClassName;
  isUISurface := (_className = 'TUIPanel') or (_className = 'TUIPanelWithEffects') or
                 (_className = 'TUIImage') or (_className = 'TUILabel') or
                 (_className = 'TUIButton') or (_className = 'TUICheck') or
                 (_className = 'TUIRadio') or (_className = 'TUIScrollBar') or
                 (_className = 'TUIProgressBar') or (_className = 'TUIScrollBox') or
                 (_className = 'TUIListBox') or (_className = 'TUITextArea');
  if isUISurface then begin
    // UI objects
    if BodyShapeData <> '' then begin
      prop.Add('BodyShapeData', BodyShapeData);
      if BackGradientData <> '' then prop.Add('BackGradientData', BackGradientData);
    end;
    if BGDarkenColor.alpha <> 0 then prop.Add('BGDarkenColor', BGDarkenColor);
    if OnShowScenario <> '' then  prop.Add('OnShowScenario', OnShowScenario);
    if OnHideScenario <> '' then  prop.Add('OnHideScenario', OnHideScenario);
    // UI objects with text (textured font)
    if FontDescriptorName <> '' then begin
      prop.Add('FontDescriptorName', FontDescriptorName);
      if Caption <> '' then prop.Add('Caption', Caption, True);
      prop.Add('TextAlignment', Ord(TextAlignment));
    end;
    // UI objects with check
    if (_className = 'TUICheck') or (_className = 'TUIRadio') then begin
      if Checked then prop.Add('Checked', Checked);
      prop.Add('CheckShape', Ord(CheckShape));
      prop.Add('CheckFill', Ord(CheckFill));
      prop.Add('CheckColorChecked', CheckColorChecked);
      if not CheckAdjustImageToFontHeight then
        prop.Add('AdjustImageToFontHeight', CheckAdjustImageToFontHeight);
    end;
    // autosize
    if not AutoSize then prop.Add('AutoSize', AutoSize);

    if _className = 'TUIScrollBar' then begin
      prop.Add('Orientation', Ord(uiOrientation));
      prop.Add('scrollbarMin', scrollbarMin);
      prop.Add('scrollbarMax', scrollbarMax);
      prop.Add('scrollbarPageSize', scrollbarPageSize);
      prop.Add('scrollbarPosition', scrollbarPosition);
//      prop.Add('scrollbarCursorBodyShapeData', scrollbarCursorBodyShapeData);
//      prop.Add('scrollbarCursorGradientData', scrollbarCursorGradientData);
    end;

    if _className = 'TUIProgressBar' then begin
      prop.Add('Orientation', Ord(uiOrientation));
      prop.Add('ProgressBarReversed', ProgressBarReversed);
      prop.Add('ProgressBarPercent', ProgressBarPercent);
      prop.Add('GradientData', ProgressBarGradientData);
    end;

    if _className = 'TUIScrollBox' then begin
      prop.Add('UseVScrollbar', scrollboxUseVScrollbar);
      prop.Add('UseHScrollbar', scrollboxUseHScrollbar);
    end;

    if _className = 'TUIListBox' then begin
      prop.Add('UnselectedTextColor', listboxUnselectedTextColor);
      prop.Add('UnselectedGradientData', listboxUnselectedGradientData);
      prop.Add('SelectedTextColor', listboxSelectedTextColor);
      prop.Add('SelectedGradientData', listboxSelectedGradientData);
      prop.Add('MultiSelect', listboxMultiSelect);
      prop.Add('ItemHeight', listboxItemHeight);
    end;

    if _className = 'TUITextArea' then begin
      prop.Add('Text', TextAreaText, True);
      prop.Add('TextTint', TextAreaTextTint);
    end;
  end;

  Result := prop.PackedProperty;
end;

procedure TSurfaceDescriptor.LoadFromString(const s: string);
var prop: TProperties;
  s1: string;
  v: integer;
  tex: PTexture;
begin
  s1 := '';
  v := 0;
  tex := NIL;
  InitDefault;
  prop.Split(s, '~');
  prop.IntegerValueOf('ID', id, -1);
  prop.IntegerValueOf('ParentID', parentID, -1);
  prop.IntegerValueOf('ZOrder', zOrder, 0);
  prop.IntegerValueOf('LayerIndex', layerindex, 0);

  prop.StringValueOf('Name', name, 'noname');
  prop.StringValueOf('Classtype', s1, 'TSprite');
  case s1 of
    'TSprite': classtype := TSprite;
    'TTiledSprite': classtype := TTiledSprite;
    'TSpriteWithElasticCorner': classtype := TSpriteWithElasticCorner;
    'TDeformationGrid': classtype := TDeformationGrid;
    'TScrollableSprite': classtype := TScrollableSprite;
    'TPolarSprite': classtype := TPolarSprite;
    'TShapeOutline': classtype := TShapeOutline;
    'TSpriteContainer': classtype := TSpriteContainer;
    'TQuad4Color': classtype := TQuad4Color;
    'TGradientRectangle': classtype := TGradientRectangle;
    'TFreeText': classtype := TFreeText;
    'TUIPanel': classtype := TUIPanel;
    'TUIPanelWithEffects': classtype := TUIPanelWithEffects;
    'TUILabel': classtype := TUILabel;
    'TUIImage': classtype := TUIImage;
    'TUIButton': classtype := TUIButton;
    'TUICheck': classtype := TUICheck;
    'TUIRadio': classtype := TUIRadio;
    'TUIScrollBar': classtype := TUIScrollBar;
    'TUIProgressBar': classtype := TUIProgressBar;
    'TUIScrollBox': classtype := TUIScrollBox;
    'TUIListBox': classtype := TUIListBox;
    'TUITextArea': classtype := TUITextArea;
    else exception.create('forgot to implement!');
  end;
  prop.StringValueOf('TextureName', textureName, textureName);
  if not ParentList.ModeForLevelEditor then
    tex := GetTextureFromTextureName;

  prop.SingleValueOf('PivotX', pivotX, 0.5);
  prop.SingleValueOf('PivotY', pivotY, 0.5);
  prop.SingleValueOf('Angle', angle, 0.0);
  prop.SingleValueOf('X', x, 0.0);
  prop.SingleValueOf('Y', y, 0.0);
  prop.SingleValueOf('ScaleX', scaleX, 1.0);
  prop.SingleValueOf('ScaleY', scaleY, 1.0);

  if not ParentList.ModeForLevelEditor then
    if tex <> NIL then v := tex^.FrameWidth else v := 100;
  prop.IntegerValueOf('Width', width, v);
  if not ParentList.ModeForLevelEditor then
    if tex <> NIL then v := tex^.FrameHeight else v := 100;
  prop.IntegerValueOf('Height', height, v);

  prop.BooleanValueOf('FlipH', flipH, False);
  prop.BooleanValueOf('FlipV', flipV, False);
  prop.SingleValueOf('Opacity', opacity, 255);
  prop.BGRAPixelValueOf('Tint', tint, BGRA(0,0,0,0));
  v := 0;
  prop.IntegerValueOf('TintMode', v, Ord(tmReplaceColor));
  tintmode := TTintMode(v);
  prop.SingleValueOf('FrameIndex', frameindex, 1.0);
  prop.ByteValueOf('BlendMode', blendmode, FX_BLEND_NORMAL);

  // TSpriteWithElasticCorner
  prop.SingleValueOf('TopLeftOffsetX', TopLeftOffsetX, 0);
  prop.SingleValueOf('TopLeftOffsetY', TopLeftOffsetY, 0);
  prop.SingleValueOf('TopRightOffsetX', TopRightOffsetX, 0);
  prop.SingleValueOf('TopRightOffsetY', TopRightOffsetY, 0);
  prop.SingleValueOf('BottomRightOffsetX', BottomRightOffsetX, 0);
  prop.SingleValueOf('BottomRightOffsetY', BottomRightOffsetY, 0);
  prop.SingleValueOf('BottomLeftOffsetX', BottomLeftOffsetX, 0);
  prop.SingleValueOf('BottomLeftOffsetY', BottomLeftOffsetY, 0);

  // TQuad4Color
  prop.BGRAPixelValueOf('TopLeftColor', TopLeftColor, BGRA(255,0,0));
  prop.BGRAPixelValueOf('TopRightColor', TopRightColor, BGRA(0,255,0));
  prop.BGRAPixelValueOf('BottomRightColor', BottomRightColor, BGRA(0,0,255));
  prop.BGRAPixelValueOf('BottomLeftColor', BottomLeftColor, BGRA(255,255,0));

  // TGradientRectangle
  prop.StringValueOf('GradientData', GradientData, '');

  // TDeformationGrid
  prop.StringValueOf('DeformationGridData', DeformationGridData, '');

  // UI objects
  prop.StringValueOf('BodyShapeData', BodyShapeData, '');
  prop.StringValueOf('BackGradientData', BackGradientData, '');
  prop.BGRAPixelValueOf('BGDarkenColor', BGDarkenColor, BGRA(0,0,0,100));
  prop.StringValueOf('OnShowScenario', OnShowScenario, '');
  prop.StringValueOf('OnHideScenario', OnHideScenario, '');

  // UI objects with text (textured font)
  prop.StringValueOf('FontDescriptorName', FontDescriptorName, '');
  prop.StringValueOf('Caption', Caption, '');
  prop.IntegerValueOf('TextAlignment', v, 0);
  TextAlignment := TOGLCAlignment(v);

  // UI objects with check
  prop.BooleanValueOf('Checked', Checked, False);
  prop.StringValueOf('TextureName2', textureName2, '');
  prop.IntegerValueOf('CheckShape', v, 1);
  CheckShape := TUICheckShape(v);
  prop.IntegerValueOf('CheckFill', v, 0);
  CheckFill := TUICheckFill(v);
  prop.BGRAPixelValueOf('CheckColorChecked', CheckColorChecked, BGRAWhite);
  prop.BooleanValueof('AdjustImageToFontHeight', CheckAdjustImageToFontHeight, True);
  // autosize
  prop.BooleanValueOf('AutoSize', AutoSize, True);
  // ScrollBar
  prop.IntegerValueOf('Orientation', v, 0);
  uiOrientation := TUIOrientation(v);
  prop.Int64ValueOf('scrollbarMin', scrollbarMin, 0);
  prop.Int64ValueOf('scrollbarMax', scrollbarMax, 100);
  prop.Int64ValueOf('scrollbarPageSize', scrollbarPageSize, 10);
  prop.Int64ValueOf('scrollbarPosition', scrollbarPosition, 0);
//  prop.StringValueOf('scrollbarCursorBodyShapeData', scrollbarCursorBodyShapeData, scrollbarCursorBodyShapeData);
//  prop.StringValueOf('scrollbarCursorGradientData', scrollbarCursorGradientData, scrollbarCursorGradientData);
  // ProgressBar
  prop.BooleanValueOf('ProgressBarReversed', ProgressBarReversed, False);
  prop.SingleValueOf('ProgressBarPercent', ProgressBarPercent, 0.0);
  prop.StringValueOf('GradientData', ProgressBarGradientData, ProgressBarGradientData);

  // scrollbox
  prop.BooleanValueOf('UseVScrollbar', scrollboxUseVScrollbar, True);
  prop.BooleanValueOf('UseHScrollbar', scrollboxUseHScrollbar, False);
  // listbox
  prop.BGRAPixelValueOf('UnselectedTextColor', listboxUnselectedTextColor, BGRA(220,220,220));
  prop.StringValueOf('UnselectedGradientData', listboxUnselectedGradientData, '');
  prop.BGRAPixelValueOf('SelectedTextColor', listboxSelectedTextColor, BGRAWhite);
  prop.StringValueOf('SelectedGradientData', listboxSelectedGradientData, '');
  prop.BooleanValueOf('MultiSelect', listboxMultiSelect, False);
  prop.IntegerValueOf('ItemHeight', listboxItemHeight, listboxItemHeight);

  // TUITextArea
  prop.StringValueOf('Text', TextAreaText, '');
  prop.BGRAPixelValueOf('TextTint', TextAreaTextTint, BGRA(220,220,220));
end;

// L=layerindex X,Y=coor W,H=size PX,PY=pivot A=angle O=opacity F=flip T=tint M=tintmode
// I=frameindex
function TSurfaceDescriptor.ExportToPascalString(aTextureList: TTextureList): string;
var prop: TProperties;
  f: integer;
  texItem: PTextureItem;
begin
  prop.Init(',');
  prop.Add('L', layerindex-APP_LAYER_COUNT);

  if IsTextured then begin
    texItem := aTextureList.GetItemByName(textureName);
    prop.Add('N', ExtractFilename(texItem^.filename));
  end;
  prop.Add('X', x);
  prop.Add('Y', y);
  prop.Add('W', width);
  prop.Add('H', height);
  if pivotx <> 0.5 then prop.Add('PX', pivotx);
  if pivoty <> 0.5 then prop.Add('PY', pivoty);
  if angle <> 0 then prop.Add('A', angle);
  if opacity <> 255 then prop.Add('O', opacity);
  f := 0;
  if fliph then inc(f);
  if flipv then inc(f, 2);
  if f <> 0 then prop.Add('F', f);
  if tint <> BGRA(0,0,0,0) then prop.Add('T', tint);
  if tintmode <> tmReplaceColor then prop.Add('M', Ord(tintmode));
  if frameindex <> 1.0 then prop.Add('I', frameindex);
  Result := prop.PackedProperty;
end;

{ TSurfaceList }

function TSurfaceList.NextID: integer;
begin
  inc(FID);
  Result := FID;
end;

function TSurfaceList.GetSortedSurfaceFromNearestTofurthest: ArrayOfPSurfaceDescriptor;
  procedure AddToResult(item: PSurfaceDescriptor);
  begin
    if item = NIL then exit;
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := item;
  end;
  procedure CheckRecursive(aSurface: TSimpleSurfaceWithEffect);
  var i: integer;
    o: TSimpleSurfaceWithEffect;
    parentIsRegistered: boolean=False;
    item: PSurfaceDescriptor;
  begin
    parentIsRegistered := False;
    for i:=aSurface.ChildCount-1 downto 0 do begin
      o := aSurface.Childs[i];
      // TUIClickableAndScrollableWithBodyShape can have TUIScrollBarOwned childs -> this produce a bug
      // we ignore them
      if o is TUIScrollBarOwned then continue;
      //if not(o is TUIScrollBarOwned) then begin
        if not parentIsRegistered and (o.ZOrderAsChild < 0) then begin
          parentIsRegistered := True;
          item := GetItemBySurface(aSurface);
          if item <> NIL then AddToResult(item);
        end;
        if o.ChildCount > 0 then CheckRecursive(o);

        AddToResult(GetItemBySurface(o));
      //end;
    end;
    if not parentIsRegistered and not (aSurface is TUIScrollBarOwned) then begin
      item := GetItemBySurface(aSurface);
      if item <> NIL then AddToResult(item);
    end;
  end;
  procedure CopySurfaceList;
  var i, j: SizeUInt;
  begin
    SetLength(Result, Size);
    j := Size;
    for i:=0 to Size-1 do begin
      dec(j);
      Result[i] := Mutable[j];
    end;
  end;

begin
  Result := NIL;
  if Size = 0 then exit;
  if FModeForLevelEditor then CopySurfaceList
    else begin
      if RootIsDefined then
        CheckRecursive(GetRootItem^.surface);
    end;
end;

function TSurfaceList.GetTextures: TTextureList;
begin
  if Assigned(FTextures) then Result := FTextures
    else if Assigned(FOnGetTexture) then Result := FOnGetTexture()
      else raise exception.create('forgot to inialize either the property, either the callback to retrieve the texture list');
end;

procedure TSurfaceList.MoveItemTo(aCurrentIndex, aNewindex: integer);
var o: TSurfaceDescriptor;
begin
  if aCurrentIndex = aNewindex then exit;
  if (aCurrentIndex >= Size) or (aNewindex >= Size) then begin
    Raise exception.Create('index out of bounds');
    exit;
  end;

  o := Mutable[aCurrentIndex]^;
  Erase(aCurrentIndex);
  Insert(aNewindex, o);
end;

function TSurfaceList.ItemsDescriptorToArrayOfID(aItems: ArrayOfPSurfaceDescriptor): TArrayOfInteger;
var i: integer;
begin
  Result := NIL;
  SetLength(Result, Length(aItems));
  for i:=0 to High(Result) do
    Result[i] := aItems[i]^.id;
end;

constructor TSurfaceList.Create;
begin
  inherited Create;
  FUndoRedoManager := TSurfaceUndoRedoManager.Create;

  FOnGetTexture := NIL;
  FTextures := NIL;
end;

destructor TSurfaceList.Destroy;
begin
  FUndoRedoManager.Free;
  FUndoRedoManager := NIL;
  inherited Destroy;
end;

procedure TSurfaceList.SetModeForLevelEditor;
begin
  FModeForLevelEditor := True;
end;

procedure TSurfaceList.Clear;
var i: SizeUInt;
begin
  if Size > 0 then
    for i:=0 to Size-1 do
      Mutable[i]^.KillSurface;
  inherited Clear;
  FID := 0;
  FUndoRedoManager.Clear;
end;

function TSurfaceList.AddEmpty: PSurfaceDescriptor;
var o: TSurfaceDescriptor;
begin
  o.InitDefault;
  o.ParentList := Self;
  o.id := NextID;
  PushBack(o);
  Result := Mutable[Size-1];
end;

function TSurfaceList.GetItemByID(aID: integer): PSurfaceDescriptor;
var i: SizeUInt;
begin
  Result := NIL;
  if Size = 0 then exit;

  for i:=0 to Size-1 do
    if Mutable[i]^.id = aID then begin
      Result := Mutable[i];
      exit;
    end;
end;

function TSurfaceList.GetItemIndexByID(aID: integer): integer;
var i: SizeUInt;
begin
  Result := -1;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.id = aID then begin
      Result := i;
      exit;
    end;
end;

function TSurfaceList.GetByIndex(aIndex: integer): PSurfaceDescriptor;
begin
  Result := NIL;
  if Size = 0 then exit;
  Result := Mutable[aIndex];
end;

function TSurfaceList.GetItemBySurface(aSurface: TSimpleSurfaceWithEffect): PSurfaceDescriptor;
var i: SizeUInt;
begin
  Result := NIL;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.surface = aSurface then begin
      Result := Mutable[i];
      exit;
    end;
end;

function TSurfaceList.GetItemsThatUseThisTexture(aTextureItem: PTextureItem): ArrayOfPSurfaceDescriptor;
var i: SizeUInt;
begin
  Result := NIL;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.textureName = aTextureItem^.name then begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := Mutable[i];
    end;
end;

function TSurfaceList.GetRootItem: PSurfaceDescriptor;
var i: SizeUInt;
begin
  Result := NIL;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.parentID = -1 then begin
      Result := Mutable[i];
      exit;
    end;
end;

function TSurfaceList.RootIsDefined: boolean;
begin
  Result := GetRootItem <> NIL;
end;

function TSurfaceList.GetItemsAt(aX, aY: integer): ArrayOfPSurfaceDescriptor;
var collisionPoint: TOGLCBodyItem;
  sorted: ArrayOfPSurfaceDescriptor;
  i: integer;
  o: TSimpleSurfaceWithEffect;
begin
  Result := NIL;
  if Size = 0 then exit;

  collisionPoint.BodyType := _btPoint;
  collisionPoint.pt := PointF(aX, aY);

  sorted := GetSortedSurfaceFromNearestTofurthest;
  for i:=0 to High(sorted) do begin
    o := sorted[i]^.surface;
    // do not return surfaces on layer not visible
    if ModeForLevelEditor and not o.ParentLayer.Visible then continue;
    o.CollisionBody.SetTransformMatrix(o.GetMatrixSurfaceToScene);
    if o.CollisionBody.CheckCollisionWith(collisionPoint) then begin
      SetLength(Result, length(Result)+1);
      Result[High(Result)] := sorted[i];
    end;
  end;
end;

function TSurfaceList.NameExists(const aName: string): boolean;
var i: SizeUInt;
begin
  Result := False;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then exit(True);
end;

function TSurfaceList.TextureNameisUsedByASurface(const aTextureName: string): boolean;
var i: SizeUInt;
begin
  Result := False;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.textureName = aTextureName then exit(True);
end;

function TSurfaceList.UseTexture: boolean;
var i: SizeUInt;
begin
  Result := False;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.IsTextured then exit(True);
end;

procedure TSurfaceList.DeleteItemByID(aID: integer);
var i: integer;
begin
  i := GetItemIndexByID(aID);
  if i <> -1 then begin
    Mutable[i]^.KillSurface;
    Self.Erase(i);
    ScreenSpriteBuilder.Postures.DeleteValueEntryInEachPosture(i);
  end;
end;

function TSurfaceList.DuplicateItemsByID(aItems: ArrayOfPSurfaceDescriptor): ArrayOfPSurfaceDescriptor;
var i: integer;
  ids: array of integer;
begin
  Result := NIL;
  if Length(aItems) = 0 then exit;

  // retrieve the item's id to duplicate
  ids := ItemsDescriptorToArrayOfID(aItems);

  SetLength(Result, Length(aItems));
  for i:=0 to High(aItems) do begin
    Result[i] := AddEmpty;
    GetItemByID(ids[i])^.DuplicateTo(Result[i]);
  end;
end;

function TSurfaceList.DuplicateAndShiftItemsByID(
  aItems: ArrayOfPSurfaceDescriptor; aCoeffLeft, aCoeffRight, aCoeffTop,
  aCoeffBottom, aOverlapValue: single): ArrayOfPSurfaceDescriptor;
var i: integer;
  ids: array of integer;
begin
  Result := NIL;
  if Length(aItems) = 0 then exit;

  // retrieve the item's id to duplicate
  ids := ItemsDescriptorToArrayOfID(aItems);

  SetLength(Result, Length(aItems));
  for i:=0 to High(aItems) do begin
    Result[i] := AddEmpty;
    with GetItemByID(ids[i])^ do begin
      DuplicateTo(Result[i]);
      Result[i]^.surface.X.Value := Result[i]^.surface.X.Value - (Result[i]^.surface.Width-aOverlapValue) * aCoeffLeft;
      Result[i]^.surface.X.Value := Result[i]^.surface.X.Value + (Result[i]^.surface.Width-aOverlapValue) * aCoeffRight;
      Result[i]^.surface.Y.Value := Result[i]^.surface.Y.Value - (Result[i]^.surface.Height-aOverlapValue) * aCoeffTop;
      Result[i]^.surface.Y.Value := Result[i]^.surface.Y.Value + (Result[i]^.surface.Height-aOverlapValue) * aCoeffBottom;
    end;
  end;
end;

function TSurfaceList.GetItemsIndexesByIDSortedSmallToHigh(
  aItems: ArrayOfPSurfaceDescriptor): TArrayOfInteger;
var i: SizeUInt;
  k: integer;
  flag: Boolean;
begin
  Result := NIL;
  if Size = 0 then exit;
  if Length(aItems) = 0 then exit;

  SetLength(Result, Length(aItems));
  for i:=0 to High(Result) do
    Result[i] := GetItemIndexByID(aItems[i]^.id);
  if Length(Result) = 1 then exit;

  // sort the index array from small to high
  repeat
    flag := False;
    for i:=0 to High(Result)-1 do
      if Result[i] > Result[i+1] then begin
        k := Result[i];
        Result[i] := Result[i+1];
        Result[i+1] := k;
        flag := True;
      end;
  until not flag;
end;

function TSurfaceList.GetIndexesForALayer(aLayer: TLayer): TArrayOfInteger;
var i: SizeUInt;
begin
  Result := NIL;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.surface.ParentLayer = aLayer then
      Result.Add(i);
end;

procedure TSurfaceList.ShiftItemToTopOneStep(aItemIndex: integer);
begin
  if Size = 0 then exit;
  if aItemIndex = Size-1 then exit;
  MoveItemTo(aItemIndex, aItemIndex+1);
end;

procedure TSurfaceList.ShiftItemToBackOneStep(aItemIndex: integer);
begin
  if Size = 0 then exit;
  if aItemIndex = 0 then exit;
  MoveItemTo(aItemIndex, aItemIndex-1);
end;

procedure TSurfaceList.SelectNone;
var i: SizeUInt;
begin
  if Size > 0 then
   for i:=0 to Size-1 do
     Mutable[i]^.Selected := False;
end;

function TSurfaceList.GetItemsBounds: TRectF;
var i: SizeUInt;
  xmin, xmax, ymin, ymax, xx, yy, w, h: single;
begin
  if Size = 0 then begin
    Result := RectF(0, 0, 0, 0);
    exit;
  end;

  xmin := MaxSingle;
  yMin := MaxSingle;
  xmax := MinSingle;
  ymax := MinSingle;
  for i:=0 to Size-1 do begin
    with Mutable[i]^.surface do begin
      xx := X.Value;
      yy := Y.Value;
      w := Width;
      h := Height;
    end;
    if xx < xmin then xmin := xx;
    if yy < ymin then ymin := yy;
    if xx+w > xmax then xmax := xx + w;
    if yy+h > ymax then ymax := yy + h;
  end;

  Result := RectF(PointF(xmin, ymin), PointF(xmax, ymax));
end;

procedure TSurfaceList.SetValuesFromTemporaryVariables;
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    Mutable[i]^.SetValuesFromTemporaryVariables;
end;

procedure TSurfaceList.CopySurfaceValuesToTemporaryVariables;
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    Mutable[i]^.DuplicateValuesToTemporaryVariables;
end;

function TSurfaceList.SaveToString: string;
var prop: TProperties;
  i: SizeUInt;
begin
  prop.Init('|');
  prop.Add('Count', integer(Size));
  if Size > 0 then
    for i:=0 to Size-1 do
      prop.Add('Surface'+i.ToString, Mutable[i]^.SaveToString);
  Result := prop.PackedProperty;
end;

procedure TSurfaceList.LoadFromString(const s: string; aCreateSurfaces: boolean);
var prop: TProperties;
  i: SizeUInt;
  c: integer;
  s1: string;
  o: TSurfaceDescriptor;
begin
  Clear;
  prop.Split(s, '|');
  c := 0;
  s1 := '';
  prop.IntegerValueOf('Count', c, 0);
  if c = 0 then exit;

  for i:=0 to c-1 do
    if prop.StringValueOf('Surface'+i.ToString, s1, '') then begin
      o.ParentList := Self;
      o.LoadFromString(s1);
      PushBack(o);
    end;

  // update FID
  for i:=0 to Size-1 do
    with Mutable[i]^ do
      if FID < id then FID := id;

  if not aCreateSurfaces then exit;

  // create the surfaces and init its values
  for i:=0 to Size-1 do
    with Mutable[i]^ do begin
      CreateSurface;
      SetValuesFromTemporaryVariables;
    end;

  // create the childs dependencies
  for i:=0 to Size-1 do
    with Mutable[i]^ do
      SetChildDependency;
end;

{const SPRITE_BUILDER_SURFACES_SECTION = '[SPRITE_BUILDER_SURFACES]';
procedure TSurfaceList.SaveTo(t: TStringList);
begin
  t.Add(SPRITE_BUILDER_SURFACES_SECTION);
  t.Add(SaveToString);
end;

procedure TSurfaceList.LoadFrom(t: TStringList);
var k: integer;
begin
  Clear;
  k := t.IndexOf(SPRITE_BUILDER_SURFACES_SECTION);
  if (k = -1) or (k = t.Count-1) then exit;
  LoadFromString(t.Strings[k+1]);
end;  }

procedure TSurfaceList.FillComboBox(aCB: TComboBox);
var i: SizeUInt;
begin
  aCB.Clear;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    aCB.Items.Add(Mutable[i]^.id.ToString);
end;

procedure TSurfaceList.ReplaceNameInComboBox(aCB: TComboBox);
var i: integer;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    aCB.Items.strings[i] := Mutable[i]^.id.ToString;
end;

procedure TSurfaceList.FillListBox(aLB: TListBox);
var i: SizeUInt;
begin
  aLB.Clear;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    aLB.Items.Add(Mutable[i]^.name);
end;

function TSurfaceList.AddMainPanel(const aName: string; aWidth, aHeight: integer): TUIPanelWithEffects;
var o: PSurfaceDescriptor;
  bodyshape: TBodyShape;
begin
  o := AddEmpty;
  o^.name := aName;
  o^.classtype := TUIPanelWithEffects;
  o^.width := aWidth;
  o^.height := aHeight;
  o^.parentID := -1;
  o^.layerindex := WorkingLayer;

  bodyshape.InitDefault(FScene);
  bodyShape.Fill.Color := BGRA(30,15,7);
  bodyshape.SetShapeRectangle(aWidth, aHeight, 3);
  o^.BodyShapeData := bodyshape.SaveToString;
  o^.BackGradientData := '';
  o^.BGDarkenColor := BGRAPixelTransparent;
  o^.CreateSurface(True);
  o^.SetChildDependency;
  //o^.surface.CenterOnScene;
  //o^.x := o^.surface.X.Value;
  //o^.y := o^.surface.Y.Value;
  Result := TUIPanelWithEffects(o^.surface);
end;

end.

