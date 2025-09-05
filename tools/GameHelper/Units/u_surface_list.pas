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
  textureName: string;   // the filename+extension without path
  classtype: classOfSimpleSurfaceWithEffect;
  // temporary variables used when loading. They keep safe the original values
  pivotX, pivotY, angle, x, y, scaleX, scaleY: single;
  zOrder: integer;
  flipH, flipV: boolean;
  opacity: single;
  tint: TBGRAPixel;
  tintMode: TTintMode;
  width, height: integer;
  procedure InitDefault;
  procedure KillSurface;
  procedure CreateCollisionBody;
  procedure CreateSurface(aCreateUIHandle: boolean=True);
  procedure RecreateSurfaceBecauseTextureChanged;
  procedure SetChildDependency;
  function GetTextureFromTextureName: PTexture;

  procedure SaveAndRemoveChilds;
  procedure RestoreChilds;

  procedure SetValuesFromTemporaryVariables; // sets x, y, pivot,...
  procedure DuplicateValuesToTemporaryVariables;

  function IsRoot: boolean;

  function IsOverScaleHandle(aWorldPt: TPointF; out aType: TScaleHandle): boolean;
  function IsOverRotateHandle(aWorldPt: TPointF): boolean;
  function IsOverPivotHandle(aWorldPt: TPointF): boolean;
  procedure AddOffsetToPivot(aOffset: TPointF); // in world coordinates
  procedure UpdateHandlePosition;
  procedure ToogleScaledAndRotatedHandle;

  procedure SaveCurrentAngleBeforeRotation;
  procedure ComputeAngle(aPreviousReferencePointInWorld,
    aReferencePointInWorld: TPointF; aUseIncrement: boolean);
  procedure SaveCurrentScaleValueBeforeScaling;
  procedure ComputeScale(aType: TScaleHandle; aDelta: TPointF; aKeepAspectRatio: boolean);
  procedure SaveCurrentPosAndSizeBeforeResizing;
  procedure AddDeltaToSpriteSize(aType: TScaleHandle; aDelta: TPoint; aKeepAspectRatio: boolean);

  property Selected: boolean read GetSelected write SetSelected;
  property Pivot: TPointF read GetPivot write SetPivot;

  function SaveToString: string;
  procedure LoadFromString(const s: string);

end;
PSurfaceDescriptor = ^TSurfaceDescriptor;
ArrayOfPSurfaceDescriptor = array of PSurfaceDescriptor;

{$define _INTERFACE}
{$I u_surface_undoredo.inc}
{$undef _INTERFACE}

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

  // Returns the items located at the specified position.
  // Items are sorted from nearest to furthest away.
  function GetItemsAt(aX, aY: integer): ArrayOfPSurfaceDescriptor;

  function NameExists(const aName: string): boolean;
  function TextureNameisUsedByASurface(const aTextureName: string): boolean;

  procedure DeleteItemByID(aID: integer);

  procedure SelectNone;

  procedure SetValuesFromTemporaryVariables; // sets x, y, pivot,... on all surfaces
  procedure CopySurfaceValuesToTemporaryVariables;

  function SaveToString: string;
  procedure LoadFromString(const s: string);
{  procedure SaveTo(t:TStringList);
  procedure LoadFrom(t:TStringList);  }

  // items are the ID of each TSurfaceDescriptor
  procedure FillComboBox(aCB: TComboBox);
  procedure ReplaceNameInComboBox(aCB: TComboBox);
  procedure FillListBox(aLB: TListBox);

  function Textures: TTextureList; virtual; abstract;

  property WorkingLayer: integer read FWorkingLayer write FWorkingLayer;
  property UndoRedoManager: TSurfaceUndoRedoManager read FUndoRedoManager;

  property ModeForLevelEditor: boolean read FModeForLevelEditor;
end;


implementation

uses u_common, u_screen_spritebuilder, Math;

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
  HandleManager.ShowPivotHandle(surface);
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
  tint := BGRA(0,0,0,0);
  tintmode := tmReplaceColor;
  opacity := 255;

  classtype := TSimpleSurfaceWithEffect;
  HandleManager.InitDefault;
end;

procedure TSurfaceDescriptor.KillSurface;
begin
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
var //texItem: PTextureItem;
  tex: PTexture;
begin
{  texItem := ParentList.Textures.GetItemByName(textureName);
  if texItem <> NIL then tex := texItem^.texture else tex := NIL;  }
  tex := GetTextureFromTextureName;

  if classType = TSprite then
begin
    surface := TSprite.Create(tex, False);
    if ParentList.FModeForLevelEditor then
      TSprite(surface).SetSize(width, height);
//fscene.LogDebug('created sprite from texture "'+tex^.Filename+'" w='+tex^.FrameWidth.ToString+' h='+tex^.FrameHeight.ToString+ 'id='+tex^.ID.ToString);
end
  else
  if classType = TSpriteWithElasticCorner then
    surface := TSpriteWithElasticCorner.Create(tex, False)
  else
  if classType = TTiledSprite then
    surface := TTiledSprite.Create(tex, False)
  else
  if classType = TPolarSprite then
    surface := TPolarSprite.Create(tex, False)
  else
  if classType = TScrollableSprite then
    surface := TScrollableSprite.Create(tex, False)
  else
  if classType = TShapeOutline then
    surface := TShapeOutline.Create(FScene)
  else
  if classType = TGradientRectangle then
    surface := TGradientRectangle.Create(FScene)
  else
  if classType = TQuad4Color then begin
    surface := TQuad4Color.Create(FScene);
    TQuad4Color(surface).SetSize(10, 10);
  end else
  if classType = TDeformationGrid then
    surface := TDeformationGrid.Create(tex, False)
  else
  if classType = TSpriteContainer then begin
    surface := TSpriteContainer.Create(FScene);
    TSpriteContainer(surface).ShowOrigin := True;
fscene.LogDebug('created sprite_container');
  end
  else raise exception.create('forgot to implement!');

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
  if parentID = -1 then FScene.Add(surface, ParentList.WorkingLayer)
  else begin
      parentItem := ParentList.GetItemByID(parentID);
      if parentItem = NIL
        then raise exception.create('parent with ID='+parentID.ToString+' not found !')
        else surface.SetChildOf(parentItem^.surface, zOrder);
  end;
end;

function TSurfaceDescriptor.GetTextureFromTextureName: PTexture;
var texItem: PTextureItem;
  tex: PTexture;
begin
  texItem := ParentList.Textures.GetItemByName(textureName);
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
end;

function TSurfaceDescriptor.IsRoot: boolean;
begin
  Result := parentID = -1;
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

procedure TSurfaceDescriptor.AddDeltaToSpriteSize(aType: TScaleHandle;
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
  TSprite(surface).SetSize(newW, newH);
  CreateCollisionBody;
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

function TSurfaceDescriptor.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('~');
  prop.Add('ID', id);
  {if parentID <> -1 then} prop.Add('ParentID', parentID);

  if not ParentList.FModeForLevelEditor then
  {if surface.ZOrderAsChild <> 0 then} prop.Add('ZOrder', surface.ZOrderAsChild);

  if not ParentList.FModeForLevelEditor then
    prop.Add('Name', name);

  if not ParentList.FModeForLevelEditor then
  {if classtype <> TSprite then} prop.Add('Classtype', classtype.ClassName);

  prop.Add('TextureName', textureName);
  {if surface.Pivot.x <> 0.5 then} prop.Add('PivotX', surface.Pivot.x);
  {if surface.Pivot.y <> 0.5 then} prop.Add('PivotY', surface.Pivot.y);
  {if surface.Angle.Value <> 0 then} prop.Add('Angle', surface.Angle.Value);
  prop.Add('X', surface.X.Value);
  prop.Add('Y', surface.Y.Value);

  if not ParentList.FModeForLevelEditor then begin
    {if surface.Scale.X.Value <> 1.0 then} prop.Add('ScaleX', surface.Scale.X.Value);
    {if surface.Scale.Y.Value <> 1.0 then} prop.Add('ScaleY', surface.Scale.Y.Value);
  end else begin
    prop.Add('Width', surface.Width);
    prop.Add('Height', surface.Height);
  end;

  {if surface.FlipH then} prop.Add('FlipH', surface.FlipH);
  {if surface.FlipV then} prop.Add('FlipV', surface.FlipV);
  {if surface.Opacity.Value <> 255 then} prop.Add('Opacity', surface.Opacity.Value);
  {if surface.Tint.Value <> BGRA(0,0,0,0) then} prop.Add('Tint', surface.Tint.Value);
  {if surface.TintMode <> tmReplaceColor then} prop.Add('TintMode', Ord(surface.TintMode));
  Result := prop.PackedProperty;
end;

procedure TSurfaceDescriptor.LoadFromString(const s: string);
var prop: TProperties;
  s1: string;
  v: integer;
begin
  s1 := '';
  InitDefault;
  prop.Split(s, '~');
  prop.IntegerValueOf('ID', id, id);
  prop.IntegerValueOf('ParentID', parentID, -1);
  prop.IntegerValueOf('ZOrder', zOrder, 0);
  prop.StringValueOf('Name', name, 'noname');
  prop.StringValueOf('Classtype', s1, 'TSprite');
  case s1 of
    'TSprite': classtype := TSprite;
    'TDeformationGrid': classtype := TDeformationGrid;
    'TSpriteContainer': classtype := TSpriteContainer;
    else exception.create('forgot to implement!');
  end;
  prop.StringValueOf('TextureName', textureName, textureName);

  prop.SingleValueOf('PivotX', pivotX, 0.5);
  prop.SingleValueOf('PivotY', pivotY, 0.5);
  prop.SingleValueOf('Angle', angle, 0.0);
  prop.SingleValueOf('X', x, 0.0);
  prop.SingleValueOf('Y', y, 0.0);
  prop.SingleValueOf('ScaleX', scaleX, 1.0);
  prop.SingleValueOf('ScaleY', scaleY, 1.0);

  if ParentList.FModeForLevelEditor then begin
    prop.IntegerValueOf('Width', width, 100);
    prop.IntegerValueOf('Height', height, 100);
  end;

  prop.BooleanValueOf('FlipH', flipH, False);
  prop.BooleanValueOf('FlipV', flipV, False);
  prop.SingleValueOf('Opacity', opacity, 255);
  prop.BGRAPixelValueOf('Tint', tint, BGRA(0,0,0,0));
  v := 0;
  prop.IntegerValueOf('TintMode', v, Ord(tmReplaceColor));
  tintmode := TTintMode(v);
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
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := item;
  end;
  procedure CheckRecursive(aSurface: TSimpleSurfaceWithEffect);
  var i: integer;
    o: TSimpleSurfaceWithEffect;
    parentIsRegistered: boolean=False;
  begin
    parentIsRegistered := False;
    for i:=aSurface.ChildCount-1 downto 0 do begin
      o := aSurface.Childs[i];
      if not parentIsRegistered and (o.ZOrderAsChild < 0) then begin
        parentIsRegistered := True;
        AddToResult(GetItemBySurface(aSurface));
      end;
      if o.ChildCount > 0 then CheckRecursive(o);
      AddToResult(GetItemBySurface(o))
    end;
    if not parentIsRegistered then
      AddToResult(GetItemBySurface(aSurface));
  end;
  procedure CopySurfaceList;
  var i: SizeUInt;
  begin
    SetLength(Result, Size);
    for i:=0 to Size-1 do
      Result[i] := Mutable[i];
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

constructor TSurfaceList.Create;
begin
  inherited Create;
  FUndoRedoManager := TSurfaceUndoRedoManager.Create;
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

procedure TSurfaceList.SelectNone;
var i: SizeUInt;
begin
  if Size > 0 then
   for i:=0 to Size-1 do
     Mutable[i]^.Selected := False;
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

procedure TSurfaceList.LoadFromString(const s: string);
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

end.

