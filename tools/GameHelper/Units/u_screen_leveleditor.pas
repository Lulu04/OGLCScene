unit u_screen_leveleditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_screen_template,
  u_texture_list, u_ui_handle, u_surface_list;

type

{ TLevelEditorSurfaceList }

TLevelEditorSurfaceList = class(TSurfaceList)
  function Textures: TTextureList; override;
end;

{ TScreenLevelEditor }

TScreenLevelEditor = class(TCustomScreenTemplate)
private
  FSelected: ArrayOfPSurfaceDescriptor;
  FAlternateOverlappedIndex: integer;  // used to chose between several overlapped objects
  FScaleHandleType: TScaleHandle;
  function GetSelectedCount: integer;
  procedure AddToSelected(aItems: ArrayOfPSurfaceDescriptor);
  function AlreadySelected(aItems: ArrayOfPSurfaceDescriptor): boolean; overload;
  function AlreadySelected(item: PSurfaceDescriptor): boolean; overload;
  procedure AddOnlyTheFirstToSelected(aItems: ArrayOfPSurfaceDescriptor);
  procedure AddOffsetCoordinateToSelection(aOffset: TPointF);
  procedure AddOffsetToPivotOnSelection(aOffset: TPointF);
  procedure SaveCurrentAngleBeforeRotationOnSelection;
  procedure RotateSelection(aPreviousReferencePoint, aReferencePoint: TPointF; aUseIncrement: boolean);
  procedure SaveCurrentScaleValueBeforeScalingSelection;
  procedure ScaleSelection(aDelta: TPointF; aKeepAspectRatio: boolean);
  procedure DeleteSelection;
  procedure UpdateHandlePositionOnSelected;
  function MouseIsOverPivotHandle(aWorldPt: TPointF): boolean;
  function MouseIsOverRotateHandle(aWorldPt: TPointF): boolean;
  function MouseIsOverScaleHandle(aWorldPt: TPointF): boolean;
private
  procedure LoopMoveSelection;
  procedure LoopMovePivotOnSelection;
private
  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseWheel({%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean); override;
  procedure ProcessKeyUpForChild(var Key: Word; {%H-}Shift: TShiftState); override;
public
  Surfaces: TLevelEditorSurfaceList;
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  procedure SelectNone;
  procedure ToogleScaledAndRotatedHandleOnSelection;
  procedure MoveSelection(aDelta: TPointF);
  procedure SetPositionOnSelection(aX, aY: single);
  procedure SetSizeOnSelection(aValue: boolean);
  procedure SetAngleOnSelection(aAngle: single);
  procedure SetFlipHOnSelection(aValue: boolean);
  procedure SetFlipVOnSelection(aValue: boolean);
  procedure SetOpacityOnSelection(aValue: single);
  procedure SetTintOnSelection(aValue: TBGRAPixel);
  procedure SetTintModeOnSelection(aValue: TTintMode);
end;

var ScreenLevelEditor: TScreenLevelEditor;

implementation

uses u_levelbank;

{ TLevelEditorSurfaceList }

function TLevelEditorSurfaceList.Textures: TTextureList;
begin
  Result := LevelBank.Textures;
end;

{ TScreenLevelEditor }

procedure TScreenLevelEditor.CreateObjects;
begin
  ShowLayers([LAYER_UI, LAYER_LEVELEDITOR]);
  // camera
  CreateCamera([LAYER_LEVELEDITOR]);
end;

procedure TScreenLevelEditor.FreeObjects;
begin
  //SelectNone;
  FreeCamera;
end;

procedure TScreenLevelEditor.Initialize;
begin
  Surfaces := TLevelEditorSurfaceList.Create;
  Surfaces.WorkingLayer := LAYER_LEVELEDITOR;
end;

procedure TScreenLevelEditor.Finalize;
begin
  Surfaces.Free;
  Surfaces := NIL;
end;

procedure TScreenLevelEditor.SelectNone;
begin

end;

end.

