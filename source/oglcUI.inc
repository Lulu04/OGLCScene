{$ifdef oglcINTERFACE}
type
TUIClickableObject = class;

const
{$if defined(Darwin)}
  CTRLKEY = ssMeta;
{$else}
  CTRLKEY = ssCtrl;
{$endif}
type

TMouseEvent = Controls.TMouseEvent;
TMouseWheelEvent = Controls.TMouseWheelEvent;
TMouseButton = Controls.TMouseButton;


TOGLCMouseEvent = procedure(Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

{ TMouseManager }

TMouseManager = class
private
  FMousePoolEnabled: boolean;
  FOnClickOnScene: TOGLCMouseEvent;
  FSurfaceUnderMouse,
  FSurfaceLockedByMouse: TUIClickableObject;
  FSystemMouseCursorVisible: boolean;
  FMouseButtonState: array[TMouseButton] of boolean;
  function GetButtonState(btn: TMouseButton): boolean;
  procedure SetMousePoolEnabled(AValue: boolean);
  procedure SetSystemMouseCursorVisible(AValue: boolean);
  function CheckIfPosIsInParentClientArea(aChild: TSimpleSurfaceWithEffect; const aScenePt: TPoint): boolean;
  procedure ProcessOnMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ProcessOnMouseUp(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
  procedure ProcessOnMouseMove(Shift: TShiftState; X, Y: Integer);
  procedure ProcessOnMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
protected
  FSprite: TSprite;
  FClickPointOffset: TPointF; // in pixel
  FParentScene: TOGLCScene;
  FMousePos: TPoint;
  procedure PrepareBeforeUpdate;
  procedure UpDate(const AElapsedTime: single);
  procedure Draw;
public
  constructor Create(aParentScene: TOGLCScene);
  destructor Destroy; override;

  procedure SetCursorSprite(aSprite: TSprite; aClickPointOffset: TPointF); overload;
  procedure SetCursorSprite(aTexture: PTexture; aTextureOwner: boolean; aClickPointOffset: TPointF); overload;
  procedure DeleteCursorSprite;

  function IsOver(aGUISurface: TUIClickableObject): boolean; overload;
  function IsOver(aRect: TRect): boolean; overload;

  property ButtonState[btn: TMouseButton]: boolean read GetButtonState;

  property MousePoolEnabled: boolean read FMousePoolEnabled write SetMousePoolEnabled;
  // the current surface under the mouse. NIL if none.
  property SurfaceUnder: TUIClickableObject read FSurfaceUnderMouse;
  // the current surface locked by the mouse. NIL if none.
  property SurfaceLocked: TUIClickableObject read FSurfaceLockedByMouse write FSurfaceLockedByMouse;
  property MouseSprite: TSprite read FSprite; // for accessing all fx methods and property of TSprite
  property Position: TPoint read FMousePos; // in scene coordinates
  // Fired when a mouse button is released and:
  //    - mouse is not clicking on instance of an TUIClickableObject descendant
  //    - mouse is not over an instance of TUIClickableObject descendant
  property OnClickOnScene: TOGLCMouseEvent read FOnClickOnScene write FOnClickOnScene;
  property SystemMouseCursorVisible: boolean read FSystemMouseCursorVisible write SetSystemMouseCursorVisible;
end;

TUIEvent = procedure(Sender: TSimpleSurfaceWithEffect) of object;
TUIAnimCallBack = procedure(Sender: TSimpleSurfaceWithEffect) of object;
TAnchorType = (atNone, atAnchorToSurface, atAnchorToParent);
THAnchorType = (haNone, haLeft, haCenter, haRight);
TVAnchorType = (vaNone, vaTop, vaCenter, vaBottom);
THAnchor = record
  AnchorType: TAnchorType;
  SelfSide, TargetSide: THAnchorType;
  TargetSurface: TSimpleSurfaceWithEffect;
  Margin: integer;
end;
TVAnchor = record
  AnchorType: TAnchorType;
  SelfSide, TargetSide: TVAnchorType;
  TargetSurface: TSimpleSurfaceWithEffect;
  Margin: integer;
end;

{ TUIClickableObject }

TUIClickableObject = class(TSimpleSurfaceWithEffect)
private
  FMouseInteractionEnabled: boolean;
  FMouseEntered,
  FStateDown: boolean;
  FOnMouseLeave,
  FOnMouseEnter,
  FOnClick: TUIEvent;
  FOnMouseDown: TMouseEvent;
  FOnMouseUp: TMouseEvent;
  FOnAnimClick: TUIAnimCallBack;
  FOnAnimMouseDown: TUIAnimCallBack;
  FOnAnimMouseEnter: TUIAnimCallBack;
  FOnAnimMouseLeave: TUIAnimCallBack;
  FOnAnimMouseUp: TUIAnimCallBack;
private
  procedure CheckMouseEnterOrLeave;
  procedure SetMouseInteractionEnabled(AValue: boolean); virtual;
private
  FOnMouseMove: TMouseMoveEvent;
  FOnMouseWheel: TMouseWheelEvent;
private
  FHAnchor: THAnchor;
  FVAnchor: TVAnchor;
  procedure DoPositionAnchor;
private
  FScrollOffset: TPoint;
  procedure SetScrollOffset(AValue: TPoint);
  // offset when the surface is a child of a TUIClickableAndScrollableWithBodyShape parent.
  property ScrollOffset: TPoint read FScrollOffset write SetScrollOffset;
public
  procedure DoOnMouseEnter; virtual;
  procedure DoOnMouseLeave; virtual;
  procedure DoOnMouseClick; virtual;
  procedure DoOnMouseDown(aButton: TMouseButton; aShift: TShiftState; aPt: TPoint); virtual;
  procedure DoOnMouseUp(aButton: TMouseButton; aShift: TShiftState; aPt: TPoint); virtual;
  procedure DoOnMouseMove(aShift: TShiftState; aPt: TPoint); virtual;
  procedure DoOnMouseWheel(aShift: TShiftState; aWheelDelta: Integer; aMousePos: TPoint; var aHandled: Boolean); virtual;
public
  procedure Update(const aElapsedTime: single); override;
public
  constructor Create(aParentScene: TOGLCScene);

  // Convert the surface coordinates to scene coordinates.
  function SurfaceToSceneUI(aPt: TPoint): TPoint;
  // Convert scene coordinates to surface coordinates.
  function SceneToSurfaceUI(aPt: TPoint): TPoint;
  // Convert surface coordinates to a parent coordinates. You can select the parent.
  // If aParent is set to NIL, the ParentSurface property is used.
  // if ParentSurface property is NIL, the function returns aPt.
  function SurfaceToParentUI(aPt: TPoint; aParent: TSimpleSurfaceWithEffect=NIL): TPoint;
  // Convert a parent coordinates to surface coordinates. You can select the parent.
  // If aParent is set to NIL, the ParentSurface property is used.
  // if ParentSurface property is NIL, the function returns aPt.
  function ParentToSurfaceUI(aPt: TPoint; aParent: TSimpleSurfaceWithEffect=NIL): TPoint;

  // Callback to customize the animation of the surface when the mouse enters
  property OnAnimMouseEnter: TUIAnimCallBack read FOnAnimMouseEnter write FOnAnimMouseEnter;
  // Callback to customize the animation of the surface when the mouse leaves
  property OnAnimMouseLeave: TUIAnimCallBack read FOnAnimMouseLeave write FOnAnimMouseLeave;
  // Callback to customize the animation of the surface when the user clicks on it
  property OnAnimClick: TUIAnimCallBack read FOnAnimClick write FOnAnimClick;
  // Callback to customize the animation of the surface when the user press a mouse button
  property OnAnimMouseDown: TUIAnimCallBack read FOnAnimMouseDown write FOnAnimMouseDown;
  // Callback to customize the animation of the surface when the user release a mouse button
  property OnAnimMouseUp: TUIAnimCallBack read FOnAnimMouseUp write FOnAnimMouseUp;

  // Callback fired when the mouse enter the surface
  property OnMouseEnter: TUIEvent read FOnMouseEnter write FOnMouseEnter;
  // Callback fired when the mouse leaves the surface
  property OnMouseLeave: TUIEvent read FOnMouseLeave write FOnMouseLeave;
  // Callback fired when user click on the surface
  property OnClick: TUIEvent read FOnClick write FOnClick;
  // Callback fired when user press a mouse button on the srface
  property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
  // Callback fired when user release a mouse button on the surface
  property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  // Callback fired when user moves the mouse on the surface
  property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
  // Callback fired when user use the mouse wheel on the surface
  property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;

  // True if the mouse cursor is currently over the widget.
  property MouseIsOver: boolean read FMouseEntered;

  // Enable mouse interaction with the widget. Default is True.
  property MouseInteractionEnabled: boolean read FMouseInteractionEnabled write SetMouseInteractionEnabled;

public // anchor utils - only the position, do not change the size - H for Horizontal, V for Vertical
  // Anchor the surface horizontally to its parent. if there is no parent, the surface is anchored to the scene.
  procedure AnchorHPosToParent(aHSelfSide, aHTargetSide: THAnchorType; aHMargin: integer);
  // Anchor the surface vertically to its parent. if there is no parent, the surface is anchored to the scene.
  procedure AnchorVPosToParent(aVSelfSide, aVTargetSide: TVAnchorType; aVMargin: integer);
  // Anchor the surface both horizontally and vertically to its parent. if there is no parent, the surface is anchored to the scene.
  procedure AnchorPosToParent(aHSelfSide, aHTargetSide: THAnchorType; aHMargin: integer;
                              aVSelfSide, aVTargetSide: TVAnchorType; aVMargin: integer);

  // Anchor the surface horizontally to another surface.
  procedure AnchorHPosToSurface(aSurface: TSimpleSurfaceWithEffect; aHSelfSide, aHTargetSide: THAnchorType; aHMargin: integer);
  // Anchor the surface vertically to another surface.
  procedure AnchorVPosToSurface(aSurface: TSimpleSurfaceWithEffect; aVSelfSide, aVTargetSide: TVAnchorType; aVMargin: integer);
  // Anchor the surface both horizontally and vertically to another surface.
  procedure AnchorPosToSurface(aSurface: TSimpleSurfaceWithEffect;
                               aHSelfSide, aHTargetSide: THAnchorType; aHMargin: integer;
                               aVSelfSide, aVTargetSide: TVAnchorType; aVMargin: integer);
end;

{ TBodyShape }

TBodyShape = record
private type TShapeType=(stRectangle, stRoundRect, stEllipse, stCustom);
private
  FOnShapeChanged: TOGLCEvent;
  FMaskEventOnShapeChanged: boolean;
  FParentScene: TOGLCScene;
  FShapetype: TShapeType;
  FRoundRectOptions: TRoundRectangleOptions;
  FRx, FRy: single;
  procedure SetShape(aBorderPath, aFillPath: TOGLCPath; aWidth, aHeight: integer; aBorderWidth: single);
public // utils for Game Helper
  function SaveToString: string;
  procedure LoadFromString(const data: string)
public
  Border: TOGLCBorder;
  Fill: TOGLCFill;
  Width, Height: integer;
  procedure InitDefault(aParentScene: TOGLCScene);
  procedure SetShapeRectangle(aWidth, aHeight: integer; aBorderWidth: single);
  procedure SetShapeRoundRect(aWidth, aHeight: integer; aRx, aRy, aBorderWidth: single; aOptions: TRoundRectangleOptions=[]);
  procedure SetShapeEllipse(aWidth, aHeight: integer; aBorderWidth: single);
  procedure SetCustomShape(const aPath: TOGLCPath; aBorderWidth: single);
  procedure ResizeCurrentShape(aNewWidth, aNewHeight: integer; aFireEventOnShapeChanged: boolean);
  procedure DrawBorder(const aComputedTint: TColorF; aOpacity: single);
  procedure DrawBackground(const aComputedTint: TColorF; aOpacity: single; aForceDrawForStencilBuffer: boolean);
  property OnShapeChanged: TOGLCEvent read FOnShapeChanged write FOnShapeChanged;
end;

{ TUIClickableWithBodyShape }

TUIClickableWithBodyShape = class(TUIClickableObject)
private const MARGIN_BETWEEN_BORDER_AND_CAPTION = 2;
private
  FChildClippingEnabled: boolean;
  FClientArea: TRect;
  procedure ProcessCallbackSomethingChangeTheSize; virtual;
  procedure ProcessBackGradientColorChange;
  procedure UpdateWidgetSizeAndLayout; virtual;
protected
  FAutoSize,
  FNeedToRecomputeSize,
  FNeedLayout: boolean;
  function Margin: integer; virtual;
  function ComputeWidgetSizeToContain(w, h: integer): TSize;
  procedure ComputeClientArea;
  procedure SetAutoSize(AValue: boolean);
  function GetWidth: integer; override;
  function GetHeight: integer; override;
  procedure DrawBorder(const aComputedTint: TColorF; aOpacity: single);
  procedure DrawBackground(const aComputedTint: TColorF; aOpacity: single);
  // Return True if the surface is visible on the scene or on its parent.
  // False if the parent is not visible or the surface is not in the client area of the parent.
  //function MouseCanInteract: boolean;
public
  // Allow to control the shape of the widget and give access to Border and Fill objects.
  BodyShape: TBodyShape;
  // This optional gradient is drawn after the BodyShape fill object (the background).
  // It allow you to customize the background of the widget with a multicolor gradient
  BackGradient: TGradientDescriptor;
  procedure Draw(const aLayerPercentOpacity: single); override;
  procedure DoDraw; override;
  constructor Create(aParentScene: TOGLCScene);
  destructor Destroy; override;

  // Enable clipping (using stencil buffer) when draw the child surfaces. Default is True.
  property ChildClippingEnabled: boolean read FChildClippingEnabled write FChildClippingEnabled;
  // The client area of the surface
  property ClientArea: TRect read FClientArea;
  property AutoSize: boolean read FAutoSize write SetAutoSize;
end;

{ TUIPanel }

TUIPanel = class(TUIClickableWithBodyShape);


{ TUICaptionDescriptor }

TUICaptionDescriptor = record
private
  FAlignment: TOGLCAlignment;
  FCallbackSizeChanged: TOGLCEvent;
  FTexturedFont: TTexturedFont;
  FCaption: UTF8String;
  FCharIndexes: TTexturedCharIndexes;
  FComputedSize: TSize;
  procedure InitDefault;
  procedure SetAlignment(AValue: TOGLCAlignment);
  procedure SetCaption(AValue: UTF8String);
  procedure SetTexturedFont(AValue: TTexturedFont);
  procedure UpdateCharIndexesAndComputeSize;
public
  procedure Init(const aCaption: UTF8String; aTexturedFont: TTexturedFont; aAlignment: TOGLCAlignment);
  function CanBeDrawn: boolean;
  // return the location in the TRect according to Alignment
  function ComputeLocation(const aRect: TRect): TPointF;
  property Caption: UTF8String read FCaption write SetCaption;
  property TexturedFont: TTexturedFont read FTexturedFont write SetTexturedFont;
  property Size: TSize read FComputedSize; // gives the caption size in pixel
  property Alignment: TOGLCAlignment read FAlignment write SetAlignment;
  // fired when caption or Textured font change.
  property OnSizeChanged: TOGLCEvent read FCallbackSizeChanged write FCallbackSizeChanged;
end;


{ TUILabel }
// a widget to show text without size constraint.
TUILabel = class(TUIClickableObject)
private
  function GetCaption: string;
  procedure SetCaption(AValue: string);
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  CaptionDescriptor: TUICaptionDescriptor;
  procedure DoDraw; override;
  constructor Create(aParentScene: TOGLCScene; const aCaption: string; aFont: TTexturedFont);
  // Convenient property to allow direct access to property CaptionDescriptor.Caption
  property Caption: string read GetCaption write SetCaption;
end;

{ TUIImage }
// a widget to show an image (from texture) in a rectangular area.
TUIImage = class(TSprite)
private
  FAutoSize, FKeepProportion, FCentered: boolean;
  FDrawOffset: TPointF;
  FRectToRender: TRect;
  procedure SetAutoSize(AValue: boolean);
  procedure SetCentered(AValue: boolean);
  procedure SetKeepProportion(AValue: boolean);
  procedure SetTexture(AValue: PTexture); override;
  procedure AdjustImageInClientRect;
private
  FCallbackTextureChanged: TOGLCEvent;
  FCallbackSizeChanged: TOGLCEvent;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure Draw(const aLayerPercentOpacity: single);  override;
public
  constructor Create(aParentScene: TOGLCScene; aTexture: PTexture; aWidth, aHeight: integer);
  procedure SetSize(aWidth, aHeight: integer);
  property AutoSize: boolean read FAutoSize write SetAutoSize;
  property KeepProportion: boolean read FKeepProportion write SetKeepProportion;
  property Centered: boolean read FCentered write SetCentered;
  property OnTextureChanged: TOGLCEvent read FCallbackTextureChanged write FCallbackTextureChanged;
  property OnSizeChanged: TOGLCEvent read FCallbackSizeChanged write FCallbackSizeChanged;
end;

{ TUIButton }
// a Button with an image and caption
TUIButton = class(TUIClickableWithBodyShape)
private const DEFAULT_MARGIN_BETWEEN_IMAGE_AND_CAPTION = 3;
private
  FLabel: TUILabel;
  FImage: TUIImage;
  FImageMargin: integer;
  function GetCaption: string;
  procedure SetCaption(AValue: string);
  procedure UpdateWidgetSizeAndLayout; override;
  procedure SetMouseInteractionEnabled(AValue: boolean); override;
  procedure DoAnim_OnMouseEnter(Sender: TSimpleSurfaceWithEffect);
  procedure DoAnim_OnMouseLeave(Sender: TSimpleSurfaceWithEffect);
  procedure DoAnim_OnMouseDown(Sender: TSimpleSurfaceWithEffect);
  procedure DoAnim_OnMouseUp(Sender: TSimpleSurfaceWithEffect);
  procedure DoAnim_OnClick(Sender: TSimpleSurfaceWithEffect);
public
  constructor Create(aParentScene: TOGLCScene; const aCaption: string; aFont: TTexturedFont; aTexture: PTexture);
  property Image: TUIImage read FImage;
  property _Label: TUILabel read FLabel;
  // Convenient property to allow direct access to property _Label.CaptionDescriptor.Caption
  property Caption: string read GetCaption write SetCaption;
  // the margin in pixel between the image and the caption. Default value is 3 pixels.
  property ImageMargin: integer read FImageMargin write FImageMargin;
end;


TUICheckShape = (ctNone, ctRectangle, ctRoundRect, ctCircle, ctUseTexture);
TUICheckFill = (cfColor, cfCross);

{ TUICheck }
// a check box with caption. The check box style can be customized.
TUICheck = class(TUIButton)
private
  FCheckShapeType: TUICheckShape;
  FCheckFillType: TUICheckFill;
  FCheckBorder: TOGLCBorder;
  FCheckFill: TOGLCFill;
  FCheckShapeSize: TSize;
  FCheckShapeLocation: TPointF;
  FCheckCross: TArrayOfOGLCPath;
  FColorChecked: TBGRAPixel;
  FOnAnimChange: TUIAnimCallback;
  FTextureUncheck, FTextureCheck: PTexture;
  FChecked: boolean;
  FOnChange: TUIEvent;
  procedure SetChecked(AValue: boolean); virtual;
  procedure SetColorChecked(AValue: TBGRAPixel);
  procedure UpdateWidgetSizeAndLayout; override;
public
  procedure DoOnMouseClick; override;
  procedure DoDraw; override;
  constructor Create(aParentScene: TOGLCScene; const aCaption: string; aFont: TTexturedFont);

  // Customize the check with predefined shape and fill.
  // The shape can be ctRectangle, ctRoundRect or ctCircle.
  // The fill mode can be cfColor or cfCross.
  procedure CustomizeCheckBox(aShape: TUICheckShape; aFill: TUICheckFill); overload;
  // customize the check box with 2 textures. If aAdjustImageToFontHeight is True,
  // the size of the rendered textures is reduced or increased to match with the font height.
  procedure CustomizeCheckBox(aTextureUncheck, aTextureCheck: PTexture; aAdjustImageToFontHeight: boolean=True); overload;
  procedure CustomizeWithoutCheckBox;

  property Checked: boolean read FChecked write SetChecked;
  // The color used when the checkbox is checked: to fill it or to draw the cross.
  // Ignored if textures are used.
  property ColorChecked: TBGRAPixel read FColorChecked write SetColorChecked;
  // Fired when the check box state is changed by user.
  property OnChange: TUIEvent read FOnChange write FOnChange;
  // callback to animate the control when its checked state change
  property OnAnimChange: TUIAnimCallback read FOnAnimChange write FOnAnimChange;
end;

{ TUIRadio }
// a radio button with caption. The check box style can be customized.
// Only one radio at a time can be checked (true) on the same parent or on the scene.
TUIRadio = class(TUICheck)
private
  procedure InternalSetChecked(aValue: boolean);
  procedure UncheckOtherOnParent;
  procedure SetChecked(AValue: boolean); override;
public
  procedure DoOnMouseClick; override;
  constructor Create(aParentScene: TOGLCScene; const aCaption: string; aFont: TTexturedFont);
end;


// to specify the orientation of the widgets that can be oriented (TUIScrollBar, TUIProgressBar,...)
TUIOrientation = (uioHorizontal, uioVertical);

{ TUIScrollBar }
// a classic scroll bar. Mouse wheel to move the cursor by step.
TUIScrollBar = class(TUIClickableWithBodyShape)
private
  FOnChange: TUIEvent;
  FOrientation: TUIOrientation;
  FMin, FMax: ptrint;
  FPageSize: ptrint;
  FPosition: ptrint;
  FMouseWheelDeltaValue: ptrint;
  FSliderPosToDraw: TPoint;
  FClickOrigin, FSliderPosToDrawWhenClicked: integer;
  FSliderClickedByMouse: boolean;
  FInertie: TBoundedFParam;
  FInertieSign: TValueSign;
  function GetPercent: single;
  procedure ResizeSlider;
  procedure ComputeSliderPosToDraw;
  function ClientAreaPosToPositionValue(aXY: integer): ptrint;
  procedure SetMouseWheelDeltaValue(AValue: ptrint);
  procedure UpdateWidgetSizeAndLayout; override;
  procedure SetMax(AValue: ptrint);
  procedure SetMin(AValue: ptrint);
  procedure SetPageSize(AValue: ptrint);
  procedure SetPosition(AValue: ptrint);
  procedure DoAnim_OnMouseEnter(Sender: TSimpleSurfaceWithEffect);
  procedure DoAnim_OnMouseLeave(Sender: TSimpleSurfaceWithEffect);
protected
  function Margin: integer; override;
public
  procedure DoOnMouseEnter; override;
  procedure DoOnMouseLeave; override;
  procedure DoOnMouseDown(aButton: TMouseButton; aShift: TShiftState; aPt: TPoint); override;
  procedure DoOnMouseUp(aButton: TMouseButton; aShift: TShiftState; aPt: TPoint); override;
  procedure DoOnMouseMove(aShift: TShiftState; aPt: TPoint); override;
  procedure DoOnMouseWheel(aShift: TShiftState; aWheelDelta: Integer; aMousePos: TPoint; var aHandled: Boolean); override;
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
public
  // Allow to control the shape of the slider.
  SliderShape: TBodyShape;
  constructor Create(aParentScene: TOGLCScene; aOrientation: TUIOrientation);
  destructor Destroy; override;

  procedure SetParams(aPosition, aMin, aMax: ptrint); overload;
  procedure SetParams(aPosition, aMin, aMax, aPageSize: ptrint); overload;

  property Min: ptrint read FMin write SetMin;
  property Max: ptrint read FMax write SetMax;
  property PageSize: ptrint read FPageSize write SetPageSize;
  property Position: ptrint read FPosition write SetPosition;
  property PositionPercent: single read GetPercent; // the position of the srcollbar expressed in range [0..1]
  property MouseWheelDeltaValue: ptrint read FMouseWheelDeltaValue write SetMouseWheelDeltaValue;
  property OnChange: TUIEvent read FOnChange write FOnChange;
end;


{ TUIProgressBar }
// a classic progress bar.

TUIProgressBar = class(TUIClickableWithBodyShape)
private
  FOrientation: TUIOrientation;
  FPercent: single;
  FFillForStencil: TOGLCFill;
  FReversed: boolean;
  procedure SetPercent(AValue: single);
  procedure SetReversed(AValue: boolean);
  procedure UpdateWidgetSizeAndLayout; override;
protected
  function Margin: integer; override;
public
  procedure DoDraw; override;
public
  // Use this object to define the color used to fill the progress.
  // You can define a simple color or a complex gradient.
  Gradient: TGradientDescriptor;
  // aOrientation can be uioHorizontal or uioVertical
  constructor Create(aParentScene: TOGLCScene; aOrientation: TUIOrientation);
  property Orientation: TUIOrientation read FOrientation write FOrientation;
  property Percent: single read FPercent write SetPercent; // [0..1]
  property Reversed: boolean read FReversed write SetReversed;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TUIRadio }

procedure TUIRadio.InternalSetChecked(aValue: boolean);
begin
  FChecked := AValue;
  if FCheckShapeType = ctUseTexture then begin
    if FChecked then FImage.Texture := FTextureCheck
      else FImage.Texture := FTextureUncheck;
    UpdateWidgetSizeAndLayout;
  end;
end;

procedure TUIRadio.UncheckOtherOnParent;
var L, i: integer;
   o: TUIRadio;
   s: TSimpleSurfaceWithEffect;
begin
  if FParentSurface <> NIL then begin
    // the control have a parent -> uncheck the other radio on this parent
    for i:=0 to FParentSurface.ChildCount-1 do
     if FParentSurface.Childs[i] is TUIRadio then begin
       o := FParentSurface.Childs[i] as TUIRadio;
       if o.FChecked and (o <> Self) then begin
         o.InternalSetChecked(FALSE);
         if o.FOnAnimChange <> NIL then o.FOnAnimChange(o);
       end;
     end;
  end else begin
    // the control is on the scene -> uncheck other radio without a parent
  for L:=0 to FParentScene.LayerCount-1 do
   for i:=0 to FParentScene.Layer[L].SurfaceCount-1 do begin
     s := FParentScene.Layer[L].Surface[i];
     if (s is TUIRadio) and (s.FParentSurface = NIL) and (s <> Self) then begin
       o := s as TUIRadio;
       if o.FChecked then begin
         o.InternalSetChecked(FALSE);
         if o.FOnAnimChange <> NIL then o.FOnAnimChange(o);
       end;
     end;
   end;
  end;
end;

procedure TUIRadio.SetChecked(AValue: boolean);
begin
  if FChecked = AValue then Exit;
  UncheckOtherOnParent;

  InternalSetChecked(AValue);
  if FOnAnimChange <> NIL then FOnAnimChange(Self);
end;

procedure TUIRadio.DoOnMouseClick;
begin
  if FChecked then exit;

  SetChecked(True);
  if FOnAnimClick <> NIL then FOnAnimClick(Self);
  if FOnClick <> NIL then FOnClick(Self);
  if FOnAnimChange <> NIL then FOnAnimChange(Self);
  if FOnChange <> NIL then FOnChange(self);
end;

constructor TUIRadio.Create(aParentScene: TOGLCScene; const aCaption: string; aFont: TTexturedFont);
begin
  inherited Create(aParentScene, aCaption, aFont);
  BodyShape.SetShapeRectangle(30,10,0);
  BodyShape.Fill.Visible := False;

  FCheckFill.CenterColor := BGRA(255,255,255);
  FCheckFill.Color := BGRA(64,64,64);
  FColorChecked := BGRA(255,255,255);
  CustomizeCheckBox(ctCircle, cfColor);
end;

{ TUICheck }

procedure TUICheck.SetChecked(AValue: boolean);
begin
  if FChecked = AValue then Exit;
  FChecked := AValue;

  if FCheckShapeType = ctUseTexture then begin
    if FChecked then FImage.Texture := FTextureCheck
      else FImage.Texture := FTextureUncheck;
    UpdateWidgetSizeAndLayout;
  end;

  if FOnAnimChange <> NIL then FOnAnimChange(Self);
end;

procedure TUICheck.DoOnMouseClick;
begin
  SetChecked(not FChecked);
  inherited DoOnMouseClick;
  if FOnChange <> NIL then FOnChange(self);
end;

procedure TUICheck.SetColorChecked(AValue: TBGRAPixel);
begin
  if FColorChecked = AValue then Exit;
  FColorChecked := AValue;
  FCheckFill.Color := AValue;
end;

procedure TUICheck.UpdateWidgetSizeAndLayout;
var newWidgetSize: TSize;
   w, h: integer;
begin
  if FNeedToRecomputeSize then begin
    FNeedToRecomputeSize := False;
    if FCheckShapeType = ctUseTexture then begin
      w := FImage.Width + FImageMargin + FLabel.Width;
      h := Max(FImage.Height, FLabel.Height);
    end else if FCheckShapeType <> ctNone then begin
      w := FCheckShapeSize.cx + FImageMargin + FLabel.Width;
      h := Max(FCheckShapeSize.cy, FLabel.Height);
    end else begin
      w := FLabel.Width;
      h := FLabel.Height;
    end;
    newWidgetSize := ComputeWidgetSizeToContain(w, h);
    BodyShape.ResizeCurrentShape(newWidgetSize.cx, newWidgetSize.cy, False);
    FNeedLayout := True;
  end;

  if FNeedLayout then begin
    FNeedLayout := False;
    ComputeClientArea;
    if FCheckShapeType = ctUseTexture then begin
      FImage.SetCoordinate(ClientArea.Left, (ClientArea.Top+ClientArea.Height-FImage.Height)*0.5);
      // we force label aligned with the bottom of the image
      FLabel.X.Value := ClientArea.Left + FImage.Width + FImageMargin;
      FLabel.BottomY := FImage.BottomY;
    end else if FCheckShapeType <> ctNone then begin
      FCheckShapeLocation := PointF(ClientArea.Left, (ClientArea.Top+ClientArea.Height-FCheckShapeSize.cy)*0.5);
      // we force label aligned with the bottom of the check shape
      FLabel.X.Value := ClientArea.Left + FCheckShapeSize.cx + FImageMargin;
      FLabel.BottomY := FCheckShapeLocation.y + FCheckShapeSize.cy;
    end else begin
      FLabel.X.Value := ClientArea.Left;
      FLabel.BottomY := FCheckShapeLocation.y;
    end;
  end;
end;

procedure TUICheck.DoDraw;
var p: pointer;
begin
  if (FCheckShapeType = ctUseTexture) or (FCheckShapeType = ctNone) then exit;

  FParentScene.ModelViewMatrix.Translate(FCheckShapeLocation.x, FCheckShapeLocation.y);
  // draw checkbox background
  if FChecked then begin
    case FCheckFillType of
      cfColor: begin
        FParentScene.ThreeColorTriangleRenderer.Prepare(ptTriangleFan, FParentScene.MVPMatrix, FComputedOpacity,
                                                        FComputedTint, FBlendMode);
        FParentScene.ThreeColorTriangleRenderer.PushIndices(FCheckFill.FIndices);
        p := FParentScene.ThreeColorTriangleRenderer.QueryVertex(FCheckFill.VertexCount);
        FCheckFill.CopyVertexTo(p);
        FParentScene.ThreeColorTriangleRenderer.Draw;
      end;
      cfCross: begin
        FParentScene.SmoothLineRenderer.PushPaths(FCheckCross, lpMiddle, FColorChecked, FCheckBorder.Width,
                                            FComputedTint, FComputedOpacity, True);
      end;
    end;
  end;

  // draw checkbox border
  FParentScene.SmoothLineRenderer.DrawBorder(FCheckBorder, FComputedOpacity, FComputedTint);

  FParentScene.ModelViewMatrix.Translate(-FCheckShapeLocation.x, -FCheckShapeLocation.y);
end;

constructor TUICheck.Create(aParentScene: TOGLCScene; const aCaption: string; aFont: TTexturedFont);
begin
  inherited Create(aParentScene, aCaption, aFont, NIL);

  BodyShape.SetShapeRectangle(30,10,0);
  BodyShape.Fill.Visible := False;

  FColorChecked := BGRA(255,255,255);
  CustomizeCheckBox(ctRectangle, cfColor);

  // no default animation
  FOnAnimMouseEnter := NIL;
  FOnAnimMouseLeave := NIL;
  FOnAnimMouseDown := NIL;
  FOnAnimMouseUp := NIL;
  FOnAnimClick := NIL;
end;

procedure TUICheck.CustomizeCheckBox(aShape: TUICheckShape; aFill: TUICheckFill);
var borderPath, fillPath: TOGLCPath;
   w: integer;
   lineWidth, v: single;
   rectCross: TRectF;
begin
  if aShape = ctUseTexture then raise exception.create('You can not use ''ctUseTexture''. Use instead the other ''CustomizeCheckBox'' method to set texture');

  FCheckShapeType := aShape;
  FCheckFillType := aFill;
  w := Max(Round(_Label.CaptionDescriptor.FTexturedFont.Font.FontHeight), FParentScene.ScaleDesignToScene(6));
  FCheckShapeSize.cx := w;
  FCheckShapeSize.cy := w;
  lineWidth := Max(2.5, w*0.19);
  borderPath := NIL;
  fillPath := NIL;
  FCheckCross := NIL;
  SetLength(FCheckCross, 2);
  case aShape of
    ctRectangle: begin
      borderPath.CreateRect(0, 0, w, w, True);
      fillPath.CreateRect(0, 0, w-2, w-2, False);
      rectCross := RectF(lineWidth*0.5, lineWidth*0.5, w-lineWidth, w-lineWidth);
      FCheckCross[0].CreateLine(rectCross.TopLeft, rectCross.BottomRight);
      FCheckCross[1].CreateLine(PointF(rectCross.Left, rectCross.Bottom), PointF(rectCross.Right, rectCross.Top));
    end;
    ctRoundRect: begin
      v := Max(2, w*0.3);
      borderPath.CreateRoundRect(0, 0, w, w, v, v, [], True);
      fillPath.CreateRoundRect(0, 0, w-2, w-2, v, v, [], False);
      v := v*0.5;
      rectCross := RectF(v, v, w-v-lineWidth*0.5, w-v-lineWidth*0.5);
      FCheckCross[0].CreateLine(rectCross.TopLeft, rectCross.BottomRight);
      FCheckCross[1].CreateLine(PointF(rectCross.Left, rectCross.Bottom), PointF(rectCross.Right, rectCross.Top));
    end;
    ctCircle: begin
      borderPath.CreateEllipse(w*0.5, w*0.5, w*0.5, w*0.5, True);
      fillPath.CreateEllipse((w-2)*0.5, (w-2)*0.5, (w-2)*0.5, (w-2)*0.5, False);
      v := w * 0.5 * 0.707 - lineWidth*0.5;   // for angle 45° 135° 225° and 315° -> sin and cos = +-0.707
      rectCross := RectF(w*0.5-v, w*0.5-v, w*0.5+v, w*0.5+v);
      FCheckCross[0].CreateLine(rectCross.TopLeft, rectCross.BottomRight);
      FCheckCross[1].CreateLine(PointF(rectCross.Left, rectCross.Bottom), PointF(rectCross.Right, rectCross.Top));
    end;
  end;
  fillPath.Translate(PointF(1,1));

  FCheckBorder.InitDefault;
  FCheckBorder.LinePosition := lpInside;
  FCheckBorder.InitFromPath(borderPath, lineWidth);
  FCheckBorder.Color := BGRA(100,100,100);

  FCheckFill.InitDefault;
  FCheckFill.Color := FCheckBorder.Color;
  FCheckFill.CenterColor := FColorChecked;
  FCheckFill.InitFromPath(fillPath, PointF(w*0.5,w*0.5));

  FNeedToRecomputeSize := True;
  FImage.Texture := NIL;
//  UpdateWidgetSizeAndLayout;
end;

procedure TUICheck.CustomizeCheckBox(aTextureUncheck, aTextureCheck: PTexture; aAdjustImageToFontHeight: boolean);
var s: TSize;
begin
  FTextureUncheck := aTextureUncheck;
  FTextureCheck := aTextureCheck;
  FCheckShapeType := ctUseTexture;


  if aAdjustImageToFontHeight then begin
    // use the font height as size
    s.cx := _Label.CaptionDescriptor.TexturedFont.Font.FontHeight;
    s.cy := s.cx;
  end else begin
    // use the max size of the two texture
    if aTextureUncheck <> NIL then s.Create(aTextureUncheck^.FrameWidth, aTextureUncheck^.FrameHeight)
      else s.Create(0,0);
    if aTextureCheck <> NIL then begin
      s.cx := Max(s.cx, aTextureCheck^.FrameWidth);
      s.cy := Max(s.cy, aTextureCheck^.FrameHeight);
    end;
  end;


  FImage.SetSize(s.cx, s.cy);
  FNeedToRecomputeSize := True;
  if FChecked then FImage.Texture := FTextureCheck else FImage.Texture := FTextureUncheck;
end;

procedure TUICheck.CustomizeWithoutCheckBox;
begin
  FCheckShapeType := ctNone;
  FCheckShapeSize.Create(0, 0);
  FNeedToRecomputeSize := True;
  UpdateWidgetSizeAndLayout;
end;

{ TUIScrollBar }

function TUIScrollBar.Margin: integer;
begin
  if BodyShape.Border.Visible then Result := Round(BodyShape.Border.Width)
    else Result := 1;
end;

procedure TUIScrollBar.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  FInertie.OnElapse(aElapsedTime);
  if FInertie.Value <> 0 then Position := Position + trunc(FInertie.Value)*FInertieSign;
end;

procedure TUIScrollBar.ResizeSlider;
var w, h: integer;
begin
  case FOrientation of
    uioHorizontal: begin
      w := Math.Max(2, Round(ClientArea.Width*FPageSize/(FMax-FMin)));
      h := ClientArea.Height;
      SliderShape.ResizeCurrentShape(w, h, False);
    end;
    uioVertical: begin
     w := ClientArea.Width;
     h := Math.Max(2, Round(ClientArea.Height*FPageSize/(FMax-FMin)));
     SliderShape.ResizeCurrentShape(w, h, False);
    end;
  end;
end;

function TUIScrollBar.GetPercent: single;
begin
  if FMax > FMin then Result := (FPosition-FMin)/(FMax-FMin)
    else Result := 0;
end;

procedure TUIScrollBar.ComputeSliderPosToDraw;
var pc: single;
begin
  if FMax > FMin then begin
    pc := (FPosition-FMin)/(FMax-FMin);
    case FOrientation of
      uioHorizontal: begin
        FSliderPosToDraw.x := ClientArea.Left + Round(ClientArea.Width*pc);
        FSliderPosToDraw.y := ClientArea.Top+(ClientArea.Height-SliderShape.Height) div 2;
      end;
      uioVertical: begin
       FSliderPosToDraw.x := ClientArea.Left + (ClientArea.Width-SliderShape.Width) div 2;
       FSliderPosToDraw.y := ClientArea.Top + Round(ClientArea.Height*pc);
      end;
    end;
  end;
end;

procedure TUIScrollBar.UpdateWidgetSizeAndLayout;
var newWidgetSize: TSize;
   w, h: integer;
begin
  if FNeedToRecomputeSize then begin
    FNeedToRecomputeSize := False;
    w := Width;
    h := Height;
    newWidgetSize := ComputeWidgetSizeToContain(w, h);
    BodyShape.ResizeCurrentShape(newWidgetSize.cx, newWidgetSize.cy, False);
    FNeedLayout := True;
  end;

  if FNeedLayout then begin
    FNeedLayout := False;
    ComputeClientArea;
    ResizeSlider;
    ComputeSliderPosToDraw;
   end;
end;

procedure TUIScrollBar.SetMax(AValue: ptrint);
var totalRange: ptrint;
begin
  if FMax = AValue then Exit;
  FMax := AValue;
  if FMin > FMax then FMin := FMax;
  totalRange := FMax - FMin;
  if FPageSize > totalRange then FPageSize := totalRange;
  if Position > FMax-FPageSize then Position := FMax-FPageSize
    else if FPosition < FMin then Position := FMin;
end;

procedure TUIScrollBar.SetMin(AValue: ptrint);
var totalRange: ptrint;
begin
  if FMin = AValue then Exit;
  FMin := AValue;
  if FMax < FMin then FMax := FMin;
  totalRange := FMax - FMin;
  if FPageSize > totalRange then FPageSize := totalRange;
  if Position > FMax-FPageSize then Position := FMax-FPageSize
    else if Position < FMin then Position := FMin;
end;

procedure TUIScrollBar.SetPageSize(AValue: ptrint);
var totalRange: ptrint;
begin
  if FPageSize = AValue then Exit;
  FPageSize := AValue;
  totalRange := FMax - FMin;
  if FPageSize > totalRange then begin
    FPageSize := totalRange;
    Position := 0;
  end else if Position > FMax-FPageSize then Position := FMax-FPageSize;

  FNeedLayout := True;
  UpdateWidgetSizeAndLayout;
end;

procedure TUIScrollBar.SetPosition(AValue: ptrint);
var v: ptrint;
begin
  v := EnsureRange(AValue, FMin, FMax-FPageSize);
  if FPosition = v then Exit;
  if v <> AValue then FInertie.Value := 0;
  FPosition := v;
  ComputeSliderPosToDraw;

  if FOnChange <> NIL then FOnChange(Self);
end;

function TUIScrollBar.ClientAreaPosToPositionValue(aXY: integer): ptrint;
begin
  if FOrientation = uioHorizontal then begin
    if ClientArea.Width > 0 then Result := Round((aXY-ClientArea.Left)*(FMax-FMin)/ClientArea.Width) + FMin
      else Result := FMin;
  end else begin
    if ClientArea.Height > 0 then Result := Round((aXY-ClientArea.Top)*(FMax-FMin)/ClientArea.Height) + FMin
      else Result := FMin;
  end;
  Result := EnsureRange(Result, FMin, FMax-FPageSize);
end;

procedure TUIScrollBar.SetMouseWheelDeltaValue(AValue: ptrint);
begin
  if AValue < 1 then AValue := 1;
  FMouseWheelDeltaValue := AValue;
end;

procedure TUIScrollBar.DoOnMouseEnter;
begin
  if FOnAnimMouseEnter <> NIL then FOnAnimMouseEnter(Self);
  if FOnMouseEnter <> NIL then FOnMouseEnter (Self);
end;

procedure TUIScrollBar.DoOnMouseLeave;
begin
  if FParentScene.Mouse.SurfaceLocked = Self then exit;

  if FOnAnimMouseLeave <> NIL then FOnAnimMouseLeave(Self);
  if FOnMouseLeave <> NIL then FOnMouseLeave(Self);
  if FStateDown then begin
    if FOnAnimMouseUp <> NIL then FOnAnimMouseUp(Self);
    FStateDown := False;
  end;
end;

procedure TUIScrollBar.DoOnMouseDown(aButton: TMouseButton; aShift: TShiftState; aPt: TPoint);
begin
  inherited DoOnMouseDown(aButton, aShift, aPt);

  if aButton = mbLeft then begin
    if FOrientation = uioHorizontal then begin
      FClickOrigin := aPt.x;
      FSliderPosToDrawWhenClicked := FSliderPosToDraw.x;
      FSliderClickedByMouse := Inrange(aPt.x, FSliderPosToDraw.x, FSliderPosToDraw.x+SliderShape.Width);
    end  else begin
      FClickOrigin := aPt.y;
      FSliderPosToDrawWhenClicked := FSliderPosToDraw.y;
      FSliderClickedByMouse := InRange(aPt.y, FSliderPosToDraw.y, FSliderPosToDraw.y+SliderShape.Height);
    end;
  end;
end;

procedure TUIScrollBar.DoOnMouseUp(aButton: TMouseButton; aShift: TShiftState; aPt: TPoint);
begin
  inherited DoOnMouseUp(aButton, aShift, aPt);

  if aButton = mbLeft then begin
      if FParentScene.Mouse.IsOver(Self) and not FSliderClickedByMouse then begin
        //  page size jump
        if FOrientation = uioHorizontal then begin
          if aPt.x < FSliderPosToDraw.x then Position := Position - FPageSize
            else Position := Position + FPageSize;
        end  else begin
          if aPt.y < FSliderPosToDraw.y then Position := Position - FPageSize
            else Position := Position + FPageSize;
        end;
        FInertie.Value := 0;
      end;
      FSliderClickedByMouse := False;
  end;
end;

procedure TUIScrollBar.DoOnMouseMove(aShift: TShiftState; aPt: TPoint);
var offset, newPos: Integer;
begin
  inherited DoOnMouseMove(aShift, aPt);

  if FSliderClickedByMouse then begin
    if (FOrientation = uioHorizontal) and (ClientArea.Width > 0) then begin
      offset := aPt.x - FClickOrigin;
      newPos := EnsureRange(FSliderPosToDrawWhenClicked + offset, ClientArea.Left, ClientArea.Right-SliderShape.Width);
      FSliderPosToDraw.x := newPos;
      FPosition := ClientAreaPosToPositionValue(newPos);
      if FOnChange <> NIL then FOnChange(Self);
    end else if ClientArea.Height > 0 then begin
      offset := aPt.y - FClickOrigin;
      newPos := EnsureRange(FSliderPosToDrawWhenClicked + offset, ClientArea.Top, ClientArea.Bottom-SliderShape.Height);
      FSliderPosToDraw.y := newPos;
      FPosition := ClientAreaPosToPositionValue(newPos);
      if FOnChange <> NIL then FOnChange(Self);
    end;
  end;
end;

procedure TUIScrollBar.DoOnMouseWheel(aShift: TShiftState; aWheelDelta: Integer; aMousePos: TPoint; var aHandled: Boolean);
var v: ptrint;
begin
  inherited DoOnMouseWheel(aShift, aWheelDelta, aMousePos, aHandled);
  if not aHandled then begin
    v := FMouseWheelDeltaValue * Sign(aWheelDelta);
    if FOrientation = uioVertical then v := -v;
    if FInertie.Value = 0 then begin
      Position := Position + v;
      FInertie.Value := Abs(v);
      FInertieSign := Sign(v);
    end else begin
      if FInertieSign <> Sign(v) then FInertie.Value := 0;
      FInertie.Value := FInertie.Value + Abs(v);
      FInertieSign := Sign(v);
      Position := Position + Trunc(FInertie.Value)*FInertieSign;
    end;
    FInertie.ChangeTo(0, FInertie.Value*0.1, idcStartFastEndSlow);
    aHandled := True;
  end;
end;

procedure TUIScrollBar.DoDraw;
var m: TOGLCMatrix;
   computedTint: TColorF;
begin
  m.CopyFrom(FParentScene.ModelViewMatrix);
  FParentScene.ModelViewMatrix.Translate(FSliderPosToDraw.x, FSliderPosToDraw.y);

  if Tint.Value.alpha = 0 then computedTint.InitFromTint(TintMode, SliderShape.Fill.Tint)
    else computedTint.CopyFrom(FComputedTint);
  SliderShape.DrawBackground(computedTint, FComputedOpacity, False);

  if Tint.Value.alpha = 0 then computedTint.InitFromTint(TintMode, SliderShape.Border.Tint)
    else computedTint.CopyFrom(FComputedTint);
  SliderShape.DrawBorder(computedTint, FComputedOpacity);
  FParentScene.ModelViewMatrix.CopyFrom(m);
end;

procedure TUIScrollBar.DoAnim_OnMouseEnter(Sender: TSimpleSurfaceWithEffect);
begin
  SliderShape.Border.Tint := BGRA(0,255,255);
  SliderShape.Fill.Tint := BGRA(0,255,255,25);
end;

procedure TUIScrollBar.DoAnim_OnMouseLeave(Sender: TSimpleSurfaceWithEffect);
begin
  SliderShape.Border.Tint.alpha := 0;
  SliderShape.Fill.Tint.alpha := 0;
end;

constructor TUIScrollBar.Create(aParentScene: TOGLCScene; aOrientation: TUIOrientation);
begin
  inherited Create(aParentScene);
  FInertie := CreateBoundedFParam(0, 100000, False);
  FAutoSize := False;
  FOrientation := aOrientation;
  FMin := 0;
  FMax := 100;
  FMouseWheelDeltaValue := 5;
  BodyShape.Border.LinePosition := lpInside;
  if aOrientation = uioHorizontal then BodyShape.SetShapeRoundRect(100, 15, 10, 10, 2)
    else BodyShape.SetShapeRoundRect(15, 100, 10, 10, 2);

  SliderShape.InitDefault(aParentScene);
  SliderShape.Border.LinePosition := lpInside;
  if aOrientation = uioHorizontal then SliderShape.SetShapeRoundRect(20, 13, 7, 7, 3)
    else SliderShape.SetShapeRoundRect(13, 20, 7, 7, 3);

  FNeedToRecomputeSize := True;
  UpdateWidgetSizeAndLayout;

  FOnAnimMouseEnter := @DoAnim_OnMouseEnter;
  FOnAnimMouseLeave := @DoAnim_OnMouseLeave;
end;

destructor TUIScrollBar.Destroy;
begin
  FInertie.Free;
  FInertie := NIL;
  inherited Destroy;
end;

procedure TUIScrollBar.SetParams(aPosition, aMin, aMax: ptrint);
begin
  if aMin > aMax then aMin := aMax;
  FMin := aMin;
  FMax := aMax;
  FPosition := EnsureRange(aPosition, FMin, FMax-FPageSize);
  FNeedLayout := True;
  UpdateWidgetSizeAndLayout;
end;

procedure TUIScrollBar.SetParams(aPosition, aMin, aMax, aPageSize: ptrint);
begin
  if aMin > aMax then aMin := aMax;
  FMin := aMin;
  FMax := aMax;
  PageSize := aPageSize;
  FPosition := EnsureRange(aPosition, FMin, FMax-FPageSize);
  FNeedLayout := True;
  UpdateWidgetSizeAndLayout;
end;

{ TUIProgressBar }

constructor TUIProgressBar.Create(aParentScene: TOGLCScene; aOrientation: TUIOrientation);
begin
  inherited Create(aParentScene);
  FOrientation := aOrientation;
  FPercent := 0.0;

  FFillForStencil.InitDefault;

  Gradient.InitDefault;
  Gradient.CreateSingleColor(BGRA(55,200,50));
  Gradient.OnChange := @ProcessCallbackSomethingChangeTheSize;

  BodyShape.SetShapeRoundRect(100,20,10,10,3);
end;

procedure TUIProgressBar.SetPercent(AValue: single);
begin
  if AValue > 1.0 then AValue := 1.0;
  if AValue < 0 then AValue := 0;
  if FPercent = AValue then exit;
  FPercent := AValue;

  //FNeedLayout := True;
  UpdateWidgetSizeAndLayout;
end;

procedure TUIProgressBar.SetReversed(AValue: boolean);
begin
  if FReversed = AValue then Exit;
  FReversed := AValue;
  UpdateWidgetSizeAndLayout;
end;

procedure TUIProgressBar.UpdateWidgetSizeAndLayout;
var w, h, xx, yy: single;
   path: TOGLCPath;
begin
  ComputeClientArea;
  if FPercent > 0 then begin
    xx := 0;
    yy := 0;
    if FOrientation = uioHorizontal then begin
      w := ClientArea.Width*FPercent; // Width*FPercent;
      h := ClientArea.Height; // Height;
      if FReversed then
        xx := ClientArea.Width - w;
    end else begin
      w := ClientArea.Width;  //Width;
      h := ClientArea.Height*FPercent; //Height*FPercent;
      if FReversed then
        yy := ClientArea.Height - h;
    end;
    path := NIL;
    path.CreateRect(xx, yy, w, h, False);
    FFillForStencil.InitFromPath(path, PointF(w*0.5, h*0.5));
  end;
  Gradient.ComputeVerticesAndIndices(Width, Height);
end;

function TUIProgressBar.Margin: integer;
begin
  if BodyShape.Border.Visible then Result := Round(BodyShape.Border.Width-0.5)
    else Result := 0;
end;

procedure TUIProgressBar.DoDraw;
var computedTint: TColorF;
   m: TOGLCMatrix;
begin
  if FPercent = 0 then exit;
  computedTint.SetAsTransparent;
  m.CopyFrom(FParentScene.ModelViewMatrix);
  FParentScene.ModelViewMatrix.Translate(ClientArea.Left, ClientArea.Top);
  // draw progress background -> increase stencil value in the area
  glStencilFunc(GL_EQUAL, FParentScene.FStencilClipping.Value, $FF);
  glStencilOp(GL_INCR, GL_INCR, GL_INCR);
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glDepthMask(GL_FALSE);

  ParentScene.ThreeColorTriangleRenderer.PushFill(@FFillForStencil, 1.0, computedTint, True);

  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glDepthMask(GL_TRUE);
  FParentScene.FStencilClipping.NextValue;

  // Allow drawing only on progress backgroung area
  glStencilFunc(GL_EQUAL, FParentScene.FStencilClipping.Value, $FF);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);

  FParentScene.ThreeColorTriangleRenderer.Prepare(ptTriangleStrip, FParentScene.MVPMatrix, FComputedOpacity, FComputedTint, FBlendMode);
  FParentScene.ThreeColorTriangleRenderer.PushGradient(@Gradient);

  // again, it's necessary to flush all previous drawing before decreasing the value in the stencil buffer background area
  FParentScene.TexturedMVTriangleRenderer.Batch_Flush;

  // draw again background -> decrease stencil
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glDepthMask(GL_FALSE);
  glStencilFunc(GL_EQUAL, FParentScene.FStencilClipping.Value, $FF);
  glStencilOp(GL_DECR, GL_DECR, GL_DECR);

  ParentScene.ThreeColorTriangleRenderer.PushFill(@FFillForStencil, 1.0, computedTint, True);

  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glDepthMask(GL_TRUE);
  FParentScene.FStencilClipping.PreviousValue;

  FParentScene.ModelViewMatrix.CopyFrom(m);
end;


{ TUILabel }

function TUILabel.GetCaption: string;
begin
  Result := CaptionDescriptor.Caption;
end;

procedure TUILabel.SetCaption(AValue: string);
begin
  CaptionDescriptor.Caption := AValue;
end;

function TUILabel.GetWidth: integer;
begin
  Result := CaptionDescriptor.Size.cx;
end;

function TUILabel.GetHeight: integer;
begin
  Result := CaptionDescriptor.Size.cy;
end;

procedure TUILabel.DoDraw;
begin
  if not CaptionDescriptor.CanBeDrawn then exit;

  with CaptionDescriptor do
    FTexturedFont.DrawChars(FCharIndexes, 0, 0, FlipToIndex, NIL,
                             NIL, FComputedOpacity, FComputedTint, FBlendMode);
end;

constructor TUILabel.Create(aParentScene: TOGLCScene; const aCaption: string; aFont: TTexturedFont);
begin
  inherited Create(aParentScene);
  CaptionDescriptor.Init(aCaption, aFont, taCenterLeft);
end;

{ TUIImage }

procedure TUIImage.SetAutoSize(AValue: boolean);
begin
  if FAutoSize = AValue then Exit;
  FAutoSize := AValue;
  AdjustImageInClientRect;
end;

procedure TUIImage.SetCentered(AValue: boolean);
begin
  if FCentered = AValue then Exit;
  FCentered := AValue;
  AdjustImageInClientRect;
end;

procedure TUIImage.SetKeepProportion(AValue: boolean);
begin
  if FKeepProportion = AValue then Exit;
  FKeepProportion := AValue;
  AdjustImageInClientRect;
end;

procedure TUIImage.SetTexture(AValue: PTexture);
begin
  inherited SetTexture(AValue);
  AdjustImageInClientRect;
  if FCallbackTextureChanged <> NIL then FCallbackTextureChanged();
end;

procedure TUIImage.AdjustImageInClientRect;
var scaleValue: single;
   xx, yy, w, h: integer;
begin
  if (FTexture <> NIL) and (FTexture^.FrameWidth > 0) and (FTexture^.FrameHeight > 0) then begin
    if FAutoSize then begin
      if FKeepProportion then begin
        scaleValue := Min(FWidth/FTexture^.FrameWidth, FHeight/FTexture^.FrameHeight);
        w := Trunc(FTexture^.FrameWidth * scaleValue);
        h := Trunc(FTexture^.FrameHeight * scaleValue);
      end else begin
        w := FWidth;
        h := FHeight;
      end;
    end else begin
      // keep original size
      w := FTexture^.FrameWidth;
      h := FTexture^.FrameHeight;
    end;
    if FCentered then begin
      xx := (FWidth - w) div 2;
      yy := (FHeight -h) div 2;
    end else begin
      xx := 0;
      yy := 0;
    end;
//    FModelArea.SetAsRectangle(xx, yy, w, h);
    FRectToRender.Create(Point(xx,yy), w, h);
  end;
end;

function TUIImage.GetWidth: integer;
begin
  Result := FWidth;
end;

function TUIImage.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TUIImage.Draw(const aLayerPercentOpacity: single);
var fi: Int64;
begin
  if FTexture = NIL then exit;
  FComputedOpacity := aLayerPercentOpacity*Opacity.Value*OneDiv255;
  if FComputedOpacity = 0 then exit;
  FComputedTint.InitFromTint(TintMode, Tint.Value);
  FComputedOpacity :=1.0;

  FPreviousModelViewMatrix.CopyFrom(FParentScene.ModelViewMatrix);
  FParentScene.ModelViewMatrix.AddTransform(X.Value+FDrawOffset.x, Y.Value+FDrawOffset.y, Scale.x.Value, Scale.y.Value,
                                            Angle.Value, Width*Pivot.x, Height*Pivot.y);

  if FChildsUseParentOpacity then DrawChildSurfacesWithNegativeZOrder(FComputedOpacity)
    else DrawChildSurfacesWithNegativeZOrder(aLayerPercentOpacity);

  fi := EnsureRange(Trunc(Frame), 0, High(FTexture^.FramesCoord));

  FParentScene.TexturedMVTriangleRenderer.Prepare(ptTriangles, FTexture, 0, BlendMode);
  FParentScene.TexturedMVTriangleRenderer.PushQuad3(FRectToRender, FTexture^.FramesCoord[fi],
                                                    FlipToIndex, NIL, FComputedOpacity, FComputedTint);

  if FChildsUseParentOpacity then DrawChildSurfacesWithPositiveZOrder(FComputedOpacity)
    else DrawChildSurfacesWithPositiveZOrder(aLayerPercentOpacity);

  {$IFDEF DEBUG_MODE_ON}
    FParentScene.BlendMode := FX_BLEND_NORMAL;
    DrawBox(FParentScene, 0, 0, Width, Height, BGRA(255,0,0));
  {$ENDIF}

  // restore the parent scene modelview matrix
  FParentScene.ModelViewMatrix.CopyFrom(FPreviousModelViewMatrix);
end;

constructor TUIImage.Create(aParentScene: TOGLCScene; aTexture: PTexture; aWidth, aHeight: integer);
begin
  inherited Create(aTexture, False);
  FParentScene := aParentScene;
  FWidth := aWidth;
  FHeight := aHeight;
  FAutoSize := True;
  FKeepProportion := True;
  FCentered := True;
  AdjustImageInClientRect;
end;

procedure TUIImage.SetSize(aWidth, aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
  AdjustImageInClientRect;
  if FCallbackSizeChanged <> NIL then FCallbackSizeChanged();
end;

{ TUIButton }

function TUIButton.GetCaption: string;
begin
  Result := FLabel.CaptionDescriptor.Caption;
end;

procedure TUIButton.SetCaption(AValue: string);
begin
  FLabel.CaptionDescriptor.Caption := AValue;
end;

procedure TUIButton.UpdateWidgetSizeAndLayout;
var captionArea: TRect;
   newWidgetSize: TSize;
   w, h: integer;
begin
  if FNeedToRecomputeSize then begin
    FNeedToRecomputeSize := False;

    if (FImage.Texture <> NIL) and FImage.Visible then begin
        w := FImage.Width + FImageMargin;
        h := FImage.Height;
    end else begin
        w := 0;
        h := 0;
    end;
    w := w + FLabel.Width;
    h := Max(h, FLabel.Height);
    newWidgetSize := ComputeWidgetSizeToContain(w, h);
    BodyShape.ResizeCurrentShape(newWidgetSize.cx, newWidgetSize.cy, False);
    FNeedLayout := True;
  end;

  if FNeedLayout then begin
   FNeedLayout := False;
   ComputeClientArea;

   if (FImage.Texture <> NIL) and FImage.Visible then begin
     if AutoSize then begin
       FImage.SetCoordinate(ClientArea.Left, (Height-FImage.Height)*0.5);
       captionArea.Create(Point(ClientArea.Left + FImage.Width + FImageMargin, 0),
                          Width-(FImage.Width + FImageMargin*2), Height);
     end else begin
       w := FImage.Width +FImageMargin + FLabel.Width;
       FImage.SetCoordinate(ClientArea.Left+(ClientArea.Width-w)*0.5, (Height-FImage.Height)*0.5);
       captionArea.Create(Point(Round(FImage.RightX) + FImageMargin, 0),
                          ClientArea.Width-(FImage.Width + FImageMargin*2), Height);
       FLabel.CaptionDescriptor.Alignment := taCenterLeft;
     end;
     FLabel.CaptionDescriptor.Alignment := taCenterLeft;
   end else begin
     captionArea.Create(ClientArea);
     FLabel.CaptionDescriptor.Alignment := taCenterCenter;
   end;

   FLabel.SetCoordinate(FLabel.CaptionDescriptor.ComputeLocation(captionArea));
  end;
end;

procedure TUIButton.SetMouseInteractionEnabled(AValue: boolean);
begin
  FMouseInteractionEnabled := AValue;
end;

constructor TUIButton.Create(aParentScene: TOGLCScene; const aCaption: string; aFont: TTexturedFont; aTexture: PTexture);
begin
  inherited Create(aParentScene);

  if aTexture <> NIL then FImage := TUIImage.Create(aParentScene, aTexture, aTexture^.FrameWidth, aTexture^.FrameHeight)
    else FImage := TUIImage.Create(aParentScene, aTexture, 0, 0);
  AddChild(FImage);
  FImage.OnTextureChanged := @ProcessCallbackSomethingChangeTheSize;
  FImage.OnSizeChanged := @ProcessCallbackSomethingChangeTheSize;

  FLabel := TUILabel.Create(aParentScene, aCaption, aFont);
  FLabel.CaptionDescriptor.Alignment := taCenterLeft;
  AddChild(FLabel);
  FLabel.MouseInteractionEnabled := False; // disable mouse interaction after AddChild() !!
  FLabel.CaptionDescriptor.OnSizeChanged := @ProcessCallbackSomethingChangeTheSize;

  FImageMargin := FParentScene.ScaleDesignToScene(DEFAULT_MARGIN_BETWEEN_IMAGE_AND_CAPTION);
  FNeedToRecomputeSize := True;
  UpdateWidgetSizeAndLayout;

  // default animation
  FOnAnimMouseEnter := @DoAnim_OnMouseEnter;
  FOnAnimMouseLeave := @DoAnim_OnMouseLeave;
  FOnAnimMouseDown := @DoAnim_OnMouseDown;
  FOnAnimMouseUp := @DoAnim_OnMouseUp;
  FOnAnimClick := @DoAnim_OnClick;
end;

procedure TUIButton.DoAnim_OnMouseEnter(Sender: TSimpleSurfaceWithEffect);
begin
  BodyShape.Border.Tint := BGRA(0,255,255);
  BodyShape.Fill.Tint := BGRA(0,255,255,25);
end;

procedure TUIButton.DoAnim_OnMouseLeave(Sender: TSimpleSurfaceWithEffect);
begin
  BodyShape.Border.Tint.alpha := 0;
  BodyShape.Fill.Tint.alpha := 0;
end;

procedure TUIButton.DoAnim_OnMouseDown(Sender: TSimpleSurfaceWithEffect);
begin
  if GetLightness(BodyShape.Fill.Color) > 32767 then Tint.Value := BGRA(0,0,0,70)
    else Tint.Value := BGRA(255,255,255,70);
end;

procedure TUIButton.DoAnim_OnMouseUp(Sender: TSimpleSurfaceWithEffect);
begin
  Tint.Alpha.Value := 0;
end;

procedure TUIButton.DoAnim_OnClick(Sender: TSimpleSurfaceWithEffect);
begin
  Tint.Value := BGRA(0, 0, 0, 150);
  Tint.Alpha.ChangeTo(0, 0.2);
end;

{ TUIClickableObject }

procedure TUIClickableObject.DoOnMouseEnter;
begin
  if FOnAnimMouseEnter <> NIL then FOnAnimMouseEnter(Self);
  if FOnMouseEnter <> NIL then FOnMouseEnter (Self);
end;

procedure TUIClickableObject.DoOnMouseLeave;
begin
  if FParentScene.Mouse.SurfaceLocked = Self then exit;

  if FOnAnimMouseLeave <> NIL then FOnAnimMouseLeave(Self);
  if FOnMouseLeave <> NIL then FOnMouseLeave(Self);
  if FStateDown then begin
    if FOnAnimMouseUp <> NIL then FOnAnimMouseUp(Self);
    FStateDown := False;
  end;
end;

procedure TUIClickableObject.DoOnMouseClick;
begin
  if FOnAnimClick <> NIL then FOnAnimClick(Self);
  if FOnClick <> NIL then FOnClick(Self);
end;

procedure TUIClickableObject.DoOnMouseDown(aButton: TMouseButton; aShift: TShiftState; aPt: TPoint);
begin
  FStateDown := aButton in [mbLeft, mbRight];
  if FOnAnimMouseDown <> NIL then FOnAnimMouseDown(Self);

  if FOnMouseDown <> NIL then FOnMouseDown(Self, aButton, aShift, aPt.x, aPt.y);
end;

procedure TUIClickableObject.DoOnMouseUp(aButton: TMouseButton; aShift: TShiftState; aPt: TPoint);
begin
  if not FParentScene.Mouse.IsOver(Self) then begin
    if FOnAnimMouseUp <> NIL then FOnAnimMouseUp(Self);
    if FOnAnimMouseLeave <> NIL then FOnAnimMouseLeave(Self);
    exit;
  end;

  if FOnAnimMouseUp <> NIL then FOnAnimMouseUp(Self);
  if FStateDown then DoOnMouseClick;
  FStateDown := False;

  if FOnMouseUp <> NIL then FOnMouseUp(Self, aButton, aShift, aPt.x, aPt.y);
end;

procedure TUIClickableObject.DoOnMouseMove(aShift: TShiftState; aPt: TPoint);
begin
  if FOnMouseMove <> NIL then FOnMouseMove(Self, aShift, aPt.x, aPt.y);
end;

procedure TUIClickableObject.DoOnMouseWheel(aShift: TShiftState; aWheelDelta: Integer; aMousePos: TPoint; var aHandled: Boolean);
begin
  if FOnMouseWheel <> NIL then FOnMouseWheel(Self, aShift, aWheelDelta, aMousePos, aHandled);
end;

procedure TUIClickableObject.CheckMouseEnterOrLeave;
begin
  if not FMouseInteractionEnabled then exit;

  if FParentScene.Mouse.IsOver(Self) then begin
    if not FMouseEntered then begin
      FMouseEntered := True;
      DoOnMouseEnter;
    end
  end else if FMouseEntered then begin
    FMouseEntered := False;
    DoOnMouseLeave;
  end;
end;

procedure TUIClickableObject.SetMouseInteractionEnabled(AValue: boolean);
begin
  if FMouseInteractionEnabled = AValue then Exit;
  FMouseInteractionEnabled := AValue;
end;

procedure TUIClickableObject.DoPositionAnchor;
var targetSurface: TSimpleSurfaceWithEffect;
  p: TPointF;
begin
  // Horizontal Anchor
  if FHAnchor.AnchorType <> atNone then begin
    if (FHAnchor.AnchorType = atAnchorToParent) and (FParentSurface = NIL) then begin
      // H anchor to parent scene
      case FHAnchor.SelfSide of
        haLeft: begin
          case FHAnchor.TargetSide of
            haLeft: X.Value := FHAnchor.Margin;
            haCenter: X.Value := FParentScene.Width*0.5 + FHAnchor.Margin;
            haRight: X.Value := FParentScene.Width + FHAnchor.Margin;
          end;
        end;
        haCenter: begin
          case FHAnchor.TargetSide of
            haLeft: CenterX := FHAnchor.Margin;
            haCenter: CenterX := FParentScene.Width*0.5 + FHAnchor.Margin;
            haRight: CenterX := FParentScene.Width + FHAnchor.Margin;
          end;
        end;
        haRight: begin
          case FHAnchor.TargetSide of
            haLeft: RightX := FHAnchor.Margin;
            haCenter: RightX := FParentScene.Width*0.5 + FHAnchor.Margin;
            haRight: RightX := FParentScene.Width + FHAnchor.Margin;
          end;
        end;
      end;
    end  // H anchor to parent scene
    else begin
      // H anchor to parent surface or another surface
      if FHAnchor.AnchorType = atAnchorToParent then targetSurface := FParentSurface
        else targetSurface := FHAnchor.TargetSurface;
      if targetSurface <> NIL then begin
        if FHAnchor.AnchorType = atAnchorToParent then begin
          p := targetSurface.SurfaceToScene(PointF(0,0));
          p := FParentSurface.SceneToSurface(p);
        end else begin
            if ParentSurface <> NIL then begin
              p := targetSurface.SurfaceToScene(PointF(0,0));
              p := ParentSurface.SceneToSurface(p);
            end else begin
              p := targetSurface.SurfaceToScene(PointF(0,0)); // GetXY;
            end;
        end;
        case FHAnchor.TargetSide of
          haLeft: ; //p.x := targetSurface.X.Value;
          haCenter: p.x := p.x + targetSurface.Width*0.5;
          haRight: p.x := p.x + targetSurface.Width;
        end;
        case FHAnchor.SelfSide of
          haLeft: X.Value := p.x + FHAnchor.Margin;
          haCenter: CenterX := p.x + FHAnchor.Margin;
          haRight: RightX := p.x + FHAnchor.Margin;
        end;
      end;
    end;
  end;// Horizontal Anchor

  // Vertical Anchor
  if FVAnchor.AnchorType <> atNone then begin
    if (FVAnchor.AnchorType = atAnchorToParent) and (FParentSurface = NIL) then begin
      // V anchor to parent scene
      case FVAnchor.SelfSide of
        vaTop: begin
          case FVAnchor.TargetSide of
            vaTop: Y.Value := 0 + FVAnchor.Margin;
            vaCenter: Y.Value := FParentScene.Height*0.5 + FVAnchor.Margin;
            vaBottom: Y.Value := FParentScene.Height + FVAnchor.Margin;
          end;
        end;
        vaCenter: begin
          case FVAnchor.TargetSide of
            vaTop: CenterY := 0 + FVAnchor.Margin;
            vaCenter: CenterY := FParentScene.Height*0.5 + FVAnchor.Margin;
            vaBottom: CenterY := FParentScene.Height + FVAnchor.Margin;
          end;
        end;
        vaBottom: begin
          case FVAnchor.TargetSide of
            vaTop: BottomY := 0 + FVAnchor.Margin;
            vaCenter: BottomY := FParentScene.Height*0.5 + FVAnchor.Margin;
            vaBottom: BottomY := FParentScene.Height + FVAnchor.Margin;
          end;
        end;
      end;
    end // V anchor to parent scene
    else begin
      // V anchor to parent surface or another surface
      if FVAnchor.AnchorType = atAnchorToParent then targetSurface := FParentSurface
        else targetSurface := FVAnchor.TargetSurface;
      if targetSurface <> NIL then begin
        if FVAnchor.AnchorType = atAnchorToParent then begin
          p := targetSurface.SurfaceToScene(PointF(0,0));
          p := FParentSurface.SceneToSurface(p);
        end else begin
          if ParentSurface <> NIL then begin
            p := targetSurface.SurfaceToScene(PointF(0,0));
            p := ParentSurface.SceneToSurface(p);
          end else begin
             p := targetSurface.SurfaceToScene(PointF(0,0)); // GetXY;
          end;
        end;
        case FVAnchor.TargetSide of
          vaTop:; //p.y := 0;
          vaCenter: p.y := p.y + targetSurface.Height*0.5;
          vaBottom: p.y := p.y + targetSurface.Height;
        end;
        case FVAnchor.SelfSide of
          vaTop: Y.Value := p.y + FVAnchor.Margin;
          vaCenter: CenterY := p.y + FVAnchor.Margin;
          vaBottom: BottomY := p.y + FVAnchor.Margin;
        end;
      end;
    end;
  end;// Vertical Anchor

{  if FAnchorType = atNone then exit;

  // anchor
  if (FAnchorType = atAnchorToParent) and (FParentSurface = NIL) then begin
    // anchor to parent scene
    case FHAnchor.SelfSide of
      haLeft: begin
        case FHAnchor.TargetSide of
          haLeft: X.Value := 0 + FHAnchor.Margin;
          haCenter: X.Value := FParentScene.Width*0.5 + FHAnchor.Margin;
          haRight: X.Value := FParentScene.Width + FHAnchor.Margin;
        end;
      end;
      haCenter: begin
        case FHAnchor.TargetSide of
          haLeft: CenterX := 0 + FHAnchor.Margin;
          haCenter: CenterX := FParentScene.Width*0.5 + FHAnchor.Margin;
          haRight: CenterX := FParentScene.Width + FHAnchor.Margin;
        end;
      end;
      haRight: begin
        case FHAnchor.TargetSide of
          haLeft: RightX := 0 + FHAnchor.Margin;
          haCenter: RightX := FParentScene.Width*0.5 + FHAnchor.Margin;
          haRight: RightX := FParentScene.Width + FHAnchor.Margin;
        end;
      end;
    end;
    case FVAnchor.SelfSide of
      vaTop: begin
        case FVAnchor.TargetSide of
          vaTop: Y.Value := 0 + FVAnchor.Margin;
          vaCenter: Y.Value := FParentScene.Height*0.5 + FVAnchor.Margin;
          vaBottom: Y.Value := FParentScene.Height + FVAnchor.Margin;
        end;
      end;
      vaCenter: begin
        case FVAnchor.TargetSide of
          vaTop: CenterY := 0 + FVAnchor.Margin;
          vaCenter: CenterY := FParentScene.Height*0.5 + FVAnchor.Margin;
          vaBottom: CenterY := FParentScene.Height + FVAnchor.Margin;
        end;
      end;
      vaBottom: begin
        case FVAnchor.TargetSide of
          vaTop: BottomY := 0 + FVAnchor.Margin;
          vaCenter: BottomY := FParentScene.Height*0.5 + FVAnchor.Margin;
          vaBottom: BottomY := FParentScene.Height + FVAnchor.Margin;
        end;
      end;
    end;
    exit;
  end;

  // anchor to parent surface or another surface
  if FAnchorType = atAnchorToParent then targetSurface := FParentSurface
    else targetSurface := FAnchorTargetSurface;
  if targetSurface = NIL then exit;

  if FAnchorType = atAnchorToParent then begin
    p := targetSurface.SurfaceToScene(PointF(0,0));
    p := FParentSurface.SceneToSurface(p);
  end else begin
      if ParentSurface <> NIL then begin
        p := targetSurface.SurfaceToScene(PointF(0,0));
        p := ParentSurface.SceneToSurface(p);
      end else begin
        p := targetSurface.SurfaceToScene(PointF(0,0)); // GetXY;
      end;
  end;

  case FHAnchor.TargetSide of
    haLeft: ; //p.x := targetSurface.X.Value;
    haCenter: p.x := p.x + targetSurface.Width*0.5;
    haRight: p.x := p.x + targetSurface.Width;
  end;
  case FVAnchor.TargetSide of
    vaTop:; //p.y := 0;
    vaCenter: p.y := p.y + targetSurface.Height*0.5;
    vaBottom: p.y := p.y + targetSurface.Height;
  end;

  case FHAnchor.SelfSide of
    haLeft: X.Value := p.x + FHAnchor.Margin;
    haCenter: CenterX := p.x + FHAnchor.Margin;
    haRight: RightX := p.x + FHAnchor.Margin;
  end;
  case FVAnchor.SelfSide of
    vaTop: Y.Value := p.y + FVAnchor.Margin;
    vaCenter: CenterY := p.y + FVAnchor.Margin;
    vaBottom: BottomY := p.y + FVAnchor.Margin;
  end; }
end;

procedure TUIClickableObject.SetScrollOffset(AValue: TPoint);
var i: Integer;
begin
  FScrollOffset := AValue;
  for i:=0 to ChildCount-1 do
    if Childs[i] is TUIClickableObject then
      TUIClickableObject(Childs[i]).ScrollOffset := AValue;
end;

procedure TUIClickableObject.Update(const aElapsedTime: single);
var i: integer;
  allParentHaveMouseInteractionEnabled: boolean;
  par: TSimpleSurfaceWithEffect;
begin
  inherited Update(aElapsedTime);

  DoPositionAnchor;

  // check if all parents have MouseInteractionEnabled sets to True
  allParentHaveMouseInteractionEnabled := True;
  par := ParentSurface;
  while par <> NIL do begin
    if par is TUIClickableObject then
      allParentHaveMouseInteractionEnabled := allParentHaveMouseInteractionEnabled and TUIClickableObject(par).MouseInteractionEnabled;
    par := par.ParentSurface;
  end;

  if not MouseInteractionEnabled or not allParentHaveMouseInteractionEnabled then begin
    if FMouseEntered then begin
      DoOnMouseLeave;
      FMouseEntered := False;
    end;
    if FParentScene.Mouse.FSurfaceUnderMouse = Self then FParentScene.Mouse.FSurfaceUnderMouse := NIL;
    if FParentScene.Mouse.FSurfaceLockedByMouse = Self then FParentScene.Mouse.FSurfaceLockedByMouse := NIL;
    exit;
  end;

  if FFreeze or not FVisible or (Opacity.Value = 0) or
    (FParentScene.Mouse.SurfaceUnder <> NIL) then exit;

  // first check mouse on childs
  for i:=ChildCount-1 downto 0 do
   if Childs[i] is TUIClickableObject then
     TUIClickableObject(Childs[i]).CheckMouseEnterOrLeave;

  CheckMouseEnterOrLeave;
end;

constructor TUIClickableObject.Create(aParentScene: TOGLCScene);
begin
  inherited Create;
  FParentScene := aParentScene;
  FMouseInteractionEnabled := True;
end;

function TUIClickableObject.SurfaceToSceneUI(aPt: TPoint): TPoint;
begin
  Result := FSurfaceOrigin.Round + aPt;
end;

function TUIClickableObject.SceneToSurfaceUI(aPt: TPoint): TPoint;
begin
  Result := aPt - FSurfaceOrigin.Round;
end;

function TUIClickableObject.SurfaceToParentUI(aPt: TPoint; aParent: TSimpleSurfaceWithEffect): TPoint;
var s: TSimpleSurfaceWithEffect;
begin
  Result := aPt;
  if aParent = NIL then aParent := FParentSurface  // use first parent
    else if not aParent.IsParentOf(Self) then exit;

  if aParent <> NIL then begin
    s := self;
    repeat
      Result := Result + Point(Round(s.X.Value), Round(s.Y.Value));
      s := s.FParentSurface;
    until s = aParent;
  end;
end;

function TUIClickableObject.ParentToSurfaceUI(aPt: TPoint; aParent: TSimpleSurfaceWithEffect): TPoint;
var s: TSimpleSurfaceWithEffect;
begin
  Result := aPt;
  if aParent = NIL then aParent := FParentSurface  // use first parent
    else if not aParent.IsParentOf(Self) then exit;

  if aParent <> NIL then begin
    s := self;
    repeat
      Result := Result - Point(Round(s.X.Value), Round(s.Y.Value));
      s := s.FParentSurface;
    until s = aParent;
  end;
end;

procedure TUIClickableObject.AnchorHPosToParent(aHSelfSide, aHTargetSide: THAnchorType; aHMargin: integer);
begin
  FHAnchor.AnchorType := atAnchorToParent;
  FHAnchor.SelfSide := aHSelfSide;
  FHAnchor.TargetSide := aHTargetSide;
  FHAnchor.Margin := aHMargin;
  FHAnchor.TargetSurface := NIL;
end;

procedure TUIClickableObject.AnchorVPosToParent(aVSelfSide, aVTargetSide: TVAnchorType; aVMargin: integer);
begin
  FVAnchor.AnchorType := atAnchorToParent;
  FVAnchor.SelfSide := aVSelfSide;
  FVAnchor.TargetSide := aVTargetSide;
  FVAnchor.Margin := aVMargin;
  FVAnchor.TargetSurface := NIL;
end;

procedure TUIClickableObject.AnchorPosToParent(aHSelfSide, aHTargetSide: THAnchorType; aHMargin: integer;
  aVSelfSide, aVTargetSide: TVAnchorType; aVMargin: integer);
begin
  AnchorHPosToParent(aHSelfSide, aHTargetSide, aHMargin);
  AnchorVPosToParent(aVSelfSide, aVTargetSide, aVMargin);
end;

procedure TUIClickableObject.AnchorHPosToSurface(aSurface: TSimpleSurfaceWithEffect;
  aHSelfSide, aHTargetSide: THAnchorType; aHMargin: integer);
begin
  if aSurface = NIL then begin
    FHAnchor.AnchorType := atNone;
    exit;
  end;
  FHAnchor.AnchorType := atAnchorToSurface;
  FHAnchor.TargetSurface := aSurface;
  FHAnchor.SelfSide := aHSelfSide;
  FHAnchor.TargetSide := aHTargetSide;
  FHAnchor.Margin := aHMargin;
end;

procedure TUIClickableObject.AnchorVPosToSurface(aSurface: TSimpleSurfaceWithEffect;
  aVSelfSide, aVTargetSide: TVAnchorType; aVMargin: integer);
begin
  if aSurface = NIL then begin
    FVAnchor.AnchorType := atNone;
    exit;
  end;
  FVAnchor.AnchorType := atAnchorToSurface;
  FVAnchor.TargetSurface := aSurface;
  FVAnchor.SelfSide := aVSelfSide;
  FVAnchor.TargetSide := aVTargetSide;
  FVAnchor.Margin := aVMargin;
end;

procedure TUIClickableObject.AnchorPosToSurface(aSurface: TSimpleSurfaceWithEffect;
  aHSelfSide, aHTargetSide: THAnchorType; aHMargin: integer;
  aVSelfSide, aVTargetSide: TVAnchorType; aVMargin: integer);
begin
  AnchorHPosToSurface(aSurface, aHSelfSide, aHTargetSide, aHMargin);
  AnchorVPosToSurface(aSurface, aVSelfSide, aVTargetSide, aVMargin);
end;

{ TBodyShape }

procedure TBodyShape.SetShape(aBorderPath, aFillPath: TOGLCPath; aWidth, aHeight: integer; aBorderWidth: single);
begin
  Width := aWidth;
  Height := aHeight;

  Border.InitFromPath(aBorderPath, aBorderWidth);
  Fill.InitFromPath(aFillPath, PointF(aWidth*0.5, aHeight*0.5));

  if FMaskEventOnShapeChanged and (FOnShapeChanged <> NIL) then FOnShapeChanged();
end;

function TBodyShape.SaveToString: string;
var prop: TProperties;
  s: string;
  path: TOGLCPath;
begin
  prop.Init(',');
  prop.Add('ShapeType', Ord(FShapetype));
  if FShapeType = stRoundRect then begin
    prop.Add('RoundX', FRx);
    prop.Add('RoundY', FRy);
    if FRoundRectOptions <> [] then begin
      s := '';
      if rrTopLeftSquare in FRoundRectOptions then s := s + 'A';
      if rrTopRightSquare in FRoundRectOptions then s := s + 'B';
      if rrBottomRightSquare in FRoundRectOptions then s := s + 'C';
      if rrBottomLeftSquare in FRoundRectOptions then s := s + 'D';
      if rrTopLeftBevel in FRoundRectOptions then s := s + 'E';
      if rrTopRightBevel in FRoundRectOptions then s := s + 'F';
      if rrBottomRightBevel in FRoundRectOptions then s := s + 'G';
      if rrBottomLeftBevel in FRoundRectOptions then s := s + 'H';
      if rrDefault in FRoundRectOptions then s := s + 'I';
      prop.Add('RoundRectOption', s);
    end;
  end;
  if FShapeType = stCustom then begin
    path := Copy(Border.Path);
    if (Length(path) > 1) and (Path[0] = Path[High(Path)]) then
      system.Delete(path, High(path), 1); // re-open the path
    prop.Add('CustomPath', path.SaveToString);
  end;
  prop.Add('Width', Width);
  prop.Add('Height', Height);
  prop.Add('FillData', Fill.SaveToString);
  prop.Add('BorderData', Border.SaveToString);
  Result := prop.PackedProperty;
end;

procedure TBodyShape.LoadFromString(const data: string);
var prop: TProperties;
  vi: integer;
  s: string;
  path: TOGLCPath;
begin
  InitDefault(FParentScene);
  prop.Split(data, ',');
  vi := 0;
  prop.IntegerValueOf('ShapeType', vi, Ord(FShapeType));
  FShapeType := TShapeType(vi);
  if FShapeType = stRoundRect then begin
    prop.SingleValueOf('RoundX', FRx, 0);
    prop.SingleValueOf('RoundY', FRy, 0);
    FRoundRectOptions := [];
    prop.StringValueOf('RoundRectOption', s, '');
    if s.Contains('A') then Include(FRoundRectOptions, rrTopLeftSquare);
    if s.Contains('B') then Include(FRoundRectOptions, rrTopRightSquare);
    if s.Contains('C') then Include(FRoundRectOptions, rrBottomRightSquare);
    if s.Contains('D') then Include(FRoundRectOptions, rrBottomLeftSquare);
    if s.Contains('E') then Include(FRoundRectOptions, rrTopLeftBevel);
    if s.Contains('F') then Include(FRoundRectOptions, rrTopRightBevel);
    if s.Contains('G') then Include(FRoundRectOptions, rrBottomRightBevel);
    if s.Contains('H') then Include(FRoundRectOptions, rrBottomLeftBevel);
    if s.Contains('I') then Include(FRoundRectOptions, rrDefault);
  end;
  if FShapeType = stCustom then begin
    prop.StringValueOf('CustomPath', s, '');
    path := NIL;
    path.LoadFromString(s);
  end;
  prop.IntegerValueOf('Width', Width, 100);
  prop.IntegerValueOf('Height', Height, 100);

  prop.StringValueOf('FillData', s, '');
  Fill.LoadFromString(s);
  prop.StringValueOf('BorderData', s, '');
  Border.LoadFromString(s);

  // create the shape
  case FShapeType of
    stRectangle: SetShapeRectangle(Width, Height, Border.Width);
    stRoundRect: SetShapeRoundRect(Width, Height, FRx, FRy, Border.Width, FRoundRectOptions);
    stEllipse: SetShapeEllipse(Width, Height, Border.Width);
    stCustom: SetCustomShape(path, Border.Width);
  end;
end;

procedure TBodyShape.InitDefault(aParentScene: TOGLCScene);
begin
  FParentScene := aParentScene;
  Border.InitDefault;
  Fill.InitDefault;
  Width := 0;
  Height := 0;
  FOnShapeChanged := NIL;
  FMaskEventOnShapeChanged := True;
end;

procedure TBodyShape.SetShapeRectangle(aWidth, aHeight: integer; aBorderWidth: single);
var borderPath, fillPath: TOGLCPath;
begin
  borderPath := NIL;
  fillPath := NIL;
  FShapetype := stRectangle;

  if aBorderWidth > 0 then borderPath.CreateRect(0, 0, aWidth, aHeight, True);

  fillPath.CreateRect(0, 0, aWidth-2, aHeight-2, False);
  fillPath.Translate(PointF(1,1));

  SetShape(borderPath, fillPath, aWidth, aHeight, aBorderWidth);
end;

procedure TBodyShape.SetShapeRoundRect(aWidth, aHeight: integer; aRx, aRy,
  aBorderWidth: single; aOptions: TRoundRectangleOptions);
var borderPath, fillPath: TOGLCPath;
   v: single;
begin
  borderPath := NIL;
  fillPath := NIL;
  FShapetype := stRoundRect;
  FRoundRectOptions := aOptions;
  FRx := aRx;
  FRy := aRy;

  // adjust round corner to avoid artifacts
  if aRx = aRy then begin
    v := Min(aWidth*0.4, aHeight*0.4);
    if (aRx > v) or (aRy > v) then begin
      aRx := v;
      aRy := v;
    end;
  end else begin
    if aRx > aWidth*0.4 then aRx := aWidth*0.4;
    if aRy > aHeight*0.4 then aRy := aHeight*0.4;
  end;

  if aBorderWidth > 0 then begin
    borderPath.CreateRoundRect(0, 0, aWidth, aHeight, aRx, aRy, aOptions, True);
  end;
  fillPath.CreateRoundRect(0, 0, aWidth-2, aHeight-2, aRx, aRy, aOptions, False);
  fillPath.Translate(PointF(1,1));

  SetShape(borderPath, fillPath, aWidth, aHeight, aBorderWidth);
end;

procedure TBodyShape.SetShapeEllipse(aWidth, aHeight: integer; aBorderWidth: single);
var borderPath, fillPath: TOGLCPath;
begin
  borderPath := NIL;
  fillPath := NIL;
  FShapetype := stEllipse;

  if aBorderWidth > 0 then begin
    borderPath.CreateEllipse(aWidth*0.5, aHeight*0.5, aWidth*0.5, aHeight*0.5, True);
  end;
  fillPath.CreateEllipse((aWidth-2)*0.5, (aHeight-2)*0.5, (aWidth-2)*0.5, (aHeight-2)*0.5, False);
  fillPath.Translate(PointF(1,1));

  SetShape(borderPath, fillPath, aWidth, aHeight, aBorderWidth);
end;

procedure TBodyShape.SetCustomShape(const aPath: TOGLCPath; aBorderWidth: single);
var borderPath, fillPath: TOGLCPath;
  r: TRectF;
begin
  FShapetype := stCustom;

  Border.ClearArrays;
  Fill.ClearArrays;
  if Length(aPath) = 0 then exit;

  borderPath := NIL;
  if aBorderWidth > 0 then begin
    borderPath := Copy(aPath);
    borderPath.ClosePath;
  end;

  fillPath := NIL;
  fillPath := Copy(aPath);
  fillPath.Reduce(-0.5);
  fillPath.Translate(PointF(0.25, 0.25));
  r := aPath.Bounds;

  if (r.Left <> 0) or (r.Top <> 0) then begin
    if Length(borderPath) > 0 then borderPath.Translate(PointF(-r.Left,-r.Top));
    fillPath.Translate(PointF(-r.Left,-r.Top));
    r.Offset(-r.Left, -r.Top);
  end;

  SetShape(borderPath, fillPath, Round(r.Right-r.Left), Round(r.Bottom-r.Top), aBorderWidth);
end;

procedure TBodyShape.ResizeCurrentShape(aNewWidth, aNewHeight: integer; aFireEventOnShapeChanged: boolean);
begin
  if (Width = aNewWidth) and (Height = aNewHeight) then exit;

  FMaskEventOnShapeChanged := aFireEventOnShapeChanged;
  case FShapetype of
    stRectangle: SetShapeRectangle(aNewWidth, aNewHeight, Border.Width);
    stRoundRect: SetShapeRoundRect(aNewWidth, aNewHeight, FRx, FRy, Border.Width, FRoundRectOptions);
    stEllipse: SetShapeEllipse(aNewWidth, aNewHeight, Border.Width);
  end;
  FMaskEventOnShapeChanged := True;
end;

procedure TBodyShape.DrawBorder(const aComputedTint: TColorF; aOpacity: single);
begin
  if Border.Visible and (Border.VertexCount > 0) then
    FParentScene.SmoothLineRenderer.DrawBorder(Border, aOpacity, aComputedTint);
end;

procedure TBodyShape.DrawBackground(const aComputedTint: TColorF; aOpacity: single; aForceDrawForStencilBuffer: boolean);
begin
  if (Fill.VertexCount > 0) and (Fill.Visible or aForceDrawForStencilBuffer) then
    FParentScene.ThreeColorTriangleRenderer.PushFill(@Fill, aOpacity, aComputedTint, True);
end;

{ TUIClickableWithBodyShape }

procedure TUIClickableWithBodyShape.ProcessCallbackSomethingChangeTheSize;
begin
  if AutoSize then FNeedToRecomputeSize := True
    else FNeedLayout := True;
  UpdateWidgetSizeAndLayout;
end;

procedure TUIClickableWithBodyShape.ProcessBackGradientColorChange;
begin
  if Length(BackGradient.Rows) > 0 then
    BackGradient.ComputeVerticesAndIndices(Width, Height);
end;

procedure TUIClickableWithBodyShape.UpdateWidgetSizeAndLayout;
begin
  ComputeClientArea;
end;

function TUIClickableWithBodyShape.Margin: integer;
begin
  if BodyShape.Border.Visible then Result := FParentScene.ScaleDesignToScene(MARGIN_BETWEEN_BORDER_AND_CAPTION) + Round(BodyShape.Border.Width)
    else Result := FParentScene.ScaleDesignToScene(1);
end;

function TUIClickableWithBodyShape.ComputeWidgetSizeToContain(w, h: integer): TSize;
var m: Integer;
begin
  m := Margin*2;
  Result.cx := w + m;
  Result.cy := h + m;
end;

procedure TUIClickableWithBodyShape.ComputeClientArea;
var m: integer;
begin
  m := Margin;
  FClientArea.Create(Point(m, m), Width-m-m, Height-m-m);
end;

function TUIClickableWithBodyShape.GetWidth: integer;
begin
  Result := BodyShape.Width;
end;

function TUIClickableWithBodyShape.GetHeight: integer;
begin
  Result := BodyShape.Height;
end;

procedure TUIClickableWithBodyShape.DrawBorder(const aComputedTint: TColorF; aOpacity: single);
begin
  BodyShape.DrawBorder(aComputedTint, aOpacity);
end;

procedure TUIClickableWithBodyShape.DrawBackground(const aComputedTint: TColorF; aOpacity: single);
begin
  BodyShape.DrawBackground(aComputedTint, aOpacity, True);
end;

{function TUIClickableWithBodyShape.MouseCanInteract: boolean;
var par: TUIClickableWithBodyShape;
begin
  if ParentSurface = NIL then begin
    // there is no parent: mouse can interact if the surface is visible on the scene
    Result := (RightX > 0) and (X.Value < ParentScene.Width) and
              (BottomY > 0) and (Y.Value < ParentScene.Height);
    exit;
  end;

  // if the parent don't have clipping, childs are all visible
  if not (ParentSurface is TUIClickableWithBodyShape) then exit(True);

  par := TUIClickableWithBodyShape(ParentSurface);

  // if the parent don't apply clipping, childs are all visible
  if not par.ChildClippingEnabled then exit(True);

  Result := par.MouseCanInteract and ClientArea.IntersectsWith(par.ClientArea);
end; }

procedure TUIClickableWithBodyShape.Draw(const aLayerPercentOpacity: single);
var clippingWasActive: Boolean;
begin
  if not Visible then exit;

  FComputedOpacity := aLayerPercentOpacity*Opacity.Value*OneDiv255;
  if FComputedOpacity = 0 then exit;
  if Tint.Value.alpha <> 0 then FComputedTint.InitFromTint(TintMode, Tint.Value)
    else FComputedTint.InitFromTint(TintMode, BodyShape.Fill.Tint);
  PushAndPrepareSceneMV;

  if not ChildClippingEnabled then begin
    if BodyShape.Fill.Visible and (BodyShape.Fill.Color.alpha <> 0) then
      DrawBackground(FComputedTint, FComputedOpacity);
  end else begin
    // it's necessary to flush all previous drawing to not interfere with stencil buffer
    FParentScene.TexturedMVTriangleRenderer.Batch_Flush;

    clippingWasActive := FParentScene.FStencilClipping.IsActive;
    if not clippingWasActive then FParentScene.FStencilClipping.Start;

    // draw background -> increase stencil value in the area
    glStencilFunc(GL_EQUAL, FParentScene.FStencilClipping.Value, $FF);
    glStencilOp(GL_INCR, GL_INCR, GL_INCR);
    if BodyShape.Fill.Visible and (BodyShape.Fill.Color.alpha <> 0) then
      DrawBackground(FComputedTint, FComputedOpacity)
    else begin
      glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
      glDepthMask(GL_FALSE);
      DrawBackground(FComputedTint, 1.0);
      glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
      glDepthMask(GL_TRUE);
    end;
    FParentScene.FStencilClipping.NextValue;

    // Allow drawing only on backgroung area
    glStencilFunc(GL_EQUAL, FParentScene.FStencilClipping.Value, $FF);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  end;

  // if needed draw the BackGradient
  if BackGradient.Visible and (Length(BackGradient.Vertices) > 0) then begin
    FParentScene.ThreeColorTriangleRenderer.Prepare(ptTriangleStrip, FParentScene.MVPMatrix, FComputedOpacity, FComputedTint, FBlendMode);
    FParentScene.ThreeColorTriangleRenderer.PushGradient(@BackGradient);
  end;

  DoDraw;
  if ChildsUseParentOpacity then DrawChildSurfacesWithPositiveZOrder(FComputedOpacity)
    else DrawChildSurfacesWithPositiveZOrder(aLayerPercentOpacity);

  if ChildClippingEnabled then begin
    // again, it's necessary to flush all previous drawing before decreasing the value in the stencil buffer background area
    FParentScene.TexturedMVTriangleRenderer.Batch_Flush;

    // redraw background only in the stencil buffer -> decrease its value in the area
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
    glDepthMask(GL_FALSE);
    glStencilFunc(GL_EQUAL, FParentScene.FStencilClipping.Value, $FF);
    glStencilOp(GL_DECR, GL_DECR, GL_DECR);
    DrawBackground(FComputedTint, FComputedOpacity);
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    glDepthMask(GL_TRUE);
    FParentScene.FStencilClipping.PreviousValue;

    // draw border on the previous level of clipping
    glStencilFunc(GL_EQUAL, FParentScene.FStencilClipping.Value, $FF);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  end;
  if Tint.Value.alpha = 0 then FComputedTint.InitFromTint(TintMode, BodyShape.Border.Tint);
  DrawBorder(FComputedTint, FComputedOpacity);

  if ChildClippingEnabled and not clippingWasActive then FParentScene.FStencilClipping.Stop;

  {$IFDEF DEBUG_MODE_ON}
    FParentScene.BlendMode := FX_BLEND_NORMAL;
    DrawBox(FParentScene, 0, 0, Width, Height, BGRA(255,0,0));
  {$ENDIF}

  // restore the parent scene modelview matrix
  PopSceneMV;
end;

procedure TUIClickableWithBodyShape.DoDraw;
begin
  // do nothing here
end;

constructor TUIClickableWithBodyShape.Create(aParentScene: TOGLCScene);
begin
  inherited Create(aParentScene);
  BodyShape.InitDefault(aParentScene);
  BodyShape.Border.LinePosition := lpInside;
  BodyShape.OnShapeChanged := @ProcessCallbackSomethingChangeTheSize;
  FAutoSize := True;

  BackGradient.InitDefault;
  BackGradient.OnChange := @ProcessBackGradientColorChange;

  FChildClippingEnabled := True;
end;

destructor TUIClickableWithBodyShape.Destroy;
begin
  inherited Destroy;
end;

procedure TUIClickableWithBodyShape.SetAutoSize(AValue: boolean);
begin
  if FAutoSize = AValue then Exit;
  FNeedToRecomputeSize := not FAutoSize;
  FAutoSize := AValue;
  ComputeClientArea;
end;

{ TUICaptionDescriptor }

procedure TUICaptionDescriptor.InitDefault;
begin
  FillChar(Self, SizeOf(TUICaptionDescriptor), 0);
end;

procedure TUICaptionDescriptor.SetAlignment(AValue: TOGLCAlignment);
begin
  FAlignment := AValue;
end;

procedure TUICaptionDescriptor.SetCaption(AValue: UTF8String);
begin
  if AValue = FCaption then exit;
  FCaption := AValue;
  UpdateCharIndexesAndComputeSize;
end;

procedure TUICaptionDescriptor.SetTexturedFont(AValue: TTexturedFont);
begin
  if FTexturedFont = AValue then Exit;
  FTexturedFont := AValue;
  UpdateCharIndexesAndComputeSize;
end;

procedure TUICaptionDescriptor.UpdateCharIndexesAndComputeSize;
begin
  FCharIndexes := NIL;
  FComputedSize.cx := 0;
  FComputedSize.cy := 0;

  if FTexturedFont <> NIL then begin
    FCharIndexes := FTexturedFont.StringToCharIndexes(FCaption);
    FComputedSize := FTexturedFont.CharIndexesToSize(FCharIndexes);
  end;

  if FCallbackSizeChanged <> NIL then FCallbackSizeChanged();
end;

procedure TUICaptionDescriptor.Init(const aCaption: UTF8String;
  aTexturedFont: TTexturedFont; aAlignment: TOGLCAlignment);
begin
  FCaption := aCaption;
  FTexturedFont := aTexturedFont;
  FAlignment := aAlignment;
  UpdateCharIndexesAndComputeSize;
end;

function TUICaptionDescriptor.CanBeDrawn: boolean;
begin
  Result := (Length(FCharIndexes) > 0) and (FTexturedFont <> NIL);
end;

function TUICaptionDescriptor.ComputeLocation(const aRect: TRect): TPointF;
var dx, dy: single;
begin
  if Length(FCharIndexes) = 0 then Result := PointF(aRect.Left,aRect.Top)
  else begin
    dx := (aRect.Width - FComputedSize.cx)*0.5;
    if dx < 0 then dx := 0;
    dy := (aRect.Height - FComputedSize.cy)*0.5;
    case FAlignment of
      taTopLeft: Result := PointF(aRect.Left, aRect.Top);
      taTopCenter: Result := PointF(aRect.Left+dx, aRect.Top);
      taTopRight: Result := PointF(aRect.Right-FComputedSize.cx, aRect.Top);
      taCenterLeft: Result := PointF(aRect.Left, aRect.Top+dy);
      taCenterCenter: Result := PointF(aRect.Left+dx, aRect.Top+dy);
      taCenterRight: Result := PointF(aRect.Right-FComputedSize.cx, aRect.Top+dy);
      taBottomLeft: Result := PointF(aRect.Left, aRect.Bottom-FComputedSize.cy);
      taBottomCenter: Result := PointF(aRect.Left+dx, aRect.Bottom-FComputedSize.cy);
      taBottomRight: Result := PointF(aRect.Right-FComputedSize.cx, aRect.Bottom-FComputedSize.cy);
    end;
  end;
end;

{ TMouseManager }

destructor TMouseManager.Destroy;
begin
  FSprite.Free;
  FSprite := NIL;
  inherited Destroy;
end;

function TMouseManager.IsOver(aGUISurface: TUIClickableObject): boolean;
var xx, yy: integer;
  r, rpar: TRect;
  par: TSimpleSurfaceWithEffect;
begin
  if (aGUISurface = NIL) or (aGUISurface.FKill = True) or not FMousePoolEnabled then exit(False);
  if (FSurfaceLockedByMouse <> NIL) and (FSurfaceLockedByMouse <> aGUISurface) then exit(False);
  if not aGUISurface.MouseInteractionEnabled or not aGUISurface.Visible then exit(False);
  if aGUISurface.Opacity.Value = 0 then exit(False);

  xx := Round(aGUISurface.FSurfaceOrigin.x) + aGUISurface.ScrollOffset.x;
  yy := Round(aGUISurface.FSurfaceOrigin.y) + aGUISurface.ScrollOffset.y;
  r := Rect(xx, yy, xx+aGUISurface.Width, yy+aGUISurface.Height);
  par := aGUISurface.ParentSurface;
  while par <> NIL do begin
    xx := Round(par.FSurfaceOrigin.x);
    yy := Round(par.FSurfaceOrigin.y);
    if (par is TUIClickableWithBodyShape) and TUIClickableWithBodyShape(par).ChildClippingEnabled then begin
      xx := xx + TUIClickableWithBodyShape(par).ScrollOffset.x;
      yy := yy + TUIClickableWithBodyShape(par).ScrollOffset.y;
      rpar := Rect(xx, yy, xx+par.Width, yy+par.Height);
      if not CheckAndAdjust(rpar, r) then exit(False);
    end;
    par := par.ParentSurface;
  end;

  Result := (FMousePos.x >= r.Left) and (FMousePos.x <= r.Right) and
            (FMousePos.y >= r.Top) and (FMousePos.y <= r.Bottom);



 {
  with aGUISurface do begin
    // we use ScrollOffset because the surface can be a child of a scrollable parent
    xx := Round(aGUISurface.FSurfaceOrigin.x) + ScrollOffset.x;
    yy := Round(aGUISurface.FSurfaceOrigin.y) + ScrollOffset.y;
    Result := (FMousePos.x >= xx) and
              (FMousePos.x <= xx+Width) and
              (FMousePos.y >= yy) and
              (FMousePos.y <= yy+Height);
  end;  }

  // because a surface can be a child and outside its parent ClientArea
//  if Result then
//    Result := CheckIfPosIsInParentClientArea(aGUISurface, FMousePos);

  if Result then
    FSurfaceUnderMouse := aGUISurface;
end;

function TMouseManager.IsOver(aRect: TRect): boolean;
begin
  Result := (FMousePos.x >= aRect.Left) and
            (FMousePos.x <= aRect.Right) and
            (FMousePos.y >= aRect.Top) and
            (FMousePos.y <= aRect.Bottom);
end;

procedure TMouseManager.SetSystemMouseCursorVisible(AValue: boolean);
begin
  FSystemMouseCursorVisible := AValue;
  FParentScene.SetSystemMouseCursorVisible(AValue);
end;

function TMouseManager.GetButtonState(btn: TMouseButton): boolean;
begin
  Result := FMouseButtonState[btn];
end;

procedure TMouseManager.SetMousePoolEnabled(AValue: boolean);
begin
  FMousePoolEnabled := AValue;
  if not FMousePoolEnabled then begin
    FSurfaceUnderMouse := NIL;
    FSurfaceLockedByMouse := NIL;
  end;
end;

function TMouseManager.CheckIfPosIsInParentClientArea(aChild: TSimpleSurfaceWithEffect; const aScenePt: TPoint): boolean;
var o: TUIClickableObject;
  p: TPoint;
begin
  Result := True;
  if (aChild is TUIClickableObject) and
     (aChild.ParentSurface <> NIL) and
     (aChild.ParentSurface is TUIClickableWithBodyShape) and
     TUIClickableWithBodyShape(aChild.ParentSurface).ChildClippingEnabled then begin
    o := TUIClickableObject(aChild);
    p := o.SceneToSurfaceUI(aScenePt);
    p := o.SurfaceToParentUI(p, NIL) {- o.ScrollOffset};
    Result := TUIClickableWithBodyShape(aChild.ParentSurface).ClientArea.Contains(p);
  end;
end;

procedure TMouseManager.ProcessOnMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseButtonState[Button] := True;
  FMousePos.x := X;
  FMousePos.y := Y;

  if FSurfaceLockedByMouse <> NIL then begin
    FSurfaceLockedByMouse.DoOnMouseDown(Button, Shift, FSurfaceLockedByMouse.SceneToSurfaceUI(Point(X, Y))-FSurfaceLockedByMouse.ScrollOffset)

  end else if FSurfaceUnderMouse <> NIL then begin
//    if not CheckIfPosIsInParentClientArea(FSurfaceUnderMouse, Point(X, Y)) then exit;

    FSurfaceUnderMouse.DoOnMouseDown(Button, Shift, FSurfaceUnderMouse.SceneToSurfaceUI(Point(X, Y))-FSurfaceUnderMouse.ScrollOffset);
    FSurfaceLockedByMouse := FSurfaceUnderMouse;
  end;
end;

procedure TMouseManager.ProcessOnMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseButtonState[Button] := False;
  FMousePos.x := X;
  FMousePos.y := Y;

  if FSurfaceLockedByMouse <> NIL then begin
    FSurfaceLockedByMouse.DoOnMouseUp(Button, Shift, FSurfaceLockedByMouse.SceneToSurfaceUI(Point(X, Y))-FSurfaceLockedByMouse.ScrollOffset);
    FSurfaceLockedByMouse := NIL;
  end else if FSurfaceUnderMouse <> NIL then begin
//    if not CheckIfPosIsInParentClientArea(FSurfaceUnderMouse, Point(X, Y)) then exit;

    FSurfaceUnderMouse.DoOnMouseUp(Button, Shift, FSurfaceUnderMouse.SceneToSurfaceUI(Point(X, Y))-FSurfaceUnderMouse.ScrollOffset)
  end else if FOnClickOnScene <> NIL then FOnClickOnScene(Button, Shift, X, Y);
end;

procedure TMouseManager.ProcessOnMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  FMousePos.x := X;
  FMousePos.y := Y;

  if FSurfaceLockedByMouse <> NIL then begin
      FSurfaceLockedByMouse.DoOnMouseMove(Shift, FSurfaceLockedByMouse.SceneToSurfaceUI(Point(X, Y))-FSurfaceLockedByMouse.ScrollOffset)
  end else if FSurfaceUnderMouse <> NIL then begin
//      if CheckIfPosIsInParentClientArea(FSurfaceUnderMouse, Point(X,Y)) then
        FSurfaceUnderMouse.DoOnMouseMove(Shift, FSurfaceUnderMouse.SceneToSurfaceUI(Point(X, Y))-FSurfaceUnderMouse.ScrollOffset);
  end;
end;

procedure TMouseManager.ProcessOnMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if FSurfaceUnderMouse <> NIL then
    FSurfaceUnderMouse.DoOnMouseWheel(Shift, WheelDelta, FSurfaceUnderMouse.SceneToSurfaceUI(MousePos)-FSurfaceUnderMouse.ScrollOffset, Handled);
end;

procedure TMouseManager.PrepareBeforeUpdate;
begin
  FSurfaceUnderMouse := NIL;
end;

procedure TMouseManager.UpDate(const AElapsedTime: single);
begin
  FMousePos := FParentScene.GetMouseCoorOnViewPort;

  if FSprite <> NIL then begin
    FSprite.Update(AElapsedTime);
    FSprite.X.Value := FMousePos.x - FClickPointOffset.x;
    FSprite.Y.Value := FMousePos.y - FClickPointOffset.y;
  end;
end;

procedure TMouseManager.Draw;
begin
  if (FSprite <> NIL) and FSprite.Visible then FSprite.Draw(1.0);
end;

constructor TMouseManager.Create(aParentScene: TOGLCScene);
begin
  FParentScene := aParentScene;
  FSystemMouseCursorVisible := True;
  FMousePoolEnabled := True;
end;


procedure TMouseManager.SetCursorSprite(aSprite: TSprite; aClickPointOffset: TPointF);
begin
  DeleteCursorSprite;
  FSprite := aSprite;
  FSprite.ParentScene := FParentScene;
  FClickPointOffset := aClickPointOffset;

  SystemMouseCursorVisible := False;
end;

procedure TMouseManager.SetCursorSprite(aTexture: PTexture; aTextureOwner: boolean; aClickPointOffset: TPointF);
begin
  SetCursorSprite(TSprite.Create(aTexture, aTextureOwner), aClickPointOffset);
end;

procedure TMouseManager.DeleteCursorSprite;
begin
  FSprite.Free;
  FSprite := NIL;
  SystemMouseCursorVisible := True;
end;

{$endif oglcIMPLEMENTATION}


