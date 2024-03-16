{$ifdef oglcINTERFACE}

function CurveIDToString(ACurveID: word): string;
function StringToCurveID(ACurveName: string): word;

type

  TParamState = (psNO_CHANGE=0,    // parameter value is not changing
                 psADD_CONSTANT,   // adding a constant
                 psUSE_CURVE,      // using a velocity curve
                 psCHANGING);      // returned by parameter with multiple sub-param
                                   // to indicate one of its sub-param is changing


  TCustomParam = class;

  { TDataCurve }

  TDataCurve = class
  protected
    FID: word;
    FName: string;
    FImage: TBGRABitmap;
    function GetPointCount: integer;
    procedure CreateExtremities;
  public
    Points: array of TPointF;
    constructor Create;
    destructor Destroy; override;
    // tools for curve construct
    procedure Clear(aCreateExtremities: boolean = True);

    procedure DeletePoint(aIndex: integer);
    procedure CopyPointsFrom(const aSource: array of TPointf);
    function ValidIndex(aIndex: integer): boolean;
    // Gives an TBGRABitmap with rendered curve. Don't free it, it's done internally
    function GetBGRABitmapImage( aImageWidth, aImageHeight: integer;
                                 DrawInvertedCurve: boolean = True): TBGRABitmap;

    property Name: string read FName write FName;
    property ID: word read FID;
    property PointCount: integer read GetPointCount;
  end;

  { TVelocityCurve }
  TVelocityCurve = class
  private
    FDataCurveToUse: TDataCurve;
    FFinished, FInvert: boolean;
    FX, FDuration, FYOrigin, FYTarget, FDeltaY: single;
    FCurrentIndexPoint1: integer;
    a, x1, y1, x2, y2: single;
    procedure GetSegment;
  public
    constructor Create;
    // initiate calculation
    procedure InitParameters(aCurrentValue, aTargetValue, aSeconds: single;
      aCurveID: word = idcLinear);
    // Computes and return the new value according to elapsed time
    function Compute(const AElapsedSec: single): single;
    property Finished: boolean read FFinished write FFinished;
  end;



  { TDataCurveList }
  TDataCurveList = class
  private
    FList: TList;
    FNumID: integer;
    function GetDataCurveCount: integer;
    function NextID: integer;
  public
    procedure Clear;
    procedure DeleteByIndex(aIndex: integer);
    procedure DeleteByID(aIDCurve: word);
    procedure DeleteByName(ACurveName: string);
  public
    constructor Create;
    destructor Destroy; override;
    // Add new curve to the list. Return the ID of the created curve
    function AddCurve(const aName: string; const Pts: array of TPointF): word;
    function GetCurveByID(aID: word): TDataCurve;
    function GetCurveByIndex(aIndex: integer): TDataCurve;
    function CurveNameAlreadyExist(const aCurveName: string): boolean;
    function ValidCurveID( aID: word ): boolean;
    function ValidCurveIndex( aIndex: integer ): boolean;
    function IndexOf( aID: word ): integer;
    property Count: integer read GetDataCurveCount;
  end;


type

  TCustomParam = class
    procedure OnElapse(const AElapsedSec: single); virtual; abstract;
  end;


  TFParam = class(TCustomParam)
  private
    FValue, FConstPerSecond: single;
    FState: TParamState;
    FCurve: TVelocityCurve;
  protected
    function GetValue: single; virtual;
    procedure SetValue(AValue: single); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    // return True if the value have changed
    procedure OnElapse(const AElapsedSec: single); override;

    // use velocity curve
    procedure ChangeTo(aNewValue, aSecond: single; aCurveID: word = idcLinear); virtual;
    // add a constant per second
    procedure AddConstant(aConstPerSecond: single);
    // Current value of the parameter. Setting a value, stop an "ChangeTo" or "AddConstant" action.
    property Value: single read GetValue write SetValue;
    property State: TParamState read FState;
  end;

type
  { TBoundedFParam }
  // parameter with boundary

  TBoundedFParam = class(TFParam)
  private
    function GetpcValue: single;
    procedure SetpcValue(AValue: single);
    procedure ClampPercent( var AValue: single );
  protected
    function GetValue: single; override;
    procedure SetValue(AValue: single); override;
    procedure ApplyBounds(var AValue: single);
  public
    MinValue, MaxValue: single;
    Loop: boolean;
    // if Loop is set to TRUE, value can loop between bounds (usefull for i.e. [0..360] angle)
    // if it's set to FALSE, value is clamped to MinValue and MaxValue.
    procedure SetBoundary(aMin, aMax: single; aLoop: boolean = False);
    // return True if the value have changed
    procedure OnElapse(const AElapsedSec: single); override;
    // percentage range is [0..1], 0 is MinValue and 1 is MaxValue (see SetBoundary)
    function PercentToValue(aPercentage: single): single;
    function ValueToPercent(aValue: single): single;
    function pcRandomValueBetween(PercentageMin, PercentageMax: single): single;
    procedure pcChangeTo(aNewPercentValue, aSecond: single; aCurveID: word = idcLinear);
    property pcValue: single read GetpcValue write SetpcValue;
  end;

function CreateBoundedFParam(Min, Max: single; Loop: boolean = False): TBoundedFParam;


type
  { TPointFParam }

  TPointFParam = class(TCustomParam)
  private
    function GetPointF: TPointF;
    function GetState: TParamState;
    procedure SetValue(AValue: TPointF);
  public
    x, y: TFParam;
    constructor Create;
    destructor Destroy; override;
    // return True if the value have changed
    procedure OnElapse(const AElapsedSec: single); override;
    procedure ChangeTo(aNewValue: TPointF; aSeconds: single; aCurveID: word = idcLinear);
    property Value: TPointF read GetPointF write SetValue;
    property State: TParamState read GetState;
  end;


  // Polar coordinates represented by an angle in degree and a distance
  TPolarCoor = record
    Distance,
    Angle: single;
  end;
  function PolarCoor( aDistance, aAngle: single ): TPolarCoor;
  function CartesianToPolar( const aCenter, aPoint: TPointF ): TPolarCoor;
  function PolarToCartesian( const aCenter: TPointF; const aPoint: TPolarCoor ): TPointF;

type
  { TPolarSystemParam }

  TPolarSystemParam = class(TCustomParam)
  private
    function GetCartesianValue: TPointF;
    function GetPolarCoor: TPolarCoor;
    function GetState: TParamState;
    procedure SetCartesianValue(AValue: TPointF);
    procedure SetPolarCoor(aValue: TPolarCoor);
  public
    constructor Create;
    destructor Destroy; override;
    // return True if the value have changed
    procedure OnElapse(const AElapsedSec: single); override;
  public
    Center: TPointFParam;   // center of the polar system coordinates
    Distance: TFParam;      // distance of the point in the system
    Angle: TFParam;         // angle of the point in the systeme
    procedure ChangeTo(aNewValue: TPolarCoor; aSeconds: single; aCurveID: word = idcLinear);
    property Value: TPolarCoor read GetPolarCoor write SetPolarCoor;
    property CartesianValue: TPointF read GetCartesianValue write SetCartesianValue;
    property State: TParamState read GetState;
  end;


  { TQuadParamF }

  TQuadParamF = class(TCustomParam)
  private
    function GetState: TParamState;
  public
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft: TPointFParam;
    constructor Create;
    destructor Destroy; override;
    // return True if the value have changed
    procedure OnElapse(const AElapsedSec: single); override;
    property State: TParamState read GetState;
  end;


{ TBGRAParam }
  TBGRAParam = class(TCustomParam)
  private
    function GetBGRAPixel: TBGRAPixel;
    function GetState: TParamState;
    procedure SetBGRAPixel(aValue: TBGRAPixel);
  public
    Red, Green, Blue, Alpha: TBoundedFParam;
    constructor Create;
    destructor Destroy; override;
    // return True if the value have changed
    procedure OnElapse(const AElapsedSec: single); override;
    procedure ChangeTo(aNewValue: TBGRAPixel; aSeconds: single; aCurveID: word = idcLinear);
    property Value: TBGRAPixel read GetBGRAPixel write SetBGRAPixel;
    property State: TParamState read GetState;
  end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

var
  VelocityCurveList: TDataCurveList=NIL;
  FVelocityCurveListRefCount: integer=0;


function CurveIDToString(ACurveID: word): string;
begin
  case ACurveID of
    idcStartFastEndSlow: Result := 'StartFastEndSlow';
    idcStartSlowEndFast: Result := 'StartSlowEndFast';
    idcSinusoid: Result := 'Sinusoid';
    idcSinusoid2: Result := 'Sinusoid2';
    idcBouncy: Result := 'Bouncy';
    idcSpring: Result := 'Spring';
    idcSpring2: Result := 'Spring2';
    idc5steps: Result := '5Steps';
    idcExtend: Result := 'Extend';
    idcExtend2: Result := 'Extend2';
    idcDrop: Result := 'Drop';
    idcSlowAtMiddle: Result := 'SlowAtMiddle';
    idcPauseAtMiddle: Result := 'PauseAtMiddle';
    idcSingleRebound: Result := 'SingleRebound'
    else
      Result := 'Linear';
  end;
end;

function StringToCurveID(ACurveName: string): word;
begin
  case LowerCase(ACurveName) of
    'startfastendslow': Result := idcStartFastEndSlow;
    'startslowendfast': Result := idcStartSlowEndFast;
    'sinusoid': Result := idcSinusoid;
    'sinusoid2': Result := idcSinusoid2;
    'bouncy': Result := idcBouncy;
    'spring': Result := idcSpring;
    'spring2': Result := idcSpring2;
    '5steps': Result := idc5steps;
    'extend': Result := idcExtend;
    'extend2': Result := idcExtend2;
    'drop': Result := idcDrop;
    'slowatmiddle': Result := idcSlowAtMiddle;
    'pauseatmiddle': Result := idcPauseAtMiddle;
    'SingleRebound': Result := idcSingleRebound
    else
      Result := idcLinear;
  end;
end;

function CreateBoundedFParam(Min, Max: single; Loop: boolean): TBoundedFParam;
begin
  Result := TBoundedFParam.Create;
  Result.SetBoundary(Min, Max);
  Result.Loop := Loop;
end;

function PolarCoor(aDistance, aAngle: single): TPolarCoor;
begin
 Result.Distance:=aDistance;
 Result.Angle:=aAngle;
end;

// https://en.wikipedia.org/wiki/Polar_coordinate_system#Converting_between_polar_and_Cartesian_coordinates
function CartesianToPolar(const aCenter, aPoint: TPointF): TPolarCoor;
var xx,yy:single;
begin
 xx := aPoint.x-aCenter.x;
 yy := aPoint.y-aCenter.y;
 with Result do begin
    Distance := sqrt( xx*xx+yy*yy );
    if (xx>0) and (yy>=0)
    then Angle := arctan(yy/xx)*Rad2Deg
    else if (xx>0) and (yy<0)
         then Angle := (arctan(yy/xx)+2*PI)*Rad2Deg
         else if xx<0
              then Angle := (arctan(yy/xx)+PI)*Rad2Deg
              else if (xx=0) and (yy>0)
                   then Angle := Pi/2*Rad2Deg
                   else if (xx=0) and (yy<0)
                        then Angle := 1.5*PI*Rad2Deg
                        else Angle :=0;
 end;
end;

function PolarToCartesian(const aCenter: TPointF; const aPoint: TPolarCoor): TPointF;
begin
  Result.x := aCenter.x+cos(aPoint.Angle*Deg2Rad)*aPoint.Distance;
  Result.y := aCenter.y+sin(aPoint.Angle*Deg2Rad)*aPoint.Distance;
end;

{ TQuadParamF }

function TQuadParamF.GetState: TParamState;
begin
  if (TopLeft.State = psNO_CHANGE) and
     (TopRight.State = psNO_CHANGE) and
     (BottomRight.State = psNO_CHANGE) and
     (BottomLeft.State = psNO_CHANGE) then Result := psNO_CHANGE
  else if (TopLeft.State = psADD_CONSTANT) and
     (TopRight.State = psADD_CONSTANT) and
     (BottomRight.State = psADD_CONSTANT) and
     (BottomLeft.State = psADD_CONSTANT) then Result := psADD_CONSTANT
  else if (TopLeft.State = psUSE_CURVE) and
     (TopRight.State = psUSE_CURVE) and
     (BottomRight.State = psUSE_CURVE) and
     (BottomLeft.State = psUSE_CURVE) then Result := psUSE_CURVE
  else Result := psCHANGING;
end;

constructor TQuadParamF.Create;
begin
  TopLeft := TPointFParam.Create;
  TopRight := TPointFParam.Create;
  BottomRight := TPointFParam.Create;
  BottomLeft := TPointFParam.Create;
end;

destructor TQuadParamF.Destroy;
begin
  FreeAndNil(TopLeft);
  FreeAndNil(TopRight);
  FreeAndNil(BottomRight);
  FreeAndNil(BottomLeft);
  inherited Destroy;
end;

procedure TQuadParamF.OnElapse(const AElapsedSec: single);
begin
  TopLeft.OnElapse(AElapsedSec);
  TopRight.OnElapse(AElapsedSec);
  BottomRight.OnElapse(AElapsedSec);
  BottomLeft.OnElapse(AElapsedSec);
end;

{ TPolarSystemParam }

function TPolarSystemParam.GetCartesianValue: TPointF;
begin
  Result.x := Center.Value.x+cos(Angle.Value*Deg2Rad)*Distance.Value;
  Result.y := Center.Value.y+sin(Angle.Value*Deg2Rad)*Distance.Value;
end;

function TPolarSystemParam.GetPolarCoor: TPolarCoor;
begin
 Result.Angle:=Angle.Value;
 Result.Distance:=Distance.Value;
end;

function TPolarSystemParam.GetState: TParamState;
begin
 if (Distance.State = psNO_CHANGE) and (Angle.State = psNO_CHANGE) and
    (Center.State = psNO_CHANGE) then
   Result := psNO_CHANGE
 else if (Distance.State = psADD_CONSTANT) and (Angle.State = psADD_CONSTANT) and
         (Center.State = psADD_CONSTANT) then
   Result := psADD_CONSTANT
 else if (Distance.State = psUSE_CURVE) and (Angle.State = psUSE_CURVE) and
         (Center.State = psUSE_CURVE) then
   Result := psUSE_CURVE
 else Result := psCHANGING;
end;

procedure TPolarSystemParam.SetCartesianValue(AValue: TPointF);
var xx,yy:single;
begin
  xx := AValue.x-Center.Value.x;
  yy := AValue.y-Center.Value.y;

  Distance.Value := sqrt( xx*xx+yy*yy );
  if (xx>0) and (yy>=0)
    then Angle.Value := arctan(yy/xx)*Rad2Deg
    else if (xx>0) and (yy<0)
         then Angle.Value := (arctan(yy/xx)+2*PI)*Rad2Deg
         else if xx<0
              then Angle.Value := (arctan(yy/xx)+PI)*Rad2Deg
              else if (xx=0) and (yy>0)
                   then Angle.Value := Pi/2*Rad2Deg
                   else if (xx=0) and (yy<0)
                        then Angle.Value := 1.5*PI*Rad2Deg
                        else Angle.Value :=0;
end;

procedure TPolarSystemParam.SetPolarCoor(aValue: TPolarCoor);
begin
 Distance.Value := aValue.Distance;
 Angle.Value := aValue.Angle;
end;

constructor TPolarSystemParam.Create;
begin
 inherited Create;
 Center := TPointFParam.Create;
 Distance := TFParam.Create;
 Angle := TFParam.Create;
end;

destructor TPolarSystemParam.Destroy;
begin
 Center.Free;
 Distance.Free;
 Angle.Free;
 inherited Destroy;
end;

procedure TPolarSystemParam.OnElapse(const AElapsedSec: single);
begin
  Center.OnElapse(AElapsedSec);
  Distance.OnElapse(AElapsedSec);
  Angle.OnElapse(AElapsedSec);
end;

procedure TPolarSystemParam.ChangeTo(aNewValue: TPolarCoor; aSeconds: single; aCurveID: word);
begin
 Distance.ChangeTo( aNewValue.Distance, aSeconds, aCurveID );
 Angle.ChangeTo( aNewValue.Angle, aSeconds, aCurveID );
end;


{ TBGRAParam }

function TBGRAParam.GetBGRAPixel: TBGRAPixel;
begin
  Result.red := round(red.Value);
  Result.green := round(green.Value);
  Result.blue := round(blue.Value);
  Result.alpha := round(alpha.Value);
end;

function TBGRAParam.GetState: TParamState;
begin
 if (Red.State = psNO_CHANGE) and (Green.State = psNO_CHANGE) and
    (Blue.State = psNO_CHANGE) and (Alpha.State = psNO_CHANGE) then
   Result := psNO_CHANGE
   else if (Red.State = psADD_CONSTANT) or (Green.State = psADD_CONSTANT) or
    (Blue.State = psADD_CONSTANT) or (Alpha.State = psADD_CONSTANT) then
   Result := psADD_CONSTANT
   else Result := psUSE_CURVE;
end;

procedure TBGRAParam.SetBGRAPixel(aValue: TBGRAPixel);
begin
  red.Value := aValue.red;
  green.Value := aValue.green;
  blue.Value := aValue.blue;
  alpha.Value := aValue.alpha;
end;

constructor TBGRAParam.Create;
begin
  red := CreateBoundedFParam(0, 255);
  green := CreateBoundedFParam(0, 255);
  blue := CreateBoundedFParam(0, 255);
  alpha := CreateBoundedFParam(0, 255);
end;

destructor TBGRAParam.Destroy;
begin
 FreeAndNil( red );
 FreeAndNil( green );
 FreeAndNil( blue );
 FreeAndNil( alpha );
 inherited Destroy;
end;

procedure TBGRAParam.OnElapse(const AElapsedSec: single);
begin
  red.OnElapse(AElapsedSec);
  green.OnElapse(AElapsedSec);
  blue.OnElapse(AElapsedSec);
  alpha.OnElapse(AElapsedSec);
end;

procedure TBGRAParam.ChangeTo(aNewValue: TBGRAPixel; aSeconds: single; aCurveID: word);
begin
  red.ChangeTo(aNewValue.red, aSeconds, aCurveID);
  green.ChangeTo(aNewValue.green, aSeconds, aCurveID);
  blue.ChangeTo(aNewValue.blue, aSeconds, aCurveID);
  alpha.ChangeTo(aNewValue.alpha, aSeconds, aCurveID);
end;

{ TPointFParam }

function TPointFParam.GetPointF: TPointF;
begin
  Result.x := x.Value;
  Result.y := y.Value;
end;

function TPointFParam.GetState: TParamState;
begin
  if (x.State = psNO_CHANGE) and (y.State = psNO_CHANGE) then
    Result := psNO_CHANGE
  else if (x.State = psADD_CONSTANT) and (y.State = psADD_CONSTANT) then
    Result := psADD_CONSTANT
  else if (x.State = psUSE_CURVE) and (y.State = psUSE_CURVE) then
    Result := psUSE_CURVE
  else
    Result := psCHANGING;
end;

procedure TPointFParam.SetValue(AValue: TPointF);
begin
  x.Value := AValue.x;
  y.Value := AValue.y;
end;

constructor TPointFParam.Create;
begin
  x := TFParam.Create;
  y := TFParam.Create;
end;

destructor TPointFParam.Destroy;
begin
  FreeAndNil(x);
  FreeAndNil(y);
  inherited Destroy;
end;

procedure TPointFParam.OnElapse(const AElapsedSec: single);
begin
  x.OnElapse(AElapsedSec);
  y.OnElapse(AElapsedSec);
end;

procedure TPointFParam.ChangeTo(aNewValue: TPointF; aSeconds: single; aCurveID: word);
begin
  x.ChangeTo(aNewValue.x, aSeconds, aCurveID);
  y.ChangeTo(aNewValue.y, aSeconds, aCurveID);
end;

{ TDataCurve }

function TDataCurve.GetPointCount: integer;
begin
  Result := Length(Points);
end;

procedure TDataCurve.CreateExtremities;
begin
  SetLength(Points, 2);
  Points[0] := PointF(0, 1);
  Points[1] := PointF(1, 0);
end;

constructor TDataCurve.Create;
begin
  inherited Create;
  CreateExtremities;
end;

destructor TDataCurve.Destroy;
begin
  Clear(False);
  {$IFDEF _UseBGRA}
  if FImage <> NIL then FImage.Free;
  {$ENDIF}
  inherited Destroy;
end;

procedure TDataCurve.Clear(aCreateExtremities: boolean);
begin
  SetLength(Points, 0);
  if aCreateExtremities then
    CreateExtremities;
end;

procedure TDataCurve.DeletePoint(aIndex: integer);
var
  i: integer;
begin
  if (aIndex < 1) or (aIndex > Length(Points) - 2) then
    exit;
  for i := GetPointCount - 1 downto aIndex do
    Points[i - 1] := Points[i];
  SetLength(Points, Length(Points) - 1);
end;

procedure TDataCurve.CopyPointsFrom(const aSource: array of TPointf);
var
  i: integer;
begin
  Clear(False);
  SetLength(Points, Length(aSource));
  for i := 0 to Length(aSource) - 1 do
    Points[i] := aSource[i];
end;

function TDataCurve.ValidIndex(aIndex: integer): boolean;
begin
  Result := (aIndex >= 0) and (aIndex < GetPointCount);
end;

// Gives an TBGRABitmap with rendered curve. Don't free it, it's done internally
function TDataCurve.GetBGRABitmapImage( aImageWidth, aImageHeight: integer;
                             DrawInvertedCurve: boolean = True): TBGRABitmap;
var
  x1, y1, x2, y2, w: single;
  i: integer;
  cline, clineinvert, c: TBGRAPixel;
begin
  if FImage <> nil then
  begin
    if (FImage.Width = aImageWidth) and (FImage.Height = aImageHeight) then
    begin
      Result := FImage;
      exit;
    end
    else
      FreeAndNil(FImage);
  end;

  FImage := TBGRABitmap.Create(aImageWidth, aImageHeight, BGRA(50, 20, 20));
  cline := BGRA(255, 140, 0);
  clineinvert := BGRA(60, 24, 24); // BGRA(20, 80, 100);
  with FImage do
  begin
    if (Width > 20) and (Height > 20) then
      w := 1.5
    else
      w := 0.5;
    // inverted curve
    if DrawInvertedCurve then
    begin
      for i := 1 to GetPointCount - 1 do
      begin
        with Points[i - 1] do
        begin
          x1 := x * Width;
          y1 := Height * (1 - y);
        end;
        with Points[i] do
        begin
          x2 := x * Width;
          y2 := Height * (1 - y);
        end;
        DrawLineAntialias(x1, y1, x2, y2, clineinvert, 2);
      end;
    end;
    // axis
    c := BGRA(150, 100, 100);
    HorizLine(0, Height - 1, Width, c, dmSet);
    DrawLine(0, 0, 0, Height, c, True, dmSet);
    if (Width > 20) and (Height > 20) then
    begin
      HorizLine(0, Height - 2, Width, c, dmSet);
      DrawLine(1, 0, 1, Height, c, True, dmSet);
    end;
    // normal curve
    for i := 1 to GetPointCount - 1 do
    begin
      with Points[i - 1] do
      begin
        x1 := x * Width;
        y1 := y * Height;
      end;
      with Points[i] do
      begin
        x2 := x * Width;
        y2 := y * Height;
      end;
      DrawLineAntialias(x1, y1, x2, y2, cline, w);
    end;
  end;
  Result := FImage;
end;

{ TBoundedFParam }

function TBoundedFParam.GetpcValue: single;
begin
  Result := ValueToPercent(FValue);
end;

procedure TBoundedFParam.SetpcValue(AValue: single);
var
  v: single;
begin
  v := PercentToValue(AValue);
  SetValue(v);
end;

procedure TBoundedFParam.ClampPercent(var AValue: single);
begin
 if AValue < 0.0
   then AValue := 0.0
   else if AValue > 1.0
          then Avalue := 1.0;
end;

function TBoundedFParam.GetValue: single;
begin
 Result := FValue;
 ApplyBounds( Result );
end;

procedure TBoundedFParam.SetValue(AValue: single);
begin
  ApplyBounds({%H-}AValue);
  inherited SetValue({%H-}AValue);
end;

procedure TBoundedFParam.ApplyBounds(var AValue: single);
var
  delta: single;
begin
  if Loop then
  begin  // loop mode
    delta := MaxValue - MinValue;
    while AValue < MinValue do AValue += delta;
    while AValue > MaxValue do AValue -= delta;
  end
  else
  begin  // clamp mode
    if AValue < MinValue then
      AValue := MinValue
    else if AValue > MaxValue then
      AValue := MaxValue;
  end;
end;

procedure TBoundedFParam.SetBoundary(aMin, aMax: single; aLoop: boolean);
begin
  if aMin > aMax then
  begin
    MinValue := aMax;
    MaxValue := aMin;
  end
  else
  begin
    MinValue := aMin;
    MaxValue := aMax;
  end;

  Loop := aLoop;
end;

function TBoundedFParam.PercentToValue(aPercentage: single): single;
begin
  ClampPercent( {%H-}aPercentage );
  Result := ( MaxValue - MinValue ) * aPercentage + MinValue;
end;

function TBoundedFParam.ValueToPercent(aValue: single): single;
begin
  Result := (aValue - MinValue) / (MaxValue - MinValue);
end;

function TBoundedFParam.pcRandomValueBetween(PercentageMin, PercentageMax:
  single): single;
var
  p: single;
begin
  ClampPercent( {%H-}PercentageMin );
  ClampPercent( {%H-}PercentageMax );

  if PercentageMin > PercentageMax then
  begin
    p := PercentageMax;
    PercentageMax := PercentageMin;
    PercentageMin := p;
  end;

  p := random(round((PercentageMax - PercentageMin) * 1000000.0)) *
    0.0000001 + PercentageMin;
  Result := PercentToValue(p);
end;

procedure TBoundedFParam.pcChangeTo(aNewPercentValue, aSecond: single; aCurveID: word);
var
  v: single;
begin
  ClampPercent( {%H-}aNewPercentValue );
  v := PercentToValue(aNewPercentValue);
  ChangeTo(v, aSecond, aCurveID);
end;

procedure TBoundedFParam.OnElapse(const AElapsedSec: single);
begin
  case FState of
    psADD_CONSTANT:
    begin
      FValue += FConstPerSecond * AElapsedSec;
      if not Loop then
      begin
        if FValue <= MinValue then
        begin
          FValue := MinValue;
          FState := psNO_CHANGE;
        end
        else if FValue >= MaxValue then
        begin
          FValue := MaxValue;
          FState := psNO_CHANGE;
        end;
      end;
    end;

    psUSE_CURVE:
    begin
      FValue := FCurve.Compute(AElapsedSec);
      if FCurve.Finished then
        FState := psNO_CHANGE;
    end;
  end;
end;


{ TFParam }


function TFParam.GetValue: single;
begin
  Result := FValue;
end;

procedure TFParam.SetValue(AValue: single);
begin
  FValue := AValue;
  FState := psNO_CHANGE;
end;

constructor TFParam.Create;
begin
  FState := psNO_CHANGE;
  FValue := 0.0;
  FConstPerSecond := 0.0;
  FCurve := TVelocityCurve.Create;
end;

destructor TFParam.Destroy;
begin
  FreeAndNil(FCurve);
  inherited Destroy;
end;

procedure TFParam.ChangeTo(aNewValue, aSecond: single; aCurveID: word);
begin
  if aSecond <= 0 then
  begin
    SetValue(aNewValue);
    exit;
  end;

  if aNewValue <> FValue then
  begin
    FState := psUSE_CURVE;
    FCurve.InitParameters(FValue, aNewValue, aSecond, aCurveID);
  end
  else
    FState := psNO_CHANGE;
end;

procedure TFParam.AddConstant(aConstPerSecond: single);
begin
  if aConstPerSecond <> 0 then
  begin
    FState := psADD_CONSTANT;
    FConstPerSecond := aConstPerSecond;
  end
  else
    FState := psNO_CHANGE;
end;

procedure TFParam.OnElapse(const AElapsedSec: single);
begin
  case FState of
    psADD_CONSTANT: FValue += FConstPerSecond * AElapsedSec;

    psUSE_CURVE:
    begin
      FValue := FCurve.Compute(AElapsedSec);
      if FCurve.FFinished then
        FState := psNO_CHANGE;
    end;
  end;
end;

{ TVelocityCurve }

constructor TVelocityCurve.Create;
begin
  inherited Create;
  FFinished := True;
  FInvert := False;
end;

procedure TVelocityCurve.GetSegment;
var
  fp, fp1: PPointF;
begin
  fp := @FDataCurveToUse.Points[FCurrentIndexPoint1];
  fp1 := @FDataCurveToUse.Points[FCurrentIndexPoint1 + 1];
  x1 := fp^.x;
  x2 := fp1^.x;

  if FInvert then
  begin
    y1 := fp^.y;
    y2 := fp1^.y;
  end
  else
  begin
    y1 := 1 - fp^.y;
    y2 := 1 - fp1^.y;
  end;

  a := (y2 - y1) / (x2 - x1);
end;

procedure TVelocityCurve.InitParameters(aCurrentValue, aTargetValue, aSeconds: single;
  aCurveID: word);
begin
  FX := 0;
  FYOrigin := aCurrentValue;
  FYTarget := aTargetValue;

  FInvert := aCurrentValue > aTargetValue;

  FDeltaY := FYTarget - FYOrigin;
  FDuration := aSeconds;
  FFinished := aSeconds = 0.0;
  FCurrentIndexPoint1 := 0;

  FDataCurveToUse := VelocityCurveList.GetCurveByID(aCurveID);
  if FDataCurveToUse = nil then
    raise Exception.Create('Velocities curves error ! no curve available for ID=' +
      IntToStr(aCurveID) + ')');

  GetSegment;
end;

function TVelocityCurve.Compute(const AElapsedSec: single): single;
var
  xcurve: single;
begin
  FX += AElapsedSec;
  if (FX >= FDuration) or (FDuration <= 0) then
    FFinished := True;
  if FFinished then
  begin
    Result := FYTarget;
    exit;
  end
  else
  begin
    xcurve := FX / FDuration;
    while xcurve > x2 do
    begin
      Inc(FCurrentIndexPoint1);
      GetSegment;
    end;

    if FInvert
      then Result := FYOrigin + (1 - (a * (xcurve - x1) + y1)) * FDeltaY
      else Result := FYOrigin + (a * (xcurve - x1) + y1) * FDeltaY;
  end;
end;




//------------------------------------------------------------------------------------

//                            TDataCurveList




{ TDataCurveList }

constructor TDataCurveList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FNumID := -1;
end;

destructor TDataCurveList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TDataCurveList.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    TDataCurve(FList.Items[i]).Free;
  FList.Clear;
  FNumID := -1;
end;

function TDataCurveList.GetDataCurveCount: integer;
begin
  Result := FList.Count;
end;

function TDataCurveList.NextID: integer;
begin
  Inc(FNumID);
  Result := FNumID;
end;


function TDataCurveList.AddCurve(const aName: string; const Pts: array of TPointF): word;
var
  o: TDataCurve;
begin
  o := TDataCurve.Create;
  o.Name := aName;
  o.FID := NextID;
  o.CopyPointsFrom(Pts);
  FList.Add(o);
  Result := o.ID;
end;

procedure TDataCurveList.DeleteByIndex(aIndex: integer);
begin
  if (aIndex < 0) or (aIndex >= FList.Count) then
    exit;
  TDataCurve(FList.Items[aIndex]).Free;
  FList.Delete(aIndex);
end;

procedure TDataCurveList.DeleteByID(aIDCurve: word);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if GetCurveByIndex(i).ID = aIDCurve then
    begin
      DeleteByIndex(i);
      exit;
    end;
end;

procedure TDataCurveList.DeleteByName(ACurveName: string);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if GetCurveByIndex(i).Name = ACurveName then
    begin
      DeleteByIndex(i);
      exit;
    end;
end;

function TDataCurveList.GetCurveByID(aID: word): TDataCurve;
var
  i: integer;
  dc: TDataCurve;
begin
  Result := GetCurveByIndex(0); // Linear curve by default
  for i := 0 to FList.Count - 1 do
  begin
    dc := GetCurveByIndex(i);
    if dc.ID = aID then
    begin
      Result := dc;
      exit;
    end;
  end;
end;

function TDataCurveList.GetCurveByIndex(aIndex: integer): TDataCurve;
begin
  if not ValidCurveIndex(aIndex)
    then Result := TDataCurve(FList.Items[0])  // Linear curve by default
    else Result := TDataCurve(FList.Items[aIndex]);
end;

function TDataCurveList.CurveNameAlreadyExist(const aCurveName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do
    if GetCurveByIndex(i).FName = aCurveName then
    begin
      Result := True;
      exit;
    end;
end;

function TDataCurveList.ValidCurveID(aID: word): boolean;
begin
 Result := IndexOf( aID )<>-1;
end;

function TDataCurveList.ValidCurveIndex(aIndex: integer): boolean;
begin
 Result := (aIndex >= 0) or (aIndex < FList.Count)
end;

function TDataCurveList.IndexOf(aID: word): integer;
var i: Integer;
begin
 Result := -1;
 for i:=0 to FList.Count-1 do
   if GetCurveByIndex(i).ID = aID then begin
      Result := i;
      exit;
   end;
end;


procedure CreateVelocityCurveList;
begin
  inc(FVelocityCurveListRefCount);
  if FVelocityCurveListRefCount > 1 then exit;

  VelocityCurveList := TDataCurveList.Create;
  VelocityCurveList.AddCurve('Linear', [PointF(0, 1), PointF(1, 0)]);

  VelocityCurveList.AddCurve('StartFastEndSlow', [PointF(0, 1),
    PointF(0.020024182, 0.93652451), PointF(0.040863961, 0.875130057),
    PointF(0.062519327, 0.81581676), PointF(0.084990293, 0.758584738),
    PointF(0.108276844, 0.70343399), PointF(0.132378981, 0.65036422),
    PointF(0.157296717, 0.599375546), PointF(
    0.183030054, 0.550468266), PointF(0.209578991, 0.503642082),
    PointF(0.236943483, 0.458896935),
    PointF(0.265123576, 0.416233003), PointF(
    0.294119269, 0.375650346), PointF(0.323930591, 0.337148845),
    PointF(0.354557455, 0.300728381),
    PointF(0.385999888, 0.266389161), PointF(
    0.418257982, 0.234131128), PointF(0.451331586, 0.20395425),
    PointF(0.48522082, 0.175858527),
    PointF(0.519925535, 0.149843991), PointF(0.555446029, 0.12591061),
    PointF(0.591781974, 0.10405837), PointF(0.628933609, 0.084287308),
    PointF(0.666900694, 0.066597417), PointF(
    0.705683529, 0.050988663), PointF(0.745281875, 0.037461091),
    PointF(0.785695732, 0.026014673),
    PointF(0.826925337, 0.016649416), PointF(
    0.868970394, 0.009365318), PointF(0.911831021, 0.004162385),
    PointF(0.955507398, 0.001040612), PointF(1, 0)]);

  VelocityCurveList.AddCurve('StartSlowEndFast', [PointF(0, 1),
    PointF(0.051241662, 0.998875022), PointF(
    0.101217754, 0.995674491), PointF(0.149928272, 0.990398407),
    PointF(0.197373211, 0.98304683),
    PointF(0.24355258, 0.973619819), PointF(0.288466364, 0.962117136),
    PointF(0.332114607, 0.948538899), PointF(0.374497294, 0.93288511),
    PointF(0.415614307, 0.915156007), PointF(
    0.455465853, 0.895351171), PointF(0.494051784, 0.873470724),
    PointF(0.53137207, 0.849514902),
    PointF(0.56742692, 0.823483407), PointF(0.602216125, 0.79537642),
    PointF(0.635739744, 0.76519376), PointF(0.667997777, 0.732935786),
    PointF(0.698990226, 0.698602259), PointF(0.728717089, 0.66219312),
    PointF(0.757178485, 0.623708546), PointF(0.784374237, 0.58314836),
    PointF(0.810304403, 0.540512621), PointF(
    0.834969103, 0.495801389), PointF(0.85836798, 0.449014604),
    PointF(0.880501449, 0.400152236),
    PointF(0.901369393, 0.349214405), PointF(0.920971751, 0.29620102),
    PointF(0.939308524, 0.241112053), PointF(
    0.956379592, 0.183947578), PointF(0.972185254, 0.124707572),
    PointF(0.98672545, 0.063392036), PointF(1, 0)]);

  VelocityCurveList.AddCurve('Sinusoid', [PointF(0, 1),
    PointF(0.024732787, 0.998385906), PointF(
    0.049185738, 0.993821561), PointF(0.073372178, 0.986447573),
    PointF(0.097305417, 0.976404548),
    PointF(0.1209988, 0.963833213), PointF(0.14446567, 0.948873878),
    PointF(0.167719305, 0.931667387), PointF(
    0.190773025, 0.912354112), PointF(0.213640243, 0.891074657),
    PointF(0.23633422, 0.867969751),
    PointF(0.258868277, 0.843180001), PointF(
    0.281255752, 0.816845596), PointF(0.303509951, 0.789107561),
    PointF(0.325644255, 0.760106564),
    PointF(0.347671956, 0.729982734), PointF(
    0.369606346, 0.698877037), PointF(0.391460717, 0.666929662),
    PointF(0.413248628, 0.634281814),
    PointF(0.434983134, 0.601073563), PointF(
    0.456677645, 0.567445576), PointF(0.478345513, 0.53353852),
    PointF(0.500000119, 0.499493062),
    PointF(0.521654665, 0.465449661), PointF(
    0.543322504, 0.431548983), PointF(0.565017045, 0.397931635),
    PointF(0.586751521, 0.364737958),
    PointF(0.608539283, 0.332108915), PointF(
    0.630393684, 0.300184816), PointF(0.652328074, 0.269106299),
    PointF(0.674355745, 0.23901397),
    PointF(0.69648999, 0.210048467), PointF(0.718744099, 0.182350308),
    PointF(0.741131604, 0.15606007), PointF(0.763665676, 0.131318435),
    PointF(0.786359549, 0.108265862), PointF(
    0.809226751, 0.087043017), PointF(0.832280397, 0.067790486),
    PointF(0.855533957, 0.050648808),
    PointF(0.879000902, 0.035758596), PointF(
    0.902694285, 0.023260433), PointF(0.926627457, 0.013294909),
    PointF(0.95081389, 0.006002605),
    PointF(0.975266695, 0.001524106), PointF(1, 0)]);

  VelocityCurveList.AddCurve('Sinusoid2', [PointF(0, 1),
    PointF(0.036280163, 0.997963548), PointF(0.070699692, 0.992895722),
    PointF(0.103349388, 0.984944582), PointF(0.134319976, 0.974257946),
    PointF(0.16370222, 0.960983336), PointF(0.191586897, 0.945268929),
    PointF(0.218064785, 0.927262723), PointF(0.243226603, 0.907112002),
    PointF(0.267163068, 0.884965181), PointF(0.289965093, 0.86097008),
    PointF(0.311723292, 0.835274339), PointF(0.332528561, 0.808026016),
    PointF(0.35247153, 0.779372931), PointF(0.371643096, 0.749462903),
    PointF(0.390133888, 0.718443811), PointF(0.408034772, 0.686463714),
    PointF(0.425436437, 0.653670132), PointF(0.442429692, 0.620211244),
    PointF(0.459105253, 0.586234868), PointF(0.47555393, 0.551888585),
    PointF(0.491866469, 0.517320752), PointF(0.50813359, 0.48267895),
    PointF(0.52444613, 0.448111027), PointF(0.540894747, 0.413764864),
    PointF(0.557570398, 0.379788399), PointF(0.574563563, 0.34632957),
    PointF(0.591965258, 0.313536048), PointF(0.609866142, 0.281555861),
    PointF(0.628356934, 0.250536829), PointF(0.647528529, 0.220626801),
    PointF(0.667471528, 0.191973701), PointF(0.688276827, 0.164725393),
    PointF(0.710035086, 0.139029726), PointF(0.732836962, 0.115034543),
    PointF(0.756773591, 0.092887774), PointF(0.781935513, 0.072737254),
    PointF(0.808413386, 0.054730862), PointF(0.836297989, 0.03901647),
    PointF(0.865680397, 0.025741952), PointF(0.89665091, 0.015055172),
    PointF(0.929300606, 0.007104006), PointF(0.963720202, 0.002036323),
    PointF(1, 0)]);

  VelocityCurveList.AddCurve('Bouncy',[
    PointF(0,1), PointF(0.0810811,0.972972989),
    PointF(0.1621622,0.932432413), PointF(0.222973019,0.885135114),
    PointF(0.277777791,0.825163364), PointF(0.326797396,0.751634002),
    PointF(0.367647052,0.66993463), PointF(0.392156869,0.596405208),
    PointF(0.418918878,0.493243188), PointF(0.5,0),
    PointF(0.527027011,0.0810811), PointF(0.536486506,0.110810809),
    PointF(0.548648655,0.14054054), PointF(0.559459448,0.154054061),
    PointF(0.57432431,0.1621622), PointF(0.587837815,0.156756759),
    PointF(0.60135138,0.148648649), PointF(0.620270252,0.110810809),
    PointF(0.628378391,0.081081077), PointF(0.64864862,0),
    PointF(0.66993463,0.040849674), PointF(0.686274529,0.065359481),
    PointF(0.701351345,0.086486489), PointF(0.712162137,0.098648645),
    PointF(0.727124214,0.106209151), PointF(0.74594593,0.100000001),
    PointF(0.758108079,0.085135132), PointF(0.770270288,0.064864866),
    PointF(0.782432437,0.037837837), PointF(0.797297299,0.006756757),
    PointF(0.813513517,0.03108108), PointF(0.82432431,0.047297299),
    PointF(0.840540528,0.064864866), PointF(0.858107924,0.074324302),
    PointF(0.871621609,0.066216215), PointF(0.891891897,0.047297299),
    PointF(0.912162244,0), PointF(0.928378403,0.024324324),
    PointF(0.939189255,0.033783782), PointF(0.952702761,0.040540501),
    PointF(0.966216326,0.033783782), PointF(0.975675702,0.018918918),
    PointF(0.981081069,0), PointF(1,0)] );

  VelocityCurveList.AddCurve('Spring',[
                       PointF(0,1), PointF(0.050000001,0.994594574),
                       PointF(0.093243241,0.986486495), PointF(0.131081074,0.977026999),
                       PointF(0.178378373,0.96081084), PointF(0.221621618,0.939189196),
                       PointF(0.259459466,0.904054046), PointF(0.290540546,0.862162173),
                       PointF(0.313513517,0.812162161), PointF(0.352702707,0.699999988),
                       PointF(0.39459458,0.577027023), PointF(0.421621621,0.487837851),
                       PointF(0.447297305,0.400000006), PointF(0.477027029,0.277027041),
                       PointF(0.493243247,0.190540537), PointF(0.510810792,0.091891892),
                       PointF(0.522973001,0.036486488), PointF(0.536486506,0.013513514),
                       PointF(0.547297299,0.001351351), PointF(0.567567587,0.001351351),
                       PointF(0.586486459,0.016216217), PointF(0.604054034,0.048648648),
                       PointF(0.629729748,0.120270267), PointF(0.647297323,0.164864868),
                       PointF(0.658108115,0.182432428), PointF(0.667567551,0.187837839),
                       PointF(0.678378403,0.178378373), PointF(0.690540552,0.156756759),
                       PointF(0.705405414,0.104054056), PointF(0.717567563,0.052702703),
                       PointF(0.727026999,0.025675675), PointF(0.736486495,0.017567568),
                       PointF(0.74594593,0.025675675), PointF(0.755405426,0.048648648),
                       PointF(0.766216218,0.090540543), PointF(0.778378367,0.129729733),
                       PointF(0.786486506,0.147297293), PointF(0.794594586,0.154054061),
                       PointF(0.805405378,0.147297293), PointF(0.812162161,0.110810809),
                       PointF(0.821621597,0.062162161), PointF(0.831081092,0.03108108),
                       PointF(0.839189172,0.024324324), PointF(0.847297311,0.03108108),
                       PointF(0.856756747,0.060810812), PointF(0.860810816,0.085135132),
                       PointF(0.866216242,0.1027027), PointF(0.878378391,0.112162165),
                       PointF(0.889189184,0.104054056), PointF(0.897297323,0.067567565),
                       PointF(0.905405402,0.020270269), PointF(0.913513541,0.012162162),
                       PointF(0.922972977,0.020270269), PointF(0.93378377,0.07027027),
                       PointF(0.943243265,0.079729728), PointF(0.951351345,0.07027027),
                       PointF(0.96486485,0.014864865), PointF(0.971621633,0.006756757),
                       PointF(0.979729712,0.014864865), PointF(0.991891921,0.009459459),
                       PointF(1,0)] );

  VelocityCurveList.AddCurve('Extend',[
                       PointF(0,1), PointF(0.021628296,1.00674057),
                       PointF(0.031418052,1.01181066), PointF(0.042488199,1.018592119),
                       PointF(0.054731101,1.026621103), PointF(0.068039574,1.035434008),
                       PointF(0.082305536,1.044566512), PointF(0.097421765,1.053555846),
                       PointF(0.113280401,1.061937332), PointF(0.129773766,1.069247246),
                       PointF(0.146794572,1.07502234), PointF(0.164234996,1.078798056),
                       PointF(0.181987375,1.080111265), PointF(0.199944407,1.078498125),
                       PointF(0.217998177,1.073494196), PointF(0.236041337,1.06463635),
                       PointF(0.253966093,1.051460505), PointF(0.271664858,1.033502817),
                       PointF(0.289160788,1.01006639), PointF(0.306729496,0.98071003),
                       PointF(0.324402571,0.946088672), PointF(0.342198551,0.906896591),
                       PointF(0.360136658,0.863827527), PointF(0.378234655,0.817575276),
                       PointF(0.396511734,0.768833756), PointF(0.414986044,0.718296885),
                       PointF(0.433675945,0.666658282), PointF(0.452600539,0.614612103),
                       PointF(0.471778005,0.562851608), PointF(0.491226822,0.51207149),
                       PointF(0.510966003,0.46296525), PointF(0.531013608,0.416226447),
                       PointF(0.551388621,0.372549057), PointF(0.572109163,0.332627147),
                       PointF(0.593193948,0.297154307), PointF(0.61493808,0.266015142),
                       PointF(0.637906194,0.237498119), PointF(0.661897898,0.211377054),
                       PointF(0.686679184,0.187507242), PointF(0.712017238,0.165744022),
                       PointF(0.737677217,0.145942569), PointF(0.763426304,0.127958268),
                       PointF(0.789030194,0.111646593), PointF(0.81425488,0.096862435),
                       PointF(0.838867188,0.083461598), PointF(0.862633109,0.071298659),
                       PointF(0.885318875,0.060229786), PointF(0.906690776,0.050109863),
                       PointF(0.926514983,0.04079403), PointF(0.944558024,0.032137696),
                       PointF(0.960585892,0.023996308), PointF(0.974364579,0.016224962),
                       PointF(1,0)] );

  VelocityCurveList.AddCurve('Extend2',[
                       PointF(0,1), PointF(0.100596204,1.026163936),
                       PointF(0.193881139,1.043063045), PointF(0.280142874,1.051244259),
                       PointF(0.359669268,1.051253438), PointF(0.432748407,1.043637514),
                       PointF(0.499668211,1.028942347), PointF(0.560716808,1.007714987),
                       PointF(0.616182148,0.980501175), PointF(0.666352153,0.947847486),
                       PointF(0.711514831,0.910300434), PointF(0.751958251,0.868406415),
                       PointF(0.787970245,0.822711647), PointF(0.819839001,0.773762763),
                       PointF(0.84785223,0.72210604), PointF(0.872298121,0.668287873),
                       PointF(0.893464744,0.6128546), PointF(0.911639988,0.556352913),
                       PointF(0.927111804,0.499328852), PointF(0.940168262,0.44232899),
                       PointF(0.951097369,0.385899723), PointF(0.960187078,0.330587357),
                       PointF(0.967725277,0.276938379), PointF(0.974000096,0.225499198),
                       PointF(0.979299545,0.176816165), PointF(0.983911514,0.131435677),
                       PointF(0.988124013,0.089904144), PointF(0.992225051,0.052767973),
                       PointF(0.996502757,0.020573545), PointF(1,0)] );

  VelocityCurveList.AddCurve('5Steps', [PointF(0, 1),
    PointF(0.00476256, 0.998198748), PointF(0.01001783, 0.997401655),
    PointF(0.016586913, 0.996307433), PointF(0.02446977, 0.993674755),
    PointF(0.03366648, 0.988263667), PointF(0.044176985, 0.978832901),
    PointF(0.056086004, 0.963840008), PointF(0.069737703, 0.941852033),
    PointF(0.084797069, 0.915453315), PointF(0.100826055, 0.88770926),
    PointF(0.1173869, 0.861685932), PointF(0.134041592, 0.840448678),
    PointF(0.14986001, 0.827404022), PointF(0.165319234, 0.822299838),
    PointF(0.180999905, 0.821759641), PointF(0.196729809, 0.822855532),
    PointF(0.212336704, 0.822659731), PointF(0.227648303, 0.8182441),
    PointF(0.242492482, 0.806681752), PointF(0.25666675, 0.785713494),
    PointF(0.270275384, 0.757253945), PointF(0.283605009, 0.724631786),
    PointF(0.296942919, 0.691177368), PointF(0.310576051, 0.660220385),
    PointF(0.324791402, 0.635090232), PointF(0.339277983, 0.619380832),
    PointF(0.353822023, 0.612306178), PointF(0.368825495, 0.610563397),
    PointF(0.384196669, 0.611486435), PointF(0.399843752, 0.612409592),
    PointF(0.415674627, 0.610666871), PointF(0.431597352, 0.603591919),
    PointF(0.447597653, 0.588620067), PointF(0.463997573, 0.565705121),
    PointF(0.480650008, 0.537434995), PointF(0.49732542, 0.506522119),
    PointF(0.513794065, 0.475678205), PointF(0.529826105, 0.447614998),
    PointF(0.545191526, 0.425044417), PointF(0.559649169, 0.410597384),
    PointF(0.573107302, 0.404411435), PointF(0.585852742, 0.403787076),
    PointF(0.598207474, 0.40587458), PointF(0.610493481, 0.407824278),
    PointF(0.623032153, 0.406786263), PointF(0.636145294, 0.399910569),
    PointF(0.650130808, 0.384445608), PointF(0.664943099, 0.360299051),
    PointF(0.680306613, 0.330269039), PointF(0.695945978, 0.297297269),
    PointF(0.711585343, 0.264325589), PointF(0.726948977, 0.234295577),
    PointF(0.74176085, 0.210148841), PointF(0.755756915, 0.194641665),
    PointF(0.768937349, 0.187498108), PointF(0.781566083, 0.186100006),
    PointF(0.793918848, 0.187781528), PointF(0.806271791, 0.189876765),
    PointF(0.818900585, 0.189719737), PointF(0.832080722, 0.184644371),
    PointF(0.846693695, 0.171546355), PointF(0.863076866, 0.150098905),
    PointF(0.880165517, 0.123468272), PointF(0.897327781, 0.094524868),
    PointF(0.91393286, 0.066139199), PointF(0.929347813, 0.041181549),
    PointF(0.942942917, 0.022522518), PointF(0.954777062, 0.010850859),
    PointF(0.965332329, 0.004099932), PointF(0.974607587, 0.001006695),
    PointF(0.982603788, 0.000308192), PointF(0.989320636, 0.000741532),
    PointF(0.99475807, 0.001043589), PointF(1, 0)]);

  VelocityCurveList.AddCurve('Drop',[
    PointF(0,1), PointF(0.051241662,0.998875082),
    PointF(0.087837838,0.993243217), PointF(0.125675678,0.983783782),
    PointF(0.174324319,0.96891892), PointF(0.21891892,0.952702701),
    PointF(0.263513505,0.931081057), PointF(0.302702695,0.908108115),
    PointF(0.343243241,0.882432461), PointF(0.386486501,0.85135138),
    PointF(0.42567569,0.8202703), PointF(0.467567563,0.782432437),
    PointF(0.506756783,0.741891921), PointF(0.544594586,0.702702701),
    PointF(0.575675666,0.670270264), PointF(0.60945946,0.628378391),
    PointF(0.636486471,0.594594598), PointF(0.671621621,0.551351368),
    PointF(0.708108127,0.498648643), PointF(0.741891921,0.451351345),
    PointF(0.783783793,0.387837827), PointF(0.817567587,0.332432419),
    PointF(0.845945954,0.286486477), PointF(0.864864886,0.251351357),
    PointF(0.885135114,0.216216221), PointF(0.908108115,0.175675675),
    PointF(0.927027047,0.141891897), PointF(0.940540552,0.112162165),
    PointF(0.955405414,0.087837838), PointF(0.970270276,0.056756757),
    PointF(0.986486495,0.025675675), PointF(1,0)] );

  VelocityCurveList.AddCurve('SlowAtMiddle',[
    PointF(0,1), PointF(0.044594593,0.664864838),
    PointF(0.055405404,0.635135114), PointF(0.072972976,0.606756747),
    PointF(0.097297296,0.586486459), PointF(0.133783787,0.5702703),
    PointF(0.822972953,0.435135126), PointF(0.860810816,0.42567569),
    PointF(0.891891897,0.408108115), PointF(0.912162185,0.374324322),
    PointF(0.92567569,0.339189202), PointF(1,0)] );

  VelocityCurveList.AddCurve('PauseAtMiddle',[
    PointF(0,1), PointF(0.041891892,0.604054034),
    PointF(0.056756757,0.548648655), PointF(0.072972976,0.524324298),
    PointF(0.090540543,0.509459436), PointF(0.108108111,0.501351357),
    PointF(0.893243253,0.5), PointF(0.909459472,0.490540534),
    PointF(0.927027047,0.474324316), PointF(0.941891909,0.448648661),
    PointF(0.958108127,0.39459458), PointF(1,0)] );

  VelocityCurveList.AddCurve('Spring2',[
    PointF(0,1), PointF(0.093243241,0.032432433),
    PointF(0.1027027,0.017567568), PointF(0.113513514,0.032432433),
    PointF(0.151351348,0.408108115), PointF(0.181081086,0.602702677),
    PointF(0.189189196,0.627027035), PointF(0.202702701,0.64864862),
    PointF(0.21891892,0.627027035), PointF(0.227027029,0.60135138),
    PointF(0.252702713,0.259459466), PointF(0.278378367,0.033783782),
    PointF(0.295945942,0.016216217), PointF(0.314864874,0.017567568),
    PointF(0.331081092,0.035135135), PointF(0.35540542,0.172972977),
    PointF(0.409459472,0.485135138), PointF(0.427027017,0.505405426),
    PointF(0.443243235,0.487837851), PointF(0.458108097,0.441891879),
    PointF(0.474324316,0.35540542), PointF(0.494594604,0.244594589),
    PointF(0.513513505,0.148648649), PointF(0.536486506,0.03108108),
    PointF(0.547297299,0.001351351), PointF(0.567567587,0.001351351),
    PointF(0.578378379,0.032432433), PointF(0.589189172,0.079729728),
    PointF(0.598648667,0.152702704), PointF(0.613513529,0.231081083),
    PointF(0.625675678,0.302702695), PointF(0.636486471,0.322972983),
    PointF(0.64864862,0.302702695), PointF(0.678378403,0.172972977),
    PointF(0.695945919,0.1027027), PointF(0.704054058,0.050000001),
    PointF(0.713513494,0.025675675), PointF(0.725675702,0.006756757),
    PointF(0.739189208,0.022972973), PointF(0.75,0.054054055),
    PointF(0.758108079,0.095945947), PointF(0.764864862,0.143243238),
    PointF(0.771621644,0.190540537), PointF(0.786486506,0.206756756),
    PointF(0.802702725,0.19324325), PointF(0.814864874,0.120270267),
    PointF(0.821621597,0.062162161), PointF(0.831081092,0.03108108),
    PointF(0.839189172,0.024324324), PointF(0.847297311,0.03108108),
    PointF(0.856756747,0.060810812), PointF(0.860810816,0.085135132),
    PointF(0.866216242,0.1027027), PointF(0.878378391,0.141891897),
    PointF(0.889189184,0.104054056), PointF(0.897297323,0.067567565),
    PointF(0.905405402,0.020270269), PointF(0.913513541,0.012162162),
    PointF(0.922972977,0.020270269), PointF(0.93378377,0.07027027),
    PointF(0.943243265,0.079729728), PointF(0.951351345,0.07027027),
    PointF(0.96486485,0.014864865), PointF(0.971621633,0.006756757),
    PointF(0.979729712,0.014864865), PointF(0.991891921,0.009459459),
    PointF(1,0)] );

  VelocityCurveList.AddCurve('SingleRebound',[
    PointF(0,1), PointF(0.060810812,0.945945919),
    PointF(0.148648649,0.858108103), PointF(0.243243247,0.756756783),
    PointF(0.344594598,0.628378391), PointF(0.445945948,0.486486495),
    PointF(0.533783793,0.331081092), PointF(0.614864886,0.168918923),
    PointF(0.682432413,0.006756757), PointF(0.709459484,0.020270269),
    PointF(0.770270288,0.087837838), PointF(0.804054081,0.114864863),
    PointF(0.837837815,0.094594598), PointF(0.871621609,0.054054055),
    PointF(0.905405402,0), PointF(1,0)] );
end;

procedure FreeVelocityCurveList;
begin
  if FVelocityCurveListRefCount > 0 then begin
    dec(FVelocityCurveListRefCount);
    if FVelocityCurveListRefCount = 0 then FreeAndNil(VelocityCurveList);
  end;
end;

{$endif oglcIMPLEMENTATION}
