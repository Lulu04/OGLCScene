{$ifdef oglcINTERFACE}
type

TScenarioAction = (
     saLoop,
     saLabel,
     saGotoLabel,
     saWait,

     saKill,
     saFreeze,
     saVisible,

     saFlipH,
     saFlipV,
     saToggleFlipH,
     saToggleFlipV,

     saOpacity,
     saOpacityChange,

     saAnimate,
     saIncFrame,
     saDecFrame,
     saSetFrame,

     saTint,
     saTintChange,
     saTintRedChange,
     saTintGreenChange,
     saTintBlueChange,
     saTintAlphaChange,

     saAngle,
     saRotate,
     saRotateTo,
     saRotateAroundPoint,

     saScale,
     saScaleChange,
     saScaleH,
     saScaleHChange,
     saScaleV,
     saScaleVChange,

     saBlink,
     saStopBlink,

     saMoveTo,
     saMoveXTo,
     saMoveYTo,

     saMoveCenterTo,
     saMoveXCenterTo,
     saMoveYCenterTo,

     saMoveRelative,
     saMoveXRelative,
     saMoveYRelative,

     saSetCoor,
     saSetCenterCoor,
     saCenterOnScene,

     saPostMessage);

const
ScenarioActionNames: array[TScenarioAction] of string=(
     'Loop', 'Label', 'Goto', 'Wait',
     'Kill', 'Freeze', 'Visible',
     'FlipH', 'FlipV', 'ToggleFlipH', 'ToggleFlipV',
     'Opacity', 'OpacityChange',
     'Animate', 'Incframe', 'DecFrame', 'SetFrame',
     'Tint', 'TintChange', 'TintRedChange', 'TintGreenChange', 'TintBlueChange', 'TintAlphaChange',
     'Angle', 'Rotate', 'RotateTo', 'RotateAroundPoint',
     'Scale', 'ScaleChange', 'ScaleH', 'ScaleHChange', 'ScaleV', 'ScaleVChange',
     'Blink', 'StopBlink',
     'MoveTo', 'MoveXTo', 'MoveYTo',
     'MoveCenterTo', 'MoveXCenterTo', 'MoveXCenterTo',
     'MoveRelative', 'MoveXRelative', 'MoveYRelative',
     'SetCoor', 'SetCenterCoor',
     'CenterOnScene',
     'PostMessage');

VelocityCurvesNames: array[0..14] of string=(
  'idclinear', 'idcstartfastendslow', 'idcstartslowendfast', 'idcsinusoid',
  'idcsinusoid2', 'idcbouncy', 'idcspring', 'idcextend',
  'idcextend2', 'idc5steps', 'idcdrop', 'idcslowatmiddle',
  'idcpauseatmiddle', 'idcspring2', 'idcsinglerebound');

type
TScenarioCompiledAction = record
  Action: TScenarioAction;
  Single1, Single2, Single3: single;
  Integer1, Integer2, Integer3, Integer4: integer;
  Boolean1: boolean;
  String1: string;
  Curve1: word;
end;

TCompiledScenario = array of TScenarioCompiledAction;

{ TScenario }

TScenario = class
private
  FLabelArray: array of record
    Name: string;
    LineIndex: integer;
  end;
private
  FActions: TStringlist;
  FCompiled: TCompiledScenario;
  FCurrentLine: integer;
  FWaitTime: single;
  FParentSurface: TSimpleSurfaceWithEffect;
  FIsPlaying: boolean;
  FError: boolean;
  FLineError: integer;
  FErrorMessage: string;
  FKilled: boolean;
  procedure Execute(const aElapsedTime: single);
  function GetIsPlaying: boolean;
  procedure DecodeAction;
  procedure CompileScenario;
  function CompileLine(aLineIndex: integer): boolean;
  function CheckAction(const aLine: string): boolean;
  function GetLineContentWhereErrorOccur: string;
public // utils for Game Helper
  procedure FormatScenario;
public
  constructor Create;
  destructor Destroy; override;
  // load a scenario from a file
  procedure LoadFromFile(const aFilename: string);
  // aLines is a string that contains the actions to execute,
  // one action per line and each line separated by #10
  procedure InitWith(const aLines: string);
  procedure AddLine(const aLine: string); deprecated 'use procedure InitWith(const aLines: string)';
  procedure Play(aFromBeginning: boolean);
  procedure Stop;
  // Free the scenario
  procedure Kill;
  // Return True if the scenario is running
  property IsPlaying: boolean read GetIsPlaying;
  // True if an error occurs while interpreting the scenario.
  property Error: boolean read FError;
  property LineError: integer read FLineError;
  property ErrorMessage: string read FErrorMessage;
  property LineContentWhereErrorOccur: string read GetLineContentWhereErrorOccur;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TScenario }

constructor TScenario.Create;
begin
  FActions := TStringList.Create;
  FCurrentLine := 0;
  FWaitTime := 0.0;
  FParentSurface := NIL;
  FIsPlaying := FALSE;
end;

destructor TScenario.Destroy;
begin
  FActions.Free;
  FActions := NIL;
end;

procedure TScenario.Execute(const aElapsedTime: single);
var OriginLine: integer;
begin
  if FError then exit;

  if FWaitTime > 0
    then FWaitTime -= aElapsedTime;

  if FIsPlaying and (FWaitTime <= 0.000) then begin
    OriginLine := FCurrentLine;
    repeat
      DecodeAction;
      inc(FCurrentLine);
    until (FWaitTime > 0) or (FCurrentLine >= FActions.Count) or
           not FIsPlaying or (OriginLine = FCurrentLine); // the latter avoid infinite loop in buggy scenario

    if FCurrentLine >= FActions.Count then FIsPlaying := FALSE;
  end;
end;

function TScenario.GetIsPlaying: boolean;
begin
  Result := FIsPlaying or (FWaitTime > 0);
end;

procedure TScenario.LoadFromFile(const aFilename: string);
begin
 FActions.Clear;
 try
   FActions.LoadFromFile(aFilename);
   FormatScenario;
   CompileScenario;
 except
   FParentSurface.ParentScene.LogError('TScenario.LoadFromFile: error while loading scenario file "'+aFilename+'"');
 end;
end;

procedure TScenario.DecodeAction;
begin
  if FError then exit;

  with FCompiled[FCurrentLine] do
    case Action of
      saLoop: FCurrentLine := -1;
      saLabel:;
      saGotoLabel: FCurrentLine := Integer1;
      saWait: FWaitTime := Single1;
      saKill: FParentSurface.Kill;
      saFreeze: FParentSurface.Freeze := Boolean1;
      saVisible: FParentSurface.Visible := Boolean1;
      saFlipH: FParentSurface.FlipH := Boolean1;
      saFlipV: FParentSurface.FlipV := Boolean1;
      saToggleFlipH: FParentSurface.FlipH := not FParentSurface.FlipH;
      saToggleFlipV: FParentSurface.FlipV := not FParentSurface.FlipV;
      saOpacity: FParentSurface.Opacity.Value := Integer1;
      saOpacityChange: FParentSurface.Opacity.ChangeTo(Integer1, Single1, Curve1);
      saAnimate: begin
        FParentSurface.SetFrameLoopBounds(Integer1, Integer2);
        FParentSurface.FrameAddPerSecond(Single1);
      end;
      saIncFrame: FParentSurface.Frame := FParentSurface.Frame + 1;
      saDecFrame: FParentSurface.Frame := FParentSurface.Frame - 1;
      saSetFrame: FParentSurface.Frame := Integer1;
      saTint: FParentSurface.Tint.Value := BGRA(Integer1, Integer2, Integer3, Integer4);
      saTintChange: FParentSurface.Tint.ChangeTo(BGRA(Integer1, Integer2, Integer3, Integer4), Single1, Curve1);
      saTintRedChange: FParentSurface.Tint.Red.ChangeTo(Integer1, Single1, Curve1);
      saTintGreenChange: FParentSurface.Tint.Green.ChangeTo(Integer1, Single1, Curve1);
      saTintBlueChange: FParentSurface.Tint.Blue.ChangeTo(Integer1, Single1, Curve1);
      saTintAlphaChange: FParentSurface.Tint.Alpha.ChangeTo(Integer1, Single1, Curve1);
      saAngle: FParentSurface.Angle.Value := Single1;
      saRotate: FParentSurface.Angle.AddConstant(Single1);
      saRotateTo: FParentSurface.Angle.ChangeTo(Single1, Single2, Curve1);
      saRotateAroundPoint: FParentSurface.RotationAroundPoint(Single1, Single2, Single3, Boolean1);
      saScale: FParentSurface.Scale.Value := PointF(Single1, Single1);
      saScaleChange: FParentSurface.Scale.ChangeTo(PointF(Single1, Single1), Single2, Curve1);
      saScaleH: FParentSurface.Scale.x.Value := Single1;
      saScaleHChange: FParentSurface.Scale.x.ChangeTo(Single1, Single2, Curve1);
      saScaleV: FParentSurface.Scale.y.Value := Single1;
      saScaleVChange: FParentSurface.Scale.y.ChangeTo(Single1, Single2, Curve1);
      saBlink: FParentSurface.Blink(Integer1, Single1, Single2);
      saStopBlink: FParentSurface.StopBlink;
      saMoveTo: FParentSurface.MoveTo(Single1, Single2, Single3, Curve1);
      saMoveXTo: FParentSurface.X.ChangeTo(Single1, Single2, Curve1);
      saMoveYTo: FParentSurface.Y.ChangeTo(Single1, Single2, Curve1);
      saMoveCenterTo: FParentSurface.MoveCenterTo(Single1, Single2, Single3, Curve1);
      saMoveXCenterTo: FParentSurface.MoveXCenterTo(Single1, Single2, Curve1);
      saMoveYCenterTo: FParentSurface.MoveYCenterTo(Single1, Single2, Curve1);
      saMoveRelative: FParentSurface.MoveRelative(Single1, Single2, Single3, Curve1);
      saMoveXRelative: FParentSurface.MoveXRelative(Single1, Single2, Curve1);
      saMoveYRelative: FParentSurface.MoveYRelative(Single1, Single2, Curve1);
      saSetCoor: FParentSurface.SetCoordinate(Single1, Single2);
      saSetCenterCoor: FParentSurface.SetCenterCoordinate(Single1, Single2);
      saCenterOnScene: if FParentSurface.ParentScene <> NIL then
                         FParentSurface.CenterOnScene;
      saPostMessage: FParentSurface.PostMessage(Integer1, Single1);
    end;
end;

procedure TScenario.CompileScenario;
var i, j: integer;
    flagFound: boolean;
begin
  FCompiled := NIL;
  if FActions.Count = 0 then begin
    FError := True;
    FErrorMessage := 'scenario is empty';
    exit;
  end;

  FError := False;
  FErrorMessage := '';
  FLineError := 0;
  FLabelArray := NIL;

  SetLength(FCompiled, FActions.Count);
  for i:=0 to High(FCompiled) do
    if not CompileLine(i) then exit;

  // check if labels are not declared twice
  for i:=0 to High(FLabelArray)-1 do
    for j:=i+1 to High(FLabelArray) do
      if FLabelArray[i].Name = FLabelArray[j].Name then begin
        FError := True;
        FErrorMessage := 'label already defined at line '+FLabelArray[i].LineIndex.ToString;
        FLineError := j;
        exit;
      end;

  // check if all goto match to a label
  for i:=0 to High(FCompiled) do
    if FCompiled[i].Action = saGotoLabel then begin
      flagFound := False;
      for j:=0 to High(FLabelArray) do
        if FCompiled[i].String1 = FLabelArray[j].Name then begin
          flagFound := True;
          FCompiled[i].Integer1 := FLabelArray[j].LineIndex;
          break;
        end;
      if not flagFound then begin
        FError := True;
        FErrorMessage := 'this label doesn''t exist';
        FLineError := i;
        exit;
      end;
    end;
  FLabelArray := NIL;
end;

function TScenario.CompileLine(aLineIndex: integer): boolean;
var temp: TStringArray;
    i: integer;
    function CheckParamCount(aCount: integer): boolean;
    begin
      Result := Length(Temp) = aCount + 1;
      if not Result then begin
        FErrorMessage := 'wrong parameter count';
        FError := True;
        FLineError := aLineIndex;
      end;
    end;
    function TryStringToBoolean(s: string; out aValue: boolean): boolean;
    begin
      s := LowerCase(s);
      aValue := s = 'true';
      Result := (s = 'true') or (s = 'false');
    end;
    function CheckString(const s: string; out aValue: string): boolean;
    begin
      aValue := s;
      Result := s <> '';
    end;
    function TryStringToCurve(s: string; out aCurve: word): boolean;
    var i: integer;
    begin
      s := LowerCase(s);
      for i:=0 to High(VelocityCurvesNames) do
        if s = VelocityCurvesNames[i] then begin
          aCurve := i;
          exit(True);
        end;
      Result := False;
    end;

begin
  temp := Lowercase(FActions.Strings[aLineIndex]).Split([' ']);
  if Length(temp) = 0 then exit(False);

  FError := True;
  for i:=0 to Ord(High(TScenarioAction)) do
    if temp[0] = Lowercase(ScenarioActionNames[TScenarioAction(i)]) then begin
      FError := False;
      FCompiled[aLineIndex].Action := TScenarioAction(i);
      break;
    end;
  if FError then begin
    FErrorMessage := 'unrecognized action';
    FLineError := aLineIndex;
    exit(False);
  end;

  case FCompiled[aLineIndex].Action of
    saLoop, saKill, saToggleFlipH, saToggleFlipV, saIncFrame, saDecFrame,
    saStopBlink, saCenterOnScene:
      Result := CheckParamCount(0);

    saLabel, saGotoLabel: begin
      Result := CheckParamCount(1) and
                CheckString(temp[1], FCompiled[aLineIndex].String1);

      // keep track of each labels
      if Result and (FCompiled[aLineIndex].Action = saLabel) then begin
        SetLength(FLabelArray, Length(FLabelArray)+1);
        FLabelArray[High(FLabelArray)].Name := FCompiled[aLineIndex].String1;
        FLabelArray[High(FLabelArray)].LineIndex := aLineIndex;
      end;
    end;

    saWait, saAngle, saRotate, saScale, saScaleH, saScaleV: begin
      Result := CheckParamCount(1) and
                TryStringToSingle(temp[1], FCompiled[aLineIndex].Single1);
    end;

    saFreeze, saVisible, saFlipH, saFlipV: begin
      Result := CheckParamCount(1) and
                TryStringToBoolean(temp[1], FCompiled[aLineIndex].Boolean1);
    end;

    saOpacity, saSetFrame: begin
      Result := CheckParamCount(1) and
                TryStrToInt(temp[1], FCompiled[aLineIndex].Integer1);
    end;

    saOpacityChange: begin
      Result := CheckParamCount(3) and
                TryStrToInt(temp[1], FCompiled[aLineIndex].Integer1) and
                TryStringToSingle(temp[2], FCompiled[aLineIndex].Single1) and
                TryStringToCurve(temp[3], FCompiled[aLineIndex].Curve1);
    end;

    saAnimate: begin
      Result := CheckParamCount(3) and
                TryStrToInt(temp[1], FCompiled[aLineIndex].Integer1) and
                TryStrToInt(temp[2], FCompiled[aLineIndex].Integer2) and
                TryStringToSingle(temp[3], FCompiled[aLineIndex].Single1);
    end;

    saTint: begin
      Result := CheckParamCount(4) and
                TryStrToInt(temp[1], FCompiled[aLineIndex].Integer1) and
                TryStrToInt(temp[2], FCompiled[aLineIndex].Integer2) and
                TryStrToInt(temp[3], FCompiled[aLineIndex].Integer3) and
                TryStrToInt(temp[4], FCompiled[aLineIndex].Integer4);
    end;

    saTintChange: begin
      Result := CheckParamCount(6) and
                TryStrToInt(temp[1], FCompiled[aLineIndex].Integer1) and
                TryStrToInt(temp[2], FCompiled[aLineIndex].Integer2) and
                TryStrToInt(temp[3], FCompiled[aLineIndex].Integer3) and
                TryStrToInt(temp[4], FCompiled[aLineIndex].Integer4) and
                TryStringToSingle(temp[5], FCompiled[aLineIndex].Single1) and
                TryStringToCurve(temp[6], FCompiled[aLineIndex].Curve1);
     end;

    saTintRedChange, saTintGreenChange, saTintBlueChange, saTintAlphaChange: begin
      Result := CheckParamCount(3) and
                TryStrToInt(temp[1], FCompiled[aLineIndex].Integer1) and
                TryStringToSingle(temp[2], FCompiled[aLineIndex].Single1) and
                TryStringToCurve(temp[3], FCompiled[aLineIndex].Curve1);
    end;

    saRotateTo, saScaleChange, saScaleHChange, saScaleVChange, saMoveXTo, saMoveYTo,
    saMoveXCenterTo, saMoveYCenterTo, saMoveXRelative, saMoveYRelative: begin
      Result := CheckParamCount(3) and
                TryStringToSingle(temp[1], FCompiled[aLineIndex].Single1) and
                TryStringToSingle(temp[2], FCompiled[aLineIndex].Single2) and
                TryStringToCurve(temp[3], FCompiled[aLineIndex].Curve1);
    end;

    saRotateAroundPoint, saMoveTo, saMoveCenterTo, saMoveRelative: begin
      Result := CheckParamCount(4) and
                TryStringToSingle(temp[1], FCompiled[aLineIndex].Single1) and
                TryStringToSingle(temp[2], FCompiled[aLineIndex].Single2) and
                TryStringToSingle(temp[3], FCompiled[aLineIndex].Single3) and
                TryStringToCurve(temp[4], FCompiled[aLineIndex].Curve1);

    end;

    saBlink: begin
      Result := CheckParamCount(3) and
                TryStrToInt(temp[1], FCompiled[aLineIndex].Integer1) and
                TryStringToSingle(temp[2], FCompiled[aLineIndex].Single1) and
                TryStringToSingle(temp[3], FCompiled[aLineIndex].Single2)
    end;

    saSetCoor, saSetCenterCoor: begin
      Result := CheckParamCount(2) and
                TryStringToSingle(temp[1], FCompiled[aLineIndex].Single1) and
                TryStringToSingle(temp[2], FCompiled[aLineIndex].Single2)
    end;

    saPostMessage: begin
      Result := CheckParamCount(2) and
                TryStrToInt(temp[1], FCompiled[aLineIndex].Integer1) and
                TryStringToSingle(temp[2], FCompiled[aLineIndex].Single1);
    end;
  end;
end;

function TScenario.CheckAction(const aLine: string): boolean;
var temp: TStringArray;
    function ActionIs(act: string): boolean;
    begin
      Result := LowerCase(act) = temp[0];
    end;
    function CheckParamCount(aCount: integer): boolean;
    begin
      Result := Length(Temp) = aCount + 1;
      if not Result then FErrorMessage := 'wrong parameter count';
    end;
    function CheckParamIsFloat(aParamIndex: integer): boolean;
    var v: single;
    begin
      Result := TryStringToSingle(temp[aParamIndex], v);
    end;
    function CheckParamIsInteger(aParamIndex: integer): boolean;
    var v: integer;
    begin
      Result := TryStrToInt(temp[aParamIndex], v);
    end;
    function CheckParamIsBoolean(aParamIndex: integer): boolean;
    var s: string;
    begin
      s := Lowercase(temp[aParamIndex]);
      Result := (s = 'true') or (s = 'false');
    end;
    function CheckParamIsCurve(aParamIndex: integer): boolean;
    var s: string;
        i: integer;
    begin
      s := Lowercase(temp[aParamIndex]);
      for i:=0 to High(VelocityCurvesNames) do
        if s = LowerCase(VelocityCurvesNames[i]) then exit(True);
      Result := False;
    end;
begin
  if Length(aLine) = 0 then exit(False);
  temp := Lowercase(FActions.Strings[FCurrentLine]).Split([' ']);
  if Length(temp) = 0 then exit(False);

  if ActionIs(acWait) then
    if CheckParamCount(1) and
       CheckParamIsFloat(1) then exit(True)
      else exit(False);

  if ActionIs(acKill) then
    if CheckParamCount(0) then exit(True)
      else exit(False);

  if ActionIs(acFreeze) then
    if CheckParamCount(1) and
       CheckParamIsBoolean(1) then exit(True)
      else exit(False);

  if ActionIs(acVisible) then
     if CheckParamCount(1) and
        CheckParamIsBoolean(1) then exit(True)
       else exit(False);

  if ActionIs(acOpacity) then
     if CheckParamCount(1) and
        CheckParamIsInteger(1) then exit(True)
       else exit(False);

  if ActionIs(acOpacityChange) then
     if CheckParamCount(3) and
        CheckParamIsInteger(1) and
        CheckParamIsFloat(2) and
        CheckParamIsCurve(3) then exit(True)
       else exit(False);

  if ActionIs(acTint) then
     if CheckParamCount(4) and
        CheckParamIsInteger(1) and
        CheckParamIsInteger(2) and
        CheckParamIsInteger(3) and
        CheckParamIsInteger(4) then exit(True)
       else exit(False);

  if ActionIs(acTintChange) then
     if CheckParamCount(6) and
        CheckParamIsInteger(1) and
        CheckParamIsInteger(2) and
        CheckParamIsInteger(3) and
        CheckParamIsInteger(4) and
        CheckParamIsFloat(5) and
        CheckParamIsCurve(6) then exit(True)
       else exit(False);

  if ActionIs(acTintRedChange) or ActionIs(acTintGreenChange) or
     ActionIs(acTintBlueChange) or ActionIs(acTintAlphaChange) then
     if CheckParamCount(3) and
        CheckParamIsInteger(1) and
        CheckParamIsFloat(2) and
        CheckParamIsCurve(3) then exit(True)
       else exit(True);

  if ActionIs(acTintGreenChange) then
     if CheckParamCount(3) and
        CheckParamIsInteger(1) and
        CheckParamIsFloat(2) and
        CheckParamIsCurve(3) then exit(True)
       else exit(False);

  if ActionIs(acAngle) or ActionIs(acRotate) then
     if CheckParamCount(1) and
        CheckParamIsFloat(1) then exit(True)
       else exit(True);

  if ActionIs(acRotateTo) then
     if CheckParamCount(3) and
        CheckParamIsFloat(1) and
        CheckParamIsFloat(2) and
        CheckParamIsFloat(3) then exit(True)
       else exit(False);

  if ActionIs(acRotateTo) then
     if CheckParamCount(4) and
        CheckParamIsFloat(1) and
        CheckParamIsFloat(2) and
        CheckParamIsFloat(3) and
        CheckParamIsBoolean(4) then exit(True)
       else exit(False);

  if ActionIs(acScale) then
     if CheckParamCount(2) and
        CheckParamIsFloat(1) and
        CheckParamIsFloat(2) then exit(True)
       else exit(False);

  if ActionIs(acScaleChange) or ActionIs(acScaleHChange) or ActionIs(acScaleVChange) then
     if CheckParamCount(3) and
        CheckParamIsFloat(1) and
        CheckParamIsFloat(2) and
        CheckParamIsCurve(3) then exit(True)
       else exit(False);

  if ActionIs(acScaleH) or ActionIs(acScaleV) then
     if CheckParamCount(1) and
        CheckParamIsFloat(1) then exit(True)
       else exit(True);

  if ActionIs(acBlink) then
     if CheckParamCount(3) and
        CheckParamIsInteger(1) and
        CheckParamIsFloat(2) and
        CheckParamIsFloat(3) then exit(True)
       else exit(False);

  if ActionIs(acStopBlink) or ActionIs(acToggleFlipH) or ActionIs(acToggleFlipV) then
     if CheckParamCount(0) then exit(True)
       else exit(False);

  if ActionIs(acFlipH) or ActionIs(acFlipV) then
     if CheckParamCount(1) and
        CheckParamIsBoolean(1) then exit(True)
       else exit(True);

  if ActionIs(acAnimate) then
     if CheckParamCount(3) and
        CheckParamIsInteger(1) and
        CheckParamIsInteger(2) and
        CheckParamIsFloat(3) then exit(True)
       else exit(False);

  if ActionIs(acIncFrame) or ActionIs(acDecFrame) or
     ActionIs(acCenterOnScene) or ActionIs(acLoop) then
     if CheckParamCount(0) then exit(True)
       else exit(False);

  if ActionIs(acSetFrame) then
     if CheckParamCount(1) and
        CheckParamIsInteger(1) then exit(True)
       else exit(False);

  if ActionIs(acMoveTo) or ActionIs(acMoveCenterTo) or
     ActionIs(acMoveRelative) then
     if CheckParamCount(4) and
        CheckParamIsFloat(1) and
        CheckParamIsFloat(2) and
        CheckParamIsFloat(3) and
        CheckParamIsCurve(4) then exit(True)
       else exit(False);

  if ActionIs(acMoveXTo) or ActionIs(acMoveYTo)  or
     ActionIs(acMoveXCenterTo) or ActionIs(acMoveYCenterTo) or
     ActionIs(acMoveXRelative) or ActionIs(acMoveYRelative) then
     if CheckParamCount(3) and
        CheckParamIsFloat(1) and
        CheckParamIsFloat(2) and
        CheckParamIsCurve(3) then exit(True)
       else exit(False);


  if ActionIs(acSetCoor) or ActionIs(acSetCenterCoor) then
     if CheckParamCount(2) and
        CheckParamIsFloat(1) and
        CheckParamIsFloat(2) then exit(True)
       else exit(False);

  if ActionIs(acPostMessage) then
     if CheckParamCount(2) and
        CheckParamIsInteger(1) and
        CheckParamIsFloat(2) then exit(True)
       else exit(False);

  if ActionIs(acLabel) or ActionIs(acGotoLabel) then
     if CheckParamCount(1) then exit(True)
       else exit(False);

  Result := False;
  FErrorMessage := 'unrecognized action';
end;

function TScenario.GetLineContentWhereErrorOccur: string;
begin
 if (FLineError >= 0) and (FLineError < FActions.Count) then Result := FActions.Strings[FLineError]
   else Result := '';
end;

procedure TScenario.FormatScenario;
var i, j: integer;
  s: string;
  A: TStringArray;
begin
  if FActions.Count = 0 then exit;

  // on each line, remove trailing space and set first letter uppercase
  for i:=0 to FActions.Count-1 do begin
    s := Trim(FActions.Strings[i]).TrimRight;
    if Length(s) > 0 then
      if s[1] in ['a'..'z'] then s[1] := Chr(Ord(S[1])-32);
    FActions.Strings[i] := s;
  end;

  // remove empty lines
  for i:=FActions.Count-1 downto 0 do
    if FActions.Strings[i] = '' then
      FActions.Delete(i);

  // ensure there is only one space between parameters
  for i:=0 to FActions.Count-1 do begin
    s := FActions.Strings[i];
    A := s.Split([' ']);
    for j:= High(A) downto 0 do
      if A[j] = '' then system.Delete(A, j, 1);
    s := '';
    for j:=0 to High(A)-1 do
      s := s + A[j] + ' ';
    s := s + A[High(A)];
    FActions.Strings[i] := s;
  end;
end;

procedure TScenario.InitWith(const aLines: string);
begin
  FActions.Clear;
  FActions.TextLineBreakStyle := tlbsLF;
  FActions.Text := aLines;
  FormatScenario;
  CompileScenario;
end;

procedure TScenario.AddLine(const aLine: string);
begin
  FActions.Add(aLine);
end;

procedure TScenario.Play(aFromBeginning: boolean);
begin
  if aFromBeginning then begin
    FCurrentLine := 0;
    FWaitTime := 0.0;
  end;

  FIsPlaying := True;
end;

procedure TScenario.Stop;
begin
  FIsPlaying := False;
end;

procedure TScenario.Kill;
begin
  FKilled := True;
end;

{$endif oglcIMPLEMENTATION}

