{$ifdef oglcINTERFACE}
type

{ TZBufferClipping }
// Don't work with nested clipped surfaces.

// concept from https://stackoverflow.com/questions/13742556/best-approach-to-draw-clipped-ui-elements-in-opengl
{TZBufferClipping = record
private const DELTA_ZVALUE = 0.001;
public
  IsActive: boolean;
  FrontZValue,
  BackZValue: single;
  procedure Start; inline;
  procedure NextZValues; inline;
  procedure Stop; inline;
end;  }




// concept from https://stackoverflow.com/questions/7465360/nested-scissor-boxes-with-opengl


{ TStencilClipping }

TStencilClipping = record
  IsActive: boolean;
  Value: byte;
  procedure InitDefault;
  procedure Start;
  procedure NextValue;
  procedure PreviousValue;
  procedure Stop;
end;


{ TUIClipArea }
// a software clipping

TUIClipArea = TRect;
PUIClipArea = ^TUIClipArea;

TUIClipAreaHelper = record helper for TUIClipArea
private
public
  // Check if the specified surface is visible in the clip area
  //   > return True if at least one part of the surface is visible (the object can be drawn)
  //   > return False if the surface is completely outside.
  // If only a part is visible, the function 'cut' the surface area and its
  // texture coordinates in respect of the clipping area.
  function CheckAndAdjust(var aSurfaceArea: TRect; var aTexCoor: TQuadCoor): boolean;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TStencilClipping }

procedure TStencilClipping.InitDefault;
begin
  IsActive := False;
  Value := 0;
end;

procedure TStencilClipping.Start;
begin
  if IsActive then exit;
  IsActive := True;
  glEnable(GL_STENCIL_TEST);
  glClear(GL_STENCIL_BUFFER_BIT);
  Value := 0;
end;

procedure TStencilClipping.NextValue;
begin
  if Value < 255 then inc(Value);
end;

procedure TStencilClipping.PreviousValue;
begin
  if Value > 0 then dec(Value);
end;

procedure TStencilClipping.Stop;
begin
  glDisable(GL_STENCIL_TEST);
  IsActive := False;
end;

{ TZBufferClipping }

{procedure TZBufferClipping.Start;
begin
  if IsActive then exit;

  glEnable(GL_DEPTH_TEST);
  glClearDepth(0.0);
  glClear(GL_DEPTH_BUFFER_BIT);
  glDepthFunc(GL_LEQUAL);
  FrontZValue := 0.0;
  BackZValue := 1.0;
  IsActive := True;
end;

procedure TZBufferClipping.NextZValues;
begin
  FrontZValue := FrontZValue + DELTA_ZVALUE;
  BackZValue := BackZValue - DELTA_ZVALUE;
end;

procedure TZBufferClipping.Stop;
begin
  glDisable(GL_DEPTH_TEST);
  FrontZValue := 0.0;
  BackZValue := 1.0;
  IsActive := False;
end; }

{ TUIClipAreaHelper }

function TUIClipAreaHelper.CheckAndAdjust(var aSurfaceArea: TRect; var aTexCoor: TQuadCoor): boolean;
var delta: integer;
  uvHorizontalRange, uvVerticalRange, pc: single;
begin
  if (aSurfaceArea.Left > Right) or (aSurfaceArea.Right < Left) or
     (aSurfaceArea.Top > Bottom) or (aSurfaceArea.Bottom < Top)  then exit(False);

  Result := True;
  uvHorizontalRange := aTexCoor[cTR].x - aTexCoor[cTL].x;
  uvVerticalRange := aTexCoor[cBL].y - aTexCoor[cTL].y;

  if aSurfaceArea.Left < Left then begin  //trim the left
    delta := Left - aSurfaceArea.Left;
    pc := delta / aSurfaceArea.Width;
    aSurfaceArea.Left := aSurfaceArea.Left + delta;
    aTexCoor[cTL].x := aTexCoor[cTL].x + uvHorizontalRange * pc;
    aTexCoor[cBL].x := aTexCoor[cBL].x + uvHorizontalRange * pc;
  end;

  if aSurfaceArea.Right > Right then begin  //trim the right
    delta := aSurfaceArea.Right - Right;
    pc := delta / aSurfaceArea.Width;
    aSurfaceArea.Right := aSurfaceArea.Right - delta;
    aTexCoor[cTR].x := aTexCoor[cTR].x - uvHorizontalRange * pc;
    aTexCoor[cBR].x := aTexCoor[cBR].x - uvHorizontalRange * pc;
  end;

  if aSurfaceArea.Top < Top then begin  //trim the top
    delta := Top - aSurfaceArea.Top;
    pc := delta / aSurfaceArea.Height;
    aSurfaceArea.Top := aSurfaceArea.Top + delta;
    aTexCoor[cTL].y := aTexCoor[cTL].y + uvVerticalRange * pc;
    aTexCoor[cTR].y := aTexCoor[cTR].y + uvVerticalRange * pc;
  end;

  if aSurfaceArea.Bottom > Bottom then begin  //trim the bottom
   delta := aSurfaceArea.Bottom - Bottom;
   pc := delta / aSurfaceArea.Height;
   aSurfaceArea.Bottom := aSurfaceArea.Bottom - delta;
   aTexCoor[cBL].y := aTexCoor[cBL].y - uvVerticalRange * pc;
   aTexCoor[cBR].y := aTexCoor[cBR].y - uvVerticalRange * pc;
  end;
end;

{$endif oglcIMPLEMENTATION}

