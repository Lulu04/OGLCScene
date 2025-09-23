{$ifdef oglcINTERFACE}
{
  This file is originaly written by Chris Rorden and modified for OGLCScene.
  You can find the original at
    https://github.com/neurolabusc/OpenGLCoreTutorials/blob/master/gl_core_matrix.pas
}


type
TOGLCMatrix44 = packed array [0..3, 0..3] of single;
POGLCMatrix44 = ^TOGLCMatrix44;

{ TOGLCMatrix }

TOGLCMatrix = record
  Matrix: TOGLCMatrix44;
  function Data: pointer; inline;
  procedure CopyFrom(const aMat: TOGLCMatrix44); inline;
  procedure CopyFrom(const aMat: POGLCMatrix44); inline;
  procedure CopyFrom(const aMat: TOGLCMatrix); inline;
  function EqualTo(const aMat: TOGLCMatrix): boolean; inline;
  procedure Ortho(left, right, bottom, top, zNear, zFar: single);

  procedure SetAsTranslation(const aX, aY: single);
  procedure SetAsScaling(const aX, aY: single);
  procedure SetAsRotation(const aAngleDeg: single);  // 2D rotation is around Z axis only

  procedure LoadIdentity; inline;
  procedure AddTransform(aXPos, aYPos, aXScale, aYScale, aAngle, aXPivot, aYPivot: single; aXOffset: single=0; aYOffset: single=0);
  procedure Translate(const aX, aY: single);
  procedure Scale(const aX, aY: single);
  procedure Rotate(const aAngleDeg: single);

  function Transform(const aPt: TPointF): TPointF; overload; inline;
  function Transform(const aRect: TRectF): TRectF; overload;
  function Transform(const aQuad: TQuadF): TQuadF; overload;
  function Transform(const aPts: ArrayOfTPointF): ArrayOfTPointF; overload;

  function Inverse: TOGLCMatrix;
end;
POGLCMatrix = ^TOGLCMatrix;

function MultMat(const m1, m2: TOGLCMatrix44): TOGLCMatrix44;
// this one don't work, see the next
function InverseMat(const m: TOGLCMatrix44): TOGLCMatrix44;
// https://github.com/neurolabusc/Metal-Demos/blob/5ddd15005f7eb9a48620cf74ebd929dc11f83c73/common/VectorMath.pas
function InverseMat2(const m: TOGLCMatrix44): TOGLCMatrix44;

{$endif oglcINTERFACE}

{$ifdef oglcIMPLEMENTATION}

  const _MatIdentity: TOGLCMatrix44 =
      ((1,0,0,0),
      (0,1,0,0),
      (0,0,1,0),
      (0,0,0,1));

{ TOGLCMatrix }

procedure TOGLCMatrix.LoadIdentity;
begin
  Move(_MatIdentity, Matrix, SizeOf(TOGLCMatrix44));
end;

procedure TOGLCMatrix.AddTransform(aXPos, aYPos, aXScale, aYScale, aAngle, aXPivot, aYPivot: single; aXOffset: single=0; aYOffset: single=0);
var doscale, dorotate: boolean;
begin
  doscale := (aXScale <> 1.0) or (aYScale <> 1.0);
  dorotate := aAngle <> 0;
  if doscale or dorotate then begin
    Translate(aXPos+aXPivot, aYPos+aYPivot);
    if doscale then Scale(aXScale, aYScale);
    if dorotate then Rotate(aAngle);
    Translate(-aXPivot+aXOffset, -aYPivot+aYOffset);
  end else
    Translate(aXPos+aXOffset, aYPos+aYOffset);
end;

procedure TOGLCMatrix.Translate(const aX, aY: single);
var m: TOGLCMatrix;
begin
  m.SetAsTranslation(aX, aY);
  Matrix := MultMat(m.Matrix, Matrix);
end;

procedure TOGLCMatrix.Scale(const aX, aY: single);
var m: TOGLCMatrix;
begin
  m.SetAsScaling(aX, aY);
  Matrix := MultMat(m.Matrix, Matrix);
end;

procedure TOGLCMatrix.Rotate(const aAngleDeg: single);
var m: TOGLCMatrix;
begin
  m.SetAsRotation(aAngleDeg);
  Matrix := MultMat(m.Matrix, Matrix);
end;

function MultMat(const m1, m2: TOGLCMatrix44): TOGLCMatrix44;
var i, j: integer;
begin
  for i := 0 to 3 do
   for j := 0 to 3 do
     Result[i, j] := m1[i, 0] * m2[0,j] + m1[i, 1] * m2[1, j] +
                     m1[i, 2] * m2[2, j] + m1[i, 3] * m2[3, j];
end;

function InverseMat(const m: TOGLCMatrix44): TOGLCMatrix44;
//invert matrix see nifti_mat44_inverse( mat44 R )
// http://niftilib.sourceforge.net/c_api_html/nifti1__io_8h.html#a36
var
   r11,r12,r13,r21,r22,r23,r31,r32,r33,v1,v2,v3 , deti: double;
begin
  r11 := m[0,0]; r12 := m[0,1]; r13 := m[0,2];
  r21 := m[1,0]; r22 := m[1,1]; r23 := m[1,2];
  r31 := m[2,0]; r32 := m[2,1]; r33 := m[2,2];
  v1  := m[0,3]; v2  := m[1,3]; v3  := m[2,3];
  deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
  +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
  if( deti <> 0.0 ) then deti := 1.0 / deti ;
  Result[0,0] := deti*( r22*r33-r32*r23) ;
  Result[0,1] := deti*(-r12*r33+r32*r13) ;
  Result[0,2] := deti*( r12*r23-r22*r13) ;
  Result[0,3] := deti*(-r12*r23*v3+r12*v2*r33+r22*r13*v3
                    -r22*v1*r33-r32*r13*v2+r32*v1*r23) ;
  Result[1,0] := deti*(-r21*r33+r31*r23) ;
  Result[1,1] := deti*( r11*r33-r31*r13) ;
  Result[1,2] := deti*(-r11*r23+r21*r13) ;
  Result[1,3] := deti*( r11*r23*v3-r11*v2*r33-r21*r13*v3
                    +r21*v1*r33+r31*r13*v2-r31*v1*r23) ;
  Result[2,0] := deti*( r21*r32-r31*r22) ;
  Result[2,1] := deti*(-r11*r32+r31*r12) ;
  Result[2,2] := deti*( r11*r22-r21*r12) ;
  Result[2,3] := deti*(-r11*r22*v3+r11*r32*v2+r21*r12*v3
                    -r21*r32*v1-r31*r12*v2+r31*r22*v1) ;
  Result[3,0] := 0;
  Result[3,1] := 0;
  Result[3,2] := 0;
  Result[3,3] := 1;
  if (deti = 0.0) then Result[3,3] := 0; // failure flag if deti = 0
end;

function InverseMat2(const m: TOGLCMatrix44): TOGLCMatrix44;
var t0, t4, t8, t12, d: single;
begin
  t0:=(((m[1,1]*m[2,2]*m[3,3])-(m[1,1]*m[2,3]*m[3,2]))-(m[2,1]*m[1,2]*m[3,3])+(m[2,1]*m[1,3]*m[3,2])+(m[3,1]*m[1,2]*m[2,3]))-(m[3,1]*m[1,3]*m[2,2]);
  t4:=((((-(m[1,0]*m[2,2]*m[3,3]))+(m[1,0]*m[2,3]*m[3,2])+(m[2,0]*m[1,2]*m[3,3]))-(m[2,0]*m[1,3]*m[3,2]))-(m[3,0]*m[1,2]*m[2,3]))+(m[3,0]*m[1,3]*m[2,2]);
  t8:=((((m[1,0]*m[2,1]*m[3,3])-(m[1,0]*m[2,3]*m[3,1]))-(m[2,0]*m[1,1]*m[3,3]))+(m[2,0]*m[1,3]*m[3,1])+(m[3,0]*m[1,1]*m[2,3]))-(m[3,0]*m[1,3]*m[2,1]);
  t12:=((((-(m[1,0]*m[2,1]*m[3,2]))+(m[1,0]*m[2,2]*m[3,1])+(m[2,0]*m[1,1]*m[3,2]))-(m[2,0]*m[1,2]*m[3,1]))-(m[3,0]*m[1,1]*m[2,2]))+(m[3,0]*m[1,2]*m[2,1]);
  d:=(m[0,0]*t0)+(m[0,1]*t4)+(m[0,2]*t8)+(m[0,3]*t12);
  if d<>0.0 then begin
   d:=1.0/d;
   result[0,0]:=t0*d;
   result[0,1]:=(((((-(m[0,1]*m[2,2]*m[3,3]))+(m[0,1]*m[2,3]*m[3,2])+(m[2,1]*m[0,2]*m[3,3]))-(m[2,1]*m[0,3]*m[3,2]))-(m[3,1]*m[0,2]*m[2,3]))+(m[3,1]*m[0,3]*m[2,2]))*d;
   result[0,2]:=(((((m[0,1]*m[1,2]*m[3,3])-(m[0,1]*m[1,3]*m[3,2]))-(m[1,1]*m[0,2]*m[3,3]))+(m[1,1]*m[0,3]*m[3,2])+(m[3,1]*m[0,2]*m[1,3]))-(m[3,1]*m[0,3]*m[1,2]))*d;
   result[0,3]:=(((((-(m[0,1]*m[1,2]*m[2,3]))+(m[0,1]*m[1,3]*m[2,2])+(m[1,1]*m[0,2]*m[2,3]))-(m[1,1]*m[0,3]*m[2,2]))-(m[2,1]*m[0,2]*m[1,3]))+(m[2,1]*m[0,3]*m[1,2]))*d;
   result[1,0]:=t4*d;
   result[1,1]:=((((m[0,0]*m[2,2]*m[3,3])-(m[0,0]*m[2,3]*m[3,2]))-(m[2,0]*m[0,2]*m[3,3])+(m[2,0]*m[0,3]*m[3,2])+(m[3,0]*m[0,2]*m[2,3]))-(m[3,0]*m[0,3]*m[2,2]))*d;
   result[1,2]:=(((((-(m[0,0]*m[1,2]*m[3,3]))+(m[0,0]*m[1,3]*m[3,2])+(m[1,0]*m[0,2]*m[3,3]))-(m[1,0]*m[0,3]*m[3,2]))-(m[3,0]*m[0,2]*m[1,3]))+(m[3,0]*m[0,3]*m[1,2]))*d;
   result[1,3]:=(((((m[0,0]*m[1,2]*m[2,3])-(m[0,0]*m[1,3]*m[2,2]))-(m[1,0]*m[0,2]*m[2,3]))+(m[1,0]*m[0,3]*m[2,2])+(m[2,0]*m[0,2]*m[1,3]))-(m[2,0]*m[0,3]*m[1,2]))*d;
   result[2,0]:=t8*d;
   result[2,1]:=(((((-(m[0,0]*m[2,1]*m[3,3]))+(m[0,0]*m[2,3]*m[3,1])+(m[2,0]*m[0,1]*m[3,3]))-(m[2,0]*m[0,3]*m[3,1]))-(m[3,0]*m[0,1]*m[2,3]))+(m[3,0]*m[0,3]*m[2,1]))*d;
   result[2,2]:=(((((m[0,0]*m[1,1]*m[3,3])-(m[0,0]*m[1,3]*m[3,1]))-(m[1,0]*m[0,1]*m[3,3]))+(m[1,0]*m[0,3]*m[3,1])+(m[3,0]*m[0,1]*m[1,3]))-(m[3,0]*m[0,3]*m[1,1]))*d;
   result[2,3]:=(((((-(m[0,0]*m[1,1]*m[2,3]))+(m[0,0]*m[1,3]*m[2,1])+(m[1,0]*m[0,1]*m[2,3]))-(m[1,0]*m[0,3]*m[2,1]))-(m[2,0]*m[0,1]*m[1,3]))+(m[2,0]*m[0,3]*m[1,1]))*d;
   result[3,0]:=t12*d;
   result[3,1]:=(((((m[0,0]*m[2,1]*m[3,2])-(m[0,0]*m[2,2]*m[3,1]))-(m[2,0]*m[0,1]*m[3,2]))+(m[2,0]*m[0,2]*m[3,1])+(m[3,0]*m[0,1]*m[2,2]))-(m[3,0]*m[0,2]*m[2,1]))*d;
   result[3,2]:=(((((-(m[0,0]*m[1,1]*m[3,2]))+(m[0,0]*m[1,2]*m[3,1])+(m[1,0]*m[0,1]*m[3,2]))-(m[1,0]*m[0,2]*m[3,1]))-(m[3,0]*m[0,1]*m[1,2]))+(m[3,0]*m[0,2]*m[1,1]))*d;
   result[3,3]:=(((((m[0,0]*m[1,1]*m[2,2])-(m[0,0]*m[1,2]*m[2,1]))-(m[1,0]*m[0,1]*m[2,2]))+(m[1,0]*m[0,2]*m[2,1])+(m[2,0]*m[0,1]*m[1,2]))-(m[2,0]*m[0,2]*m[1,1]))*d;
  end;
end;


function TOGLCMatrix.Transform(const aPt: TPointF): TPointF;
begin
  Result.x := Matrix[0,0]*aPt.x + Matrix[1,0]*aPt.y + Matrix[3,0];
  Result.y := Matrix[0,1]*aPt.x + Matrix[1,1]*aPt.y + Matrix[3,1];
end;

function TOGLCMatrix.Transform(const aRect: TRectF): TRectF;
begin
  Result.TopLeft := Transform(aRect.TopLeft);
  Result.BottomRight := Transform(aRect.BottomRight);
end;

function TOGLCMatrix.Transform(const aQuad: TQuadF): TQuadF;
begin
  Result[cTL] := Transform(aQuad[cTL]);
  Result[cTR] := Transform(aQuad[cTR]);
  Result[cBL] := Transform(aQuad[cBL]);
  Result[cBR] := Transform(aQuad[cBR]);
end;

function TOGLCMatrix.Transform(const aPts: ArrayOfTPointF): ArrayOfTPointF;
var i: integer;
begin
  Result := NIL;
  SetLength(Result, Length(aPts));
  for i:=0 to High(aPts) do
    Result[i] := Transform(aPts[i]);
end;

function TOGLCMatrix.Inverse: TOGLCMatrix;
begin
  Result.Matrix := InverseMat2(Self.Matrix);
end;


function TOGLCMatrix.Data: pointer;
begin
  Result := @Matrix[0,0];
end;

procedure TOGLCMatrix.CopyFrom(const aMat: TOGLCMatrix44);
begin
  Move(aMat, Matrix, SizeOf(TOGLCMatrix44));
end;

procedure TOGLCMatrix.CopyFrom(const aMat: POGLCMatrix44);
begin
  Move(aMat^, Matrix, SizeOf(TOGLCMatrix44));
end;

procedure TOGLCMatrix.CopyFrom(const aMat: TOGLCMatrix);
begin
  Move(aMat.Matrix, Matrix, SizeOf(TOGLCMatrix44));
end;

function TOGLCMatrix.EqualTo(const aMat: TOGLCMatrix): boolean;
begin
  Result := CompareDWord(Self.Matrix, aMat.Matrix, 16) = 0; // 1 DWord = 1 single
end;

procedure TOGLCMatrix.Ortho(left, right, bottom, top, zNear, zFar: single);
var rml,tmb,fmn: single;
begin
  rml:=Right-Left;
  tmb:=Top-Bottom;
  fmn:=zFar-zNear;
  Matrix[0,0]:=2.0/rml;
  Matrix[0,1]:=0.0;
  Matrix[0,2]:=0.0;
  Matrix[0,3]:=0.0;
  Matrix[1,0]:=0.0;
  Matrix[1,1]:=2.0/tmb;
  Matrix[1,2]:=0.0;
  Matrix[1,3]:=0.0;
  Matrix[2,0]:=0.0;
  Matrix[2,1]:=0.0;
  Matrix[2,2]:=(-2.0)/fmn;
  Matrix[2,3]:=0.0;
  Matrix[3,0]:=(-(Right+Left))/rml;
  Matrix[3,1]:=(-(Top+Bottom))/tmb;
  Matrix[3,2]:=(-(zFar+zNear))/fmn;
  Matrix[3,3]:=1.0;
end;

procedure TOGLCMatrix.SetAsTranslation(const aX, aY: single);
begin
  LoadIdentity;
  Matrix[3,0] := aX;
  Matrix[3,1] := aY;
end;

procedure TOGLCMatrix.SetAsScaling(const aX, aY: single);
begin
  LoadIdentity;
  Matrix[0,0] := aX;
  Matrix[1,1] := aY;
  Matrix[2,2] := 1.0;
end;

procedure TOGLCMatrix.SetAsRotation(const aAngleDeg: single);
var c, s: single;
begin
  LoadIdentity;
  SinCos(aAngleDeg*deg2rad, s, c);
  Matrix[0,0] := c;
  Matrix[1,0] := -s;
  Matrix[2,0] := 0;
  Matrix[0,1] := s;
  Matrix[1,1] := c;
  Matrix[2,1] := 0;
  Matrix[0,2] := 0;
  Matrix[1,2] := 0;
  Matrix[2,2] := 1;
end;

{$endif oglcIMPLEMENTATION}

