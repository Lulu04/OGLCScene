{
  The first part of this file is originaly written by Chris Rorden and modified for OGLCScene.
  You can find the original at
    https://github.com/neurolabusc/OpenGLCoreTutorials/gl_core_matrix.pas
}

{$ifdef oglcINTERFACE}
type

  TnMode = (nGL_PROJECTION, nGL_MODELVIEW);
  TnMat44 = packed array [0..3, 0..3] of single;
  TnMat33 = packed array [0..2, 0..2] of single;
  TnGL = record
        mode: TnMode;
        mat: array[TnMode] of TnMat44;
  end;
  PnMat44 = ^TnMat44;

  function inverseMat (Rm: TnMat44): TnMat44;
  procedure nglMatrixMode (mode: TnMode);
  procedure nglLoadIdentity ();
  procedure nglScalef(x, y, z: single);
  procedure nglRotatef(angle, x, y, z: single);
  procedure nglTranslatef(x, y, z: single);
  procedure nglOrtho (left, right, bottom, top, zNear, zFar: single);
  procedure ngluPerspective (fovy, aspect, zNear, zFar: single);
  function ngl_ModelViewProjectionMatrix : TnMat44;
  function ngl_ModelViewMatrix : TnMat44;
  function ngl_ModelViewProjectionMatrixInverse : TnMat44;
  function ngl_ModelViewMatrixInverse : TnMat44;
  function ngl_NormalMatrix: TnMat33;
  function ngl_ProjectionMatrix : TnMat44;
  function transposeMat(a: TnMat44): TnMat44;

  // addition for OGLCScene
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

    procedure SetAsTranslation(const aX, aY: single); inline;
    procedure SetAsScaling(const aX, aY: single); inline;
    procedure SetAsRotation(const aAngleDeg: single); inline; // 2D rotation is around Z axis only

    procedure LoadIdentity; inline;
    procedure AddTransform(aXPos, aYPos, aXScale, aYScale, aAngle, aXPivot, aYPivot: single; aXOffset: single=0; aYOffset: single=0);
    procedure Translate(const aX, aY: single);
    procedure Scale(const aX, aY: single);
    procedure Rotate(const aAngleDeg: single);

    function Transform(const aPt: TPointF): TPointF;
  end;
  POGLCMatrix = ^TOGLCMatrix;

  function MultMat(const m1, m2: TOGLCMatrix44): TOGLCMatrix44;

type

  { TStraightLineEquation }

  TStraightLineEquation = record
    a, b,           // y = a*x + b
    c: single;      // for vertical straight line ( for all y, x = c)
    IsVertical: boolean;
    procedure InitFrom(const pt1, pt2: TPointF); inline;
    function IsParallelWith(const aLine: TStraightLineEquation): boolean; inline;
    function ComputeIntersectionPoint(const aLine: TStraightLineEquation): TPointF; inline;
    // returns the angle in degree between this line and another
    function ComputeAngle(const aLine: TStraightLineEquation): single;
  end;

  TQuadF = array[0..3] of TPointF;


//   q0                   q1    ￪        antialiasing is done here
//   |                    |    width1
//  pt1<------line------>pt2
//   |                    |    width2
//   q3                   q2    ￬        antialiasing is done here
function LineSegmentToQuadF_PosMiddle(const pt1, pt2: TPointF; const aWidth1, aWidth2: single): TQuadF;


//  pt1<------line------>pt2    ￪
//   |                    |   width1 small value to allow antialising
//   q0                   q1
//   |                    |   width2
//   q3                   q2    ￬
function LineSegmentToQuadF_PosInside(const pt1, pt2: TPointF; const aWidth1, aWidth2: single): TQuadF;

// Q1 right side is modified and Q2 left side is modified
procedure AdjustCornerOnConsecutiveQuadF(var Q1, Q2: TQuadF);


{$endif oglcINTERFACE}

{$ifdef oglcIMPLEMENTATION}
  var  gnGL: TnGL;

  const _MatIdentity:TnMat44=((1,0,0,0),
                              (0,1,0,0),
                              (0,0,1,0),
                              (0,0,0,1));

function ngl_ModelViewProjectionMatrixInverse : TnMat44;
begin
 result := inverseMat(ngl_ModelViewProjectionMatrix);
end;

function ngl_ModelViewMatrixInverse : TnMat44;
begin
 result := inverseMat(ngl_ModelViewMatrix);
end;

function multMat(a, b: TnMat44): TnMat44;
var i,j: integer;
begin
   for i := 0 to 3 do begin
       for j := 0 to 3 do begin
           result[i, j] := A[i, 0] * B[0,j]
           + A[i, 1] * B[1, j]
           + A[i, 2] * B[2, j]
           + A[i, 3] * B[3, j];
       end;  //for j
   end; //for i
end; //multMat()

function transposeMat(a: TnMat44): TnMat44;
var i,j: integer;
begin
   for i := 0 to 3 do
       for j := 0 to 3 do
           result[i, j] := A[j,i]
end; //transposeMat()


function inverseMat (Rm: TnMat44): TnMat44;
//invert matrix see nifti_mat44_inverse( mat44 R )
// http://niftilib.sourceforge.net/c_api_html/nifti1__io_8h.html#a36
var
   r11,r12,r13,r21,r22,r23,r31,r32,r33,v1,v2,v3 , deti: double;
   Qm : TnMat44;
begin
  r11 := Rm[0,0]; r12 := Rm[0,1]; r13 := Rm[0,2];
  r21 := Rm[1,0]; r22 := Rm[1,1]; r23 := Rm[1,2];
  r31 := Rm[2,0]; r32 := Rm[2,1]; r33 := Rm[2,2];
  v1  := Rm[0,3]; v2  := Rm[1,3]; v3  := Rm[2,3];
  deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
  +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
  if( deti <> 0.0 ) then deti := 1.0 / deti ;
  Qm[0,0] := deti*( r22*r33-r32*r23) ;
  Qm[0,1] := deti*(-r12*r33+r32*r13) ;
  Qm[0,2] := deti*( r12*r23-r22*r13) ;
  Qm[0,3] := deti*(-r12*r23*v3+r12*v2*r33+r22*r13*v3
                    -r22*v1*r33-r32*r13*v2+r32*v1*r23) ;
  Qm[1,0] := deti*(-r21*r33+r31*r23) ;
  Qm[1,1] := deti*( r11*r33-r31*r13) ;
  Qm[1,2] := deti*(-r11*r23+r21*r13) ;
  Qm[1,3] := deti*( r11*r23*v3-r11*v2*r33-r21*r13*v3
                    +r21*v1*r33+r31*r13*v2-r31*v1*r23) ;
  Qm[2,0] := deti*( r21*r32-r31*r22) ;
  Qm[2,1] := deti*(-r11*r32+r31*r12) ;
  Qm[2,2] := deti*( r11*r22-r21*r12) ;
  Qm[2,3] := deti*(-r11*r22*v3+r11*r32*v2+r21*r12*v3
                    -r21*r32*v1-r31*r12*v2+r31*r22*v1) ;
  Qm[3,0] := 0;
  Qm[3,1] := 0;
  Qm[3,2] := 0;
  Qm[3,3] := 1;
  if (deti = 0.0) then Qm[3,3] := 0; // failure flag if deti = 0
  result := Qm ;
end;

function ngl_NormalMatrix: TnMat33;
//Returns normal matrix, in GLSL this would be
// mat4 NormalMatrix = transpose(inverse(ModelViewMatrix));
var
   m, q: TnMat44;
   i,j: integer;
begin
     q := inverseMat(gnGL.mat[nGL_MODELVIEW]);
     m := transposeMat(q);
     for i := 0 to 2 do //normal matrix is orthonormal, so only ignore final row/column
         for j := 0 to 2 do
             result[i,j] := m[i,j];
end;

procedure nglMatrixMode (mode: TnMode);
begin
     gnGL.mode := mode;
end;

function RSqrt(v: single): single;
begin
     result := 1/sqrt(v);
end;

function IdentityHmgMatrix: TnMat44;
var
   i,j: integer;
begin
     for i := 0 to 3 do
         for j := 0 to 3 do
             result[i,j] := 0;
     for i := 0 to 3 do
         result[i,i] := 1;
end;

function CreateGlRotateMatrix(angle, x, y, z: single) : TnMat44;
//http://www.gamedev.net/topic/600537-instead-of-glrotatef-build-a-matrix/
var
	c, s: single;
	invLen : Single;
begin
	angle:= Math.degtorad(angle);
	invLen:= RSqrt(x * x + y * y + z * z);
	x:= x * invLen;
	y:= y * invLen;
	z:= z * invLen;
	result:= IdentityHmgMatrix;
	c:= cos(angle);
	s:= sin(angle);
	result[0,0] := (x*x) * (1-c)+c; // cos(angle)
	result[1,0] := x*y * (1-c)-z*s; // -sin(angle)
	result[2,0] := x*z * (1-c)+y*s;
	result[0,1] := y*x * (1-c)+z*s; // sin(angle)
	result[1,1] := (y*y) * (1-c)+c; // cos(angle)
	result[2,1] := y*z * (1-c)-x*s;
	result[0,2] := x*z * (1-c)-y*s;
	result[1,2] := y*z * (1-c)+x*s;
	result[2,2] := (z*z) * (1-c)+c;
end;

function ngl_ModelViewProjectionMatrix : TnMat44;
 //gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;
begin
   //result := matrixMult(gnGL.mat[nGL_PROJECTION], gnGL.mat[nGL_MODELVIEW]);
   result := multMat( gnGL.mat[nGL_MODELVIEW], gnGL.mat[nGL_PROJECTION]);
end;

procedure nglRotatef(angle, x, y, z: single);
var
    m : TnMat44;
begin
     m := CreateGlRotateMatrix(angle, x, y, z);
    gnGL.mat[gnGL.mode] := multMat(m, gnGL.mat[gnGL.mode]);
end;

procedure nglTranslatef(x, y, z: single);
var
   m : TnMat44;
begin
     //m := gnGL.mat[gnGL.mode];
     m := IdentityHmgMatrix;
     m[3,0] := m[3,0] + x;
     m[3,1] := m[3,1] + y;
     m[3,2] := m[3,2] + z;
     //gnGL.mat[gnGL.mode] := m;
     gnGL.mat[gnGL.mode] := multMat(m, gnGL.mat[gnGL.mode]);
end;

procedure nglScalef(x, y, z: single);
var
   m : TnMat44;
begin
     m := IdentityHmgMatrix;
     m[0,0] :=  x;
     m[1,1] :=  y;
     m[2,2] :=  z;
     gnGL.mat[gnGL.mode] := multMat(m, gnGL.mat[gnGL.mode]);
end;

procedure nglLoadIdentity ();
begin
     gnGL.mat[gnGL.mode] := IdentityHmgMatrix
end;

procedure ngluPerspective (fovy, aspect, zNear, zFar: single);
//https://www.opengl.org/sdk/docs/man2/xhtml/gluPerspective.xml
var
  f: single;
  m : TnMat44;
begin
   m :=  IdentityHmgMatrix;
   f :=  cot(Math.degtorad(fovy)/2);
   m[0,0] := f/aspect;
   m[1,1] := f;
   m[2,2] := (zFar+zNear)/(zNear-zFar) ;
   m[3,2] := (2*zFar*zNear)/(zNear-zFar);
   m[2,3] := -1;
   m[3,3] := 0;
   //raise an exception if zNear = 0??
   gnGL.mat[gnGL.mode] := multMat(m,gnGL.mat[gnGL.mode]);
end;

procedure nglOrtho (left, right, bottom, top, zNear, zFar: single);
//https://www.opengl.org/sdk/docs/man2/xhtml/glOrtho.xml
var
   m : TnMat44;
begin
        m :=  IdentityHmgMatrix;
	m[0,0] := 2 / (right - left);
	m[1,1] := 2 / (top - bottom);
	m[2,2] := - 2 / (zFar - zNear);
	m[3,0] := - (right + left) / (right - left);
	m[3,1] := - (top + bottom) / (top - bottom);
	m[3,2] := - (zFar + zNear) / (zFar - zNear);
	//gnGL.mat[gnGL.mode] := m;
        gnGL.mat[gnGL.mode] := multMat(m,gnGL.mat[gnGL.mode]);
end;

function ngl_ModelViewMatrix : TnMat44;
begin;
  result := gnGL.mat[nGL_MODELVIEW];
end;

function ngl_ProjectionMatrix : TnMat44;
begin
  result := gnGL.mat[nGL_PROJECTION];
end;

{ TOGLCMatrix }

procedure TOGLCMatrix.LoadIdentity;
begin
  Move(_MatIdentity, Matrix, SizeOf(tnMat44));
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

{ TStraightLineEquation }

procedure TStraightLineEquation.InitFrom(const pt1, pt2: TPointF);
begin
  IsVertical := pt1.x = pt2.x;

  if not IsVertical then begin
    a := (pt2.y - pt1.y) / (pt2.x - pt1.x);  // F(x)=ax+b
    b := pt1.y - pt1.x*a;
  end else c := pt1.x; // for all y values, one have the same x
end;

function TStraightLineEquation.IsParallelWith(const aLine: TStraightLineEquation): boolean;
begin
  //Result := (IsVertical and aLine.IsVertical) or (a = aLine.a);
  Result := IsVertical and aLine.IsVertical;
  if not Result then Result := a = aLine.a;
end;

function TStraightLineEquation.ComputeIntersectionPoint(const aLine: TStraightLineEquation): TPointF;
begin
  if IsVertical and aLine.IsVertical then
    raise exception.create('both lines are vertical (parallel), intersection doesn''t exists')

  else if not IsVertical and not aLine.IsVertical then begin
    if a = aLine.a then raise exception.create('both lines are parallel, intersection doesn''t exists');
    Result.x := (aLine.b - b) / (a - aLine.a);
    Result.y := a * Result.x + b;

  end else if IsVertical then begin
    Result.x := c;
    Result.y := aLine.a * c + aLine.b;
  end else begin
    Result.x := aLine.c;
    Result.y := a * aLine.c + b;
  end;
end;

function TStraightLineEquation.ComputeAngle(const aLine: TStraightLineEquation): single;
begin
  if IsParallelWith(aLine) then Result := 0
    else Result := ArcTan((a-aLine.a)/(1+a*aLine.a))*rad2deg;
end;

function LineSegmentToQuadF_PosMiddle(const pt1, pt2: TPointF; const aWidth1, aWidth2: single): TQuadF;
var v, vv, normal: TPointF;
begin
  v := pt2-pt1;                     //   q0                   q1    ￪     ̂̌
  v.Normalize;                      //   |                    |    width1
  vv := v * aWidth1;                //  pt1.......line.......pt2
  normal := PointF(vv.y, -vv.x);   //   |                    |    width2
  Result[0] := pt1 + normal;        //   q3                   q2    ￬
  Result[1] := pt2 + normal;
  vv := v * aWidth2;
  normal := PointF(-vv.y, vv.x);
  Result[2] := pt2 + normal;
  Result[3] := pt1 + normal;
end;

function LineSegmentToQuadF_PosInside(const pt1, pt2: TPointF; const aWidth1, aWidth2: single): TQuadF;
var v, vv, normal: TPointF;
begin
  v := pt2-pt1;
  v.Normalize;
  vv := v * aWidth1;
  normal := PointF(vv.y, -vv.x);
  Result[0] := pt1 + normal;
  Result[1] := pt2 + normal;
  vv := v * aWidth2;
  normal := PointF(vv.y, -vv.x);
  Result[2] := Result[1] + normal;
  Result[3] := Result[0] + normal;
end;

procedure AdjustCornerOnConsecutiveQuadF(var Q1, Q2: TQuadF);
var F1, F2: TStraightLineEquation;
begin
  F1.InitFrom(Q1[0], Q1[1]);
  F2.InitFrom(Q2[0], Q2[1]);
  if not F1.IsParallelWith(F2) then begin
    Q1[1] := F1.ComputeIntersectionPoint(F2);
    Q2[0] := Q1[1];
  end;

  F1.InitFrom(Q1[3], Q1[2]);
  F2.InitFrom(Q2[3], Q2[2]);
  if not F1.IsParallelWith(F2) then begin
    Q1[2] := F1.ComputeIntersectionPoint(F2);
    Q2[3] := Q1[2];
  end;
end;

function TOGLCMatrix.Transform(const aPt: TPointF): TPointF;
begin
  Result.x:= Matrix[0,0]*aPt.x + Matrix[1,0]*aPt.y + Matrix[3,0];
  Result.y:= Matrix[0,1]*aPt.x + Matrix[1,1]*aPt.y + Matrix[3,1];
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

