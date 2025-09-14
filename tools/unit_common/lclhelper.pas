unit LCLHelper;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, StdCtrls, ComCtrls, Buttons, ExtCtrls;

type

TArrayOfInteger = array of integer;

{ TListBoxHelper }

TListBoxHelper = class helper for TListBox
 private
   function SaveSelectedIndexes: TArrayOfInteger;
   procedure LoadSelectedIndexes( A: TArrayOfInteger; aOffset: integer );
 public
    // The selected items are moved by adding aOffset to their native index.
    // aOffset is first truncated according to the possible moves.
    // the function return this truncated offset.
   function MoveSelection(aOffset: integer): integer;

   procedure MoveSelectionToBeginning;
   procedure MoveSelectionToEnd;

   procedure MoveSelectionUp;
   procedure MoveSelectionDown;
   // -1 if none
   function FirstSelectedIndex: integer;
   function LastSelectedIndex: integer;
end;


{ TTrackBarHelper }

TTrackBarHelper = class helper for TTrackBar
private
  function GetPercent: single;
  procedure SetPercent(AValue: single);
  function DeltaValue: integer;
 public
   // the current normalized percentage
   // the percentage value is normalized between [0..1]
   property Percent: single read GetPercent write SetPercent;
end;


implementation

uses Math;

{ TTrackBarHelper }

function TTrackBarHelper.GetPercent: single;
begin
  Result := (Position+Abs(Min))/DeltaValue;
end;

procedure TTrackBarHelper.SetPercent(AValue: single);
begin
  AValue:=EnsureRange(AValue, 0, 1);
  Position := Round( DeltaValue*AValue );
end;

function TTrackBarHelper.DeltaValue: integer;
begin
  Result := Abs(Max-Min);
end;

{ TListBoxHelper }

function TListBoxHelper.SaveSelectedIndexes: TArrayOfInteger;
var i, k: Integer;
begin
 SetLength( Result{%H-}, Self.SelCount );
 if Self.SelCount=0 then exit;

 k := 0;
 for i:=0 to Self.Count-1 do
   if Self.Selected[i] then begin
    Result[k] := i;
    inc(k);
   end;
end;

procedure TListBoxHelper.LoadSelectedIndexes(A: TArrayOfInteger; aOffset: integer);
var i, k: Integer;
begin
 if Length(A)=0 then exit;
 if A[0]+aOffset<0 then aOffset := 0;
 if A[High(A)]+aOffset>=Self.Count then aOffset := 0;

 for i:=0 to High(A) do begin
   k := A[i]+aOffset;
   Self.Selected[k] := TRUE;
 end;
end;

function TListBoxHelper.MoveSelection(aOffset: integer): integer;
var i, index, newindex: integer;
  A: TArrayOfInteger;
begin
 Result:=0;
 if (SelCount=0) or (aOffset=0) then exit;

 if aOffset<0 then begin
   // move up
   index:=FirstSelectedIndex;
   if index=0 then exit;
   newindex:=index+aOffset;
   if newindex<0 then newindex:=0;
   aOffset:=newindex-index;
   A := SaveSelectedIndexes;
   for i:=index to Count-1 do
    if Selected[i] then begin
      Items.Move(i, i+aOffset);
    end;
   LoadSelectedIndexes(A, aOffset);
 end else begin
   // move down
   index:=LastSelectedIndex;
   if index=Count-1 then exit;
   newindex:=index+aOffset;
   if newindex>Count-1 then newindex:=Count-1;
   aOffset:=newindex-index;
   A := SaveSelectedIndexes;
   for i:=Count-2 downto 0 do
    if Selected[i] then begin
      Items.Move(i, i+aOffset);
    end;
   LoadSelectedIndexes(A, aOffset);
 end;
 Result:= aOffset;
end;

procedure TListBoxHelper.MoveSelectionToBeginning;
var i: integer;
begin
  i:=FirstSelectedIndex;
  if i>-1
   then MoveSelection(-i);
end;

procedure TListBoxHelper.MoveSelectionToEnd;
var i: integer;
begin
  i:=LastSelectedIndex;
  if i>-1
   then MoveSelection(Count-i-1);
end;

procedure TListBoxHelper.MoveSelectionUp;
begin
 MoveSelection(-1);
end;

procedure TListBoxHelper.MoveSelectionDown;
begin
 MoveSelection(1);
end;

function TListBoxHelper.FirstSelectedIndex: integer;
var i: integer;
begin
  Result:=-1;
  for i:=0 to Count-1 do
   if Selected[i] then begin
     Result:=i;
     exit;
   end;
end;

function TListBoxHelper.LastSelectedIndex: integer;
var i: integer;
begin
  Result:=-1;
  for i:=Count-1 downto 0 do
   if Selected[i] then begin
     Result:=i;
     exit;
   end;
end;

end.

