{$ifdef oglcINTERFACE}
type

PTimerObject = ^TTimerObject;
TTimerObject = record
                Interval : QWord;
                Time     : QWord; //TTimeStamp;
                CallBack : TOGLCEvent;
                Kill     : boolean;
                Next     : PTimerObject;
                Prev     : PTimerObject;
               end;

{ TTimerManager }

TTimerManager = class(specialize TOGLCGList<TTimerObject>)
private
 procedure ProcessTimer;
public
 function Add(aCallBack: TOGLCEvent; aIntervalMSec: integer): PTimerObject;
 procedure Delete(var aTimer: PTimerObject);
 property Count: integer read FCount;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}
{ TTimerManager }

function TTimerManager.Add(aCallBack: TOGLCEvent; aIntervalMSec: integer ): PTimerObject;
begin
  Result := AddInstanceToList;
  Result^.CallBack := aCallBack ;
  Result^.Interval := QWord(aIntervalMSec) ;
  Result^.Kill:=FALSE;
  Result^.Time := GetTickCount64;
end;

procedure TTimerManager.Delete( var aTimer: PTimerObject);
begin
  if aTimer = NIL then exit;
  aTimer^.Kill := TRUE;
  aTimer := NIL;
end;

procedure TTimerManager.ProcessTimer;
var p: PTimerObject;
    tactual, timetop: QWord;
begin
  // delete all killed timers
  p := FInstance0;
  while p <> NIL do
  begin
    if p^.Next <> NIL then
    begin
      if p^.Next^.Kill then DoDelete(p^.Next)
        else p := p^.Next;
    end else p := NIL;
  end;

  // process timers
  p := FInstance0^.Next;
  tactual := GetTickCount64;
  while p <> NIL do
   begin
    timetop := p^.Time + p^.Interval;
    if timetop <= tactual then begin
      p^.Time := timetop;
      if p^.CallBack <> NIL then p^.CallBack();
    end;
    p := p^.Next;
   end;
end;


{$endif oglcIMPLEMENTATION}

