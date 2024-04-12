{$ifdef oglcINTERFACE}
type
PDelayObject = ^TDelayObject;
TDelayObject = record
                DelayMs  : integer;
                CallBack : TDelayCallBack;
                UserValue: word;
                Next     : PDelayObject;
                Prev     : PDelayObject;
end;

{ TDelayManager }

TDelayManager = class(specialize TOGLCGList<TDelayObject>)
 Constructor Create;
private
 FActual, FOrigin: QWord;
 procedure ProcessDelay;
public
 function Add(aDelayMSec: integer; aCallBack: TDelayCallBack; aUserValue: word): PDelayObject;
end;

var
 DelayManager: TDelayManager;


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

var
 TimerManager: TTimerManager;

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


{ TDelayManager }

constructor TDelayManager.Create;
begin
 Inherited Create;
 FOrigin := GetTickCount64;
end;

procedure TDelayManager.ProcessDelay;
var p :PDelayObject ;
    delta : QWord;
begin
 p := FInstance0;
 Factual := GetTickCount64;
 delta := FActual - FOrigin;
 while p^.Next <> NIL do
  begin
   p^.Next^.DelayMs := p^.Next^.DelayMs - delta;
   if p^.Next^.DelayMs <= 0
     then begin
           if p^.Next^.CallBack <> NIL then p^.Next^.CallBack ( p^.Next^.UserValue );
           DoDelete( p^.Next );
     end else p := p^.Next;
  end;
 FOrigin := FActual;
end;

function TDelayManager.Add(aDelayMSec: integer; aCallBack: TDelayCallBack; aUserValue: word): PDelayObject;
begin
 Result := AddInstanceToList;
 Result^.DelayMs := aDelayMSec;
 Result^.CallBack := aCallBack;
 Result^.UserValue := aUserValue;
end;
{$endif oglcIMPLEMENTATION}

