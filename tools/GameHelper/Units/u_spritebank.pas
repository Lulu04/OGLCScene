unit u_spritebank;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  OGLCScene, gvector,
  u_surface_list;

type

{ TSpriteBankItem }

TSpriteBankItem = record
  name,
  textures,
  surfaces: string;
  procedure InitDefault;
  function SaveToString: string;
  procedure LoadFromString(const s: string);
end;
PSpriteBankItem = ^TSpriteBankItem;

{ TSpriteBank }

TSpriteBank = class(specialize TVector<TSpriteBankItem>)
  function AddEmpty: PSpriteBankItem;

  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);
end;

var
  SpriteBank: TSpriteBank;

implementation

{ TSpriteBankItem }

procedure TSpriteBankItem.InitDefault;
begin
  FillChar(Self, SizeOf(TSpriteBankItem), 0);
end;

function TSpriteBankItem.SaveToString: string;
begin
  Result := name+'#'+textures+'#'+surfaces;
end;

procedure TSpriteBankItem.LoadFromString(const s: string);
var A: TStringArray;
begin
  A := s.Split(['#']);
  name := A[0];
  textures := A[1];
  surfaces := A[2];
end;

{ TSpriteBank }

function TSpriteBank.AddEmpty: PSpriteBankItem;
var o: TSpriteBankItem;
begin
  o.InitDefault;
  PushBack(o);
  Result := Mutable[Size-1];
end;

procedure TSpriteBank.SaveTo(t: TStringList);
var prop: TProperties;
  i: SizeUInt;
begin
  t.Add('[SPRITE BANK]');
  t.Add(integer(Size).ToString);
  if Size > 0 then
    for i:=0 to Size-1 do begin
      t.Add(Mutable[i]^.name);
      t.Add(Mutable[i]^.textures);
      t.Add(Mutable[i]^.surfaces);
    end;
end;

procedure TSpriteBank.LoadFrom(t: TStringList);
var k, c, i: integer;
  o: TSpriteBankItem;
begin
  Clear;
  k := t.IndexOf('[SPRITE BANK]');
  if k = -1 then exit;

  inc(k);
  c := t.Strings[k].ToInteger;

  if c = 0 then exit;
  for i:=0 to c-1 do begin
    inc(k);
    o.name := t.Strings[k];
    inc(k);
    o.textures := t.Strings[k];
    inc(k);
    o.surfaces := t.Strings[k];
    PushBack(o);
  end;
end;

end.

