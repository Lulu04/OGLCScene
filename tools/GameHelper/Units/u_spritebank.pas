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
  surfaces: string;
  procedure InitDefault;
end;
PSpriteBankItem = ^TSpriteBankItem;

{ TSpriteBank }

TSpriteBank = class(specialize TVector<TSpriteBankItem>)
  function AddEmpty: PSpriteBankItem;

  procedure SaveTo(t:TStringList);
  procedure LoadFrom(t:TStringList);
end;

var
  SpriteBank: TSpriteBank;

implementation

{ TSpriteBankItem }

procedure TSpriteBankItem.InitDefault;
begin
  FillChar(Self, SizeOf(TSpriteBankItem), 0);
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
begin
  prop.Init('|');

  t.Add('[SPRITE BANK]');
  t.Add(prop.PackedProperty);
end;

procedure TSpriteBank.LoadFrom(t: TStringList);
var prop: TProperties;
begin
  Clear;
  if prop.SplitFrom(t, '[SPRITE BANK]', '|') then begin

  end;
end;

end.

