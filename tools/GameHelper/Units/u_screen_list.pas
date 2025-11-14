unit u_screen_list;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  OGLCScene, gvector,
  BGRABitmap, BGRABitmapTypes,
  u_undo_redo, u_texture_list, u_layerlist;

type
TSpriteOnScreen = record
 spriteName: string;
end;
PSpriteOnScreen = ^TSpriteOnScreen;
TArrayOfSpriteOnScreen = array of TSpriteOnScreen;

TScreenItem = record
  name: string;
  decorName: string;
  useCamera: boolean;
  useOneCameraPerDecorLayer: boolean;
  sprites: TStringArray;
end;
PScreenItem =^TScreenItem;

TScreenBank = class(specialize TVector<TScreenItem>)

 { constructor Create;
  destructor Destroy; override;

  function AddEmpty: PScreenItem;
  function NameExists(const aName: string): boolean;
  function GetItemByName(const aName: string): PScreenItem;
  procedure DeleteByIndex(aIndex: integer);
  procedure DeleteByName(const aName: string);

  //procedure Clear; reintroduce;

  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);  }
end;

var ScreenBank: TScreenBank;

implementation

end.

