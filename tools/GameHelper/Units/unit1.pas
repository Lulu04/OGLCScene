unit Unit1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene,
  BGRABitmap, BGRABitmapTypes;

type




TDecorsContainer = class(TSimpleSurfaceWithEffect)
private type
  TDecorItem = record
    texture: PTexture;
    x, y, w, h, pivotX, pivotY, angle, opacity: single;
    flipIndex: integer;
    computedtint: TColorF;
  end;
  TDecorItemList = class(specialize TVector<TDecorItem>);
  var
  FItemList: TDecorItemList;
public
  class LoadTexture(aAtlas: TAtlas);
  constructor Create(aParentScene: TOGLCScene);
  destructor Destroy; override;
  procedure LoadFromFile(const aFilename: string);
end;


implementation

end.

