unit u_datamodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    ILIcon24: TImageList;
    ImageList1: TImageList;
    ILIconAlign: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    FPath: string;
    procedure AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
  public
    procedure RedrawImageForCursor;
    procedure RedrawImageForIconAlign;
  end;

var
  DataModule1: TDataModule1;

implementation

uses u_app_pref, form_main, BGRABitmap, BGRABitmapTypes, OGLCScene;

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  RedrawImageForCursor;
  RedrawImageForIconAlign;
end;

procedure TDataModule1.AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
var ima: TBGRABitmap;
begin
  ima := LoadBitmapFromSVG(FPath+aSVGFilename, aIL.Width, -1);
  aIL.Add(ima.Bitmap, NIL);
  ima.Free;
end;

procedure TDataModule1.RedrawImageForCursor;
begin
  ILIcon24.BeginUpdate;
  ILIcon24.Clear;
  ILIcon24.Width := FormMain.ScaleDesignToForm(24);
  ILIcon24.Height := ILIcon24.Width;

  FPath := GetIconFolder;
  AddImageToImageList('Select.svg', ILIcon24);
  AddImageToImageList('Line.svg', ILIcon24);
  AddImageToImageList('Circle.svg', ILIcon24);
  AddImageToImageList('Rectangle.svg', ILIcon24);
  AddImageToImageList('Polygon.svg', ILIcon24);
  AddImageToImageList('AddNode.svg', ILIcon24);
  AddImageToImageList('Point.svg', ILIcon24);

  ILIcon24.EndUpdate;
end;

procedure TDataModule1.RedrawImageForIconAlign;
begin
  ILIconAlign.BeginUpdate;
  ILIconAlign.Clear;
  ILIconAlign.Width := FormMain.ScaleDesignToForm(24);
  ILIconAlign.Height := ILIconAlign.Width;

  FPath := GetIconFolder;
  // 0
  AddImageToImageList('AlignHRightToLeft.svg', ILIconAlign);
  AddImageToImageList('AlignHRightToCenter.svg', ILIconAlign);
  AddImageToImageList('AlignHLeft.svg', ILIconAlign);
  AddImageToImageList('AlignHCenter.svg', ILIconAlign);
  AddImageToImageList('AlignHRight.svg', ILIconAlign);
  AddImageToImageList('AlignHLeftToCenter.svg', ILIconAlign);
  AddImageToImageList('AlignHLeftToRight.svg', ILIconAlign);

  // 7
  AddImageToImageList('AlignVBottomToTop.svg', ILIconAlign);
  AddImageToImageList('AlignVBottomToCenter.svg', ILIconAlign);
  AddImageToImageList('AlignVTop.svg', ILIconAlign);
  AddImageToImageList('AlignVCenter.svg', ILIconAlign);
  AddImageToImageList('AlignVBottom.svg', ILIconAlign);
  AddImageToImageList('AlignVTopToCenter.svg', ILIconAlign);
  AddImageToImageList('AlignVTopToBottom.svg', ILIconAlign);

  // 14
  AddImageToImageList('Rotate90CCW.svg', ILIconAlign);
  AddImageToImageList('Rotate90CW.svg', ILIconAlign);
  AddImageToImageList('MirrorH.svg', ILIconAlign);
  AddImageToImageList('MirrorV.svg', ILIconAlign);
  AddImageToImageList('ShiftTop.svg', ILIconAlign);
  AddImageToImageList('ShiftTopOneStep.svg', ILIconAlign);
  AddImageToImageList('ShiftBackOneStep.svg', ILIconAlign);
  AddImageToImageList('ShiftBack.svg', ILIconAlign);

  // 22
  AddImageToImageList('ZoomAll.svg', ILIconAlign);
  AddImageToImageList('ZoomOnSelection.svg', ILIconAlign);
end;

end.

