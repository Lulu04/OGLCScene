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
    procedure DataModuleCreate(Sender: TObject);
  private
    FPath: string;
    procedure AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
  public
    procedure RedrawImageForCursor;
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

end.

