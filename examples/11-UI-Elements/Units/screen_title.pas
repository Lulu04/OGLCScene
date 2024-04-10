unit screen_title;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to contains all images in a single texture

  FtexFont: TTexturedFont;
  FtexWarning, FtexChecked, FtexUnchecked: PTexture;

  FLabel1, FLabel2, FLabel3, FLabel4, FLabel5, FLabel6, FLabel7, FLabel8, FLabel9, FLabel10, FLabel11: TUILabel;
  FButton1, FButtonClear: TUIButton;
  FPanel1, FPanel2, FPanel3, FPanel4, FPanel5: TUIPanel;
  FCheckBox1, FCheckBox2: TUICheck;
  FRadio1, FRadio2, FRadio3: TUIRadio;
  FProgressBar1, FProgressBar2: TUIProgressBar;
  FScrollBar1: TUIScrollBar;
  FListBox1: TUIListBox;
  FScrollBox1: TUIScrollBox;
  FTextArea1: TUITextArea;
  FTheme1, FTheme2, FTheme3: TUIRadio;

  procedure InitCallbackOn(aSurface: TUIClickableObject);
  procedure ProcessUIClick(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessUIMouseEnter(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessUIMouseLeave(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessUIMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ProcessUIMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ProcessUIMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  procedure ProcessUICheckboxChange(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessUIRadioChange(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessListBoxSelectionChange(Sender: TSimpleSurfaceWithEffect);
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const aElapsedTime: single); override;

  procedure SetColorTheme;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, Graphics, form_main;

{ TScreenDemo }

procedure TScreenDemo.InitCallbackOn(aSurface: TUIClickableObject);
begin
  aSurface.OnClick := @ProcessUIClick;
  aSurface.OnMouseEnter := @ProcessUIMouseEnter;
  aSurface.OnMouseLeave := @ProcessUIMouseLeave;
  aSurface.OnMouseDown := @ProcessUIMouseDown;
  aSurface.OnMouseUp := @ProcessUIMouseUp;
  aSurface.OnMouseWheel := @processUIMouseWheel;
end;

procedure TScreenDemo.ProcessUIClick(Sender: TSimpleSurfaceWithEffect);
var s: string;
begin
  s := '';
  if Sender = FButtonClear then FTextArea1.Text.Caption := '';
  if Sender = FLabel1 then s := 'Label1 - clicked';
  if Sender = FButton1 then s := 'Button1 - clicked';
  if Sender = FPanel1 then s := 'Panel1 - clicked';
  if Sender = FCheckBox1 then s := 'CheckBox1 - clicked';
  if Sender = FScrollBar1 then FLabel3.Caption := Format('TUIScrollBar     %d/%d  page size %d', [FScrollBar1.Position, FScrollBar1.Max, FScrollBar1.PageSize]);

  if s <> '' then begin
    FTextArea1.Text.Caption := FTextArea1.Text.Caption + #10 + s;
    FTextArea1.MakeLastLineVisible;
  end;
end;

procedure TScreenDemo.ProcessUIMouseEnter(Sender: TSimpleSurfaceWithEffect);
var s: string;
begin
  s := '';
  if Sender = FLabel1 then s := 'Label1 - mouse enter';
  if Sender = FButton1 then s := 'Button1 - mouse enter';
  if Sender = FPanel1 then s := 'Panel1 - mouse enter';
  if Sender = FCheckBox1 then s := 'CheckBox1 - mouse enter';

  if s <> '' then begin
    FTextArea1.Text.Caption := FTextArea1.Text.Caption + #10 + s;
    FTextArea1.MakeLastLineVisible;
  end;
end;

procedure TScreenDemo.ProcessUIMouseLeave(Sender: TSimpleSurfaceWithEffect);
var s: string;
begin
  s := '';
  if Sender = FLabel1 then s := 'Label1 - mouse leave';
  if Sender = FButton1 then s := 'Button1 - mouse leave';
  if Sender = FPanel1 then s := 'Panel1 - mouse leave';
  if Sender = FCheckBox1 then s := 'CheckBox1 - mouse leave';

  if s <> '' then begin
    FTextArea1.Text.Caption := FTextArea1.Text.Caption + #10 + s;
    FTextArea1.MakeLastLineVisible;
  end;
end;

procedure TScreenDemo.ProcessUIMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var s, s1: string;
begin
  s := '';
  s1 := ' @('+X.ToString+','+Y.ToString+')';
  if Sender = FLabel1 then s := 'Label1 - mouse down' + s1;
  if Sender = FButton1 then s := 'Button1 - mouse down' + s1;
  if Sender = FPanel1 then s := 'Panel1 - mouse down' + s1;
  if Sender = FCheckBox1 then s := 'CheckBox1 - mouse down' + s1;
  if Sender = FScrollBox1 then begin
    s1 := ' @('+(X+FScrollBox1.ViewOffset.x).ToString+','+(Y+FScrollBox1.ViewOffset.y).ToString+')';
    s := 'ScrollBox1 - mouse down'+s1;
  end;

  if s <> '' then begin
    FTextArea1.Text.Caption := FTextArea1.Text.Caption + #10 + s;
    FTextArea1.MakeLastLineVisible;
  end;
end;

procedure TScreenDemo.ProcessUIMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var s, s1: string;
begin
  s := '';
  s1 := ' @('+X.ToString+','+Y.ToString+')';
  if Sender = FLabel1 then s := 'Label1 - mouse up'+s1;
  if Sender = FButton1 then s := 'Button1 - mouse up'+s1;
  if Sender = FPanel1 then s := 'Panel1 - mouse up'+s1;
  if Sender = FCheckBox1 then s := 'CheckBox1 - mouse up'+s1;
  if Sender = FScrollBox1 then begin
    s1 := ' @('+(X+FScrollBox1.ViewOffset.x).ToString+','+(Y+FScrollBox1.ViewOffset.y).ToString+')';
    s := 'ScrollBox1 - mouse up'+s1;
  end;

  if s <> '' then begin
    FTextArea1.Text.Caption := FTextArea1.Text.Caption + #10 + s;
    FTextArea1.MakeLastLineVisible;
  end;
end;

procedure TScreenDemo.ProcessUIMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var s: string;
begin
  s := '';
  if Sender = FLabel1 then s := 'Label1 - mouse wheel';
  if Sender = FButton1 then s := 'Button1 - mouse wheel';
  if Sender = FPanel1 then s := 'Panel1 - mouse wheel';
  if Sender = FCheckBox1 then s := 'CheckBox1 - mouse wheel';

  if s <> '' then begin
    FTextArea1.Text.Caption := FTextArea1.Text.Caption + #10 + s;
    FTextArea1.MakeLastLineVisible;
  end;
end;

procedure TScreenDemo.ProcessUICheckboxChange(Sender: TSimpleSurfaceWithEffect);
begin
  if Sender = FCheckBox1 then FPanel1.ChildClippingEnabled := FCheckBox1.Checked;
  if Sender = FCheckBox2 then begin
    FPanel2.ChildClippingEnabled := FCheckBox2.Checked;
    FPanel3.ChildClippingEnabled := FCheckBox2.Checked;
    FPanel4.ChildClippingEnabled := FCheckBox2.Checked;
  end;
end;

procedure TScreenDemo.ProcessUIRadioChange(Sender: TSimpleSurfaceWithEffect);
var s: string;
begin
  if (Sender = FTheme1) or (Sender = FTheme2) or (Sender = FTheme3) then begin
    SetColorTheme;
    exit;
  end;

  s := '';
  if FRadio1.Checked then s := 'Radio1 checked';
  if FRadio2.Checked then s := 'Radio2 checked';
  if FRadio3.Checked then s := 'Radio3 checked';


  if s <> '' then begin
    FTextArea1.Text.Caption := FTextArea1.Text.Caption + #10 + s;
    FTextArea1.MakeLastLineVisible;
  end;
end;

procedure TScreenDemo.ProcessListBoxSelectionChange(Sender: TSimpleSurfaceWithEffect);
var i: Integer;
  s: string;
begin
  s := '';
  if FListBox1.SelectedCount = 0 then s := 'ListBox1: no selection'
  else begin
    s := #10'ListBox1 selection:';
    for i:=0 to FListBox1.Count-1 do
      if FListBox1.Selected[i] then s := s + #10'   '+FListBox1.Items[i];
  end;

  if s <> '' then begin
    FTextArea1.Text.Caption := FTextArea1.Text.Caption + #10 + s;
    FTextArea1.MakeLastLineVisible;
  end;
end;

procedure TScreenDemo.SetColorTheme;
const COLOR_POSITION: array[0..2] of single=(0, 0.5, 1);  // left, middle, right
var cLeft, cMiddle, cRight: TBGRAPixel;
  gradHeavy, gradSoft, gradButton: TGradientDescriptor;
  o: TSimpleSurfaceWithEffect;
  i: Integer;

begin
  gradHeavy.InitDefault;  // we have to do that because type of TGradientDescriptor is record.
  gradSoft.InitDefault;
  gradButton.InitDefault;
  cLeft := BGRA(30,30,30);
  cRight := cLeft;
  cMiddle := BGRA(220,220,220);

  if FTheme1.Checked then begin
    cLeft := BGRA(100,50,255,100);
    cMiddle := BGRA(255,0,255,200);
    cRight := BGRA(50,0,255,100);
    gradHeavy.CreateHorizontal([cLeft, cMiddle, cRight], COLOR_POSITION);
    gradSoft.CreateHorizontal([BGRA(0,0,255,10), BGRA(255,0,255,30), BGRA(0,0,255,10)], COLOR_POSITION);
    gradButton.CreateVertical([cLeft, cMiddle, cRight], COLOR_POSITION);
  end else
  if FTheme2.Checked then begin
    cLeft := BGRA(100,255,50,30);
    cMiddle := BGRA(100,255,0,180);
    cRight := BGRA(50,255,0,30);
    gradHeavy.CreateHorizontal([cLeft, cMiddle, cRight], COLOR_POSITION);
    gradSoft.CreateHorizontal([BGRA(100,255,50,10), BGRA(100,255,0,30), BGRA(50,255,0,10)], COLOR_POSITION);
    gradButton.CreateVertical([cLeft, cMiddle, cRight], COLOR_POSITION);
  end else
  if FTheme3.Checked then begin
    cLeft := BGRA(255,64,0,60);
    cMiddle := BGRA(255,128,32,220);
    cRight := BGRA(255,64,0,60);
    gradHeavy.CreateHorizontal([cLeft, cMiddle, cRight], COLOR_POSITION);
    gradSoft.CreateHorizontal([BGRA(255,64,32,10), BGRA(255,64,32,30), BGRA(255,128,32,10)], COLOR_POSITION);
    gradButton.CreateVertical([cLeft, cMiddle, cRight], COLOR_POSITION);
  end;

  // we scan all the surfaces in the layer where are the UI elements.
  for i:=0 to FScene.Layer[0].SurfaceCount-1 do begin
    o := FScene.Layer[0].Surface[i];
    if (o is TUIPanel) or (o is TUITextArea) or (o is TUIScrollBar) or (o is TUIScrollBox) then
      TUIClickableWithBodyShape(o).BackGradient.CopyFrom(gradSoft) // gradHeavy)
    else
    if (o is TUIListBox) then begin
      TUIListBox(o).ItemColor.GradientItem.CopyFrom(gradSoft);
      TUIListBox(o).ItemColor.GradientItemSelected.CopyFrom(gradHeavy);
    end else
    if (o is TUIProgressBar) then
      TUIProgressBar(o).BodyShape.Fill.Color := cLeft
    else
    if (o is TUICheck) then
      TUICheck(o).ColorChecked := cMiddle
    else
    if (o is TUIRadio) then
      TUIRadio(o).ColorChecked := cMiddle
    else
    if (o is TUIButton) then
      TUIButton(o).BackGradient.CopyFrom(gradButton);
  end;
end;

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  fd: TFontDescriptor;
  itemWidth: integer;
begin
  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;

  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> speed optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  // we define a font for the text
  fd.Create('Arial', FScene.ScaleDesignToScene(18), [], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, SIMPLELATIN_CHARSET + ASCII_SYMBOL_CHARSET); // use 2 predefined charsets

  FtexWarning := FAtlas.AddFromSVG(path+'DlgWarning.svg', FScene.ScaleDesignToScene(32), -1);
  FtexUnchecked := FAtlas.AddFromSVG(path+'CheckBoxUnchecked.svg', FScene.ScaleDesignToScene(32), -1);
  FtexChecked := FAtlas.AddFromSVG(path+'CheckBoxChecked.svg', FScene.ScaleDesignToScene(32), -1);

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;

  // label on the scene
  FLabel1 := FScene.Add_UILabel('Hello! My name is Label1 and I''m a TUILabel ! I''m only clipped by the bounds of the scene.', FtexFont);
  FLabel1.AnchorPosToParent(haLeft, haLeft, 0, vaTop, vaTop, FtexFont.Font.FontHeight);
  FLabel1.Tint.Value := BGRA(220,220,220);
  InitCallbackOn(FLabel1);

  // button on the scene
  FButton1 := FScene.Add_UIButton('Button1', FtexFont, FtexWarning);
  FButton1.BodyShape.SetShapeRoundRect(10,5,8,8,2);
  FButton1._Label.Tint.Value := BGRA(220,220,220);
  FButton1.AnchorPosToSurface(FLabel1, haCenter, haCenter, 0, vaTop, vaBottom, 0);
  InitCallbackOn(FButton1);

  itemWidth := FScene.Width div 3 - FScene.ScaleDesignToScene(10);

  // Panel1
  FPanel1 := FScene.Add_UIPanel;
  FPanel1.BodyShape.SetShapeRoundRect(itemWidth, FScene.ScaleDesignToScene(90), 10, 10, 2, []);
  FPanel1.AnchorHPosToParent(haLeft, haLeft, FScene.ScaleDesignToScene(10));
  FPanel1.AnchorVPosToSurface(FButton1, vaTop, vaBottom, FScene.ScaleDesignToScene(10));
  InitCallbackOn(FPanel1);
    // label2, child of FPanel1
    FLabel2 := TUILabel.Create(FScene, 'A TUILabel child of TUIPanel -> I''m clipped by my parent!', FtexFont);
    FPanel1.AddChild(FLabel2);
    FLabel2.CenterOnParent;
    FLabel2.Tint.Value := BGRA(255,128,220);
    FLabel2.Angle.AddConstant(10);
    // checkbox1, child of FPanel1
    FCheckBox1 := TUICheck.Create(FScene, 'Enabled Child clipping', FtexFont);
    FPanel1.AddChild(FCheckBox1);
    FCheckBox1.AnchorPosToParent(haCenter, haCenter, 0, vaTop, vaTop, FScene.ScaleDesignToScene(5));
    FCheckBox1._Label.Tint.Value := BGRA(230,230,200);
    FCheckBox1.CustomizeCheckBox(ctRectangle, cfCross);
    FCheckBox1.ColorChecked := BGRA(255,255,0);
    FCheckBox1.Checked := True;
    FCheckBox1.OnChange := @ProcessUICheckboxChange;
    InitCallbackOn(FCheckBox1);
  // label above panel1
  FLabel6 := FScene.Add_UILabel('TUIPanel', FtexFont);
  FLabel6.AnchorPosToSurface(FPanel1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel6.Tint.Value := BGRA(255,255,0);


  // Panel2
  FPanel2 := FScene.Add_UIPanel;
  FPanel2.BodyShape.SetShapeRoundRect(FScene.ScaleDesignToScene(300), FScene.ScaleDesignToScene(200), 10, 10, 2, []);
  FPanel2.AnchorPosToSurface(FPanel1, haRight, haRight, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  FPanel2.BodyShape.Border.Color := BGRA(255,255,255);
  FPanel2.BodyShape.Border.LinePosition := lpInside; // bpInside   bpMiddle
  FPanel2.BodyShape.Fill.Color := BGRA(81,79,66);
    // checkbox2, child of FPanel2
    FCheckBox2 := TUICheck.Create(FScene, 'Enabled Child clipping (nested)', FtexFont);
    FPanel2.AddChild(FCheckBox2);
    FCheckBox2.AnchorPosToParent(haLeft, haLeft, 0, vaTop, vaTop, FScene.ScaleDesignToScene(5));
    FCheckBox2._Label.Tint.Value := BGRA(230,230,200);
    FCheckBox2.CustomizeCheckBox(ctCircle, cfColor);
    FCheckBox2.ColorChecked := BGRA(255,255,0);
    FCheckBox2.Checked := True;
    FCheckBox2.OnChange := @ProcessUICheckboxChange;
    // panel3 child of panel2
    FPanel3 := TUIPanel.Create(FScene);
    FPanel3.BodyShape.SetShapeRoundRect(FScene.ScaleDesignToScene(200), FScene.ScaleDesignToScene(150), 5, 5, 5);
    FPanel3.BodyShape.Fill.Color := BGRA(255,0,0);
    FPanel3.BodyShape.Border.Color := BGRA(0,255,0);
    FPanel3.BodyShape.Border.LinePosition := lpInside;
    FPanel2.AddChild(FPanel3);
    FPanel3.SetCenterCoordinate(20, FPanel2.Height*0.5);
      // panel4, child of panel3
      FPanel4 := TUIPanel.Create(FScene);
      FPanel4.BodyShape.SetShapeEllipse(FScene.ScaleDesignToScene(240), FScene.ScaleDesignToScene(35), 3);
      FPanel4.BodyShape.Fill.Color := BGRA(0,0,255, 80);
      FPanel4.BodyShape.Border.Color := BGRA(255,128,64);
      FPanel3.AddChild(FPanel4);
      FPanel4.SetCenterCoordinate(FPanel3.Width*0.5, FPanel3.Height*0.5);
      FPanel4.Angle.AddConstant(-30);
        // panel5, child of panel4
        FPanel5 := TUIPanel.Create(FScene);
        FPanel5.BodyShape.SetShapeRectangle(FScene.ScaleDesignToScene(280), FScene.ScaleDesignToScene(10), 3);
        FPanel5.BodyShape.Fill.Color := BGRA(255,0,255);
        FPanel5.BodyShape.Border.Color := BGRA(0,255,255);
        FPanel4.AddChild(FPanel5);
        FPanel5.SetCenterCoordinate(FPanel4.Width*0.5, FPanel4.Height*0.5);
        FPanel5.Angle.AddConstant(30);

    // 3x radio button on panel2
    FRadio2 := TUIRadio.Create(FScene, 'TUIRadio 2', FtexFont);
    FPanel2.AddChild(FRadio2);
    FRadio2.AnchorPosToParent(haLeft, haCenter, 0, vaCenter, vaCenter, 0);
    FRadio2.CustomizeCheckBox(ctRectangle, cfCross);
    FRadio2._Label.Tint.Value := BGRA(220,220,220);
    FRadio2.OnChange := @ProcessUIRadioChange;
    FRadio1 := TUIRadio.Create(FScene, 'TUIRadio 1', FtexFont);
    FPanel2.AddChild(FRadio1);
    FRadio1.AnchorPosToSurface(FRadio2, haLeft, haLeft, 0, vaBottom, vaTop, -FRadio2.Height);
    FRadio1._Label.Tint.Value := BGRA(220,220,220);
    FRadio1.Checked := True;
    FRadio1.OnChange := @ProcessUIRadioChange;
    // radio3 is customized with textures
    FRadio3 := TUIRadio.Create(FScene, 'TUIRadio 3', FtexFont);
    FPanel2.AddChild(FRadio3);
    FRadio3.AnchorPosToSurface(FRadio2, haLeft, haLeft, 0, vaTop, vaBottom, FRadio2.Height);
    FRadio3.CustomizeCheckBox(FtexUnchecked, FtexChecked, False);
    FRadio3._Label.Tint.Value := BGRA(220,220,220);
    FRadio3.OnChange := @ProcessUIRadioChange;


  // 2x progress bar
  FProgressBar1 := FScene.Add_UIProgressBar(uioHorizontal);
  FProgressBar1.AnchorHPosToSurface(FPanel1, haLeft, haLeft, 0);
  FProgressBar1.AnchorVPosToSurface(FPanel2, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  FProgressBar1.BodyShape.SetShapeRoundRect(itemWidth, FScene.ScaleDesignToScene(30),10,10,2);
  FProgressBar1.BodyShape.Border.Color := BGRA(200,200,200);
  FProgressBar1.BodyShape.Fill.Color := BGRA(20,20,20);
  FProgressBar1.Gradient.CreateHorizontal([BGRA(0,255,0), BGRA(255,255,0), BGRA(255,0,0)], [0,0.7,1]);
  // label above progressbar1
  FLabel6 := FScene.Add_UILabel('TUIProgressBar', FtexFont);
  FLabel6.AnchorPosToSurface(FProgressBar1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel6.Tint.Value := BGRA(255,255,0);

  FProgressBar2 := FScene.Add_UIProgressBar(uioHorizontal);
  FProgressBar2.AnchorPosToSurface(FProgressBar1, haLeft, haLeft, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(5));
  FProgressBar2.BodyShape.SetShapeRoundRect(itemWidth, FScene.ScaleDesignToScene(30),10,10,2);
  FProgressBar2.BodyShape.Border.Color := BGRA(200,200,200);
  FProgressBar2.BodyShape.Fill.Color := BGRA(20,20,20);
  FProgressBar2.Gradient.Clear;
  FProgressBar2.Gradient.BeginUpdate;
  FProgressBar2.Gradient.AddRow(0.0, [BGRA(128,0,0), BGRA(128,0,0), BGRA(128,0,0)], [0,0.5,1]);
  FProgressBar2.Gradient.AddRow(0.4, [BGRA(255,0,0), BGRA(255,0,0), BGRA(255,0,0)], [0,0.5,1]);
  FProgressBar2.Gradient.AddRow(0.5, [BGRA(255,255,255), BGRA(255,255,255), BGRA(255,255,255)], [0,0.5,1]);
  FProgressBar2.Gradient.AddRow(0.6, [BGRA(0,255,255), BGRA(0,255,255), BGRA(0,255,255)], [0,0.5,1]);
  FProgressBar2.Gradient.AddRow(1.0, [BGRA(0,128,128), BGRA(0,128,128), BGRA(0,128,128)], [0,0.5,1]);
  FProgressBar2.Gradient.EndUpdate;

  // scroll bar
  FScrollBar1 := FScene.Add_UIScrollBar(uioHorizontal);
  FScrollBar1.AnchorPosToSurface(FProgressBar2, haLeft, haLeft, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  FScrollBar1.BodyShape.SetShapeRoundRect(itemWidth, FScene.ScaleDesignToScene(15), 30, 30, 1.6);
  FScrollBar1.SetParams(250, 0, 1500, 250);
  FScrollBar1.OnChange := @ProcessUIClick;
  // label above the scroll bar
  FLabel3 := FScene.Add_UILabel('TUIScrollBar', FtexFont);
  FLabel3.AnchorPosToSurface(FScrollBar1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel3.Tint.Value := BGRA(255,255,0);

  // list box
  FListBox1 := FScene.Add_UIListBox(FtexFont);
  FListBox1.AnchorPosToSurface(FPanel1, haLeft, haRight, FScene.ScaleDesignToScene(10), vaTop, vaTop, 0);
  FListBox1.BodyShape.SetShapeRoundRect(itemWidth, 250, 10, 10, 2);
  FListBox1.ItemColor.ColorText := BGRA(220,220,220);
  FListBox1.ItemColor.ColorSelectedText := BGRA(255,255,255);
  FListBox1.ItemHeight := 20;

  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.FirstSelectedIndex := 3;
  FListBox1.MultiSelect := True;
  FListBox1.OnSelectionChange := @ProcessListBoxSelectionChange;
  // label above listbox1
  FLabel4 := FScene.Add_UILabel('TUIListBox: shift/ctrl for multiselect', FtexFont);
  FLabel4.AnchorPosToSurface(FListBox1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel4.Tint.Value := BGRA(255,255,0);

  // button clear
  FButtonClear := FScene.Add_UIButton('Clear', FtexFont, NIL);
  FButtonClear.BodyShape.SetShapeRoundRect(5, 5, 8, 8, 2, []);
  FButtonClear.AnchorPosToParent(haRight, haRight, 0, vaTop, vaTop, FtexFont.Font.FontHeight);
  FButtonClear._Label.Tint.Value := BGRA(220,220,220);
  FButtonClear.OnClick := @ProcessUIClick;
  // text area
  FTextArea1 := FScene.Add_UITextArea;
  FTextArea1.BodyShape.SetShapeRoundRect(itemWidth, FScene.ScaleDesignToScene(500), 8, 8, 2);
  FTextArea1.RightX := FScene.Width;
  FTextArea1.AnchorPosToSurface(FButtonClear, haRight, haRight, 0, vaTop, vaBottom, 0);
  FTextArea1.Text.TexturedFont := FtexFont;
  FTextArea1.Text.Align := taTopLeft;
  // label above text area
  FLabel5 := FScene.Add_UILabel('TUITextArea', FtexFont);
  FLabel5.AnchorPosToSurface(FTextArea1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel5.Tint.Value := BGRA(255,255,0);

  // label above theme selection
  FLabel11 := FScene.Add_UILabel('Color theme', FtexFont);
  FLabel11.AnchorPosToSurface(FTextArea1, haLeft, haLeft, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  FLabel11.Tint.Value := BGRA(255,255,0);
  // 3x radio button for theme selection
  FTheme1 := FScene.Add_UIRadio('Purple', FtexFont);
  FTheme1.AnchorPosToSurface(FLabel11, haLeft, haLeft, 0, vaTop, vaBottom, 0);
  FTheme1._Label.Tint.Value := BGRA(255,20,255);
  FTheme1.OnChange := @ProcessUIRadioChange;
  FTheme2 := FScene.Add_UIRadio('Green', FtexFont);
  FTheme2.AnchorPosToSurface(FTheme1, haLeft, haRight, FScene.ScaleDesignToScene(20), vaCenter, vaCenter, 0);
  FTheme2._Label.Tint.Value := BGRA(100,255,20);
  FTheme2.OnChange := @ProcessUIRadioChange;
  FTheme3 := FScene.Add_UIRadio('Orange', FtexFont);
  FTheme3.AnchorPosToSurface(FTheme2, haLeft, haRight, FScene.ScaleDesignToScene(20), vaCenter, vaCenter, 0);
  FTheme3._Label.Tint.Value := BGRA(255,128,64);
  FTheme3.OnChange := @ProcessUIRadioChange;


  FScrollBox1 := FScene.Add_UIScrollBox(True, True);
  FScrollBox1.BodyShape.SetShapeRoundRect(itemWidth, FScene.ScaleDesignToScene(150), 8, 8, 2);
  FScrollBox1.AnchorPosToSurface(FListBox1, haLeft, haLeft, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  InitCallbackOn(FScrollBox1);
  // label above scrollbox1
  FLabel9 := FScene.Add_UILabel('TUIScrollBox', FtexFont);
  FLabel9.AnchorPosToSurface(FScrollBox1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel9.Tint.Value := BGRA(255,255,0);
    FLabel7 := TUILabel.Create(FScene, 'Use mouse wheel on the ScrollBars!', FtexFont);
    FLabel7.Tint.Value := BGRA(220,220,220);
    FScrollBox1.AddChild(FLabel7);
    FLabel10 := TUILabel.Create(FScene, 'Click on ScrollBox to see the coordinates', FtexFont);
    FLabel10.Tint.Value := BGRA(220,220,220);
    FScrollBox1.AddChild(FLabel10);
    FLabel10.SetCoordinate(0, FLabel7.BottomY);

    FLabel8 := TUILabel.Create(FScene, 'You found me!', FtexFont);
    FLabel8.Tint.Value := BGRA(220,220,220);
    FScrollBox1.AddChild(FLabel8);
    FLabel8.SetCoordinate(FScrollBox1.Width*2, FScrollBox1.Height*2);

end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);

  // update periodically the progress bars
  FProgressBar1.Percent := FScene.Mouse.Position.X / FScene.Width;
  FProgressBar2.Percent := FScene.Mouse.Position.Y / FScene.Height;
end;


end.

