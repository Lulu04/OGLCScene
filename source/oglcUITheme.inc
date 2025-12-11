{$ifdef oglcINTERFACE}
type

TUIFillDescriptor = record
  Visible: boolean;
  BlendMode: byte;
  Color,
  CenterColor: TBGRAPixel;
end;

TUIBorderDescriptor = record
  Visible: boolean;
  BlendMode: byte;
  LinePosition: TOGLCLinePosition;
  Color: TBGRAPixel;
  Width: single
end;

TUIGradientDescriptor = record
  Data: string;
end;

TThemeLabelDescriptor = record
  Name: string;
  FontColor: TBGRAPixel;
end;

TUIPanelTheme = record
  Fill: TUIFillDescriptor;
  Border: TUIBorderDescriptor;
  BackGradient: TUIGradientDescriptor;
end;

TUIButtonTheme = record
  ButtonFill: TUIFillDescriptor;
  ButtonBorder: TUIBorderDescriptor;
  ButtonBackGradient: TUIGradientDescriptor;
  LabelColor: TBGRAPixel;
end;

TUICheckBoxTheme = record
  CheckShape: string;
  CheckFill: string;
end;

TUIThemeTemplate = record
  Name: string;
  PanelTheme: string;
  ButtonTheme: string;
  CheckBoxTheme: string;
  RadioButtonTheme: string;
  ScrollBartheme: string;
  ProgressBarTheme: string;
  ScrollBoxTheme: string;
  ListBoxTheme: string;
  TextAreaTheme: string;
end;
PUIThemeTemplate = ^TUIThemeTemplate;

TUIThemeManager = class(specialize TVector<TUIThemeTemplate>)
  //function AddDefault: PUIThemeTemplate;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}



{$endif oglcIMPLEMENTATION}

