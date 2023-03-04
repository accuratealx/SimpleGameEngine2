unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeFont, sgeFontGlyph, sgeSprite,

  GDIPAPI, GDIPOBJ,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, StdCtrls, Spin, Windows, Types;

type
  TMainForm = class(TForm)
    edGlyphCaption: TEdit;
    edGlyphX2: TSpinEdit;
    edGlyphY1: TSpinEdit;
    edGlyphBaseLine: TSpinEdit;
    edGlyphY2: TSpinEdit;
    lblGlyphCaption: TLabel;
    lblGlyphY2: TLabel;
    lblGlyphX1: TLabel;
    lblGlyphY1: TLabel;
    lblGlyphBaseLine: TLabel;
    lbGlyphList: TListBox;
    lblGlyphX2: TLabel;
    MainMenu: TMainMenu;
    miShowSymbolDescent: TMenuItem;
    miShowAllGlyphRect: TMenuItem;
    miView: TMenuItem;
    miNew: TMenuItem;
    miExport: TMenuItem;
    miImport: TMenuItem;
    miGenerate: TMenuItem;
    miSprite: TMenuItem;
    miFile: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miSeparator1: TMenuItem;
    miClose: TMenuItem;
    pnlPaint: TPanel;
    pnlGlyphEditor: TPanel;
    pnlWorkArea: TPanel;
    pnlGlyph: TPanel;
    PanelSplitter: TSplitter;
    edGlyphX1: TSpinEdit;
    WorkAreaScrollBox: TScrollBox;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    procedure edGlyphBaseLineChange(Sender: TObject);
    procedure edGlyphCaptionChange(Sender: TObject);
    procedure edGlyphX2Change(Sender: TObject);
    procedure edGlyphY2Change(Sender: TObject);
    procedure edGlyphY1Change(Sender: TObject);
    procedure edGlyphX1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbGlyphListClick(Sender: TObject);
    procedure miGenerateClick(Sender: TObject);
    procedure miShowAllGlyphRectClick(Sender: TObject);
    procedure miShowSymbolDescentClick(Sender: TObject);
    procedure pnlPaintMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure pnlPaintMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure pnlPaintPaint(Sender: TObject);
  private
    FFont: TsgeFont;
    FGlyphCanvas: Graphics.TBitmap;
    FShowAllGlyphRect: Boolean;
    FShowGlyphDescent: Boolean;

    function  GetGlyphEditIndex: Integer;
    procedure SpriteToGlyphCanvas(Sprite: TsgeSprite);

    procedure SetEnableEditorHandlers(AEnable: Boolean);
    procedure ReloadGlyphListBoxItemByIndex(Index: Integer);
    procedure ReloadGlyphListBox;
    procedure PaintGlyphBaseLine(Index: Integer; AColor: TColor; ACanvas: TCanvas);
    procedure PaintGlyphRect(Glyph: TsgeFontGlyph; AColor: TColor; ACanvas: TCanvas);
    procedure PaintGlyphRects(AColor: TColor; ACanvas: TCanvas);

    procedure RepaintPaintBox;

    procedure SetGlyphEditorByIndex(Index: Integer = -1);

    procedure GenerateFont(FontName: String; FontSize: Word; FontAttrib: TsgeFontAttributes);
    procedure FillFontGlyphInfo(AFont: TGPFont);
    procedure CopyGpBitmapToBitmap(GpBmp: TGPBitmap; Bmp: Graphics.TBitmap);
    function  FontAttribToGPFontAttrib(Attrib: TsgeFontAttributes): Integer;
    function  MultiByteToWideChar(Str: String): UnicodeString;
    function  GetFontSprite(AFont: TGPFont; SymbolSize: Integer): TGPBitmap;

  private
    FZoom: Single;
    FZoomMin: Single;
    FZoomMax: Single;
    FZoomFactor: Single;
    procedure ZoomIn;
    procedure ZoomOut;
  public


  end;


var
  MainForm: TMainForm;


implementation

{$R *.lfm}


procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
  end;
end;


procedure TMainForm.lbGlyphListClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetGlyphEditIndex;
  SetGlyphEditorByIndex(Index);

  if (Index >= 0) and (Index <= $FF) then
  begin
    PaintGlyphRect(FFont.GlyphList[Index], clRed, pnlPaint.Canvas);
    pnlPaint.Invalidate;
  end;
end;


procedure TMainForm.miGenerateClick(Sender: TObject);
begin
  FZoom := 1;
  GenerateFont('Lucida Console', 22, []); //???
  //GenerateFont('Times new roman', 22, []);
  //GenerateFont('Segoe UI', 22, [faBold]);

  SetEnableEditorHandlers(False);
  ReloadGlyphListBox;
  SetGlyphEditorByIndex;
  SetEnableEditorHandlers(True);
  RepaintPaintBox;
end;


procedure TMainForm.miShowAllGlyphRectClick(Sender: TObject);
begin
  FShowAllGlyphRect := not FShowAllGlyphRect;
  RepaintPaintBox;
end;


procedure TMainForm.miShowSymbolDescentClick(Sender: TObject);
begin
  FShowGlyphDescent := not FShowGlyphDescent;
  RepaintPaintBox;
end;


procedure TMainForm.pnlPaintMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ZoomOut;
  RepaintPaintBox;
end;


procedure TMainForm.pnlPaintMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ZoomIn;
  RepaintPaintBox;
end;


procedure TMainForm.pnlPaintPaint(Sender: TObject);
begin
  RepaintPaintBox;
end;


procedure TMainForm.SetEnableEditorHandlers(AEnable: Boolean);
begin
  if AEnable = false then
  begin
    edGlyphCaption.OnChange := nil;
    edGlyphX1.OnChange := nil;
    edGlyphY1.OnChange := nil;
    edGlyphX2.OnChange := nil;
    edGlyphY2.OnChange := nil;
    edGlyphBaseLine.OnChange := nil;
  end
  else
  begin
    edGlyphCaption.OnChange := @edGlyphCaptionChange;
    edGlyphX1.OnChange := @edGlyphX1Change;
    edGlyphY1.OnChange := @edGlyphY1Change;
    edGlyphX2.OnChange := @edGlyphX2Change;
    edGlyphY2.OnChange := @edGlyphY2Change;
    edGlyphBaseLine.OnChange := @edGlyphBaseLineChange;
  end;
end;


procedure TMainForm.ReloadGlyphListBoxItemByIndex(Index: Integer);
begin
  if (Index < 0) or (Index > $FF) then
    Exit;

  lbGlyphList.Items.Strings[Index] := Format('%3d. %s', [Index, FFont.GlyphList[Index].Caption]);
end;


procedure TMainForm.ReloadGlyphListBox;
var
  i: Integer;
begin
  lbGlyphList.Items.BeginUpdate;

  lbGlyphList.Items.Clear;

  for i := 0 to $FF do
  begin
    lbGlyphList.Items.Add('');
    ReloadGlyphListBoxItemByIndex(i);
  end;

  lbGlyphList.Items.EndUpdate;
end;


procedure TMainForm.PaintGlyphBaseLine(Index: Integer; AColor: TColor; ACanvas: TCanvas);
var
  Y: Integer;
begin
  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Style := psSolid;
  Y := Round((Index * FFont.Height - FFont.GlyphDescent) * FZoom);
  ACanvas.Line(0, Y, ACanvas.Width, Y);
end;


function TMainForm.GetGlyphEditIndex: Integer;
begin
  Result := lbGlyphList.ItemIndex;
end;


procedure TMainForm.PaintGlyphRect(Glyph: TsgeFontGlyph; AColor: TColor; ACanvas: TCanvas);
var
  X1, X2, Y: Integer;
begin
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Pen.Color := AColor;

    //Рамка
    Rectangle(
      Round(Glyph.SpriteRect.X1 * FZoom),
      Round(Glyph.SpriteRect.Y1 * FZoom),
      Round(Glyph.SpriteRect.X2 * FZoom),
      Round(Glyph.SpriteRect.Y2 * FZoom));

    //Базовая линия
    X1 := Round(Glyph.SpriteRect.X1 * FZoom);
    X2 := Round(Glyph.SpriteRect.X2 * FZoom);
    Y := Round((Glyph.SpriteRect.Y2 - Glyph.BaseLine) * FZoom);
    Line(X1, Y, X2, Y);
  end;
end;


procedure TMainForm.PaintGlyphRects(AColor: TColor; ACanvas: TCanvas);
var
  i: Integer;
begin
  for i := 0 to $FF do
    PaintGlyphRect(FFont.GlyphList[i], AColor, ACanvas);
end;


procedure TMainForm.SpriteToGlyphCanvas(Sprite: TsgeSprite);
var
  SpriteLine, BitmapLine: Pointer;
  i, BytesPerLine: Integer;
begin
  //Поправить размеры холста
  FGlyphCanvas.Width := Sprite.Width;
  FGlyphCanvas.Height := Sprite.Height;

  //Скопировать данные на холст
  BytesPerLine := Sprite.Width * 4;
  for i := 0 to Sprite.Height - 1 do
  begin
    SpriteLine := Sprite.Data + i * BytesPerLine;
    BitmapLine := FGlyphCanvas.ScanLine[Sprite.Height - 1 - i];
    Move(SpriteLine^, BitmapLine^, BytesPerLine);
  end;
end;


procedure TMainForm.RepaintPaintBox;
var
  Index: Integer;
begin
  pnlPaint.Width := Round(FGlyphCanvas.Width * FZoom);
  pnlPaint.Height := Round(FGlyphCanvas.Height * FZoom);

  //Вывод спрайта с символами
  pnlPaint.Canvas.StretchDraw(TRect(sgeGetIntRect(0, 0, pnlPaint.Width, pnlPaint.Height)), FGlyphCanvas);

  //Вывод рамок всех глифов
  if FShowAllGlyphRect then
  begin
    for Index := 0 to $FF do
      PaintGlyphRect(FFont.GlyphList[Index], RGB($7F, $0, $7F), pnlPaint.Canvas);
  end;

  //Вывод базовой линии шрифта
  if FShowGlyphDescent then
  begin
    for Index := 0 to 15 do
      PaintGlyphBaseLine(Index, RGB($7F, $0, $7F), pnlPaint.Canvas);
  end;

  //Вывод рамки активного глифа
  Index := GetGlyphEditIndex;
  if (Index >= 0) and (Index <= $FF) then
    PaintGlyphRect(FFont.GlyphList[Index], clYellow, pnlPaint.Canvas);
end;


procedure TMainForm.SetGlyphEditorByIndex(Index: Integer);
begin
  pnlGlyphEditor.Enabled := (Index >= 0) and (Index <= $FF);

  if (Index < 0) or (Index > $FF) then
  begin
    edGlyphCaption.Text := '';
    edGlyphX1.Value := 0;
    edGlyphY1.Value := 0;
    edGlyphX2.Value := 0;
    edGlyphY2.Value := 0;
    edGlyphBaseLine.Value := 0;
  end
  else
  begin
    edGlyphCaption.Text := FFont.GlyphList[Index].Caption;
    edGlyphX1.Value := FFont.GlyphList[Index].SpriteRect.X1;
    edGlyphY1.Value := FFont.GlyphList[Index].SpriteRect.Y1;
    edGlyphX2.Value := FFont.GlyphList[Index].SpriteRect.X2;
    edGlyphY2.Value := FFont.GlyphList[Index].SpriteRect.Y2;
    edGlyphBaseLine.Value := FFont.GlyphList[Index].BaseLine;
  end;
end;


function TMainForm.MultiByteToWideChar(Str: String): UnicodeString;
var
  Len: Integer;
begin
  Result := '';

  //Определить длину возвращаемого буфера
  Len := Windows.MultiByteToWideChar(CP_ACP, 0, PChar(Str), Length(Str), nil, 0);

  if Len <= 0 then
    Exit;

  //Выделить память под ответ
  SetLength(Result, Len);

  //Получить преобразованную строку
  Windows.MultiByteToWideChar(CP_ACP, 0, PChar(Str), Length(Str), PWideChar(Result), Len);
end;


procedure TMainForm.GenerateFont(FontName: String; FontSize: Word; FontAttrib: TsgeFontAttributes);
var
  FontFamily: TGPFontFamily;
  AFont: TGPFont;
  SymbolSize, Descent: Integer;
  Bmp: TGPBitmap;
begin
  //Создать шрифт с нужными параметрами
  FontFamily := TGPFontFamily.Create(WideString(FontName));
  AFont := TGPFont.Create(FontFamily, FontSize, FontAttribToGPFontAttrib(FontAttrib));

  //Записать высоты нисхождения символов
  Descent := Round(FontFamily.GetCellDescent(FontAttribToGPFontAttrib(FontAttrib)) / 75);
  FFont.GlyphDescent := Descent;

  //Размер одной клетки
  SymbolSize := Round(AFont.GetHeight(96));

  //Сгенерировать спрайт с глифами
  Bmp := GetFontSprite(AFont, SymbolSize);

  //Скопировать спрайт на рабочий битмап
  CopyGpBitmapToBitmap(Bmp, FGlyphCanvas);

  //Заполнить шрифт информацией о глифах
  FillFontGlyphInfo(AFont);

  //Почистить память
  Bmp.Free;
  AFont.Free;
  FontFamily.Free;
end;


procedure TMainForm.FillFontGlyphInfo(AFont: TGPFont);
var
  X, Y, i, SymbolSize: Integer;
  Glyph: TsgeFontGlyph;
begin
  //Размер глифа
  SymbolSize := Round(AFont.GetHeight(96));

  //Заполнить параметры
  FFont.Height := SymbolSize;
  FFont.LineSpace := 0;
  FFont.GlyphSpace := 0;

  //Начальные координаты глифа
  X := -SymbolSize;
  Y := -SymbolSize;

  //Заполнить глифы
  for i := 0 to $FF do
  begin
    Glyph := FFont.GlyphList[i];

    Glyph.Caption := MultiByteToWideChar(Chr(i));
    Glyph.BaseLine := 0;

    //Заполнить текстурные координаты
    X := X + SymbolSize;
    if i mod 16 = 0 then
    begin
      X := 0;
      Y := Y + SymbolSize;
    end;

    //Прямоугольник вывода глифа
    Glyph.SpriteRect := sgeGetFloatRect(X, Y, X + SymbolSize, Y + SymbolSize);
  end;
end;


procedure TMainForm.CopyGpBitmapToBitmap(GpBmp: TGPBitmap; Bmp: Graphics.TBitmap);
var
  Graphic: TGPGraphics;
begin
  //Поправить размеры битмапа
  Bmp.Width := GpBmp.GetWidth;
  Bmp.Height := GpBmp.GetHeight;

  //Вспомогательный класс для рисования
  Graphic := TGPGraphics.Create(Bmp.Canvas.Handle);

  //Вывод
  Graphic.DrawImage(GpBmp, 0, 0);

  //Почистить память
  Graphic.Free;
end;


function TMainForm.FontAttribToGPFontAttrib(Attrib: TsgeFontAttributes): Integer;
begin
  Result := FontStyleRegular;

  if faBold in Attrib then
    Result := Result + FontStyleBold;

  if faItalic in Attrib then
    Result := Result + FontStyleItalic;

  if faUnderline in Attrib then
    Result := Result or FontStyleUnderline;

  if faStrikeOut in Attrib then
    Result := Result or FontStyleStrikeout;
end;


function TMainForm.GetFontSprite(AFont: TGPFont; SymbolSize: Integer): TGPBitmap;
var
  FontBrush, ContourBrush, BGBrush: TGPSolidBrush;
  Graphic: TGPGraphics;
  SpriteSize, X, Y, i: Integer;
  C: UnicodeString;

  procedure DrawSymbol(Char: UnicodeString; X, Y: Integer; Brush: TGPBrush);
  var
    GlyphRect: TGPRectF;
  begin
    GlyphRect.X := X;
    GlyphRect.Y := Y;
    //GlyphRect.Width := SymbolSize2;
    //GlyphRect.Height := SymbolSize2;
    GlyphRect.Width := 0;
    GlyphRect.Height := 0;

    Graphic.DrawString(Char, Length(Char), AFont, GlyphRect, nil, Brush);
  end;

begin
  //Размер спрайта
  SpriteSize := SymbolSize * 16;

  //Создать битмап
  Result := TGPBitmap.Create(SpriteSize, SpriteSize, PixelFormat32bppPARGB);

  //Подготовить кисти для заливки
  FontBrush := TGPSolidBrush.Create(MakeColor(255, 255, 255));
  ContourBrush := TGPSolidBrush.Create(MakeColor(0, 0, 0));
  BGBrush := TGPSolidBrush.Create(MakeColor(127, 127, 127));

  //Вспомогательный класс рисования
  Graphic := TGPGraphics.Create(Result);
  Graphic.SetTextRenderingHint(TextRenderingHintSingleBitPerPixelGridFit);

  //Стереть фон
  Graphic.FillRectangle(BGBrush, MakeRect(0, 0, SpriteSize, SpriteSize));

  //Вывести глифы
  X := -SymbolSize;
  Y := -SymbolSize;

  for i := 0 to $FF do
  begin
    //Поправить координаты
    X := X + SymbolSize;
    if i mod 16 = 0 then
    begin
      X := 0;
      Y := Y + SymbolSize;
    end;

    //Преобразование кодировки
    C := MultiByteToWideChar(Chr(i));

    //Контур
    DrawSymbol(C, X - 1, Y - 1, ContourBrush);
    DrawSymbol(C, X + 1, Y - 1, ContourBrush);
    DrawSymbol(C, X - 1, Y + 1, ContourBrush);
    DrawSymbol(C, X + 1, Y + 1, ContourBrush);
    DrawSymbol(C, X - 1, Y    , ContourBrush);
    DrawSymbol(C, X + 1, Y    , ContourBrush);
    DrawSymbol(C, X, Y + 1    , ContourBrush);
    DrawSymbol(C, X, Y - 1    , ContourBrush);

    //Символ
    DrawSymbol(C, X, Y, FontBrush);
  end;

  //Почистить память
  Graphic.Free;
  FontBrush.Free;
  ContourBrush.Free;
  BGBrush.Free;
end;


procedure TMainForm.ZoomIn;
begin
  FZoom := FZoom * FZoomFactor;
  if FZoom > FZoomMax then
     FZoom := FZoomMax;
end;


procedure TMainForm.ZoomOut;
begin
  FZoom := FZoom / FZoomFactor;
  if FZoom < FZoomMin then
     FZoom := FZoomMin;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  FShowAllGlyphRect := False;
  FZoomMin := 0.01;
  FZoomMax := 10;
  FZoom := 1;
  FZoomFactor := 1.2;

  FFont := TsgeFont.Create('');
  FGlyphCanvas := Graphics.TBitmap.Create;
  FGlyphCanvas.PixelFormat := pf32bit;
  SetBkMode(FGlyphCanvas.Canvas.Handle, TRANSPARENT);

  ReloadGlyphListBox;
  SetGlyphEditorByIndex;


  //Удалить
  FFont.Sprite.Width := 200;
  FFont.Sprite.Height := 200;
  FFont.Sprite.FillChessBoard(25);

  SpriteToGlyphCanvas(FFont.Sprite);
end;


procedure TMainForm.edGlyphCaptionChange(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetGlyphEditIndex;

  FFont.GlyphList[Idx].Caption := edGlyphCaption.Text;
  ReloadGlyphListBoxItemByIndex(Idx);
end;


procedure TMainForm.edGlyphX2Change(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetGlyphEditIndex;

  FFont.GlyphList[Idx].X2 := edGlyphX2.Value;
  ReloadGlyphListBoxItemByIndex(Idx);
  RepaintPaintBox;
end;


procedure TMainForm.edGlyphY2Change(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetGlyphEditIndex;

  FFont.GlyphList[Idx].Y2 := edGlyphY2.Value;
  ReloadGlyphListBoxItemByIndex(Idx);
  RepaintPaintBox;
end;


procedure TMainForm.edGlyphBaseLineChange(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetGlyphEditIndex;

  FFont.GlyphList[Idx].BaseLine := edGlyphBaseLine.Value;
  ReloadGlyphListBoxItemByIndex(Idx);
  RepaintPaintBox;
end;


procedure TMainForm.edGlyphY1Change(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetGlyphEditIndex;

  FFont.GlyphList[Idx].Y1 := edGlyphY1.Value;
  ReloadGlyphListBoxItemByIndex(Idx);
  RepaintPaintBox;
end;


procedure TMainForm.edGlyphX1Change(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetGlyphEditIndex;

  FFont.GlyphList[Idx].X1 := edGlyphX1.Value;
  ReloadGlyphListBoxItemByIndex(Idx);
  RepaintPaintBox;
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGlyphCanvas);
  FFont.Free;
end;



end.


