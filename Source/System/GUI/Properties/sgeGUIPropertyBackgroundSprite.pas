{
Пакет             Simple Game Engine 2
Файл              sgeGUIBackgroundSprite.pas
Версия            1.0
Создан            23.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Спрайт
}
{$Include Defines.inc}

unit sgeGUIPropertyBackgroundSprite;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicSprite,
  sgeGUIProperty, sgeGUIPropertyFloatRect, sgeGUIPropertySegmentOffset;


type
  //Способ масштабирования
  TsgeGUIPropertySpriteScaleMode = (pssmNormal, pssmStretch, pssmFitIn, pssmFitOut, pssmUser);


  //Горизонтальное выравнивание
  TsgeGUIPropertyHorizontalAlign = (phaLeft, phaMiddle, phaRight, phaUser);


  //Вертикальное выравнивание
  TsgeGUIPropertyVerticalAlign = (pvaTop, pvaMiddle, pvaBottom, pvaUser);


  //Режим вывода спрайта (Целый, Плитка, Часть)
  TsgeGUISpriteDrawMode = (sdmFull, sdmTile, sdmRect);


  //Метод вывода спрайта (Целиком, По частям)
  TsgeGUISpriteDrawMethod = (sdmNormal, sdmSegment);


  TsgeGUIPropertyBackgroundSprite = class(TsgeGUIProperty)
  private
    FSprite: TsgeGraphicSprite;                                     //Ссылка на спрайт

    FScaleMode: TsgeGUIPropertySpriteScaleMode;                     //Режим масштабирования спрайта
    FScaleUserRect: TsgeGUIPropertyFloatRect;                       //Координаты пользовательского масштаба
    FHorizontalAligh: TsgeGUIPropertyHorizontalAlign;               //Горизонтальное выравнивание
    FHorizontalOffset: Integer;                                     //Горизонтальное смещение
    FVerticalAligh: TsgeGUIPropertyVerticalAlign;                   //Вертикальное выравнивание
    FVerticalOffset: Integer;                                       //Вертикальное смещение
    FDrawMode: TsgeGUISpriteDrawMode;                               //Режим вывода
    FDrawTileX: Word;                                               //Номер плитки X
    FDrawTileY: Word;                                               //Номер плитки Y
    FDrawRect: TsgeGUIPropertyFloatRect;                            //Прямоугольник вывода части спрайта
    FDrawMethod: TsgeGUISpriteDrawMethod;                           //Метод вывода
    FSegmentOffset: TsgeGUIPropertySegmentOffset;                   //Смещение для сегментного вывода

    procedure SetSprite(ASprite: TsgeGraphicSprite);
    procedure SetScaleMode(AScaleMode: TsgeGUIPropertySpriteScaleMode);

    procedure SetHorizontalAlign(AAlign: TsgeGUIPropertyHorizontalAlign);
    procedure SetVerticalAlign(AAlign: TsgeGUIPropertyVerticalAlign);
    procedure SetHorizontalOffset(AOffset: Integer);
    procedure SetVerticalOffset(AOffset: Integer);
    procedure SetDrawMode(AMode: TsgeGUISpriteDrawMode);
    procedure SetDrawTileX(AX: Word);
    procedure SetDrawTileY(AY: Word);
    procedure SetDrawMethod(AMethod: TsgeGUISpriteDrawMethod);
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    procedure Draw;

    property Sprite: TsgeGraphicSprite read FSprite write SetSprite;
    property ScaleMode: TsgeGUIPropertySpriteScaleMode read FScaleMode write SetScaleMode;
    property ScaleUserRect: TsgeGUIPropertyFloatRect read FScaleUserRect;
    property HorizontalAligh: TsgeGUIPropertyHorizontalAlign read FHorizontalAligh write SetHorizontalAlign;
    property VerticalAligh: TsgeGUIPropertyVerticalAlign read FVerticalAligh write SetVerticalAlign;
    property HorizontalOffset: Integer read FHorizontalOffset write SetHorizontalOffset;
    property VerticalOffset: Integer read FVerticalOffset write SetVerticalOffset;
    property DrawMode: TsgeGUISpriteDrawMode read FDrawMode write SetDrawMode;
    property DrawTileX: Word read FDrawTileX write SetDrawTileX;
    property DrawTileY: Word read FDrawTileY write SetDrawTileY;
    property DrawRect: TsgeGUIPropertyFloatRect read FDrawRect;
    property DrawMethod: TsgeGUISpriteDrawMethod read FDrawMethod write SetDrawMethod;
    property SegmentOffset: TsgeGUIPropertySegmentOffset read FSegmentOffset;
  end;


implementation

uses
  sgeTypes, sgeGraphic, sgeGraphicColor, sgeGraphicUtils, sgeVars,
  sgeGUIElement;

type
  TsgeGUIElementHack = class(TsgeGUIElement);


procedure TsgeGUIPropertyBackgroundSprite.SetSprite(ASprite: TsgeGraphicSprite);
begin
  if FSprite = ASprite then Exit;

  FSprite := ASprite;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundSprite.SetScaleMode(AScaleMode: TsgeGUIPropertySpriteScaleMode);
begin
  if FScaleMode = AScaleMode then Exit;

  FScaleMode := AScaleMode;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundSprite.SetHorizontalAlign(AAlign: TsgeGUIPropertyHorizontalAlign);
begin
  if FHorizontalAligh = AAlign then Exit;

  FHorizontalAligh := AAlign;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundSprite.SetVerticalAlign(AAlign: TsgeGUIPropertyVerticalAlign);
begin
  if FVerticalAligh = AAlign then Exit;

  FVerticalAligh := AAlign;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundSprite.SetHorizontalOffset(AOffset: Integer);
begin
  if FHorizontalOffset = AOffset then Exit;

  FHorizontalOffset := AOffset;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundSprite.SetVerticalOffset(AOffset: Integer);
begin
  if FVerticalOffset = AOffset then Exit;

  FVerticalOffset := AOffset;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundSprite.SetDrawMode(AMode: TsgeGUISpriteDrawMode);
begin
  if FDrawMode = AMode then Exit;

  FDrawMode := AMode;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundSprite.SetDrawTileX(AX: Word);
begin
  if FDrawTileX = AX then Exit;

  FDrawTileX := AX;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundSprite.SetDrawTileY(AY: Word);
begin
  if FDrawTileY = AY then Exit;

  FDrawTileY := AY;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundSprite.SetDrawMethod(AMethod: TsgeGUISpriteDrawMethod);
begin
  if FDrawMethod = AMethod then Exit;

  FDrawMethod := AMethod;
  RepaintParent;
end;


constructor TsgeGUIPropertyBackgroundSprite.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  //Создать свойства
  FScaleUserRect := TsgeGUIPropertyFloatRect.Create(AOwner);
  FSegmentOffset := TsgeGUIPropertySegmentOffset.Create(AOwner);
  FDrawRect := TsgeGUIPropertyFloatRect.Create(AOwner);

  //Задать параметры
  FSprite := nil;
  FScaleMode := pssmNormal;
  FDrawMode := sdmFull;
  FHorizontalOffset := 0;
  FVerticalOffset := 0;
  FDrawTileX := 0;
  FDrawTileY := 0;
  FDrawMethod := sdmNormal;
end;


destructor TsgeGUIPropertyBackgroundSprite.Destroy;
begin
  FDrawRect.Free;
  FSegmentOffset.Free;
  FScaleUserRect.Free;

  inherited Destroy;
end;


procedure TsgeGUIPropertyBackgroundSprite.Draw;

  function FitIn(ElementW, ElementH, ImageW, ImageH: Integer): TsgeIntPoint;
  var
    k: Single;
  begin
    if (ElementW * ImageH < ImageW * ElementH) then k := ElementW / ImageW else k := ElementH / ImageH;
    Result.X := Round(k * ImageW);
    Result.Y := Round(k * ImageH);
  end;

  function FitOut(ElementW, ElementH, ImageW, ImageH: Integer): TsgeIntPoint;
  var
    k: Single;
  begin
    if (ElementW * ImageH > ImageW * ElementH) then k := ElementW / ImageW else k := ElementH / ImageH;
    Result.X := Round(k * ImageW);
    Result.Y := Round(k * ImageH);
  end;

var
  DrawOpt: TsgeGraphicDrawOptions;
  ElementWidth, ElementHeight, RectWidth, RectHeight: Integer;
  Size: TsgeIntPoint;
  X, Y: Single;
begin
  if FSprite = nil then Exit;

  //Определить размеры элемента
  ElementWidth := TsgeGUIElementHack(FOwner).FWidth;
  ElementHeight := TsgeGUIElementHack(FOwner).FHeight;

  //Подготовить стандартныйе настройки вывода
  DrawOpt := DefaultDrawOptions;

  //Установить спрайт
  DrawOpt.Sprite := FSprite;
  DrawOpt.CoordinateType := gctClassic;

  //Определить область вывода
  case FScaleMode of
    pssmNormal:
      begin
      RectWidth := FSprite.Width;
      RectHeight := FSprite.Height;
      end;

    pssmStretch:
      begin
      RectWidth := ElementWidth;
      RectHeight := ElementHeight;
      end;

    pssmFitIn:
      begin
      Size := FitIn(ElementWidth, ElementHeight, FSprite.Width, FSprite.Height);
      RectWidth := Size.X;
      RectHeight := Size.Y;
      end;

    pssmFitOut:
      begin
      Size := FitOut(ElementWidth, ElementHeight, FSprite.Width, FSprite.Height);
      RectWidth := Size.X;
      RectHeight := Size.Y;
      end;

    pssmUser:
      begin
      RectWidth := Abs(Round(FScaleUserRect.Right - FScaleUserRect.Left));
      RectHeight := Abs(Round(FScaleUserRect.Bottom - FScaleUserRect.Top));
      end;
  end;

  //Определить выравнивание по X
  case FHorizontalAligh of
    phaLeft   : X := 0;
    phaMiddle : X := ElementWidth / 2 - RectWidth / 2;
    phaRight  : X := ElementWidth - RectWidth;
    phaUser   : X := FHorizontalOffset;
  end;

  //Определить выравнивание по Y
  case FVerticalAligh of
    pvaTop    : Y := 0;
    pvaMiddle : Y := ElementHeight / 2 - RectHeight / 2;
    pvaBottom : Y := ElementHeight - RectHeight;
    pvaUser   : Y := FVerticalOffset;
  end;

  //Поправить область вывода
  DrawOpt.Rect.X1 := X;
  DrawOpt.Rect.Y1 := Y;
  DrawOpt.Rect.X2 := DrawOpt.Rect.X1 + RectWidth;
  DrawOpt.Rect.Y2 := DrawOpt.Rect.Y1 + RectHeight;

  //Определить часть спрайта для вывода если плитка
  case FDrawMode of
    sdmTile:
      DrawOpt.SpriteRect := sgeGetTextureTileRect(FSprite, FDrawTileX, FDrawTileY);

    sdmRect:
      DrawOpt.SpriteRect := sgeGetTextureRect(FSprite, FDrawRect.Rect);
  end;

  //Вывод в зависимости от метода
  case FDrawMethod of
    sdmNormal : SGE.ExtGraphic.Graphic.DrawSprite(DrawOpt);
    sdmSegment: SGE.ExtGraphic.Graphic.DrawSpriteSegment(DrawOpt, FSegmentOffset.Rect);
  end;
end;



end.

