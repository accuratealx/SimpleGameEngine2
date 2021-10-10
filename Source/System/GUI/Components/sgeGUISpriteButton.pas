{
Пакет             Simple Game Engine 2
Файл              sgeGUISpriteButton.pas
Версия            1.0
Создан            10.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Спрайтовая кнопка
}
{$Include Defines.inc}

unit sgeGUISpriteButton;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeSimpleParameters,
  sgeGUIElement, sgeGUIPropertySegmentOffset,
  sgeGraphicSprite;


type
  TsgeGUISpriteButton = class(TsgeGUIElement)
  private
    FSprite: TsgeGraphicSprite;
    FOffset: TsgeGUIPropertySegmentOffsetExt;

    procedure SetSprite(ASprite: TsgeGraphicSprite);

    function GetOffset: TsgeGUIPropertySegmentOffset;
  protected
    class function GetParameterSectionName: String; override;
    procedure LoadData(Data: TsgeSimpleParameters); override;
    procedure DrawBefore; override;

  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    property Sprite: TsgeGraphicSprite read FSprite write SetSprite;
    property Offset: TsgeGUIPropertySegmentOffset read GetOffset;
  end;


implementation

uses
  sgeVars,
  sgeGraphic, sgeGraphicUtils;


procedure TsgeGUISpriteButton.SetSprite(ASprite: TsgeGraphicSprite);
begin
  if FSprite = ASprite then Exit;

  FSprite := ASprite;

  //Спрайт всегда должен быть
  if FSprite = nil then FSprite := SGE.ExtResourceList.Default.Sprite;

  Repaint;
end;


function TsgeGUISpriteButton.GetOffset: TsgeGUIPropertySegmentOffset;
begin
  Result := FOffset;
end;


class function TsgeGUISpriteButton.GetParameterSectionName: String;
begin
  Result := 'SpriteButton';
end;


procedure TsgeGUISpriteButton.LoadData(Data: TsgeSimpleParameters);
var
  ParamName: String;
begin
  inherited LoadData(Data);

  //Sprite
  ParamName := 'Sprite.Name';
  if Data.Exist[ParamName] then
    FSprite := SGE.ExtResourceList.GetSprite(Data.GetValue(ParamName, ''));

  //Offset
  FOffset.LoadParameters(Data, 'Offset.');
end;


procedure TsgeGUISpriteButton.DrawBefore;
var
  DrawOpt: TsgeGraphicDrawOptions;
begin
  //Подготовить стандартныйе настройки вывода
  DrawOpt := DefaultDrawOptions;

  //Установить спрайт
  DrawOpt.Sprite := FSprite;
  DrawOpt.CoordinateType := gctClassic;

  //Поправить область вывода
  DrawOpt.Rect.X1 := 0;
  DrawOpt.Rect.Y1 := 0;
  DrawOpt.Rect.X2 := FWidth;
  DrawOpt.Rect.Y2 := FHeight;

  //Поправить область вывода спрайта
  DrawOpt.SpriteRect := sgeGetTextureTileRect(1, 4, 0, 1);

  //Вывод спрайта
  SGE.ExtGraphic.Graphic.DrawSpriteSegment(DrawOpt, FOffset.Rect);
end;


constructor TsgeGUISpriteButton.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  FOffset := TsgeGUIPropertySegmentOffsetExt.Create(Self);

  FSprite := SGE.ExtResourceList.Default.Sprite;

  Repaint;
end;


destructor TsgeGUISpriteButton.Destroy;
begin
  FOffset.Free;

  inherited Destroy;
end;

end.


