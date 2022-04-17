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
  sgeEventMouse, sgeGUIElement, sgeGUIPropertySegmentOffset,
  sgeGraphicSprite;


type
  TsgeGUISpriteButton = class(TsgeGUIElement)
  private
    type
      TButtonState = (bsDisable, bsNormal, bsActive, bsPressed);

  private
    FSprite: TsgeGraphicSprite;
    FOffset: TsgeGUIPropertySegmentOffsetExt;

    FButtonState: TButtonState;

    procedure SetSprite(ASprite: TsgeGraphicSprite);

    function GetOffset: TsgeGUIPropertySegmentOffset;
  protected
    FColCount: Word;

    function GetColSpriteIndex: Word; virtual;

    class function GetParameterSectionName: String; override;
    procedure LoadData(Data: TsgeSimpleParameters); override;
    procedure DrawBefore; override;
    procedure SetEnable(AEnabled: Boolean); override;

    procedure Handler_MouseEnter(Mouse: TsgeEventMouse); override;
    procedure Handler_MouseLeave(Mouse: TsgeEventMouse); override;
    procedure Handler_MouseDown(Mouse: TsgeEventMouse); override;
    procedure Handler_MouseUp(Mouse: TsgeEventMouse); override;
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
  if FSprite = ASprite then
    Exit;

  FSprite := ASprite;

  //Спрайт всегда должен быть
  if FSprite = nil then
    FSprite := SGE.ExtResourceList.Default.Sprite;

  Repaint;
end;


function TsgeGUISpriteButton.GetOffset: TsgeGUIPropertySegmentOffset;
begin
  Result := FOffset;
end;


function TsgeGUISpriteButton.GetColSpriteIndex: Word;
begin
  Result := 0;
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
  //Подготовить стандартные настройки вывода
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
  DrawOpt.SpriteRect := sgeGetTextureTileRect(FColCount, 4, GetColSpriteIndex, Ord(FButtonState));

  //Вывод спрайта
  SGE.ExtGraphic.Graphic.DrawSpriteSegment(DrawOpt, FOffset.Rect);
end;


procedure TsgeGUISpriteButton.SetEnable(AEnabled: Boolean);
begin
  if AEnabled then
    FButtonState := bsNormal
  else
    FButtonState := bsDisable;

  inherited SetEnable(AEnabled);
end;


procedure TsgeGUISpriteButton.Handler_MouseEnter(Mouse: TsgeEventMouse);
begin
  if FPressed then
    FButtonState := bsPressed
  else
    FButtonState := bsActive;
  Repaint;

  inherited Handler_MouseEnter(Mouse);
end;


procedure TsgeGUISpriteButton.Handler_MouseLeave(Mouse: TsgeEventMouse);
begin
  FButtonState := bsNormal;
  Repaint;

  inherited Handler_MouseLeave(Mouse);
end;


procedure TsgeGUISpriteButton.Handler_MouseDown(Mouse: TsgeEventMouse);
begin
  if FPressed then
    FButtonState := bsPressed;
  Repaint;

  inherited Handler_MouseDown(Mouse);
end;


procedure TsgeGUISpriteButton.Handler_MouseUp(Mouse: TsgeEventMouse);
begin
  FButtonState := bsActive;
  Repaint;

  inherited Handler_MouseUp(Mouse);
end;


constructor TsgeGUISpriteButton.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  FOffset := TsgeGUIPropertySegmentOffsetExt.Create(Self);

  FSprite := SGE.ExtResourceList.Default.Sprite;

  FButtonState := bsNormal;
  FColCount := 1;

  Repaint;
end;


destructor TsgeGUISpriteButton.Destroy;
begin
  FOffset.Free;

  inherited Destroy;
end;

end.


