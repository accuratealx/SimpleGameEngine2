{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementSpriteCashed.pas
Версия            1.1
Создан            07.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Кэшированный спрайт
}
{$Include Defines.inc}

unit sgeGraphicElementSpriteCashed;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeGraphicElementSprite,
  sgeGraphic, sgeGraphicColor, sgeGraphicSprite;


type
  TsgeGraphicElementSpriteCashed = class(TsgeGraphicElementSprite)
  private
    FSprite: TsgeGraphicSprite;                                     //Спрайт для вывода
    FRedraw: Boolean;                                               //Флаг обновления спрайта перед перерисовкой

    procedure PostCreate;
  protected
    procedure UpdateData; override;
  public
    constructor Create(X, Y: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType = gctNormal); override;
    constructor Create(X, Y, W, H: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType = gctNormal); override;

    destructor Destroy; override;

    procedure Draw(Graphic: TsgeGraphic); override;
  end;


implementation



procedure TsgeGraphicElementSpriteCashed.PostCreate;
begin
  //Создать спрайт
  FSprite := TsgeGraphicSprite.Create(16, 16);

  //Обновить данные
  UpdateData;
end;


procedure TsgeGraphicElementSpriteCashed.UpdateData;
begin
  //Скопировать данные
  inherited UpdateData;

  //Заменить указатель спрайта на внутренний
  FData.Sprite := FSprite;

  //Установить флаг перерисовки спрайта при выводе
  FRedraw := True;
end;

constructor TsgeGraphicElementSpriteCashed.Create(X, Y: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType);
begin
  inherited Create(X, Y, Sprite, CoordType);

  PostCreate;
end;

constructor TsgeGraphicElementSpriteCashed.Create(X, Y, W, H: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType);
begin
  inherited Create(X, Y, W, H, Sprite, CoordType);

  PostCreate;
end;


destructor TsgeGraphicElementSpriteCashed.Destroy;
begin
  FSprite.Free;

  inherited Destroy;
end;


procedure TsgeGraphicElementSpriteCashed.Draw(Graphic: TsgeGraphic);
begin
  //Проверить на обновление
  if FRedraw then
  begin
    //Убрат флаг обновления
    FRedraw := False;

    //Проверить размеры
    if (FSprite.Width <> FNewData.Sprite.Width) or (FSprite.Height <> FNewData.Sprite.Height) then
      FSprite.SetSize(FNewData.Sprite.Width, FNewData.Sprite.Height);

    //Подготовить графику
    Graphic.PushAttrib;                                             //Сохранить параметры
    Graphic.Reset;                                                  //Сбросить геометрию
    Graphic.RenderSprite := FSprite;                                //Установить спрайт для вывода
    Graphic.RenderPlace := grpSprite;                               //Переключить режим вывода на спрайт
    Graphic.ResetDrawOptions;                                       //Сбросить настройки вывода

    //Стереть фон
    Graphic.BGColor := cBlack;
    Graphic.EraseBG;

    //Скопировать спрайт на себя
    Graphic.ColorBlend := False;
    Graphic.DrawSprite(0, 0, FSprite.Width, FSprite.Height, FNewData.Sprite);
    Graphic.ColorBlend := True;

    //Восстановить графику
    Graphic.RenderPlace := grpScreen;                               //Переключить режим вывода на экран
    Graphic.RenderSprite := nil;                                    //Убрать спрайт из вывода
    Graphic.PopAttrib;                                              //Восстановить параметры
  end;

  //Вывести спрайт
  inherited Draw(Graphic);
end;

end.

