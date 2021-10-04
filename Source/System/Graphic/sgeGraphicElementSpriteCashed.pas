{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementSpriteCashed.pas
Версия            1.0
Создан            07.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Кэшированный спрайт
}
{$Include Defines.inc}

unit sgeGraphicElementSpriteCashed;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicElementSprite,
  sgeGraphic, sgeGraphicSprite;


type
  TsgeGraphicElementSpriteCashed = class(TsgeGraphicElementSprite)
  private
    FSprite: TsgeGraphicSprite;             //Спрайт для вывода
    FRedraw: Boolean;                       //Флаг обновления спрайтв перед перерисовкой

    procedure AfterConstruction; override;
  public
    destructor Destroy; override;

    procedure UpdateData; override;
    procedure Draw(Graphic: TsgeGraphic); override;
  end;


implementation




procedure TsgeGraphicElementSpriteCashed.AfterConstruction;
begin
  inherited AfterConstruction;

  //Создать спрайт
  FSprite := TsgeGraphicSprite.Create(200, 200);

  //Обновить данные
  UpdateData;
end;


destructor TsgeGraphicElementSpriteCashed.Destroy;
begin
  FSprite.Free;
  inherited Destroy;
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

    //Залить прозрачным цветом
    Graphic.PushAttrib;                                             //Сохранить параметры
    Graphic.ColorBlend := False;                                    //Убрать смешивание цветов
    Graphic.RenderSprite := FSprite;                                //Установить спрайт для вывода
    Graphic.RenderPlace := grpSprite;                               //Переключить режим вывода на спрайт
    Graphic.PoligonMode := gpmFill;                                 //Включить заливку у полигонов
    Graphic.ResetDrawOptions;                                       //Сбросить настройки вывода
    Graphic.DrawSprite(0, 0, FSprite.Width, FSprite.Height, FNewData.Sprite); //Скопировать спрайт на себя
    Graphic.RenderPlace := grpScreen;                               //Переключить режим вывода на экран
    Graphic.RenderSprite := nil;                                    //Убрать спрайт из вывода
    Graphic.ColorBlend := True;                                     //Восстановить смешивание цветов
    Graphic.PopAttrib;                                              //Восстановить параметры
    end;

  //Вывести спрайт
  inherited Draw(Graphic);
end;

end.

