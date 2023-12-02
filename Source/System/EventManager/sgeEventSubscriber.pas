{
Пакет             Simple Game Engine 2
Файл              sgeEventSubscriber.pas
Версия            1.0
Создан            12.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс подписчика на события
}
{$Include Defines.inc}

unit sgeEventSubscriber;

{$mode objfpc}{$H+}

interface

uses
  sgeEventBase;


type
  TsgeEventSubscriber = class
  private
    FEnable: Boolean;                 //Флаг активности
    FPriority: Word;                  //Приоритет подписки
    FHandler: TsgeEventHandler;       //Обработчик события

  public
    constructor Create(Handler: TsgeEventHandler; Priority: Word = 0; Enable: Boolean = True);

    property Enable: Boolean read FEnable write FEnable;
    property Priority: Word read FPriority;
    property Handler: TsgeEventHandler read FHandler;
  end;



implementation


constructor TsgeEventSubscriber.Create(Handler: TsgeEventHandler; Priority: Word; Enable: Boolean);
begin
  FHandler := Handler;
  FPriority := Priority;
  FEnable := Enable;
end;


end.

