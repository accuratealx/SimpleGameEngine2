{
Пакет             Simple Game Engine
Файл              sgeEventBase.pas
Версия            1.2
Создан            22.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс события: Базовое. Все события наследуются от этого класса
}
{$Include Defines.inc}

unit sgeEventBase;

{$mode objfpc}{$H+}

interface


const
  //Приоритеты подписок событий
  Event_Priority_Max        = $FFFF;
  Event_Priority_Shell      = $FFFE;
  Event_Priority_KeyCommand = $FFFD;
  Event_Priority_GUI        = $FFFC;


type
  //Результат вызова подписчика события
  TsgeEventHandlerResult = (
    ehrDefault,                                                     //Посылать объект подписчикам дальше и удалить
    ehrStopSend,                                                    //Не посылать объект подписчикам дальше и удалить
    ehrBreak                                                        //Не посылать объект подписчикам дальше и не удалять
  );


  //Базовый класс события
  TsgeEventBase = class
  protected
    FName: ShortString;

  public
    constructor Create(Name: ShortString);

    property Name: ShortString read FName;
  end;


  //Обработчик события
  TsgeEventHandler = function(EventObj: TsgeEventBase): TsgeEventHandlerResult of object;


implementation


constructor TsgeEventBase.Create(Name: ShortString);
begin
  FName := Name;
end;


end.

