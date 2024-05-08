{
Пакет             Simple Game Engine
Файл              sgeEventBase.pas
Версия            1.3
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

  //События
  Event_Empty = 'Empty';


type
  //Базовый класс события
  TsgeEventBase = class
  protected
    function GetName: ShortString; virtual;

  public
    function Copy: TsgeEventBase; virtual;

    property Name: ShortString read GetName;
  end;


  //Результат вызова подписчика события
  TsgeEventHandlerResult = (
    ehrNormal,  //Посылать объект подписчикам дальше
    ehrBreak    //Не посылать объект подписчикам дальше
  );


  //Обработчик события
  TsgeEventHandler = function(EventObj: TsgeEventBase): TsgeEventHandlerResult of object;


implementation


function TsgeEventBase.GetName: ShortString;
begin
  Result := Event_Empty;
end;


function TsgeEventBase.Copy: TsgeEventBase;
begin
  Result := TsgeEventBase.Create;
end;



end.

