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
  EventPriorityMax = $FFFF;
  EventPriorityMaxMinusOne = $FFFE;
  EventPriorityMaxMinusTwo = $FFFD;

type
  //Базовый класс события
  TsgeEventBase = class
  protected
    FName: ShortString;

  public
    constructor Create(Name: ShortString);

    property Name: ShortString read FName;
  end;


  //Обработчик события
  TsgeEventHandler = function(EventObj: TsgeEventBase): Boolean of object;


implementation


constructor TsgeEventBase.Create(Name: ShortString);
begin
  FName := Name;
end;


end.

