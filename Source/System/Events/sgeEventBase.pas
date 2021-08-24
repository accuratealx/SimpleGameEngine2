{
Пакет             Simple Game Engine
Файл              sgeEventBase.pas
Версия            1.1
Создан            22.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс события: Базовое. Все события наследуются от этого класса
}
{$Include Defines.inc}

unit sgeEventBase;

{$mode objfpc}{$H+}

interface


const
  EventProirityMax = $FFFF;
  EventProirityMaxMinusOne = $FFFE;


type
  //Базовый класс события
  TsgeEventBase = class
  end;


  //Обработчик события
  TsgeEventHandler = function(EventObj: TsgeEventBase): Boolean of object;


implementation


end.

