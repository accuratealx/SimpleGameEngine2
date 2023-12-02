{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandTypes.pas
Версия            1.1
Создан            05.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Общие типы
}
{$Include Defines.inc}

unit sgeKeyCommandTypes;

{$mode objfpc}{$H+}

interface

const
  Err_KeyNotFound = 'KeyNotFound';
  Err_IndexOutOfBounds  = 'IndexOutOfBounds';


type
  //Класс хранения действий на отпускание и нажатие
  TsgeKeyCommandAction = class
    Up: String;
    Down: String;
  end;

  //Класс хранения действий на отпускание и нажатие клавиши мыши
  TsgeKeyCommandActionMouse = class
    Up: String;
    Down: String;
    DblClick: String;
  end;


implementation


end.

