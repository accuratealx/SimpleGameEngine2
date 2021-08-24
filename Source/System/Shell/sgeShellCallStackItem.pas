{
Пакет             Simple Game Engine 2
Файл              sgeShellCallStackItem.pas
Версия            1.0
Создан            25.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент стека вызовов
}
{$Include Defines.inc}

unit sgeShellCallStackItem;

{$mode objfpc}{$H+}

interface


type
  TsgeShellStackItem = class
  private
    FName: ShortString;                                             //Имя сценария
    FPos: Integer;                                                  //Положение в файле
  public
    constructor Create(Name: ShortString; Pos: Integer = 0);

    property Name: ShortString read FName write FName;
  end;


implementation


constructor TsgeShellStackItem.Create(Name: ShortString; Pos: Integer);
begin
  FName := Name;
  FPos := Pos;
end;



end.

