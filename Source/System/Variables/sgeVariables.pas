{
Пакет             Simple Game Engine 2
Файл              sgeVariables.pas
Версия            1.0
Создан            24.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Методы виртуальных переменных
}
{$Include Defines.inc}

unit sgeVariables;

{$mode objfpc}{$H+}

interface


procedure sgeVariables_Init(SGEObject: TObject);


implementation

uses
  SimpleGameEngine;


var
  SGE: TSimpleGameEngine;




procedure sgeVariables_Init(SGEObject: TObject);
begin
  //Запомнить указатель
  SGE := TSimpleGameEngine(SGEObject);

  //Добавить переменные



end;



end.

