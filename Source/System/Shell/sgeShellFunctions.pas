{
Пакет             Simple Game Engine 2
Файл              sgeShellFunctions.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Методы оболочки
}
{$Include Defines.inc}

unit sgeShellFunctions;

{$mode objfpc}{$H+}

interface


procedure sgeShellFunctions_Init(SGEObject: TObject);


implementation

uses
  SimpleGameEngine, sgeSimpleCommand;


const
  Group_System = 'System';


var
  SGE: TSimpleGameEngine; //Указатель на главный класс




function sgeSystem_Stop(Command: TsgeSimpleCommand): String;
begin
  Result := '';

  SGE.Stop;
end;





/////////////////////////////////////////////////////////////
//                  Регистрация команд                     //
/////////////////////////////////////////////////////////////
procedure sgeShellFunctions_Init(SGEObject: TObject);
begin
  //Запомнить указатель
  SGE := TSimpleGameEngine(SGEObject);

  //Зарегестрировать функции
  with SGE.ExtShell.CommandList do
    begin
    Add('Stop', @sgeSystem_Stop, 0, Group_System);



    end;
end;





end.

