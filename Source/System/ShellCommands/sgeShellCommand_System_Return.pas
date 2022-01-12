{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Return.pas
Версия            1.0
Создан            26.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Return;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Удалить последний элемент стека вызовов
  Синтаксис:
    System.Return <Name>
  Параметры:
    Name - Имя метки
  }
  TsgeShellCommand_System_Return = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine;



constructor TsgeShellCommand_System_Return.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Return', Group_System);

  FParameters.AddString('Name', False);
end;


function TsgeShellCommand_System_Return.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Удалить последний элемент стека вызовов
  SGE.ExtShell.CallStack.DeleteLast;
end;






end.

