{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Exit.pas
Версия            1.0
Создан            26.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Exit;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Остановить выполнение сценария
  Синтаксис:
    System.Exit
  }
  TsgeShellCommand_System_Exit = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine;



constructor TsgeShellCommand_System_Exit.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Exit', Group_System);
end;


function TsgeShellCommand_System_Exit.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Очистить стек вызовов
  SGE.ExtShell.CallStack.Clear;
end;






end.

