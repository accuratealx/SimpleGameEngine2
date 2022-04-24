{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Procedure.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда оболочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Procedure;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Заголовок подпрограммы
  Синтаксис:
    System.Procedure [Name]
  Параметры:
    Name - Имя подпрограммы
  Заметка:
    Пропускает строки до return [Name]
  }
  TsgeShellCommand_System_Procedure = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors,
  sgeShellCallStackItem, sgeShellScript, sgeShellCommandsUtils;

const
  _UNITNAME = 'ShellCommand_System_Procedure';



constructor TsgeShellCommand_System_Procedure.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Procedure', Group_System);

  FParameters.AddString('Name', True);
end;


function TsgeShellCommand_System_Procedure.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  Call: TsgeShellStackItem;
  Script: TsgeShellScript;
  Pos: Integer;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Получить последний вызов стека
  Call := SGE.ExtShell.CallStack.GetLast;

  //Найти скрипт
  Script := SGE.ExtShell.ScriptList.GetByName(Call.Name);

  //Если не найден скрипт, то выход
  if Script = nil then
    Exit(sgeCreateErrorString(_UNITNAME, Err_ScriptNotFound, Call.Name));

  //Найти конец процедуры
  Pos := sgeGetProcedureEndInScript(Script, Command.Part[1], Call.Pos);

  //Конец процедуры не найден
  if Pos = -1 then
    Exit(sgeCreateErrorString(_UNITNAME, Err_CantFindProcedureEnd, Command.Part[1]));

  //Изменить текущее положение
  Call.Pos := Pos + 1;
end;






end.

