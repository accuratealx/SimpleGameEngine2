{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Run.pas
Версия            1.0
Создан            27.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Run;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Выполнить сценарий
  Синтаксис:
    System.Run [Script]
  Параметры:
    Script - Имя скрипта
  }
  TsgeShellCommand_System_Run = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors, sgeShellScript;

const
  _UNITNAME = 'ShellCommand_System_Run';



constructor TsgeShellCommand_System_Run.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Run', Group_System);

  FParameters.AddString('Script', True);
end;


function TsgeShellCommand_System_Run.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  ScriptName: String;
  Script: TsgeShellScript;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Имя скрипта
  ScriptName := Command.Part[1];

  //Найти скрипт
  Script := SGE.ExtShell.ScriptList.GetByName(ScriptName);

  //Проверить на пустой указатель
  if Script = nil then
    Exit(sgeCreateErrorString(_UNITNAME, Err_ScriptNotFound, ScriptName));

  //Добавить новый элемент стека вызовов
  SGE.ExtShell.CallStack.Add(ScriptName, 0);
end;






end.

