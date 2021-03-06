{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Call.pas
Версия            1.1
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда оболочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Call;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Вызвать подпрограмму
  Синтаксис:
    System.Call [<Script:>ProcName]
  Параметры:
    Script - Имя сценария
    ProcName - Имя процедуры
  }
  TsgeShellCommand_System_Call = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors, sgeShellCommandsUtils, sgeShellScript, sgeShellCallStackItem;


const
  _UNITNAME = 'ShellCommand_System_Call';


constructor TsgeShellCommand_System_Call.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Call', Group_System);
  FParameters.AddString('ProcName', True);
end;


function TsgeShellCommand_System_Call.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  LabelName, ScriptName: ShortString;
  Call: TsgeShellStackItem;
  Script: TsgeShellScript;
  Pos: Integer;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Выделить имя сценария и метки
  sgeExtractScriptAndLabel(Command.Part[1], ScriptName, LabelName);

  //Взять последний вызов стека
  Call := SGE.ExtShell.CallStack.GetLast;

  //Найти сылку на скрипт
  Script := SGE.ExtShell.ScriptList.GetByName(Call.Name);

  //Если указано имя скрипта, то найти
  if ScriptName <> '' then
  begin
    //Найти указатель на сценарий
    Script := SGE.ExtShell.ScriptList.GetByName(ScriptName);

    //Проверить на пустой указатель
    if Script = nil then
      Exit(sgeCreateErrorString(_UNITNAME, Err_ScriptNotFound, ScriptName));
  end;

  //Найти номер строки метки
  Pos := sgeGetProcedurePosInScript(Script, LabelName);

  //Не найдена процедура
  if Pos = -1 then
    Exit(sgeCreateErrorString(_UNITNAME, Err_ProcedureNotFound, Command.Part[1]));

  //Добавить новый элемент стека вызовов
  SGE.ExtShell.CallStack.Add(Script.Name, Pos + 1);
end;






end.

