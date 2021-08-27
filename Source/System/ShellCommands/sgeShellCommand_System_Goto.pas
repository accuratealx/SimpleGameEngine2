{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Goto.pas
Версия            1.0
Создан            27.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Goto;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Перейти на метку
  Синтаксис:
    System.Goto [<Script:>LabelName]
  Параметры:
    Script - Имя сценария
    LabelName - Имя метки
  }
  TsgeShellCommand_System_Goto = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors, sgeShellCommandsUtils, sgeShellScript, sgeShellCallStackItem;


const
  _UNITNAME = 'ShellCommand_System_Goto';

  Err_ScriptNotFound  = 'ScriptNotFound';
  Err_LabelNotFound   = 'LabelNotFound';


constructor TsgeShellCommand_System_Goto.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Goto', Group_System);

  //Добавить параметры
  FParameters.AddString('Label', True);
end;


function TsgeShellCommand_System_Goto.Execute(Command: TsgeSimpleCommand): String;
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
      begin
      Result := sgeCreateErrorString(_UNITNAME, Err_ScriptNotFound, ScriptName);
      Exit;
      end;
    end;

  //Найти номер строки метки
  Pos := sgeGetLabelPosInScript(Script, LabelName);

  //Не найдена метка
  if Pos = -1 then
    begin
    Result := sgeCreateErrorString(_UNITNAME, Err_LabelNotFound, Command.Part[1]);
    Exit;
    end;

  //Внести изменения в стек вызовов
  if ScriptName <> '' then Call.Name := ScriptName;
  Call.Pos := Pos + 1;
end;






end.

