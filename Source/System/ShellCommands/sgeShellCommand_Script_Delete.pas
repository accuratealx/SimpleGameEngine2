{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Script_Delete.pas
Версия            1.0
Создан            29.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_Script_Delete;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Удалить скрипт из системы
  Синтаксис:
    Script.Delete [Name]
  Параметры:
    Name - Имя скрипта
  }
  TsgeShellCommand_Script_Delete = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors;

const
  _UNITNAME = 'sgeShellCommand_Script_Delete';



constructor TsgeShellCommand_Script_Delete.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Delete', Group_Script);

  FParameters.AddString('Name', True);
end;


function TsgeShellCommand_Script_Delete.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Удалить скрипт
  try
    SGE.ExtShell.ScriptList.Delete(Command.Part[1]);
  except
    Result := sgeCreateErrorString(_UNITNAME, Err_ScriptNotFound, Command.Part[1]);
  end;
end;




end.


