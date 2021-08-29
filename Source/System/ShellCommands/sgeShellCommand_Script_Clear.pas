{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Script_Clear.pas
Версия            1.0
Создан            29.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_Script_Clear;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Удалить все скрипт из системы
  Синтаксис:
    Script.Clear
  }
  TsgeShellCommand_Script_Clear = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine;


constructor TsgeShellCommand_Script_Clear.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Clear', Group_Script);
end;


function TsgeShellCommand_Script_Clear.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Удалить все скрипты
  SGE.ExtShell.ScriptList.Clear;
end;




end.


