{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_CommandHistory_Clear.pas
Версия            1.0
Создан            12.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_CommandHistory_Clear;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Очистить историю введённых команд
  Синтаксис:
    CommandHistory.Clear
  }
  TsgeShellCommand_CommandHistory_Clear = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine;



constructor TsgeShellCommand_CommandHistory_Clear.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Clear', Group_CommandHistory);
end;


function TsgeShellCommand_CommandHistory_Clear.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  SGE.ExtShell.CommandHistory.Clear;
end;



end.

