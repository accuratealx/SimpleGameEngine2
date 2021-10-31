{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Variable_Clear.pas
Версия            1.1
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_Variable_Clear;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Удалить все переменные
  Синтаксис:
    Variable.Clear
  }
  TsgeShellCommand_Variable_Clear = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine;



constructor TsgeShellCommand_Variable_Clear.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Clear', Group_Variables);
end;


function TsgeShellCommand_Variable_Clear.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  SGE.ExtVariables.Variables.DeleteNonSystem;
end;






end.

