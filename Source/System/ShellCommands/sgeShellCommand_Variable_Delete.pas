{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Variable_Delete.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_Variable_Delete;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Удалить переменную
  Синтаксис:
    Variable.Delete [VariableName]
  Параметры:
    Name - Имя переменной
  }
  TsgeShellCommand_Variable_Delete = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine;



constructor TsgeShellCommand_Variable_Delete.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Delete', Group_Variables);

  FParameters.AddString('VariableName', True);
end;


function TsgeShellCommand_Variable_Delete.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  SGE.ExtVariables.Variables.Delete(Command.Part[1]);
end;






end.

