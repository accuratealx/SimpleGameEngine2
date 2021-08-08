{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Variable_Set.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_Variable_Set;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Изменить значение переменной
  Синтаксис:
    Variable.Set [Name] [Value]
  Параметры:
    Name - Имя переменной
    Value - Новое значение
  Заметка:
    Если переменная не существовала, то будет создана новая
  }
  TsgeShellCommand_Variable_Set = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors;



constructor TsgeShellCommand_Variable_Set.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Set', Group_Variables);

  FParameters.AddString('VariableName', True);
  FParameters.AddString('Value', True);
end;


function TsgeShellCommand_Variable_Set.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  try
    SGE.ExtVariables.SetString(Command.Part[1], Command.Part[2]);
  except
    Result := sgeCreateErrorString(_UNITNAME, Err_VariableIsReadOnly, Command.Part[1]);
  end;
end;






end.

