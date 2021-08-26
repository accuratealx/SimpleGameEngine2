{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Procedure.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
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
  sgeShellCallStackItem, sgeShellScript;

const
  _UNITNAME = 'sgeShellCommand_System_Procedure';

  Err_ScriptNotFound        = 'ScriptNotFound';
  Err_CantFindProcedureEnd  = 'CantFindProcedureEnd';



constructor TsgeShellCommand_System_Procedure.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Procedure', Group_System);

  //Добавить параметры
  FParameters.AddString('Name', True);
end;


function TsgeShellCommand_System_Procedure.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  Call: TsgeShellStackItem;
  Script: TsgeShellScript;
  Line: TsgeSimpleCommand;
  i: Integer;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Получить последний вызов стека
  Call := SGE.ExtShell.CallStack.GetLast;

  //Найти скрипт
  Script := SGE.ExtShell.ScriptList.GetByName(Call.Name);

  //Если не найден скрипт, то выход
  if Script = nil then
    begin
    Result := sgeCreateErrorString(_UNITNAME, Err_ScriptNotFound, Call.Name);
    Exit;
    end;

  //Просмотреть дальше по списку до команды Return
  Line := TsgeSimpleCommand.Create;
  try
    for i := Call.Pos to Script.Count - 1 do
      begin
      Line.Command := Script.Item[i];

      //Пропуск если нет двух частей
      if Line.Count < 2 then Continue;

      //Проверить совпадение
      if (LowerCase(Line.Part[0]) = 'return') and (LowerCase(Line.Part[1]) = LowerCase(Command.Part[1])) then
        begin
        Call.Pos := i + 1;
        Exit;
        end;
      end;

  finally
    Line.Free;
  end;


  //Если не найден конец процедуры
  Result := sgeCreateErrorString(_UNITNAME, Err_CantFindProcedureEnd, Command.Part[1]);
end;






end.

