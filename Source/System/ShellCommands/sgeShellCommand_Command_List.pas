{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Command_List.pas
Версия            1.0
Создан            24.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда оболочки
}
{$Include Defines.inc}

unit sgeShellCommand_Command_List;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Вывести список команд
  Синтаксис:
    Command.List <Mask>
  Параметры:
    Mask - Маска поиска
  }
  TsgeShellCommand_Command_List = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeSystemUtils, sgeStringUtils, sgeShellCommandsUtils, sgeExtensionShell;



constructor TsgeShellCommand_Command_List.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'List', Group_Command);
end;


function TsgeShellCommand_Command_List.Execute(Command: TsgeSimpleCommand): String;
const
  Indent = '  ';
var
  SGE: TSimpleGameEngine;
  i, c: Integer;
  Mask: String;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Маска поиска
  Mask := sgeGetMaskSafe(Command.GetPartSafe(1), SGE.ExtShell.StrictSearch);

  //Заголовок
  SGE.ExtShell.LogMessage(SGE.ExtShell.GetLocalizedString(Const_CommandList) + ': ' + Mask, smtNote);

  //Вывод списка комманд
  c := 0;
  for i := 0 to SGE.ExtShell.CommandList.Count - 1 do
  begin
    if sgeMatchString(SGE.ExtShell.CommandList.Item[i].Name, Mask, SGE.ExtShell.IgnoreCase) or
      sgeMatchString(SGE.ExtShell.CommandList.Item[i].GetFullName, Mask, SGE.ExtShell.IgnoreCase) then
    begin
      SGE.ExtShell.LogMessage(Indent + SGE.ExtShell.CommandList.Item[i].GetFullName, smtText);
      Inc(c);
    end;
  end;

  //Итог
  SGE.ExtShell.LogMessage(SGE.ExtShell.GetLocalizedString(Const_Total) + ': ' + sgeIntToStr(c), smtNote);
  SGE.ExtShell.LogMessage('');
end;






end.

