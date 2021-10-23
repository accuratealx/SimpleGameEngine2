{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandList.pas
Версия            1.5
Создан            30.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка команд оболочки
}
{$Include Defines.inc}

unit sgeShellCommandList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateThreadSafeCollection,
  sgeShellCommand;

type
  TsgeShellCommandList = class(specialize TsgeTemplateThreadSafeCollection<TsgeShellCommand>)
  type
    TMatchType = (mtName, mtGroup);
  public
    procedure GetMatchCommandList(Name: ShortString; MatchType: TMatchType; List: TsgeShellCommandList);

    function  IndexOf(Command: TsgeShellCommand): Integer;

    procedure Add(Command: TsgeShellCommand);
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'ShellCommandList';

  Err_CommandAlreadyExist = 'CommandAlreadyExist';



procedure TsgeShellCommandList.GetMatchCommandList(Name: ShortString; MatchType: TMatchType; List: TsgeShellCommandList);
var
  i: Integer;
  S: String;
begin
  //Подготовить имя команды
  Name := LowerCase(Name);

  //Просмотреть список команд
  for i := 0 to FCount - 1 do
    begin
    //Определить имя
    case MatchType of
      mtName  : S := FList[i].Name;
      mtGroup : S := FList[i].GetFullName;
    end;

    //Проверить на совпадение
    if LowerCase(S) = Name then List.Add(FList[i]);
    end;
end;


function TsgeShellCommandList.IndexOf(Command: TsgeShellCommand): Integer;
var
  i: Integer;
  CommandName: String;
begin
  FCS.Enter;
  try

    Result := -1;

    CommandName := LowerCase(Command.GetFullName);
    for i := 0 to FCount - 1 do
      if CommandName = LowerCase(FList[i].GetFullName) then Exit(i);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeShellCommandList.Add(Command: TsgeShellCommand);
begin
  FCS.Enter;
  try

    //Проверить команду на существование
    if IndexOf(Command) <> -1 then
      raise EsgeException.Create(_UNITNAME, Err_CommandAlreadyExist, Command.GetFullName);

    inherited Add(Command);

  finally
    FCS.Leave;
  end;
end;



end.

