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
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeCriticalSection,
  sgeShellCommand, sgeTemplateObjectCollection;

type
  TsgeShellCommandListTemplate = specialize TsgeTemplateObjectCollection<TsgeShellCommand>;


  TsgeShellCommandList = class(TsgeShellCommandListTemplate)
  type
    TMatchType = (mtName, mtGroup);
  private
    FCS: TsgeCriticalSection;

    function GetItem(Index: Integer): TsgeShellCommand;
  public
    constructor Create(FreeObjects: Boolean = True); override;
    destructor  Destroy; override;

    procedure Lock;
    procedure UnLock;

    function  IndexOf(Command: TsgeShellCommand): Integer;

    //Вернуть список подходящих команд
    procedure GetMatchCommandList(Name: ShortString; MatchType: TMatchType; List: TsgeShellCommandList);

    procedure Clear;
    procedure Add(Command: TsgeShellCommand);
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Item: TsgeShellCommand);
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'ShellCommandList';

  Err_CommandAlreadyExist = 'CommandAlreadyExist';


function TsgeShellCommandList.GetItem(Index: Integer): TsgeShellCommand;
begin
  FCS.Enter;
  try

    Result := inherited GetItem(Index);

  finally
    FCS.Leave;
  end;
end;


constructor TsgeShellCommandList.Create(FreeObjects: Boolean);
begin
  inherited Create(FreeObjects);
  FCS := TsgeCriticalSection.Create;
end;


destructor TsgeShellCommandList.Destroy;
begin
  FCS.Free;
  inherited Destroy;
end;


procedure TsgeShellCommandList.Lock;
begin
  FCS.Enter;
end;


procedure TsgeShellCommandList.UnLock;
begin
  FCS.Leave;
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


procedure TsgeShellCommandList.Clear;
begin
  FCS.Enter;
  try

    inherited Clear;

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


procedure TsgeShellCommandList.Delete(Index: Integer);
begin
  FCS.Enter;
  try

    inherited Delete(Index);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeShellCommandList.Insert(Index: Integer; Item: TsgeShellCommand);
begin
  FCS.Enter;
  try

    inherited Insert(Index, Item);

  finally
    FCS.Leave;
  end;
end;


end.

