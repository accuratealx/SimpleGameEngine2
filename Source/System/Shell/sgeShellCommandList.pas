{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandList.pas
Версия            1.4
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
  private
    FCS: TsgeCriticalSection;

    function GetItem(Index: Integer): TsgeShellCommand;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Lock;
    procedure UnLock;

    function  IndexOf(Name: ShortString): TsgeShellCommand;

    procedure Clear;
    procedure Add(Command: TsgeShellCommand);
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Item: TsgeShellCommand);
  end;


implementation


function TsgeShellCommandList.GetItem(Index: Integer): TsgeShellCommand;
begin
  FCS.Enter;
  try

    Result := inherited GetItem(Index);

  finally
    FCS.Leave;
  end;
end;


constructor TsgeShellCommandList.Create;
begin
  inherited Create(True);
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


function TsgeShellCommandList.IndexOf(Name: ShortString): TsgeShellCommand;
var
  i: Integer;
begin
  FCS.Enter;
  try
    Result := nil;

    //Поиск по формату Group.Name
    Name := LowerCase(Name);
    for i := 0 to FCount - 1 do
      if LowerCase(FList[i].Group + '.' + FList[i].Name) = Name then Exit(FList[i]);

  finally
    FCS.Leave;
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

