{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandList.pas
Версия            1.1
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

    function  IndexOf(Name: ShortString): Integer;

    procedure Clear;
    procedure Add(Name: ShortString; Proc: TsgeShellCommandProc; MinParamCount: Word; Group: ShortString);
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


function TsgeShellCommandList.IndexOf(Name: ShortString): Integer;
var
  i: Integer;
begin
  FCS.Enter;
  try

    Result := -1;

    Name := LowerCase(Name);
    for i := 0 to FCount - 1 do
      if LowerCase(FList[i].Name) = Name then
        begin
        Result := i;
        Break;
        end;

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


procedure TsgeShellCommandList.Add(Name: ShortString; Proc: TsgeShellCommandProc; MinParamCount: Word; Group: ShortString);
begin
  FCS.Enter;
  try

    inherited Add(TsgeShellCommand.Create(Name, Proc, MinParamCount, Group));

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

