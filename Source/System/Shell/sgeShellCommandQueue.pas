{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandQueue.pas
Версия            1.0
Создан            01.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс очереди команд на выполнение
}
{$Include Defines.inc}

unit sgeShellCommandQueue;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateList, sgeCriticalSection;


type
  TsgeShellCommandQueueTemplate = specialize TsgeTemplateList<String>;


  TsgeShellCommandQueue = class(TsgeShellCommandQueueTemplate)
  private
    FCS: TsgeCriticalSection;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure ClearItem;
    procedure Add(Command: String);

    function PullFirstCommand: String;
  end;



implementation



constructor TsgeShellCommandQueue.Create;
begin
  inherited Create;

  FCS := TsgeCriticalSection.Create;
end;


destructor TsgeShellCommandQueue.Destroy;
begin
  FCS.Free;

  inherited Destroy;
end;


procedure TsgeShellCommandQueue.ClearItem;
begin
  FCS.Enter;
  try

    inherited ClearItem;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeShellCommandQueue.Add(Command: String);
begin
  FCS.Enter;
  try

    AddItem(Command);

  finally
    FCS.Leave;
  end;
end;


function TsgeShellCommandQueue.PullFirstCommand: String;
begin
  FCS.Enter;
  try

    if FCount > 0 then
      begin
      //Вернуть результат
      Result := GetItem(0);

      //Удалить первую команду
      inherited DeleteItem(0);
      end;

  finally
    FCS.Leave;
  end;
end;


end.

