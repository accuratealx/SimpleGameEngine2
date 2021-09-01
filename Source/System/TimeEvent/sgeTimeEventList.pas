{
Пакет             Simple Game Engine 2
Файл              sgeTimeEventList.pas
Версия            1.0
Создан            31.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список таймерных элементов
}
{$Include Defines.inc}
{$ModeSwitch duplicatelocals+}

unit sgeTimeEventList;

{$mode objfpc}{$H+}

interface

uses
  sgeCriticalSection, sgeTemplateObjectCollection, sgeTimeEventItem, sgeEventTimeEvent;


type
  TsgeTimeEventListTemplate = specialize TsgeTemplateObjectCollection<TsgeTimeEventItem>;


  TsgeTimeEventList = class(TsgeTimeEventListTemplate)
  private
    FCS: TsgeCriticalSection;
  public
    constructor Create;
    destructor  Destroy; override;

    function IndexOf(Proc: TsgeTimeEventProc): Integer;

    procedure Lock;
    procedure Unlock;

    procedure Clear;
    procedure Add(AItem: TsgeTimeEventItem);
    procedure Delete(Index: Integer);
  end;


implementation


constructor TsgeTimeEventList.Create;
begin
  inherited Create(True);

  FCS := TsgeCriticalSection.Create;
end;


destructor TsgeTimeEventList.Destroy;
begin
  FCS.Free;

  inherited Destroy;
end;


function TsgeTimeEventList.IndexOf(Proc: TsgeTimeEventProc): Integer;
var
  i: Integer;
begin
  Result := -1;

  FCS.Enter;
  try

    for i := 0 to FCount - 1 do
      if FList[i].Proc = Proc then Exit(i);

  finally
    FCS.Free;
  end;
end;


procedure TsgeTimeEventList.Lock;
begin
  FCS.Enter;
end;


procedure TsgeTimeEventList.Unlock;
begin
  FCS.Leave;
end;


procedure TsgeTimeEventList.Clear;
begin
  FCS.Enter;
  try

    inherited Clear;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeTimeEventList.Add(AItem: TsgeTimeEventItem);
begin
  FCS.Enter;
  try

    inherited Add(AItem);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeTimeEventList.Delete(Index: Integer);
begin
  FCS.Enter;
  try

    inherited Delete(Index);

  finally
    FCS.Leave;
  end;
end;



end.

