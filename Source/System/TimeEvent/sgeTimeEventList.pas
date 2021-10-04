{
Пакет             Simple Game Engine 2
Файл              sgeTimeEventList.pas
Версия            1.1
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
  sgeCriticalSection, sgeTemplateCollection, sgeTimeEventItem;


type
  TsgeTimeEventListTemplate = specialize TsgeTemplateCollection<TsgeTimeEventItem>;


  TsgeTimeEventList = class(TsgeTimeEventListTemplate)
  private
    FCS: TsgeCriticalSection;

  public
    constructor Create; override;
    destructor  Destroy; override;

    function IndexOf(AItem: TsgeTimeEventItem): Integer;

    procedure Lock;
    procedure Unlock;

    procedure Add(AItem: TsgeTimeEventItem);
    procedure Delete(AItem: TsgeTimeEventItem);
  end;


implementation


constructor TsgeTimeEventList.Create;
begin
  inherited Create;

  FCS := TsgeCriticalSection.Create;
end;


destructor TsgeTimeEventList.Destroy;
begin
  FCS.Free;

  inherited Destroy;
end;


function TsgeTimeEventList.IndexOf(AItem: TsgeTimeEventItem): Integer;
var
  i: Integer;
begin
  Result := -1;

  FCS.Enter;
  try

    for i := 0 to FCount - 1 do
      if FList[i] = AItem then Exit(i);

  finally
    FCS.Leave;
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


procedure TsgeTimeEventList.Add(AItem: TsgeTimeEventItem);
begin
  FCS.Enter;
  try

    inherited Add(AItem);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeTimeEventList.Delete(AItem: TsgeTimeEventItem);
var
  Idx: Integer;
begin
  FCS.Enter;
  try

    Idx := IndexOf(AItem);
    if Idx <> -1 then
       inherited Delete(Idx);

  finally
    FCS.Leave;
  end;
end;



end.

