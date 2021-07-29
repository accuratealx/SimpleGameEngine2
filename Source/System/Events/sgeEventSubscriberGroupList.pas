{
Пакет             Simple Game Engine 2
Файл              sgeEventSubscriberGroupList.pas
Версия            1.4
Создан            12.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка групп подписчиков
}
{$Include Defines.inc}

unit sgeEventSubscriberGroupList;

{$mode objfpc}{$H+}

interface

uses
  sgeCriticalSection,
  sgeTemplateObjectCollection, sgeEventBase, sgeEventSubscriber, sgeEventSubscriberGroup;

type
  //Шаблон списка
  TsgeEventSubscriberGroupListTemplate = specialize TsgeTemplateObjectCollection<TsgeEventSubscriberGroup>;


  //Список групп подписчиков
  TsgeEventSubscriberGroupList = class(TsgeEventSubscriberGroupListTemplate)
  private
    FCS: TsgeCriticalSection;

  public
    constructor Create; reintroduce;
    destructor  Destroy; override;

    function IndexOf(Name: ShortString): Integer;

    procedure Lock;
    procedure Unlock;

    procedure Clear;
    procedure Add(Name: String);
    procedure Delete(Index: Integer);

    function  Subscribe(EventName: ShortString; Handler: TsgeEventHandler; Priority: Word = 0; Enable: Boolean = True): TsgeEventSubscriber;

    procedure UnSubscribe(EventName: ShortString; Handler: TsgeEventHandler);
    procedure UnSubscribe(EventName: ShortString; Obj: TObject);
    procedure UnSubscribe(Handler: TsgeEventHandler);
    procedure UnSubscribe(Obj: TObject);
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'EventSubscriberGroupList';

  Err_IndexOutOfBounds = 'IndexOutOfBounds';



constructor TsgeEventSubscriberGroupList.Create;
begin
  inherited Create;
  FCS := TsgeCriticalSection.Create;
end;


destructor TsgeEventSubscriberGroupList.Destroy;
begin
  FCS.Free;
  inherited Destroy;
end;


function TsgeEventSubscriberGroupList.IndexOf(Name: ShortString): Integer;
var
  i: Integer;
begin
  FCS.Enter;
  try

    Result := -1;

    Name := LowerCase(Name);
    for i := 0 to FCount - 1 do
      if Name = LowerCase(FList[i].Name) then
        begin
        Result := i;
        Break;
        end;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.Lock;
begin
  FCS.Enter;
end;


procedure TsgeEventSubscriberGroupList.Unlock;
begin
  FCS.Leave;
end;


procedure TsgeEventSubscriberGroupList.Clear;
begin
  FCS.Enter;
  try

    inherited Clear;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.Add(Name: String);
begin
  FCS.Enter;
  try

    inherited Add(TsgeEventSubscriberGroup.Create(Name));

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.Delete(Index: Integer);
begin
  FCS.Enter;
  try

    inherited Delete(Index);

  finally
    FCS.Leave;
  end;
end;


function TsgeEventSubscriberGroupList.Subscribe(EventName: ShortString; Handler: TsgeEventHandler; Priority: Word; Enable: Boolean): TsgeEventSubscriber;
var
  Idx: Integer;
begin
  FCS.Enter;
  try

    //Найти индекс группы
    Idx := IndexOf(EventName);

    //Если нет группы, то создать
    if Idx = -1 then
      begin
      Add(EventName);
      Idx := FCount - 1;
      end;

    //Добавить подписчика
    Result := FList[Idx].Subscribers.Add(Handler, Priority, Enable);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(EventName: ShortString; Handler: TsgeEventHandler);
var
  Idx: Integer;
begin
  FCS.Enter;
  try

    //Поиск группы
    Idx := IndexOf(EventName);

    //Группа не найдена
    if Idx = -1 then Exit;

    //Удалить подписчика
    FList[Idx].Subscribers.Delete(Handler);

    //Проверить на пустую группу
    if FList[Idx].Subscribers.Count = 0 then Delete(Idx);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(EventName: ShortString; Obj: TObject);
var
  Idx: Integer;
begin
  FCS.Enter;
  try

    //Поиск группы
    Idx := IndexOf(EventName);

    //Группа не найдена
    if Idx = -1 then Exit;

    //Удалить подписчика
    FList[Idx].Subscribers.Delete(Obj);

    //Проверить на пустую группу
    if FList[Idx].Subscribers.Count = 0 then Delete(Idx);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(Handler: TsgeEventHandler);
var
  i: Integer;
begin
  FCS.Enter;
  try

    i := -1;
    while i < FCount - 1 do
      begin
      Inc(i);

      //Удалить подписчика в группе
      FList[i].Subscribers.Delete(Handler);

      //Проверить на пустую группу
      if FList[i].Subscribers.Count = 0 then
        begin
        Delete(i);
        Dec(i)
        end;
      end;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(Obj: TObject);
var
  i: Integer;
begin
  FCS.Enter;
  try

    i := -1;
    while i < FCount - 1 do
      begin
      Inc(i);

      //Удалить подписчика в группе по объекту
      FList[i].Subscribers.Delete(Obj);

      //Проверить на пустую группу
      if FList[i].Subscribers.Count = 0 then
        begin
        Delete(i);
        Dec(i)
        end;
      end;

  finally
    FCS.Leave;
  end;
end;


end.

