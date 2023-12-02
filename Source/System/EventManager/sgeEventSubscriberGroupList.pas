{
Пакет             Simple Game Engine 2
Файл              sgeEventSubscriberGroupList.pas
Версия            1.6
Создан            12.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка групп подписчиков
}
{$Include Defines.inc}

unit sgeEventSubscriberGroupList;

{$mode objfpc}{$H+}

interface

uses
  Contnrs, sgeCriticalSection,
  sgeEventBase, sgeEventSubscriber, sgeEventSubscriberList;


type
  TsgeEventSubscriberGroupList = class
  private
    FTable: TFPHashObjectList;
    FCS: TsgeCriticalSection;

    function GetSubscribers(EventName: String): TsgeEventSubscriberList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Lock;
    procedure Unlock;

    function  Subscribe(EventName: ShortString; Handler: TsgeEventHandler; Priority: Word = 0; Enable: Boolean = True): TsgeEventSubscriber;

    procedure UnSubscribe(EventName: ShortString; Handler: TsgeEventHandler); //Отписаться от события по имени и обработчику
    procedure UnSubscribe(EventName: ShortString; Obj: TObject);              //Отписаться от события по имени и объекту
    procedure UnSubscribe(Handler: TsgeEventHandler);                         //Отписаться от событий по обработчику
    procedure UnSubscribe(Obj: TObject);                                      //Отписаться от событий по объекту

    //Получить список подписчиков, если подписчиков нет, то возвращается nil
    property Subscribers[EventName: String]: TsgeEventSubscriberList read GetSubscribers;
  end;


implementation


function TsgeEventSubscriberGroupList.GetSubscribers(EventName: String): TsgeEventSubscriberList;
begin
  Result := TsgeEventSubscriberList(FTable.Find(EventName));
end;


constructor TsgeEventSubscriberGroupList.Create;
begin
  FCS := TsgeCriticalSection.Create;
  FTable := TFPHashObjectList.Create(True);
end;


destructor TsgeEventSubscriberGroupList.Destroy;
begin
  FTable.Free;
  FCS.Free;
end;


procedure TsgeEventSubscriberGroupList.Lock;
begin
  FCS.Enter;
end;


procedure TsgeEventSubscriberGroupList.Unlock;
begin
  FCS.Leave;
end;


function TsgeEventSubscriberGroupList.Subscribe(EventName: ShortString; Handler: TsgeEventHandler; Priority: Word; Enable: Boolean): TsgeEventSubscriber;
var
  SubscribersList: TsgeEventSubscriberList;
begin
  FCS.Enter;
  try
    //Найти список подпсичиков
    SubscribersList := GetSubscribers(EventName);

    //Если нет списка, то создать
    if not Assigned(SubscribersList) then
    begin
      SubscribersList := TsgeEventSubscriberList.Create(True);
      FTable.Add(EventName, SubscribersList);
    end;

    //Добавить подписчика
    Result := SubscribersList.Add(Handler, Priority, Enable);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(EventName: ShortString; Handler: TsgeEventHandler);
var
  SubscribersList: TsgeEventSubscriberList;
begin
  FCS.Enter;
  try
    //Найти список подпсичиков
    SubscribersList := GetSubscribers(EventName);

    //Группа не найдена, нечего отписывать
    if not Assigned(SubscribersList) then
      Exit;

    //Удалить подписчика
    SubscribersList.Delete(Handler);

    //Проверить на пустую группу
    if SubscribersList.Count = 0 then
      FTable.Remove(SubscribersList);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(EventName: ShortString; Obj: TObject);
var
  SubscribersList: TsgeEventSubscriberList;
begin
  FCS.Enter;
  try
    //Найти список подпсичиков
    SubscribersList := GetSubscribers(EventName);

    //Группа не найдена, нечего отписывать
    if not Assigned(SubscribersList) then
      Exit;

    //Удалить подписчика
    SubscribersList.Delete(Obj);

    //Проверить на пустую группу
    if SubscribersList.Count = 0 then
      FTable.Remove(SubscribersList);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(Handler: TsgeEventHandler);
var
  SubscribersList: TsgeEventSubscriberList;
  i: Integer;
begin
  FCS.Enter;
  try
    for i := FTable.Count -1 downto 0 do
    begin
      //Ссылка на список подписчиков
      SubscribersList := TsgeEventSubscriberList(FTable.Items[i]);

      //Удалить подписчика
      SubscribersList.Delete(Handler);

      //Проверить на пустую группу
      if SubscribersList.Count = 0 then
        FTable.Remove(SubscribersList);
    end;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(Obj: TObject);
var
  SubscribersList: TsgeEventSubscriberList;
  i: Integer;
begin
  FCS.Enter;
  try
    for i := FTable.Count -1 downto 0 do
    begin
      //Ссылка на список подписчиков
      SubscribersList := TsgeEventSubscriberList(FTable.Items[i]);

      //Удалить подписчика
      SubscribersList.Delete(Obj);

      //Проверить на пустую группу
      if SubscribersList.Count = 0 then
        FTable.Remove(SubscribersList);
    end;

  finally
    FCS.Leave;
  end;
end;



end.

