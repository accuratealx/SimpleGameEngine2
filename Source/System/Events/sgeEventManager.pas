{
Пакет             Simple Game Engine 2
Файл              sgeEventManager.pas
Версия            1.1
Создан            22.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс системы событий
}
{$Include Defines.inc}

unit sgeEventManager;

{$mode objfpc}{$H+}

interface

uses
  sgeCriticalSection, sgeSystemEvent,
  sgeEventBase, sgeEventList, sgeEventSubscriberGroupList;


const
  Object_EventManager = 'EventManager';


type
  //Менеджер событий
  TsgeEventManager = class
  private
    //Классы
    FCS: TsgeCriticalSection;                                               //Критическая секция изменения списка
    FEvent: TsgeSystemEvent;                                                //Событие добавления объекта

    FEventList: TsgeEventList;                                              //Список событий
    FSubscriberGroupList: TsgeEventSubscriberGroupList;                     //Список подписчиков
  public
    constructor Create;
    destructor  Destroy; override;

    //Управление списками
    procedure ClearEvents;                                                  //Очистить список событий
    procedure ClearSubscribers;                                             //Очистить список подписчиков

    //Публикация
    procedure Publish(EventName: String; EventObj: TsgeEventBase = nil);    //Добавить событие в очередь

    //Подписка
    procedure Subscribe(EventName: String; Handler: TsgeEventHandler; Priority: Word = 0; Enable: Boolean = True);

    //Отписка
    procedure UnSubscribe(EventName: String; Handler: TsgeEventHandler);    //Отписаться от события по имени и обработчику
    procedure UnSubscribe(EventName: String; Obj: TObject);                 //Отписаться от события по имени и объекту
    procedure UnSubscribe(Handler: TsgeEventHandler);                       //Отписаться от событий по обработчику
    procedure UnSubscribe(Obj: TObject);                                    //Отписаться от событий по объекту

    //Доставка
    procedure DispatchEvents;                                               //Метод обработки событий
  end;



implementation

uses
  sgeEventSubscriberList;


constructor TsgeEventManager.Create;
begin
  FCS := TsgeCriticalSection.Create;
  FEvent := TsgeSystemEvent.Create(True, False);

  FEventList := TsgeEventList.Create;
  FSubscriberGroupList := TsgeEventSubscriberGroupList.Create;
end;


destructor TsgeEventManager.Destroy;
begin
  FCS.Enter;     //Поднять флаг изменения списка, если потоки что-то пишут

  FSubscriberGroupList.Free;
  FEventList.Free;

  FEvent.Free;
  FCS.Free;
end;


procedure TsgeEventManager.ClearEvents;
begin
  FCS.Enter;
  FEventList.Clear;
  FCS.Leave;
end;


procedure TsgeEventManager.ClearSubscribers;
begin
  FCS.Enter;
  FSubscriberGroupList.Clear;
  FCS.Leave;
end;


procedure TsgeEventManager.Publish(EventName: String; EventObj: TsgeEventBase);
begin
  //Добавить событие
  FCS.Enter;
  FEventList.Add(EventName, EventObj);
  FCS.Leave;

  //Поднять флаг доставки события
  FEvent.Up;
end;


procedure TsgeEventManager.Subscribe(EventName: String; Handler: TsgeEventHandler; Priority: Word; Enable: Boolean);
begin
  FCS.Enter;
  FSubscriberGroupList.Subscribe(EventName, Handler, Priority, Enable);
  FCS.Leave;
end;


procedure TsgeEventManager.UnSubscribe(EventName: String; Handler: TsgeEventHandler);
begin
  FCS.Enter;
  FSubscriberGroupList.UnSubscribe(EventName, Handler);
  FCS.Leave;
end;


procedure TsgeEventManager.UnSubscribe(EventName: String; Obj: TObject);
begin
  FCS.Enter;
  FSubscriberGroupList.UnSubscribe(EventName, Obj);
  FCS.Leave;
end;


procedure TsgeEventManager.UnSubscribe(Handler: TsgeEventHandler);
begin
  FCS.Enter;
  FSubscriberGroupList.UnSubscribe(Handler);
  FCS.Leave;
end;


procedure TsgeEventManager.UnSubscribe(Obj: TObject);
begin
  FCS.Enter;
  FSubscriberGroupList.UnSubscribe(Obj);
  FCS.Leave;
end;


procedure TsgeEventManager.DispatchEvents;
var
  Idx, i: Integer;
  EventName: String;
  EventObj: TsgeEventBase;
  SubscriberList: TsgeEventSubscriberList;
begin
  //Ждём событие поднятия флага
  FEvent.Wait;

  //Рассылаем события
  while FEventList.Count <> 0 do
    begin
    //Определить параметры события
    EventName := FEventList.Item[0].Name;
    EventObj := FEventList.Item[0].Obj;

    //Найти группу
    Idx := FSubscriberGroupList.IndexOf(EventName);

    if Idx <> -1 then
      begin
      //Ссылка на список
      SubscriberList := FSubscriberGroupList.Item[Idx].Subscribers;

      //Доставить событие подписчикам
      for i := 0 to SubscriberList.Count - 1 do
        try
          //Пропуск неактивных
          if not SubscriberList.Item[i].Enable then Continue;

          //Вызвать обработчик
          if SubscriberList.Item[i].Handler(EventObj) then Break;
        except
          //Обработать ошибку
          //Отослать в ErrorManager
        end;
      end;

    //Удалить объект события
    FCS.Enter;
    FEventList.Delete(0);
    FCS.Leave;
    end;
end;


end.

