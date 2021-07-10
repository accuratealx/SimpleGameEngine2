{
Пакет             Simple Game Engine 2
Файл              sgeEventManager.pas
Версия            1.0
Создан            22.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс системы событий
}
{$Include Defines.inc}

unit sgeEventManager;

{$mode objfpc}{$H+}

interface

uses
  sgeEventSubscriberList, sgeEventList, sgeEventBase, sgeEventHandlerList,
  sgeCriticalSection, sgeSystemEvent;

const
  Object_EventManager = 'EventManager';


type
  //Подписчик события
  TsgeEventHandler = procedure(EventObj: TsgeEventBase) of object;


  //Менеджер событий
  TsgeEventManager = class
  private
    //Классы
    FCritSection: TsgeCriticalSection;                                      //Критическая секция изменения списка
    FEvent: TsgeSystemEvent;                                                //Событие доставки событий

    FEventList: TsgeEventList;                                              //Списиок событий
    FSubscriberList: TsgeEventSubscriberList;                               //Списиок подписчиков
  public
    constructor Create;
    destructor  Destroy; override;

    //Управление списками
    procedure ClearEvents;                                                  //Очистить список событий
    procedure ClearSubscribers;                                             //Очистить список подписчиков

    //Публикация
    procedure Publish(EventName: String; EventObj: TsgeEventBase = nil);    //Добавить событие в очередь

    //Подписка
    procedure Subscribe(EventName: String; EventProc: TsgeEventHandler);    //Подписка обработчика на событие

    //Отписка
    procedure UnSubscribe(EventName: String; EventProc: TsgeEventHandler);  //Отписаться от события
    procedure UnSubscribe(EventName: String; Obj: TObject);                 //Отписаться от события
    procedure UnSubscribe(Obj: TObject);                                    //Отписаться от событий одним объектом

    //Доставка
    procedure DispatchEvents;                                               //Метод обработки событий
  end;



implementation


constructor TsgeEventManager.Create;
begin
  FCritSection := TsgeCriticalSection.Create;
  FEvent := TsgeSystemEvent.Create(True, False);

  FEventList := TsgeEventList.Create;
  FSubscriberList := TsgeEventSubscriberList.Create;
end;


destructor TsgeEventManager.Destroy;
begin
  FCritSection.Enter;     //Поднять флаг изменения списка, если потоки что-то пишут

  FSubscriberList.Free;
  FEventList.Free;

  FEvent.Free;
  FCritSection.Free;
end;


procedure TsgeEventManager.ClearEvents;
begin
  FCritSection.Enter;
  FEventList.Clear;
  FCritSection.Leave;
end;


procedure TsgeEventManager.ClearSubscribers;
begin
  FCritSection.Enter;
  FSubscriberList.Clear;
  FCritSection.Leave;
end;


procedure TsgeEventManager.Publish(EventName: String; EventObj: TsgeEventBase);
begin
  //Добавить событие
  FCritSection.Enter;
  FEventList.Add(EventName, EventObj);
  FCritSection.Leave;

  //Поднять флаг доставки события
  FEvent.Up;
end;


procedure TsgeEventManager.Subscribe(EventName: String; EventProc: TsgeEventHandler);
begin
  FCritSection.Enter;
  FSubscriberList.Add(EventName, EventProc);
  FCritSection.Leave;
end;


procedure TsgeEventManager.UnSubscribe(EventName: String; EventProc: TsgeEventHandler);
begin
  FCritSection.Enter;
  FSubscriberList.Delete(EventName, EventProc);
  FCritSection.Leave;
end;


procedure TsgeEventManager.UnSubscribe(EventName: String; Obj: TObject);
begin
  FCritSection.Enter;
  FSubscriberList.Delete(EventName, Obj);
  FCritSection.Leave;
end;


procedure TsgeEventManager.UnSubscribe(Obj: TObject);
begin
  FCritSection.Enter;
  FSubscriberList.Delete(Obj);
  FCritSection.Leave;
end;


procedure TsgeEventManager.DispatchEvents;
var
  i, c: Integer;
  EventName: String;
  EventObj: TsgeEventBase;
  HandlerList: TsgeEventHandlerList;
begin
  //Ждём событие поднятия флага
  FEvent.Wait;

  //Рассылаем события
  while FEventList.Count <> 0 do
    begin
    //Определить тип события
    EventName := FEventList.Item[0].Name;
    EventObj := FEventList.Item[0].Obj;

    //Получить указатель на список подписчиков
    HandlerList := FSubscriberList.HandlerList[EventName];

    //Проверить наличие списка
    if HandlerList <> nil then
      begin
      //Отослать слушателям объект
      c := HandlerList.Count - 1;
      for i := 0 to c do
        HandlerList.Item[i](EventObj);  //Тут бы проверку на возникновение ошибки и протоколирование
      end;

    //Удалить объект события
    FCritSection.Enter;
    FEventList.Delete(0);
    FCritSection.Leave;
    end;
end;


end.

