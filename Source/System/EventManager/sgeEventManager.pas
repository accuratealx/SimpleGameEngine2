{
Пакет             Simple Game Engine 2
Файл              sgeEventManager.pas
Версия            1.5
Создан            22.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс системы событий
}
{$Include Defines.inc}

unit sgeEventManager;

{$mode objfpc}{$H+}

interface

uses
  sgeErrors, sgeCriticalSection, sgeSystemEvent,
  sgeEventBase, sgeEventList, sgeEventSubscriberGroupList, sgeEventSubscriberList;


const
  Object_EventManager = 'EventManager';


type
  //Менеджер событий
  TsgeEventManager = class
  private
    //Классы
    FCS: TsgeCriticalSection;                           //Критическая секция изменения списка
    FEvent: TsgeSystemEvent;                            //Событие добавления объекта

    FEventList: TsgeEventList;                          //Список событий
    FSubscriberGroupList: TsgeEventSubscriberGroupList; //Список подписчиков
    FSubscriberForAll: TsgeEventSubscriberList;         //Подписчики на все события

    FErrorHandler: TsgeErrorHandler;                    //Внешний обрабочик ошибок
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Publish(EventObj: TsgeEventBase);         //Добавить событие в очередь
    procedure DispatchEvents;                           //Метод обработки событий

    property EventList: TsgeEventList read FEventList;
    property SubscriberGroupList: TsgeEventSubscriberGroupList read FSubscriberGroupList;
    property SubscriberForAll: TsgeEventSubscriberList read FSubscriberForAll;

    property ErrorHandler: TsgeErrorHandler read FErrorHandler write FErrorHandler;
  end;



implementation

const
  _UNITNAME = 'EventManager';

  Err_DispatchError = 'DispatchError';


constructor TsgeEventManager.Create;
begin
  FCS := TsgeCriticalSection.Create;
  FEvent := TsgeSystemEvent.Create(True, False);

  FEventList := TsgeEventList.Create(True);
  FSubscriberGroupList := TsgeEventSubscriberGroupList.Create;
  FSubscriberForAll := TsgeEventSubscriberList.Create(True);
end;


destructor TsgeEventManager.Destroy;
begin
  //Поднять флаг изменения списка, если потоки что-то пишут
  FCS.Enter;

  FSubscriberForAll.Free;
  FSubscriberGroupList.Free;
  FEventList.Free;
  FEvent.Free;
  FCS.Free;
end;


procedure TsgeEventManager.Publish(EventObj: TsgeEventBase);
begin
  //Добавить событие
  FEventList.Add(EventObj);

  //Поднять флаг доставки события
  FEvent.Up;
end;


procedure TsgeEventManager.DispatchEvents;
var
  i: Integer;
  EventObj: TsgeEventBase;
  SubscriberList: TsgeEventSubscriberList;
begin
  //Ждём событие поднятия флага
  FEvent.Wait;

  //Рассылаем события
  while FEventList.Count <> 0 do
  begin
    //Получить событие
    EventObj := FEventList.Item[0];


    //Подписчики на все подряд
    //Заблокировать подписчиков
    FSubscriberForAll.Lock;

    for i := 0 to FSubscriberForAll.Count - 1 do
    begin
      //Пропуск неактивных
      if not FSubscriberForAll.Item[i].Enable then
        Continue;

      //Вызвать обработчик
      try
        if FSubscriberForAll.Item[i].Handler(EventObj) = ehrBreak then
          Break;

      except
        on E: EsgeException do
          if Assigned(FErrorHandler)
            then FErrorHandler(sgeCreateErrorString(_UNITNAME, Err_DispatchError, EventObj.Name, E.Message));
      end;
    end;

    //Разблокировать подписчиков
    FSubscriberForAll.Unlock;



    //Подписчики по имени
    //Заблокировать подписчиков
    FSubscriberGroupList.Lock;

    //Найти группу
    SubscriberList := FSubscriberGroupList.Subscribers[EventObj.Name];

    if Assigned(SubscriberList) then
    begin
      //Доставить событие подписчикам
      for i := 0 to SubscriberList.Count - 1 do
      begin
        //Пропуск неактивных
        if not SubscriberList.Item[i].Enable then
          Continue;

        //Вызвать обработчик
        try
          if SubscriberList.Item[i].Handler(EventObj) = ehrBreak then
            Break;

        except
          on E: EsgeException do
            if Assigned(FErrorHandler)
              then FErrorHandler(sgeCreateErrorString(_UNITNAME, Err_DispatchError, EventObj.Name, E.Message));
        end;
      end;
    end;

    //Разблокировать подписчиков
    FSubscriberGroupList.Unlock;

    //Удалить объект события
    FEventList.Delete(0);
  end;
end;



end.
