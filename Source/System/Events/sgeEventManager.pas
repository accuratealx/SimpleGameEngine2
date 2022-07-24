{
Пакет             Simple Game Engine 2
Файл              sgeEventManager.pas
Версия            1.3
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

    FErrorHandler: TsgeErrorHandler;                                        //Внешний обрабочик ошибок
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Publish(EventObj: TsgeEventBase = nil);                       //Добавить событие в очередь
    procedure DispatchEvents;                                               //Метод обработки событий

    property EventList: TsgeEventList read FEventList;
    property SubscriberGroupList: TsgeEventSubscriberGroupList read FSubscriberGroupList;

    property ErrorHandler: TsgeErrorHandler read FErrorHandler write FErrorHandler;
  end;



implementation

uses
  sgeEventSubscriberList;


const
  _UNITNAME = 'EventManager';

  Err_DispatchError = 'DispatchError';


constructor TsgeEventManager.Create;
begin
  FCS := TsgeCriticalSection.Create;
  FEvent := TsgeSystemEvent.Create(True, False);

  FEventList := TsgeEventList.Create(True);
  FSubscriberGroupList := TsgeEventSubscriberGroupList.Create(True);
end;


destructor TsgeEventManager.Destroy;
begin
  //Поднять флаг изменения списка, если потоки что-то пишут
  FCS.Enter;

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
  Idx, i: Integer;
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

    //Заблокировать подписчиков
    FSubscriberGroupList.Lock;

    //Найти группу
    Idx := FSubscriberGroupList.IndexOf(EventObj.Name);

    if Idx <> -1 then
    begin
      //Ссылка на список
      SubscriberList := FSubscriberGroupList.Item[Idx].Subscribers;

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
              then FErrorHandler(sgeCreateErrorString(_UNITNAME, Err_DispatchError, '', E.Message));
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
