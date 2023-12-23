{
Пакет             Simple Game Engine 2
Файл              TsgeSceneBase.pas
Версия            1.0
Создан            14.12.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Базовый класс сцены
}
{$Include Defines.inc}

unit sgeSceneBase;

{$mode ObjFPC}{$H+}

interface

uses
  sgeErrorManager, sgeEventManager, sgeEventBase, sgeEventSubscriber;

type
  TsgeSceneBase = class
  private
    FErrorManager: TsgeErrorManager;                                //Ссылка на обработчик ошибок
    FEventManager: TsgeEventManager;                                //Ссылка на менеджер событий

    FVisible: Boolean;                                              //Флаг видимости
    FEnableSubscribers: Boolean;                                    //Флаг активности подписчиков на события
    FSubscribeAll: TsgeEventSubscriber;                             //Общий подписчик

  protected
    procedure Prepare; virtual;
    procedure Done; virtual;

    procedure SetVisible(AVisible: Boolean); virtual;
    procedure SetEnableSubscribers(AEnable: Boolean); virtual;

    procedure SubscribeEvents; virtual;
    procedure UnSubscribeEvents; virtual;
    function  EventSubscriber(EventObj: TsgeEventBase): TsgeEventHandlerResult; virtual;
  public
    procedure Activate; virtual;
    procedure Deactivate; virtual;

    constructor Create;
    destructor Destroy; override;

    property ErrorManager: TsgeErrorManager read FErrorManager;
    property EventManager: TsgeEventManager read FEventManager;
    property Visible: Boolean read FVisible write SetVisible;
    property EnableSubscribes: Boolean read FEnableSubscribers write SetEnableSubscribers;
  end;


implementation

uses
  sgeErrors, sgeCorePointerUtils;

const
  _UNITNAME = 'SceneBase';

  Err_CantCreateScene = 'CantCreateScene';


procedure TsgeSceneBase.SetEnableSubscribers(AEnable: Boolean);
begin
  //Сохранить новое значение
  FEnableSubscribers := AEnable;

  //Изменить активность подписчика
  FSubscribeAll.Enable := AEnable;
end;


procedure TsgeSceneBase.Prepare;
begin
  //Пользователький конструктор
end;


procedure TsgeSceneBase.Done;
begin
  //Пользовательский деструктор
end;


procedure TsgeSceneBase.SetVisible(AVisible: Boolean);
begin
  FVisible := AVisible;
end;


procedure TsgeSceneBase.SubscribeEvents;
begin
  //Пользовательская подписка на события
end;


procedure TsgeSceneBase.UnSubscribeEvents;
begin
  //Пользовательская отписка на событий
end;


function TsgeSceneBase.EventSubscriber(EventObj: TsgeEventBase): TsgeEventHandlerResult;
begin
  //Обработчик всех событий системы
  Result := ehrNormal;
end;


procedure TsgeSceneBase.Activate;
begin
  //Активация сцены
end;


procedure TsgeSceneBase.Deactivate;
begin
  //Деактивация сцены
end;


constructor TsgeSceneBase.Create;
begin
  try
    //Поиск указателей
    FErrorManager := sgeCorePointer_GetErrorManager;
    FEventManager := sgeCorePointer_GetEventManager;

    //Параметры
    FVisible := False;
    FEnableSubscribers := False;

    //Пользовательская инициализация
    Prepare;

    //Пользовательская подписка
    SubscribeEvents;

    //Подписка на все события
    FSubscribeAll := FEventManager.SubscriberForAll.Add(@EventSubscriber, 0, False);

  except
    on E: EsgeException do
      EsgeException.Create(_UNITNAME, Err_CantCreateScene, '', E.Message);
  end;
end;


destructor TsgeSceneBase.Destroy;
begin
  //Пользовательская отписка
  UnSubscribeEvents;

  //Отписка от всех событий
  FEventManager.SubscriberForAll.Delete(@EventSubscriber);

  //Пользовательская финализация
  Done;
end;


end.

