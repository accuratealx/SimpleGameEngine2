{
Пакет             Simple Game Engine 2
Файл              TsgeSceneBase.pas
Версия            1.0
Создан            14.12.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Базовый класс сцены
}
//{$Include Defines.inc}

unit sgeSceneBase;

{$mode ObjFPC}{$H+}

interface

uses
  sgeErrorManager, sgeEventManager, sgeEventBase, sgeEventSubscriber;

type
  TsgeSceneBase = class
  private
    FErrorManager: TsgeErrorManager;
    FEventManager: TsgeEventManager;

    FSubscribe: TsgeEventSubscriber;

    procedure SetEnableSubscriber(AEnable: Boolean);
    function  GetEnableSubscriber: Boolean;
  protected
    procedure Prepare; virtual;
    procedure Done; virtual;

    function EventSubscriber(EventObj: TsgeEventBase): TsgeEventHandlerResult; virtual;
  public
    procedure Activate; virtual;
    procedure Deactivate; virtual;

    constructor Create;
    destructor Destroy; override;

    property ErrorManager: TsgeErrorManager read FErrorManager;
    property EventManager: TsgeEventManager read FEventManager;
    property EnableSubscribe: Boolean read GetEnableSubscriber write SetEnableSubscriber;
  end;


implementation

uses
  sgeErrors, sgeCorePointerUtils;

const
  _UNITNAME = 'SceneBase';

  Err_CantCreateScene = 'CantCreateScene';


procedure TsgeSceneBase.SetEnableSubscriber(AEnable: Boolean);
begin
  FSubscribe.Enable := AEnable;
end;


function TsgeSceneBase.GetEnableSubscriber: Boolean;
begin
  Result := FSubscribe.Enable;
end;


procedure TsgeSceneBase.Prepare;
begin
  //Пользователький конструктор
end;


procedure TsgeSceneBase.Done;
begin
  //Пользовательский деструктор
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

    //Пользовательская инициализация
    Prepare;

    //Подписка на все события
    FSubscribe := FEventManager.SubscriberForAll.Add(@EventSubscriber);

  except
    on E: EsgeException do
      EsgeException.Create(_UNITNAME, Err_CantCreateScene, '', E.Message);
  end;
end;


destructor TsgeSceneBase.Destroy;
begin
  //Отписка от всех событий
  FEventManager.SubscriberForAll.Delete(@EventSubscriber);

  //Пользовательская финализация
  Done;
end;


end.

