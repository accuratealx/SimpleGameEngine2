{
Пакет             Simple Game Engine 2
Файл              SimpleGameEngine.pas
Версия            1.0
Создан            28.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Ядро движка
}
{$Include Defines.inc}

unit SimpleGameEngine;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeErrorManager, sgeNamedObjectList, sgeExtensionList, sgeEventManager, sgeEventBase, sgeEventWindow,
  sgeExtensionWindow, sgeExtensionGraphic, sgeExtensionPackList, sgeExtensionFileSystem, sgeExtensionShell,
  sgeExtensionResourceList, sgeExtensionStartParameters, sgeExtensionSound, sgeExtensionControllers;


const
  //Имена событий
  Event_KernelCreate  = 'Kernel.Create';
  Event_KernelDestroy = 'Kernel.Destroy';


type
  //Основной класс движка
  TSimpleGameEngine = class
  private
    FCursorPos: TsgeIntPoint;
    //Параметры
    FWorking: Boolean;                                              //Флаг работы

    //Классы
    FObjectList: TsgeNamedObjectList;                               //Список объектов
    FErrorManager: TsgeErrorManager;                                //Обработчик ошибок
    FEventManager: TsgeEventManager;                                //Обработчик событий
    FExtensionList: TsgeExtensionList;                              //Список расширений

    //Расширения
    FExtensionStartParameters: TsgeExtensionStartParameters;        //Расширение: Стартовые параметры
    FExtensionWindow: TsgeExtensionWindow;                          //Расширение: Окно
    FExtensionGraphic: TsgeExtensionGraphic;                        //Расширение: Графика
    FExtensionPackFiles: TsgeExtensionPackList;                     //Расширение: Файловые архивы
    FExtensionFileSystem: TsgeExtensionFileSystem;                  //Расширение: Файловая система
    FExtensionResourceList: TsgeExtensionResourceList;              //Расширение: Список ресурсов
    FExtensionSound: TsgeExtensionSound;                            //Расширение: Звуковая система
    FExtensionControllers: TsgeExtensionControllers;                //Расширение: Контроллеры
    FExtensionShell: TsgeExtensionShell;                            //Расширение: Оболочка

    //Обработчики событий
    procedure RegisterEventHandlers;                                //Подписать системные обработчики событий
    function  EventWindowClose(Obj: TsgeEventBase): Boolean;        //Закрытие окна

  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure Run;                                                  //Запустить приложение
    procedure Stop;                                                 //Остановить приложение
    function  CloseWindow: Boolean; virtual;                        //Возможность закрытия окна

    //Свойства
    property CursorPos: TsgeIntPoint read FCursorPos;

    //Объекты
    property ErrorManager: TsgeErrorManager read FErrorManager;
    property ObjectList: TsgeNamedObjectList read FObjectList;
    property EventManager: TsgeEventManager read FEventManager;
    property ExtensionList: TsgeExtensionList read FExtensionList;

    //Расширения
    property ExtStartParameters: TsgeExtensionStartParameters read FExtensionStartParameters;
    property ExtWindow: TsgeExtensionWindow read FExtensionWindow;
    property ExtGraphic: TsgeExtensionGraphic read FExtensionGraphic;
    property ExtPackFiles: TsgeExtensionPackList read FExtensionPackFiles;
    property ExtFileSystem: TsgeExtensionFileSystem read FExtensionFileSystem;
    property ExtResourceList: TsgeExtensionResourceList read FExtensionResourceList;
    property ExtSound: TsgeExtensionSound read FExtensionSound;
    property ExtControllers: TsgeExtensionControllers read FExtensionControllers;
    property ExtShell: TsgeExtensionShell read FExtensionShell;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;


const
  _UNITNAME = 'SimpleGameEngine';

  Err_CantCreateSimpleGameEngine = 'CantCreateSimpleGameEngine';


procedure TSimpleGameEngine.RegisterEventHandlers;
begin
  FEventManager.Subscribe(Event_WindowClose, @EventWindowClose);
end;


function TSimpleGameEngine.EventWindowClose(Obj: TsgeEventBase): Boolean;
begin
  if CloseWindow then Stop;
end;


constructor TSimpleGameEngine.Create;
var
  s: String;
begin
  //Параметры
  FWorking := True;

  //Классы
  FErrorManager := TsgeErrorManager.Create;                         //Менеджер ошибок
  FObjectList := TsgeNamedObjectList.Create;                        //Список объектов
  FExtensionList := TsgeExtensionList.Create;                       //Список расширений
  FEventManager := TsgeEventManager.Create;                         //Менеджер событий

  //Добавить классы в список объектов
  FObjectList.Add(Object_ErrorManager, FErrorManager);
  FObjectList.Add(Object_ObjecttList, FObjectList);
  FObjectList.Add(Object_EventManager, FEventManager);
  FObjectList.Add(Object_ExtensionList, FExtensionList);

  try
    //Создать расширения
    FExtensionStartParameters := TsgeExtensionStartParameters.Create(FObjectList);  //Стартовые параметры
    FExtensionWindow := TsgeExtensionWindow.Create(FObjectList);                    //Окно
    FExtensionGraphic := TsgeExtensionGraphic.Create(FObjectList);                  //Графика
    FExtensionPackFiles :=   TsgeExtensionPackList.Create(FObjectList);             //Файловые архивы
    FExtensionFileSystem := TsgeExtensionFileSystem.Create(FObjectList);            //Файловая система
    FExtensionSound := TsgeExtensionSound.Create(FObjectList);                      //Звуковая система
    FExtensionResourceList := TsgeExtensionResourceList.Create(FObjectList);        //Список ресурсов
    FExtensionControllers := TsgeExtensionControllers.Create(FObjectList);          //Контроллеры
    FExtensionShell := TsgeExtensionShell.Create(FObjectList);                      //Оболочка

  except
    //Ошибка инициализации движка
    on E: EsgeException do
      begin
      s := sgeCreateErrorString(_UNITNAME, Err_CantCreateSimpleGameEngine, '', E.Message);
      FErrorManager.LogError(s);
      FErrorManager.ShowMessage(s);
      Halt;
      end;
  end;


  //Зарегестрировать системные обработчики событий
  RegisterEventHandlers;

  //Событие создания
  FEventManager.Publish(Event_KernelCreate, nil);
end;


destructor TSimpleGameEngine.Destroy;
begin
  //Остановить приложение
  Stop;

  //Событие разрушения
  FEventManager.Publish(Event_KernelDestroy, nil);

  //Классы
  FErrorManager.Free;
  FExtensionList.Free;
  FObjectList.Free;
  FEventManager.Free;
end;


procedure TSimpleGameEngine.Run;
begin
  while FWorking do
    FEventManager.DispatchEvents;
end;


procedure TSimpleGameEngine.Stop;
begin
  //Изменить флаг работы
  FWorking := False;

  //Очистить массив элементов графики
  FExtensionGraphic.DrawList.ClearLayers;

  //Разбудить основной поток
  FEventManager.Publish('');
end;


function TSimpleGameEngine.CloseWindow: Boolean;
begin
  Result := True;
end;


end.

