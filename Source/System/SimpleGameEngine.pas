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
  sgeErrorManager, sgeNamedObjectList, sgeExtensionList, sgeEventManager, sgeEventBase, sgeEventWindow,
  sgeExtensionWindow, sgeExtensionGraphic, sgeExtensionPackList, sgeExtensionFileSystem, sgeExtensionShell,
  sgeExtensionResourceList, sgeExtensionStartParameters, sgeExtensionSound, sgeExtensionControllers,
  sgeExtensionVariables, sgeExtensionKeyCommand;


const
  Object_SGE = 'ObjectSGE';

  //Имена событий
  Event_KernelCreate  = 'Kernel.Create';
  Event_KernelDestroy = 'Kernel.Destroy';


type
  //Основной класс движка
  TSimpleGameEngine = class
  private
    //Параметры
    FWorking: Boolean;                                              //Флаг работы
    FDebug: Boolean;                                                //Режим отладки

    //Классы
    FObjectList: TsgeNamedObjectList;                               //Список объектов
    FErrorManager: TsgeErrorManager;                                //Обработчик ошибок
    FEventManager: TsgeEventManager;                                //Обработчик событий
    FExtensionList: TsgeExtensionList;                              //Список расширений

    //Расширения
    FExtensionStartParameters: TsgeExtensionStartParameters;        //Расширение: Стартовые параметры
    FExtensionVariables: TsgeExtensionVariables;                    //Расширение: Переменные
    FExtensionWindow: TsgeExtensionWindow;                          //Расширение: Окно
    FExtensionGraphic: TsgeExtensionGraphic;                        //Расширение: Графика
    FExtensionPackFiles: TsgeExtensionPackList;                     //Расширение: Файловые архивы
    FExtensionFileSystem: TsgeExtensionFileSystem;                  //Расширение: Файловая система
    FExtensionResourceList: TsgeExtensionResourceList;              //Расширение: Список ресурсов
    FExtensionSound: TsgeExtensionSound;                            //Расширение: Звуковая система
    FExtensionControllers: TsgeExtensionControllers;                //Расширение: Контроллеры
    FExtensionShell: TsgeExtensionShell;                            //Расширение: Оболочка
    FExtensionKeyCommand: TsgeExtensionKeyCommand;                  //Расширение: Команды на клавишах

    //Свойства
    procedure SetDebug(ADebug: Boolean);

    //Вспомогательные методы
    procedure ProcessSystemStartParameters;                         //Обработать стартовые параметры

    //Обработчики событий
    procedure RegisterEventHandlers;                                //Подписать системные обработчики событий
    function  EventWindowClose(Obj: TsgeEventBase): Boolean;        //Закрытие окна
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AttachDefaultCommand;                                 //Привязать на кнопки стандартные действия
    procedure ScreenShot(FileName: String = '');                    //Сохранить снимок окна в BMP

    procedure Run;                                                  //Запустить приложение
    procedure Stop;                                                 //Остановить приложение
    function  CloseWindow: Boolean; virtual;                        //Возможность закрытия окна

    //Свойства
    property Debug: Boolean read FDebug write SetDebug;

    //Объекты
    property ErrorManager: TsgeErrorManager read FErrorManager;
    property ObjectList: TsgeNamedObjectList read FObjectList;
    property EventManager: TsgeEventManager read FEventManager;
    property ExtensionList: TsgeExtensionList read FExtensionList;

    //Расширения
    property ExtStartParameters: TsgeExtensionStartParameters read FExtensionStartParameters;
    property ExtVariables: TsgeExtensionVariables read FExtensionVariables;
    property ExtWindow: TsgeExtensionWindow read FExtensionWindow;
    property ExtGraphic: TsgeExtensionGraphic read FExtensionGraphic;
    property ExtPackFiles: TsgeExtensionPackList read FExtensionPackFiles;
    property ExtFileSystem: TsgeExtensionFileSystem read FExtensionFileSystem;
    property ExtResourceList: TsgeExtensionResourceList read FExtensionResourceList;
    property ExtSound: TsgeExtensionSound read FExtensionSound;
    property ExtControllers: TsgeExtensionControllers read FExtensionControllers;
    property ExtShell: TsgeExtensionShell read FExtensionShell;
    property ExtKeyCommand: TsgeExtensionKeyCommand read FExtensionKeyCommand;
  end;


implementation

uses
  sgeErrors, sgeKeys, sgeMemoryStream,
  sgeOSPlatform, sgeDateUtils, sgeFileUtils, sgeShellCommands, sgeVariables;


const
  _UNITNAME = 'SimpleGameEngine';

  Err_CantCreateSimpleGameEngine = 'CantCreateSimpleGameEngine';
  Err_CantCreateScreenShot = 'CantCreateScreenShot';

  Ext_Journal = 'Journal';

  spDebug = 'Debug';



procedure TSimpleGameEngine.SetDebug(ADebug: Boolean);
begin
  FDebug := ADebug;

  //Установить запись в журнал
  FErrorManager.WriteToJournal := FDebug;
end;


procedure TSimpleGameEngine.ProcessSystemStartParameters;
begin
  if FExtensionStartParameters.Parameters.Exist[spDebug] then SetDebug(True);
end;


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
  JFile: String;
begin
  //Параметры
  FWorking := True;

  //Уникальное имя журнала
  JFile := sgeGetApplicationDirectory + 'Journals\' + sgeFormatDateTime('yyyy.mm.dd-hh.nn.ss', sgeNow) + '.' + Ext_Journal;

  //Классы
  FErrorManager := TsgeErrorManager.Create(JFile);                  //Менеджер ошибок
  FObjectList := TsgeNamedObjectList.Create;                        //Список объектов
  FExtensionList := TsgeExtensionList.Create;                       //Список расширений
  FEventManager := TsgeEventManager.Create;                         //Менеджер событий
  FEventManager.ErrorHandler := @FErrorManager.ProcessError;

  //Добавить классы в список объектов
  FObjectList.Add(Object_SGE, Self);
  FObjectList.Add(Object_ErrorManager, FErrorManager);
  FObjectList.Add(Object_ObjecttList, FObjectList);
  FObjectList.Add(Object_EventManager, FEventManager);
  FObjectList.Add(Object_ExtensionList, FExtensionList);

  try
    //Создать расширения
    FExtensionVariables := TsgeExtensionVariables.Create(FObjectList);              //Переменные

    FExtensionStartParameters := TsgeExtensionStartParameters.Create(FObjectList);  //Стартовые параметры
    ProcessSystemStartParameters;                                                   //Обработать системные стартовые параметры

    FExtensionWindow := TsgeExtensionWindow.Create(FObjectList);                    //Окно
    FExtensionGraphic := TsgeExtensionGraphic.Create(FObjectList);                  //Графика
    FExtensionPackFiles :=   TsgeExtensionPackList.Create(FObjectList);             //Файловые архивы
    FExtensionFileSystem := TsgeExtensionFileSystem.Create(FObjectList);            //Файловая система
    FExtensionSound := TsgeExtensionSound.Create(FObjectList);                      //Звуковая система
    FExtensionResourceList := TsgeExtensionResourceList.Create(FObjectList);        //Список ресурсов
    FExtensionControllers := TsgeExtensionControllers.Create(FObjectList);          //Контроллеры
    FExtensionShell := TsgeExtensionShell.Create(FObjectList);                      //Оболочка
    FExtensionKeyCommand := TsgeExtensionKeyCommand.Create(FObjectList);            //Команда на кнопках

  except
    //Ошибка инициализации движка
    on E: EsgeException do
      begin
      FErrorManager.ShowMessage := True;
      FErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_CantCreateSimpleGameEngine, '', E.Message));
      Halt;
      end;
  end;

  //Зарегестрировать функции оболочки
  sgeShellCommands_Init(Self);

  //Зарегестрировать системные переменные
  sgeVariables_Init(Self);

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


procedure TSimpleGameEngine.AttachDefaultCommand;
begin
  //Открыть оболочку на тильду
  FExtensionKeyCommand.Keyboard.Key[keyEscape].Down := 'System.Stop';
  FExtensionKeyCommand.Keyboard.Key[keyTilde].Down := 'Variable.Set Shell.Enable On';
end;


procedure TSimpleGameEngine.ScreenShot(FileName: String);
const
  ShotExt = '.bmp';
var
  s: String;
  MS: TsgeMemoryStream;
begin
  //Подготовить имя файла
  if FileName = '' then s := sgeGetUniqueFileName else s := FileName;
  s := s + ShotExt;

  //Подготовить каталог
  sgeForceDirectories(FExtensionFileSystem.ScreenshotDir);

  MS := TsgeMemoryStream.Create;
  try
    try
      //Запросить снимок экрана
      FExtensionGraphic.Graphic.ScreenShot(MS);

      //Сохранить в файл
      MS.SaveToFile(FExtensionFileSystem.ScreenshotDir + s);
    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantCreateScreenShot, '', E.Message);
    end;

  finally
    MS.Free;
  end;
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

