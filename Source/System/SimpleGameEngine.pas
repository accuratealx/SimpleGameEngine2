{
Пакет             Simple Game Engine 2
Файл              SimpleGameEngine.pas
Версия            1.1
Создан            28.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Ядро движка
}
{$Include Defines.inc}

unit SimpleGameEngine;

{$mode objfpc}{$H+}

interface

uses
  sgeErrorManager, sgeNamedObjectList, sgeExtensionList, sgeEventManager, sgeEventBase,
  sgeExtensionWindow, sgeExtensionGraphic, sgeExtensionPackList, sgeExtensionFileSystem, sgeExtensionShell,
  sgeExtensionResourceList, sgeExtensionStartParameters, sgeExtensionSound, sgeExtensionControllers,
  sgeExtensionVariables, sgeExtensionKeyCommand, sgeExtensionTimeEvent, sgeExtensionGUI, sgeExtensionMusicPlayer,
  sgeExtensionCursor;


const
  Object_SGE = 'ObjectSGE';


type
  //Параметры инициализации ядра
  TsgeInitOptions = set of (
    ioSound,                //Поддержка звуков
    ioAutoStartScript,      //Автозапуск скрипта старта Autostart.s
    ioAutoStopScript,       //Автозапуск скрипта остановки AutoStop.s
    ioAttachDefaultKeys,    //Установить привязку клавишь по умолчанию
    ioSupportRunCommand,    //Поддержка выполнения команды в строке параметров Run='Список команд'
    ioFindAndLoadPacks      //Рекурсивный поиск архивов в каталоге программы
    );


const
  InitOptionsAll = [ioSound, ioAutoStartScript, ioAutoStopScript, ioAttachDefaultKeys,
                    ioSupportRunCommand, ioFindAndLoadPacks];

type
  TSimpleGameEngine = class
  private
    //Параметры
    FWorking: Boolean;                                              //Флаг работы
    FDebug: Boolean;                                                //Режим отладки
    FInitOptions: TsgeInitOptions;                                  //Режим инициализации

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
    FExtensionCursor: TsgeExtensionCursor;                          //Расширение: Курсоры
    FExtensionControllers: TsgeExtensionControllers;                //Расширение: Контроллеры
    FExtensionShell: TsgeExtensionShell;                            //Расширение: Оболочка
    FExtensionKeyCommand: TsgeExtensionKeyCommand;                  //Расширение: Команды на клавишах
    FExtensionTimeEvent: TsgeExtensionTimeEvent;                    //Расширение: Таймерные события
    FExtensionGUI: TsgeExtensionGUI;                                //Расширение: GUI
    FExtensionSound: TsgeExtensionSound;                            //Расширение: Звуковая система
    FExtensionMusicPlayer: TsgeExtensionMusicPlayer;                //Расширение: Музыкальный проигрыватель


    //Свойства
    procedure SetDebug(ADebug: Boolean);

    //Вспомогательные методы
    procedure CheckStartParameter_Run;
    procedure CheckStartParameter_Debug;                            //Обработать стартовые параметры
    procedure RunScript(ScriptName: String; Wait: Boolean = False); //Запустить скрипт

    //Обработчики событий
    procedure RegisterEventHandlers;                                //Подписать системные обработчики событий
    procedure UnregisterEventHandlers;                              //Отписать системные обработчики событий
    function  EventWindowClose(Obj: TsgeEventBase): Boolean;        //Закрытие окна
    function  EventTime(Obj: TsgeEventBase): Boolean;               //Таймерное событие
  public
    constructor Create(Options: TsgeInitOptions = InitOptionsAll); virtual;
    destructor  Destroy; override;

    procedure AttachDefaultKeys;                                    //Привязать на кнопки стандартные действия
    procedure ScreenShot(FileName: String = '');                    //Сохранить снимок окна в BMP

    procedure Init; virtual;                                        //Пользовательская инициализация
    function  CloseWindow: Boolean; virtual;                        //Возможность закрытия окна

    procedure Run;                                                  //Запустить приложение
    procedure Stop;                                                 //Остановить приложение

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
    property ExtControllers: TsgeExtensionControllers read FExtensionControllers;
    property ExtShell: TsgeExtensionShell read FExtensionShell;
    property ExtKeyCommand: TsgeExtensionKeyCommand read FExtensionKeyCommand;
    property ExtTimeEvent: TsgeExtensionTimeEvent read FExtensionTimeEvent;
    property ExtGUI: TsgeExtensionGUI read FExtensionGUI;
    property ExtSound: TsgeExtensionSound read FExtensionSound;
    property ExtMusicPlayer: TsgeExtensionMusicPlayer read FExtensionMusicPlayer;
    property ExtCursor: TsgeExtensionCursor read FExtensionCursor;
  end;



implementation

uses
  sgeErrors, sgeKeys, sgeMemoryStream, sgeVars,
  sgeOSPlatform, sgeDateUtils, sgeFileUtils, sgeShellCommands, sgeVariables,
  sgeEventWindow, sgeEventTimeEvent;


const
  _UNITNAME = 'SimpleGameEngine';

  Err_CantCreateSimpleGameEngine  = 'CantCreateSimpleGameEngine';
  Err_CantCreateScreenShot        = 'CantCreateScreenShot';
  Err_EmptyMethodPointer          = 'EmptyMethodPointer';

  //Имена файлов
  Script_AutoStartName = 'AutoStart';
  Script_AutoStopName = 'AutoStop';

  //Расширения
  Ext_Journal = 'Journal';
  Ext_Script  = 's';

  //Имена стартовых параметров
  spDebug = 'Debug';
  spRunCommand = 'Run';
  spNoSound = 'NoSound';


procedure TSimpleGameEngine.SetDebug(ADebug: Boolean);
begin
  FDebug := ADebug;

  //Установить запись в журнал
  FErrorManager.WriteToJournal := FDebug;
end;


procedure TSimpleGameEngine.CheckStartParameter_Run;
begin
  if FExtensionStartParameters.Parameters.Exist[spRunCommand] then
    FExtensionShell.DoCommand(FExtensionStartParameters.Parameters.Value[spRunCommand]);
end;


procedure TSimpleGameEngine.CheckStartParameter_Debug;
begin
  if FExtensionStartParameters.Parameters.Exist[spDebug] then
    SetDebug(True);
end;


procedure TSimpleGameEngine.RunScript(ScriptName: String; Wait: Boolean);
var
  s: String;
begin
  s := ScriptName + '.' + Ext_Script;
  if FExtensionFileSystem.FileExists(s) then
    FExtensionShell.DoCommand('Script.Load ' + s + '; System.Run ' + ScriptName, Wait);
end;


procedure TSimpleGameEngine.RegisterEventHandlers;
begin
  FEventManager.SubscriberGroupList.Subscribe(Event_WindowClose, @EventWindowClose);
  FEventManager.SubscriberGroupList.Subscribe(Event_TimeEvent, @EventTime);
end;


procedure TSimpleGameEngine.UnregisterEventHandlers;
begin
  FEventManager.SubscriberGroupList.UnSubscribe(Self);
end;


function TSimpleGameEngine.EventWindowClose(Obj: TsgeEventBase): Boolean;
begin
  Result := CloseWindow;

  if Result then Stop;
end;


function TSimpleGameEngine.EventTime(Obj: TsgeEventBase): Boolean;
var
  Proc: TsgeTimeEventProc;
begin
  //Не передавать дальше событие
  Result := True;

  //Ссылка на метод
  Proc := TsgeEventTimeEvent(Obj).Proc;

  //Проверка на пустой указатель
  if Proc = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyMethodPointer);

  //Выполнить функцию
  Proc();
end;


procedure TSimpleGameEngine.Init;
begin
  //Код инициализации пользователя.
  //Метод вызывается в конструкторе
end;


function TSimpleGameEngine.CloseWindow: Boolean;
begin
  Result := True;
end;


constructor TSimpleGameEngine.Create(Options: TsgeInitOptions);
var
  JFile: String;
begin
  //Сохранить режим инициализации
  FInitOptions := Options;

  //Записать себя в глобальную переменную
  sgeVars.SGE := Self;

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
    FExtensionStartParameters := TsgeExtensionStartParameters.Create(FObjectList);  //Стартовые параметры
    CheckStartParameter_Debug;                                                      //Проверить режим отладки

    FExtensionVariables := TsgeExtensionVariables.Create(FObjectList);              //Переменные
    FExtensionWindow := TsgeExtensionWindow.Create(FObjectList);                    //Окно
    FExtensionGraphic := TsgeExtensionGraphic.Create(FObjectList);                  //Графика
    FExtensionPackFiles :=   TsgeExtensionPackList.Create(FObjectList);             //Файловые архивы
    FExtensionFileSystem := TsgeExtensionFileSystem.Create(FObjectList);            //Файловая система
    FExtensionResourceList := TsgeExtensionResourceList.Create(FObjectList);        //Список ресурсов
    FExtensionCursor := TsgeExtensionCursor.Create(FObjectList);                    //Курсоры
    FExtensionControllers := TsgeExtensionControllers.Create(FObjectList);          //Контроллеры
    FExtensionShell := TsgeExtensionShell.Create(FObjectList);                      //Оболочка
    FExtensionKeyCommand := TsgeExtensionKeyCommand.Create(FObjectList);            //Команда на кнопках
    FExtensionTimeEvent := TsgeExtensionTimeEvent.Create(FObjectList);              //Таймерные события
    FExtensionGUI := TsgeExtensionGUI.Create(FObjectList);                          //GUI

    if (ioSound in FInitOptions) and (not FExtensionStartParameters.Parameters.Exist[spNoSound]) then
      begin
      FExtensionSound := TsgeExtensionSound.Create(FObjectList);                    //Звуковая система
      FExtensionMusicPlayer := TsgeExtensionMusicPlayer.Create(FObjectList);        //Музыкальный проигрыватель
      end;

    //Зарегестрировать функции оболочки
    sgeShellCommands_Init(Self);

    //Зарегестрировать системные переменные
    sgeVariables_Init(Self);

    //Зарегестрировать системные обработчики событий
    RegisterEventHandlers;

    //Проверить автозагрузку архивов
    if ioFindAndLoadPacks in FInitOptions then
      FExtensionPackFiles.LoadPackFromDirectory;

    //Пользовательская инициализация
    Init;

    //Проверить привязку кнопок по умолчанию
    if ioAttachDefaultKeys in FInitOptions then
      AttachDefaultKeys;

    //Проверить скрипт автозапуска
    if ioAutoStartScript in FInitOptions then
      RunScript(Script_AutoStartName);

    //Проверить команды оболочки в стартовых параметрах
    if ioSupportRunCommand in FInitOptions then
      CheckStartParameter_Run;

  except
    //Ошибка инициализации движка
    on E: EsgeException do
      begin
      FErrorManager.ShowMessage := True;
      FErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_CantCreateSimpleGameEngine, '', E.Message));
      Halt;
      end;
  end;
end;


destructor TSimpleGameEngine.Destroy;
begin
  //Проверить скрипт автоостанова
  if ioAutoStopScript in FInitOptions then
    RunScript(Script_AutoStopName, True);

  //Отписать системные обработчики событий
  UnregisterEventHandlers;

  //Классы
  FErrorManager.Free;
  FExtensionList.Free;
  FObjectList.Free;
  FEventManager.Free;
end;


procedure TSimpleGameEngine.AttachDefaultKeys;
begin
  //Открыть оболочку на тильду
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
      FExtensionGraphic.ScreenShot(MS);

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

  //Очистить список элементов графики
  FExtensionGraphic.LayerList.Clear;

  //Разбудить основной поток
  FEventManager.Publish(TsgeEventBase.Create(''));
end;



end.

