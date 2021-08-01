{
Пакет             Simple Game Engine 2
Файл              sgeExtensionShell.pas
Версия            1.0
Создан            14.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Оболочка
}
{$Include Defines.inc}

unit sgeExtensionShell;

{$mode objfpc}{$H+}

interface

uses
  sgeThread, sgeSystemEvent, sgeSimpleCommand, sgeSimpleParameters,
  sgeExtensionBase, sgeEventWindow, sgeEventSubscriber, sgeShellCommandQueue,
  sgeShellCommandList, sgeLineEditor, sgeCommandHistory,
  sgeExtensionGraphic, sgeExtensionResourceList, sgeExtensionVariables;


const
  Extension_Shell = 'Shell';


type
  TsgeExtensionShell = class(TsgeExtensionBase)
  private
    //Вспомогательные параметры
    FCommandQueue: TsgeShellCommandQueue;                           //Очередь комманд на выполнение
    FCommandIsRunning: Boolean;                                     //Флаг выполнения команды
    FReadMode: Boolean;                                             //Флаг обработки команды Read, ReadLn

  private
    //Ссылки на расширения
    FExtGraphic: TsgeExtensionGraphic;
    FExtResList: TsgeExtensionResourceList;
    FExtVariables: TsgeExtensionVariables;

    //Классы
    FEvent: TsgeSystemEvent;                                        //События для Read, ReadLn
    FThread: TsgeThread;                                            //Поток обработки команд
    FCommandList: TsgeShellCommandList;                             //Список команд оболочки
    FHistory: TsgeCommandHistory;                                   //История введённых команд
    FEditor: TsgeLineEditor;                                        //Однострочный редактор
    FAliases: TsgeSimpleParameters;                                 //Псевдонимы

    //Ссылки на объекты подписки
    FSubKeyDown: TsgeEventSubscriber;
    FSubKeyUp: TsgeEventSubscriber;
    FSubKeyChar: TsgeEventSubscriber;

    //Параметры
    FEnable: Boolean;
    FWeakSeparator: Boolean;

    //Обработчики событий
    procedure RegisterEventHandlers;
    procedure UnRegisterEventHandlers;
    function  Handler_KeyDown(EventObj: TsgeEventWindowKeyboard): Boolean;
    function  Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
    function  Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;

    //Вспомогательные методы
    procedure RegisterDefaultAliases;                               //Добавить алиасы по умолчанию
    function  SubstituteVariables(Str: String): String;             //Подставить здначеня переменных в строку
    procedure RunCommand(Cmd: TsgeSimpleCommand);                   //Выполнение разобранной команды
    procedure ExecuteCommand(Command: String);                      //Разбор строки на алиасы и выполнение

    //Методы потока
    procedure ProcessCommand;                                       //Функция разбора и выполнения команды

    //Обработчик ошибок для ErrorManager
    procedure ErrorHandler(Txt: String);                            //Вывести в журнал ошибку

    //Свойства
    procedure SetEnable(AEnable: Boolean);

    //Залипон, УДАЛИТЬ
    procedure Draw;

  protected
    class function GetName: String; override;
    procedure SetReadMode;                                          //Установить флаг особого нажатия Enter

    property MyEvent: TsgeSystemEvent read FEvent;
  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    procedure DoCommand(Cmd: String);                               //Выполнить команду

    property Enable: Boolean read FEnable write SetEnable;
    property Aliases: TsgeSimpleParameters read FAliases;
    property CommandList: TsgeShellCommandList read FCommandList;
    property Editor: TsgeLineEditor read FEditor;
    property WeakSeparator: Boolean read FWeakSeparator write FWeakSeparator;
  end;



implementation

uses
  sgeErrors, sgeEventBase, sgeSystemUtils, sgeStringUtils, sgeVariableBase, sgeShellCommand,
  sgeKeys, sgeGraphicColor;

const
  _UNITNAME = 'ExtensionShell';

  Err_CommandStillRunning = 'CommandStillRunning';
  Err_CommandNotFound     = 'CommandNotFound';
  Err_EmptyPointer        = 'EmptyPointer';
  Err_NotEnoughParameters = 'NotEnoughParameters';
  Err_UnexpectedError     = 'UnexpectedError';
  Err_CommandError        = 'CommandError';

  //Приоритеты
  HandlerPriority = $FFFF;

  //Настройки парсера
  CommandSeparator = ';';
  VariablePrefix = '@';



procedure TsgeExtensionShell.RegisterEventHandlers;
begin
  FSubKeyDown := EventManager.Subscribe(Event_WindowKeyDown, TsgeEventHandler(@Handler_KeyDown), HandlerPriority, False);
  FSubKeyUp := EventManager.Subscribe(Event_WindowKeyUp, TsgeEventHandler(@Handler_KeyUp), HandlerPriority, False);
  FSubKeyChar := EventManager.Subscribe(Event_WindowChar, TsgeEventHandler(@Handler_KeyChar), HandlerPriority, False);
end;


procedure TsgeExtensionShell.UnRegisterEventHandlers;
begin
  EventManager.UnSubscribe(Event_WindowKeyDown, TsgeEventHandler(@Handler_KeyDown));
  EventManager.UnSubscribe(Event_WindowKeyUp, TsgeEventHandler(@Handler_KeyUp));
  EventManager.UnSubscribe(Event_WindowChar, TsgeEventHandler(@Handler_KeyChar));
end;


function TsgeExtensionShell.Handler_KeyDown(EventObj: TsgeEventWindowKeyboard): Boolean;
var
  s: String;
begin
  Result := True;

  //Обработать системные клавиши
  case EventObj.Key of

    //Закрыть оболочку
    keyEscape: Enable := False;

    //Выполнить команду
    keyEnter:
      begin
      //Проверить на команду Read
      if FReadMode then
        begin
        FReadMode := False;
        FEvent.Up;
        Exit;
        end;

      //Проверить не выполняется ли ещё команда
      if FCommandIsRunning then
        begin
        //Ошибка, команда ещё выполняется
        ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_CommandStillRunning));
        Exit;
        end;

      //Подготовить команду
      s := sgeTrim(FEditor.Line);
      if s <> '' then
        begin
        //Стереть строку ввода
        FEditor.Line := '';

        //Записать в историю
        FHistory.AddCommand(s);

        //Выполнить команду
        DoCommand(s);
        end;
      end


    else
      FEditor.ProcessKey(EventObj.Key, EventObj.KeyboardButtons);
  end;
end;


function TsgeExtensionShell.Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
begin
  Result := True;
end;


function TsgeExtensionShell.Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;
begin
  Result := True;

  //Отослать в редактор
  FEditor.ProcessChar(EventObj.Char, EventObj.KeyboardButtons);
end;


procedure TsgeExtensionShell.RegisterDefaultAliases;
begin
  FAliases.SetValue('Close', 'Stop');
  FAliases.SetValue('Quit', 'Stop');
  {FAliases.SetValue('Echo', 'Write');
  FAliases.SetValue('Print', 'Write');
  FAliases.SetValue('Echoc', 'Writec');
  FAliases.SetValue('Printc', 'Writec');
  FAliases.SetValue('Exec', 'Run');}
end;


function TsgeExtensionShell.SubstituteVariables(Str: String): String;
var
  i: Integer;
  V: TsgeVariableBase;
begin
  Result := Str;
  for i := 0 to FExtVariables.Variables.Count - 1 do
    begin
    V := FExtVariables.Variables.Item[i];
    Result := sgeStringReplace(Result, VariablePrefix + V.Name, V.StrValue, [rfReplaceAll, rfIgnoreCase]);
    end;
end;


procedure TsgeExtensionShell.RunCommand(Cmd: TsgeSimpleCommand);
const
  ModeEmpty = 0;
  ModeCommand = 1;
  ModeAutor = 2;
var
  Idx: Integer;
  Mode: Byte;
  CmdResult: String;
  CmdProc: TsgeShellCommand;
begin
  //Прокрутить экран вниз
  //if FJournalAutoScroll then FJournalOffset := 0;

  //Определить режим работы
  Mode := ModeEmpty;
  if LowerCase(Cmd.Part[0]) = 'autor' then Mode := ModeAutor;
  Idx := FCommandList.IndexOf(Cmd.Part[0]);
  CmdProc := FCommandList.Item[Idx];
  if CmdProc <> nil then Mode := ModeCommand;

  //Обработать режим
  case Mode of
    ModeEmpty:
      ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_CommandNotFound, Cmd.Part[0]));


    ModeCommand:
      begin
      //Проверить указатель команды
      if CmdProc.Proc = nil then
        begin
        ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_EmptyPointer, Cmd.Command));
        Exit;
        end;

      //Проверить хватает ли параметров
      if Cmd.Count < CmdProc.MinParamCount + 1 then
        begin
        ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_NotEnoughParameters, Cmd.Command));
        Exit;
        end;

      //Выполнить команду
      try
        CmdResult := CmdProc.Proc(Cmd);
      except
        ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_UnexpectedError, Cmd.Command));
      end;

      //Проверить результат выполнения
      if CmdResult <> '' then
        ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_CommandError, Cmd.Command, CmdResult));
      end;


    //ModeAutor:
    //  FJournal.Add(sgeGraphicColor_GetRandomColor, 'sge.ntlab.su  accuratealx@gmail.com');
  end;

end;


procedure TsgeExtensionShell.ExecuteCommand(Command: String);
var
  Line, Cmd: TsgeSimpleCommand;
  i, c, Idx: Integer;
  Str: String;
begin
  try
    //Разобрать строку на части по ;
    Line := TsgeSimpleCommand.Create(Command, True, CommandSeparator);

    //Выполнить части по очереди
    c := Line.Count - 1;
    for i := 0 to c do
      begin
      Str := sgeTrim(Line.Part[i]);

      //Пустая строка
      if Str = '' then Continue;

      //Заметка
      if Str[1] = '#' then Continue;

      //Подставить параметры в строку
      Str := SubstituteVariables(Str);

      try
        //Разобрать команду
        Cmd := TsgeSimpleCommand.Create(Str, FWeakSeparator);

        //Проверить на алиас
        Idx := FAliases.IndexOf(Cmd.Part[0]);
        if Idx <> -1 then
          begin
          //Подставить алиас
          Str := sgeStringReplace(Str, FAliases.Parameter[Idx].Name, FAliases.Parameter[Idx].Value, [rfIgnoreCase]);

          //Выполнить
          ExecuteCommand(Str);
          Continue;
          end;

        //Выполнить команду
        RunCommand(Cmd);
      finally
        Cmd.Free;
      end;


      end;  //For

  finally
    Line.Free;
  end;
end;


procedure TsgeExtensionShell.ProcessCommand;
begin
  //Установить флаг выполнения команды
  FCommandIsRunning := True;

  //Выполнить накопленные команды
  while FCommandQueue.Count > 0 do
    ExecuteCommand(FCommandQueue.PullFirstCommand);

  //Снять флаг выполнения команды
  FCommandIsRunning := False;
end;


procedure TsgeExtensionShell.ErrorHandler(Txt: String);
begin
  //Добавить в журнал строку с ошибкой
end;


procedure TsgeExtensionShell.Draw;
begin
  if not FEnable then Exit;

  //Тут мы рисуем оболочку на экране
  with FExtGraphic.Graphic do
    begin
    PushAttrib;
    Color := cGray;
    DrawRect(0, 0, Width, FExtResList.Default.Font.Height + 10);
    Color := cWhite;
    DrawText(0, 0, FExtResList.Default.Font, FEditor.Line);
    PopAttrib;
    end;
end;


procedure TsgeExtensionShell.SetEnable(AEnable: Boolean);
begin
  //Запомнить состояние
  FEnable := AEnable;

  //Поправить подписчиков событий
  FSubKeyDown.Enable := FEnable;
  FSubKeyUp.Enable := FEnable;
  FSubKeyChar.Enable := FEnable;
end;


procedure TsgeExtensionShell.SetReadMode;
begin
  FReadMode := True;
end;


class function TsgeExtensionShell.GetName: String;
begin
  Result := Extension_Shell;
end;


constructor TsgeExtensionShell.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Поиск указателей
    FExtGraphic := TsgeExtensionGraphic(GetExtension(Extension_Graphic));
    FExtVariables := TsgeExtensionVariables(GetExtension(Extension_Variables));
    FExtResList := TsgeExtensionResourceList(GetExtension(Extension_ResourceList));

    //Создать объекты
    FEvent := TsgeSystemEvent.Create(True, False);
    FThread := TsgeThread.Create;
    FCommandQueue := TsgeShellCommandQueue.Create;
    FHistory := TsgeCommandHistory.Create;
    FAliases := TsgeSimpleParameters.Create;
    FCommandList := TsgeShellCommandList.Create;
    FEditor := TsgeLineEditor.Create;

    //Задать параметры
    FEnable := False;
    FWeakSeparator := True;
    FReadMode := False;

    //Установить обработчик ошибок
    ErrorManager.ShellHandler := @ErrorHandler;

    //Подписать обработчики
    RegisterEventHandlers;

    //Добавить стандартные алиасы
    RegisterDefaultAliases;

    //Установить метод отрисовки оболочки
    FExtGraphic.DrawShellproc := @Draw;
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionShell.Destroy;
begin
  //Отписать подписчиков
  UnRegisterEventHandlers;

  //Удалить объекты
  FThread.Free;
  FEvent.Free;
  FCommandQueue.Free;
  FEditor.Free;
  FHistory.Free;
  FAliases.Free;
  FCommandList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionShell.DoCommand(Cmd: String);
begin
  //Добавить команду в список
  FCommandQueue.Add(Cmd);

  //Установить обработчик для потока
  FThread.RunProc(@ProcessCommand, tpemSuspend);
end;


end.


