{
Пакет             Simple Game Engine 2
Файл              sgeExtensionShell.pas
Версия            1.2
Создан            14.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Оболочка
}
{$Include Defines.inc}

unit sgeExtensionShell;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeThread, sgeSystemEvent, sgeSimpleCommand, sgeSimpleParameters, sgeGraphic, sgeGraphicColor,
  sgeGraphicFont, sgeExtensionBase, sgeEventWindow, sgeEventSubscriber, sgeShellCommandQueue,
  sgeShellCommandList, sgeLineEditor, sgeCommandHistory, sgeShellLineList, sgeExtensionGraphic,
  sgeExtensionResourceList, sgeExtensionVariables, sgeGraphicSprite, sgeGraphicElementSpriteCashed;


const
  Extension_Shell = 'Shell';


type
  //Тип текста
  TsgeShellMessageType = (smtError, smtText, smtNote);


  TsgeExtensionShell = class(TsgeExtensionBase)
  private
    //Ссылки
    FExtGraphic: TsgeExtensionGraphic;
    FExtResList: TsgeExtensionResourceList;
    FExtVariables: TsgeExtensionVariables;
    FElementSprite: TsgeGraphicElementSpriteCashed;                 //Указатель на элемент отрисовки

    //Классы
    FThread: TsgeThread;                                            //Поток обработки команд
    FCommandQueue: TsgeShellCommandQueue;                           //Очередь комманд на выполнение
    FCommandList: TsgeShellCommandList;                             //Список команд оболочки
    FCommandHistory: TsgeCommandHistory;                            //История введённых команд
    FJournal: TsgeShellLineList;                                    //Журнал
    FEditor: TsgeLineEditor;                                        //Однострочный редактор
    FAliases: TsgeSimpleParameters;                                 //Псевдонимы
    FCanvas: TsgeGraphicSprite;                                     //Холст для отрисовки оболочки
    FFont: TsgeGraphicFont;                                         //Шрифт

    //Ссылки на объекты подписки
    FSubKeyDown : TsgeEventSubscriber;
    FSubKeyUp   : TsgeEventSubscriber;
    FSubKeyChar : TsgeEventSubscriber;

    //Параметры
    FEnable: Boolean;
    FWeakSeparator: Boolean;
    FJournalLines: Byte;                                            //Количество строк журнала

    FBGSprite: TsgeGraphicSprite;                                   //Фоновый спрайт

    FBGColor: TsgeColor;                                            //Цвет фона
    FEditorTextColor: TsgeColor;                                    //Цвет текста строки редактора
    FEditorSelectColor: TsgeColor;                                  //Цвет выделения строки редактора
    FEditorCursorColor: TsgeColor;                                  //Цвет курсора строки редактора
    FErrorColor: TsgeColor;                                         //Цвет ошибки
    FTextColor: TsgeColor;                                          //Цвет простого текста
    FNoteColor: TsgeColor;                                          //Цвет заметки

    //Вспомогательные параметры
    FNewWidth: Integer;                                             //Новая ширина
    FNewHeight: Integer;                                            //Новая высота
    FChangeSize: Boolean;                                           //Флаг изменения размеров
    FSkipChar: Boolean;                                             //Флаг пропуска события WM_CHAR
    FCommandIsRunning: Boolean;                                     //Флаг выполнения команды

    //Обработчики событий
    procedure RegisterEventHandlers;
    procedure UnRegisterEventHandlers;
    function  Event_WindowResize(Obj: TsgeEventWindowSize): Boolean;
    function  Handler_KeyDown(EventObj: TsgeEventWindowKeyboard): Boolean;
    function  Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
    function  Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;

    //Вспомогательные методы
    procedure RegisterDefaultAliases;                               //Добавить алиасы по умолчанию
    function  FitSprite(ShellW, ShellH: Single; ImageW, ImageH: Single): TsgeFloatRect; //Взять координаты вывода фонового спрайта
    function  SubstituteVariables(Str: String): String;             //Подставить здначеня переменных в строку
    procedure RunCommand(Cmd: TsgeSimpleCommand);                   //Выполнение разобранной команды
    procedure ExecuteCommand(Command: String);                      //Разбор строки на алиасы и выполнение
    procedure PaintCanvas(Graphic: TsgeGraphic);                    //Перерисовать холст
    procedure RepaintInner;                                         //Перерисовать из основного потока

    //Методы потока
    procedure InitGraphic;                                          //Подготовить графику
    procedure DoneGraphic;                                          //Освободить графику
    procedure ChangeGraphicSize;                                    //Изменить размеры контекста
    procedure ProcessCommand;                                       //Функция разбора и выполнения команды

    //Обработчик ошибок для ErrorManager
    procedure ErrorHandler(Txt: String);                            //Вывести в журнал ошибку

    //Свойства
    procedure SetEnable(AEnable: Boolean);
    procedure SetBGSprite(ASprite: TsgeGraphicSprite);

    //Системные переменные
    procedure RegisterVariables;
    procedure Variable_SetEnable(AEnable: Boolean);
    function  Variable_GetEnable: Boolean;
    procedure Variable_SetBGColor(AColor: TsgeRGBA);
    function  Variable_GetBGColor: TsgeRGBA;
  protected
    //Специальные команды
    FStopExecuting: Boolean;                                        //Флаг прерывания работы скрипта
    FReadMode: Boolean;                                             //Флаг обработки команды Read, ReadLn
    FReadKeyMode: Boolean;                                          //Флаг обработки команды ReadKey
    FreadKeyChar: Byte;                                             //Код символа нажатия
    FEvent: TsgeSystemEvent;                                        //События для Read, ReadLn
    procedure RepaintThread;                                        //Перерисовать из потока оболочки

    class function GetName: String; override;
  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    //Добавить строку в журнал
    procedure LogMessage(Text: String; MsgType: TsgeShellMessageType = smtText);

    //Выполнить команду
    procedure DoCommand(Cmd: String);
    procedure StopCommand;

    //Классы
    property Aliases: TsgeSimpleParameters read FAliases;
    property CommandList: TsgeShellCommandList read FCommandList;
    property CommandHistory: TsgeCommandHistory read FCommandHistory;
    property Editor: TsgeLineEditor read FEditor;

    //Параметры
    property Enable: Boolean read FEnable write SetEnable;
    property Journal: TsgeShellLineList read FJournal;
    property WeakSeparator: Boolean read FWeakSeparator write FWeakSeparator;

    property BGSprite: TsgeGraphicSprite read FBGSprite write SetBGSprite;
    property BGColor: TsgeColor read FBGColor write FBGColor;
    property EditorTextColor: TsgeColor read FEditorTextColor write FEditorTextColor;
    property EditorSelectColor: TsgeColor read FEditorSelectColor write FEditorSelectColor;
    property EditorCursorColor: TsgeColor read FEditorCursorColor write FEditorCursorColor;
    property ErrorColor: TsgeColor read FErrorColor write FErrorColor;
    property TextColor: TsgeColor read FTextColor write FTextColor;
    property NoteColor: TsgeColor read FNoteColor write FNoteColor;
  end;



implementation

uses
  sgeErrors, sgeStringList, sgeEventBase, sgeSystemUtils, sgeStringUtils,
  sgeVariableBase, sgeShellCommand, sgeKeys, sgeShellLine, sgeShellLineItem, sgeMathUtils;

const
  _UNITNAME = 'ExtensionShell';

  Err_CommandStillRunning = 'CommandStillRunning';
  Err_CommandNotFound     = 'CommandNotFound';
  Err_NotEnoughParameters = 'NotEnoughParameters';
  Err_UnexpectedError     = 'UnexpectedError';
  Err_CommandError        = 'CommandError';
  Err_MultipleCommand     = 'MultipleCommand';

  //Приоритеты
  HandlerPriority = $FFFF;

  //Настройки парсера
  CommandSeparator = ';';
  VariablePrefix = '@';

type
  TsgeExtensionGraphicHack = class(TsgeExtensionGraphic);



procedure TsgeExtensionShell.RegisterEventHandlers;
begin
  FSubKeyDown := EventManager.Subscribe(Event_WindowKeyDown, TsgeEventHandler(@Handler_KeyDown), HandlerPriority, False);
  FSubKeyUp := EventManager.Subscribe(Event_WindowKeyUp, TsgeEventHandler(@Handler_KeyUp), HandlerPriority, False);
  FSubKeyChar := EventManager.Subscribe(Event_WindowChar, TsgeEventHandler(@Handler_KeyChar), HandlerPriority, False);

  EventManager.Subscribe(Event_WindowSize, TsgeEventHandler(@Event_WindowResize), $FFFE, True);
end;


procedure TsgeExtensionShell.UnRegisterEventHandlers;
begin
  //Отписаться от всех событий
  EventManager.UnSubscribe(Self);
end;


function TsgeExtensionShell.Event_WindowResize(Obj: TsgeEventWindowSize): Boolean;
begin
  Result := False;

  RepaintInner;

  //Сохранить новые размеры окна
  FNewWidth := Obj.Width;
  FNewHeight := Obj.Height;
  FChangeSize := True;
end;


function TsgeExtensionShell.Handler_KeyDown(EventObj: TsgeEventWindowKeyboard): Boolean;
var
  s: String;
begin
  Result := True;

  //Проверить на команду ReadKey
  if FReadKeyMode then
    begin
    FSkipChar := True;            //Флаг пропуска ввода символа в консоль
    FReadKeyMode := False;        //Выключить режим чтения одной кнопки
    FreadKeyChar := EventObj.Key; //Запомнить код клавиши
    FEvent.Up;                    //Сказать потоку что нажали кнопку
    Exit;
    end;


  //Обработать системные клавиши
  case EventObj.Key of

    //Закрыть оболочку
    keyEscape: Enable := False;

    //Выполнить команду
    keyEnter:
      begin
      //Проверить на команду Read, ReadLn
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
        FCommandHistory.AddCommand(s);

        //Выполнить команду
        DoCommand(s);
        end;
      end;


      //Установить предыдущую команду в поле редактора
      keyUp: FEditor.Line := FCommandHistory.GetPreviousCommand;


      //Установить следующую команду в поле редактора
      keyDown: FEditor.Line := FCommandHistory.GetNextCommand;


      //Очистить журнал оболочки
      keyL:
        if (kbCtrl in EventObj.KeyboardButtons) then FJournal.Clear;

      //Остановить выполнение команды
      keyPause:
        if (kbCtrl in EventObj.KeyboardButtons) then StopCommand;

    else
      FEditor.ProcessKey(EventObj.Key, EventObj.KeyboardButtons);
  end;

  //Перерисовать оболочку
  RepaintInner;
end;


function TsgeExtensionShell.Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
begin
  Result := True;
end;


function TsgeExtensionShell.Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;
begin
  Result := True;

  //Проверить на пропуск ввода
  if FSkipChar then
    begin
    FSkipChar := False;
    Exit;
    end;

  //Отослать в редактор
  FEditor.ProcessChar(EventObj.Char, EventObj.KeyboardButtons);

  //Перерисовать
  RepaintInner;
end;


procedure TsgeExtensionShell.RegisterDefaultAliases;
begin
  FAliases.SetValue('Close', 'System.Stop');
  FAliases.SetValue('Quit', 'System.Stop');
  FAliases.SetValue('Echo', 'System.Write');
  FAliases.SetValue('Print', 'System.Write');
  //FAliases.SetValue('Exec', 'Run');
end;


function TsgeExtensionShell.FitSprite(ShellW, ShellH: Single; ImageW, ImageH: Single): TsgeFloatRect;
var
  scrR, imgR: Double;
  D: Single;
begin
  scrR := ShellW / ShellH;
  imgR := ImageW / ImageH;

  if scrR < imgR then
    begin
    D := ShellH * ImageW / ImageH;

    Result.Y1 := 0;
    Result.Y2 := ShellH;
    Result.X1 := (ShellW / 2) - (D / 2);
    Result.X2 := Result.X1 + D;
    end else
    begin
    D := ShellW * ImageH / ImageW;

    Result.X1 := 0;
    Result.X2 := ShellW;
    Result.Y1 := (ShellH / 2) - (D / 2);
    Result.Y2 := Result.Y1 + D;
    end;
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

  MatchError = 0;
  MatchOneCommand = 1;
  MatchDuplicate = 2;
var
  Mode, Match: Byte;
  i, c: Integer;
  CmdResult, S: String;
  Command: TsgeShellCommand;
  MatchList: TsgeShellCommandList;
  JLine: TsgeShellLine;
begin
  Command := nil;

  //Определить режим работы
  Mode := ModeEmpty;
  if LowerCase(Cmd.Part[0]) = 'autor' then Mode := ModeAutor;

  //Найти имена команд без имени группы
  MatchList := TsgeShellCommandList.Create(False);
  FCommandList.GetMatchCommandList(Cmd.Part[0], mtName, MatchList); //Получить список

  //Определить результат поиска
  Match := MatchError;                                              //Нет команды
  if MatchList.Count = 1 then Match := MatchOneCommand;             //Одна команда
  if MatchList.Count > 1 then Match := MatchDuplicate;              //Больше одной

  //Обработать результат поиска без групп
  case Match of
    MatchError:
      begin
      //Найти список команд с полным именем
      FCommandList.GetMatchCommandList(Cmd.Part[0], mtGroup, MatchList);

      //Если есть хоть одна запись, то запомнить указатель
      if MatchList.Count = 1 then Command := MatchList.Item[0];
      end;

    MatchOneCommand:
      Command := MatchList.Item[0];

    MatchDuplicate:
      begin
      //Подготовить список найденных комманд для ошибки
      c := MatchList.Count - 1;
      for i := 0 to c do
        begin
        S := S + MatchList.Item[i].GetFullName;
        if i <> c then S := S + ', ';
        end;

      //Обработать ошибку
      ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_MultipleCommand, S));
      Exit;
      end;
  end;

  //Почистить память
  MatchList.Free;

  //Проверить указатель команды
  if Command <> nil then Mode := ModeCommand;

  //Обработать режим работы
  case Mode of
    ModeEmpty:
      ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_CommandNotFound, Cmd.Part[0]));


    ModeCommand:
      begin
      //Проверить хватает ли параметров
      if Cmd.Count < Command.MinParamCount + 1 then
        begin
        ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_NotEnoughParameters, Cmd.Command));
        Exit;
        end;

      //Выполнить команду
      try
        CmdResult := Command.Execute(Cmd);
      except
        ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_UnexpectedError, Cmd.Command));
      end;

      //Проверить результат выполнения
      if CmdResult <> '' then
        ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_CommandError, Cmd.Command, CmdResult));
      end;


    ModeAutor:
      begin
      S := Utf8ToAnsi('Творческий Человек  [accuratealx@gmail.com]');
      JLine := FJournal.Add;
      for i := 1 to Length(S) do
        JLine.Add(S[i], sgeGetRandomColor, cBlack);
      end;
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

        //Проверить пусто
        if Cmd.Count = 0 then Continue;

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


procedure TsgeExtensionShell.PaintCanvas(Graphic: TsgeGraphic);
const
  Indent = 5;
  JournalLineIndent = 1;
var
  W, H, JEnd, JBegin, LineH, CharW, JournalH, MaxCharWidth, i, j, DrawChar, C, CharToCut, ItemW, XOffset: Integer;
  X, Y, Y1, Y2, X1, X2: Single;
  Line: TsgeShellLine;
  Item: TsgeShellLineItem;
  Rct: TsgeFloatRect;
  s: String;
begin
  //Расчёты
  W := FExtGraphic.Graphic.Width;                                   //Ширина спрайта
  LineH := FFont.GetStringHeight('W');                              //Высота строки текста
  CharW := FFont.GetStringWidth('W');                               //Ширина символа
  JournalH := (LineH + JournalLineIndent * 2) * FJournalLines;      //Высота журнала
  H := JournalH + LineH + Indent * 3;                               //Высота оболочки
  MaxCharWidth := sgeFloor((W - Indent * 2) / CharW);               //Максимум символов по ширине
  XOffset := 0;


  //Проверить размеры холста и поправить
  if (FCanvas.Width <> W) or (FCanvas.Height <> H) then FCanvas.SetSize(W, H);


  with Graphic do
    begin
    //Подготовить спрайт для вывода
    PushAttrib;                                                     //Сохранить параметры
    RenderSprite := FCanvas;                                        //Установить спрайт для вывода
    RenderPlace := grpSprite;                                       //Переключить режим вывода
    ResetDrawOptions;                                               //Сбросить настройки вывода
    doCoordinateType := gctClassic;


    //Залить фоновым цветом
    ColorBlend := False;                                            //Отключить смешивание цветов
    PoligonMode := gpmFill;                                         //Установить режим заливки полигонов
    Color := FBGColor;                                              //Установить фоновый цвет
    DrawRect(0, 0, FCanvas.Width, FCanvas.Height);                  //Вывести прямоугольник
    ColorBlend := True;                                             //Включить смештвание цветов


    //Вывод фоновой картинки если есть
    if Assigned(FBGSprite) then
      begin
      Rct := FitSprite(W, H, FBGSprite.Width, FBGSprite.Height);
      DrawSprite(Rct, FBGSprite);
      end;


    //Координаты Начала вывода строки редактора
    X := Indent;
    Y := H - Indent - LineH + JournalLineIndent;


    //Вывод спецсимвола ожидания ввода
    s := '';
    if FReadMode then s := '?';
    if FReadKeyMode then s := '#';
    if s <> '' then
      begin
      Color := FEditorTextColor;
      DrawText(X, Y, FFont, s);
      XOffset := CharW;
      end;


    //Вывод строки редактора
    Color := FEditorTextColor;                                      //Установить цвет текста
    DrawText(X + XOffset, Y, FFont, FEditor.Line);                  //Вывод


    //Границы высоты выделения строки редактора
    Y1 := Y - 2;
    Y2 := H - Indent + 2;


    //Выделение строки редактора
    if FEditor.SelectCount > 0 then
      begin
      X1 := Indent + XOffset + FFont.GetStringWidth(FEditor.GetTextBeforePos(FEditor.SelectBeginPos));
      X2 := Indent + XOffset + FFont.GetStringWidth(FEditor.GetTextBeforePos(FEditor.SelectEndPos));
      Color := FEditorSelectColor;
      DrawRect(X1, Y1, X2, Y2);
      end;


    //Курсор строки редактора
    X1 := Indent + XOffset + FFont.GetStringWidth(FEditor.GetTextBeforePos(FEditor.CursorPos));
    Color := FEditorCursorColor;
    DrawLine(X1, Y1, X1, Y2);


    //Вывод журнала оболочки
    JEnd := FJournal.Count - 1;
    JBegin := FJournal.Count - sgeMin(FJournal.Count, FJournalLines);
    Y := Indent + JournalH - LineH;

    for i := JEnd downto JBegin do
      begin
      Line := FJournal.Item[i];                                     //Ссылка на строку
      Y1 := Y - (JEnd - i) * (LineH + JournalLineIndent * 2);       //Координата Y строки
      DrawChar := 0;                                                //Выведено символов в строке

      //Вывод элементов
      for j := 0 to Line.Count - 1 do
        begin
        Item := Line.Item[j];                                       //Ссылка на элемент
        if DrawChar >= MaxCharWidth then Break;                     //Проверить на переполнение вывода
        C := DrawChar + Length(Item.Text);                          //Длина текста с предыдущим выводом
        s := Item.Text;
        X1 := Indent + DrawChar * CharW;                            //Координата X элемента строки

        //Если текст вылез за границу спрайта, то обрезать
        if C > MaxCharWidth then
          begin
          CharToCut := C - MaxCharWidth + 3;                        //Сколько отрезать от вывода
          Delete(s, Length(s) - CharToCut + 1, CharToCut);          //Отрезать лишний текст
          s := s + '...';                                           //Для красоты
          end;

        //Вывод фона
        ItemW := Length(s);
        Color := Item.BGColor;
        DrawRect(X1, Y1 - 1, X1 + ItemW * CharW, Y1 + LineH + 1);

        //Вывод текста
        Color := Item.Color;
        DrawText(X1, Y1 + JournalLineIndent + 1, FFont, s);

        //Сместить начало вывода нового символа
        Inc(DrawChar, ItemW);
        end;
      end;


    //Восстановить графику
    RenderPlace := grpScreen;                                       //Изменить вывод на экран
    RenderSprite := nil;                                            //Отвязать спрайт от вывода
    PopAttrib;                                                      //Восстановить параметры


    //Выполнить действия над спрайтом
    Finish;
    end;


  //Обновить графический примитив
  FElementSprite.W := W;
  FElementSprite.H := H;
  FElementSprite.Update;
end;


procedure TsgeExtensionShell.RepaintInner;
begin
  PaintCanvas(FExtGraphic.Graphic);
end;


procedure TsgeExtensionShell.InitGraphic;
begin
  with TsgeExtensionGraphicHack(FExtGraphic).FGraphicShell do
    begin
    Init;
    Activate;
    end;
end;


procedure TsgeExtensionShell.DoneGraphic;
begin
  TsgeExtensionGraphicHack(FExtGraphic).FGraphicShell.Done;
end;


procedure TsgeExtensionShell.ChangeGraphicSize;
begin
  FChangeSize := False;
  TsgeExtensionGraphicHack(FExtGraphic).FGraphicShell.ChangeViewArea(FNewWidth, FNewHeight);
end;


procedure TsgeExtensionShell.ProcessCommand;
begin
  //Установить флаг выполнения команды
  FCommandIsRunning := True;

  //Выполнить накопленные команды
  while FCommandQueue.Count > 0 do
    begin
    //Проверить на изменение размеров контекста
    if FChangeSize then ChangeGraphicSize;

    //Выполнить следующую команду
    ExecuteCommand(FCommandQueue.PullFirstCommand);
    end;

  //Снять флаг выполнения команды
  FCommandIsRunning := False;
end;


procedure TsgeExtensionShell.ErrorHandler(Txt: String);
var
  List: TsgeStringList;
  i: Integer;
begin
  //Разобрать ошибку на строки
  List := TsgeStringList.Create;
  List.FromString(Txt);

  //Добавить в журнал строки, первая ошибка
  for i := 0 to List.Count - 1 do
    if i = 0 then LogMessage(List.Part[i], smtError) else LogMessage('  ' + List.Part[i], smtError);

  //Перерисовать оболочку
  RepaintInner;
end;


procedure TsgeExtensionShell.RegisterVariables;
begin
  FExtVariables.AddBoolean('Shell.Enable', False, @Variable_GetEnable, @Variable_SetEnable, 'On', 'Off');
  FExtVariables.AddColor('Shell.BGColor', sgeGetRGBA(128, 0, 128, 128), @Variable_GetBGColor, @Variable_SetBGColor);
end;


procedure TsgeExtensionShell.Variable_SetEnable(AEnable: Boolean);
begin
  Enable := AEnable;
end;


function TsgeExtensionShell.Variable_GetEnable: Boolean;
begin
  Result := Enable;
end;


procedure TsgeExtensionShell.Variable_SetBGColor(AColor: TsgeRGBA);
begin
  BGColor := sgeRGBAToColor(AColor);
end;


function TsgeExtensionShell.Variable_GetBGColor: TsgeRGBA;
begin
  Result := sgeColorToRGBA(BGColor);
end;


procedure TsgeExtensionShell.SetEnable(AEnable: Boolean);
begin
  //Запомнить состояние
  FEnable := AEnable;

  //Поправить элемент отрисовки
  FElementSprite.Visible := FEnable;

  //Поправить подписчиков событий
  FSubKeyDown.Enable := FEnable;
  FSubKeyUp.Enable := FEnable;
  FSubKeyChar.Enable := FEnable;
end;


procedure TsgeExtensionShell.SetBGSprite(ASprite: TsgeGraphicSprite);
begin
  //Сохранить спрайт
  FBGSprite := ASprite;

  //Перерисовать оболочку
  RepaintInner;
end;


procedure TsgeExtensionShell.RepaintThread;
begin
  PaintCanvas(TsgeExtensionGraphicHack(FExtGraphic).FGraphicShell);
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
    FCommandHistory := TsgeCommandHistory.Create;
    FJournal := TsgeShellLineList.Create;
    FAliases := TsgeSimpleParameters.Create;
    FCommandList := TsgeShellCommandList.Create;
    FEditor := TsgeLineEditor.Create;
    FFont := TsgeGraphicFont.Create('Lucida Console', 12, [gfaBold]);

    //Задать параметры
    FEnable := False;
    FWeakSeparator := True;
    FReadMode := False;
    FReadKeyMode := False;
    FJournalLines := 12;

    FBGColor            := sgeRGBAToColor(128, 0, 128, 128);
    FEditorTextColor    := sgeRGBAToColor(255, 255, 255, 255);
    FEditorCursorColor  := sgeRGBAToColor(255, 255, 255, 255);
    FEditorSelectColor  := sgeChangeColorAlpha(FEditorCursorColor, 0.5);
    FErrorColor         := sgeRGBAToColor(255, 0, 0, 255);
    FTextColor          := sgeRGBAToColor(255, 255, 255, 255);
    FNoteColor          := sgeRGBAToColor(255, 255, 255, 127);


    //Активировать графику
    FThread.RunProcAndWait(@InitGraphic, tpemSuspend);

    //Установить обработчик ошибок
    ErrorManager.ShellHandler := @ErrorHandler;

    //Подписать обработчики
    RegisterEventHandlers;

    //Добавить стандартные алиасы
    RegisterDefaultAliases;

    //Зарегестрировать переменные
    RegisterVariables;

    //Создать холст
    FCanvas := TsgeGraphicSprite.Create(500, 300);

    //Создать слой отрисовки
    FExtGraphic.DrawList.AddLayer(Extension_Shell, $FF);

    //Создать элемент отрисовки
    FElementSprite := TsgeGraphicElementSpriteCashed.Create(0, 0, FCanvas.Width, FCanvas.Height, FCanvas);
    FElementSprite.Visible := False;

    //Добавить элемент в список отрисовки
    FExtGraphic.DrawList.AddElement(FElementSprite, Extension_Shell);

    //Перерисовать оболочку
    RepaintInner;
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionShell.Destroy;
begin
  //Отписать подписчиков
  UnRegisterEventHandlers;

  //Удалить элемент отрисовки
  FElementSprite.Delete;

  //Удалить холст
  FCanvas.Free;

  //Остановить выполнение команд
  StopCommand;

  //Освободить графику
  FThread.RunProcAndWait(@DoneGraphic);

  //Удалить объекты
  FThread.Free;
  FFont.Free;
  FEvent.Free;
  FCommandQueue.Free;
  FEditor.Free;
  FJournal.Free;
  FCommandHistory.Free;
  FAliases.Free;
  FCommandList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionShell.LogMessage(Text: String; MsgType: TsgeShellMessageType);
var
  Color: TsgeColor;
begin
  //Определить цвет строки
  case MsgType of
    smtText : Color := FTextColor;
    smtError: Color := FErrorColor;
    smtNote : Color := FNoteColor;
  end;

  //Добавить строку в журнал
  FJournal.Add(Text, Color);
end;


procedure TsgeExtensionShell.DoCommand(Cmd: String);
begin
  //Добавить команду в список
  FCommandQueue.Add(Cmd);

  //Установить обработчик для потока
  FThread.RunProc(@ProcessCommand, tpemSuspend);
end;


procedure TsgeExtensionShell.StopCommand;
begin
  //FStopExecuting := True;
  //FEvent.Up;
end;


end.


