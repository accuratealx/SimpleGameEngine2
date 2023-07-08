{
Пакет             Simple Game Engine 2
Файл              sgeExtensionGraphic.pas
Версия            1.9
Создан            14.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Графика
}
{$Include Defines.inc}

unit sgeExtensionGraphic;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeThread, sgeMemoryStream, sgeCounter, sgeWindow, sgeAnsiFont,
  sgeExtensionBase,
  sgeGraphicColor, sgeGraphicOpenGL,
  sgeGraphicOpenGLDrawObjectFade, sgeGraphicOpenGLDrawObjectFadeItem,
  sgeDisplayElementAnsiText, sgeGraphicOpenGLDrawObjectAnsiText,

  sgeGraphicElementLayerList,

  sgeEventList, sgeEventBase, sgeEventWindow, sgeEventGraphic,
  sgeExtensionWindow;


const
  Extension_Graphic = 'Graphic';


type
  //Тип перехода затемнения
  TsgeExtensionFadeMode = sgeGraphicOpenGLDrawObjectFadeItem.TsgeFadeMode;


  //Режим ограничения кадров
  TsgeExtensionGraphicDrawControl = (
    gdcSync,    //Вертикальная синхронизация
    gdcProgram  //Програмный способ
  );


  TsgeExtensionGraphic = class(TsgeExtensionBase)
  private
    //Ссылки на объекты
    FExtWindow: TsgeExtensionWindow;
    FFont: TsgeAnsiFont;                                            //ССылка на класс шрифта

    //Классы
    //FLayerList: TsgeGraphicElementLayerList;                        //Класс слоёв отрисовки
    FGraphic: TsgeGraphicOpenGL;                                    //Класс графики для потока рендера сцены
    FThread: TsgeThread;                                            //Поток основного класса графики
    FEventList: TsgeEventList;                                      //Список объектов событий
    FFadeElement: TsgeGraphicOpenGLDrawObjectFade;                  //Элемент затемнения

    FDisplayFPS: TsgeDisplayElementAnsiText;                        //Элемент рисования
    FDrawFPS: TsgeGraphicOpenGLDrawObjectAnsiText;                  //Элемент рендера
    FFPSCounter: TsgeCounter;                                       //Счётчик FPS

    //Перменные
    FScreenSize: TsgeFloatPoint;                                    //Размеры экрана
    FDrawControl: TsgeExtensionGraphicDrawControl;                  //Способ ограничения кадров
    FMaxFPS: Word;                                                  //Максимальное количество кадров в секунду
    FAutoEraseBG: Boolean;                                          //Автостирание фона перед выводом кадра

    //Вспомогательные параметры
    FDrawLastTime: Int64;
    FDrawCurrentTime: Int64;
    FDrawDelay: Int64;
    //FScreenshotStream: TsgeMemoryStream;                            //Ссылка на память

    //Методы потока
    procedure InitGraphic;
    procedure DoneGraphic;
    procedure CreateObjects;

    procedure ProcessEvents;
    procedure ProcessEvent_WindowResize(Event: TsgeEventWindow);
    procedure ProcessEvent_ShaderAdd(Event: TsgeEventGraphicShaderAdd);
    procedure ProcessEvent_FadeNew(Event: TsgeEventGraphicFadeNew);
    procedure ProcessEvent_LayerAdd(Event: TsgeEventGraphicLayer);
    procedure ProcessEvent_LayerModify(Event: TsgeEventGraphicLayer);
    procedure ProcessEvent_LayerDelete(Event: TsgeEventGraphicLayer);


    procedure ChangeDrawControl;
    //procedure GetScreenshot;
    procedure SystemDraw;
    procedure Draw;

    //Вывод графики
    procedure DrawElements;
    procedure ProcessFPS;
    procedure DrawFPS;

    //Свойства
    procedure SetDrawControl(AMetod: TsgeExtensionGraphicDrawControl);
    procedure SetMaxFPS(AMaxFPS: Word);

    //Затемнение
    procedure FadeCallBackProc(Time: TsgePassedTime; ID: Integer);

    //Подписка на события
    function EventHandler_WindowResize(Obj: TsgeEventWindow): TsgeEventHandlerResult;
    function EventHandler_ShaderAdd(Obj: TsgeEventGraphicShaderAdd): TsgeEventHandlerResult;
    function EventHandler_FadeNew(Obj: TsgeEventGraphicFadeNew): TsgeEventHandlerResult;
    function EventHandler_Layer(Obj: TsgeEventGraphicLayer): TsgeEventHandlerResult;


  protected
    function GetName: String; override;
    procedure RegisterEventHandlers; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    //Методы
    procedure Init;                                                           //Создние необходимых объектов
    procedure AddShader(ShaderName: String; ShaderStream: TsgeMemoryStream);  //Добавить шейдерную программу
    procedure SetSystemFont(Font: TsgeAnsiFont);                             //Установить системный шрифт
    procedure StartRender;                                                    //Запуск потока рендерера

    procedure Fade(Mode: TsgeExtensionFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer = -1);
    //procedure ScreenShot(Stream: TsgeMemoryStream);

    //Объекты
    //property Graphic: TsgeGraphicOpenGL read FGraphic;
    //property LayerList: TsgeGraphicElementLayerList read FLayerList;
    //property FPS: TsgeGraphicFPS read FFPS;

    //Параметры
    property MaxFPS: Word read FMaxFPS write SetMaxFPS;
    property DrawControl: TsgeExtensionGraphicDrawControl read FDrawControl write SetDrawControl;
    property AutoEraseBG: Boolean read FAutoEraseBG write FAutoEraseBG;
  end;


implementation

uses
  sgeErrors, sgeOSPlatform,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLShaderProgramTable,
  sgeGraphicOpenGLLayerTable, sgeGraphicOpenGLLayer, dglOpenGL;


const
  _UNITNAME = 'ExtensionGraphic';

  ERR_EVENT_ERROR = 'EventError';


procedure TsgeExtensionGraphic.InitGraphic;
begin
  FGraphic.Activate;
  FGraphic.ChangeViewPort(0, 0);
  FGraphic.SetBGColor(cDarkGray);
end;


procedure TsgeExtensionGraphic.DoneGraphic;
begin
  FDrawFPS.Free;
  FDisplayFPS.Free;
  FFadeElement.Free;
  FGraphic.Deactivate;
end;


procedure TsgeExtensionGraphic.CreateObjects;
begin
  //Специальный объект  затемнения
  FFadeElement := TsgeGraphicOpenGLDrawObjectFade.Create;

  //FPS
  FDisplayFPS := TsgeDisplayElementAnsiText.Create(10, 10, cWhite, FFont, '0');
  FDrawFPS := TsgeGraphicOpenGLDrawObjectAnsiText.Create(FDisplayFPS);
end;


procedure TsgeExtensionGraphic.ProcessEvents;
var
  Event: TsgeEventBase;
begin
  while FEventList.Count > 0 do
  begin

    try
      //Указатель на объект события
      Event := FEventList.Item[0];

      //Обработать события
      case Event.Name of

        //Изменение размеров окна
        Event_WindowSize:
          ProcessEvent_WindowResize(TsgeEventWindow(Event));

        //Добавление нового шейдера
        Event_GraphicShaderAdd:
          ProcessEvent_ShaderAdd(TsgeEventGraphicShaderAdd(Event));

        //Добавление нового затемнения
        Event_GraphicFadeNew:
          ProcessEvent_FadeNew(TsgeEventGraphicFadeNew(Event));

        //Добавление нового слоя
        Event_Graphic_LayerAdd:
          ProcessEvent_LayerAdd(TsgeEventGraphicLayer(Event));

        //Изменение слоя
        Event_Graphic_LayerModify:
          ProcessEvent_LayerModify(TsgeEventGraphicLayer(Event));

        //Удаление слоя
        Event_Graphic_LayerDelete:
          ProcessEvent_LayerDelete(TsgeEventGraphicLayer(Event));

      end;


    except
      on E: EsgeException do
        ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, ERR_EVENT_ERROR, Event.Name, E.Message));
    end;


    //Удалить первый элемент
    FEventList.Delete(0);
  end;
end;


procedure TsgeExtensionGraphic.ProcessEvent_WindowResize(Event: TsgeEventWindow);
begin
  //Запомнить размеры экрана
  FScreenSize := sgeGetFloatPoint(Event.Width, Event.Height);

  //Изменить область вывода
  FGraphic.ChangeViewPort(Round(FScreenSize.X), Round(FScreenSize.Y));
end;


procedure TsgeExtensionGraphic.ProcessEvent_ShaderAdd(Event: TsgeEventGraphicShaderAdd);
var
  ShaderProgram: TsgeGraphicOpenGLShaderProgram;
begin
  //Создать микропорограмму
  ShaderProgram := TsgeGraphicOpenGLShaderProgram.Create(Event.ShaderName, Event.ShaderStream);

  //Добавить в таблицу
  OpenGLShaderProgramTable.Add(ShaderProgram);
end;


procedure TsgeExtensionGraphic.ProcessEvent_FadeNew(Event: TsgeEventGraphicFadeNew);
begin
  FFadeElement.Add(Event.Mode, Event.Color, Event.Time, Event.ID, Event.TimeProc);
end;


procedure TsgeExtensionGraphic.ProcessEvent_LayerAdd(Event: TsgeEventGraphicLayer);
var
  Layer: TsgeGraphicElementLayer;
begin
  //Создать слой
  Layer := TsgeGraphicElementLayer.Create(Event.Layer);

  //Добавить слой в список слоев


  //Добавить слой в хэштаблицу
  OpenGLLayerTable.Add(Event.UniqueID, Layer);

  //Удалить копию объекта
  Event.Layer.Free;
end;


procedure TsgeExtensionGraphic.ProcessEvent_LayerModify(Event: TsgeEventGraphicLayer);
var
  Layer: TsgeGraphicElementLayer;
begin
  //Найти слой по ID
  //Layer := TsgeGraphicLayerTable.Create(Event.Layer);

  //Обновить слой
  Layer.Update(Event.Layer);

  //Удалить копию объекта
  Event.Layer.Free;
end;


procedure TsgeExtensionGraphic.ProcessEvent_LayerDelete(Event: TsgeEventGraphicLayer);
begin

end;


procedure TsgeExtensionGraphic.ChangeDrawControl;
begin
  FGraphic.VerticalSync := FDrawControl = gdcSync;
end;


{procedure TsgeExtensionGraphic.GetScreenshot;
begin
  FGraphic.ScreenShot(FScreenshotStream);
end;}


procedure TsgeExtensionGraphic.SystemDraw;
begin
  //Если уничтожение, то не рисовать
  if FDestroying then
    Exit;

  //Выборка событий если есть
  if FEventList.Count > 0 then
    ProcessEvents;

  //Отрисовка
  case FDrawControl of
    gdcSync: Draw;

    gdcProgram:
    begin
      FDrawCurrentTime := sgeGetCPUCounter;
      if (FDrawCurrentTime - FDrawLastTime) >= FDrawDelay then
      begin
        FDrawLastTime := FDrawCurrentTime;
        Draw;
      end;
    end;
  end;
end;


procedure TsgeExtensionGraphic.Draw;
begin
  //Если уничтожение, то не рисовать
  if FDestroying then
    Exit;

  //Стереть фон
  if FAutoEraseBG then
    FGraphic.EraseBG;

  //Вывод елементов
  //DrawElements;

  //Вывод затемнения
  FFadeElement.Draw(FGraphic);

  //Обработать FPS
  ProcessFPS;

  //Вывод FPS
  if FDrawFPS.Visible then
    DrawFPS;

  //Смена кадров
  FGraphic.SwapBuffers;
end;


procedure TsgeExtensionGraphic.DrawElements;
{var
  I: Integer;
  El: TsgeGraphicElementBase;
  Layer: TsgeGraphicElementLayer;}
begin
  (*
  //Заблокировать список
  FLayerList.Lock;
  try

    //Вывод слоёв
    for I := 0 to FLayerList.Count - 1 do
    begin
      //Если уничтожение, то не рисовать
      if FDestroying then
        Exit;

      //Ссылка на слой
      Layer := FLayerList.Item[I];

      //Проверить видимость слоя
      if not Layer.Visible then
        Continue;

      //Поправить смещение
      FGraphicInner.SetPos(Layer.Offset);

      //Поправить масштаб
      FGraphicInner.SetScale(Layer.Scale, Layer.Scale);}

      //Обработать элементы в слое
      El := Layer.Elements.GetFirst;
      while El <> nil do
      begin
        //Удалить элемент
        if El.NeedDelete then
        begin
          Layer.Elements.DeleteCurrentElement;
          El := Layer.Elements.GetNext;
          Continue;
        end;

        //Вывести элемент
        if El.Visible then
        begin
          //Обновить данные
          if El.NeedUpdate then
            TsgeGraphicElementBaseExt(El).ApplySettings;

          //Нарисовать элемент
          El.Draw(FGraphicInner);
        end;

        //Следующий элемент
        El := Layer.Elements.GetNext;
      end;

      //Восстановить параметры графики
      FGraphicInner.PopAttrib;
    end;


  finally
    //Разблокировать список
    FLayerList.UnLock;
  end;
  *)
end;


procedure TsgeExtensionGraphic.ProcessFPS;
begin
  //Увеличить счётчик
  FFPSCounter.Inc;

  //Обновить параметр
  if FFPSCounter.StrCount <> FDisplayFPS.Text then
  begin
    FDisplayFPS.Text := FFPSCounter.StrCount;
    FDrawFPS.Update(FDisplayFPS);
  end;
end;


procedure TsgeExtensionGraphic.DrawFPS;
const
  LayerInfo: TsgeFloatRect = (
    X1: 0;  //PosX
    Y1: 0;  //PosY
    X2: 1;  //ScaleX
    Y2: 1   //ScaleY
  );
begin
  FDrawFPS.Draw(FGraphic, FScreenSize, LayerInfo);
end;


procedure TsgeExtensionGraphic.SetDrawControl(AMetod: TsgeExtensionGraphicDrawControl);
begin
  if FDrawControl = AMetod then
    Exit;

  FDrawControl := AMetod;
  FThread.RunProcAndWait(@ChangeDrawControl);

  //Проверить на ошибки
  if FThread.Exception <> nil then
    raise EsgeException.Create(FThread.Exception.Message);
end;


procedure TsgeExtensionGraphic.SetMaxFPS(AMaxFPS: Word);
begin
  if AMaxFPS = 0 then
    AMaxFPS := 1;
  if FMaxFPS = AMaxFPS then
    Exit;

  FMaxFPS := AMaxFPS;
  FDrawDelay := Round(OneSecFrequency / FMaxFPS);
end;


procedure TsgeExtensionGraphic.FadeCallBackProc(Time: TsgePassedTime; ID: Integer);
var
  Event: TsgeEventBase;
begin
  //Создать событие смены состояния затемнения
  Event := TsgeEventGraphicFade.Create(Event_GraphicFade, Time, ID);

  //Опубликовать событие смены времени затемнения
  EventManager.Publish(Event);
end;


function TsgeExtensionGraphic.EventHandler_WindowResize(Obj: TsgeEventWindow): TsgeEventHandlerResult;
begin
  Result := ehrNormal;
  FEventList.Add(Obj.Copy);
end;


function TsgeExtensionGraphic.EventHandler_ShaderAdd(Obj: TsgeEventGraphicShaderAdd): TsgeEventHandlerResult;
begin
  Result := ehrNormal;
  FEventList.Add(Obj.Copy);
end;


function TsgeExtensionGraphic.EventHandler_FadeNew(Obj: TsgeEventGraphicFadeNew): TsgeEventHandlerResult;
begin
  Result := ehrNormal;
  FEventList.Add(Obj.Copy);
end;


function TsgeExtensionGraphic.EventHandler_Layer(Obj: TsgeEventGraphicLayer): TsgeEventHandlerResult;
begin
  Result := ehrNormal;
  FEventList.Add(Obj.Copy);
end;


function TsgeExtensionGraphic.GetName: String;
begin
  Result := Extension_Graphic;
end;


procedure TsgeExtensionGraphic.RegisterEventHandlers;
begin
  //Установить обработчики
  EventManager.SubscriberGroupList.Subscribe(Event_WindowSize, TsgeEventHandler(@EventHandler_WindowResize), Event_Priority_Max - 0, True);
  EventManager.SubscriberGroupList.Subscribe(Event_GraphicShaderAdd, TsgeEventHandler(@EventHandler_ShaderAdd), Event_Priority_Max - 1, True);
  EventManager.SubscriberGroupList.Subscribe(Event_GraphicFadeNew, TsgeEventHandler(@EventHandler_FadeNew), Event_Priority_Max - 2, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_LayerAdd, TsgeEventHandler(@EventHandler_Layer), Event_Priority_Max - 3, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_LayerModify, TsgeEventHandler(@EventHandler_Layer), Event_Priority_Max - 4, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_LayerDelete, TsgeEventHandler(@EventHandler_Layer), Event_Priority_Max - 5, True);
end;


constructor TsgeExtensionGraphic.Create;
begin
  try
    inherited Create;

    //Параметры
    FDrawControl := gdcSync;
    FAutoEraseBG := True;
    SetMaxFPS(120);

    //Получить ссылки на объекты
    FExtWindow := TsgeExtensionWindow(GetExtension(Extension_Window));

    //Список объктов событий
    FEventList := TsgeEventList.Create(True);

    //Создать поток
    FThread := TsgeThread.Create(Extension_Graphic, nil, True, False);

    //Создать контекст
    FGraphic := TsgeGraphicOpenGL.Create(FExtWindow.Window.DC, 4, 6);

    //Инициализировать контекст
    FThread.RunProcAndWait(@InitGraphic);

    //Проверить создание графики на ошибку
    if FThread.Exception <> nil then
      raise EsgeException.Create(FThread.Exception.Message);

    //Слои отрисовки
    //FLayerList := TsgeGraphicElementLayerList.Create(True);
    //FLayerList.Add(Graphic_Layer_System_Fade, Graphic_LayerIndex_Fade, True);

    //Создать объекты
    FFPSCounter := TsgeCounter.Create(1000);
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionGraphic.Destroy;
begin
  FDestroying := True;

  //Прибить поток
  if FThread <> nil then
  begin
    FThread.LoopProc := nil;
    FThread.RunProcAndWait(@DoneGraphic);
  end;

  //Удалить объекты
  FThread.Free;
  FEventList.Free;
  FGraphic.Free;
  FFPSCounter.Free;

  inherited Destroy;
end;


procedure TsgeExtensionGraphic.Init;
begin
  //Обработать очередь событий, потому что основной поток еще не работает
  FThread.RunProcAndWait(@ProcessEvents);

  //Создать недостающие объекты
  FThread.RunProcAndWait(@CreateObjects);

  if FThread.Exception <> nil then
    raise EsgeException.Create(FThread.Exception.Message);
end;


procedure TsgeExtensionGraphic.AddShader(ShaderName: String; ShaderStream: TsgeMemoryStream);
var
  Event: TsgeEventGraphicShaderAdd;
begin
  //Создать событие
  Event := TsgeEventGraphicShaderAdd.Create(Event_GraphicShaderAdd, ShaderName, ShaderStream);

  //Добавить в собственную очередь
  FEventList.Add(Event);
end;


procedure TsgeExtensionGraphic.SetSystemFont(Font: TsgeAnsiFont);
begin
  if FFont = Font then
    Exit;

  //Запомнить ссылку на шрифт
  FFont := Font;
end;


procedure TsgeExtensionGraphic.StartRender;
begin
  //Установить метод отрисовки
  FThread.LoopProc := @SystemDraw;

  //Продолжить выполнение потока
  FThread.Resume;
end;


procedure TsgeExtensionGraphic.Fade(Mode: TsgeExtensionFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer);
var
  Event: TsgeEventBase;
begin
  //Создать событие
  Event := TsgeEventGraphicFadeNew.Create(Event_GraphicFadeNew, Mode, Color, Time, ID, @FadeCallBackProc);

  //Добавить в очередь
  FEventList.Add(Event);
end;

{procedure TsgeExtensionGraphic.ScreenShot(Stream: TsgeMemoryStream);
begin
  FScreenshotStream := Stream;

  FThread.RunProcAndWait(@GetScreenshot);
end;}


end.

