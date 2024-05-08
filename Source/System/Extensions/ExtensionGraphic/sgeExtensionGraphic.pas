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
  sgeTypes, sgeThread, sgeMemoryStream, sgeCounter, sgeAnsiFont, sgeSprite,
  sgeExtensionBase,
  sgeColor, sgeGraphicOpenGL, sgeGraphicOpenGLLayerList,
  sgeGraphicOpenGLDrawObjectFade, sgeGraphicOpenGLDrawObjectFadeItem,
  sgeDisplayElementAnsiText, sgeGraphicOpenGLDrawObjectAnsiText,
  sgeDisplayElement, sgeGraphicOpenGLDrawObject,

  sgeEventList, sgeEventBase,
  sgeEventWindowSize,
  sgeEventGraphicShaderAdd,
  sgeEventGraphicFade, sgeEventGraphicFadeAdd,
  sgeEventGraphicLayerAdd, sgeEventGraphicLayerDelete, sgeEventGraphicLayerUpdate,
  sgeEventGraphicElementDelete, sgeEventGraphicElementVisible, sgeEventGraphicElementClipRect,
  sgeEventGraphicElementUpdate, sgeEventGraphicElementAdd,

  sgeExtensionWindow;


const
  Extension_Graphic = 'Graphic';


type
  //Режим ограничения кадров
  TsgeExtensionGraphicDrawControl = (
    gdcSync,    //Вертикальная синхронизация
    gdcProgram  //Програмный способ
  );


  TsgeExtensionGraphic = class(TsgeExtensionBase)
  private
    //Ссылки на объекты
    FExtWindow: TsgeExtensionWindow;
    FFont: TsgeAnsiFont;                            //Ссылка на класс шрифта

    //Классы
    FGraphic: TsgeGraphicOpenGL;                    //Класс графики для потока рендера сцены
    FThread: TsgeThread;                            //Поток основного класса графики
    FEventList: TsgeEventList;                      //Список объектов событий
    FLayerList: TsgeGraphicOpenGLLayerList;         //Список слоёв отрисовки
    FFadeElement: TsgeGraphicOpenGLDrawObjectFade;  //Элемент затемнения

    FDisplayFPS: TsgeDisplayElementAnsiText;        //Элемент рисования
    FDrawFPS: TsgeGraphicOpenGLDrawObjectAnsiText;  //Элемент рендера
    FFPSCounter: TsgeCounter;                       //Счётчик FPS

    //Перменные
    FScreenSize: TsgeFloatPoint;                    //Размеры экрана
    FDrawControl: TsgeExtensionGraphicDrawControl;  //Способ ограничения кадров
    FMaxFPS: Word;                                  //Максимальное количество кадров в секунду
    FAutoEraseBG: Boolean;                          //Автостирание фона перед выводом кадра
    FDestroying: Boolean;                           //Флаг уничтожения объекта

    //Вспомогательные параметры
    FDrawLastTime: Int64;
    FDrawCurrentTime: Int64;
    FDrawDelay: Int64;

    FScreenDataStream: TsgeMemoryStream;            //Ссылка на память для точек
    FScreenDataWidth: Integer;                      //Ширина скриншота
    FScreenDataHeight: Integer;                     //Высота скриншота

    //Методы потока
    procedure InitGraphic;
    procedure DoneGraphic;
    procedure CreateObjects;
    procedure ClearDrawObjects;
    procedure ClearLayers;

    procedure ProcessEvents;
    procedure ProcessEvent_WindowResize(Event: TsgeEventWindowSize);
    procedure ProcessEvent_ShaderAdd(Event: TsgeEventGraphicShaderAdd);
    procedure ProcessEvent_FadeNew(Event: TsgeEventGraphicFadeAdd);
    procedure ProcessEvent_LayerAdd(Event: TsgeEventGraphicLayerAdd);
    procedure ProcessEvent_LayerUpdate(Event: TsgeEventGraphicLayerUpdate);
    procedure ProcessEvent_LayerDelete(Event: TsgeEventGraphicLayerDelete);
    procedure ProcessEvent_ItemAdd(Event: TsgeEventGraphicElementAdd);
    procedure ProcessEvent_ItemUpdate(Event: TsgeEventGraphicElementUpdate);
    procedure ProcessEvent_ItemDelete(Event: TsgeEventGraphicElementDelete);
    procedure ProcessEvent_ItemVisible(Event: TsgeEventGraphicElementVisible);
    procedure ProcessEvent_ItemClipRect(Event: TsgeEventGraphicElementClipRect);

    procedure ChangeDrawControl;
    procedure GetScreenData;

    procedure SystemDraw;
    procedure Draw;
    procedure DrawElements;
    procedure ProcessFPS;
    procedure DrawFPS;

    //Свойства
    procedure SetDrawControl(AMetod: TsgeExtensionGraphicDrawControl);
    procedure SetMaxFPS(AMaxFPS: Word);
    procedure SetEnableFPS(AEnable: Boolean);
    function  GetEnableFPS: Boolean;

    //Затемнение
    procedure FadeCallBackProc(Time: TsgePassedTime; ID: Integer);

    //Подписка на события
    function EventHandler(Obj: TsgeEventBase): TsgeEventHandlerResult;

    //Вспомогательные методы
    function CreateDrawObjectByDisplayElement(DisplayElement: TsgeDisplayElement): TsgeGraphicOpenGLDrawObject;

  protected
    function GetName: String; override;
    procedure RegisterEventHandlers; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    //Методы
    procedure Init;                                                           //Создние необходимых объектов
    procedure AddShader(ShaderName: String; ShaderStream: TsgeMemoryStream);  //Добавить шейдерную программу
    procedure SetSystemFont(Font: TsgeAnsiFont);                              //Установить системный шрифт
    procedure StartRender;                                                    //Запуск потока рендерера

    procedure Fade(Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer = -1);
    procedure Screenshot(Sprite: TsgeSprite);

    //Параметры
    property EnableFPS: Boolean read GetEnableFPS write SetEnableFPS;
    property MaxFPS: Word read FMaxFPS write SetMaxFPS;
    property DrawControl: TsgeExtensionGraphicDrawControl read FDrawControl write SetDrawControl;
    property AutoEraseBG: Boolean read FAutoEraseBG write FAutoEraseBG;
  end;


implementation

uses
  sgeErrors, sgeOSPlatform,
  sgeGraphicOpenGLShaderProgram,
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLLayerTable, sgeGraphicOpenGLDrawObjectTable,
  sgeGraphicOpenGLLayer, sgeGraphicOpenGLTypes,
  sgeDisplayElementRect, sgeGraphicOpenGLDrawObjectRect,
  sgeDisplayElementFrame, sgeGraphicOpenGLDrawObjectFrame,
  sgeDisplayElementSprite, sgeGraphicOpenGLDrawObjectSprite,
  sgeDisplayElementSpritePart, sgeGraphicOpenGLDrawObjectSpritePart,
  sgeDisplayElementSpriteTile, sgeGraphicOpenGLDrawObjectSpriteTile,
  sgeDisplayElementSpriteNine, sgeGraphicOpenGLDrawObjectSpriteNine,
  sgeDisplayElementAnimation, sgeGraphicOpenGLDrawObjectAnimation,
  sgeDisplayElementAnimationUnmanaged, sgeGraphicOpenGLDrawObjectAnimationUnamnaged;


const
  _UNITNAME = 'ExtensionGraphic';

  Err_EventError           = 'EventError';
  Err_LayerNotFound        = 'LayerNotFound';
  Err_CantCreateDrawObject = 'CantCreateDrawObject';
  Err_EmptySprite          = 'EmptySprite';


procedure TsgeExtensionGraphic.InitGraphic;
begin
  FGraphic.Activate;
  FGraphic.ChangeViewPort(0, 0);
end;


procedure TsgeExtensionGraphic.DoneGraphic;
begin
  //Удалить объекты в слоях
  ClearDrawObjects;

  //Удалить слои
  ClearLayers;

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


procedure TsgeExtensionGraphic.ClearDrawObjects;
var
  i, j: Integer;
  Layer: TsgeGraphicOpenGLLayer;
begin
  for i := 0 to FLayerList.Count - 1 do
  begin
    //Ссылка на слой
    Layer := FLayerList.Item[i];

    //Удалить отображаемые объекты
    for j := 0 to Layer.Items.Count - 1 do
      Layer.Items.Item[j].Free;

    //Очистить список с элементами
    Layer.Items.Clear;
  end;
end;


procedure TsgeExtensionGraphic.ClearLayers;
var
  i: Integer;
begin
  for i := 0 to FLayerList.Count - 1 do
    FLayerList.Item[i].Free;

  //Очистить список слоев
  FLayerList.Clear;
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
          ProcessEvent_WindowResize(TsgeEventWindowSize(Event));

        //Добавление нового шейдера
        Event_Graphic_ShaderAdd:
          ProcessEvent_ShaderAdd(TsgeEventGraphicShaderAdd(Event));

        //Добавление нового затемнения
        Event_Graphic_FadeAdd:
          ProcessEvent_FadeNew(TsgeEventGraphicFadeAdd(Event));

        //Добавление нового слоя
        Event_Graphic_LayerAdd:
          ProcessEvent_LayerAdd(TsgeEventGraphicLayerAdd(Event));

        //Изменение слоя
        Event_Graphic_LayerUpdate:
          ProcessEvent_LayerUpdate(TsgeEventGraphicLayerUpdate(Event));

        //Удаление слоя
        Event_Graphic_LayerDelete:
          ProcessEvent_LayerDelete(TsgeEventGraphicLayerDelete(Event));

        //Добавление элемента
        Event_Graphic_ItemAdd:
          ProcessEvent_ItemAdd(TsgeEventGraphicElementAdd(Event));

        //Изменение элемента
        Event_Graphic_ItemUpdate:
          ProcessEvent_ItemUpdate(TsgeEventGraphicElementUpdate(Event));

        //Удаление элемента
        Event_Graphic_ItemDelete:
          ProcessEvent_ItemDelete(TsgeEventGraphicElementDelete(Event));

        //Изменение видимости элемента
        Event_Graphic_ItemVisible:
          ProcessEvent_ItemVisible(TsgeEventGraphicElementVisible(Event));

        //Режим обрезки
        Event_Graphic_ItemClipRect:
          ProcessEvent_ItemClipRect(TsgeEventGraphicElementClipRect(Event));
      end;

    except
      on E: EsgeException do
        ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_EventError, Event.Name, E.Message));
    end;

    //Удалить первый элемент
    FEventList.Delete(0);
  end;
end;


procedure TsgeExtensionGraphic.ProcessEvent_WindowResize(Event: TsgeEventWindowSize);
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


procedure TsgeExtensionGraphic.ProcessEvent_FadeNew(Event: TsgeEventGraphicFadeAdd);
begin
  FFadeElement.Add(Event.Mode, Event.Color, Event.Time, Event.ID, Event.TimeProc);
end;


procedure TsgeExtensionGraphic.ProcessEvent_LayerAdd(Event: TsgeEventGraphicLayerAdd);
var
  Layer: TsgeGraphicOpenGLLayer;
begin
  //Создать слой
  Layer := TsgeGraphicOpenGLLayer.Create(Event.Layer);

  //Добавить слой в список слоев
  FLayerList.Add(Layer);

  //Добавить слой в хэштаблицу
  OpenGLLayerTable.Add(Event.UniqueID, Layer);

  //Удалить копию объекта
  Event.Layer.Free;
end;


procedure TsgeExtensionGraphic.ProcessEvent_LayerUpdate(Event: TsgeEventGraphicLayerUpdate);
var
  Layer: TsgeGraphicOpenGLLayer;
begin
  //Найти слой по ID
  Layer := OpenGLLayerTable.Get(Event.UniqueID);

  //Обновить слой
  Layer.Update(Event.Layer);

  //Удалить копию объекта
  Event.Layer.Free;
end;


procedure TsgeExtensionGraphic.ProcessEvent_LayerDelete(Event: TsgeEventGraphicLayerDelete);
var
  LayerName: String;
begin
  //Получить имя удаляемого слоя
  LayerName := OpenGLLayerTable.Get(Event.UniqueID).Name;

  //Удалить слой из списка
  FLayerList.Delete(LayerName);

  //Удалить слой из таблицы
  OpenGLLayerTable.Delete(Event.UniqueID);
end;


procedure TsgeExtensionGraphic.ProcessEvent_ItemAdd(Event: TsgeEventGraphicElementAdd);
var
  LayerIdx: Integer;
  DrawObject: TsgeGraphicOpenGLDrawObject;
begin
  try
    //Найти слой
    LayerIdx := FLayerList.IndexOf(Event.LayerName);
    if LayerIdx = -1 then
    begin
      ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_LayerNotFound, Event.LayerName));
      Exit;
    end;

    //Создать объект
    try
      DrawObject := CreateDrawObjectByDisplayElement(Event.Item);
    except
      on E: EsgeException do
      begin
        ErrorManager.ProcessError(E.Message);
        Exit;
      end;
    end;

    //Добавить объект на слой
    FLayerList.Item[LayerIdx].Items.Add(DrawObject);

    //Добавить объект в таблицу для быстрого поиска
    OpenGLDrawObjectTable.Add(Event.UniqueID, DrawObject);

  finally
    //Удалить копию
    Event.Item.Free;
  end;
end;


procedure TsgeExtensionGraphic.ProcessEvent_ItemUpdate(Event: TsgeEventGraphicElementUpdate);
var
  DrawObject: TsgeGraphicOpenGLDrawObject;
begin
  //Найти объект по ID
  DrawObject := OpenGLDrawObjectTable.Get(Event.UniqueID);

  //Обновить
  DrawObject.Update(Event.Item);

  //Удалить копию
  Event.Item.Free;
end;


procedure TsgeExtensionGraphic.ProcessEvent_ItemDelete(Event: TsgeEventGraphicElementDelete);
var
  DrawObject: TsgeGraphicOpenGLDrawObject;
begin
  //Найти объект по ID
  DrawObject := OpenGLDrawObjectTable.Get(Event.UniqueID);

  //Удалить объект из слоя
  FLayerList.DeleteDrawObject(DrawObject);

  //Удалить из таблицы
  OpenGLDrawObjectTable.Delete(Event.UniqueID);
end;


procedure TsgeExtensionGraphic.ProcessEvent_ItemVisible(Event: TsgeEventGraphicElementVisible);
var
  DrawObject: TsgeGraphicOpenGLDrawObject;
begin
  //Найти объект по ID
  DrawObject := OpenGLDrawObjectTable.Get(Event.UniqueID);

  //Изменить видимость
  DrawObject.Visible := Event.Visible;
end;


procedure TsgeExtensionGraphic.ProcessEvent_ItemClipRect(Event: TsgeEventGraphicElementClipRect);
var
  DrawObject: TsgeGraphicOpenGLDrawObject;
begin
  //Найти объект по ID
  DrawObject := OpenGLDrawObjectTable.Get(Event.UniqueID);

  //Изменить обрезку
  DrawObject.Clipped := Event.Clipped;
  DrawObject.ClipRect := Event.ClipRect;
end;


procedure TsgeExtensionGraphic.ChangeDrawControl;
begin
  FGraphic.VerticalSync := FDrawControl = gdcSync;
end;


procedure TsgeExtensionGraphic.GetScreenData;
begin
  FGraphic.GetScreenData(FScreenDataStream, FScreenDataWidth, FScreenDataHeight);
end;


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
    gdcSync:
      Draw;

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
  DrawElements;

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
var
  I: Integer;
  DrawObject: TsgeGraphicOpenGLDrawObject;
  Layer: TsgeGraphicOpenGLLayer;
  LayerInfo: TsgeLayerInfo;
begin
  //Вывод слоёв
  for I := 0 to FLayerList.Count - 1 do
  begin
    //Если уничтожение, то не рисовать
    if FDestroying then
      Exit;

    //Ссылка на слой
    Layer := FLayerList.Item[I];

    //Информация о слое
    LayerInfo := Layer.LayerInfo;

    //Не рисовать невидимые слои
    if not Layer.Visible then
      Continue;

    //Обработать элементы в слое
    DrawObject := Layer.Items.GetFirst;
    while DrawObject <> nil do
    begin
      //Вывести элемент, если он видимый
      if DrawObject.Visible then
      begin

        //Проверить режим обрезки
        if DrawObject.Clipped then
        begin
          FGraphic.Enable(gcScissor);
          FGraphic.SetScissor(
            DrawObject.ClipRect.X,
            DrawObject.ClipRect.Y,
            DrawObject.ClipRect.Width,
            DrawObject.ClipRect.Height
          );
        end;

        //Вывод элемента
        DrawObject.Draw(FGraphic, FScreenSize, LayerInfo);

        //Отключить обрезку
        if DrawObject.Clipped then
          FGraphic.Disable(gcScissor);

      end;

      //Следующий элемент
      DrawObject := Layer.Items.GetNext;
    end;
  end;
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
  LayerInfo: TsgeLayerInfo = (PosX: 0; PosY: 0; ScaleX: 1; ScaleY: 1);
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


procedure TsgeExtensionGraphic.SetEnableFPS(AEnable: Boolean);
begin
  if FDrawFPS.Visible = AEnable then
    Exit;

  FDrawFPS.Visible := AEnable;
end;


function TsgeExtensionGraphic.GetEnableFPS: Boolean;
begin
  Result := FDrawFPS.Visible;
end;


procedure TsgeExtensionGraphic.FadeCallBackProc(Time: TsgePassedTime; ID: Integer);
var
  Event: TsgeEventBase;
begin
  //Создать событие смены состояния затемнения
  Event := TsgeEventGraphicFade.Create(Time, ID);

  //Опубликовать событие смены времени затемнения
  EventManager.Publish(Event);
end;


function TsgeExtensionGraphic.EventHandler(Obj: TsgeEventBase): TsgeEventHandlerResult;
begin
  Result := ehrNormal;
  FEventList.Add(Obj.Copy);
end;


function TsgeExtensionGraphic.CreateDrawObjectByDisplayElement(DisplayElement: TsgeDisplayElement): TsgeGraphicOpenGLDrawObject;
begin
  Result := nil;

  //Rect
  if DisplayElement is TsgeDisplayElementRect then
    Result := TsgeGraphicOpenGLDrawObjectRect.Create(DisplayElement);

  //Frame
  if DisplayElement is TsgeDisplayElementFrame then
    Result := TsgeGraphicOpenGLDrawObjectFrame.Create(DisplayElement);

  //Sprite
  if DisplayElement is TsgeDisplayElementSprite then
    Result := TsgeGraphicOpenGLDrawObjectSprite.Create(DisplayElement);

  //SpritePart
  if DisplayElement is TsgeDisplayElementSpritePart then
    Result := TsgeGraphicOpenGLDrawObjectSpritePart.Create(DisplayElement);

  //SpriteTile
  if DisplayElement is TsgeDisplayElementSpriteTile then
    Result := TsgeGraphicOpenGLDrawObjectSpriteTile.Create(DisplayElement);

  //SpriteNine
  if DisplayElement is TsgeDisplayElementSpriteNine then
    Result := TsgeGraphicOpenGLDrawObjectSpriteNine.Create(DisplayElement);

  //Animation
  if DisplayElement is TsgeDisplayElementAnimation then
    Result := TsgeGraphicOpenGLDrawObjectAnimation.Create(DisplayElement);

  //AnimationUnmanaged
  if DisplayElement is TsgeDisplayElementAnimationUnmanaged then
    Result := TsgeGraphicOpenGLDrawObjectAnimationUnamnaged.Create(DisplayElement);

  //AnsiText
  if DisplayElement is TsgeDisplayElementAnsiText then
    Result := TsgeGraphicOpenGLDrawObjectAnsiText.Create(DisplayElement);


  //Ошибка если не получилось создать
  if Result = nil then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateDrawObject, DisplayElement.ClassName);
end;


function TsgeExtensionGraphic.GetName: String;
begin
  Result := Extension_Graphic;
end;


procedure TsgeExtensionGraphic.RegisterEventHandlers;
begin
  //Установить обработчики
  EventManager.SubscriberGroupList.Subscribe(Event_WindowSize, TsgeEventHandler(@EventHandler), Event_Priority_Max - 0, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_ShaderAdd, TsgeEventHandler(@EventHandler), Event_Priority_Max - 1, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_Fade, TsgeEventHandler(@EventHandler), Event_Priority_Max - 2, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_LayerAdd, TsgeEventHandler(@EventHandler), Event_Priority_Max - 3, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_LayerUpdate, TsgeEventHandler(@EventHandler), Event_Priority_Max - 4, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_LayerDelete, TsgeEventHandler(@EventHandler), Event_Priority_Max - 5, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_ItemAdd, TsgeEventHandler(@EventHandler), Event_Priority_Max - 6, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_ItemUpdate, TsgeEventHandler(@EventHandler), Event_Priority_Max - 7, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_ItemDelete, TsgeEventHandler(@EventHandler), Event_Priority_Max - 8, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_ItemVisible, TsgeEventHandler(@EventHandler), Event_Priority_Max - 9, True);
  EventManager.SubscriberGroupList.Subscribe(Event_Graphic_ItemClipRect, TsgeEventHandler(@EventHandler), Event_Priority_Max - 10, True);
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
    FLayerList := TsgeGraphicOpenGLLayerList.Create(False);

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
  FLayerList.Free;
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
  Event := TsgeEventGraphicShaderAdd.Create(ShaderName, ShaderStream);

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


procedure TsgeExtensionGraphic.Fade(Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer);
var
  Event: TsgeEventBase;
begin
  //Создать событие
  Event := TsgeEventGraphicFadeAdd.Create(Mode, Color, Time, ID, @FadeCallBackProc);

  //Добавить в очередь
  FEventList.Add(Event);
end;


procedure TsgeExtensionGraphic.Screenshot(Sprite: TsgeSprite);
var
  Stream: TsgeMemoryStream;
begin
  if not Assigned(Sprite) then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  Stream := TsgeMemoryStream.Create;
  try
    //Установить ссылку на стрим
    FScreenDataStream := Stream;

    //Получить данные из переднего буфера
    FThread.RunProcAndWait(@GetScreenData);

    //Изменить рамер спрайта
    Sprite.SetSize(FScreenDataWidth, FScreenDataHeight);

    //Скопировать данные в спрайт
    Move(Stream.Data^, Sprite.Data^, Stream.Size);

  finally
    FScreenDataStream := nil;
    Stream.Free;
  end;
end;



end.

