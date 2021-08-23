{
Пакет             Simple Game Engine 2
Файл              sgeExtensionGraphic.pas
Версия            1.6
Создан            14.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Графика
}
{$Include Defines.inc}

unit sgeExtensionGraphic;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeThread,
  sgeExtensionBase, sgeGraphicColor, sgeGraphicDrawList, sgeGraphicElementBase, sgeGraphicFPS,
  sgeCounter, sgeScreenFade, sgeEventBase, sgeEventWindow,
  sgeGraphic, sgeExtensionWindow;


const
  Extension_Graphic = 'Graphic';


type
  //Тип перехода затемнения
  TsgeExtensionFadeMode = (efmNormalToColor, efmColorToNormal, efmNormalToColorToNormal, efmColorToNormalToColor);


  //Режим ограничения кадров (Вертикальная синхронизация, Програмный способ)
  TsgeExtensionGraphicDrawControl = (gdcSync, gdcProgram);


  TsgeExtensionGraphic = class(TsgeExtensionBase)
  private
    //Ссылки на объекты
    FExtWindow: TsgeExtensionWindow;

    //Классы
    FDrawList: TsgeGraphicDrawList;                                 //Класс слоёв отрисовки
    FGraphicInner: TsgeGraphic;                                     //Класс графики для потока рендера сцены
    FGraphic: TsgeGraphic;                                          //Класс графики для основного потока
    FThread: TsgeThread;                                            //Поток основного класса графики
    FFPS: TsgeGraphicFPS;                                           //Настройки вывода FPS
    FFPSCounter: TsgeCounter;                                       //Счётчик FPS
    FFade: TsgeScreenFade;                                          //Затемнение экрана

    //Перменные
    FDrawControl: TsgeExtensionGraphicDrawControl;                  //Способ ограничения кадров
    FMaxFPS: Word;                                                  //Максимальное количество кадров в секунду
    FAutoEraseBG: Boolean;                                          //Автостирание фона перед выводом кадра

    //Вспомогательные параметры
    FDrawLastTime: Int64;
    FDrawCurrentTime: Int64;
    FDrawDelay: Int64;

    //FChangeViewArea: Boolean;
    FNewWidth: Integer;
    FNewHeight: Integer;

    //Методы потока
    procedure InitGraphic;
    procedure DoneGraphic;
    procedure ChangeSize;
    procedure ChangeDrawControl;
    procedure SystemDraw;
    procedure Draw;

    //Вывод графики
    procedure DrawElements;
    procedure DrawFPS;
    procedure DrawFade;

    //Свойства
    procedure SetDrawControl(AMetod: TsgeExtensionGraphicDrawControl);
    procedure SetMaxFPS(AMaxFPS: Word);

    //Затемнение
    procedure FadeCallBackProc(Time: TsgePassedTime);

    //Подписка на события
    function Event_WindowResize(Obj: TsgeEventWindowSize): Boolean;

  protected
    FGraphicShell: TsgeGraphic;                                     //Класс графики для потока оболочки

    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    //Методы
    procedure Fade(Mode: TsgeExtensionFadeMode; Color: TsgeColor; Time: Cardinal);

    //Объекты
    property Graphic: TsgeGraphic read FGraphic;
    property DrawList: TsgeGraphicDrawList read FDrawList;
    property FPS: TsgeGraphicFPS read FFPS;

    //Параметры
    property MaxFPS: Word read FMaxFPS write SetMaxFPS;
    property DrawControl: TsgeExtensionGraphicDrawControl read FDrawControl write SetDrawControl;
    property AutoEraseBG: Boolean read FAutoEraseBG write FAutoEraseBG;
  end;




implementation

uses
  sgeErrors, sgeOSPlatform,
  sgeEventGraphic, sgeGraphicElementLayer;


const
  _UNITNAME = 'ExtensionGraphic';


procedure TsgeExtensionGraphic.InitGraphic;
begin
  FGraphicInner.Init;
  FGraphicInner.Activate;
end;


procedure TsgeExtensionGraphic.DoneGraphic;
begin
  FGraphicInner.Done;
end;


procedure TsgeExtensionGraphic.ChangeSize;
begin
  FGraphicInner.ChangeViewArea(FNewWidth, FNewHeight);
end;


procedure TsgeExtensionGraphic.ChangeDrawControl;
begin
  FGraphicInner.VerticalSync := FDrawControl = gdcSync;
end;


procedure TsgeExtensionGraphic.SystemDraw;
begin
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
  //Увеличить счётчик кадров
  FFPSCounter.Inc;

  //Стереть фон
  if FAutoEraseBG then FGraphic.EraseBG;

  //Вывод елементов
  DrawElements;

  //Вывод затемнения
  if FFade.Enable then DrawFade;

  //Вывод FPS
  if FFPS.Enable then DrawFPS;

  //Смена кадров
  case FGraphic.RenderBuffer of
    grbBack : FGraphicInner.SwapBuffers;
    grbFront: FGraphicInner.Finish;
  end;
end;


procedure TsgeExtensionGraphic.DrawElements;
var
  I: Integer;
  El: TsgeGraphicElementBase;
  Layer: TsgeGraphicElementLayer;
begin
  //Заблокировать список
  FDrawList.Lock;

  //Вывод слоёв
  for I := 0 to FDrawList.LayerList.Count - 1 do
    begin
    //Ссылка на слой
    Layer := FDrawList.LayerList.Item[I];

    //Проверить видимость слоя
    if not Layer.Visible then Continue;

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

      //Обновить данные
      if El.NeedUpdate then El.ApplySettings;

      //Нарисовать элемент
      if El.Visible then El.Draw(FGraphicInner);

      //Следующий элемент
      El := Layer.Elements.GetNext;
      end;

    end;

  //Разблокировать список
  FDrawList.UnLock;
end;


procedure TsgeExtensionGraphic.DrawFPS;
var
  X, Y: Integer;
  TxtW, TxtH: Integer;
  s: String;
begin
  s := FFPSCounter.StrCount;

  //Размеры текста
  TxtW := FFPS.Font.GetStringWidth(s);
  TxtH := FFPS.Font.CharHeight;

  //Вертикальное выравнивание
  case FFPS.VerticalAlign of
    vaTop   : Y := 0;
    vaCenter: Y := FExtWindow.Window.Height div 2 - TxtH div 2;
    vaBottom: Y := FExtWindow.Window.Height - TxtH;
  end;

  //Горизонтальное выравнивание
  case FFPS.HorizontalAlign of
    haLeft  : X := 0;
    haCenter: X := FExtWindow.Window.Width div 2 - TxtW div 2;
    haRight : X := FExtWindow.Window.Width - TxtW;
  end;

  //Смещение
  X := X + FFPS.XOffset;
  Y := Y + FFPS.YOffset;

  //Вывод FPS
  FGraphicInner.Color := FFPS.Color;
  FGraphicInner.DrawText(X, Y, FFPS.Font, s);
end;


procedure TsgeExtensionGraphic.DrawFade;
begin
  //Подготовить графику
  FGraphicInner.PushAttrib;
  FGraphicInner.ResetDrawOptions;
  FGraphicInner.PoligonMode := gpmFill;
  FGraphicInner.ColorBlend := True;

  //Вывод
  FGraphicInner.Color := FFade.GetColor;
  FGraphicInner.doCoordinateType := gctClassic;
  FGraphicInner.DrawRect(0, 0, FGraphic.Width, FGraphic.Height);
  FGraphicInner.ResetDrawOptions;

  //Восстановить графику
  FGraphicInner.PopAttrib;
end;


procedure TsgeExtensionGraphic.SetDrawControl(AMetod: TsgeExtensionGraphicDrawControl);
begin
  if FDrawControl = AMetod then Exit;

  FDrawControl := AMetod;
  FThread.RunProcAndWait(@ChangeDrawControl);

  //Проверить действие в другом потоке на ошибки
  if FThread.Exception <> nil then
    raise EsgeException.Create(FThread.Exception.Message);
end;


procedure TsgeExtensionGraphic.SetMaxFPS(AMaxFPS: Word);
begin
  if AMaxFPS = 0 then AMaxFPS := 1;
  if FMaxFPS = AMaxFPS then Exit;

  FMaxFPS := AMaxFPS;
  FDrawDelay := Round(OneSecFrequency / FMaxFPS);
end;


procedure TsgeExtensionGraphic.FadeCallBackProc(Time: TsgePassedTime);
begin
  EventManager.Publish(Event_GraphicFade, TsgeEventGraphicFade.Create(Time));
end;


function TsgeExtensionGraphic.Event_WindowResize(Obj: TsgeEventWindowSize): Boolean;
begin
  Result := False;

  //Сохранить новые размеры
  FNewWidth := Obj.Width;
  FNewHeight := Obj.Height;

  //Изменить размер контекста основного потока
  FGraphic.ChangeViewArea(FNewWidth, FNewHeight);

  //Изменить размер контекста внутреннего потока
  if FGraphicInner <> nil then FThread.RunProc(@ChangeSize);
end;


class function TsgeExtensionGraphic.GetName: String;
begin
  Result := Extension_Graphic;
end;


constructor TsgeExtensionGraphic.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Параметры
    FDrawControl := gdcSync;
    FAutoEraseBG := True;
    SetMaxFPS(120);

    //Получить ссылки на объекты
    FExtWindow := TsgeExtensionWindow(GetExtension(Extension_Window));

    //Контекст графики
    FGraphicInner := TsgeGraphic.Create(FExtWindow.Window.DC, FExtWindow.Window.Width, FExtWindow.Window.Height);
    FGraphicShell := TsgeGraphic.Create(FExtWindow.Window.DC, FExtWindow.Window.Width, FExtWindow.Window.Height);
    FGraphic := TsgeGraphic.Create(FExtWindow.Window.DC, FExtWindow.Window.Width, FExtWindow.Window.Height);

    //Расшарить ресурсы между контекстами
    FGraphicInner.ShareList(FGraphic.Context);
    FGraphicInner.ShareList(FGraphicShell.Context);

    //Создать поток
    FThread := TsgeThread.Create(nil, True, False);

    //Настроить основной контекст графики
    FThread.RunProcAndWait(@InitGraphic);

    //Проверить создание графики на ошибку
    if FThread.Exception <> nil then
      raise EsgeException.Create(FThread.Exception.Message);

    //Активировать контекст основного потока
    FGraphic.Init;
    FGraphic.Activate;

    //Создать объекты
    FDrawList := TsgeGraphicDrawList.Create;
    FFPS := TsgeGraphicFPS.Create;
    FFPSCounter := TsgeCounter.Create(1000);
    FFade := TsgeScreenFade.Create;

    //Установить обработчик изменения размеров окна
    EventManager.Subscribe(Event_WindowSize, TsgeEventHandler(@Event_WindowResize), $FFFF, True);

    //Установить метод отрисовки
    FThread.LoopProc := @SystemDraw;
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionGraphic.Destroy;
begin
  //Отписаться от событий
  EventManager.UnSubscribe(Self);

  //Прибить поток
  if FThread <> nil then
    begin
    FThread.LoopProc := nil;              //Убрать метод отрисовки
    FThread.RunProcAndWait(@DoneGraphic); //Деактивировать контекст
    end;

  //Удалить объекты
  FThread.Free;
  FGraphicShell.Free;
  FGraphic.Done;
  FGraphic.Free;
  FGraphicInner.Free;
  FDrawList.Free;
  FFPS.Free;
  FFPSCounter.Free;
  FFade.Free;

  inherited Destroy;
end;

procedure TsgeExtensionGraphic.Fade(Mode: TsgeExtensionFadeMode; Color: TsgeColor; Time: Cardinal);
begin
  //Запустить затемнение
  FFade.Start(TsgeScreenFadeMode(Mode), Color, Time, @FadeCallBackProc);
end;


end.

