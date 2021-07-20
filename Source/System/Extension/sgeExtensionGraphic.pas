{
Пакет             Simple Game Engine 2
Файл              sgeExtensionGraphic.pas
Версия            1.5
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
  sgeGraphic, sgeGraphicBase, sgeExtensionWindow;


const
  Extension_Graphic = 'Graphic';


type
  //Тип перехода затемнения
  TsgeExtensionFadeMode = (efmNormalToColor, efmColorToNormal, efmNormalToColorToNormal, efmColorToNormalToColor);


  //Режим ограничения кадров (Вертикальная синхронизация, Програмный способ)
  TsgeExtensionGraphicDrawControl = (gdcSync, gdcProgram);


  //Метод
  TsgeExtensionGraphicDrawProc = procedure of object;


  TsgeExtensionGraphic = class(TsgeExtensionBase)
  private
    //Ссылки на объекты
    FExtWindow: TsgeExtensionWindow;

    //Классы
    FDrawList: TsgeGraphicDrawList;                               //Класс слоёв отрисовки
    FGraphic: TsgeGraphic;                                        //Основной класс графики
    FThread: TsgeThread;                                          //Поток основного класса графики
    FContext: TsgeGraphicBase;                                    //Дополнительный контекст для связки ресурсов из разных потоков
    FFPS: TsgeGraphicFPS;                                         //Настройки вывода FPS
    FFPSCounter: TsgeCounter;                                     //Счётчик FPS
    FFade: TsgeScreenFade;                                        //Затемнение экрана

    //Перменные
    FDrawControl: TsgeExtensionGraphicDrawControl;                //Способ ограничения кадров
    FMaxFPS: Word;                                                //Максимальное количество кадров в секунду
    FAutoEraseBG: Boolean;                                        //Автостирание фона перед выводом кадра
    FDrawShellProc: TsgeExtensionGraphicDrawProc;                 //Ссылка на метод отрисовки оболочки

    //Вспомогательные параметры
    FDrawLastTime: Int64;
    FDrawCurrentTime: Int64;
    FDrawDelay: Int64;
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
    procedure DrawShell;

    //Свойства
    procedure SetDrawControl(AMetod: TsgeExtensionGraphicDrawControl);
    procedure SetMaxFPS(AMaxFPS: Word);

    //Затемнение
    procedure FadeCallBackProc(Time: TsgePassedTime);

    //Подписка на события
    function Event_WindowResize(Obj: TsgeEventWindowSize): Boolean;

  protected
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
    property DrawShellproc: TsgeExtensionGraphicDrawProc read FDrawShellProc write FDrawShellProc;
  end;




implementation

uses
  sgeErrors, sgeOSPlatform,
  sgeEventGraphic, sgeGraphicElementLayer;


const
  _UNITNAME = 'ExtensionGraphic';


procedure TsgeExtensionGraphic.InitGraphic;
begin
  FGraphic.Init;
  FGraphic.Activate;
end;


procedure TsgeExtensionGraphic.DoneGraphic;
begin
  FGraphic.Deactivate;
end;


procedure TsgeExtensionGraphic.ChangeSize;
begin
  FGraphic.ChangeViewArea(FNewWidth, FNewHeight);
end;


procedure TsgeExtensionGraphic.ChangeDrawControl;
begin
  FGraphic.VerticalSync := FDrawControl = gdcSync;
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

  //Вывод оболочки
  DrawShell;

  //Смена кадров
  case FGraphic.RenderBuffer of
    grbBack : FGraphic.SwapBuffers;
    grbFront: FGraphic.Finish;
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
      if El.Visible then El.Draw(FGraphic);

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
  TxtH := FFPS.Font.GetStringHeight(s);

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
  FGraphic.Color := FFPS.Color;
  FGraphic.DrawText(X, Y, FFPS.Font, s);
end;


procedure TsgeExtensionGraphic.DrawFade;
begin
  //Подготовить графику
  FGraphic.PushAttrib;
  FGraphic.ResetDrawOptions;
  FGraphic.PoligonMode := gpmFill;
  FGraphic.ColorBlend := True;

  //Вывод
  FGraphic.Color := FFade.GetColor;
  FGraphic.doCoordinateType := gctClassic;
  FGraphic.DrawRect(0, 0, FGraphic.Width, FGraphic.Height);
  FGraphic.ResetDrawOptions;

  //Восстановить графику
  FGraphic.PopAttrib;
end;


procedure TsgeExtensionGraphic.DrawShell;
begin
  if Assigned(FDrawShellProc) then DrawShellproc;
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
  Result := True;

  if FGraphic <> nil then
    begin
    FNewWidth := Obj.Width;
    FNewHeight := Obj.Height;
    FThread.RunProc(@ChangeSize);
    end;
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
    FGraphic := TsgeGraphic.Create(FExtWindow.Window.DC, FExtWindow.Window.Width, FExtWindow.Window.Height);  //Создать основной контекст графики
    FContext := TsgeGraphicBase.Create(FExtWindow.Window.DC, 0, 0);                 //Создать контекст для основного потока
    FGraphic.ShareList(FContext.Context);                                           //Расшарить ресурсы между контекстами
    FContext.Activate;                                                              //Активировать контекст основного потока

    //Создать поток
    FThread := TsgeThread.Create(nil, True, False);

    //Настроить основной контекст графики
    FThread.RunProcAndWait(@InitGraphic);

    //Проверить создание графики на ошибку
    if FThread.Exception <> nil then
      raise EsgeException.Create(FThread.Exception.Message);

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
  FGraphic.Free;
  FContext.Free;
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

