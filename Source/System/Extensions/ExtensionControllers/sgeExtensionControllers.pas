{
Пакет             Simple Game Engine 2
Файл              sgeExtensionControllers.pas
Версия            1.6
Создан            20.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Контроллеры
}
{$Include Defines.inc}

unit sgeExtensionControllers;

{$mode objfpc}{$H+}

interface

uses
  sgeExtensionBase,
  sgeThread, sgeControllerList, sgeController;


const
  Extension_Controllers = 'Controllers';


type
  TsgeExtensionControllers = class(TsgeExtensionBase)
  type
    TEventType = (etNone, etMinDown, etMinUp, etMaxDown, etMaxUp, etMinUpMaxDown, etMaxUpMinDown);
  private
    //Классы
    FControllerList: TsgeControllerList;
    FThread: TsgeThread;

    //Параметры
    FEnable: Boolean;             //Активность потока
    FScanDelay: Word;             //Задержка между опросами контроллеров
    FAutoScan: Boolean;           //Автосканирование контроллеров
    FAutoScanDelay: Cardinal;     //Задержка между сканированием новых контроллеров

    //Вспомогательные переменные
    FMaxControllerCount: Byte;
    FLastScanTime: Int64;
    FInnerScanDelay: Cardinal;
    FIdx1, FIdx2: Byte;

    procedure Work;               //Функция потока

    procedure ProcessState;       //Опрос состояния
    procedure ProcessScan;        //Проверка на подключение новых устройств
    procedure ScanControllers;    //Пересканировать устройства
    procedure ChangeControllers;  //Поменять контроллеры местами

    function  GetPadEventType(Current, Previvous: Integer): TEventType;           //Определить тип события на оси
    function  GetAxisEventType(Current, Previvous, Middle: Integer): TEventType;  //Определить тип события на оси

    procedure SetEnable(AEnable: Boolean);
    procedure SetScanDelay(ADelay: Cardinal);
  protected
    function GetName: String; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Change(Idx1, Idx2: Byte);

    property ControllerList: TsgeControllerList read FControllerList;
    property Enable: Boolean read FEnable write SetEnable;
    property ScanDelay: Word read FScanDelay write FScanDelay;
    property AutoScan: Boolean read FAutoScan write FAutoScan;
    property AutoScanDelay: Cardinal read FAutoScanDelay write FAutoScanDelay;
  end;


implementation

uses
  sgeTypes, sgeErrors, sgeOSPlatform, sgeEventController,
  sgeEventControllerAttach, sgeEventControllerDetach, sgeEventControllerButtonUp,
  sgeEventControllerButtonDown, sgeEventControllerPovUp, sgeEventControllerPovDown,
  sgeEventControllerAxisUp, sgeEventControllerAxisDown, sgeEventControllerAxisValue;


const
  _UNITNAME = 'ExtensionControllers';


type
  TsgeControllerHack = class(TsgeController);


procedure TsgeExtensionControllers.Work;
begin
  //Обновить состояние конроллеров
  ProcessState;

  //Сканирование новых контроллеров
  if FAutoScan then
    ProcessScan;

  //Задержка между опросом
  if FScanDelay <> 0 then
    sgeSleep(FScanDelay);
end;


procedure TsgeExtensionControllers.ProcessState;
var
  idx, btnIdx, c: Integer;
  I: TsgeControllerAxisType;
  Joy: TsgeController;
begin
  idx := 0;
  while idx < FControllerList.Count do
  begin
    try
      //Ссылка на контроллер
      Joy := FControllerList.Item[idx];

      //Запросить текущее состояние
      Joy.GetInfo;


      //Проверить Кнопки
      c := Length(Joy.CurrentInfo.Buttons) - 1;
      for btnIdx := 0 to c do
      begin
        //Отпускание кнопки
        if (not Joy.CurrentInfo.Buttons[btnIdx].Down) and Joy.LastInfo.Buttons[btnIdx].Down then
          EventManager.Publish(TsgeEventControllerButtonUp.Create(Idx, btnIdx));

        //Нажатие кнопки
        if Joy.CurrentInfo.Buttons[btnIdx].Down and (not Joy.LastInfo.Buttons[btnIdx].Down) then
          EventManager.Publish(TsgeEventControllerButtonDown.Create(idx, btnIdx));
      end;


      //Проверить изменения POV X
      case GetPadEventType(Joy.CurrentInfo.Pov.X, Joy.LastInfo.Pov.X) of
        etMinDown:
          EventManager.Publish(TsgeEventControllerPovDown.Create(Idx, cpdLeft));

        etMinUp:
          EventManager.Publish(TsgeEventControllerPovUp.Create(Idx, cpdLeft));

        etMaxDown: EventManager.Publish(TsgeEventControllerPovDown.Create(Idx, cpdRight));

        etMaxUp:
          EventManager.Publish(TsgeEventControllerPovUp.Create(Idx, cpdRight));

        etMinUpMaxDown:
        begin
          EventManager.Publish(TsgeEventControllerPovUp.Create(Idx, cpdLeft));
          EventManager.Publish(TsgeEventControllerPovDown.Create(Idx, cpdRight));
        end;

        etMaxUpMinDown:
        begin
          EventManager.Publish(TsgeEventControllerPovUp.Create(Idx, cpdRight));
          EventManager.Publish(TsgeEventControllerPovDown.Create(Idx, cpdLeft));
        end;
      end;

      //Проверить изменения POV Y
      case GetPadEventType(Joy.CurrentInfo.Pov.Y, Joy.LastInfo.Pov.Y) of
        etMinDown:
          EventManager.Publish(TsgeEventControllerPovDown.Create(Idx, cpdDown));

        etMinUp:
          EventManager.Publish(TsgeEventControllerPovUp.Create(Idx, cpdDown));

        etMaxDown:
          EventManager.Publish(TsgeEventControllerPovDown.Create(Idx, cpdUp));

        etMaxUp: EventManager.Publish(TsgeEventControllerPovUp.Create(Idx, cpdUp));

        etMinUpMaxDown:
        begin
          EventManager.Publish(TsgeEventControllerPovUp.Create(Idx, cpdDown));
          EventManager.Publish(TsgeEventControllerPovDown.Create(Idx, cpdUp));
        end;

        etMaxUpMinDown:
        begin
          EventManager.Publish(TsgeEventControllerPovUp.Create(Idx, cpdUp));
          EventManager.Publish(TsgeEventControllerPovDown.Create(Idx, cpdDown));
        end;
      end;


      //Проверить Оси
      for I := Low(TsgeControllerAxisType) to High(TsgeControllerAxisType) do
      begin
        //Наклоны оси
        case GetAxisEventType(Joy.CurrentInfo.Axis[i].RawValue, Joy.LastInfo.Axis[i].RawValue, TsgeControllerHack(Joy).GetAxisRawMiddleValue(I)) of
          etMinDown:
            EventManager.Publish(TsgeEventControllerAxisDown.Create(Idx, I, catMin));

          etMinUp:
            EventManager.Publish(TsgeEventControllerAxisUp.Create(Idx, I, catMin));

          etMaxDown:
            EventManager.Publish(TsgeEventControllerAxisDown.Create(Idx, I, catMax));

          etMaxUp:
            EventManager.Publish(TsgeEventControllerAxisUp.Create(Idx, I, catMax));

          etMinUpMaxDown:
          begin
            EventManager.Publish(TsgeEventControllerAxisUp.Create(Idx, I, catMin));
            EventManager.Publish(TsgeEventControllerAxisDown.Create(Idx, I, catMax));
          end;

          etMaxUpMinDown:
          begin
            EventManager.Publish(TsgeEventControllerAxisUp.Create(Idx, I, catMax));
            EventManager.Publish(TsgeEventControllerAxisDown.Create(Idx, I, catMin));
          end;
        end;

        //Изменение значения
        if Joy.CurrentInfo.Axis[I].Value <> Joy.LastInfo.Axis[I].Value then
          EventManager.Publish(TsgeEventControllerAxisValue.Create(Idx, I,  Joy.CurrentInfo.Axis[I].Value, Joy.LastInfo.Axis[I].Value));
      end;


      //Сохранить текущее состояние
      Joy.SwapInfo;


    except
      //Сказать что отвалился
      EventManager.Publish(TsgeEventControllerDetach.Create(Idx));

      //Убить устройство
      FControllerList.Delete(idx);

      //Перечитать настройки контроллеров из реестра
      sgeControllerConfigChanged;

      //Уменьшить счётчик
      Dec(idx);
    end;

    //Следующее устройство
    Inc(idx);
  end;
end;


procedure TsgeExtensionControllers.ProcessScan;
var
  c, idx: Integer;
  Joy: TsgeController;
  CurrentTime: Int64;
begin
  //Определить время проверки сканирования
  CurrentTime := sgeGetCPUCounter;
  if (CurrentTime - FLastScanTime) / OneSecFrequency <= FInnerScanDelay  then
    Exit;
  FLastScanTime := CurrentTime;


  //Проверить на подключение новых контроллеров
  c := FMaxControllerCount - 1;
  for idx := 0 to c do
  begin
    //Если контроллер существует, то следующий
    if FControllerList.Exist(idx) then
      Continue;

    //Добавить контроллер
    if sgeControllerExist(idx) then
      try
        //Создать контроллер
        Joy := TsgeController.Create(idx);

        //Добавить в список
        FControllerList.Add(Joy);

        //Событие, новый джойстик
        EventManager.Publish(TsgeEventControllerAttach.Create(FControllerList.Count - 1));
      except
      end;
  end;
end;


procedure TsgeExtensionControllers.ScanControllers;
var
  i, cnt: Integer;
  C: TsgeController;
begin
  //Определить максимальное количество контроллеров
  cnt := FMaxControllerCount - 1;

  //Создать возможные контроллеры
  for i := 0 to cnt do
    if sgeControllerExist(i) then
      try
        //Создать контроллер
        C := TsgeController.Create(i);

        //Добавить в массив
        FControllerList.Add(C);
      except
      end;
end;


procedure TsgeExtensionControllers.ChangeControllers;
begin
  FControllerList.Change(FIdx1, FIdx2);
end;


function TsgeExtensionControllers.GetPadEventType(Current, Previvous: Integer): TEventType;
begin
  Result := etNone;
  if (Current = -1) and (Previvous = 0) then
    Result := etMinDown;

  if (Current = 0)  and (Previvous = -1) then
    Result := etMinUp;

  if (Current = 1)  and (Previvous = -1) then
    Result := etMinUpMaxDown;

  if (Current = 1)  and (Previvous = 0) then
    Result := etMaxDown;

  if (Current = 0)  and (Previvous = 1) then
    Result := etMaxUp;

  if (Current = -1) and (Previvous = 1) then
    Result := etMaxUpMinDown;
end;

function TsgeExtensionControllers.GetAxisEventType(Current, Previvous, Middle: Integer): TEventType;
begin
  Result := etNone;
  if (Current < Middle) and (Previvous = Middle) then
    Result := etMinDown;

  if (Current = Middle) and (Previvous < Middle) then
    Result := etMinUp;

  if (Current > Middle) and (Previvous < Middle) then
    Result := etMinUpMaxDown;

  if (Current > Middle) and (Previvous = Middle) then
    Result := etMaxDown;

  if (Current = Middle) and (Previvous > Middle) then
    Result := etMaxUp;

  if (Current < Middle) and (Previvous > Middle) then
    Result := etMaxUpMinDown;
end;


procedure TsgeExtensionControllers.SetEnable(AEnable: Boolean);
begin
  if FEnable = AEnable then
    Exit;

  FEnable := AEnable;

  if FEnable then
    FThread.Resume
  else
    FThread.Suspend;
end;


procedure TsgeExtensionControllers.SetScanDelay(ADelay: Cardinal);
begin
  FAutoScanDelay := ADelay;

  FInnerScanDelay := FAutoScanDelay div 1000;
end;


function TsgeExtensionControllers.GetName: String;
begin
  Result := Extension_Controllers;
end;


constructor TsgeExtensionControllers.Create;
begin
  try
    inherited Create;

    //Классы
    FThread := TsgeThread.Create(Extension_Controllers, nil, True, False);
    FControllerList := TsgeControllerList.Create(True);

    //Параметры
    FEnable := False;
    FScanDelay := 50;
    FAutoScan := False;
    FMaxControllerCount := sgeGetMaxControllerCount;
    FAutoScanDelay := 5000;

    //Определить контроллеры
    FThread.RunProcAndWait(@ScanControllers, tpemSuspend);

    //Установить метод для потока
    FThread.LoopProc := @Work;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionControllers.Destroy;
begin
  //Отписаться от событий
  EventManager.SubscriberGroupList.UnSubscribe(Self);

  //Классы
  FThread.Free;
  FControllerList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionControllers.Change(Idx1, Idx2: Byte);
begin
  //Запомнить параметры
  FIdx1 := Idx1;
  FIdx2 := Idx2;

  //Поменять местами
  FThread.RunProcAndWait(@ChangeControllers);

  //Проверить на ошибки
  if FThread.Exception <> nil then
    raise EsgeException.Create(FThread.Exception.Message);
end;



end.

