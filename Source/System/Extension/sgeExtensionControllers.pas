{
Пакет             Simple Game Engine 2
Файл              sgeExtensionControllers.pas
Версия            1.4
Создан            20.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Контроллеры
}

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
    FEnable: Boolean;                                               //Активность потока
    FScanDelay: Word;                                               //Задержка между опросами контроллеров
    FAutoScan: Boolean;                                             //Автосканирование контроллеров
    FAutoScanInterval: Cardinal;                                    //Интервал сканирования новых контроллеров

    //Вспомогательные переменные
    FMaxControllerCount: Byte;
    FLastScanTime: Int64;
    FInnerScanDelay: Cardinal;
    FIdx1, FIdx2: Byte;

    procedure Work;                                                 //Функция потока

    procedure ProcessState;                                         //Опрос состояния
    procedure ProcessScan;                                          //Проверка на подключение новых устройств
    procedure ScanControllers;                                      //Пересканировать устройства
    procedure ChangeControllers;                                    //Поменять контроллеры местами

    function  GetPadEventType(Current, Previvous: Integer): TEventType;           //Определить тип события на оси
    function  GetAxisEventType(Current, Previvous, Middle: Integer): TEventType;  //Определить тип события на оси

    procedure SetEnable(AEnable: Boolean);
    procedure SetScanInterval(AInterval: Cardinal);
  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    procedure Change(Idx1, Idx2: Byte);

    property ControllerList: TsgeControllerList read FControllerList;
    property Enable: Boolean read FEnable write SetEnable;
    property ScanDelay: Word read FScanDelay write FScanDelay;
    property AutoScan: Boolean read FAutoScan write FAutoScan;
    property AutoScanInterval: Cardinal read FAutoScanInterval write FAutoScanInterval;
  end;


implementation

uses
  sgeErrors, sgeOSPlatform, sgeEventControllers;


const
  _UNITNAME = 'ExtensionControllers';


type
  TsgeControllerHack = class(TsgeController);


procedure TsgeExtensionControllers.Work;
begin
  //Обновить состояние конроллеров
  ProcessState;

  //Сканирование новых контроллеров
  if FAutoScan then ProcessScan;

  //Задержка между опросом
  if FScanDelay <> 0 then sgeSleep(FScanDelay);
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
            EventManager.Publish(Event_ControllerButtonUp, TsgeEventControllerButton.Create(Idx, btnIdx));

          //Нажатие кнопки
          if Joy.CurrentInfo.Buttons[btnIdx].Down and (not Joy.LastInfo.Buttons[btnIdx].Down) then
            EventManager.Publish(Event_ControllerButtonDown, TsgeEventControllerButton.Create(idx, btnIdx));
          end;


        //Проверить изменения POV X
        case GetPadEventType(Joy.CurrentInfo.Pov.X, Joy.LastInfo.Pov.X) of
          etMinDown     : EventManager.Publish(Event_ControllerPovDown, TsgeEventControllerPOV.Create(Idx, cpdLeft));
          etMinUp       : EventManager.Publish(Event_ControllerPovUp, TsgeEventControllerPOV.Create(Idx, cpdLeft));
          etMaxDown     : EventManager.Publish(Event_ControllerPovDown, TsgeEventControllerPOV.Create(Idx, cpdRight));
          etMaxUp       : EventManager.Publish(Event_ControllerPovUp, TsgeEventControllerPOV.Create(Idx, cpdRight));
          etMinUpMaxDown:
            begin
            EventManager.Publish(Event_ControllerPovUp, TsgeEventControllerPOV.Create(Idx, cpdLeft));
            EventManager.Publish(Event_ControllerPovDown, TsgeEventControllerPOV.Create(Idx, cpdRight));
            end;
          etMaxUpMinDown:
            begin
            EventManager.Publish(Event_ControllerPovUp, TsgeEventControllerPOV.Create(Idx, cpdRight));
            EventManager.Publish(Event_ControllerPovDown, TsgeEventControllerPOV.Create(Idx, cpdLeft));
            end;
        end;

        //Проверить изменения POV Y
        case GetPadEventType(Joy.CurrentInfo.Pov.Y, Joy.LastInfo.Pov.Y) of
          etMinDown     : EventManager.Publish(Event_ControllerPovDown, TsgeEventControllerPOV.Create(Idx, cpdDown));
          etMinUp       : EventManager.Publish(Event_ControllerPovUp, TsgeEventControllerPOV.Create(Idx, cpdDown));
          etMaxDown     : EventManager.Publish(Event_ControllerPovDown, TsgeEventControllerPOV.Create(Idx, cpdUp));
          etMaxUp       : EventManager.Publish(Event_ControllerPovUp, TsgeEventControllerPOV.Create(Idx, cpdUp));
          etMinUpMaxDown:
            begin
            EventManager.Publish(Event_ControllerPovUp, TsgeEventControllerPOV.Create(Idx, cpdDown));
            EventManager.Publish(Event_ControllerPovDown, TsgeEventControllerPOV.Create(Idx, cpdUp));
            end;
          etMaxUpMinDown:
            begin
            EventManager.Publish(Event_ControllerPovUp, TsgeEventControllerPOV.Create(Idx, cpdUp));
            EventManager.Publish(Event_ControllerPovDown, TsgeEventControllerPOV.Create(Idx, cpdDown));
            end;
        end;


        //Проверить Оси
        for I := Low(TsgeControllerAxisType) to High(TsgeControllerAxisType) do
          begin
          //Наклоны оси
          case GetAxisEventType(Joy.CurrentInfo.Axis[i].RawValue, Joy.LastInfo.Axis[i].RawValue, TsgeControllerHack(Joy).GetAxisRawMiddleValue(I)) of
            etMinDown     : EventManager.Publish(Event_ControllerAxisDown, TsgeEventControllerAxis.Create(Idx, I, catMin));
            etMinUp       : EventManager.Publish(Event_ControllerAxisUp, TsgeEventControllerAxis.Create(Idx, I, catMin));
            etMaxDown     : EventManager.Publish(Event_ControllerAxisDown, TsgeEventControllerAxis.Create(Idx, I, catMax));
            etMaxUp       : EventManager.Publish(Event_ControllerAxisUp, TsgeEventControllerAxis.Create(Idx, I, catMax));
            etMinUpMaxDown:
              begin
              EventManager.Publish(Event_ControllerAxisUp, TsgeEventControllerAxis.Create(Idx, I, catMin));
              EventManager.Publish(Event_ControllerAxisDown, TsgeEventControllerAxis.Create(Idx, I, catMax));
              end;
            etMaxUpMinDown:
              begin
              EventManager.Publish(Event_ControllerAxisUp, TsgeEventControllerAxis.Create(Idx, I, catMax));
              EventManager.Publish(Event_ControllerAxisDown, TsgeEventControllerAxis.Create(Idx, I, catMin));
              end;
          end;

          //Изменение значения
          if Joy.CurrentInfo.Axis[I].Value <> Joy.LastInfo.Axis[I].Value then
            EventManager.Publish(Event_ControllerAxisValue, TsgeEventControllerAxisValue.Create(Idx, I,  Joy.CurrentInfo.Axis[I].Value, Joy.LastInfo.Axis[I].Value));
          end;


        //Сохранить текущее состояние
        Joy.SwapInfo;


      except
        //Сказать что отвалился
        EventManager.Publish(Event_ControllerDetach, TsgeEventController.Create(Idx));

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
  if (CurrentTime - FLastScanTime) / OneSecFrequency <= FInnerScanDelay  then Exit;
  FLastScanTime := CurrentTime;


  //Проверить на подключение новых контроллеров
  c := FMaxControllerCount - 1;
  for idx := 0 to c do
    begin
    //Если контроллер существует, то следующий
    if FControllerList.Exist(idx) then Continue;

    //Добавить контроллер
    if sgeControllerExist(idx) then
      try
        //Создать контроллер
        Joy := TsgeController.Create(idx);

        //Добавить в список
        FControllerList.Add(Joy);

        //Событие, новый джойстик
        EventManager.Publish(Event_ControllerAttach, TsgeEventController.Create(FControllerList.Count - 1));
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
  if (Current = -1) and (Previvous = 0)  then Result := etMinDown;
  if (Current = 0)  and (Previvous = -1) then Result := etMinUp;
  if (Current = 1)  and (Previvous = -1) then Result := etMinUpMaxDown;
  if (Current = 1)  and (Previvous = 0)  then Result := etMaxDown;
  if (Current = 0)  and (Previvous = 1)  then Result := etMaxUp;
  if (Current = -1) and (Previvous = 1)  then Result := etMaxUpMinDown;
end;

function TsgeExtensionControllers.GetAxisEventType(Current, Previvous, Middle: Integer): TEventType;
begin
  Result := etNone;
  if (Current < Middle) and (Previvous = Middle) then Result := etMinDown;
  if (Current = Middle) and (Previvous < Middle) then Result := etMinUp;
  if (Current > Middle) and (Previvous < Middle) then Result := etMinUpMaxDown;
  if (Current > Middle) and (Previvous = Middle) then Result := etMaxDown;
  if (Current = Middle) and (Previvous > Middle) then Result := etMaxUp;
  if (Current < Middle) and (Previvous > Middle) then Result := etMaxUpMinDown;
end;


procedure TsgeExtensionControllers.SetEnable(AEnable: Boolean);
begin
  if FEnable = AEnable then Exit;

  FEnable := AEnable;

  if FEnable then FThread.Resume else FThread.Suspend;
end;


procedure TsgeExtensionControllers.SetScanInterval(AInterval: Cardinal);
begin
  FAutoScanInterval := AInterval;

  FInnerScanDelay := FAutoScanInterval div 1000;
end;


class function TsgeExtensionControllers.GetName: String;
begin
  Result := Extension_Controllers;
end;


constructor TsgeExtensionControllers.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Классы
    FThread := TsgeThread.Create(nil, True, False);
    FControllerList := TsgeControllerList.Create;

    //Параметры
    FEnable := False;
    FScanDelay := 50;
    FAutoScan := False;
    FMaxControllerCount := sgeGetMaxControllerCount;
    FAutoScanInterval := 5000;

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
  EventManager.UnSubscribe(Self);

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

