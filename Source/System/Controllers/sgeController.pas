{
Пакет             Simple Game Engine 2
Файл              sgeController.pas
Версия            1.10
Создан            20.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс работы с контроллером
}
{$Include Defines.inc}

unit sgeController;

{$mode objfpc}{$H+}

interface

uses
  sgeEventControllers,
  MMSystem;


type
  //Настройки Оси контроллера
  TsgeControllerAxisSettings = record
    DeviceRange: Cardinal;            //Диапазон значений оси устройства
    DeviceMiddleValue: Cardinal;      //Значение середины оси устройства
    Step: Single;                     //Минимальный шаг приращения
    MiddleValue: Integer;             //Среднее значение
    MinValue: Integer;                //Наименьшее значение оси
    MaxValue: Integer;                //Наибольшее значение оси
  end;


  //Крестовина контроллера
  TsgeControllerPovInfo = record
    X: SmallInt;                      //Состояние X [-1, 0, 1]
    Y: SmallInt;                      //Состояние Y [-1, 0, 1]
  end;


  //Кнопка контроллера
  TsgeControllerButtonInfo = record
    Down: Boolean;                    //Состояние нажатия
    DownOnce: Boolean;                //Нажата впервые
    RepeatCount: Cardinal;            //Количество повторов
  end;
  PsgeControllerButtonInfo = ^TsgeControllerButtonInfo;


  //Ось контроллера
  TsgeControllerAxisInfo = record
    RawValue: Cardinal;               //Реальное значение оси
    Value: Integer;                   //Значение с учётом диапазона
  end;


  //Текущее значение кнопок, осей и крестовины контроллера
  TsgeControllerInfo = record
    Pov: TsgeControllerPovInfo;
    Axis: array[TsgeControllerAxisType] of TsgeControllerAxisInfo;
    Buttons: array of TsgeControllerButtonInfo;
  end;


  TsgeController = class
  private
    //Характеристики устройства
    FDriverID: Byte;                                  //Идентификатор драйвера
    FName: String;                                    //Имя драйвера
    FOEM: String;                                     //OEM строка
    FPovType: TsgeControllerPovType;                  //Тип крестовины
    FZAxisExist: Boolean;                             //Ось Z
    FRAxisExist: Boolean;                             //Ось Rudder
    FUAxisExist: Boolean;                             //Ось U
    FVAxisExist: Boolean;                             //Ось V
    FButtonCount: Byte;                               //Количество кнопок
    FAxisCount: Byte;                                 //Количество осей

    //Настройки Осей
    FAxisSettings: array[TsgeControllerAxisType] of TsgeControllerAxisSettings;

    //Общие настройки
    FAxisSmooth: Boolean;                             //Сглаживать значения

    //Последнее состояние устройства
    FLastInfo: TsgeControllerInfo;                    //Предыдущее состояние
    FCurrentInfo: TsgeControllerInfo;                 //Текущее состояние

    //Свойства
    procedure SetMinValue(Index: Integer; AMinValue: Integer);
    function  GetMinValue(Index: Integer): Integer;
    procedure SetMaxValue(Index: Integer; AMaxValue: Integer);
    function  GetMaxValue(Index: Integer): Integer;

    //Вспомогательные методы
    procedure ZeroInfo(var Info: TsgeControllerInfo); //Обнулить значения кнопок
    procedure SetAxisSettings(var AxisSettings: TsgeControllerAxisSettings; MinVal, MaxVal: Integer);
    procedure SetAxisDefaultSettings(var AxisSettings: TsgeControllerAxisSettings; MinVal, MaxVal: Integer);

  protected
    function GetAxisRawMiddleValue(Axis: TsgeControllerAxisType): Cardinal; //Вернуть сырое среднее значение оси контроллера

  public
    constructor Create(ID: Byte);
    destructor  Destroy; override;

    procedure Reset;
    procedure GetInfo;                                //Запросить состояние устройства
    procedure SwapInfo;                               //Записать текущее стостояние в последнее

    //Характеристики устройства
    property DriverID: Byte read FDriverID;
    property Name: String read FName;
    property OEM: String read FOEM;
    property PovType: TsgeControllerPovType read FPovType;
    property ButtonCount: Byte read FButtonCount;
    property AxisCount: Byte read FAxisCount;
    property ZAxisExist: Boolean read FZAxisExist;
    property RAxisExist: Boolean read FRAxisExist;
    property UAxisExist: Boolean read FUAxisExist;
    property VAxisExist: Boolean read FVAxisExist;

    //Настройки осей
    property XMinValue: Integer index 0 read GetMinValue write SetMinValue;
    property XMaxValue: Integer index 0 read GetMaxValue write SetMaxValue;
    property YMinValue: Integer index 1 read GetMinValue write SetMinValue;
    property YMaxValue: Integer index 1 read GetMaxValue write SetMaxValue;
    property UMinValue: Integer index 2 read GetMinValue write SetMinValue;
    property UMaxValue: Integer index 2 read GetMaxValue write SetMaxValue;
    property VMinValue: Integer index 3 read GetMinValue write SetMinValue;
    property VMaxValue: Integer index 3 read GetMaxValue write SetMaxValue;
    property RMinValue: Integer index 4 read GetMinValue write SetMinValue;
    property RMaxValue: Integer index 4 read GetMaxValue write SetMaxValue;
    property ZMinValue: Integer index 5 read GetMinValue write SetMinValue;
    property ZMaxValue: Integer index 5 read GetMaxValue write SetMaxValue;

    //Настройки
    property AxisSmooth: Boolean read FAxisSmooth write FAxisSmooth;

    //Состояние устройства
    property LastInfo: TsgeControllerInfo read FLastInfo;
    property CurrentInfo: TsgeControllerInfo read FCurrentInfo;
  end;




implementation

uses
  sgeErrors, sgeSystemUtils, Math;


const
  _UNITNAME = 'Joystick';

  Err_CantGetDeviceInfo = 'CantGetDeviceInfo';
  Err_CantReadData      = 'CantReadData';



procedure TsgeController.SetMinValue(Index: Integer; AMinValue: Integer);
var
  AxisType: TsgeControllerAxisType;
begin
  AxisType := TsgeControllerAxisType(Index);
  if FAxisSettings[AxisType].MinValue = AMinValue then Exit;
  SetAxisSettings(FAxisSettings[AxisType], AMinValue, FAxisSettings[AxisType].MaxValue);
end;


function TsgeController.GetMinValue(Index: Integer): Integer;
var
  AxisType: TsgeControllerAxisType;
begin
  AxisType := TsgeControllerAxisType(Index);
  Result := FAxisSettings[AxisType].MinValue;
end;


procedure TsgeController.SetMaxValue(Index: Integer; AMaxValue: Integer);
var
  AxisType: TsgeControllerAxisType;
begin
  AxisType := TsgeControllerAxisType(Index);
  if FAxisSettings[AxisType].MaxValue = AMaxValue then Exit;
  SetAxisSettings(FAxisSettings[AxisType], FAxisSettings[AxisType].MinValue, AMaxValue);
end;


function TsgeController.GetMaxValue(Index: Integer): Integer;
var
  AxisType: TsgeControllerAxisType;
begin
  AxisType := TsgeControllerAxisType(Index);
  Result := FAxisSettings[AxisType].MaxValue;
end;


procedure TsgeController.ZeroInfo(var Info: TsgeControllerInfo);
var
  I: TsgeControllerAxisType;
begin
  //Значение осей
  for I := Low(Info.Axis) to High(Info.Axis) do
    begin
    Info.Axis[I].RawValue := FAxisSettings[I].DeviceMiddleValue;
    Info.Axis[I].Value := FAxisSettings[I].MiddleValue;
    end;

  //Крестовина
  Info.Pov.X := 0;
  Info.Pov.Y := 0;

  //Кнопки
  FillChar(Info.Buttons[0], Length(Info.Buttons) * SizeOf(TsgeControllerButtonInfo), 0);
end;


procedure TsgeController.SetAxisSettings(var AxisSettings: TsgeControllerAxisSettings; MinVal, MaxVal: Integer);
var
  D: Integer;
begin
  //Границы диапазона
  AxisSettings.MinValue := MinVal;
  AxisSettings.MaxValue := MaxVal;

  //Шаг
  D := Abs(AxisSettings.MaxValue - AxisSettings.MinValue);
  AxisSettings.Step := D / AxisSettings.DeviceRange;

  //Среднее значение
  AxisSettings.MiddleValue := Round(AxisSettings.MinValue + (D div 2));
end;


procedure TsgeController.SetAxisDefaultSettings(var AxisSettings: TsgeControllerAxisSettings; MinVal, MaxVal: Integer);
var
  d: Cardinal;
begin
  //Диапазон изменения значений устройства
  AxisSettings.DeviceRange := Abs(MaxVal - MinVal);

  //Границы диапазона
  d := AxisSettings.DeviceRange div 2;
  AxisSettings.MinValue := -d;
  AxisSettings.MaxValue := d;

  //Среднее значение оси устройства
  AxisSettings.DeviceMiddleValue := MinVal + d;

  //Шаг
  AxisSettings.Step := 1;

  //Среднее значение
  AxisSettings.MiddleValue := Round(AxisSettings.MinValue + d * AxisSettings.Step);
end;


function TsgeController.GetAxisRawMiddleValue(Axis: TsgeControllerAxisType): Cardinal;
begin
  Result := FAxisSettings[Axis].DeviceMiddleValue;
end;


constructor TsgeController.Create(ID: Byte);
var
  Caps: TJOYCAPS;
begin
  //Запросить параметры
  if joyGetDevCaps(ID, @Caps, SizeOf(TJOYCAPS)) <> JOYERR_NOERROR then
    raise EsgeException.Create(_UNITNAME, Err_CantGetDeviceInfo, sgeIntToStr(ID));

  //Определить тип крестовины
  FPovType := cptVirtual;
  if (Caps.wCaps and JOYCAPS_HASPOV) = JOYCAPS_HASPOV then
    begin
    if (Caps.wCaps and JOYCAPS_POV4DIR) = JOYCAPS_POV4DIR then FPovType := cptDirection;
    if (Caps.wCaps and JOYCAPS_POVCTS) = JOYCAPS_POVCTS then FPovType := cptDegree;
    end;

  //Наличие осей
  if (Caps.wCaps and JOYCAPS_HASZ) = JOYCAPS_HASZ then FZAxisExist := True;
  if (Caps.wCaps and JOYCAPS_HASR) = JOYCAPS_HASR then FRAxisExist := True;
  if (Caps.wCaps and JOYCAPS_HASU) = JOYCAPS_HASU then FUAxisExist := True;
  if (Caps.wCaps and JOYCAPS_HASV) = JOYCAPS_HASV then FVAxisExist := True;

  //Имя устройства
  FName := Caps.szPname;

  //OEM
  FOEM := Caps.szOEMVxD;

  //Количество кнопок
  FButtonCount := Caps.wNumButtons;

  //Количество осей
  FAxisCount := Caps.wNumAxes;

  //Создать массив кнопок
  SetLength(FLastInfo.Buttons, FButtonCount);
  SetLength(FCurrentInfo.Buttons, FButtonCount);

  //Задать начальные настройки осей
  SetAxisDefaultSettings(FAxisSettings[catX], Caps.wXmin, Caps.wXmax);
  SetAxisDefaultSettings(FAxisSettings[catY], Caps.wYmin, Caps.wYmax);
  SetAxisDefaultSettings(FAxisSettings[catU], Caps.wUmin, Caps.wUmax);
  SetAxisDefaultSettings(FAxisSettings[catV], Caps.wVmin, Caps.wVmax);
  SetAxisDefaultSettings(FAxisSettings[catR], Caps.wRmin, Caps.wRmax);
  SetAxisDefaultSettings(FAxisSettings[catZ], Caps.wZmin, Caps.wZmax);

  //Сбросить значение показателей
  Reset;

  //Параметры
  FDriverID := ID;
  FAxisSmooth :=  True;
end;


destructor TsgeController.Destroy;
begin
  //Удалить массив кнопок
  SetLength(FCurrentInfo.Buttons, 0);
  SetLength(FLastInfo.Buttons, 0);
end;


procedure TsgeController.Reset;
begin
  ZeroInfo(FLastInfo);
  ZeroInfo(FCurrentInfo);
end;


procedure TsgeController.GetInfo;
const
  PovMultiplier = 100;
  dA = Pi / 180;

var
  i, c, X, Y, A: Integer;
  Mask: Cardinal;
  InfoEx: TJOYINFOEX;
  B: PsgeControllerButtonInfo;
  Angle: Single;

  //Установить значение оси
  procedure SetAxisValue(Axis: TsgeControllerAxisType; Value: DWord);
  begin
    FCurrentInfo.Axis[Axis].RawValue := Value;
    FCurrentInfo.Axis[Axis].Value := Round(FAxisSettings[Axis].MinValue + Value * FAxisSettings[Axis].Step);
  end;

  //Установить значение оси на ноль
  procedure SetDefaultAxisValue(Axis: TsgeControllerAxisType);
  begin
    FCurrentInfo.Axis[Axis].RawValue := FAxisSettings[Axis].DeviceMiddleValue;
    FCurrentInfo.Axis[Axis].Value := FAxisSettings[Axis].MiddleValue;
  end;

begin
  //************************** Запрос данных устройства **************************
  //Размер структуры
  InfoEx.dwSize := SizeOf(TJOYINFOEX);

  //Задать основные параметры запроса
  InfoEx.dwFlags := JOY_RETURNX or JOY_RETURNY or JOY_RETURNBUTTONS;

  //Оси
  if FZAxisExist then InfoEx.dwFlags := InfoEx.dwFlags or JOY_RETURNZ;
  if FRAxisExist then InfoEx.dwFlags := InfoEx.dwFlags or JOY_RETURNR;
  if FUAxisExist then InfoEx.dwFlags := InfoEx.dwFlags or JOY_RETURNU;
  if FVAxisExist then InfoEx.dwFlags := InfoEx.dwFlags or JOY_RETURNV;

  //POV
  case FPovType of
    cptDirection: InfoEx.dwFlags := InfoEx.dwFlags or JOY_RETURNPOV;
    cptDegree   : InfoEx.dwFlags := InfoEx.dwFlags or JOY_RETURNPOVCTS;
  end;

  //Сглаживание осей
  if FAxisSmooth then InfoEx.dwFlags := InfoEx.dwFlags or JOY_USEDEADZONE;

  //Запросить значения
  if joyGetPosEx(FDriverID, @InfoEx) <> JOYERR_NOERROR then
    raise EsgeException.Create(_UNITNAME, Err_CantReadData, sgeIntToStr(FDriverID));
  //************************** Запрос данных устройства **************************


  //************************** POV **************************
  if FPovType = cptVirtual then
    begin
    //Определить состояние осей
    X := 0;
    Y := 0;
    if InfoEx.wYpos = FAxisSettings[catY].MaxValue then Y := 1;
    if InfoEx.wXpos = FAxisSettings[catX].MaxValue then X := 2;
    if InfoEx.wYpos = FAxisSettings[catY].MinValue then Y := 4;
    if InfoEx.wXpos = FAxisSettings[catX].MinValue then X := 8;

    //Вернуть значение POV
    case X + Y of
      2 : A := 0;
      3 : A := 45;
      1 : A := 90;
      9 : A := 135;
      8 : A := 180;
      12: A := 225;
      4 : A := 270;
      6 : A := 315;
      else A := -1;
    end;
    end else
      if InfoEx.dwPOV = $FFFF then A := -1 else A := InfoEx.dwPOV div PovMultiplier;

  //Направление осей Pov
  FCurrentInfo.Pov.X := 0;
  FCurrentInfo.Pov.Y := 0;
  if A <> -1 then
    begin
    //Подготовить нормальный угол
    Angle := (360 - A) mod 360;     //Преобразовать угол против часовой стрелки
    Angle := (Angle + 90) mod 360;  //Поворот на 90 градусов по часовой стрелке

    //Принадлежность к оси
    Angle := Angle * dA;
    FCurrentInfo.Pov.X := Sign(RoundTo(Cos(Angle), -2));
    FCurrentInfo.Pov.Y := Sign(RoundTo(Sin(Angle), -2));
    end;
  //************************** POV **************************


  //************************** Значение осей **************************
  SetAxisValue(catX, InfoEx.wXpos);
  SetAxisValue(catY, InfoEx.wYpos);
  if FUAxisExist then SetAxisValue(catU, InfoEx.dwUpos) else SetDefaultAxisValue(catU);
  if FVAxisExist then SetAxisValue(catV, InfoEx.dwVpos) else SetDefaultAxisValue(catV);
  if FRAxisExist then SetAxisValue(catR, InfoEx.dwRpos) else SetDefaultAxisValue(catR);
  if FZAxisExist then SetAxisValue(catZ, InfoEx.wZpos)  else SetDefaultAxisValue(catZ);
  //************************** Значение осей **************************


  //************************** Значение кнопок **************************
  Mask := 1;
  c := FButtonCount - 1;
  for i := 0 to c do
    begin
    //Ссылка на данные кнопки
    B := @FCurrentInfo.Buttons[i];

    //Повторы нажатий
    if B^.Down then Inc(B^.RepeatCount) else B^.RepeatCount := 0;

    //Нажатие кнопки
    B^.Down := (InfoEx.wButtons and Mask) = Mask;

    //Однократное нажатие
    B^.DownOnce := B^.Down and (B^.RepeatCount = 0);

    //Сдвинуть бит влево
    Mask := Mask shl 1;
    end;
  //************************** Значение кнопок **************************
end;


procedure TsgeController.SwapInfo;
begin
  Move(FCurrentInfo, FLastInfo, SizeOf(TsgeControllerInfo) - SizeOf(Pointer));
  Move(FCurrentInfo.Buttons[0], FLastInfo.Buttons[0], Length(FLastInfo.Buttons) * SizeOf(TsgeControllerButtonInfo));
end;




end.




