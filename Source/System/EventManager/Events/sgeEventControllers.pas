{
Пакет             Simple Game Engine 2
Файл              sgeEventControllers.pas
Версия            1.4
Создан            22.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллеры
}
{$Include Defines.inc}

unit sgeEventControllers;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeEventBase;


const
  Event_ControllerAttach      = 'Controller.Attach';
  Event_ControllerDetach      = 'Controller.Detach';
  Event_ControllerButtonUp    = 'Controller.ButtonUp';
  Event_ControllerButtonDown  = 'Controller.ButtonDown';
  Event_ControllerPovUp       = 'Controller.PovUp';
  Event_ControllerPovDown     = 'Controller.PovDown';
  Event_ControllerAxisDown    = 'Controller.AxisDown';
  Event_ControllerAxisUp      = 'Controller.AxisUp';
  Event_ControllerAxisValue   = 'Controller.AxisValue';


type
  //Типы осей
  TsgeControllerAxisType = (catX, catY, catU, catV, catR, catZ);


  //Тип наклона оси
  TsgeControllerAxisTilt = (catMin, catMax);


  //Тип крестовины
  TsgeControllerPovType = (cptVirtual, cptDirection, cptDegree);


  //Направление крестовины
  TsgeControllerPovDirection = (cpdUp, cpdRight, cpdDown, cpdLeft);


  //Базовый класс
  TsgeEventController = class(TsgeEventBase)
  private
    FID: Byte;
  public
    constructor Create(Name: ShortString; ID: Byte);

    property ID: Byte read FID;
  end;


  //Нажатие кнопки
  TsgeEventControllerButton = class(TsgeEventController)
  private
    FButtonID: Byte;
  public
    constructor Create(Name: ShortString; ID: Byte; ButtonID: Byte); reintroduce;

    function Copy: TsgeEventBase; override;

    property ButtonID: Byte read FButtonID;
  end;


  //Нажатие крестовины
  TsgeEventControllerPOV = class(TsgeEventController)
  private
    FDirection: TsgeControllerPovDirection;
  public
    constructor Create(Name: ShortString; ID: Byte; Direction: TsgeControllerPovDirection); reintroduce;

    function Copy: TsgeEventBase; override;

    property Direction: TsgeControllerPovDirection read FDirection;
  end;


  //Отклонение оси
  TsgeEventControllerAxis = class(TsgeEventController)
  private
    FAxis: TsgeControllerAxisType;
    FTilt: TsgeControllerAxisTilt;
  public
    constructor Create(Name: ShortString; ID: Byte; Axis: TsgeControllerAxisType; Tilt: TsgeControllerAxisTilt); reintroduce;

    function Copy: TsgeEventBase; override;

    property Axis: TsgeControllerAxisType read FAxis;
    property Tilt: TsgeControllerAxisTilt read FTilt;
  end;


  //Изменение значения оси
  TsgeEventControllerAxisValue = class(TsgeEventController)
  private
    FAxis : TsgeControllerAxisType;
    FValue: Integer;
    FPrevValue: Integer;

  public
    constructor Create(Name: ShortString; ID: Byte; Axis: TsgeControllerAxisType; Value, PrevValue: Integer); reintroduce;

    function Copy: TsgeEventBase; override;

    property Axis: TsgeControllerAxisType read FAxis;
    property Value: Integer read FValue;
    property PrevValue: Integer read FPrevValue;
  end;


implementation


constructor TsgeEventController.Create(Name: ShortString; ID: Byte);
begin
  inherited Create(Name);

  FID := ID;
end;


constructor TsgeEventControllerButton.Create(Name: ShortString; ID: Byte; ButtonID: Byte);
begin
  inherited Create(Name, ID);

  FButtonID := ButtonID;
end;


function TsgeEventControllerButton.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerButton.Create(FName, FID, FButtonID);
end;


constructor TsgeEventControllerPOV.Create(Name: ShortString; ID: Byte; Direction: TsgeControllerPovDirection);
begin
  inherited Create(Name, ID);

  FDirection := Direction;
end;


function TsgeEventControllerPOV.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerPOV.Create(FName, FID, FDirection);
end;


constructor TsgeEventControllerAxis.Create(Name: ShortString; ID: Byte; Axis: TsgeControllerAxisType; Tilt: TsgeControllerAxisTilt);
begin
  inherited Create(Name, ID);

  FAxis := Axis;
  FTilt := Tilt;
end;


function TsgeEventControllerAxis.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerAxis.Create(FName, FID, FAxis, FTilt);
end;


constructor TsgeEventControllerAxisValue.Create(Name: ShortString; ID: Byte; Axis: TsgeControllerAxisType; Value, PrevValue: Integer);
begin
  inherited Create(Name, ID);

  FAxis := Axis;
  FValue := Value;
  FPrevValue := PrevValue;
end;


function TsgeEventControllerAxisValue.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerAxisValue.Create(FName, FID, FAxis, FValue, FPrevValue);
end;



end.

