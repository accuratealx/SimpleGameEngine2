{
Пакет             Simple Game Engine 2
Файл              sgeEventControllers.pas
Версия            1.2
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
    constructor Create(ID: Byte);

    property ID: Byte read FID;
  end;


  //Нажатие кнопки
  TsgeEventControllerButton = class(TsgeEventController)
  private
    FButtonID: Byte;
  public
    constructor Create(ID: Byte; ButtonID: Byte); reintroduce;

    property ButtonID: Byte read FButtonID;
  end;


  //Нажатие крестовины
  TsgeEventControllerPOV = class(TsgeEventController)
  private
    FDirection: TsgeControllerPovDirection;
  public
    constructor Create(ID: Byte; Direction: TsgeControllerPovDirection); reintroduce;

    property Direction: TsgeControllerPovDirection read FDirection;
  end;


  //Отклонение оси
  TsgeEventControllerAxis = class(TsgeEventController)
  private
    FAxis: TsgeControllerAxisType;
    FTilt: TsgeControllerAxisTilt;
  public
    constructor Create(ID: Byte; Axis: TsgeControllerAxisType; Tilt: TsgeControllerAxisTilt); reintroduce;

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
    constructor Create(ID: Byte; Axis: TsgeControllerAxisType; Value, PrevValue: Integer); reintroduce;

    property Axis: TsgeControllerAxisType read FAxis;
    property Value: Integer read FValue;
    property PrevValue: Integer read FPrevValue;
  end;




implementation



constructor TsgeEventController.Create(ID: Byte);
begin
  FID := ID;
end;



constructor TsgeEventControllerButton.Create(ID: Byte; ButtonID: Byte);
begin
  inherited Create(ID);
  FButtonID := ButtonID;
end;



constructor TsgeEventControllerPOV.Create(ID: Byte; Direction: TsgeControllerPovDirection);
begin
  inherited Create(ID);
  FDirection := Direction;
end;



constructor TsgeEventControllerAxis.Create(ID: Byte; Axis: TsgeControllerAxisType; Tilt: TsgeControllerAxisTilt);
begin
  inherited Create(ID);
  FAxis := Axis;
  FTilt := Tilt;
end;



constructor TsgeEventControllerAxisValue.Create(ID: Byte; Axis: TsgeControllerAxisType; Value, PrevValue: Integer);
begin
  inherited Create(ID);
  FAxis := Axis;
  FValue := Value;
  FPrevValue := PrevValue;
end;




end.

