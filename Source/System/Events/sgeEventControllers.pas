{
Пакет             Simple Game Engine 2
Файл              sgeEventControllers.pas
Версия            1.1
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
  Event_ControllerAttach        = 'Controller.Attach';
  Event_ControllerDetach        = 'Controller.Detach';
  Event_ControllerButtonUp      = 'Controller.ButtonUp';
  Event_ControllerButtonDown    = 'Controller.ButtonDown';
  Event_ControllerPov           = 'Controller.Pov';
  Event_ControllerAxis          = 'Controller.Axis';

  Event_ControllerPovUp         = 'Controller.PovUp';
  Event_ControllerPovRight      = 'Controller.PovRight';
  Event_ControllerPovDown       = 'Controller.PovDown';
  Event_ControllerPovLeft       = 'Controller.PovLeft';


type
  //Типы осей
  TsgeControllerAxisType = (catX, catY, catU, catV, catR, catZ);


  //Тип наклона оси
  TsgeControllerAxisTilt = (catMin, catMax);


  //Тип крестовины
  TsgeControllerPovType = (cptVirtual, cptDirection, cptDegree);


  //Напрвление крестовины
  TsgeControllerPovDirection = (cpdUp, cpdRight, cpdDown, cpdLeft);


  TsgeEventController = class(TsgeEventBase)
  private
    FID: Byte;
  public
    constructor Create(ID: Byte);

    property ID: Byte read FID;
  end;



  TsgeEventControllerPOV = class(TsgeEventController)
  private
    FPovType: TsgeControllerPovType;
    FAngle: Integer;
    FX: SmallInt;
    FY: SmallInt;

  public
    constructor Create(ID: Byte; PovType: TsgeControllerPovType; Angle: Integer; X, Y: SmallInt); reintroduce;

    property PovType: TsgeControllerPovType read FPovType;
    property Angle: Integer read FAngle;
    property X: SmallInt read FX;
    property Y: SmallInt read FY;
  end;



  TsgeEventControllerAxis = class(TsgeEventController)
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



  TsgeEventControllerButton = class(TsgeEventController)
  private
    FButtonID: Byte;
  public
    constructor Create(ID: Byte; ButtonID: Byte); reintroduce;

    property ButtonID: Byte read FButtonID;
  end;



implementation


constructor TsgeEventController.Create(ID: Byte);
begin
  FID := ID;
end;



constructor TsgeEventControllerPOV.Create(ID: Byte; PovType: TsgeControllerPovType; Angle: Integer; X, Y: SmallInt);
begin
  inherited Create(ID);

  FPovType := PovType;
  FAngle := Angle;
  FX := X;
  FY := Y;
end;



constructor TsgeEventControllerAxis.Create(ID: Byte; Axis: TsgeControllerAxisType; Value, PrevValue: Integer);
begin
  inherited Create(ID);

  FAxis := Axis;
  FValue := Value;
  FPrevValue := PrevValue;
end;



constructor TsgeEventControllerButton.Create(ID: Byte; ButtonID: Byte);
begin
  inherited Create(ID);

  FButtonID := ButtonID;
end;




end.

