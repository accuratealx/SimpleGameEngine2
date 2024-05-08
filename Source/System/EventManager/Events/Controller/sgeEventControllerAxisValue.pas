{
Пакет             Simple Game Engine 2
Файл              sgeEventControllers.pas
Версия            1.4
Создан            22.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллеры: Изменение оси
}
{$Include Defines.inc}

unit sgeEventControllerAxisValue;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeEventBase, sgeEventController;


const
  Event_ControllerAxisValue = 'Controller.AxisValue';


type
  //Изменение значения оси
  TsgeEventControllerAxisValue = class(TsgeEventController)
  private
    FAxis : TsgeControllerAxisType;
    FValue: Integer;
    FPrevValue: Integer;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(ID: Byte; Axis: TsgeControllerAxisType; Value, PrevValue: Integer);

    function Copy: TsgeEventBase; override;

    property Axis: TsgeControllerAxisType read FAxis;
    property Value: Integer read FValue;
    property PrevValue: Integer read FPrevValue;
  end;


implementation


function TsgeEventControllerAxisValue.GetName: ShortString;
begin
  Result := Event_ControllerAxisValue;
end;


constructor TsgeEventControllerAxisValue.Create(ID: Byte; Axis: TsgeControllerAxisType; Value, PrevValue: Integer);
begin
  inherited Create(ID);

  FAxis := Axis;
  FValue := Value;
  FPrevValue := PrevValue;
end;


function TsgeEventControllerAxisValue.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerAxisValue.Create(FID, FAxis, FValue, FPrevValue);
end;



end.

