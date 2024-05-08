{
Пакет             Simple Game Engine 2
Файл              sgeEventControllerAxisUp.pas
Версия            1.0
Создан            08.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллер: Поднятие оси
}
{$Include Defines.inc}

unit sgeEventControllerAxisUp;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeEventBase, sgeEventController;


const
  Event_ControllerAxisUp = 'Controller.AxisUp';


type
  TsgeEventControllerAxisUp = class(TsgeEventController)
  protected
    FAxis: TsgeControllerAxisType;
    FTilt: TsgeControllerAxisTilt;
    function GetName: ShortString; override;
  public
    constructor Create(ID: Byte; Axis: TsgeControllerAxisType; Tilt: TsgeControllerAxisTilt);

    function Copy: TsgeEventBase; override;

    property Axis: TsgeControllerAxisType read FAxis;
    property Tilt: TsgeControllerAxisTilt read FTilt;
  end;


implementation


function TsgeEventControllerAxisUp.GetName: ShortString;
begin
  Result := Event_ControllerAxisUp;
end;


constructor TsgeEventControllerAxisUp.Create(ID: Byte; Axis: TsgeControllerAxisType; Tilt: TsgeControllerAxisTilt);
begin
  inherited Create(ID);
  FAxis := Axis;
  FTilt := Tilt;
end;


function TsgeEventControllerAxisUp.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerAxisUp.Create(FID, FAxis, FTilt);
end;



end.

