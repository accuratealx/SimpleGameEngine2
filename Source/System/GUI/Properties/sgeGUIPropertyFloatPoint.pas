{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyFloatPoint.pas
Версия            1.0
Создан            30.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Дробная точка
}
{$Include Defines.inc}

unit sgeGUIPropertyFloatPoint;

{$mode objfpc}{$H+}

interface

uses
  sgeGUIProperty;

type
  TsgeGUIPropertyFloatPoint = class(TsgeGUIProperty)
  private
    FX: Single;
    FY: Single;

    procedure SetX(AX: Single);
    procedure SetY(AY: Single);
  public
    property X: Single read FX write SetX;
    property Y: Single read FY write SetY;
  end;


implementation


procedure TsgeGUIPropertyFloatPoint.SetX(AX: Single);
begin
  FX := AX;
  UpdateParent;
end;


procedure TsgeGUIPropertyFloatPoint.SetY(AY: Single);
begin
  FY := AY;
  UpdateParent;
end;



end.

