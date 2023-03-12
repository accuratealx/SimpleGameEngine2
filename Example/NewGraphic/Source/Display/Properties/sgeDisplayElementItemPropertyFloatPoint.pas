{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemPropertyFloatPoint.pas
Версия            1.0
Создан            08.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Свойство элемента отображения: Плавающая точка
}
{$Include Defines.inc}

unit sgeDisplayElementItemPropertyFloatPoint;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes;

type
  TsgeDisplayElementItemPropertyFloatPoint = class
  private
    FPoint: TsgeFloatPoint;

  public
    constructor Create(Point: TsgeFloatPoint);
    constructor Create(X: Single = 1; Y: Single = 1);

    property Point: TsgeFloatPoint read FPoint write FPoint;
    property X: Single read FPoint.X write FPoint.X;
    property Y: Single read FPoint.Y write FPoint.Y;
  end;


implementation


constructor TsgeDisplayElementItemPropertyFloatPoint.Create(Point: TsgeFloatPoint);
begin
  FPoint := Point;
end;


constructor TsgeDisplayElementItemPropertyFloatPoint.Create(X: Single; Y: Single);
begin
  FPoint.X := X;
  FPoint.Y := Y;
end;



end.

