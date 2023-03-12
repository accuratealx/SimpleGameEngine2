{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemPropertyIntPoint.pas
Версия            1.0
Создан            08.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Свойство элемента отображения: Целочисленная точка
}
{$Include Defines.inc}

unit sgeDisplayElementItemPropertyIntPoint;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes;

type
  TsgeDisplayElementItemPropertyIntPoint = class
  private
    FPoint: TsgeIntPoint;

  public
    constructor Create(Point: TsgeIntPoint);
    constructor Create(X, Y: Integer);

    property Point: TsgeIntPoint read FPoint write FPoint;
    property X: Integer read FPoint.X write FPoint.X;
    property Y: Integer read FPoint.Y write FPoint.Y;
  end;


implementation


constructor TsgeDisplayElementItemPropertyIntPoint.Create(Point: TsgeIntPoint);
begin
  FPoint := Point;
end;


constructor TsgeDisplayElementItemPropertyIntPoint.Create(X, Y: Integer);
begin
  FPoint.X := X;
  FPoint.Y := Y;
end;



end.

