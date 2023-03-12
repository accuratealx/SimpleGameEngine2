{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemPropertyScale.pas
Версия            1.0
Создан            09.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Свойство элемента отображения: Масштаб
}
{$Include Defines.inc}

unit sgeDisplayElementItemPropertyScale;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes;

type
  TsgeDisplayElementItemPropertyScale = class
  private
    FScale: TsgeFloatPoint;

  public
    constructor Create(X: Single = 1; Y: Single = 1);

    property Scale: TsgeFloatPoint read FScale write FScale;
    property X: Single read FScale.X write FScale.X;
    property Y: Single read FScale.Y write FScale.Y;
  end;


implementation


constructor TsgeDisplayElementItemPropertyScale.Create(X: Single; Y: Single);
begin
  FScale.X := X;
  FScale.Y := Y;
end;



end.

