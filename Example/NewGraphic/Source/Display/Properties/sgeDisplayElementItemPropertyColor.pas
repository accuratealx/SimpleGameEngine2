                  {
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemPropertyColor.pas
Версия            1.0
Создан            08.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Свойство элемента отображения: Цвет
}
{$Include Defines.inc}

unit sgeDisplayElementItemPropertyColor;

{$mode ObjFPC}{$H+}

interface

uses
  sgeGraphicColor;

type
  TsgeDisplayElementItemPropertyColor = class
  private
    FColor: TsgeColor;

  public
    constructor Create(Color: TsgeColor);
    constructor Create(Red, Green, Blue, Alpha: Single);

    property Color: TsgeColor read FColor write FColor;
    property Red: Single read FColor.Red write FColor.Red;
    property Green: Single read FColor.Green write FColor.Green;
    property Blue: Single read FColor.Blue write FColor.Blue;
    property Alpha: Single read FColor.Alpha write FColor.Alpha;
  end;


implementation


constructor TsgeDisplayElementItemPropertyColor.Create(Color: TsgeColor);
begin
  FColor := Color;
end;


constructor TsgeDisplayElementItemPropertyColor.Create(Red, Green, Blue, Alpha: Single);
begin
  FColor.Red := Red;
  FColor.Green := Green;
  FColor.Blue := Blue;
  FColor.Alpha := Alpha;
end;



end.

