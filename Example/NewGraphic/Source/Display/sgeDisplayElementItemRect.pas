{
Пакет             Simple Game Engine 2
Файл              .pas
Версия            1.0
Создан            27.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Цветной прямоугольник
}
{$Include Defines.inc}

unit sgeDisplayElementItemRect;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeGraphicColor,
  sgeDisplayElementItemBase;

type
  TsgeDisplayElementItemRect = class(TsgeDisplayElementItemBase)
  private
    FColor: TsgeColor;

  public
    constructor Create(X, Y, Width, Height: Single; Color: TsgeColor);

    property Color: TsgeColor read FColor write FColor;
  end;


implementation


constructor TsgeDisplayElementItemRect.Create(X, Y, Width, Height: Single; Color: TsgeColor);
begin
  inherited Create(X, Y, Width, Height);

  FColor := Color;
end;



end.

