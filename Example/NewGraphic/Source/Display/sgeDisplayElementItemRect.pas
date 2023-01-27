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
    constructor Create(X, Y, Width, Height: Single; Color: TsgeColor; Transparent: Boolean = True);

    property Color: TsgeColor read FColor;
  end;

implementation


constructor TsgeDisplayElementItemRect.Create(X, Y, Width, Height: Single; Color: TsgeColor; Transparent: Boolean);
begin
  inherited Create(X, Y, Width, Height, Transparent);

  FColor := Color;
end;



end.

