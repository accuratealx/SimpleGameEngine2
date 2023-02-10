{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSimple.pas
Версия            1.0
Создан            10.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Простой элемент
}
{$Include Defines.inc}

unit sgeDisplayElementItemSimple;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeGraphicColor,
  sgeDisplayElementItemBase;

type
  TsgeDisplayElementSimple = class(TsgeDisplayElementItemBase)
  protected
    FColor: TsgeColor;

  public
    constructor Create(X, Y, Width, Height: Single; Color: TsgeColor);

    property Color: TsgeColor read FColor write FColor;
  end;


implementation


constructor TsgeDisplayElementSimple.Create(X, Y, Width, Height: Single; Color: TsgeColor);
begin
  inherited Create(X, Y, Width, Height);

  FColor := Color;
end;



end.

