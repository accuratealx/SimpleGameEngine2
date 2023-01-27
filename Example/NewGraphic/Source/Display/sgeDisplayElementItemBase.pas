{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemBase.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Базовый
}
{$Include Defines.inc}

unit sgeDisplayElementItemBase;

{$mode ObjFPC}{$H+}

interface

type
  TsgeDisplayElementItemBase = class
  private
    FX: Single;                                                     //X
    FY: Single;                                                     //Y
    FWidth: Single;                                                 //Ширина
    FHeight: Single;                                                //Высота
    FTransparent: Boolean;                                          //Прозрачность
  public
    constructor Create(X, Y, Width, Height: Single; Transparent: Boolean = True);

    property X: Single read FX;
    property Y: Single read FY;
    property Width: Single read FWidth;
    property Height: Single read FHeight;
    property Transparent: Boolean read FTransparent write FTransparent;
  end;


implementation



constructor TsgeDisplayElementItemBase.Create(X, Y, Width, Height: Single; Transparent: Boolean);
begin
  //Сохранить параметры
  FX := X;
  FY := Y;
  FWidth := Width;
  FHeight := Height;
  FTransparent := Transparent;
end;



end.

