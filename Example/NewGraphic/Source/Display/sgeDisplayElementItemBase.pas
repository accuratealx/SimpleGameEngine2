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
    FCentered: Boolean;                                             //Вывод по центру

    procedure SetDefaultParameter;
  public
    constructor Create(X, Y, Width, Height: Single);

    property X: Single read FX;
    property Y: Single read FY;
    property Width: Single read FWidth;
    property Height: Single read FHeight;

    property Transparent: Boolean read FTransparent write FTransparent;
    property Centered: Boolean read FCentered write FCentered;
  end;


implementation


procedure TsgeDisplayElementItemBase.SetDefaultParameter;
begin
  FTransparent := True;
  FCentered := False;
end;


constructor TsgeDisplayElementItemBase.Create(X, Y, Width, Height: Single);
begin
  //Установить параметры по умолчанию
  SetDefaultParameter;

  //Сохранить параметры
  FX := X;
  FY := Y;
  FWidth := Width;
  FHeight := Height;
end;



end.

