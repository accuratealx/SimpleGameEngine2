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

uses
  sgeTypes;

type
  TsgeDisplayElementItemBase = class
  private
    FX: Single;                                                     //X
    FY: Single;                                                     //Y
    FWidth: Single;                                                 //Ширина
    FHeight: Single;                                                //Высота
    FTransparent: Boolean;                                          //Прозрачность
    FCentered: Boolean;                                             //Вывод по центру
    FScale: Single;                                                 //Масштаб
    FAngle: Single;                                                 //Угол поворота
    FAlpha: Single;                                                 //Прозрачность

    procedure SetDefaultParameter;
  public
    constructor Create(X, Y, Width, Height: Single);

    property X: Single read FX write FX;
    property Y: Single read FY write FX;
    property Width: Single read FWidth write FWidth;
    property Height: Single read FHeight write FHeight;
    property Transparent: Boolean read FTransparent write FTransparent;
    property Centered: Boolean read FCentered write FCentered;
    property Scale: Single read FScale write FScale;
    property Angle: Single read FAngle write FAngle;
    property Alpha: Single read FAlpha write FAlpha;
  end;


implementation


procedure TsgeDisplayElementItemBase.SetDefaultParameter;
begin
  FTransparent := True;
  FCentered := False;
  FScale := 1;
  FAngle := 0;
  FAlpha := 1;
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

