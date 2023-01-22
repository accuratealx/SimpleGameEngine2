{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSprite.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Спрайт
}
{$Include Defines.inc}

unit sgeDisplayElementItemSprite;

{$mode ObjFPC}{$H+}

interface

uses
  sgeSprite,
  sgeDisplayElementItemBase;

type
  TsgeDisplayElementItemSprite = class(TsgeDisplayElementItemBase)
  private
    FSprite: TsgeSprite;                                            //Ссылка на спрайт
    FX: Single;                                                     //X
    FY: Single;                                                     //Y
    FWidth: Single;                                                 //Ширина
    FHeight: Single;                                                //Высота
  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite);

    property Sprite: TsgeSprite read FSprite;
    property X: Single read FX;
    property Y: Single read FY;
    property Width: Single read FWidth;
    property Height: Single read FHeight;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = '';

  Err_EmptySprite = 'EmptySprite';


constructor TsgeDisplayElementItemSprite.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite);
begin
  //Проверить спрайт
  if Sprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  //Сохранить параметры
  FX := X;
  FY := Y;
  FWidth := Width;
  FHeight := Height;
  FSprite := Sprite;
end;



end.

