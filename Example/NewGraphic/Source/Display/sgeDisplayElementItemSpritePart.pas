{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSpritePart.pas
Версия            1.0
Создан            09.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Часть спрайта
}
{$Include Defines.inc}

unit sgeDisplayElementItemSpritePart;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeSprite,
  sgeDisplayElementItemSprite;

type
  TsgeDisplayElementItemSpritePart = class(TsgeDisplayElementItemSprite)
  private
    FSpriteX1: Single;
    FSpriteY1: Single;
    FSpriteX2: Single;
    FSpriteY2: Single;

  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; SpriteX1, SpriteY1, SpriteX2, SpriteY2: Single);

    property SpriteX1: Single read FSpriteX1 write FSpriteX1;
    property SpriteY1: Single read FSpriteY1 write FSpriteY1;
    property SpriteX2: Single read FSpriteX2 write FSpriteX2;
    property SpriteY2: Single read FSpriteY2 write FSpriteY2;
  end;



implementation


constructor TsgeDisplayElementItemSpritePart.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; SpriteX1, SpriteY1, SpriteX2, SpriteY2: Single);
begin
  inherited Create(X, Y, Width, Height, Sprite);

  //Запомнить параметры
  FSpriteX1 := SpriteX1;
  FSpriteY1 := SpriteY1;
  FSpriteX2 := SpriteX2;
  FSpriteY2 := SpriteY2;
end;



end.

