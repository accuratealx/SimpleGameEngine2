{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSpriteTile.pas
Версия            1.0
Создан            10.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Плитка спрайта
}
{$Include Defines.inc}

unit sgeDisplayElementItemSpriteTile;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeSprite,
  sgeDisplayElementItemSprite;

type
  TsgeDisplayElementItemSpriteTile = class(TsgeDisplayElementItemSprite)
  private
    FTileCol: Word;
    FTileRow: Word;

  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; TileCol: Word = 0; TileRow: Word = 0);

    property TileCol: Word read FTileCol write FTileCol;
    property TileRow: Word read FTileRow write FTileRow;
  end;


implementation


constructor TsgeDisplayElementItemSpriteTile.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; TileCol: Word; TileRow: Word);
begin
  inherited Create(X, Y, Width, Height, Sprite);

  //Запомнить параметры
  FTileCol := TileCol;
  FTileRow := TileRow;
end;



end.

