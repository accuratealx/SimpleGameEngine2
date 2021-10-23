{
Пакет             Simple Game Engine 2
Файл              sgeGraphicUtils.pas
Версия            1.1
Создан            25.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные функции графики
}
{$Include Defines.inc}

unit sgeGraphicUtils;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeGraphicSprite;



//Вернуть координаты прямоугольника из целочисленных значений в координатах OpenGL
function sgeGetTextureRect(Sprite: TsgeGraphicSprite; const SpriteRect: TsgeFloatRect): TsgeFloatRect;

//Вернуть координаты прямоугольника из значений плитки в координатах OpenGL
function sgeGetTextureTileRect(Sprite: TsgeGraphicSprite; const Col, Row: Word): TsgeFloatRect;

//Вернуть координаты прямоугольника по количеству плиток в координатах OpenGL
function sgeGetTextureTileRect(ColCount, RowCount, Col, Row: Word): TsgeFloatRect;


implementation



function sgeGetTextureRect(Sprite: TsgeGraphicSprite; const SpriteRect: TsgeFloatRect): TsgeFloatRect;
begin
  Result.X1 := SpriteRect.X1 * Sprite.GLPixelWidth;
  Result.Y1 := 1 - SpriteRect.Y1 * Sprite.GLPixelHeight;
  Result.X2 := SpriteRect.X2 * Sprite.GLPixelWidth;
  Result.Y2 := 1 - SpriteRect.Y2 * Sprite.GLPixelHeight;
end;


function sgeGetTextureTileRect(Sprite: TsgeGraphicSprite; const Col, Row: Word): TsgeFloatRect;
begin
  Result.X1 := Col * Sprite.GLTileWidth;
  Result.Y1 := 1 - Row * Sprite.GLTileHeight;
  Result.X2 := Result.X1 + Sprite.GLTileWidth;
  Result.Y2 := Result.Y1 - Sprite.GLTileHeight;
end;


function sgeGetTextureTileRect(ColCount, RowCount, Col, Row: Word): TsgeFloatRect;
var
  dW, dH: Single;
begin
  //Размеры элемента
  dW := 1 / ColCount;
  dH := 1 / RowCount;

  //Вернуть границы
  Result.X1 := Col * dW;
  Result.Y1 := 1 - Row * dH;
  Result.X2 := Result.X1 + dW;
  Result.Y2 := Result.Y1 - dH;
end;



end.

