{
Пакет             Simple Game Engine 2
Файл              sgeGraphicUtils.pas
Версия            1.0
Создан            25.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные функции графики
}

unit sgeGraphicUtils;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeGraphicSprite;



//Вернуть координаты прямоугольника из целочисленных значений в координатах OpenGL
function sgeGetTextureRect(Sprite: TsgeGraphicSprite; const SpriteRect: TsgeFloatRect): TsgeFloatRect;

//Вернуть координаты прямоугольника из значений плитки в координатах OpenGL
function sgeGetTextureTileRect(Sprite: TsgeGraphicSprite; const Col, Row: Word): TsgeFloatRect;


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



end.

