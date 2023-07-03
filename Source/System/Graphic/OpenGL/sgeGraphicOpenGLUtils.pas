{
Пакет             Simple Game Engine 2
Файл              sgeGraphicUtils.pas
Версия            1.1
Создан            25.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Вспомогательные функции
}
{$Include Defines.inc}

unit sgeGraphicOpenGLUtils;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes;


//Вернуть координаты прямоугольника из целочисленных значений в координатах OpenGL
function sgeGetTextureRect(GLPixelWidth, GLPixelHeight: Single; const SpriteRect: TsgeFloatRect): TsgeFloatRect;

//Вернуть координаты прямоугольника из значений плитки в координатах OpenGL
function sgeGetTextureTileRect(GLTileWidth, GLTileHeight: Single; const Col, Row: Word): TsgeFloatRect;

//Вернуть координаты прямоугольника по количеству плиток в координатах OpenGL
function sgeGetTextureTileRect(ColCount, RowCount, Col, Row: Word): TsgeFloatRect;


implementation


function sgeGetTextureRect(GLPixelWidth, GLPixelHeight: Single; const SpriteRect: TsgeFloatRect): TsgeFloatRect;
begin
  Result.X1 := SpriteRect.X1 * GLPixelWidth;
  Result.Y1 := 1 - SpriteRect.Y1 * GLPixelHeight;
  Result.X2 := SpriteRect.X2 * GLPixelWidth;
  Result.Y2 := 1 - SpriteRect.Y2 * GLPixelHeight;
end;


function sgeGetTextureTileRect(GLTileWidth, GLTileHeight: Single; const Col, Row: Word): TsgeFloatRect;
begin
  Result.X1 := Col * GLTileWidth;
  Result.Y1 := 1 - Row * GLTileHeight;
  Result.X2 := Result.X1 + GLTileWidth;
  Result.Y2 := Result.Y1 - GLTileHeight;
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

