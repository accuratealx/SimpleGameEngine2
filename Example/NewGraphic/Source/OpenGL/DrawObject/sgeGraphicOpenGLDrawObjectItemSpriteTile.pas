{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemSpriteTile.pas
Версия            1.0
Создан            10.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Плитка спрайта
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemSpriteTile;

{$mode ObjFPC}{$H+}

interface

uses
  sgeGraphicOpenGLDrawObjectItemSprite;

type
  TsgeGraphicOpenGLDrawObjectItemSpriteTile = class(TsgeGraphicOpenGLDrawObjectItemSprite)
  protected
    procedure SetTexBuffer; override;

  end;


implementation

uses
  sgeDisplayElementItemSpriteTile;


procedure TsgeGraphicOpenGLDrawObjectItemSpriteTile.SetTexBuffer;
var
  SpriteTile: TsgeDisplayElementItemSpriteTile;
  X1, Y1, X2, Y2: Single;
begin
  //Указатель на element
  SpriteTile := FElement as TsgeDisplayElementItemSpriteTile;

  //Подготовить координаты
  X1 := SpriteTile.TileCol * FGLSprite.GLTileWidth;
  Y1 := 1 - SpriteTile.TileRow * FGLSprite.GLTileHeight;
  X2 := X1 + FGLSprite.GLTileWidth;
  Y2 := Y1 - FGLSprite.GLTileHeight;

  //Обновить текстурный буфер
  UpdateTextureBuffer(X1, Y1, X2, Y2);
end;



end.

