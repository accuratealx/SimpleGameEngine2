{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemSpritePart.pas
Версия            1.0
Создан            10.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Часть спрайта
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemSpritePart;

{$mode ObjFPC}{$H+}

interface

uses
  sgeGraphicOpenGLDrawObjectItemSprite;

type
  TsgeGraphicOpenGLDrawObjectItemSpritePart = class(TsgeGraphicOpenGLDrawObjectItemSprite)
  protected
    procedure SetTexBuffer; override;

  end;


implementation

uses
  sgeDisplayElementItemSpritePart;


procedure TsgeGraphicOpenGLDrawObjectItemSpritePart.SetTexBuffer;
var
  SpritePart: TsgeDisplayElementItemSpritePart;
  X1, Y1, X2, Y2: Single;
begin
  //Указатель на element
  SpritePart := FElement as TsgeDisplayElementItemSpritePart;

  //Подготовить координаты
  X1 := SpritePart.SpriteX1 * FGLSprite.GLPixelWidth;
  X2 := SpritePart.SpriteX2 * FGLSprite.GLPixelWidth;

  Y1 := 1 - SpritePart.SpriteY1 * FGLSprite.GLPixelHeight;
  Y2 := 1 - SpritePart.SpriteY2 * FGLSprite.GLPixelHeight;

  //Обновить текстурный буфер
  UpdateTextureBuffer(X1, Y1, X2, Y2);
end;



end.

