{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLSprite.pas
Версия            1.0
Создан            09.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Спрайт
}
{$Include Defines.inc}

unit sgeGraphicOpenGLSprite;

{$mode ObjFPC}{$H+}

interface

uses
  dglOpenGL,
  sgeSprite;


type
  TsgeGraphicOpenGLSprite = class
  private
    FGLHandle: GLuint;                                              //Номер OpenGL
    FGLTileWidth: Single;                                           //Ширина одной плитки в координатах OpenGL
    FGLTileHeight: Single;                                          //Высота одной плитки в координатах OpenGL
    FGLPixelWidth: Single;                                          //Ширина одного пикселя в координатах OpenGL
    FGLPixelHeight: Single;                                         //Высота одного пикселя в координатах OpenGL

  public
    constructor Create(Sprite: TsgeSprite);
    destructor Destroy; override;

    procedure LoadFromSprite(Sprite: TsgeSprite);

    property GLHandle: GLuint read FGLHandle;
    property GLTileWidth: Single read FGLTileWidth;
    property GLTileHeight: Single read FGLTileHeight;
    property GLPixelWidth: Single read FGLPixelWidth;
    property GLPixelHeight: Single read FGLPixelHeight;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'GraphicOpenGLSprite';

  Err_EptySprite = 'EptySprite';



constructor TsgeGraphicOpenGLSprite.Create(Sprite: TsgeSprite);
begin
  //Выделить память для текстуры
  glGenTextures(1, @FGLHandle);

  //Загрузить из спрайта
  LoadFromSprite(Sprite);
end;


destructor TsgeGraphicOpenGLSprite.Destroy;
begin
  glDeleteTextures(1, @FGLHandle);
end;


procedure TsgeGraphicOpenGLSprite.LoadFromSprite(Sprite: TsgeSprite);
begin
  if not Assigned(Sprite) then
    raise EsgeException.Create(_UNITNAME, Err_EptySprite);

  //Залить текстуру
  glBindTexture(GL_TEXTURE_2D, FGLHandle);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Sprite.Width, Sprite.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, Sprite.Data);
  glBindTexture(GL_TEXTURE_2D, 0);

  //Размеры плитки в координатах OpenGL
  FGLTileWidth := 1 / Sprite.TileCols;
  FGLTileHeight := 1 / Sprite.TileRows;

  //Размеры одного пикселя в координатах OpenGL
  FGLPixelWidth := 1 / Sprite.Width;
  FGLPixelHeight := 1 / Sprite.Height;
end;



end.

