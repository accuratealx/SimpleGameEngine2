{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLSprite.pas
Версия            1.1
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
    FHandle: GLuint;                                                //Номер OpenGL
    FGLTileWidth: Single;                                           //Ширина одной плитки в координатах OpenGL
    FGLTileHeight: Single;                                          //Высота одной плитки в координатах OpenGL
    FGLPixelWidth: Single;                                          //Ширина одного пикселя в координатах OpenGL
    FGLPixelHeight: Single;                                         //Высота одного пикселя в координатах OpenGL

    function GetMagFilter(AFilter: TsgeSpriteMagFilter): GLint;
    function GetMinFilter(AFilter: TsgeSpriteMinFilter): GLint;
    function GetWrapMode(AMode: TsgeSpriteWrapMode): GLint;
  public
    constructor Create(Sprite: TsgeSprite);
    destructor Destroy; override;

    procedure Attach;
    procedure Detach;

    procedure LoadFromSprite(Sprite: TsgeSprite);

    property Handle: GLuint read FHandle;
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


function TsgeGraphicOpenGLSprite.GetMagFilter(AFilter: TsgeSpriteMagFilter): GLint;
begin
  case AFilter of
    smagfNearest:
      Result := GL_NEAREST;

    smagfLinear:
      Result := GL_LINEAR;
  end;
end;


function TsgeGraphicOpenGLSprite.GetMinFilter(AFilter: TsgeSpriteMinFilter): GLint;
begin
  case AFilter of
    sminfNearest:
      Result := GL_NEAREST;

    sminfLinear:
      Result := GL_LINEAR;

    sminfNearestMipmapNearest:
      Result := GL_NEAREST_MIPMAP_NEAREST;

    sminfLinearMipmapNearest:
      Result := GL_LINEAR_MIPMAP_NEAREST;

    sminfNearestMipmapLinear:
      Result := GL_NEAREST_MIPMAP_LINEAR;

    sminfLinearMipmapLinear:
      Result := GL_LINEAR_MIPMAP_LINEAR;
  end;
end;


function TsgeGraphicOpenGLSprite.GetWrapMode(AMode: TsgeSpriteWrapMode): GLint;
begin
  case AMode of
    swmRepeat:
      Result := GL_REPEAT;

    swmMirroredRepeat:
      Result :=  GL_MIRRORED_REPEAT;

    swmClampToEdge:
      Result := GL_CLAMP_TO_EDGE;

    swmClampToBorder:
      Result := GL_CLAMP_TO_BORDER;

    swmMirrorClampToEdge:
      Result :=  GL_MIRROR_CLAMP_TO_EDGE;
  end;
end;


constructor TsgeGraphicOpenGLSprite.Create(Sprite: TsgeSprite);
begin
  //Выделить память для текстуры
  if FHandle = 0 then
    glGenTextures(1, @FHandle);

  //Загрузить из спрайта
  LoadFromSprite(Sprite);
end;


destructor TsgeGraphicOpenGLSprite.Destroy;
begin
  glDeleteTextures(1, @FHandle);
end;


procedure TsgeGraphicOpenGLSprite.Attach;
begin
  glBindTexture(GL_TEXTURE_2D, FHandle);
end;


procedure TsgeGraphicOpenGLSprite.Detach;
begin
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TsgeGraphicOpenGLSprite.LoadFromSprite(Sprite: TsgeSprite);
begin
  if not Assigned(Sprite) then
    raise EsgeException.Create(_UNITNAME, Err_EptySprite);

  //Привязать текстуру
  Attach;

  //Залить текстуру
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Sprite.Width, Sprite.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, Sprite.Data);

  //Создание MipMap
  glGenerateMipmap(GL_TEXTURE_2D);

  //Установить режим фильтрации
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GetMagFilter(Sprite.MagFilter));
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GetMinFilter(Sprite.MinFilter));

  //Установить режим оборачивания
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GetWrapMode(Sprite.WrapModeHorizontal));
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GetWrapMode(Sprite.WrapModeVertical));
  glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, @Sprite.WrapModeColor);

  //Отвязать текстуру
  Detach;

  //Размеры плитки в координатах OpenGL
  FGLTileWidth := 1 / Sprite.TileCols;
  FGLTileHeight := 1 / Sprite.TileRows;

  //Размеры одного пикселя в координатах OpenGL
  FGLPixelWidth := 1 / Sprite.Width;
  FGLPixelHeight := 1 / Sprite.Height;
end;



end.

