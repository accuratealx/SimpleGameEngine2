{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLVertexArrayObject.pas
Версия            1.0
Создан            27.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Класс вертексного объекта
}
{$Include Defines.inc}

unit sgeGraphicOpenGLVertexArrayObject;

{$mode ObjFPC}{$H+}

interface

uses
  dglOpenGL,
  sgeGraphicOpenGLBuffer;

type
  TsgeGraphicOpenGLVertexArrayObject = class
  private
    FHandle: GLUInt;
    FVertexBuffer: TsgeGraphicOpenGLBuffer;
    FTextureBuffer: TsgeGraphicOpenGLBuffer;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure BindVertexCoord(Buffer: TsgeGraphicOpenGLBuffer);
    procedure BindTextureCoord(Buffer: TsgeGraphicOpenGLBuffer);

    procedure DrawArray;

    procedure Attach;
    procedure Detach;

    property Handle: GLuint read FHandle;
  end;


implementation

uses
  sgeErrors;

const
  STRIDE = 2 * sizeof(GLfloat);

  _UNITNAME = 'GraphicOpenGLVertexArrayObject';

  Err_EmptyBuffer = 'EmptyBuffer';


constructor TsgeGraphicOpenGLVertexArrayObject.Create;
begin
  if FHandle = 0 then
    glGenVertexArrays(1, @FHandle);
end;


destructor TsgeGraphicOpenGLVertexArrayObject.Destroy;
begin
  if FHandle <> 0 then
    glDeleteVertexArrays(1, @FHandle);
end;


procedure TsgeGraphicOpenGLVertexArrayObject.BindVertexCoord(Buffer: TsgeGraphicOpenGLBuffer);
begin
  if Buffer = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyBuffer);

  //Привязать буфер вершин
  Buffer.Attach;

  //Разрешить 0 расположение в шейдере
  glEnableVertexAttribArray(0);

  //Указать параметры данных вершин
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, STRIDE, nil);

  //Сохранить указатель на буфер вершин
  FVertexBuffer := Buffer;
end;


procedure TsgeGraphicOpenGLVertexArrayObject.BindTextureCoord(Buffer: TsgeGraphicOpenGLBuffer);
begin
  if Buffer = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyBuffer);

  //Привязать буфер вершин
  Buffer.Attach;

  //Разрешить 0 расположение в шейдере
  glEnableVertexAttribArray(1);

  //Указать параметры данных вершин
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, STRIDE, nil);

  //Сохранить указатель на буфер текстурных координат
  FTextureBuffer := Buffer;
end;


procedure TsgeGraphicOpenGLVertexArrayObject.DrawArray;
begin
  //Вывести массив вершин
  glDrawArrays(GL_TRIANGLES, 0, FVertexBuffer.CoordCount);
end;


procedure TsgeGraphicOpenGLVertexArrayObject.Attach;
begin
  glBindVertexArray(FHandle);
end;


procedure TsgeGraphicOpenGLVertexArrayObject.Detach;
begin
  glBindVertexArray(0);
end;



end.

