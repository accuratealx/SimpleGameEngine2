{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLVertexArrayObject.pas
Версия            1.2
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
  sgeGraphicOpenGLTypes, sgeGraphicOpenGLBuffer;

type
  TsgeGraphicOpenGLVertexArrayObject = class
  private
    FHandle: GLUInt;
    FVertexType: TsgeGraphicOpenGLVertexType;
    FVertexBuffer: TsgeGraphicOpenGLBuffer;
    FTextureBuffer: TsgeGraphicOpenGLBuffer;

    function GetVertexCount: Integer;
  public
    constructor Create(VertexType: TsgeGraphicOpenGLVertexType = vtTriangle);
    destructor  Destroy; override;

    procedure BindVertexCoord(Buffer: TsgeGraphicOpenGLBuffer);
    procedure BindTextureCoord(Buffer: TsgeGraphicOpenGLBuffer);

    procedure Attach;
    procedure Detach;

    property Handle: GLuint read FHandle;
    property VertexType: TsgeGraphicOpenGLVertexType read FVertexType;
    property VertexCount: Integer read GetVertexCount;
  end;


implementation

uses
  sgeErrors;

const
  STRIDE = 2 * sizeof(GLfloat);

  _UNITNAME = 'GraphicOpenGLVertexArrayObject';

  Err_EmptyBuffer = 'EmptyBuffer';


function TsgeGraphicOpenGLVertexArrayObject.GetVertexCount: Integer;
begin
  Result := FVertexBuffer.GetCoordCount;
end;


constructor TsgeGraphicOpenGLVertexArrayObject.Create(VertexType: TsgeGraphicOpenGLVertexType);
begin
  //Выделить память в видеокарте
  if FHandle = 0 then
    glGenVertexArrays(1, @FHandle);

  //Запомнить тип вершин
  FVertexType := VertexType;
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


procedure TsgeGraphicOpenGLVertexArrayObject.Attach;
begin
  glBindVertexArray(FHandle);
end;


procedure TsgeGraphicOpenGLVertexArrayObject.Detach;
begin
  glBindVertexArray(0);
end;



end.

