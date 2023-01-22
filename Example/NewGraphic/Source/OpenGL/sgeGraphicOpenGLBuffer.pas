{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLBuffer.pas
Версия            1.0
Создан            22.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Набор данных
}
{$Include Defines.inc}

unit sgeGraphicOpenGLBuffer;

{$mode ObjFPC}{$H+}

interface

uses
  dglOpenGL,
  sgeGraphicBuffer;

type
  //Шаблон использованных данных
  TsgeGraphicOpenGLBufferUsage = (buStreamDraw, buStreamRead, buStreamCopy, buStaticDraw, buStaticRead,
                                  buStaticCopy, buDynamicDraw, buDynamicRead, buDynamicCopy);


  //Буфер в памяти OpenGL
  TsgeGraphicOpenGLBuffer = class
  private
    FHandle: GLUint;
    FUsage: TsgeGraphicOpenGLBufferUsage;

    function GetGLUsageByBufferUsage(Usage: TsgeGraphicOpenGLBufferUsage): GLenum;
  public
    constructor Create(Buffer: TsgeGraphicBuffer; Usage: TsgeGraphicOpenGLBufferUsage = buStaticDraw);
    destructor  Destroy; override;

    procedure Attach;
    procedure Detach;

    property Handle: GLUInt read FHandle;
    property Usage: TsgeGraphicOpenGLBufferUsage read FUsage;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'GraphicOpenGLBuffer';

  Err_EmptyBuffer = 'EmptyBuffer';


function TsgeGraphicOpenGLBuffer.GetGLUsageByBufferUsage(Usage: TsgeGraphicOpenGLBufferUsage): GLenum;
begin
  case Usage of
    buStreamDraw  : Result := GL_STREAM_DRAW;
    buStreamRead  : Result := GL_STREAM_READ;
    buStreamCopy  : Result := GL_STREAM_COPY;
    buStaticDraw  : Result := GL_STATIC_DRAW;
    buStaticRead  : Result := GL_STATIC_READ;
    buStaticCopy  : Result := GL_STATIC_COPY;
    buDynamicDraw : Result := GL_DYNAMIC_DRAW;
    buDynamicRead : Result := GL_DYNAMIC_READ;
    buDynamicCopy : Result := GL_DYNAMIC_COPY;
  end;
end;


constructor TsgeGraphicOpenGLBuffer.Create(Buffer: TsgeGraphicBuffer; Usage: TsgeGraphicOpenGLBufferUsage);
begin
  if Buffer = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyBuffer);

  //Записать параметры
  FUsage := Usage;

  //Запросить память
  if FHandle = 0 then
    glGenBuffers(1, @FHandle);

  //Выбрать буфер для работы
  Attach;

  //Залить данные
  glBufferData(GL_ARRAY_BUFFER, Buffer.Size, Buffer.Data, GetGLUsageByBufferUsage(FUsage));

  //Отвязать буфер
  Detach;
end;


destructor TsgeGraphicOpenGLBuffer.Destroy;
begin
  if FHandle <> 0 then
    glDeleteBuffers(1, @FHandle);
end;


procedure TsgeGraphicOpenGLBuffer.Attach;
begin
  glBindBuffer(GL_ARRAY_BUFFER, FHandle);
end;


procedure TsgeGraphicOpenGLBuffer.Detach;
begin
  glBindBuffer(GL_ARRAY_BUFFER, 0);
end;



end.

