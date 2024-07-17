{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLBuffer.pas
Версия            1.0
Создан            22.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Набор данных для координат прямоугольников
}
{$Include Defines.inc}

unit sgeGraphicOpenGLBuffer;

{$mode ObjFPC}{$H+}

interface

uses
  dglOpenGL,
  sgeTypes;

type
  //Шаблон использованных данных
  TsgeGraphicOpenGLBufferUsage = (buStreamDraw, buStreamRead, buStreamCopy, buStaticDraw, buStaticRead,
                                  buStaticCopy, buDynamicDraw, buDynamicRead, buDynamicCopy);


  //Буфер в памяти OpenGL
  TsgeGraphicOpenGLBuffer = class
  private
    const
      COORD_SIZE = SizeOf(Single);    //Размер памяти под одну координату
      POINT_SIZE = 2 * COORD_SIZE;    //Размер памяти для одной точки (X, Y)
      QUAD_SIZE = 6 * POINT_SIZE;     //Размер памяти для одного прямоугольника (2 треугольника)

  private
    FBuffer: Pointer;                 //Указатель на область памяти с данными
    FQuadCount: Integer;              //Количество прямоугольников
    FHandle: GLUint;                  //Хэндл буфера в OpenGL
    FUsage: TsgeGraphicOpenGLBufferUsage; //Тип используемой памяти OpenGL


    procedure SetQuadCount(Count: Integer);
    function  GetQuad(Index: Integer): TsgeFloatRect;
    procedure SetQuad(Index: Integer; Rect: TsgeFloatRect);

    function GetGLUsage: GLenum;  //Получить код OpenGL для использования памяти
  public
    constructor Create(QuadCount: Integer = 0; Usage: TsgeGraphicOpenGLBufferUsage = buStaticDraw);
    destructor  Destroy; override;

    procedure Attach;
    procedure Detach;

    function GetCoordCount: Integer;

    procedure UpdateOpenGLData;                 //Обновить всю память в OpenGL
    procedure UpdateOpenGLQuad(Index: Integer); //Обновить память одного прямоугольника

    property Handle: GLUInt read FHandle;
    property Usage: TsgeGraphicOpenGLBufferUsage read FUsage;

    property QuadCount: Integer read FQuadCount write SetQuadCount;
    property Quad[Index: Integer]: TsgeFloatRect read GetQuad write SetQuad;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'GraphicOpenGLBuffer';

  Err_IndexOutOfBounds = 'IndexOutOfBounds';


procedure TsgeGraphicOpenGLBuffer.SetQuadCount(Count: Integer);
var
  cnt: Integer;
begin
  if FQuadCount = Count then
    Exit;

  //Запомним количество
  FQuadCount := Count;

  //Посчитаем память в байтах
  cnt := FQuadCount * QUAD_SIZE;

  //Поправим память
  FBuffer := ReAllocMem(FBuffer, cnt);
end;


{$Hints Off}
function TsgeGraphicOpenGLBuffer.GetQuad(Index: Integer): TsgeFloatRect;
var
  Offset: Int64;
begin
  if (Index < 0) or (Index > FQuadCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Смещение в буфере до нужного прямоугольника
  Offset := UIntPtr(FBuffer) + Index * QUAD_SIZE;

  //Результат
  Result.X1 := PSingle(Offset + COORD_SIZE * 0)^;  //0
  Result.Y1 := PSingle(Offset + COORD_SIZE * 1)^;  //1
  Result.X2 := PSingle(Offset + COORD_SIZE * 4)^;  //4
  Result.Y2 := PSingle(Offset + COORD_SIZE * 5)^;  //5
end;


procedure TsgeGraphicOpenGLBuffer.SetQuad(Index: Integer; Rect: TsgeFloatRect);
var
  Offset: Int64;
begin
  if (Index < 0) or (Index > FQuadCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Смещение в буфере до нужного прямоугольника
  Offset := UIntPtr(FBuffer) + Index * QUAD_SIZE;

  //Обновить память
  PSingle(Offset + COORD_SIZE *  0)^ := Rect.X1;
  PSingle(Offset + COORD_SIZE *  1)^ := Rect.Y1;
  PSingle(Offset + COORD_SIZE *  2)^ := Rect.X1;
  PSingle(Offset + COORD_SIZE *  3)^ := Rect.Y2;
  PSingle(Offset + COORD_SIZE *  4)^ := Rect.X2;
  PSingle(Offset + COORD_SIZE *  5)^ := Rect.Y2;
  PSingle(Offset + COORD_SIZE *  6)^ := Rect.X1;
  PSingle(Offset + COORD_SIZE *  7)^ := Rect.Y1;
  PSingle(Offset + COORD_SIZE *  8)^ := Rect.X2;
  PSingle(Offset + COORD_SIZE *  9)^ := Rect.Y2;
  PSingle(Offset + COORD_SIZE * 10)^ := Rect.X2;
  PSingle(Offset + COORD_SIZE * 11)^ := Rect.Y1;
end;
{$Hints On}


function TsgeGraphicOpenGLBuffer.GetGLUsage: GLenum;
begin
  case FUsage of
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


constructor TsgeGraphicOpenGLBuffer.Create(QuadCount: Integer; Usage: TsgeGraphicOpenGLBufferUsage);
begin
  //Запросить память у OpenGL
  if FHandle = 0 then
    glGenBuffers(1, @FHandle);

  //Выделим память
  SetQuadCount(QuadCount);
end;


destructor TsgeGraphicOpenGLBuffer.Destroy;
begin
  //Освободить память OpenGL
  if FHandle <> 0 then
    glDeleteBuffers(1, @FHandle);

  //Почистить буфер
  SetQuadCount(0);
end;


procedure TsgeGraphicOpenGLBuffer.Attach;
begin
  glBindBuffer(GL_ARRAY_BUFFER, FHandle);
end;


procedure TsgeGraphicOpenGLBuffer.Detach;
begin
  glBindBuffer(GL_ARRAY_BUFFER, 0);
end;


function TsgeGraphicOpenGLBuffer.GetCoordCount: Integer;
begin
  Result := FQuadCount * 6; //В каждом прямоугольнике 6 вершин
end;


procedure TsgeGraphicOpenGLBuffer.UpdateOpenGLData;
begin
  //Выбрать буфер для работы
  Attach;

  //Залить данные
  glBufferData(GL_ARRAY_BUFFER, QuadCount * QUAD_SIZE, FBuffer, GetGLUsage);

  //Отвязать буфер
  Detach;
end;


procedure TsgeGraphicOpenGLBuffer.UpdateOpenGLQuad(Index: Integer);
var
  Offset, Size: Integer;
begin
  if (Index < 0) or (Index > FQuadCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Выбрать буфер для работы
  Attach;

  //Смещение в байтах от начала данных
  Offset := Index * QUAD_SIZE;

  //Залить часть данных
  glBufferSubData(GL_ARRAY_BUFFER, Offset, QUAD_SIZE, Pointer(FBuffer + Offset));

  //Отвязать буфер
  Detach;
end;



end.

