{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLCoordBuffer.pas
Версия            1.1
Создан            09.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Буфер координат
}
{$Include Defines.inc}

unit sgeGraphicOpenGLCoordBuffer;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes;

type
  TsgeGraphicOpenGLCoordBuffer = class
  private
    FData: Pointer;
    FSize: Int64;
  public
    destructor  Destroy; override;

    procedure Clear;

    procedure AddPoint(X, Y: Single);
    procedure AddPoint(Point: TsgeFloatPoint);

    procedure AddQuad(X1, Y1, X2, Y2: Single);
    procedure AddQuad(Rect: TsgeFloatRect);

    //Добавить вершины замкнутой линии
    procedure AddLineRect(X1, Y1, X2, Y2: Single);

    //Добавить 9 прямоугольников в массив вершин. X1, Y1, X2, Y2 - Координаты в пикселях
    procedure Add9Quad(Width, Height: Single; X1, Y1, X2, Y2: Single);
    procedure Add9Quad(Width, Height: Single; Offset: TsgeFloatRect);

    //Добавить 9 прямоугольников в массив текстурных координат. X1, Y1, X2, Y2 - Координаты в пикселях
    procedure Add9QuadSprite(GLPixelWidth, GLPixelHeight: Single; X1, Y1, X2, Y2: Single);
    procedure Add9QuadSprite(GLPixelWidth, GLPixelHeight: Single; Offset: TsgeFloatRect);

    property Data: Pointer read FData;
    property Size: Int64 read FSize;
  end;


implementation

const
  //Размер одного значения
  DATA_SIZE = SizeOf(Single);


destructor TsgeGraphicOpenGLCoordBuffer.Destroy;
begin
  Clear;
end;


procedure TsgeGraphicOpenGLCoordBuffer.Clear;
begin
  FSize := 0;

  //Освободить память
  FData := ReAllocMem(FData, 0)
end;


procedure TsgeGraphicOpenGLCoordBuffer.AddPoint(X, Y: Single);
begin
  //Новый размер
  FSize := FSize + 2 * DATA_SIZE;

  //Выделить больше памяти
  FData := ReAllocMem(FData, FSize);

  //Записать данные
  PSingle(FData + FSize - DATA_SIZE * 1)^ := Y;
  PSingle(FData + FSize - DATA_SIZE * 2)^ := X;
end;


procedure TsgeGraphicOpenGLCoordBuffer.AddPoint(Point: TsgeFloatPoint);
begin
  AddPoint(Point.X, Point.Y);
end;


procedure TsgeGraphicOpenGLCoordBuffer.AddQuad(X1, Y1, X2, Y2: Single);
begin
  //Новый размер
  FSize := FSize + 12 * DATA_SIZE;

  //Выделить больше памяти
  FData := ReAllocMem(FData, FSize);

  //Записать данные
  PSingle(FData + FSize - DATA_SIZE * 12)^ := X1;
  PSingle(FData + FSize - DATA_SIZE * 11)^ := Y1;
  PSingle(FData + FSize - DATA_SIZE * 10)^ := X1;
  PSingle(FData + FSize - DATA_SIZE * 9)^  := Y2;
  PSingle(FData + FSize - DATA_SIZE * 8)^  := X2;
  PSingle(FData + FSize - DATA_SIZE * 7)^  := Y2;
  PSingle(FData + FSize - DATA_SIZE * 6)^  := X1;
  PSingle(FData + FSize - DATA_SIZE * 5)^  := Y1;
  PSingle(FData + FSize - DATA_SIZE * 4)^  := X2;
  PSingle(FData + FSize - DATA_SIZE * 3)^  := Y2;
  PSingle(FData + FSize - DATA_SIZE * 2)^  := X2;
  PSingle(FData + FSize - DATA_SIZE * 1)^  := Y1;
end;


procedure TsgeGraphicOpenGLCoordBuffer.AddQuad(Rect: TsgeFloatRect);
begin
  AddQuad(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2);
end;


procedure TsgeGraphicOpenGLCoordBuffer.AddLineRect(X1, Y1, X2, Y2: Single);
begin
  //Новый размер
  FSize := FSize + 8 * DATA_SIZE;

  //Выделить больше памяти
  FData := ReAllocMem(FData, FSize);

  //Записать данные
  PSingle(FData + FSize - DATA_SIZE * 8)^  := X1;
  PSingle(FData + FSize - DATA_SIZE * 7)^  := Y1;
  PSingle(FData + FSize - DATA_SIZE * 6)^  := X1;
  PSingle(FData + FSize - DATA_SIZE * 5)^  := Y2;
  PSingle(FData + FSize - DATA_SIZE * 4)^  := X2;
  PSingle(FData + FSize - DATA_SIZE * 3)^  := Y2;
  PSingle(FData + FSize - DATA_SIZE * 2)^  := X2;
  PSingle(FData + FSize - DATA_SIZE * 1)^  := Y1;
end;


procedure TsgeGraphicOpenGLCoordBuffer.Add9Quad(Width, Height: Single; X1, Y1, X2, Y2: Single);
begin
  AddQuad(0, 0, X1, Y1);
  AddQuad(X1, 0, Width - X2,Y1);
  AddQuad(Width - X2, 0, Width, Y1);
  AddQuad(0, Y1, X1, Height - Y2);
  AddQuad(X1, Y1, Width - X2, Height - Y2);
  AddQuad(Width - X2, Y1, Width, Height - Y2);
  AddQuad(0, Height - Y2, X1, Height);
  AddQuad(X1, Height - Y2, Width - X2, Height);
  AddQuad(Width - X2, Height - Y2, Width, Height);
end;


procedure TsgeGraphicOpenGLCoordBuffer.Add9Quad(Width, Height: Single; Offset: TsgeFloatRect);
begin
  Add9Quad(Width, Height, Offset.X1, Offset.Y1, Offset.X2, Offset.Y2);
end;


procedure TsgeGraphicOpenGLCoordBuffer.Add9QuadSprite(GLPixelWidth, GLPixelHeight: Single; X1, Y1, X2, Y2: Single);
begin
  //Перевести смещение в координаты OpenGL
  X1 := X1 * GLPixelWidth;
  Y1 := Y1 * GLPixelHeight;
  X2 := X2 * GLPixelWidth;
  Y2 := Y2 * GLPixelHeight;

  //Добавить коорднаты
  AddQuad(0, 1, X1, 1 - Y1);
  AddQuad(X1, 1, 1 - X2, 1 - Y1);
  AddQuad(1 - X2, 1, 1, 1 - Y1);
  AddQuad(0, 1 - Y1, X1, Y2);
  AddQuad(X1, 1 - Y1, 1 - X2, Y2);
  AddQuad(1 - X2, 1 - Y1, 1, Y2);
  AddQuad(0, Y2, X1, 0);
  AddQuad(X1, Y2, 1 - X2, 0);
  AddQuad(1 - X2, Y2, 1, 0);
end;


procedure TsgeGraphicOpenGLCoordBuffer.Add9QuadSprite(GLPixelWidth, GLPixelHeight: Single; Offset: TsgeFloatRect);
begin
  Add9QuadSprite(GLPixelWidth, GLPixelHeight, Offset.X1, Offset.Y1, Offset.X2, Offset.Y2);
end;



end.

