{
Пакет             Simple Game Engine 2
Файл              sgeGraphicBuffer.pas
Версия            1.0
Создан            09.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс буфера
}
{$Include Defines.inc}

unit sgeGraphicBuffer;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes;

type
  TsgeGraphicBuffer = class
  private
    FData: Pointer;
    FSize: Int64;
  public
    destructor  Destroy; override;

    procedure Clear;
    procedure AddPoint(X, Y: Single);
    procedure AddPoint(Point: TsgeFloatPoint);
    procedure AddQuad(X1, Y1, X2, Y2: Single);

    property Data: Pointer read FData;
    property Size: Int64 read FSize;
  end;


implementation

const
  //Размер одного значения
  DATA_SIZE = SizeOf(Single);


destructor TsgeGraphicBuffer.Destroy;
begin
  Clear;
end;


procedure TsgeGraphicBuffer.Clear;
begin
  //Освободить память
  Freemem(FData, FSize);

  FSize := 0;
end;


procedure TsgeGraphicBuffer.AddPoint(X, Y: Single);
begin
  //Новый размер
  FSize := FSize + 2 * DATA_SIZE;

  //Выделить больше памяти
  FData := ReAllocMem(FData, FSize);

  //Записать данные
  PSingle(FData + FSize - DATA_SIZE * 1)^ := Y;
  PSingle(FData + FSize - DATA_SIZE * 2)^ := X;
end;


procedure TsgeGraphicBuffer.AddPoint(Point: TsgeFloatPoint);
begin
  AddPoint(Point.X, Point.Y);
end;


procedure TsgeGraphicBuffer.AddQuad(X1, Y1, X2, Y2: Single);
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



end.

