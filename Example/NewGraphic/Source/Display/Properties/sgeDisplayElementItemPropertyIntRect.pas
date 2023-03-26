{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemPropertyIntRect.pas
Версия            1.0
Создан            22.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Свойство элемента отображения: Целочисленный прямоугольник
}
{$Include Defines.inc}

unit sgeDisplayElementItemPropertyIntRect;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes;

type
  TsgeDisplayElementItemPropertyIntRect = class
  private
    FRect: TsgeIntRect;

    procedure SetWidth(AWidth: Integer);
    function  GetWidth: Integer;
    procedure SetHeight(AHeight: Integer);
    function  GetHeight: Integer;
  public
    constructor Create(Rect: TsgeIntRect);
    constructor Create(X1, Y1, X2, Y2: Integer);

    property Rect: TsgeIntRect read FRect write FRect;
    property X1: Integer read FRect.X1 write FRect.X1;
    property Y1: Integer read FRect.Y1 write FRect.Y1;
    property X2: Integer read FRect.X2 write FRect.X2;
    property Y2: Integer read FRect.Y2 write FRect.Y2;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;


implementation


procedure TsgeDisplayElementItemPropertyIntRect.SetWidth(AWidth: Integer);
begin
  FRect.X2 := FRect.X1 + AWidth;
end;


function TsgeDisplayElementItemPropertyIntRect.GetWidth: Integer;
begin
  Result := FRect.X2 - FRect.X1;
end;


procedure TsgeDisplayElementItemPropertyIntRect.SetHeight(AHeight: Integer);
begin
  FRect.Y2 := FRect.Y1 + AHeight;
end;


function TsgeDisplayElementItemPropertyIntRect.GetHeight: Integer;
begin
  Result := FRect.Y2 - FRect.Y1;
end;


constructor TsgeDisplayElementItemPropertyIntRect.Create(Rect: TsgeIntRect);
begin
  FRect := Rect;
end;


constructor TsgeDisplayElementItemPropertyIntRect.Create(X1, Y1, X2, Y2: Integer);
begin
  FRect.X1 := X1;
  FRect.Y1 := Y1;
  FRect.X2 := X2;
  FRect.Y2 := Y2;
end;



end.

