{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemPropertyFloatRect.pas
Версия            1.0
Создан            08.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Свойство элемента отображения: Плавающий прямоугольник
}
{$Include Defines.inc}

unit sgeDisplayElementItemPropertyFloatRect;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes;

type
  TsgeDisplayElementItemPropertyFloatRect = class
  private
    FRect: TsgeFloatRect;

    procedure SetWidth(AWidth: Single);
    function  GetWidth: Single;
    procedure SetHeight(AHeight: Single);
    function  GetHeight: Single;
  public
    constructor Create(Rect: TsgeFloatRect);
    constructor Create(X1, Y1, X2, Y2: Single);

    property Rect: TsgeFloatRect read FRect write FRect;
    property X1: Single read FRect.X1 write FRect.X1;
    property Y1: Single read FRect.Y1 write FRect.Y1;
    property X2: Single read FRect.X2 write FRect.X2;
    property Y2: Single read FRect.Y2 write FRect.Y2;
    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;
  end;


implementation


procedure TsgeDisplayElementItemPropertyFloatRect.SetWidth(AWidth: Single);
begin
  FRect.X2 := FRect.X1 + AWidth;
end;


function TsgeDisplayElementItemPropertyFloatRect.GetWidth: Single;
begin
  Result := FRect.X2 - FRect.X1;
end;


procedure TsgeDisplayElementItemPropertyFloatRect.SetHeight(AHeight: Single);
begin
  FRect.Y2 := FRect.Y1 + AHeight;
end;


function TsgeDisplayElementItemPropertyFloatRect.GetHeight: Single;
begin
  Result := FRect.Y2 - FRect.Y1;
end;


constructor TsgeDisplayElementItemPropertyFloatRect.Create(Rect: TsgeFloatRect);
begin
  FRect := Rect;
end;


constructor TsgeDisplayElementItemPropertyFloatRect.Create(X1, Y1, X2, Y2: Single);
begin
  FRect.X1 := X1;
  FRect.Y1 := Y1;
  FRect.X2 := X2;
  FRect.Y2 := Y2;
end;



end.

