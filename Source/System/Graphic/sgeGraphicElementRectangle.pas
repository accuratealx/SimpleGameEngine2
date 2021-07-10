{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementRectangle.pas
Версия            1.0
Создан            24.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Прямоугольник
}
{$Include Defines.inc}

unit sgeGraphicElementRectangle;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicElementBase,
  sgeGraphic, sgeGraphicColor;


type
  //Настройки элемента
  TsgeGraphicElementRectangleData = record
    X1: Single;
    Y1: Single;
    X2: Single;
    Y2: Single;
    CoordType: TsgeGraphicCoordinateType;
    Color: TsgeColor;
  end;


  TsgeGraphicElementRectangle = class(TsgeGraphicElementBase)
  private
    FData: TsgeGraphicElementRectangleData;
    FNewData: TsgeGraphicElementRectangleData;

    procedure SetAlpha(AAlpha: Single);
    function  GetAlpha: Single;
  public
    constructor Create;
    constructor Create(X1, Y1, X2, Y2: Single; CoordType: TsgeGraphicCoordinateType; Color: TsgeColor);

    procedure UpdateData; override;
    procedure Draw(Graphic: TsgeGraphic); override;

    property Alpha: Single read GetAlpha write SetAlpha;
    property X1: Single read FNewData.X1 write FNewData.X1;
    property Y1: Single read FNewData.Y1 write FNewData.Y1;
    property X2: Single read FNewData.X2 write FNewData.X2;
    property Y2: Single read FNewData.Y2 write FNewData.Y2;
    property CoordType: TsgeGraphicCoordinateType read FNewData.CoordType write FNewData.CoordType;
    property Color: TsgeColor read FNewData.Color write FNewData.Color;
  end;

implementation


procedure TsgeGraphicElementRectangle.SetAlpha(AAlpha: Single);
begin
  FNewData.Color := sgeChangeColorAlpha(FNewData.Color, AAlpha);
end;


function TsgeGraphicElementRectangle.GetAlpha: Single;
begin
  Result := FNewData.Color.Alpha;
end;


constructor TsgeGraphicElementRectangle.Create;
begin
  FData.X1 := 0;
  FData.Y1 := 0;
  FData.X2 := 0;
  FData.Y2 := 0;
  FData.CoordType := gctClassic;
  FData.Color := cBlack;

  FNewData := FData;
end;


constructor TsgeGraphicElementRectangle.Create(X1, Y1, X2, Y2: Single; CoordType: TsgeGraphicCoordinateType; Color: TsgeColor);
begin
  FData.X1 := X1;
  FData.Y1 := Y1;
  FData.X2 := X2;
  FData.Y2 := Y2;
  FData.CoordType := CoordType;
  FData.Color := Color;

  FNewData := FData;
end;


procedure TsgeGraphicElementRectangle.UpdateData;
begin
  FData := FNewData;
end;


procedure TsgeGraphicElementRectangle.Draw(Graphic: TsgeGraphic);
begin
  Graphic.doCoordinateType := FData.CoordType;
  Graphic.DrawRect(FData.X1, FData.Y1, FData.X2, FData.Y2, FData.Color);
end;



end.

