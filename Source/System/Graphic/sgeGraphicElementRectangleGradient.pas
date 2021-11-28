{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementGradientRectangle.pas
Версия            1.1
Создан            24.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Градиентный прямоугольник
}
{$Include Defines.inc}

unit sgeGraphicElementRectangleGradient;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicElementBase,
  sgeGraphic, sgeGraphicColor;


type
  //Настройки элемента
  TsgeGraphicElementRectangleGradientData = record
    X1: Single;
    Y1: Single;
    X2: Single;
    Y2: Single;
    CoordType: TsgeGraphicCoordinateType;
    Color1: TsgeColor;
    Color2: TsgeColor;
    Color3: TsgeColor;
    Color4: TsgeColor;
  end;


  TsgeGraphicElementRectangleGradient = class(TsgeGraphicElementBase)
  private
    FData: TsgeGraphicElementRectangleGradientData;
    FNewData: TsgeGraphicElementRectangleGradientData;

  protected
    procedure UpdateData; override;
  public
    constructor Create;
    constructor Create(X1, Y1, X2, Y2: Single; CoordType: TsgeGraphicCoordinateType; Color1, Color2, Color3, Color4: TsgeColor);

    procedure Draw(Graphic: TsgeGraphic); override;

    property X1: Single read FNewData.X1 write FNewData.X1;
    property Y1: Single read FNewData.Y1 write FNewData.Y1;
    property X2: Single read FNewData.X2 write FNewData.X2;
    property Y2: Single read FNewData.Y2 write FNewData.Y2;
    property CoordType: TsgeGraphicCoordinateType read FNewData.CoordType write FNewData.CoordType;
    property Color1: TsgeColor read FNewData.Color1 write FNewData.Color1;
    property Color2: TsgeColor read FNewData.Color2 write FNewData.Color2;
    property Color3: TsgeColor read FNewData.Color3 write FNewData.Color3;
    property Color4: TsgeColor read FNewData.Color4 write FNewData.Color4;
  end;


implementation


procedure TsgeGraphicElementRectangleGradient.UpdateData;
begin
  FData := FNewData;
end;


constructor TsgeGraphicElementRectangleGradient.Create;
begin
  inherited Create;

  FData.X1 := 0;
  FData.Y1 := 0;
  FData.X2 := 0;
  FData.Y2 := 0;
  FData.CoordType := gctClassic;
  FData.Color1 := cBlack;
  FData.Color2 := cBlack;
  FData.Color3 := cBlack;
  FData.Color4 := cBlack;

  FNewData := FData;
end;


constructor TsgeGraphicElementRectangleGradient.Create(X1, Y1, X2, Y2: Single; CoordType: TsgeGraphicCoordinateType; Color1, Color2, Color3, Color4: TsgeColor);
begin
  inherited Create;

  FData.X1 := X1;
  FData.Y1 := Y1;
  FData.X2 := X2;
  FData.Y2 := Y2;
  FData.CoordType := CoordType;
  FData.Color1 := Color1;
  FData.Color2 := Color2;
  FData.Color3 := Color3;
  FData.Color4 := Color4;

  FNewData := FData;
end;


procedure TsgeGraphicElementRectangleGradient.Draw(Graphic: TsgeGraphic);
begin
  Graphic.doCoordinateType := FData.CoordType;
  Graphic.DrawRectGradient(FData.X1, FData.Y1, FData.X2, FData.Y2, sgeGetQuadColor(FData.Color1, FData.Color2, FData.Color3, FData.Color4));
end;




end.


