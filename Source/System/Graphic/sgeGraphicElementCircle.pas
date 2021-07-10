{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementCircle.pas
Версия            1.0
Создан            19.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Круг
}
{$Include Defines.inc}

unit sgeGraphicElementCircle;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicElementBase,
  sgeGraphic, sgeGraphicColor;


type
  //Настройки элемента
  TsgeGraphicElementCircleData = record
    X: Single;
    Y: Single;
    Radius: Single;
    Quality: Word;
    Color: TsgeColor;
  end;


  TsgeGraphicElementCircle = class(TsgeGraphicElementBase)
  private
    FData: TsgeGraphicElementCircleData;
    FNewData: TsgeGraphicElementCircleData;

    procedure SetAlpha(AAlpha: Single);
    function  GetAlpha: Single;
  public
    constructor Create;
    constructor Create(X, Y, Radius: Single; Color: TsgeColor; Quality: Word = 16);

    procedure UpdateData; override;
    procedure Draw(Graphic: TsgeGraphic); override;

    property Alpha: Single read GetAlpha write SetAlpha;
    property X: Single read FNewData.X write FNewData.X;
    property Y: Single read FNewData.Y write FNewData.Y;
    property Radius: Single read FNewData.Radius write FNewData.Radius;
    property Quality: Word read FNewData.Quality write FNewData.Quality;
    property Color: TsgeColor read FNewData.Color write FNewData.Color;
  end;



implementation


procedure TsgeGraphicElementCircle.SetAlpha(AAlpha: Single);
begin
  FNewData.Color := sgeChangeColorAlpha(FNewData.Color, AAlpha);
end;


function TsgeGraphicElementCircle.GetAlpha: Single;
begin
  Result := FNewData.Color.Alpha;
end;


constructor TsgeGraphicElementCircle.Create;
begin
  FData.X := 0;
  FData.Y := 0;
  FData.Radius := 0;
  FData.Quality := 16;
  FData.Color := cBlack;

  FNewData := FData;
end;


constructor TsgeGraphicElementCircle.Create(X, Y, Radius: Single; Color: TsgeColor; Quality: Word);
begin
  FData.X := X;
  FData.Y := Y;
  FData.Radius := Radius;
  FData.Quality := Quality;
  FData.Color := Color;

  FNewData := FData;
end;


procedure TsgeGraphicElementCircle.UpdateData;
begin
  FData := FNewData;
end;


procedure TsgeGraphicElementCircle.Draw(Graphic: TsgeGraphic);
begin
  Graphic.DrawCircle(FData.X, FData.Y, FData.Radius, FData.Color, FData.Quality);
end;




end.


