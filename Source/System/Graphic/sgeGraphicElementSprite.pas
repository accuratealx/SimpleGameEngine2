{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementSprite.pas
Версия            1.1
Создан            24.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Спрайт
}
{$Include Defines.inc}

unit sgeGraphicElementSprite;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeGraphicElementBase,
  sgeGraphic, sgeGraphicColor, sgeGraphicSprite;

type
  //Настройки элемента
  TsgeGraphicElementSpriteData = TsgeGraphicDrawOptions;


  TsgeGraphicElementSprite = class(TsgeGraphicElementBase)
  protected
    FData: TsgeGraphicElementSpriteData;
    FNewData: TsgeGraphicElementSpriteData;

    procedure SetAlpha(AAlpha: Single);
    function  GetAlpha: Single;

    procedure UpdateData; override;
  public
    constructor Create(X, Y: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType = gctNormal); virtual;
    constructor Create(X, Y, W, H: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType = gctNormal); virtual;

    procedure Draw(Graphic: TsgeGraphic); override;

    property Alpha: Single read GetAlpha write SetAlpha;
    property X: Single read FNewData.Rect.X1 write FNewData.Rect.X1;
    property Y: Single read FNewData.Rect.Y1 write FNewData.Rect.Y1;
    property W: Single read FNewData.Rect.X2 write FNewData.Rect.X2;
    property H: Single read FNewData.Rect.Y2 write FNewData.Rect.Y2;
    property Angle: Single read FNewData.Angle write FNewData.Angle;
    property Anglepoint: TsgeFloatPoint read FNewData.AnglePoint write FNewData.AnglePoint;
    property Scale: TsgeFloatPoint read FNewData.Scale write FNewData.Scale;
    property CoordType: TsgeGraphicCoordinateType read FNewData.CoordinateType write FNewData.CoordinateType;
    property TransparentColor: TsgeColor read FNewData.TransparentColor write FNewData.TransparentColor;
  	property Reflect: TsgeGraphicReflectTypes read FNewData.Reflect write FNewData.Reflect;
    property Sprite: TsgeGraphicSprite read FNewData.Sprite write FNewData.Sprite;
  end;



implementation


procedure TsgeGraphicElementSprite.SetAlpha(AAlpha: Single);
begin
  FNewData.TransparentColor := sgeChangeColorAlpha(FNewData.TransparentColor, AAlpha);
end;


function TsgeGraphicElementSprite.GetAlpha: Single;
begin
  Result := FNewData.TransparentColor.Alpha;
end;


procedure TsgeGraphicElementSprite.UpdateData;
begin
  FData := FNewData;
end;


constructor TsgeGraphicElementSprite.Create(X, Y: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType);
begin
  inherited Create;

  FData := DefaultDrawOptions;

  FData.Sprite := Sprite;
  FData.CoordinateType := CoordType;
  FData.Rect.X1 := X;
  FData.Rect.Y1 := Y;
  FData.Rect.X2 := Sprite.Width;
  FData.Rect.Y2 := Sprite.Height;

  FNewData := FData;
end;


constructor TsgeGraphicElementSprite.Create(X, Y, W, H: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType);
begin
  inherited Create;

  FData := DefaultDrawOptions;

  FData.Sprite := Sprite;
  FData.CoordinateType := CoordType;
  FData.Rect.X1 := X;
  FData.Rect.Y1 := Y;
  FData.Rect.X2 := W;
  FData.Rect.Y2 := H;

  FNewData := FData;
end;


procedure TsgeGraphicElementSprite.Draw(Graphic: TsgeGraphic);
begin
  Graphic.DrawSprite(FData);
end;



end.

