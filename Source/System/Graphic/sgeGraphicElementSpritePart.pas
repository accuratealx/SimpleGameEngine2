{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementSpritePart.pas
Версия            1.1
Создан            25.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Часть спрайта
}
{$Include Defines.inc}

unit sgeGraphicElementSpritePart;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeGraphic, sgeGraphicSprite,
  sgeGraphicElementSprite;


type
  TsgeGraphicElementSpritePart = class(TsgeGraphicElementSprite)
  private
    //Координаты спрайта в пикселях
    FSpriteRect: TsgeFloatRect;

    procedure SetSpriteX1(AX: Single);
    procedure SetSpriteY1(AY: Single);
    procedure SetSpriteX2(AX: Single);
    procedure SetSpriteY2(AY: Single);

    procedure PostCreate;
  public
    constructor Create(X, Y: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType = gctNormal); override;
    constructor Create(X, Y, W, H: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType = gctNormal); override;

    property SpriteX1: Single read FSpriteRect.X1 write SetSpriteX1;
    property SpriteY1: Single read FSpriteRect.Y1 write SetSpriteY1;
    property SpriteX2: Single read FSpriteRect.X2 write SetSpriteX2;
    property SpriteY2: Single read FSpriteRect.Y2 write SetSpriteY2;
  end;



implementation

uses
  sgeGraphicUtils;



procedure TsgeGraphicElementSpritePart.SetSpriteX1(AX: Single);
begin
  FSpriteRect.X1 := AX;
  FNewData.SpriteRect := sgeGetTextureRect(FNewData.Sprite, FSpriteRect);
end;


procedure TsgeGraphicElementSpritePart.SetSpriteY1(AY: Single);
begin
  FSpriteRect.Y1 := AY;
  FNewData.SpriteRect := sgeGetTextureRect(FNewData.Sprite, FSpriteRect);
end;


procedure TsgeGraphicElementSpritePart.SetSpriteX2(AX: Single);
begin
  FSpriteRect.X2 := AX;
  FNewData.SpriteRect := sgeGetTextureRect(FNewData.Sprite, FSpriteRect);
end;


procedure TsgeGraphicElementSpritePart.SetSpriteY2(AY: Single);
begin
  FSpriteRect.Y2 := AY;
  FNewData.SpriteRect := sgeGetTextureRect(FNewData.Sprite, FSpriteRect);
end;

procedure TsgeGraphicElementSpritePart.PostCreate;
begin
  //Задать начальные размеры вывода в пикселях
  FSpriteRect := sgeGetFloatRect(0, 0, FNewData.Sprite.Width, FNewData.Sprite.Height);

  //Подготовить координаты спрайта для вывода
  FNewData.SpriteRect := sgeGetTextureRect(FNewData.Sprite, FSpriteRect);
  FData.SpriteRect := FNewData.SpriteRect;
end;


constructor TsgeGraphicElementSpritePart.Create(X, Y: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType);
begin
  inherited Create(X, Y, Sprite, CoordType);

  PostCreate;
end;


constructor TsgeGraphicElementSpritePart.Create(X, Y, W, H: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType);
begin
  inherited Create(X, Y, W, H, Sprite, CoordType);

  PostCreate;
end;



end.

