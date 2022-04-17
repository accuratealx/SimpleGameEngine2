{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementSpriteTile.pas
Версия            1.1
Создан            25.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Плитка спрайта
}
{$Include Defines.inc}

unit sgeGraphicElementSpriteTile;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeGraphicSprite, sgeGraphic,
  sgeGraphicElementSprite;


type
  TsgeGraphicElementSpriteTile = class(TsgeGraphicElementSprite)
  private
    FCol: Word;
    FRow: Word;

    procedure SetCol(ACol: Word);
    procedure SetRow(ARow: Word);

    procedure PostCreate;
  public
    constructor Create(X, Y: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType = gctNormal); override;
    constructor Create(X, Y, W, H: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType = gctNormal); override;

    property Col: Word read FCol write SetCol;
    property Row: Word read FRow write SetRow;
  end;


implementation

uses
  sgeGraphicUtils;



procedure TsgeGraphicElementSpriteTile.SetCol(ACol: Word);
begin
  if FCol = ACol then
    Exit;

  FCol := ACol;
  FNewData.SpriteRect := sgeGetTextureTileRect(FNewData.Sprite, FCol, FRow);
end;


procedure TsgeGraphicElementSpriteTile.SetRow(ARow: Word);
begin
  if FRow = ARow then
    Exit;

  FRow := ARow;
  FNewData.SpriteRect := sgeGetTextureTileRect(FNewData.Sprite, FCol, FRow);
end;

procedure TsgeGraphicElementSpriteTile.PostCreate;
begin
  //Задать начальные координаты плитки
  FCol := 0;
  FRow := 0;
  FNewData.SpriteRect := sgeGetTextureTileRect(FNewData.Sprite, FCol, FRow);
  FData.SpriteRect := FNewData.SpriteRect;
end;


constructor TsgeGraphicElementSpriteTile.Create(X, Y: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType);
begin
  inherited Create(X, Y, Sprite, CoordType);

  PostCreate;
end;

constructor TsgeGraphicElementSpriteTile.Create(X, Y, W, H: Single; Sprite: TsgeGraphicSprite; CoordType: TsgeGraphicCoordinateType);
begin
  inherited Create(X, Y, W, H, Sprite, CoordType);

  PostCreate;
end;



end.

