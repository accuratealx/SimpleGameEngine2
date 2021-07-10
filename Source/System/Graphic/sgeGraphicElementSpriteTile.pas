{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementSpriteTile.pas
Версия            1.0
Создан            25.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Плитка спрайта
}
{$Include Defines.inc}

unit sgeGraphicElementSpriteTile;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicElementSprite;


type
  TsgeGraphicElementSpriteTile = class(TsgeGraphicElementSprite)
  private
    FCol: Word;
    FRow: Word;

    procedure AfterConstruction; override;

    procedure SetCol(ACol: Word);
    procedure SetRow(ARow: Word);
  public

    property Col: Word read FCol write SetCol;
    property Row: Word read FRow write SetRow;
  end;


implementation

uses
  sgeGraphicUtils;


procedure TsgeGraphicElementSpriteTile.AfterConstruction;
begin
  inherited AfterConstruction;

  //Задать начальные координаты плитки
  FCol := 0;
  FRow := 0;
  FNewData.SpriteRect := sgeGetTextureTileRect(FNewData.Sprite, FCol, FRow);
  FData.SpriteRect := FNewData.SpriteRect;
end;


procedure TsgeGraphicElementSpriteTile.SetCol(ACol: Word);
begin
  if FCol = ACol then Exit;

  FCol := ACol;
  FNewData.SpriteRect := sgeGetTextureTileRect(FNewData.Sprite, FCol, FRow);
end;


procedure TsgeGraphicElementSpriteTile.SetRow(ARow: Word);
begin
  if FRow = ARow then Exit;

  FRow := ARow;
  FNewData.SpriteRect := sgeGetTextureTileRect(FNewData.Sprite, FCol, FRow);
end;




end.

