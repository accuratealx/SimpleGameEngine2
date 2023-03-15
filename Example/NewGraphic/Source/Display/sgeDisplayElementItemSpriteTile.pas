{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSpriteTile.pas
Версия            1.0
Создан            15.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Плитка спрайта
}
{$Include Defines.inc}

unit sgeDisplayElementItemSpriteTile;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeSprite, sgeGraphicColor,
  sgeDisplayElementItemBase, sgeDisplayElementItemPropertyFloatRect, sgeDisplayElementItemPropertyColor,
  sgeDisplayElementItemPropertyScale, sgeDisplayElementItemPropertyRotate, sgeDisplayElementItemPropertyFloatPoint,
  sgeDisplayElementItemPropertyIntPoint;

type
  TsgeDisplayElementItemSpriteTile = class(TsgeDisplayElementItemBase)
  private
    FRect: TsgeDisplayElementItemPropertyFloatRect;
    FColor: TsgeDisplayElementItemPropertyColor;
    FScale: TsgeDisplayElementItemPropertyScale;
    FOrigin: TsgeDisplayElementItemPropertyFloatPoint;
    FRotate: TsgeDisplayElementItemPropertyRotate;
    FTile: TsgeDisplayElementItemPropertyIntPoint;
    FSprite: TsgeSprite;

    procedure CreateObjects(X, Y, Width, Height: Single; Col, Row: Integer);
    procedure SetSprite(ASprite: TsgeSprite);
  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; Col, Row: Integer);
    constructor Create(X, Y: Single; Sprite: TsgeSprite; Col, Row: Integer);
    destructor  Destroy; override;

    property Rect: TsgeDisplayElementItemPropertyFloatRect read FRect;
    property Color: TsgeDisplayElementItemPropertyColor read FColor;
    property Scale: TsgeDisplayElementItemPropertyScale read FScale;
    property Origin: TsgeDisplayElementItemPropertyFloatPoint read FOrigin;
    property Rotate: TsgeDisplayElementItemPropertyRotate read FRotate;
    property Tile: TsgeDisplayElementItemPropertyIntPoint read FTile;
    property Sprite: TsgeSprite read FSprite write SetSprite;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'sgeDisplayElementItemSprite';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementItemSpriteTile.CreateObjects(X, Y, Width, Height: Single; Col, Row: Integer);
begin
  FRect := TsgeDisplayElementItemPropertyFloatRect.Create(X, Y, X + Width, X + Height);
  FColor := TsgeDisplayElementItemPropertyColor.Create(cWhite);
  FScale := TsgeDisplayElementItemPropertyScale.Create;
  FOrigin := TsgeDisplayElementItemPropertyFloatPoint.Create;
  FRotate := TsgeDisplayElementItemPropertyRotate.Create;
  FTile := TsgeDisplayElementItemPropertyIntPoint.Create(Col, Row);
end;


procedure TsgeDisplayElementItemSpriteTile.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FSprite := ASprite;
end;


constructor TsgeDisplayElementItemSpriteTile.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; Col, Row: Integer);
begin
  SetSprite(Sprite);
  CreateObjects(X, Y, Width, Height, Col, Row);
end;


constructor TsgeDisplayElementItemSpriteTile.Create(X, Y: Single; Sprite: TsgeSprite; Col, Row: Integer);
begin
  SetSprite(Sprite);
  CreateObjects(X, Y, Sprite.Width, Sprite.Height, Col, Row);
end;


destructor TsgeDisplayElementItemSpriteTile.Destroy;
begin
  FTile.Free;
  FRotate.Free;
  FOrigin.Free;
  FScale.Free;
  FColor.Free;
  FRect.Free;
end;



end.


