{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSpritePart.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Спрайт
}
{$Include Defines.inc}

unit sgeDisplayElementItemSpritePart;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeSprite, sgeGraphicColor,
  sgeDisplayElementItemBase, sgeDisplayElementItemPropertyFloatRect, sgeDisplayElementItemPropertyColor,
  sgeDisplayElementItemPropertyScale, sgeDisplayElementItemPropertyRotate, sgeDisplayElementItemPropertyFloatPoint;

type
  TsgeDisplayElementItemSpritePart = class(TsgeDisplayElementItemBase)
  private
    FRect: TsgeDisplayElementItemPropertyFloatRect;
    FColor: TsgeDisplayElementItemPropertyColor;
    FScale: TsgeDisplayElementItemPropertyScale;
    FOrigin: TsgeDisplayElementItemPropertyFloatPoint;
    FRotate: TsgeDisplayElementItemPropertyRotate;
    FSpriteRect: TsgeDisplayElementItemPropertyFloatRect;
    FSprite: TsgeSprite;

    procedure CreateObjects(X, Y, Width, Height: Single; X1, Y1, X2, Y2: Single);
    procedure SetSprite(ASprite: TsgeSprite);
  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; SpriteX1, SpriteY1, SpriteX2, SpriteY2: Single);
    constructor Create(X, Y: Single; Sprite: TsgeSprite; SpriteX1, SpriteY1, SpriteX2, SpriteY2: Single);
    destructor  Destroy; override;

    property Rect: TsgeDisplayElementItemPropertyFloatRect read FRect;
    property Color: TsgeDisplayElementItemPropertyColor read FColor;
    property Scale: TsgeDisplayElementItemPropertyScale read FScale;
    property Origin: TsgeDisplayElementItemPropertyFloatPoint read FOrigin;
    property Rotate: TsgeDisplayElementItemPropertyRotate read FRotate;
    property SpriteRect: TsgeDisplayElementItemPropertyFloatRect read FSpriteRect;
    property Sprite: TsgeSprite read FSprite write SetSprite;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'DisplayElementItemSprite';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementItemSpritePart.CreateObjects(X, Y, Width, Height: Single; X1, Y1, X2, Y2: Single);
begin
  FRect := TsgeDisplayElementItemPropertyFloatRect.Create(X, Y, X + Width, X + Height);
  FColor := TsgeDisplayElementItemPropertyColor.Create(cWhite);
  FScale := TsgeDisplayElementItemPropertyScale.Create;
  FOrigin := TsgeDisplayElementItemPropertyFloatPoint.Create;
  FRotate := TsgeDisplayElementItemPropertyRotate.Create;
  FSpriteRect := TsgeDisplayElementItemPropertyFloatRect.Create(X1, Y1, X2, Y2);
end;


procedure TsgeDisplayElementItemSpritePart.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FSprite := ASprite;
end;


constructor TsgeDisplayElementItemSpritePart.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; SpriteX1, SpriteY1, SpriteX2, SpriteY2: Single);
begin
  SetSprite(Sprite);
  CreateObjects(X, Y, Width, Height, SpriteX1, SpriteY1, SpriteX2, SpriteY2);
end;


constructor TsgeDisplayElementItemSpritePart.Create(X, Y: Single; Sprite: TsgeSprite; SpriteX1, SpriteY1, SpriteX2, SpriteY2: Single);
begin
  SetSprite(Sprite);
  CreateObjects(X, Y, Sprite.Width, Sprite.Height, SpriteX1, SpriteY1, SpriteX2, SpriteY2);
end;


destructor TsgeDisplayElementItemSpritePart.Destroy;
begin
  FSpriteRect.Free;
  FRotate.Free;
  FOrigin.Free;
  FScale.Free;
  FColor.Free;
  FRect.Free;
end;



end.


