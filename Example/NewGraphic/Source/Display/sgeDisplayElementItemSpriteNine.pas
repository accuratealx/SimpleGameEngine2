{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSpriteNine.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Спрайт 9
}
{$Include Defines.inc}

unit sgeDisplayElementItemSpriteNine;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeSprite, sgeGraphicColor,
  sgeDisplayElementItemBase, sgeDisplayElementItemPropertyFloatRect, sgeDisplayElementItemPropertyColor,
  sgeDisplayElementItemPropertyScale, sgeDisplayElementItemPropertyRotate, sgeDisplayElementItemPropertyFloatPoint;

type
  TsgeDisplayElementItemSpriteNine = class(TsgeDisplayElementItemBase)
  private
    FRect: TsgeDisplayElementItemPropertyFloatRect;
    FColor: TsgeDisplayElementItemPropertyColor;
    FScale: TsgeDisplayElementItemPropertyScale;
    FOrigin: TsgeDisplayElementItemPropertyFloatPoint;
    FRotate: TsgeDisplayElementItemPropertyRotate;
    FOffset: TsgeDisplayElementItemPropertyFloatRect;
    FSprite: TsgeSprite;

    procedure CreateObjects(X, Y, Width, Height: Single; OffsetX1, OffsetY1, OffsetX2, OffsetY2: Single);
    procedure SetSprite(ASprite: TsgeSprite);
  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; OffsetX1: Single = 0; OffsetY1: Single = 0; OffsetX2: Single = 0; OffsetY2: Single = 0);
    constructor Create(X, Y: Single; Sprite: TsgeSprite; OffsetX1: Single = 0; OffsetY1: Single = 0; OffsetX2: Single = 0; OffsetY2: Single = 0);
    destructor  Destroy; override;

    property Rect: TsgeDisplayElementItemPropertyFloatRect read FRect;
    property Color: TsgeDisplayElementItemPropertyColor read FColor;
    property Scale: TsgeDisplayElementItemPropertyScale read FScale;
    property Origin: TsgeDisplayElementItemPropertyFloatPoint read FOrigin;
    property Rotate: TsgeDisplayElementItemPropertyRotate read FRotate;
    property Offset: TsgeDisplayElementItemPropertyFloatRect read FOffset;
    property Sprite: TsgeSprite read FSprite write SetSprite;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'DisplayElementItemSpriteNine';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementItemSpriteNine.CreateObjects(X, Y, Width, Height: Single; OffsetX1, OffsetY1, OffsetX2, OffsetY2: Single);
begin
  FRect := TsgeDisplayElementItemPropertyFloatRect.Create(X, Y, X + Width, X + Height);
  FColor := TsgeDisplayElementItemPropertyColor.Create(cWhite);
  FScale := TsgeDisplayElementItemPropertyScale.Create;
  FOrigin := TsgeDisplayElementItemPropertyFloatPoint.Create;
  FRotate := TsgeDisplayElementItemPropertyRotate.Create;
  FOffset := TsgeDisplayElementItemPropertyFloatRect.Create(OffsetX1, OffsetY1, OffsetX2, OffsetY2);
end;


procedure TsgeDisplayElementItemSpriteNine.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FSprite := ASprite;
end;


constructor TsgeDisplayElementItemSpriteNine.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; OffsetX1: Single; OffsetY1: Single; OffsetX2: Single; OffsetY2: Single);
begin
  SetSprite(Sprite);
  CreateObjects(X, Y, Width, Height, OffsetX1, OffsetY1, OffsetX2, OffsetY2);
end;


constructor TsgeDisplayElementItemSpriteNine.Create(X, Y: Single; Sprite: TsgeSprite; OffsetX1: Single; OffsetY1: Single; OffsetX2: Single; OffsetY2: Single);
begin
  SetSprite(Sprite);
  CreateObjects(X, Y, Sprite.Width, Sprite.Height, OffsetX1, OffsetY1, OffsetX2, OffsetY2);
end;


destructor TsgeDisplayElementItemSpriteNine.Destroy;
begin
  FOffset.Free;
  FRotate.Free;
  FOrigin.Free;
  FScale.Free;
  FColor.Free;
  FRect.Free;
end;



end.

