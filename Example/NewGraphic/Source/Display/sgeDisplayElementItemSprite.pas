{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSprite.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Спрайт
}
{$Include Defines.inc}

unit sgeDisplayElementItemSprite;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeSprite, sgeGraphicColor,
  sgeDisplayElementItemBase, sgeDisplayElementItemPropertyFloatRect, sgeDisplayElementItemPropertyColor,
  sgeDisplayElementItemPropertyScale, sgeDisplayElementItemPropertyRotate, sgeDisplayElementItemPropertyFloatPoint,
  sgeDisplayElementItemPropertySprite;

type
  TsgeDisplayElementItemSprite = class(TsgeDisplayElementItemBase)
  private
    FRect: TsgeDisplayElementItemPropertyFloatRect;
    FColor: TsgeDisplayElementItemPropertyColor;
    FScale: TsgeDisplayElementItemPropertyScale;
    FOrigin: TsgeDisplayElementItemPropertyFloatPoint;
    FRotate: TsgeDisplayElementItemPropertyRotate;
    FSprite: TsgeDisplayElementItemPropertySprite;

    procedure CreateObjects(X, Y, Width, Height: Single);
  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite);
    constructor Create(X, Y: Single; Sprite: TsgeSprite);
    destructor  Destroy; override;

    property Rect: TsgeDisplayElementItemPropertyFloatRect read FRect;
    property Color: TsgeDisplayElementItemPropertyColor read FColor;
    property Scale: TsgeDisplayElementItemPropertyScale read FScale;
    property Origin: TsgeDisplayElementItemPropertyFloatPoint read FOrigin;
    property Rotate: TsgeDisplayElementItemPropertyRotate read FRotate;
    property Sprite: TsgeDisplayElementItemPropertySprite read FSprite;
  end;


implementation


procedure TsgeDisplayElementItemSprite.CreateObjects(X, Y, Width, Height: Single);
begin
  FRect := TsgeDisplayElementItemPropertyFloatRect.Create(X, Y, X + Width, X + Height);
  FColor := TsgeDisplayElementItemPropertyColor.Create(cWhite);
  FScale := TsgeDisplayElementItemPropertyScale.Create;
  FOrigin := TsgeDisplayElementItemPropertyFloatPoint.Create;
  FRotate := TsgeDisplayElementItemPropertyRotate.Create;
end;


constructor TsgeDisplayElementItemSprite.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite);
begin
  FSprite := TsgeDisplayElementItemPropertySprite.Create(Sprite);
  CreateObjects(X, Y, Width, Height);
end;


constructor TsgeDisplayElementItemSprite.Create(X, Y: Single; Sprite: TsgeSprite);
begin
  FSprite := TsgeDisplayElementItemPropertySprite.Create(Sprite);
  CreateObjects(X, Y, Sprite.Width, Sprite.Height);
end;


destructor TsgeDisplayElementItemSprite.Destroy;
begin
  FSprite.Free;
  FRotate.Free;
  FOrigin.Free;
  FScale.Free;
  FColor.Free;
  FRect.Free;
end;



end.

