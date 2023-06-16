{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemAnimation.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Анимация
}
{$Include Defines.inc}

unit sgeDisplayElementItemAnimation;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeSprite, sgeGraphicColor, sgeAnimation, sgeAnimationFrameList,
  sgeDisplayElementItemBase, sgeDisplayElementItemPropertyFloatRect, sgeDisplayElementItemPropertyColor,
  sgeDisplayElementItemPropertyScale, sgeDisplayElementItemPropertyRotate, sgeDisplayElementItemPropertyFloatPoint;

type
  TsgeDisplayElementItemAnimation = class(TsgeDisplayElementItemBase)
  private
    FRect: TsgeDisplayElementItemPropertyFloatRect;
    FColor: TsgeDisplayElementItemPropertyColor;
    FScale: TsgeDisplayElementItemPropertyScale;
    FOrigin: TsgeDisplayElementItemPropertyFloatPoint;
    FRotate: TsgeDisplayElementItemPropertyRotate;
    FSprite: TsgeSprite;
    FAnimation: TsgeAnimation;

    procedure SetSprite(ASprite: TsgeSprite);
  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; AnimationFrames: TsgeAnimationFrameList);
    destructor  Destroy; override;

    procedure UpdateAnimation;

    property Rect: TsgeDisplayElementItemPropertyFloatRect read FRect;
    property Color: TsgeDisplayElementItemPropertyColor read FColor;
    property Scale: TsgeDisplayElementItemPropertyScale read FScale;
    property Origin: TsgeDisplayElementItemPropertyFloatPoint read FOrigin;
    property Rotate: TsgeDisplayElementItemPropertyRotate read FRotate;
    property Sprite: TsgeSprite read FSprite write SetSprite;
    property Animation: TsgeAnimation read FAnimation;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'DisplayElementItemAnimation';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementItemAnimation.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FSprite := ASprite;
end;


constructor TsgeDisplayElementItemAnimation.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; AnimationFrames: TsgeAnimationFrameList);
begin
  SetSprite(Sprite);

  FAnimation := TsgeAnimation.Create(AnimationFrames);

  FRect := TsgeDisplayElementItemPropertyFloatRect.Create(X, Y, X + Width, X + Height);
  FColor := TsgeDisplayElementItemPropertyColor.Create(cWhite);
  FScale := TsgeDisplayElementItemPropertyScale.Create;
  FOrigin := TsgeDisplayElementItemPropertyFloatPoint.Create;
  FRotate := TsgeDisplayElementItemPropertyRotate.Create;
end;


destructor TsgeDisplayElementItemAnimation.Destroy;
begin
  FAnimation.Free;
  FRotate.Free;
  FOrigin.Free;
  FScale.Free;
  FColor.Free;
  FRect.Free;
end;


procedure TsgeDisplayElementItemAnimation.UpdateAnimation;
begin
  if FAnimation.IsFrameChanged then
    //Послать событие в класс графики о смене параметра FAnimation.CurrentFrameIndex
end;



end.

