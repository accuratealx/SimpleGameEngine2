{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemRect.pas
Версия            1.0
Создан            27.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент отображения: Цветной прямоугольник
}
{$Include Defines.inc}

unit sgeDisplayElementItemRect;

{$mode ObjFPC}{$H+}

interface

uses
  sgeGraphicColor,
  sgeDisplayElementItemBase, sgeDisplayElementItemPropertyFloatRect, sgeDisplayElementItemPropertyColor,
  sgeDisplayElementItemPropertyScale, sgeDisplayElementItemPropertyRotate, sgeDisplayElementItemPropertyFloatPoint;

type
  TsgeDisplayElementItemRect = class(TsgeDisplayElementItemBase)
  private
    FRect: TsgeDisplayElementItemPropertyFloatRect;
    FColor: TsgeDisplayElementItemPropertyColor;
    FScale: TsgeDisplayElementItemPropertyScale;
    FOrigin: TsgeDisplayElementItemPropertyFloatPoint;
    FRotate: TsgeDisplayElementItemPropertyRotate;
  public
    constructor Create(X1, Y1, X2, Y2: Single; Color: TsgeColor);
    destructor  Destroy; override;

    property Rect: TsgeDisplayElementItemPropertyFloatRect read FRect;
    property Color: TsgeDisplayElementItemPropertyColor read FColor;
    property Scale: TsgeDisplayElementItemPropertyScale read FScale;
    property Origin: TsgeDisplayElementItemPropertyFloatPoint read FOrigin;
    property Rotate: TsgeDisplayElementItemPropertyRotate read FRotate;
  end;


implementation


constructor TsgeDisplayElementItemRect.Create(X1, Y1, X2, Y2: Single; Color: TsgeColor);
begin
  FRect := TsgeDisplayElementItemPropertyFloatRect.Create(X1, Y1, X2, Y2);
  FColor := TsgeDisplayElementItemPropertyColor.Create(Color);
  FScale := TsgeDisplayElementItemPropertyScale.Create;
  FOrigin := TsgeDisplayElementItemPropertyFloatPoint.Create;
  FRotate := TsgeDisplayElementItemPropertyRotate.Create;
end;


destructor TsgeDisplayElementItemRect.Destroy;
begin
  FRotate.Free;
  FOrigin.Free;
  FScale.Free;
  FColor.Free;
  FRect.Free;
end;



end.

