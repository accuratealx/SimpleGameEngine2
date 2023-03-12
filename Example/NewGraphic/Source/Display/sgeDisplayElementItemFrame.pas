{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemFrame.pas
Версия            1.0
Создан            11.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Цветная рамка
}
{$Include Defines.inc}

unit sgeDisplayElementItemFrame;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeGraphicColor,
  sgeDisplayElementItemBase, sgeDisplayElementItemPropertyFloatRect, sgeDisplayElementItemPropertyColor,
  sgeDisplayElementItemPropertyScale, sgeDisplayElementItemPropertyRotate, sgeDisplayElementItemPropertyFloatPoint,
  sgeDisplayElementItemPropertyLine;

type
  TsgeDisplayElementItemFrame = class(TsgeDisplayElementItemBase)
  protected
    FRect: TsgeDisplayElementItemPropertyFloatRect;
    FColor: TsgeDisplayElementItemPropertyColor;
    FScale: TsgeDisplayElementItemPropertyScale;
    FOrigin: TsgeDisplayElementItemPropertyFloatPoint;
    FRotate: TsgeDisplayElementItemPropertyRotate;
    FLine: TsgeDisplayElementItemPropertyLine;

  public
    constructor Create(X1, Y1, X2, Y2: Single; Color: TsgeColor; LineWidth: Single = 1);
    destructor  Destroy; override;

    property Rect: TsgeDisplayElementItemPropertyFloatRect read FRect;
    property Color: TsgeDisplayElementItemPropertyColor read FColor;
    property Scale: TsgeDisplayElementItemPropertyScale read FScale;
    property Origin: TsgeDisplayElementItemPropertyFloatPoint read FOrigin;
    property Rotate: TsgeDisplayElementItemPropertyRotate read FRotate;
    property Line: TsgeDisplayElementItemPropertyLine read FLine;
  end;


implementation


constructor TsgeDisplayElementItemFrame.Create(X1, Y1, X2, Y2: Single; Color: TsgeColor; LineWidth: Single = 1);
begin
  FRect := TsgeDisplayElementItemPropertyFloatRect.Create(X1, Y1, X2, Y2);
  FColor := TsgeDisplayElementItemPropertyColor.Create(Color);
  FScale := TsgeDisplayElementItemPropertyScale.Create;
  FOrigin := TsgeDisplayElementItemPropertyFloatPoint.Create;
  FRotate := TsgeDisplayElementItemPropertyRotate.Create;
  FLine := TsgeDisplayElementItemPropertyLine.Create(LineWidth);
end;


destructor TsgeDisplayElementItemFrame.Destroy;
begin
  FLine.Free;
  FRotate.Free;
  FOrigin.Free;
  FScale.Free;
  FColor.Free;
  FRect.Free;
end;



end.

