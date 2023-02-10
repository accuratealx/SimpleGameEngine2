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
  sgeTypes,
  sgeDisplayElementItemSimple;

type
  TsgeDisplayElementItemFrame = class(TsgeDisplayElementSimple)
  protected
    FLineWidth: Single;             //Толщина линии
    FLineStipple: TsgeLineStipple;  //Тип штриховки
    FStippleScale: Word;            //Масштаб штриховки

    procedure SetDefaultParameter; override;
  public
    property LineWidth: Single read FLineWidth write FLineWidth;
    property LineStipple: TsgeLineStipple read FLineStipple write FLineStipple;
    property StippleScale: Word read FStippleScale write FStippleScale;
  end;


implementation


procedure TsgeDisplayElementItemFrame.SetDefaultParameter;
begin
  inherited SetDefaultParameter;

  //Параметры линии
  FLineWidth := 1;
  FLineStipple := lsSolid;
  FStippleScale := 1;
end;



end.

