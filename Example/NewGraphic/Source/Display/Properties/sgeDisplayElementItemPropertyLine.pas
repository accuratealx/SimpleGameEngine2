{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemPropertyLine.pas
Версия            1.0
Создан            09.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Свойство элемента отображения: Линия
}
{$Include Defines.inc}

unit sgeDisplayElementItemPropertyLine;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes;

type
  TsgeDisplayElementItemPropertyLine = class
  private
    FWidth: Single;
    FStipple: TsgeLineStipple;
    FStippleScale: Word;

  public
    constructor Create(Width: Single = 1; Stipple: TsgeLineStipple = lsWideDash; StippleScale: Word = 1);

    property Width: Single read FWidth write FWidth;
    property Stipple: TsgeLineStipple read FStipple write FStipple;
    property StippleScale: Word read FStippleScale write FStippleScale;
  end;


implementation


constructor TsgeDisplayElementItemPropertyLine.Create(Width: Single; Stipple: TsgeLineStipple; StippleScale: Word);
begin
  FWidth := Width;
  FStipple := Stipple;
  FStippleScale := StippleScale;
end;



end.

