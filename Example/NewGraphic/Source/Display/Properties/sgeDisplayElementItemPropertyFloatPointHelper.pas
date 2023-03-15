{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemPropertyFloatPointHelper.pas
Версия            1.0
Создан            12.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Хэлпер свойства элемента отображения: Плавающая точка
}
{$Include Defines.inc}

unit sgeDisplayElementItemPropertyFloatPointHelper;

{$mode ObjFPC}{$H+}

interface

uses
  sgeDisplayElementItemPropertyFloatPoint;

type
  TsgeDisplayElementItemPropertyFloatPointHelper = class helper for TsgeDisplayElementItemPropertyFloatPoint
    procedure XToMiddle(Width: Single);
    procedure YToMiddle(Height: Single);
    procedure ToCenter(Width, Height: Single);
  end;


implementation


procedure TsgeDisplayElementItemPropertyFloatPointHelper.XToMiddle(Width: Single);
begin
  Self.X := (Width / 2);
end;


procedure TsgeDisplayElementItemPropertyFloatPointHelper.YToMiddle(Height: Single);
begin
  Self.Y := (Height / 2);
end;


procedure TsgeDisplayElementItemPropertyFloatPointHelper.ToCenter(Width, Height: Single);
begin
  XToMiddle(Width);
  YToMiddle(Height);
end;



end.

