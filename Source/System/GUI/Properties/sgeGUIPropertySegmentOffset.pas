{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertySegmentOffset.pas
Версия            1.0
Создан            23.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Смещение для сегментного вывода спрайта
}
{$Include Defines.inc}

unit sgeGUIPropertySegmentOffset;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeGUIProperty;


type
  TsgeGUIPropertySegmentOffset = class(TsgeGUIProperty)
  private
    FTop: Word;
    FLeft: Word;
    FRight: Word;
    FBottom: Word;

    function  GetRect: TsgeIntRect;

    procedure SetTop(ATop: Word);
    procedure SetLeft(ALeft: Word);
    procedure SetRight(ARight: Word);
    procedure SetBottom(ABottom: Word);
  public
    property Top: Word read FTop write SetTop;
    property Left: Word read FLeft write SetLeft;
    property Right: Word read FRight write SetRight;
    property Bottom: Word read FBottom write SetBottom;

    property Rect: TsgeIntRect read GetRect;
  end;


implementation



function TsgeGUIPropertySegmentOffset.GetRect: TsgeIntRect;
begin
  Result.X1 := FLeft;
  Result.Y1 := FTop;
  Result.X2 := FRight;
  Result.Y2 := FBottom;
end;


procedure TsgeGUIPropertySegmentOffset.SetTop(ATop: Word);
begin
  if FTop = ATop then Exit;

  FTop := ATop;
  ResizeParent;
end;


procedure TsgeGUIPropertySegmentOffset.SetLeft(ALeft: Word);
begin
  if FLeft = ALeft then Exit;

  FLeft := ALeft;
  ResizeParent;
end;


procedure TsgeGUIPropertySegmentOffset.SetRight(ARight: Word);
begin
  if FRight = ARight then Exit;

  FRight := ARight;
  ResizeParent;
end;


procedure TsgeGUIPropertySegmentOffset.SetBottom(ABottom: Word);
begin
  if FBottom = ABottom then Exit;

  FBottom := ABottom;
  ResizeParent;
end;



end.

