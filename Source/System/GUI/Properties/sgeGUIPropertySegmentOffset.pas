{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertySegmentOffset.pas
Версия            1.2
Создан            23.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Смещение для сегментного вывода спрайта
}
{$Include Defines.inc}

unit sgeGUIPropertySegmentOffset;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeSimpleParameters,
  sgeGUIProperty;


type
  TsgeGUIPropertySegmentOffset = class(TsgeGUIProperty)
  private
    FTop: Word;
    FLeft: Word;
    FRight: Word;
    FBottom: Word;

    function  GetRect: TsgeIntRect;
    procedure SetRect(ARect: TsgeIntRect);

    procedure SetTop(ATop: Word);
    procedure SetLeft(ALeft: Word);
    procedure SetRight(ARight: Word);
    procedure SetBottom(ABottom: Word);
  public
    property Top: Word read FTop write SetTop;
    property Left: Word read FLeft write SetLeft;
    property Right: Word read FRight write SetRight;
    property Bottom: Word read FBottom write SetBottom;

    property Rect: TsgeIntRect read GetRect write SetRect;
  end;


  TsgeGUIPropertySegmentOffsetExt = class(TsgeGUIPropertySegmentOffset)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
  end;

implementation



function TsgeGUIPropertySegmentOffset.GetRect: TsgeIntRect;
begin
  Result.X1 := FLeft;
  Result.Y1 := FTop;
  Result.X2 := FRight;
  Result.Y2 := FBottom;
end;


procedure TsgeGUIPropertySegmentOffset.SetRect(ARect: TsgeIntRect);
begin
  FTop := ARect.Y1;
  FLeft := ARect.X1;
  FRight := ARect.X2;
  FBottom := ARect.Y2;
end;


procedure TsgeGUIPropertySegmentOffset.SetTop(ATop: Word);
begin
  if FTop = ATop then Exit;

  FTop := ATop;
  UpdateParent;
end;


procedure TsgeGUIPropertySegmentOffset.SetLeft(ALeft: Word);
begin
  if FLeft = ALeft then Exit;

  FLeft := ALeft;
  UpdateParent;
end;


procedure TsgeGUIPropertySegmentOffset.SetRight(ARight: Word);
begin
  if FRight = ARight then Exit;

  FRight := ARight;
  UpdateParent;
end;


procedure TsgeGUIPropertySegmentOffset.SetBottom(ABottom: Word);
begin
  if FBottom = ABottom then Exit;

  FBottom := ABottom;
  UpdateParent;
end;



procedure TsgeGUIPropertySegmentOffsetExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);

  procedure SetValue(ParamName: String; var Value: Word);
  var
    s: String;
  begin
    s := Prefix + ParamName;
    if Parameters.Exist[s] then Value := Parameters.GetValue(s, 0);
  end;

begin
  //Top
  SetValue('Top', FTop);

  //Left
  SetValue('Left', FLeft);

  //Right
  SetValue('Right', FRight);

  //Bottom
  SetValue('Bottom', FBottom);
end;



end.

