{
Пакет             Simple Game Engine 2
Файл              sgeGUIElementPropertyFloatRect.pas
Версия            1.1
Создан            23.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Дробный прямоугольник
}
{$Include Defines.inc}

unit sgeGUIPropertyFloatRect;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeSimpleParameters,
  sgeGUIProperty;


type
  TsgeGUIPropertyFloatRect = class(TsgeGUIProperty)
  private
    FTop: Single;
    FLeft: Single;
    FRight: Single;
    FBottom: Single;

    function  GetTopLeft: TsgeFloatPoint;
    procedure SetTopLeft(ATopLeft: TsgeFloatPoint);
    function  GetBottomRight: TsgeFloatPoint;
    procedure SetBottomRight(ABottomRight: TsgeFloatPoint);
    function  GetRect: TsgeFloatRect;
    procedure SetRect(ARect: TsgeFloatRect);
    procedure SetTop(ATop: Single);
    procedure SetLeft(ALeft: Single);
    procedure SetRight(ARight: Single);
    procedure SetBottom(ABottom: Single);
  public
    property Top: Single read FTop write SetTop;
    property Left: Single read FLeft write SetLeft;
    property Right: Single read FRight write SetRight;
    property Bottom: Single read FBottom write SetBottom;

    property TopLeft: TsgeFloatPoint read GetTopLeft write SetTopLeft;
    property BottomRight: TsgeFloatPoint read GetBottomRight write SetBottomRight;

    property Rect: TsgeFloatRect read GetRect write SetRect;
  end;


  TsgeGUIPropertyFloatRectExt = class(TsgeGUIPropertyFloatRect)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
  end;


implementation



function TsgeGUIPropertyFloatRect.GetTopLeft: TsgeFloatPoint;
begin
  Result.X := FLeft;
  Result.Y := FTop;
end;


procedure TsgeGUIPropertyFloatRect.SetTopLeft(ATopLeft: TsgeFloatPoint);
begin
  FLeft := ATopLeft.X;
  FTop := ATopLeft.Y;

  UpdateParent;
end;


function TsgeGUIPropertyFloatRect.GetBottomRight: TsgeFloatPoint;
begin
  Result.X := FLeft;
  Result.Y := FTop;
end;


procedure TsgeGUIPropertyFloatRect.SetBottomRight(ABottomRight: TsgeFloatPoint);
begin
  FRight := ABottomRight.X;
  FBottom := ABottomRight.Y;

  UpdateParent;
end;


function TsgeGUIPropertyFloatRect.GetRect: TsgeFloatRect;
begin
  Result.X1 := FLeft;
  Result.Y1 := FTop;
  Result.X2 := FRight;
  Result.Y2 := FBottom;
end;


procedure TsgeGUIPropertyFloatRect.SetRect(ARect: TsgeFloatRect);
begin
  FLeft := ARect.X1;
  FTop := ARect.Y1;
  FRight := ARect.X2;
  FBottom := ARect.Y2;

  UpdateParent;
end;


procedure TsgeGUIPropertyFloatRect.SetTop(ATop: Single);
begin
  FTop := ATop;
  UpdateParent;
end;


procedure TsgeGUIPropertyFloatRect.SetLeft(ALeft: Single);
begin
  FLeft := ALeft;
  UpdateParent;
end;


procedure TsgeGUIPropertyFloatRect.SetRight(ARight: Single);
begin
  FRight := ARight;
  UpdateParent;
end;


procedure TsgeGUIPropertyFloatRect.SetBottom(ABottom: Single);
begin
  FBottom := ABottom;
  UpdateParent;
end;


procedure TsgeGUIPropertyFloatRectExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);

  procedure SetValue(ParamName: String; var Value: Single);
  var
    s: String;
  begin
    s := Prefix + ParamName;
    if Parameters.Exist[s] then Value := Parameters.GetValue(s, 0.0);
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

