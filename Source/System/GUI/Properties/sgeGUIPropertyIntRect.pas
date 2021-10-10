{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyIntRect.pas
Версия            1.1
Создан            24.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Целочисленный прямоугольник
}
{$Include Defines.inc}

unit sgeGUIPropertyIntRect;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeSimpleParameters,
  sgeGUIProperty;


type
  TsgeGUIPropertyIntRect = class(TsgeGUIProperty)
  private
    FTop: Integer;
    FLeft: Integer;
    FRight: Integer;
    FBottom: Integer;

    function  GetTopLeft: TsgeIntPoint;
    procedure SetTopLeft(ATopLeft: TsgeIntPoint);
    function  GetBottomRight: TsgeIntPoint;
    procedure SetBottomRight(ABottomRight: TsgeIntPoint);
    function  GetRect: TsgeIntRect;
    procedure SetRect(ARect: TsgeIntRect);
    procedure SetTop(ATop: Integer);
    procedure SetLeft(ALeft: Integer);
    procedure SetRight(ARight: Integer);
    procedure SetBottom(ABottom: Integer);
  public
    property Top: Integer read FTop write SetTop;
    property Left: Integer read FLeft write SetLeft;
    property Right: Integer read FRight write SetRight;
    property Bottom: Integer read FBottom write SetBottom;

    property TopLeft: TsgeIntPoint read GetTopLeft write SetTopLeft;
    property BottomRight: TsgeIntPoint read GetBottomRight write SetBottomRight;

    property Rect: TsgeIntRect read GetRect write SetRect;
  end;


  TsgeGUIPropertyIntRectExt = class(TsgeGUIPropertyIntRect)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
  end;


implementation

uses
  sgeGUIUtils;


function TsgeGUIPropertyIntRect.GetTopLeft: TsgeIntPoint;
begin
  Result.X := FLeft;
  Result.Y := FTop;
end;


procedure TsgeGUIPropertyIntRect.SetTopLeft(ATopLeft: TsgeIntPoint);
begin
  FLeft := ATopLeft.X;
  FTop := ATopLeft.Y;

  UpdateParent;
end;


function TsgeGUIPropertyIntRect.GetBottomRight: TsgeIntPoint;
begin
  Result.X := FLeft;
  Result.Y := FTop;
end;


procedure TsgeGUIPropertyIntRect.SetBottomRight(ABottomRight: TsgeIntPoint);
begin
  FRight := ABottomRight.X;
  FBottom := ABottomRight.Y;

  UpdateParent;
end;


function TsgeGUIPropertyIntRect.GetRect: TsgeIntRect;
begin
  Result.X1 := FLeft;
  Result.Y1 := FTop;
  Result.X2 := FRight;
  Result.Y2 := FBottom;
end;


procedure TsgeGUIPropertyIntRect.SetRect(ARect: TsgeIntRect);
begin
  FLeft := ARect.X1;
  FTop := ARect.Y1;
  FRight := ARect.X2;
  FBottom := ARect.Y2;

  UpdateParent;
end;


procedure TsgeGUIPropertyIntRect.SetTop(ATop: Integer);
begin
  FTop := ATop;
  UpdateParent;
end;


procedure TsgeGUIPropertyIntRect.SetLeft(ALeft: Integer);
begin
  FLeft := ALeft;
  UpdateParent;
end;


procedure TsgeGUIPropertyIntRect.SetRight(ARight: Integer);
begin
  FRight := ARight;
  UpdateParent;
end;


procedure TsgeGUIPropertyIntRect.SetBottom(ABottom: Integer);
begin
  FBottom := ABottom;
  UpdateParent;
end;



procedure TsgeGUIPropertyIntRectExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
begin
  //Top
  sgeGUISetValue(Parameters, Prefix + 'Top', FTop);

  //Left
  sgeGUISetValue(Parameters, Prefix + 'Left', FLeft);

  //Right
  sgeGUISetValue(Parameters, Prefix + 'Right', FRight);

  //Bottom
  sgeGUISetValue(Parameters, Prefix + 'Bottom', FBottom);
end;



end.

