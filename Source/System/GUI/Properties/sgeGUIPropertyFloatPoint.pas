{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyFloatPoint.pas
Версия            1.1
Создан            30.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Дробная точка
}
{$Include Defines.inc}

unit sgeGUIPropertyFloatPoint;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGUIProperty;

type
  TsgeGUIPropertyFloatPoint = class(TsgeGUIProperty)
  private
    FX: Single;
    FY: Single;

    procedure SetX(AX: Single);
    procedure SetY(AY: Single);
  public
    property X: Single read FX write SetX;
    property Y: Single read FY write SetY;
  end;


  TsgeGUIPropertyFloatPointExt = class(TsgeGUIPropertyFloatPoint)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
  end;


implementation



procedure TsgeGUIPropertyFloatPoint.SetX(AX: Single);
begin
  FX := AX;
  UpdateParent;
end;


procedure TsgeGUIPropertyFloatPoint.SetY(AY: Single);
begin
  FY := AY;
  UpdateParent;
end;


procedure TsgeGUIPropertyFloatPointExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);

  procedure SetValue(ParamName: String; var Value: Single);
  var
    s: String;
  begin
    s := Prefix + ParamName;
    if Parameters.Exist[s] then Value := Parameters.GetValue(s, 0.0);
  end;

begin
  //X
  SetValue('X', FX);

  //Y
  SetValue('Y', FY);
end;



end.

