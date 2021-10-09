{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyIntPoint.pas
Версия            1.1
Создан            30.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Целочисленная точка
}
{$Include Defines.inc}

unit sgeGUIPropertyIntPoint;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGUIProperty;

type
  TsgeGUIPropertyIntPoint = class(TsgeGUIProperty)
  private
    FX: Integer;
    FY: Integer;

    procedure SetX(AX: Integer);
    procedure SetY(AY: Integer);
  public
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
  end;


  TsgeGUIPropertyIntPointExt = class(TsgeGUIPropertyIntPoint)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
  end;



implementation



procedure TsgeGUIPropertyIntPoint.SetX(AX: Integer);
begin
  if FX = AX then Exit;

  FX := AX;
  UpdateParent;
end;


procedure TsgeGUIPropertyIntPoint.SetY(AY: Integer);
begin
  if FY = AY then Exit;

  FY := AY;
  UpdateParent;
end;


procedure TsgeGUIPropertyIntPointExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);

  procedure SetValue(ParamName: String; var Value: Integer);
  var
    s: String;
  begin
    s := Prefix + ParamName;
    if Parameters.Exist[s] then Value := Parameters.GetValue(s, 0);
  end;

begin
  //X
  SetValue('X', FX);

  //Y
  SetValue('Y', FY);
end;



end.

