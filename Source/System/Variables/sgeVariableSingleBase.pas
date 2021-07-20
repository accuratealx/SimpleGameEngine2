{
Пакет             Simple Game Engine 2
Файл              sgeVariableSingleBase.pas
Версия            1.0
Создан            20.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Базовое дробное число одинарной точности
}
{$Include Defines.inc}

unit sgeVariableSingleBase;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableBase;


type
  TsgeVariableSingleBase = class(TsgeVariableBase)
  protected
    FDefaultValue: Single;                                          //Значение по умолчанию
    FMinValue: Single;                                              //Наименьший порог
	  FMaxValue: Single;                                              //Наибольший порог

    procedure SetStrValue(Str: String); override;
    function  GetStrValue: String; override;

    procedure SetValue(AValue: Single); virtual; abstract;
    function  GetValue: Single; virtual; abstract;

    procedure CheckValue(var AValue: Single);
  public
    constructor Create(Name: ShortString; DefValue: Single; ReadOnly: Boolean = False; MinValue: single = 1.5E-45; MaxValue: single = 3.4E38);

    procedure SetDefaultValue; override;

    property Value: Single read GetValue write SetValue;
    property MinValue: Single read FMinValue write FMinValue;
	  property MaxValue: Single read FMaxValue write FMaxValue;
  end;


implementation

uses
  sgeSystemUtils;


procedure TsgeVariableSingleBase.SetStrValue(Str: String);
var
  i: Double;
begin
  if not sgeTryStrToFloat(Str, i) then i := FDefaultValue;
  Value := i;
end;


function TsgeVariableSingleBase.GetStrValue: String;
begin
  Result := sgeFloatToStr(Value);
end;


procedure TsgeVariableSingleBase.CheckValue(var AValue: Single);
begin
  if AValue < FMinValue then AValue := FMinValue;
  if AValue > FMaxValue then AValue := FMaxValue;
end;


constructor TsgeVariableSingleBase.Create(Name: ShortString; DefValue: Single; ReadOnly: Boolean; MinValue: single; MaxValue: single);
begin
  inherited Create(Name, ReadOnly);

  //Тип переменной
  FValueType := vtSingle;

  //Границы
  FDefaultValue := DefValue;
  FMinValue := MinValue;
  FMaxValue := MaxValue;
end;


procedure TsgeVariableSingleBase.SetDefaultValue;
begin
  SetValue(FDefaultValue);
end;


end.

