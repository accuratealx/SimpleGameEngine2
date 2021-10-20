{
Пакет             Simple Game Engine 2
Файл              sgeVariableIntegerBase.pas
Версия            1.1
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Целое число: Базовый
}
{$Include Defines.inc}

unit sgeVariableIntegerBase;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableBase;


type
  TsgeVariableIntegerBase = class(TsgeVariableBase)
  protected
    FDefaultValue: Integer;                                         //Значение по умолчанию
    FMinValue: Integer;                                             //Наименьший порог
	  FMaxValue: Integer;                                             //Наибольший порог

    procedure SetStrValue(Str: String); override;
    function  GetStrValue: String; override;

    procedure SetValue(AValue: Integer); virtual; abstract;
    function  GetValue: Integer; virtual; abstract;

    procedure CheckValue(var AValue: Integer);
  public
    constructor Create(Name: ShortString; DefValue: Integer; ReadOnly: Boolean; Imbedded: Boolean; MinValue: Integer = -MaxInt; MaxValue: Integer = MaxInt);

    procedure SetDefaultValue; override;

    property Value: Integer read GetValue write SetValue;
    property MinValue: Integer read FMinValue write FMinValue;
	  property MaxValue: Integer read FMaxValue write FMaxValue;
  end;


implementation

uses
  sgeSystemUtils;


procedure TsgeVariableIntegerBase.SetStrValue(Str: String);
var
  i: Integer;
begin
  if not sgeTryStrToInt(Str, i) then i := FDefaultValue;
  Value := i;
end;


function TsgeVariableIntegerBase.GetStrValue: String;
begin
  Result := sgeIntToStr(Value);
end;


procedure TsgeVariableIntegerBase.CheckValue(var AValue: Integer);
begin
  if AValue < FMinValue then AValue := FMinValue;
  if AValue > FMaxValue then AValue := FMaxValue;
end;


constructor TsgeVariableIntegerBase.Create(Name: ShortString; DefValue: Integer; ReadOnly: Boolean; Imbedded: Boolean; MinValue: Integer; MaxValue: Integer);
begin
  inherited Create(Name, ReadOnly, Imbedded);

  //Тип переменной
  FValueType := vtInteger;

  //Границы
  FDefaultValue := DefValue;
  FMinValue := MinValue;
  FMaxValue := MaxValue;
end;


procedure TsgeVariableIntegerBase.SetDefaultValue;
begin
  SetValue(FDefaultValue);
end;


end.

