{
Пакет             Simple Game Engine 2
Файл              sgeVariableInteger.pas
Версия            1.0
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Целое число
}
{$Include Defines.inc}

unit sgeVariableInteger;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableIntegerBase;


type
  TsgeVariableInteger = class(TsgeVariableIntegerBase)
  private
    FValue: Integer;

  protected
    function  GetValue: Integer; override;
    procedure SetValue(AValue: Integer); override;

  public
    constructor Create(Name: ShortString; Value: Integer; DefValue: Integer; ReadOnly: Boolean; MinValue: Integer = -MaxInt; MaxValue: Integer = MaxInt);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableInteger';



function TsgeVariableInteger.GetValue: Integer;
begin
  Result := FValue;
end;


procedure TsgeVariableInteger.SetValue(AValue: Integer);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Поправить значение
  CheckValue(AValue);

  //Сохранить
  FValue := AValue;
end;


constructor TsgeVariableInteger.Create(Name: ShortString; Value: Integer; DefValue: Integer; ReadOnly: Boolean; MinValue: Integer; MaxValue: Integer);
begin
  inherited Create(Name, DefValue, ReadOnly, MinValue, MaxValue);

  SetValue(Value);
end;




end.

