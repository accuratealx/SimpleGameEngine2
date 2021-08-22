{
Пакет             Simple Game Engine 2
Файл              sgeVariableIntegerNormal.pas
Версия            1.0
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Целое число: Значение
}
{$Include Defines.inc}

unit sgeVariableIntegerNormal;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableIntegerBase;


type
  TsgeVariableIntegerNormal = class(TsgeVariableIntegerBase)
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
  _UNITNAME = 'VariableIntegerNormal';



function TsgeVariableIntegerNormal.GetValue: Integer;
begin
  Result := FValue;
end;


procedure TsgeVariableIntegerNormal.SetValue(AValue: Integer);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Поправить значение
  CheckValue(AValue);

  //Сохранить
  FValue := AValue;
end;


constructor TsgeVariableIntegerNormal.Create(Name: ShortString; Value: Integer; DefValue: Integer; ReadOnly: Boolean; MinValue: Integer; MaxValue: Integer);
begin
  inherited Create(Name, DefValue, ReadOnly, MinValue, MaxValue);

  SetValue(Value);
end;




end.

