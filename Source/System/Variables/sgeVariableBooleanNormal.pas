{
Пакет             Simple Game Engine 2
Файл              sgeVariableBoolean.pas
Версия            1.1
Создан            22.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Булевая переменная: Значение
}
{$Include Defines.inc}

unit sgeVariableBooleanNormal;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableBooleanBase;


type
  TsgeVariableBooleanNormal = class(TsgeVariableBooleanBase)
  private
    FValue: Boolean;

  protected
    function  GetValue: Boolean; override;
    procedure SetValue(AValue: Boolean); override;

  public
    constructor Create(Name: ShortString; Value: Boolean; DefValue: Boolean; ReadOnly: Boolean; TrueStr: ShortString = 'True'; FalseStr: ShortString = 'False');
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableBooleanNormal';



function TsgeVariableBooleanNormal.GetValue: Boolean;
begin
  Result := FValue;
end;


procedure TsgeVariableBooleanNormal.SetValue(AValue: Boolean);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FValue := AValue;
end;


constructor TsgeVariableBooleanNormal.Create(Name: ShortString; Value: Boolean; DefValue: Boolean; ReadOnly: Boolean; TrueStr: ShortString; FalseStr: ShortString);
begin
  inherited Create(Name, DefValue, ReadOnly, False, TrueStr, FalseStr);

  SetValue(Value);
end;




end.

