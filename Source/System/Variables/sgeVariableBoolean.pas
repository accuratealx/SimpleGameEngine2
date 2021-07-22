{
Пакет             Simple Game Engine 2
Файл              sgeVariableBoolean.pas
Версия            1.0
Создан            22.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Булевая переменная
}
{$Include Defines.inc}

unit sgeVariableBoolean;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableBooleanBase;


type
  TsgeVariableBoolean = class(TsgeVariableBooleanBase)
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
  _UNITNAME = 'VariableBoolean';



function TsgeVariableBoolean.GetValue: Boolean;
begin
  Result := FValue;
end;


procedure TsgeVariableBoolean.SetValue(AValue: Boolean);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FValue := AValue;
end;


constructor TsgeVariableBoolean.Create(Name: ShortString; Value: Boolean; DefValue: Boolean; ReadOnly: Boolean; TrueStr: ShortString; FalseStr: ShortString);
begin
  inherited Create(Name, DefValue, ReadOnly, TrueStr, FalseStr);

  SetValue(Value);
end;




end.

