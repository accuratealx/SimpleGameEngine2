{
Пакет             Simple Game Engine 2
Файл              sgeVariableStringNormal.pas
Версия            1.1
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Строка: Значение
}
{$Include Defines.inc}

unit sgeVariableStringNormal;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableStringBase;


type
  TsgeVariableStringNormal = class(TsgeVariableStringBase)
  private
    FValue: String;

  protected
    function  GetValue: String; override;
    procedure SetValue(AValue: String); override;

  public
    constructor Create(Name: ShortString; Value: String; DefValue: String; ReadOnly: Boolean);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableStringNormal';



function TsgeVariableStringNormal.GetValue: String;
begin
  Result := FValue;
end;


procedure TsgeVariableStringNormal.SetValue(AValue: String);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FValue := AValue;
end;


constructor TsgeVariableStringNormal.Create(Name: ShortString; Value: String; DefValue: String; ReadOnly: Boolean);
begin
  inherited Create(Name, DefValue, ReadOnly, False);

  SetValue(Value);
end;




end.

