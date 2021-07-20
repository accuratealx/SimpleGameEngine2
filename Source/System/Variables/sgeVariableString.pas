{
Пакет             Simple Game Engine 2
Файл              sgeVariableString.pas
Версия            1.0
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Строка
}
{$Include Defines.inc}

unit sgeVariableString;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableStringBase;


type
  TsgeVariableString = class(TsgeVariableStringBase)
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
  _UNITNAME = 'VariableString';



function TsgeVariableString.GetValue: String;
begin
  Result := FValue;
end;


procedure TsgeVariableString.SetValue(AValue: String);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FValue := AValue;
end;


constructor TsgeVariableString.Create(Name: ShortString; Value: String; DefValue: String; ReadOnly: Boolean);
begin
  inherited Create(Name, DefValue, ReadOnly);

  SetValue(Value);
end;




end.

