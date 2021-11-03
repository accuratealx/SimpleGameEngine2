{
Пакет             Simple Game Engine 2
Файл              sgeVariableEnumNormal.pas
Версия            1.1
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Перечисление: Значение
}
{$Include Defines.inc}

unit sgeVariableEnumNormal;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableEnumBase;


type
  TsgeVariableEnumNormal = class(TsgeVariableEnumBase)
  private
    FValue: String;

  protected
    function  GetValue: String; override;
    procedure SetValue(AValue: String); override;

  public
    constructor Create(Name: ShortString; Value: String; Values: String; Separator: String; DefValue: Word; ReadOnly: Boolean);
  end;


implementation

uses
  sgeErrors, sgeStringUtils,
  sgeVariableBase;

const
  _UNITNAME = 'VariableEnumNormal';

  Err_UnknownValue = 'UnknownValue';


function TsgeVariableEnumNormal.GetValue: String;
begin
  Result := FValue;
end;


procedure TsgeVariableEnumNormal.SetValue(AValue: String);
var
  Idx: Integer;
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Найти индекс значения в списке
  Idx := sgeGetListIndexByValue(FList, AValue);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_UnknownValue, AValue);

  FValue := FList.Part[Idx];
end;


constructor TsgeVariableEnumNormal.Create(Name: ShortString; Value: String; Values: String; Separator: String; DefValue: Word; ReadOnly: Boolean);
begin
  inherited Create(Name, Values, Separator, DefValue, ReadOnly, False);

  SetValue(Value);
end;



end.

