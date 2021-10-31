{
Пакет             Simple Game Engine 2
Файл              sgeVariableSingleNormal.pas
Версия            1.1
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Дробное число одинарной точности: Значение
}
{$Include Defines.inc}

unit sgeVariableSingleNormal;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableSingleBase;


type
  TsgeVariableSingleNormal = class(TsgeVariableSingleBase)
  private
    FValue: Single;

  protected
    function  GetValue: Single; override;
    procedure SetValue(AValue: Single); override;

  public
    constructor Create(Name: ShortString; Value: Single; DefValue: Single; ReadOnly: Boolean; MinValue: single = 1.5E-45; MaxValue: single = 3.4E38);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableSingleNormal';



function TsgeVariableSingleNormal.GetValue: Single;
begin
  Result := FValue;
end;


procedure TsgeVariableSingleNormal.SetValue(AValue: Single);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Поправить значение
  CheckValue(AValue);

  //Сохранить
  FValue := AValue;
end;


constructor TsgeVariableSingleNormal.Create(Name: ShortString; Value: Single; DefValue: Single; ReadOnly: Boolean; MinValue: Single; MaxValue: Single);
begin
  inherited Create(Name, DefValue, ReadOnly, False, MinValue, MaxValue);

  SetValue(Value);
end;




end.

