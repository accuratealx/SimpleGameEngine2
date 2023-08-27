{
Пакет             Simple Game Engine 2
Файл              sgeVariableColorNormal.pas
Версия            1.1
Создан            22.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Цвет: Значение
}
{$Include Defines.inc}

unit sgeVariableColorNormal;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeVariableColorBase, sgeColor;


type
  TsgeVariableColorNormal = class(TsgeVariableColorBase)
  private
    FValue: TsgeRGBA;

  protected
    function  GetValue: TsgeRGBA; override;
    procedure SetValue(AValue: TsgeRGBA); override;

  public
    constructor Create(Name: ShortString; Value: TsgeRGBA; DefValue: TsgeRGBA; ReadOnly: Boolean);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableColorNormal';



function TsgeVariableColorNormal.GetValue: TsgeRGBA;
begin
  Result := FValue;
end;


procedure TsgeVariableColorNormal.SetValue(AValue: TsgeRGBA);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FValue := AValue;
end;


constructor TsgeVariableColorNormal.Create(Name: ShortString; Value: TsgeRGBA; DefValue: TsgeRGBA; ReadOnly: Boolean);
begin
  inherited Create(Name, DefValue, ReadOnly, True);

  SetValue(Value);
end;




end.

