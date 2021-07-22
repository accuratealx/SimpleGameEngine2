{
Пакет             Simple Game Engine 2
Файл              sgeVariableBooleanVirtual.pas
Версия            1.0
Создан            22.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Виртуальная Булевая переменная
}
{$Include Defines.inc}

unit sgeVariableBooleanVirtual;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableBooleanBase;


type
  //Методы изменения
  TsgeVariableBooleanSetter = procedure(AValue: Boolean) of object;
  TsgeVariableBooleanGetter = function: Boolean of object;


  TsgeVariableBooleanVirtual = class(TsgeVariableBooleanBase)
  private
    FSetter: TsgeVariableBooleanSetter;
    FGetter: TsgeVariableBooleanGetter;

  protected
    function  GetValue: Boolean; override;
    procedure SetValue(AValue: Boolean); override;

  public
    constructor Create(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanGetter; Setter: TsgeVariableBooleanSetter = nil; TrueStr: ShortString = 'True'; FalseStr: ShortString = 'False');
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableBooleanVirtual';



function TsgeVariableBooleanVirtual.GetValue: Boolean;
begin
  Result := FGetter();
end;


procedure TsgeVariableBooleanVirtual.SetValue(AValue: Boolean);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FSetter(AValue);
end;


constructor TsgeVariableBooleanVirtual.Create(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanGetter; Setter: TsgeVariableBooleanSetter; TrueStr: ShortString; FalseStr: ShortString);
begin
  //Проверить метод чтения
  if Getter = nil then
    raise EsgeException.Create(_UNITNAME, Err_VariableGetterIsEmpty);

  inherited Create(Name, DefValue, False, TrueStr, FalseStr);

  //Методы изменения
  FSetter := Setter;
  FGetter := Getter;

  //Поправить флаг только для чтения
  FReadOnly := not Assigned(FSetter);
end;




end.

