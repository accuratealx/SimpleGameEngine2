{
Пакет             Simple Game Engine 2
Файл              sgeVariableBooleanClass.pas
Версия            1.0
Создан            22.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Булевая переменная: Ссылка на метод класса
}
{$Include Defines.inc}

unit sgeVariableBooleanClass;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableBooleanBase;


type
  //Методы изменения
  TsgeVariableBooleanClassSetter = procedure(AValue: Boolean) of object;
  TsgeVariableBooleanClassGetter = function: Boolean of object;


  TsgeVariableBooleanClass = class(TsgeVariableBooleanBase)
  private
    FSetter: TsgeVariableBooleanClassSetter;
    FGetter: TsgeVariableBooleanClassGetter;

  protected
    function  GetValue: Boolean; override;
    procedure SetValue(AValue: Boolean); override;

  public
    constructor Create(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanClassGetter; Setter: TsgeVariableBooleanClassSetter = nil; TrueStr: ShortString = 'True'; FalseStr: ShortString = 'False');
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableBooleanClass';



function TsgeVariableBooleanClass.GetValue: Boolean;
begin
  Result := FGetter();
end;


procedure TsgeVariableBooleanClass.SetValue(AValue: Boolean);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FSetter(AValue);
end;


constructor TsgeVariableBooleanClass.Create(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanClassGetter; Setter: TsgeVariableBooleanClassSetter; TrueStr: ShortString; FalseStr: ShortString);
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

