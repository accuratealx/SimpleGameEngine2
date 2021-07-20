{
Пакет             Simple Game Engine 2
Файл              sgeVariableIntegerVirtual.pas
Версия            1.0
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Виртуальное целое число
}
{$Include Defines.inc}

unit sgeVariableIntegerVirtual;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableIntegerBase;


type
  //Методы изменения
  TsgeVariableIntegerSetter = procedure(AValue: Integer) of object;
  TsgeVariableIntegerGetter = function: Integer of object;


  TsgeVariableIntegerVirtual = class(TsgeVariableIntegerBase)
  private
    FSetter: TsgeVariableIntegerSetter;
    FGetter: TsgeVariableIntegerGetter;

  protected
    function  GetValue: Integer; override;
    procedure SetValue(AValue: Integer); override;

  public
    constructor Create(Name: ShortString; DefValue: Integer; Getter: TsgeVariableIntegerGetter; Setter: TsgeVariableIntegerSetter = nil; MinValue: Integer = -MaxInt; MaxValue: Integer = MaxInt);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableIntegerVirtual';



function TsgeVariableIntegerVirtual.GetValue: Integer;
begin
  Result := FGetter();
end;


procedure TsgeVariableIntegerVirtual.SetValue(AValue: Integer);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Поправить диапазон
  CheckValue(AValue);

  //Изменить значение
  FSetter(AValue);
end;


constructor TsgeVariableIntegerVirtual.Create(Name: ShortString; DefValue: Integer; Getter: TsgeVariableIntegerGetter; Setter: TsgeVariableIntegerSetter; MinValue: Integer; MaxValue: Integer);
begin
  //Проверить метод чтения
  if Getter = nil then
    raise EsgeException.Create(_UNITNAME, Err_VariableGetterIsEmpty);

  inherited Create(Name, DefValue, False, MinValue, MaxValue);

  //Методы изменения
  FSetter := Setter;
  FGetter := Getter;

  //Поправить флаг только для чтения
  FReadOnly := not Assigned(FSetter);
end;




end.

