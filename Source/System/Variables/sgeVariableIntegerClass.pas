{
Пакет             Simple Game Engine 2
Файл              sgeVariableIntegerClass.pas
Версия            1.0
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Целое число: Ссылка на метод класса
}
{$Include Defines.inc}

unit sgeVariableIntegerClass;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableIntegerBase;


type
  //Методы изменения
  TsgeVariableIntegerClassSetter = procedure(AValue: Integer) of object;
  TsgeVariableIntegerClassGetter = function: Integer of object;


  TsgeVariableIntegerClass = class(TsgeVariableIntegerBase)
  private
    FSetter: TsgeVariableIntegerClassSetter;
    FGetter: TsgeVariableIntegerClassGetter;

  protected
    function  GetValue: Integer; override;
    procedure SetValue(AValue: Integer); override;

  public
    constructor Create(Name: ShortString; DefValue: Integer; Getter: TsgeVariableIntegerClassGetter; Setter: TsgeVariableIntegerClassSetter = nil; MinValue: Integer = -MaxInt; MaxValue: Integer = MaxInt);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableIntegerClass';



function TsgeVariableIntegerClass.GetValue: Integer;
begin
  Result := FGetter();
end;


procedure TsgeVariableIntegerClass.SetValue(AValue: Integer);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Поправить диапазон
  CheckValue(AValue);

  //Изменить значение
  FSetter(AValue);
end;


constructor TsgeVariableIntegerClass.Create(Name: ShortString; DefValue: Integer; Getter: TsgeVariableIntegerClassGetter; Setter: TsgeVariableIntegerClassSetter; MinValue: Integer; MaxValue: Integer);
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

