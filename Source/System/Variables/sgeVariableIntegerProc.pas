{
Пакет             Simple Game Engine 2
Файл              sgeVariableIntegerProc.pas
Версия            1.1
Создан            23.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Целое число: Ссылка на метод
}
{$Include Defines.inc}

unit sgeVariableIntegerProc;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableIntegerBase;


type
  //Методы изменения
  TsgeVariableIntegerProcSetter = procedure(AValue: Integer);
  TsgeVariableIntegerProcGetter = function: Integer;


  TsgeVariableIntegerProc = class(TsgeVariableIntegerBase)
  private
    FSetter: TsgeVariableIntegerProcSetter;
    FGetter: TsgeVariableIntegerProcGetter;

  protected
    function  GetValue: Integer; override;
    procedure SetValue(AValue: Integer); override;

  public
    constructor Create(Name: ShortString; DefValue: Integer; Getter: TsgeVariableIntegerProcGetter; Setter: TsgeVariableIntegerProcSetter = nil; MinValue: Integer = -MaxInt; MaxValue: Integer = MaxInt);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableIntegerProc';



function TsgeVariableIntegerProc.GetValue: Integer;
begin
  Result := FGetter();
end;


procedure TsgeVariableIntegerProc.SetValue(AValue: Integer);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Поправить диапазон
  CheckValue(AValue);

  //Изменить значение
  FSetter(AValue);
end;


constructor TsgeVariableIntegerProc.Create(Name: ShortString; DefValue: Integer; Getter: TsgeVariableIntegerProcGetter; Setter: TsgeVariableIntegerProcSetter; MinValue: Integer; MaxValue: Integer);
begin
  //Проверить метод чтения
  if Getter = nil then
    raise EsgeException.Create(_UNITNAME, Err_VariableGetterIsEmpty);

  inherited Create(Name, DefValue, False, True, MinValue, MaxValue);

  //Методы изменения
  FSetter := Setter;
  FGetter := Getter;

  //Поправить флаг только для чтения
  FReadOnly := not Assigned(FSetter);
end;




end.

