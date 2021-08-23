{
Пакет             Simple Game Engine 2
Файл              sgeVariableBooleanProc.pas
Версия            1.0
Создан            23.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Булевая переменная: Ссылка на метод
}
{$Include Defines.inc}

unit sgeVariableBooleanProc;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableBooleanBase;


type
  //Методы изменения
  TsgeVariableBooleanProcSetter = procedure(AValue: Boolean);
  TsgeVariableBooleanProcGetter = function: Boolean;


  TsgeVariableBooleanProc = class(TsgeVariableBooleanBase)
  private
    FSetter: TsgeVariableBooleanProcSetter;
    FGetter: TsgeVariableBooleanProcGetter;

  protected
    function  GetValue: Boolean; override;
    procedure SetValue(AValue: Boolean); override;

  public
    constructor Create(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanProcGetter; Setter: TsgeVariableBooleanProcSetter = nil; TrueStr: ShortString = 'True'; FalseStr: ShortString = 'False');
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableBooleanProc';



function TsgeVariableBooleanProc.GetValue: Boolean;
begin
  Result := FGetter();
end;


procedure TsgeVariableBooleanProc.SetValue(AValue: Boolean);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FSetter(AValue);
end;


constructor TsgeVariableBooleanProc.Create(Name: ShortString; DefValue: Boolean; Getter: TsgeVariableBooleanProcGetter; Setter: TsgeVariableBooleanProcSetter; TrueStr: ShortString; FalseStr: ShortString);
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

