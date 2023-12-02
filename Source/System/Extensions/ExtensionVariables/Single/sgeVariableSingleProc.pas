{
Пакет             Simple Game Engine 2
Файл              sgeVariableSingleProc.pas
Версия            1.1
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Дробное число одинарной точности: Ссылка на метод
}
{$Include Defines.inc}

unit sgeVariableSingleProc;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeVariableSingleBase;


type
  //Методы изменения
  TsgeVariableSingleProcSetter = procedure(AValue: Single);
  TsgeVariableSingleProcGetter = function: Single;


  TsgeVariableSingleProc = class(TsgeVariableSingleBase)
  private
    FSetter: TsgeVariableSingleProcSetter;
    FGetter: TsgeVariableSingleProcGetter;

  protected
    function  GetValue: Single; override;
    procedure SetValue(AValue: Single); override;

  public
    constructor Create(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleProcGetter; Setter: TsgeVariableSingleProcSetter = nil; MinValue: single = 1.5E-45; MaxValue: single = 3.4E38);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableSingleProc';



function TsgeVariableSingleProc.GetValue: Single;
begin
  Result := FGetter();
end;


procedure TsgeVariableSingleProc.SetValue(AValue: Single);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Поправить диапазон
  CheckValue(AValue);

  //Изменить значение
  FSetter(AValue);
end;


constructor TsgeVariableSingleProc.Create(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleProcGetter; Setter: TsgeVariableSingleProcSetter; MinValue: single; MaxValue: single);
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

