{
Пакет             Simple Game Engine 2
Файл              sgeVariableSingleVirtual.pas
Версия            1.0
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Виртуальное дробное число одинарной точности
}
{$Include Defines.inc}

unit sgeVariableSingleVirtual;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableSingleBase;


type
  //Методы изменения
  TsgeVariableSingleSetter = procedure(AValue: Single) of object;
  TsgeVariableSingleGetter = function: Single of object;


  TsgeVariableSingleVirtual = class(TsgeVariableSingleBase)
  private
    FSetter: TsgeVariableSingleSetter;
    FGetter: TsgeVariableSingleGetter;

  protected
    function  GetValue: Single; override;
    procedure SetValue(AValue: Single); override;

  public
    constructor Create(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleGetter; Setter: TsgeVariableSingleSetter = nil; MinValue: single = 1.5E-45; MaxValue: single = 3.4E38);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableSingleVirtual';



function TsgeVariableSingleVirtual.GetValue: Single;
begin
  Result := FGetter();
end;


procedure TsgeVariableSingleVirtual.SetValue(AValue: Single);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Поправить диапазон
  CheckValue(AValue);

  //Изменить значение
  FSetter(AValue);
end;


constructor TsgeVariableSingleVirtual.Create(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleGetter; Setter: TsgeVariableSingleSetter; MinValue: single; MaxValue: single);
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

