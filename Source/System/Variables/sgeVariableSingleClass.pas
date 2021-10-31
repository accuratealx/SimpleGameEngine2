{
Пакет             Simple Game Engine 2
Файл              sgeVariableSingleClass.pas
Версия            1.1
Создан            19.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Дробное число одинарной точности: Ссылка на метод класса
}
{$Include Defines.inc}

unit sgeVariableSingleClass;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableSingleBase;


type
  //Методы изменения
  TsgeVariableSingleClassSetter = procedure(AValue: Single) of object;
  TsgeVariableSingleClassGetter = function: Single of object;


  TsgeVariableSingleClass = class(TsgeVariableSingleBase)
  private
    FSetter: TsgeVariableSingleClassSetter;
    FGetter: TsgeVariableSingleClassGetter;

  protected
    function  GetValue: Single; override;
    procedure SetValue(AValue: Single); override;

  public
    constructor Create(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleClassGetter; Setter: TsgeVariableSingleClassSetter = nil; MinValue: single = 1.5E-45; MaxValue: single = 3.4E38);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableSingleClass';



function TsgeVariableSingleClass.GetValue: Single;
begin
  Result := FGetter();
end;


procedure TsgeVariableSingleClass.SetValue(AValue: Single);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Поправить диапазон
  CheckValue(AValue);

  //Изменить значение
  FSetter(AValue);
end;


constructor TsgeVariableSingleClass.Create(Name: ShortString; DefValue: Single; Getter: TsgeVariableSingleClassGetter; Setter: TsgeVariableSingleClassSetter; MinValue: single; MaxValue: single);
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

