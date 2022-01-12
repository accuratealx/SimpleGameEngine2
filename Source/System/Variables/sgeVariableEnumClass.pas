{
Пакет             Simple Game Engine 2
Файл              sgeVariableEnumClass.pas
Версия            1.1
Создан            31.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Перечисление: Ссылка на метод класса
}
{$Include Defines.inc}

unit sgeVariableEnumClass;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeVariableEnumBase;


type
  //Методы изменения
  TsgeVariableEnumClassSetter = procedure(AValue: String) of object;
  TsgeVariableEnumClassGetter = function: String of object;


  TsgeVariableEnumClass = class(TsgeVariableEnumBase)
  private
    FSetter: TsgeVariableEnumClassSetter;
    FGetter: TsgeVariableEnumClassGetter;

  protected
    function  GetValue: String; override;
    procedure SetValue(AValue: String); override;

  public
    constructor Create(Name: ShortString; Values: String; Separator: String; DefValue: Word; Getter: TsgeVariableEnumClassGetter; Setter: TsgeVariableEnumClassSetter);
  end;


implementation

uses
  sgeErrors, sgeStringUtils,
  sgeVariableBase;

const
  _UNITNAME = 'VariableEnumClass';



function TsgeVariableEnumClass.GetValue: String;
begin
  Result := FGetter();
end;


procedure TsgeVariableEnumClass.SetValue(AValue: String);
var
  Idx: Integer;
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  //Найти индекс значения в списке
  Idx := sgeGetListIndexByValue(FList, AValue);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_UnknownValue, AValue);

  FSetter(FList.Part[Idx]);
end;


constructor TsgeVariableEnumClass.Create(Name: ShortString; Values: String; Separator: String; DefValue: Word; Getter: TsgeVariableEnumClassGetter; Setter: TsgeVariableEnumClassSetter);
begin
  //Проверить метод чтения
  if Getter = nil then
    raise EsgeException.Create(_UNITNAME, Err_VariableGetterIsEmpty);

  inherited Create(Name, Values, Separator, DefValue, False, True);

  //Методы изменения
  FSetter := Setter;
  FGetter := Getter;

  //Поправить флаг только для чтения
  FReadOnly := not Assigned(FSetter);
end;




end.

