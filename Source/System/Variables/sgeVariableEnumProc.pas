{
Пакет             Simple Game Engine 2
Файл              sgeVariableEnumProc.pas
Версия            1.1
Создан            31.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Перечисление: Ссылка на метод
}
{$Include Defines.inc}

unit sgeVariableEnumProc;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeVariableEnumBase;


type
  //Методы изменения
  TsgeVariableEnumProcSetter = procedure(AValue: String);
  TsgeVariableEnumProcGetter = function: String;


  TsgeVariableEnumProc = class(TsgeVariableEnumBase)
  private
    FSetter: TsgeVariableEnumProcSetter;
    FGetter: TsgeVariableEnumProcGetter;

  protected
    function  GetValue: String; override;
    procedure SetValue(AValue: String); override;

  public
    constructor Create(Name: ShortString; Values: String; Separator: String; DefValue: Word; Getter: TsgeVariableEnumProcGetter; Setter: TsgeVariableEnumProcSetter);
  end;


implementation

uses
  sgeErrors, sgeStringUtils,
  sgeVariableBase;

const
  _UNITNAME = 'VariableEnumProc';

  Err_UnknownValue = 'UnknownValue';


function TsgeVariableEnumProc.GetValue: String;
begin
  Result := FGetter();
end;


procedure TsgeVariableEnumProc.SetValue(AValue: String);
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


constructor TsgeVariableEnumProc.Create(Name: ShortString; Values: String; Separator: String; DefValue: Word; Getter: TsgeVariableEnumProcGetter; Setter: TsgeVariableEnumProcSetter);
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

