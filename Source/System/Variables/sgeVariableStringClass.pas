{
Пакет             Simple Game Engine 2
Файл              sgeVariableStringClass.pas
Версия            1.1
Создан            20.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Строка: Ссылка на метод класса
}
{$Include Defines.inc}

unit sgeVariableStringClass;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableStringBase;


type
  //Методы изменения
  TsgeVariableStringClassSetter = procedure(AValue: String) of object;
  TsgeVariableStringClassGetter = function: String of object;


  TsgeVariableStringClass = class(TsgeVariableStringBase)
  private
    FSetter: TsgeVariableStringClassSetter;
    FGetter: TsgeVariableStringClassGetter;

  protected
    function  GetValue: String; override;
    procedure SetValue(AValue: String); override;

  public
    constructor Create(Name: ShortString; DefValue: String; Getter: TsgeVariableStringClassGetter; Setter: TsgeVariableStringClassSetter = nil);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableStringClass';



function TsgeVariableStringClass.GetValue: String;
begin
  Result := FGetter();
end;


procedure TsgeVariableStringClass.SetValue(AValue: String);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FSetter(AValue);
end;


constructor TsgeVariableStringClass.Create(Name: ShortString; DefValue: String; Getter: TsgeVariableStringClassGetter; Setter: TsgeVariableStringClassSetter);
begin
  //Проверить метод чтения
  if Getter = nil then
    raise EsgeException.Create(_UNITNAME, Err_VariableGetterIsEmpty);

  inherited Create(Name, DefValue, False, True);

  //Методы изменения
  FSetter := Setter;
  FGetter := Getter;

  //Поправить флаг только для чтения
  FReadOnly := not Assigned(FSetter);
end;




end.

