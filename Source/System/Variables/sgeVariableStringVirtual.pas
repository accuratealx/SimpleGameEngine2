{
Пакет             Simple Game Engine 2
Файл              sgeVariableStringVirtual.pas
Версия            1.0
Создан            20.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Виртуальная строка
}
{$Include Defines.inc}

unit sgeVariableStringVirtual;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableStringBase;


type
  //Методы изменения
  TsgeVariableStringSetter = procedure(AValue: String) of object;
  TsgeVariableStringGetter = function: String of object;


  TsgeVariableStringVirtual = class(TsgeVariableStringBase)
  private
    FSetter: TsgeVariableStringSetter;
    FGetter: TsgeVariableStringGetter;

  protected
    function  GetValue: String; override;
    procedure SetValue(AValue: String); override;

  public
    constructor Create(Name: ShortString; DefValue: String; Getter: TsgeVariableStringGetter; Setter: TsgeVariableStringSetter = nil);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableStringVirtual';



function TsgeVariableStringVirtual.GetValue: String;
begin
  Result := FGetter();
end;


procedure TsgeVariableStringVirtual.SetValue(AValue: String);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FSetter(AValue);
end;


constructor TsgeVariableStringVirtual.Create(Name: ShortString; DefValue: String; Getter: TsgeVariableStringGetter; Setter: TsgeVariableStringSetter);
begin
  //Проверить метод чтения
  if Getter = nil then
    raise EsgeException.Create(_UNITNAME, Err_VariableGetterIsEmpty);

  inherited Create(Name, DefValue, False);

  //Методы изменения
  FSetter := Setter;
  FGetter := Getter;

  //Поправить флаг только для чтения
  FReadOnly := not Assigned(FSetter);
end;




end.

