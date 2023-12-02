{
Пакет             Simple Game Engine 2
Файл              sgeVariableStringProc.pas
Версия            1.1
Создан            23.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Строка: Ссылка на метод
}
{$Include Defines.inc}

unit sgeVariableStringProc;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeVariableStringBase;


type
  //Методы изменения
  TsgeVariableStringProcSetter = procedure(AValue: String);
  TsgeVariableStringProcGetter = function: String;


  TsgeVariableStringProc = class(TsgeVariableStringBase)
  private
    FSetter: TsgeVariableStringProcSetter;
    FGetter: TsgeVariableStringProcGetter;

  protected
    function  GetValue: String; override;
    procedure SetValue(AValue: String); override;

  public
    constructor Create(Name: ShortString; DefValue: String; Getter: TsgeVariableStringProcGetter; Setter: TsgeVariableStringProcSetter = nil);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableStringProc';



function TsgeVariableStringProc.GetValue: String;
begin
  Result := FGetter();
end;


procedure TsgeVariableStringProc.SetValue(AValue: String);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FSetter(AValue);
end;


constructor TsgeVariableStringProc.Create(Name: ShortString; DefValue: String; Getter: TsgeVariableStringProcGetter; Setter: TsgeVariableStringProcSetter);
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

