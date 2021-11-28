{
Пакет             Simple Game Engine 2
Файл              sgeVariableColorClass.pas
Версия            1.1
Создан            22.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Цвет: Ссылка на метод класса
}
{$Include Defines.inc}

unit sgeVariableColorClass;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeVariableColorBase, sgeGraphicColor;


type
  //Методы изменения
  TsgeVariableColorClassSetter = procedure(AValue: TsgeRGBA) of object;
  TsgeVariableColorClassGetter = function: TsgeRGBA of object;


  TsgeVariableColorClass = class(TsgeVariableColorBase)
  private
    FSetter: TsgeVariableColorClassSetter;
    FGetter: TsgeVariableColorClassGetter;

  protected
    function  GetValue: TsgeRGBA; override;
    procedure SetValue(AValue: TsgeRGBA); override;

  public
    constructor Create(Name: ShortString; DefValue: TsgeRGBA; Getter: TsgeVariableColorClassGetter; Setter: TsgeVariableColorClassSetter = nil);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableColorClass';



function TsgeVariableColorClass.GetValue: TsgeRGBA;
begin
  Result := FGetter();
end;


procedure TsgeVariableColorClass.SetValue(AValue: TsgeRGBA);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FSetter(AValue);
end;


constructor TsgeVariableColorClass.Create(Name: ShortString; DefValue: TsgeRGBA; Getter: TsgeVariableColorClassGetter; Setter: TsgeVariableColorClassSetter);
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

