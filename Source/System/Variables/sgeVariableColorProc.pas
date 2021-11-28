{
Пакет             Simple Game Engine 2
Файл              sgeVariableColorProc.pas
Версия            1.1
Создан            23.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Цвет: Ссылка на метод
}
{$Include Defines.inc}

unit sgeVariableColorProc;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeVariableColorBase, sgeGraphicColor;


type
  //Методы изменения
  TsgeVariableColorProcSetter = procedure(AValue: TsgeRGBA);
  TsgeVariableColorProcGetter = function: TsgeRGBA;


  TsgeVariableColorProc = class(TsgeVariableColorBase)
  private
    FSetter: TsgeVariableColorProcSetter;
    FGetter: TsgeVariableColorProcGetter;

  protected
    function  GetValue: TsgeRGBA; override;
    procedure SetValue(AValue: TsgeRGBA); override;

  public
    constructor Create(Name: ShortString; DefValue: TsgeRGBA; Getter: TsgeVariableColorProcGetter; Setter: TsgeVariableColorProcSetter = nil);
  end;


implementation

uses
  sgeErrors, sgeVariableBase;

const
  _UNITNAME = 'VariableColorProc';



function TsgeVariableColorProc.GetValue: TsgeRGBA;
begin
  Result := FGetter();
end;


procedure TsgeVariableColorProc.SetValue(AValue: TsgeRGBA);
begin
  if FReadOnly then
    raise EsgeException.Create(_UNITNAME, Err_VariableIsReadOnly);

  FSetter(AValue);
end;


constructor TsgeVariableColorProc.Create(Name: ShortString; DefValue: TsgeRGBA; Getter: TsgeVariableColorProcGetter; Setter: TsgeVariableColorProcSetter);
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

