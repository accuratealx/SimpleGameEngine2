{
Пакет             Simple Game Engine 2
Файл              sgeVariableColorBase.pas
Версия            1.0
Создан            22.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Цвет: Базовый
}
{$Include Defines.inc}

unit sgeVariableColorBase;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableBase, sgeGraphicColor;


type
  TsgeVariableColorBase = class(TsgeVariableBase)
  protected
    FDefaultValue: TsgeRGBA;

    procedure SetStrValue(Str: String); override;
    function  GetStrValue: String; override;

    procedure SetValue(AValue: TsgeRGBA); virtual; abstract;
    function  GetValue: TsgeRGBA; virtual; abstract;
  public
    constructor Create(Name: ShortString; DefValue: TsgeRGBA; ReadOnly: Boolean = False);

    procedure SetDefaultValue; override;

    property Value: TsgeRGBA read GetValue write SetValue;
  end;


implementation

uses
  sgeGraphicColorUtils;


procedure TsgeVariableColorBase.SetStrValue(Str: String);
begin
  Value := sgeStringToRGBA(Str, '.');
end;


function TsgeVariableColorBase.GetStrValue: String;
begin
  Result := sgeRGBAToString(Value, '.');
end;


constructor TsgeVariableColorBase.Create(Name: ShortString; DefValue: TsgeRGBA; ReadOnly: Boolean);
begin
  inherited Create(Name, ReadOnly);

  //Тип переменной
  FValueType := vtString;

  //Границы
  FDefaultValue := DefValue;
end;


procedure TsgeVariableColorBase.SetDefaultValue;
begin
  SetValue(FDefaultValue);
end;


end.

