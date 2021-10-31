{
Пакет             Simple Game Engine 2
Файл              sgeVariableStringBase.pas
Версия            1.1
Создан            20.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Cтрока: Базовый
}
{$Include Defines.inc}

unit sgeVariableStringBase;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeVariableBase;


type
  TsgeVariableStringBase = class(TsgeVariableBase)
  protected
    FDefaultValue: String;

    procedure SetStrValue(Str: String); override;
    function  GetStrValue: String; override;

    procedure SetValue(AValue: String); virtual; abstract;
    function  GetValue: String; virtual; abstract;
  public
    constructor Create(Name: ShortString; DefValue: String; ReadOnly: Boolean; Imbedded: Boolean);

    procedure SetDefaultValue; override;

    property Value: String read GetValue write SetValue;
  end;


implementation


procedure TsgeVariableStringBase.SetStrValue(Str: String);
begin
  Value := Str;
end;


function TsgeVariableStringBase.GetStrValue: String;
begin
  Result := Value;
end;


constructor TsgeVariableStringBase.Create(Name: ShortString; DefValue: String; ReadOnly: Boolean; Imbedded: Boolean);
begin
  inherited Create(Name, ReadOnly, Imbedded);

  //Тип переменной
  FValueType := vtString;

  //Границы
  FDefaultValue := DefValue;
end;


procedure TsgeVariableStringBase.SetDefaultValue;
begin
  SetValue(FDefaultValue);
end;


end.

