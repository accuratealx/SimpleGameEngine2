{
Пакет             Simple Game Engine 2
Файл              sgeVariableBooleanBase.pas
Версия            1.1
Создан            22.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Булевая переменная: Базовый
}
{$Include Defines.inc}

unit sgeVariableBooleanBase;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeVariableBase;


type
  TsgeVariableBooleanBase = class(TsgeVariableBase)
  protected
    FDefaultValue: Boolean;
    FTrueStr: ShortString;
    FFalseStr: ShortString;

    procedure SetStrValue(Str: String); override;
    function  GetStrValue: String; override;

    procedure SetValue(AValue: Boolean); virtual; abstract;
    function  GetValue: Boolean; virtual; abstract;
  public
    constructor Create(Name: ShortString; DefValue: Boolean; ReadOnly: Boolean; Imbedded: Boolean; TrueStr: ShortString = 'True'; FalseStr: ShortString = 'False');

    procedure SetDefaultValue; override;

    property Value: Boolean read GetValue write SetValue;
    property TrueStr: ShortString read FTrueStr;
    property FalseStr: ShortString read FFalseStr;
  end;


implementation


procedure TsgeVariableBooleanBase.SetStrValue(Str: String);
var
  v: Boolean;
  s: ShortString;
begin
  v := FDefaultValue;
  s := LowerCase(Str);
  if s = LowerCase(FTrueStr) then v := True;
  if s = LowerCase(FFalseStr) then v := False;
  SetValue(v);
end;


function TsgeVariableBooleanBase.GetStrValue: String;
begin
  case Value of
    True: Result := FTrueStr;
    False: Result := FFalseStr;
  end;
end;


constructor TsgeVariableBooleanBase.Create(Name: ShortString; DefValue: Boolean; ReadOnly: Boolean; Imbedded: Boolean; TrueStr: ShortString; FalseStr: ShortString);
begin
  inherited Create(Name, ReadOnly, Imbedded);

  //Тип переменной
  FValueType := vtBoolean;

  //Параметры
  FTrueStr := TrueStr;
  FFalseStr := FalseStr;

  //Границы
  FDefaultValue := DefValue;
end;


procedure TsgeVariableBooleanBase.SetDefaultValue;
begin
  SetValue(FDefaultValue);
end;


end.

