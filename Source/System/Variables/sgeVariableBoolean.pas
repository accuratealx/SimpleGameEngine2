{
Пакет             Simple Game Engine 2
Файл              sgeVariableBoolean.pas
Версия            1.0
Создан            18.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Булевое значение
}
{$Include Defines.inc}

unit sgeVariableBoolean;

{$mode objfpc}{$H+}

interface

uses
  sgeVariableBase;


type
  TsgeVariableBoolean = class(TsgeVariableBase)
  const
    DefTrueValue = 'True';
    DefFalseValue = 'False';

  private
    FValue: Boolean;
    FDefaultValue: Boolean;
    FTrueValue: ShortString;
    FFalseValue: ShortString;

    function  GetStrValue: String; override;
    procedure SetStrValue(Str: String); override;
  public
    constructor Create(Value: Boolean; DefValue: Boolean; TrueValue: ShortString = 'True'; FalseValue: ShortString = 'False');

    procedure SetDefaultValue; override;

    property Value: Boolean read FValue write FValue;
    property DefaultValue: Boolean read FDefaultValue write FDefaultValue;
  end;


implementation


function TsgeVariableBoolean.GetStrValue: String;
begin

end;


procedure TsgeVariableBoolean.SetStrValue(Str: String);
begin

end;


constructor TsgeVariableBoolean.Create(Value: Boolean; DefValue: Boolean; TrueValue: ShortString; FalseValue: ShortString);
begin
  FValueType := vtBoolean;
end;


procedure TsgeVariableBoolean.SetDefaultValue;
begin
  FValue := FDefaultValue;
end;


end.

