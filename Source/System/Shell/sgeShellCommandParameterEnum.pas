{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandParameterInteger.pas
Версия            1.1
Создан            08.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс параметра оболочки: Перечисление
}
{$Include Defines.inc}

unit sgeShellCommandParameterEnum;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeStringList,
  sgeShellCommandParameterBase;


type
  TsgeShellCommandParameterEnum = class(TsgeShellCommandParameterBase)
  private
    FValueList: TsgeStringList;
  public
    constructor Create(Name: ShortString; Values: String; Separator: String; Required: Boolean; PrefixRequired: Boolean = False; PrefixList: String = ''; PrefixSeparator: String = ',');
    destructor  Destroy; override;

    property ValueList: TsgeStringList read FValueList;
  end;


implementation


constructor TsgeShellCommandParameterEnum.Create(Name: ShortString; Values: String; Separator: String; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; PrefixSeparator: String);
var
  s: String;
begin
  inherited Create(scptEnum, Name, Required, PrefixRequired, PrefixList, PrefixSeparator);

  FValueList := TsgeStringList.Create;

  //Разобрать параметры
  s := FValueList.Separator;
  FValueList.Separator := Separator;
  FValueList.FromString(Values);
  FValueList.Trim;
  FValueList.Separator := s;
end;


destructor TsgeShellCommandParameterEnum.Destroy;
begin
  FValueList.Free;

  inherited Destroy;
end;


end.

