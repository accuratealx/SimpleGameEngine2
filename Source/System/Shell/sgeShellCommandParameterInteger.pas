{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandParameterInteger.pas
Версия            1.0
Создан            08.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс параметра оболочки: Целое число
}
{$Include Defines.inc}

unit sgeShellCommandParameterInteger;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeStringList,
  sgeShellCommandParameterBase;


type
  TsgeShellCommandParameterInteger = class(TsgeShellCommandParameterBase)
  public
    constructor Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean = False; PrefixList: TsgeStringList = nil);
  end;


implementation


constructor TsgeShellCommandParameterInteger.Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: TsgeStringList);
begin
  inherited Create(scptInteger, Name, Required, PrefixRequired, PrefixList);
end;


end.

