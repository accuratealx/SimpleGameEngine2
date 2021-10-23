{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandParameterFloat.pas
Версия            1.0
Создан            08.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс параметра оболочки: Дробное число
}
{$Include Defines.inc}

unit sgeShellCommandParameterFloat;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeStringList,
  sgeShellCommandParameterBase;


type
  TsgeShellCommandParameterFloat = class(TsgeShellCommandParameterBase)
  private
  public
    constructor Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean = False; PrefixList: TsgeStringList = nil);
  end;


implementation


constructor TsgeShellCommandParameterFloat.Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: TsgeStringList);
begin
  inherited Create(scptFloat, Name, Required, PrefixRequired, PrefixList);
end;


end.

