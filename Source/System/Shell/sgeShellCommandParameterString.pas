{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandParameterString.pas
Версия            1.0
Создан            08.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс параметра оболочки: Строка
}
{$Include Defines.inc}

unit sgeShellCommandParameterString;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeStringList, sgeShellCommandParameterBase;


type
  TsgeShellCommandParameterString = class(TsgeShellCommandParameterBase)
  private
  public
    constructor Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean = False; PrefixList: TsgeStringList = nil);
  end;


implementation


constructor TsgeShellCommandParameterString.Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: TsgeStringList);
begin
  inherited Create(scptString, Name, Required, PrefixRequired, PrefixList);
end;


end.

