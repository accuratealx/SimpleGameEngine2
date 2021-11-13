{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandParameterInteger.pas
Версия            1.2
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
  sgeShellCommandParameterBase;


type
  TsgeShellCommandParameterInteger = class(TsgeShellCommandParameterBase)
  public
    constructor Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean = False; PrefixList: String = ''; PrefixSeparator: String = ',');
  end;


implementation


constructor TsgeShellCommandParameterInteger.Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; PrefixSeparator: String);
begin
  inherited Create(scptInteger, Name, Required, PrefixRequired, PrefixList, PrefixSeparator);
end;


end.

