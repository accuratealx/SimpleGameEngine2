{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandParameterFloat.pas
Версия            1.2
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
  sgeShellCommandParameterBase;


type
  TsgeShellCommandParameterFloat = class(TsgeShellCommandParameterBase)
  public
    constructor Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean = False; PrefixList: String = ''; PrefixSeparator: String = ',');
  end;


implementation


constructor TsgeShellCommandParameterFloat.Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; PrefixSeparator: String);
begin
  inherited Create(scptFloat, Name, Required, PrefixRequired, PrefixList, PrefixSeparator);
end;


end.

