{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandParameterString.pas
Версия            1.1
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
  sgeShellCommandParameterBase;


type
  TsgeShellCommandParameterString = class(TsgeShellCommandParameterBase)
  private
  public
    constructor Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean = False; PrefixList: String = ''; Separator: String = ',');
  end;


implementation


constructor TsgeShellCommandParameterString.Create(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; Separator: String);
begin
  inherited Create(scptString, Name, Required, PrefixRequired, PrefixList, Separator);
end;


end.

