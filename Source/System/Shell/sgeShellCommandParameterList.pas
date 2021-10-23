{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandParameterList.pas
Версия            1.1
Создан            08.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс список параметров команды оболочки
}
{$Include Defines.inc}

unit sgeShellCommandParameterList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection, sgeStringList, sgeShellCommandParameterBase,
  sgeShellCommandParameterInteger, sgeShellCommandParameterFloat, sgeShellCommandParameterString;


type
  TsgeShellCommandParameterList = class(specialize TsgeTemplateCollection<TsgeShellCommandParameterBase>)
  public
    procedure AddInteger(Name: ShortString; Required: Boolean = True; PrefixRequired: Boolean = False; PrefixList: TsgeStringList = nil);
    procedure AddFloat(Name: ShortString; Required: Boolean = True; PrefixRequired: Boolean = False; PrefixList: TsgeStringList = nil);
    procedure AddString(Name: ShortString; Required: Boolean = True; PrefixRequired: Boolean = False; PrefixList: TsgeStringList = nil);
  end;



implementation


procedure TsgeShellCommandParameterList.AddInteger(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: TsgeStringList);
begin
  Add(TsgeShellCommandParameterInteger.Create(Name, Required, PrefixRequired, PrefixList));
end;


procedure TsgeShellCommandParameterList.AddFloat(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: TsgeStringList);
begin
  Add(TsgeShellCommandParameterFloat.Create(Name, Required, PrefixRequired, PrefixList));
end;


procedure TsgeShellCommandParameterList.AddString(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: TsgeStringList);
begin
  Add(TsgeShellCommandParameterString.Create(Name, Required, PrefixRequired, PrefixList));
end;


end.

