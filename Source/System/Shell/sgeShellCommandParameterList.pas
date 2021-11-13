{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandParameterList.pas
Версия            1.2
Создан            08.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс список параметров команды оболочки
}
{$Include Defines.inc}

unit sgeShellCommandParameterList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection, sgeShellCommandParameterBase,
  sgeShellCommandParameterInteger, sgeShellCommandParameterFloat, sgeShellCommandParameterString,
  sgeShellCommandParameterEnum;


type
  TsgeShellCommandParameterList = class(specialize TsgeTemplateCollection<TsgeShellCommandParameterBase>)
  public
    procedure AddInteger(Name: ShortString; Required: Boolean = True; PrefixRequired: Boolean = False; PrefixList: String = ''; PrefixSeparator: String = ',');
    procedure AddFloat(Name: ShortString; Required: Boolean = True; PrefixRequired: Boolean = False; PrefixList: String = ''; PrefixSeparator: String = ',');
    procedure AddString(Name: ShortString; Required: Boolean = True; PrefixRequired: Boolean = False; PrefixList: String = ''; PrefixSeparator: String = ',');
    procedure AddEnum(Name: ShortString; Values: String; Separator: String; Required: Boolean; PrefixRequired: Boolean = False; PrefixList: String = ''; PrefixSeparator: String = ',');
  end;



implementation


procedure TsgeShellCommandParameterList.AddInteger(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; PrefixSeparator: String);
begin
  Add(TsgeShellCommandParameterInteger.Create(Name, Required, PrefixRequired, PrefixList, PrefixSeparator));
end;


procedure TsgeShellCommandParameterList.AddFloat(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; PrefixSeparator: String);
begin
  Add(TsgeShellCommandParameterFloat.Create(Name, Required, PrefixRequired, PrefixList, PrefixSeparator));
end;


procedure TsgeShellCommandParameterList.AddString(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; PrefixSeparator: String);
begin
  Add(TsgeShellCommandParameterString.Create(Name, Required, PrefixRequired, PrefixList, PrefixSeparator));
end;


procedure TsgeShellCommandParameterList.AddEnum(Name: ShortString; Values: String; Separator: String; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; PrefixSeparator: String);
begin
  Add(TsgeShellCommandParameterEnum.Create(Name, Values, Separator, Required, PrefixRequired, PrefixList, PrefixSeparator));
end;


end.

