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
  sgeShellCommandParameterInteger, sgeShellCommandParameterFloat, sgeShellCommandParameterString;


type
  TsgeShellCommandParameterList = class(specialize TsgeTemplateCollection<TsgeShellCommandParameterBase>)
  public
    procedure AddInteger(Name: ShortString; Required: Boolean = True; PrefixRequired: Boolean = False; PrefixList: String = ''; Separator: String = ',');
    procedure AddFloat(Name: ShortString; Required: Boolean = True; PrefixRequired: Boolean = False; PrefixList: String = ''; Separator: String = ',');
    procedure AddString(Name: ShortString; Required: Boolean = True; PrefixRequired: Boolean = False; PrefixList: String = ''; Separator: String = ',');
  end;



implementation


procedure TsgeShellCommandParameterList.AddInteger(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; Separator: String);
begin
  Add(TsgeShellCommandParameterInteger.Create(Name, Required, PrefixRequired, PrefixList, Separator));
end;


procedure TsgeShellCommandParameterList.AddFloat(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; Separator: String);
begin
  Add(TsgeShellCommandParameterFloat.Create(Name, Required, PrefixRequired, PrefixList, Separator));
end;


procedure TsgeShellCommandParameterList.AddString(Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; Separator: String);
begin
  Add(TsgeShellCommandParameterString.Create(Name, Required, PrefixRequired, PrefixList, Separator));
end;


end.

