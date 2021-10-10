{
Пакет             Simple Game Engine 2
Файл              sgeGUIUtils.pas
Версия            1.0
Создан            10.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Вспомогательные функции
}
{$Include Defines.inc}

unit sgeGUIUtils;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters;


procedure sgeGUISetValue(Parameters: TsgeSimpleParameters; ParamName: String; var Value: Integer; DefaultValue: Integer = 0);
procedure sgeGUISetValue(Parameters: TsgeSimpleParameters; ParamName: String; var Value: Word; DefaultValue: Word = 0);
procedure sgeGUISetValue(Parameters: TsgeSimpleParameters; ParamName: String; var Value: Single; DefaultValue: Single = 0.0);
procedure sgeGUISetValue(Parameters: TsgeSimpleParameters; ParamName: String; var Value: Boolean; DefaultValue: Boolean = False);


implementation


procedure sgeGUISetValue(Parameters: TsgeSimpleParameters; ParamName: String; var Value: Integer; DefaultValue: Integer);
begin
  if Parameters.Exist[ParamName] then Value := Parameters.GetValue(ParamName, DefaultValue);
end;


procedure sgeGUISetValue(Parameters: TsgeSimpleParameters; ParamName: String; var Value: Word; DefaultValue: Word);
begin
  if Parameters.Exist[ParamName] then Value := Parameters.GetValue(ParamName, DefaultValue);
end;

procedure sgeGUISetValue(Parameters: TsgeSimpleParameters; ParamName: String; var Value: Single; DefaultValue: Single);
begin
  if Parameters.Exist[ParamName] then Value := Parameters.GetValue(ParamName, DefaultValue);
end;


procedure sgeGUISetValue(Parameters: TsgeSimpleParameters; ParamName: String; var Value: Boolean; DefaultValue: Boolean);
begin
  if Parameters.Exist[ParamName] then Value := Parameters.GetValue(ParamName, DefaultValue);
end;



end.

