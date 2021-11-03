{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandParameter.pas
Версия            1.1
Создан            08.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс параметра оболочки: Базовый
}
{$Include Defines.inc}

unit sgeShellCommandParameterBase;

{$mode objfpc}{$H+}

interface


uses
  sgeStringList;


type
  //Типы параметров
  TsgeShellCommandParameterType = (scptInteger, scptFloat, scptString, scptBoolean, scptEnum);


  //Параметр
  TsgeShellCommandParameterBase = class
  protected
    FParamType: TsgeShellCommandParameterType;
    FName: ShortString;
  	FRequired: Boolean;
	  FPrefixRequired: Boolean;
  	FPrefixList: TsgeStringList;
  public
    constructor Create(ParamType: TsgeShellCommandParameterType; Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; Separator: String);
    destructor  Destroy; override;

    property ParamType: TsgeShellCommandParameterType read FParamType;
    property Name: ShortString read FName;
    property Required: Boolean read FRequired;
    property PrefixRequired: Boolean read FPrefixRequired;
    property PrefixList: TsgeStringList read FPrefixList;
  end;


implementation


constructor TsgeShellCommandParameterBase.Create(ParamType: TsgeShellCommandParameterType; Name: ShortString; Required: Boolean; PrefixRequired: Boolean; PrefixList: String; Separator: String);
begin
  //Создать список префиксов
  FPrefixList := TsgeStringList.Create;

  //Установить разделитель
  FPrefixList.Separator := Separator;

  //Разобрать на строки
  FPrefixList.FromString(PrefixList);

  //Оnпилить лишнее
  FPrefixList.Trim;

  //Записать параметры
  FParamType := ParamType;
  FName := Name;
  FRequired := Required;
  FPrefixRequired := PrefixRequired;
end;


destructor TsgeShellCommandParameterBase.Destroy;
begin
  //Удалить список префиксов
  FPrefixList.Free;
end;


end.

