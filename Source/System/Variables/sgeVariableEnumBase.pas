{
Пакет             Simple Game Engine 2
Файл              sgeVariableEnumBase.pas
Версия            1.0
Создан            30.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Перечисление: Базовый
}
{$Include Defines.inc}

unit sgeVariableEnumBase;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeStringList,
  sgeVariableBase;


type
  TsgeVariableEnumBase = class(TsgeVariableBase)
  protected
    FList: TsgeStringList;                                          //Список допустимых значений
    FDefaultValue: Word;                                            //Значение по умолчанию

    procedure SetStrValue(Str: String); override;
    function  GetStrValue: String; override;

    procedure SetValue(AValue: String); virtual; abstract;
    function  GetValue: String; virtual; abstract;
  public
    constructor Create(Name: ShortString; List: TsgeStringList; DefValue: Word; ReadOnly: Boolean; Imbedded: Boolean);
    constructor Create(Name: ShortString; Values: String; Separator: String; DefValue: Word; ReadOnly: Boolean; Imbedded: Boolean);
    destructor  Destroy; override;

    procedure SetDefaultValue; override;

    property Value: String read GetValue write SetValue;

    property List: TsgeStringList read FList;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'VariableEnumBase';

  Err_ZeroEnum                = 'ZeroEnum';
  Err_DefaultIndexOutOfBounds = 'DefaultIndexOutOfBounds';




procedure TsgeVariableEnumBase.SetStrValue(Str: String);
begin
  Value := Str;
end;


function TsgeVariableEnumBase.GetStrValue: String;
begin
  Result := Value;
end;


constructor TsgeVariableEnumBase.Create(Name: ShortString; List: TsgeStringList; DefValue: Word; ReadOnly: Boolean; Imbedded: Boolean);
begin
  //Проверить количество значений
  if List.Count < 1 then
    raise EsgeException.Create(_UNITNAME, Err_ZeroEnum);

  //Проверить значение по умолчанию
  if DefValue > List.Count - 1 then
    raise EsgeException.Create(_UNITNAME, Err_DefaultIndexOutOfBounds, sgeIntToStr(DefValue));

  inherited Create(Name, ReadOnly, Imbedded);

  //Создать список
  FList := TsgeStringList.Create;

  //Подготовить список допустимых значений
  FList.CopyFrom(List);

  //Обрезать лишние пробелы
  FList.Trim;

  //Тип переменной
  FValueType := vtEnum;

  //Запомнить значение по умолчанию
  FDefaultValue := DefValue;
end;


constructor TsgeVariableEnumBase.Create(Name: ShortString; Values: String; Separator: String; DefValue: Word; ReadOnly: Boolean; Imbedded: Boolean);
var
  Lst: TsgeStringList;
begin
  Lst := TsgeStringList.Create;
  try

    Lst.Separator := Separator;
    Lst.FromString(Values);
    Create(Name, Lst, DefValue, ReadOnly, Imbedded);

  finally
    Lst.Free;
  end;
end;


destructor TsgeVariableEnumBase.Destroy;
begin
  FList.Free;
end;


procedure TsgeVariableEnumBase.SetDefaultValue;
begin
  SetValue(FList.Part[FDefaultValue]);
end;


end.

