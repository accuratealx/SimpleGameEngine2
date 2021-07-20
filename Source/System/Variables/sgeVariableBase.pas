{
Пакет             Simple Game Engine 2
Файл              sgeVariableBase.pas
Версия            1.0
Создан            18.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс переменной: Базовый
}
{$Include Defines.inc}

unit sgeVariableBase;

{$mode objfpc}{$H+}

interface


const
  Err_VariableIsReadOnly    = 'VariableIsReadOnly';
  Err_VariableGetterIsEmpty = 'VariableGetterIsEmpty';


type
  //Тип значения
  TsgeVariableType = (vtUnknown, vtBoolean, vtInteger, vtSingle, vtString, vtStringList);


  //Переменная
  TsgeVariableBase = class
  protected
    FName: ShortString;                                             //Имя переменной
    FReadOnly: Boolean;                                             //Флаг "Только для чтения"
    FValueType: TsgeVariableType;                                   //Тип переменной

    procedure SetStrValue(Str: String); virtual; abstract;          //Метод изменения значения через строку
    function  GetStrValue: String; virtual; abstract;               //Метод получения значения как строку

  public
    constructor Create(Name: ShortString; ReadOnly: Boolean = False); virtual;

    procedure SetDefaultValue; virtual; abstract;                   //Установить значение по умолчанию

    property Name: ShortString read FName;
    property ReadOnly: Boolean read FReadOnly;
    property StrValue: String read GetStrValue write SetStrValue;
    property ValueType: TsgeVariableType read FValueType;
  end;


implementation


constructor TsgeVariableBase.Create(Name: ShortString; ReadOnly: Boolean);
begin
  FName := Name;
  FReadOnly := ReadOnly;
end;


end.

