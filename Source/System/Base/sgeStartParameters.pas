{
Пакет             Simple Game Engine 2
Файл              sgeStartParameters.pas
Версия            1.1
Создан            06.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Удобные стартовые параметры приложения
}
{$Include Defines.inc}

unit sgeStartParameters;

{$mode objfpc}{$H+}

interface


type
  TsgeStartParameterItem = record
    Name: String;
    Value: String;
  end;


  TsgeStartParameters = class
  private
    FList: array of TsgeStartParameterItem;       //Массив параметров
    FParamSeparator: Char;                        //Разделитель между параметрами
    FValueSeparator: Char;                        //Разделитель между именем параметра и значением
    FParamLine: String;                           //Строка параметров без ParamStr(0)
    FCmdLine: String;                             //Сырая строка параметров от системы

    procedure SetParamSeparator(ASeparator: Char);
    procedure SetValueSeparator(ASeparator: Char);

    procedure Clear;
    procedure Add(Name, Value: String);
    function  GetCount: Integer;
    function  IndexOf(Name: string): Integer;

    function  GetValue(Name: String): String;
    function  GetExist(Name: String): Boolean;
    function  GetParameter(Index: Integer): TsgeStartParameterItem;

    procedure GetNameAndValueFromString(Str: String; var pName, pValue: String);
    procedure FindParametersFromString(Str: String);
    function  GetNormalParamString: String;
  public
    constructor Create;
    destructor  Destroy; override;

    property ParamSeparator: Char read FParamSeparator write SetParamSeparator;
    property ValueSeparator: Char read FValueSeparator write SetValueSeparator;
    property Count: Integer read GetCount;
    property Parameter[Index: Integer]: TsgeStartParameterItem read GetParameter;
    property Value[Name: String]: String read GetValue;
    property Exist[Name: String]: Boolean read GetExist;
    property ParamString: String read FParamLine;
    property CmdLine: String read FCmdLine;
  end;



implementation

uses
  sgeErrors, sgeSystemUtils, sgeOSPlatform, sgeSimpleCommand;


const
  _UNITNAME = 'StartParameters';

  Err_ParameterNotFound = 'ParameterNotFound';
  Err_IndexOutOfBounds  = 'IndexOutOfBounds';


procedure TsgeStartParameters.SetParamSeparator(ASeparator: Char);
begin
  FParamSeparator := ASeparator;
  FindParametersFromString(FParamLine);
end;


procedure TsgeStartParameters.SetValueSeparator(ASeparator: Char);
begin
  FValueSeparator := ASeparator;
  FindParametersFromString(FParamLine);
end;


procedure TsgeStartParameters.Clear;
begin
  SetLength(FList, 0);
end;


procedure TsgeStartParameters.Add(Name, Value: String);
var
  c, Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    begin
    c := GetCount;
    SetLength(FList, c + 1);
    FList[c].Name := Name;
    FList[c].Value := Value;
    end
    else FList[Idx].Value := Value;
end;


function TsgeStartParameters.GetCount: Integer;
begin
  Result := Length(FList);
end;


function TsgeStartParameters.IndexOf(Name: string): Integer;
var
  i, c: Integer;
begin
  Result := -1;

  Name := LowerCase(Name);
  c := GetCount - 1;
  for i := 0 to c do
    if Name = LowerCase(FList[i].Name) then
      begin
      Result := i;
      Break;
      end;
end;


function TsgeStartParameters.GetValue(Name: String): String;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ParameterNotFound, Name);

  Result := FList[Idx].Value;
end;


function TsgeStartParameters.GetExist(Name: String): Boolean;
begin
  Result := (IndexOf(Name) <> -1);
end;


function TsgeStartParameters.GetParameter(Index: Integer): TsgeStartParameterItem;
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FList[Index];
end;


procedure TsgeStartParameters.GetNameAndValueFromString(Str: String; var pName, pValue: String);
var
  IsName: Boolean;
  i, c: Integer;
  Symbol: Char;
begin
  pName := '';
  pValue := '';
  IsName := True;

  //Цикл по символам
  c := Length(Str);
  for i := 1 to c do
    begin
    //Выделить символ
    Symbol := Str[i];

    //Проверить на разделитель
    if (Symbol = FValueSeparator) and IsName then
      begin
      IsName := False;
      Continue;
      end;

    //Добавить символ
    case IsName of
      True : pName := pName + Symbol;
      False: pValue := pValue + Symbol;
    end;
    end;

  //Поправить результат
  pName := sgeTrim(pName);
  pValue := sgeTrim(pValue);
end;


procedure TsgeStartParameters.FindParametersFromString(Str: String);
var
  i, c: Integer;
  List: TsgeSimpleCommand;
  pName, pValue: String;
begin
  //Очистить список
  Clear;

  //Добавить путь к файлу в качестве первого параметра
  Add(ParamStr(0), '');

  //Подготовить список
  List := TsgeSimpleCommand.Create;                       //Создать список
  List.Separators := #32;                                 //Изменить разделитель на пробел
  List.WeakSeparator := True;                             //Мягкий разделитель
  List.Command := Str;                                    //Разбить строку на части через пробелы

  //Цикл по частям
  c := List.Count - 1;
  for i := 0 to c do
    begin
    GetNameAndValueFromString(List.Part[i], pName, pValue); //Вернуть из строки имя и значение
    if pName = '' then Continue;                            //Пропуск, если нет имени
    Add(pName, pValue);                                     //Добавить в массив
    end;

  //Очистить список
  List.Free;
end;


function TsgeStartParameters.GetNormalParamString: String;
var
  i, c: Integer;
  Quote: Boolean;
begin
  Result := FCmdLine;
  if Result = '' then Exit;

  //Определить длину первого параметра
  c := Length(Result);
  Quote := False;
  for i := 1 to c do
    begin
    if Result[i] = '"' then Quote := not Quote;
    if (Result[i] = ' ') and (not Quote) then Break;
    end;

  //Отрезать первый параметр
  Delete(Result, 1, i);

  //Вернуть результат
  Result := sgeTrim(Result);
end;


constructor TsgeStartParameters.Create;
begin
  //Задать способ обработки
  FParamSeparator := ' ';
  FValueSeparator := '=';

  //Запоминть сырую строку параметров
  FCmdLine := sgeGetCommandLine;

  //Запомнить строку параметров
  FParamLine := GetNormalParamString;

  //Найти параметры в строке
  FindParametersFromString(FParamLine);
end;


destructor TsgeStartParameters.Destroy;
begin
  Clear;
end;




end.

