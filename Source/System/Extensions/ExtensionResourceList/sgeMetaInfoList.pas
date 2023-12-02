{
Пакет             Simple Game Engine 2
Файл              sgeMetaInfoList.pas
Версия            1.1
Создан            14.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс хранения метаинформации
}
{$Include Defines.inc}

unit sgeMetaInfoList;

{$mode objfpc}{$H+}

interface

type
  //Запись одной метаинформации
  TsgeMetaInfo = record
    Name: String;
    Value: String;
  end;


  //Класс со списком метаинформации
  TsgeMetaInfoList = class
  private
    FMetaInfoList: array of TsgeMetaInfo;
    FSeparator: Char;
    FCount: Integer;

    function  IndexOf(Name: String): Integer;
    procedure Parse(Str: String);

    function  GetItem(Index: Integer): TsgeMetaInfo;
    procedure Add(MetaInfo: TsgeMetaInfo);
  public
    constructor Create(MetaStr: String = ''; Separator: Char = ';');
    destructor  Destroy; override;

    procedure Clear;

    procedure SetValue(Name, Value: String);

    function  GetValue(Name: String; DefValue: String = ''; Del: Boolean = False): String;
    function  GetValue(Name: String; DefValue: Integer = 0; Del: Boolean = False): Integer;
    function  GetValue(Name: String; DefValue: Double = 0; Del: Boolean = False): Double;
    function  GetValue(Name: String; DefValue: Boolean = False; TrueStr: String = 'True'; FalseStr: String = 'False'; Del: Boolean = False): Boolean;

    procedure Delete(Name: String);
    function  Exist(Name: String): Boolean;

    procedure FromString(MetaStr: String);
    function  ToString: String; reintroduce;

    property Count: Integer read FCount;
    property Item[Index: Integer]: TsgeMetaInfo read GetItem;
  end;


implementation


uses
  sgeStringList, sgeSystemUtils;



function TsgeMetaInfoList.IndexOf(Name: String): Integer;
var
  i, c: Integer;
begin
  Result := -1;

  c := Length(FMetaInfoList) - 1;
  Name := LowerCase(Name);
  for i := 0 to c do
    if Name = LowerCase(FMetaInfoList[i].Name) then
      Exit(i);
end;


procedure TsgeMetaInfoList.Parse(Str: String);
var
  List: TsgeStringList;
  i, c, j, k: Integer;
  S, sName, sValue: String;
  IsName: Boolean;
  Symbol: Char;
begin
  //Подготовить и настроить классы
  List := TsgeStringList.Create;
  List.Separator := FSeparator;

  //Разобрать строку на части
  List.FromString(sgeTrim(Str));

  //Обработать строки
  c := List.Count - 1;
  for i := 0 to c do
  begin
    //Отрезать мусор
    S := sgeTrim(List.Part[i]);
    if S = '' then
      Continue;

    //Подготовить переменные
    sName := '';
    sValue := '';
    IsName := True;

    //Цикл по символам
    k := Length(S);
    for j := 1 to k do
    begin
      //Выделить символ
      Symbol := S[j];

      //Проверить на разделитель
      if (Symbol = '=') and IsName then
      begin
        IsName := False;
        Continue;
      end;

      //Добавить символ
      case IsName of
        True : sName := sName + Symbol;
        False: sValue := sValue + Symbol;
      end;

    end;

    //Подготовить имя
    sName := sgeTrim(sName);

    //Пустых имён не может быть
    if sName = '' then
      Continue;

    //Подготовить значение
    sValue := sgeTrim(sValue);


    //Добавить элемент
    SetValue(sName, sValue);
  end;

  //Поправить количество элементов
  FCount := Length(FMetaInfoList);

  List.Free;
end;

function TsgeMetaInfoList.GetItem(Index: Integer): TsgeMetaInfo;
begin
  Result.Name := '';
  Result.Value := '';

  if (Index < 0) or (Index > FCount - 1) then
    Exit;

  Result := FMetaInfoList[Index];
end;


procedure TsgeMetaInfoList.Add(MetaInfo: TsgeMetaInfo);
var
  c, idx: Integer;
begin
  idx := IndexOf(MetaInfo.Name);

  if idx = -1 then
  begin
    c := Length(FMetaInfoList);
    SetLength(FMetaInfoList, c + 1);
    FMetaInfoList[c] := MetaInfo;
    FCount := c + 1;
  end
  else
    FMetaInfoList[idx].Value := MetaInfo.Value;
end;


constructor TsgeMetaInfoList.Create(MetaStr: String; Separator: Char);
begin
  //Определить параметры
  FSeparator := Separator;
  FCount := 0;

  //Разобрать строку на параметры
  Parse(MetaStr);
end;


destructor TsgeMetaInfoList.Destroy;
begin
  Clear;
end;


procedure TsgeMetaInfoList.Clear;
begin
  SetLength(FMetaInfoList, 0);
  FCount := 0;
end;


procedure TsgeMetaInfoList.SetValue(Name, Value: String);
var
  Mi: TsgeMetaInfo;
begin
  Mi.Name := Name;
  Mi.Value := Value;

  Add(Mi);
end;


function TsgeMetaInfoList.GetValue(Name: String; DefValue: String; Del: Boolean): String;
var
  Idx: Integer;
begin
  Result := DefValue;

  Idx := IndexOf(Name);
  if Idx <> -1 then
  begin
    Result := FMetaInfoList[Idx].Value;
    if Del then
      Delete(Name);
  end;
end;


function TsgeMetaInfoList.GetValue(Name: String; DefValue: Integer; Del: Boolean): Integer;
var
  Idx, Val: Integer;
begin
  Result := DefValue;

  Idx := IndexOf(Name);
  if Idx <> -1 then
  begin
    if sgeTryStrToInt(FMetaInfoList[Idx].Value, Val) then
      Result := Val;

    if Del then
      Delete(Name);
  end;
end;


function TsgeMetaInfoList.GetValue(Name: String; DefValue: Double; Del: Boolean): Double;
var
  Idx: Integer;
  Val: Double;
begin
  Result := DefValue;

  Idx := IndexOf(Name);
  if Idx <> -1 then
  begin
    if sgeTryStrToFloat(FMetaInfoList[Idx].Value, Val) then
      Result := Val;

    if Del then
      Delete(Name);
  end;
end;


function TsgeMetaInfoList.GetValue(Name: String; DefValue: Boolean; TrueStr: String; FalseStr: String; Del: Boolean): Boolean;
var
  Idx: Integer;
  Val: String;
begin
  Result := DefValue;

  Idx := IndexOf(Name);
  if Idx <> -1 then
  begin
    //Подготовить параметры
    TrueStr := LowerCase(TrueStr);
    FalseStr := LowerCase(FalseStr);
    Val := LowerCase(FMetaInfoList[Idx].Value);

    //Определить значение
    if Val = TrueStr then
      Result := True;
    if Val = FalseStr then
      Result := False;

    if Del then
      Delete(Name);
  end;
end;


procedure TsgeMetaInfoList.Delete(Name: String);
var
  i, c, Index: Integer;
begin
  //Если нет записи, то ничего не делаем
  Index := IndexOf(Name);
  if Index = -1 then
    Exit;

  //Сдвинуть хвост
  c := Length(FMetaInfoList) - 1;
  for i := Index to c - 1 do
    FMetaInfoList[i] := FMetaInfoList[i + 1];

  SetLength(FMetaInfoList, c);

  //Поправить количество частей
  FCount := c;
end;


function TsgeMetaInfoList.Exist(Name: String): Boolean;
begin
  Result := IndexOf(Name) <> -1;
end;


procedure TsgeMetaInfoList.FromString(MetaStr: String);
begin
  Parse(MetaStr);
end;


function TsgeMetaInfoList.ToString: String;
var
  i, c: Integer;
begin
  Result := '';
  c := FCount - 1;
  for i := 0 to c do
  begin
    Result := Result + FMetaInfoList[i].Name + '=' + FMetaInfoList[i].Value;
    if i <> c then
      Result := Result + FSeparator;
  end;
end;



end.

