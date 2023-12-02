{
Пакет             Simple Game Engine 2
Файл              sgeSimpleParameters.pas
Версия            1.0
Создан            06.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс хранения параметров
}
{$Include Defines.inc}

unit sgeSimpleParameters;

{$mode objfpc}{$H+}

interface

uses
  sgeMemoryStream;


type
  //Один параметр
  TsgeSimpleParameter = record
    Name: String;
    Value: String;
  end;


  //Хранилище параметров
  TsgeSimpleParameters = class
  private
    FParamList: array of TsgeSimpleParameter;
    FFileName: String;

    function  GetCount: Integer;
    procedure Add(Name, Value: String);
    procedure Delete(Index: Integer);

    procedure SetParameter(Index: Integer; AParameter: TsgeSimpleParameter);
    function  GetParameter(Index: Integer): TsgeSimpleParameter;
    procedure SetNamedParameter(Name: String; AParameter: TsgeSimpleParameter);
    function  GetNamedParameter(Name: String): TsgeSimpleParameter;
    procedure SetStringValue(Index: Integer; Value: String);
    function  GetStringValue(Index: Integer): String;
    procedure SetNamedValue(Name: String; AValue: String);
    function  GetNamedValue(Name: String): String;
    function  GetExist(Name: String): Boolean;
  public
    constructor Create(FileName: String = '');
    constructor Create(Stream: TsgeMemoryStream);
    destructor  Destroy; override;

    procedure Clear;
    procedure Delete(Name: String);
    function  IndexOf(Name: String): Integer;
    procedure Add(Parameters: TsgeSimpleParameters);

    procedure SetValue(Name: String; Value: String);
    procedure SetValue(Name: String; Value: Integer);
    procedure SetValue(Name: String; Value: Real);
    procedure SetValue(Name: String; Value: Boolean; TrueStr: String = 'True'; FalseStr: String = 'False');
    function  GetValue(Name: String; DefValue: String = ''): String;
    function  GetValue(Name: String; DefValue: Integer = 0): Integer;
    function  GetValue(Name: String; DefValue: Real = 0): Real;
    function  GetValue(Name: String; DefValue: Boolean = False; TrueStr: String = 'True'; FalseStr: String = 'False'): Boolean;

    //Функции с вызовом исключений
    function  GetStringValue(Name: String): String;
    function  GetIntegerValue(Name: String): Integer;
    function  GetRealValue(Name: String): Real;
    function  GetBooleanValue(Name: String; TrueStr: String = 'True'; FalseStr: String = 'False'): Boolean;

    procedure Reload;

    procedure UpdateInString(var Str: String);
    procedure UpdateFromString(Str: String);
    procedure FromString(Str: String);
    function  ToString: String; override;
    procedure FromMemoryStream(Stream: TsgeMemoryStream);
    procedure ToMemoryStream(Stream: TsgeMemoryStream);
    procedure CopyFrom(Parameters: TsgeSimpleParameters);
    procedure CopyTo(Parameters: TsgeSimpleParameters);
    procedure SaveToFile(FileName: String = '');
    procedure LoadFromFile(FileName: String = '');
    procedure UpdateInFile(FileName: String = '');
    procedure UpdateFromFile(FileName: String = '');

    property FileName: String read FFileName write FFileName;
    property Count: Integer read GetCount;
    property Exist[Name: String]: Boolean read GetExist;
    property Parameter[Index: Integer]: TsgeSimpleParameter read GetParameter write SetParameter;
    property NamedParameter[Name: String]: TsgeSimpleParameter read GetNamedParameter write SetNamedParameter;
    property Value[Index: Integer]: String read GetStringValue write SetStringValue;
    property NamedValue[Name: String]: String read GetNamedValue write SetNamedValue;
  end;




implementation

uses
  sgeErrors, sgeSystemUtils, sgeOSPlatform, sgeStringList, sgeFile;


const
  _UNITNAME = 'SimpleParameters';

  LineSeparator = #13#10;
  Separator     = '=';
  Commentary    = '#';

  Err_IndexOutOfBounds        = 'IndexOutOfBounds';
  Err_ParameterNotFound       = 'ParameterNotFound';
  Err_NameNotFound            = 'NameNotFound';
  Err_UnableToDetermineValue  = 'UnableToDetermineValue';
  Err_FileNotFound            = 'FileNotFound';
  Err_CantWriteFile           = 'CantWriteFile';
  Err_CantUpdateFromFile      = 'CantUpdateFromFile';
  Err_CantUpdateInFile        = 'CantUpdateInFile';


function TsgeSimpleParameters.GetCount: Integer;
begin
  Result := Length(FParamList);
end;


procedure TsgeSimpleParameters.Add(Name, Value: String);
var
  c: Integer;
begin
  c := GetCount;
  SetLength(FParamList, c + 1);

  FParamList[c].Name := Name;
  FParamList[c].Value := Value;
end;


procedure TsgeSimpleParameters.Delete(Index: Integer);
var
  c, i: Integer;
begin
  c := GetCount - 1;
  if (Index < 0) or (Index > GetCount) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  for i := Index to c - 1 do
    FParamList[i] := FParamList[i + 1];

  SetLength(FParamList, c);
end;


procedure TsgeSimpleParameters.SetParameter(Index: Integer; AParameter: TsgeSimpleParameter);
begin
if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  FParamList[Index] := AParameter;
end;


function TsgeSimpleParameters.GetParameter(Index: Integer): TsgeSimpleParameter;
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FParamList[Index];
end;


procedure TsgeSimpleParameters.SetNamedParameter(Name: String; AParameter: TsgeSimpleParameter);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ParameterNotFound, Name);

  FParamList[Idx] := AParameter;
end;


function TsgeSimpleParameters.GetNamedParameter(Name: String): TsgeSimpleParameter;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ParameterNotFound, Name);

  Result := FParamList[Idx];
end;


procedure TsgeSimpleParameters.SetStringValue(Index: Integer; Value: String);
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  FParamList[Index].Value := Value;
end;


function TsgeSimpleParameters.GetStringValue(Index: Integer): String;
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FParamList[Index].Value;
end;


procedure TsgeSimpleParameters.SetNamedValue(Name: String; AValue: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ParameterNotFound, Name);

  FParamList[Idx].Value := AValue;
end;


function TsgeSimpleParameters.GetNamedValue(Name: String): String;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ParameterNotFound, Name);

  Result := FParamList[Idx].Value;
end;


function TsgeSimpleParameters.GetExist(Name: String): Boolean;
begin
  Result := (IndexOf(Name) <> -1);
end;


constructor TsgeSimpleParameters.Create(FileName: String);
begin
  FFileName := FileName;
  if sgeFileExists(FFileName) then
    LoadFromFile(FFileName);
end;


constructor TsgeSimpleParameters.Create(Stream: TsgeMemoryStream);
begin
  FromMemoryStream(Stream);
end;


destructor TsgeSimpleParameters.Destroy;
begin
  Clear;
end;


procedure TsgeSimpleParameters.Clear;
begin
  SetLength(FParamList, 0);
end;


procedure TsgeSimpleParameters.Delete(Name: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_NameNotFound, Name);

  Delete(Idx);
end;


function TsgeSimpleParameters.IndexOf(Name: String): Integer;
var
  i, c: Integer;
begin
  Result := -1;

  Name := LowerCase(Name);
  c := GetCount - 1;
  for i := 0 to c do
    if LowerCase(FParamList[i].Name) = Name then
      Exit(i);
end;


procedure TsgeSimpleParameters.Add(Parameters: TsgeSimpleParameters);
var
  i, c: Integer;
begin
  c := Parameters.Count - 1;
  for i := 0 to c do
    Add(Parameters.Parameter[i].Name, Parameters.Parameter[i].Value);
end;


procedure TsgeSimpleParameters.SetValue(Name: String; Value: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    Add(Name, Value)
  else
    FParamList[Idx].Value := Value;
end;


procedure TsgeSimpleParameters.SetValue(Name: String; Value: Integer);
begin
  SetValue(Name, sgeIntToStr(Value));
end;


procedure TsgeSimpleParameters.SetValue(Name: String; Value: Real);
begin
  SetValue(Name, sgeFloatToStr(Value));
end;


procedure TsgeSimpleParameters.SetValue(Name: String; Value: Boolean; TrueStr: String; FalseStr: String);
var
  s: String;
begin
  if Value then
    s := TrueStr
  else
    s := FalseStr;

  SetValue(Name, s);
end;


function TsgeSimpleParameters.GetValue(Name: String; DefValue: String): String;
var
  Idx: Integer;
begin
  Result := DefValue;

  Idx := IndexOf(Name);
  if Idx <> -1 then
    Result := FParamList[Idx].Value;
end;


function TsgeSimpleParameters.GetValue(Name: String; DefValue: Integer): Integer;
var
  Idx, Val: Integer;
begin
  Result := DefValue;

  Idx := IndexOf(Name);
  if Idx <> -1 then
    if sgeTryStrToInt(FParamList[Idx].Value, Val) then
      Result := Val;
end;


function TsgeSimpleParameters.GetValue(Name: String; DefValue: Real): Real;
var
  Idx: Integer;
  Val: Real;
begin
  Result := DefValue;

  Idx := IndexOf(Name);
  if Idx <> -1 then
    if sgeTryStrToFloat(FParamList[Idx].Value, Val) then
      Result := Val;
end;


function TsgeSimpleParameters.GetValue(Name: String; DefValue: Boolean; TrueStr: String; FalseStr: String): Boolean;
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
    Val := LowerCase(FParamList[Idx].Value);

    //Определить значение
    if Val = TrueStr then
      Result := True;
    if Val = FalseStr then
      Result := False;
  end;
end;


function TsgeSimpleParameters.GetStringValue(Name: String): String;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_NameNotFound, Name);

  Result := FParamList[Idx].Value;
end;


function TsgeSimpleParameters.GetIntegerValue(Name: String): Integer;
var
  Idx: Integer;
  i: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_NameNotFound, Name);

  if not sgeTryStrToInt(FParamList[Idx].Value, i) then
    raise EsgeException.Create(_UNITNAME, Err_UnableToDetermineValue, FParamList[Idx].Value);

  Result := i;
end;


function TsgeSimpleParameters.GetRealValue(Name: String): Real;
var
  Idx: Integer;
  R: Real;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_NameNotFound, Name);

  if not sgeTryStrToFloat(FParamList[Idx].Value, R) then
    raise EsgeException.Create(_UNITNAME, Err_UnableToDetermineValue, FParamList[Idx].Value);

  Result := R;
end;


function TsgeSimpleParameters.GetBooleanValue(Name: String; TrueStr: String; FalseStr: String): Boolean;
var
  Idx: Integer;
  cName: String;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_NameNotFound, Name);

  cName := LowerCase(FParamList[Idx].Value);

  //True
  if cName = LowerCase(TrueStr) then
  begin
    Result := True;
    Exit;
  end;

  //Flase
  if cName = LowerCase(FalseStr) then
  begin
    Result := False;
    Exit;
  end;

  //Неизвестное значение
  raise EsgeException.Create(_UNITNAME, Err_UnableToDetermineValue, FParamList[Idx].Value);
end;


procedure TsgeSimpleParameters.Reload;
begin
  LoadFromFile(FFileName);
end;


procedure TsgeSimpleParameters.UpdateInString(var Str: String);
var
  List: TsgeStringList;
  i, c, j, k, l, m: Integer;
  Param1, Param2, S, Prm: String;
  Found: Boolean;
begin
  //Создать список и загрузить из строки
  List := TsgeStringList.Create;
  List.FromString(Str);

  //Просмотр параметров
  c := GetCount - 1;
  for i := 0 to c do
  begin
    //Подготовить имя параметра из списка
    Param1 := LowerCase(FParamList[i].Name);

    //Просмотр строк файла
    Found := False;
    k := List.Count - 1;
    for j := 0 to k do
    begin
      Prm := List.Part[j];

      //Пропуск лишнего
      S := sgeTrim(Prm);
      if S = '' then
        Continue;

      if S[1] = Commentary then
        Continue;

      //Поиск параметра в строке
      Param2 := '';
      m := Length(Prm);
      for l := 1 to m do
      begin
        if Prm[l] = Separator then
          Break;
        Param2 := Param2 + Prm[l];
      end;

      //Сравнить параметры
      if Param1 = LowerCase(sgeTrim(Param2)) then
      begin
        Found := True;
        Break;
      end;
    end;

      //Обработка параметра
      if Found then
        List.Part[j] := Copy(List.Part[j], 1, l + 1) + FParamList[i].Value
      else
        List.Add(FParamList[i].Name + ' ' + Separator + ' ' + FParamList[i].Value);
  end;


  //Вернуть результат
  Str := List.ToString;

  //Освободить память
  List.Free;
end;


procedure TsgeSimpleParameters.UpdateFromString(Str: String);
var
  Prms: TsgeSimpleParameters;
  i, c: Integer;
begin
  try
    Prms := TsgeSimpleParameters.Create;

    //Чтение из строки
    Prms.FromString(Str);

    //Изменение параметров
    c := Prms.Count - 1;
    for i := 0 to c do
      SetValue(Prms.FParamList[i].Name, Prms.FParamList[i].Value);

  finally
    Prms.Free;
  end;
end;


procedure TsgeSimpleParameters.FromString(Str: String);
var
  List: TsgeStringList;
  i, c, j, k: Integer;
  S, sParam, sValue: String;
  IsName: Boolean;
  Symbol: Char;
begin
  //Очистить список
  Clear;

  //Разбить на строки
  List := TsgeStringList.Create;
  List.FromString(Str);

  //Обработать строки
  c := List.Count - 1;
  for i := 0 to c do
  begin
    S := sgeTrim(List.Part[i]);

    if S = '' then
      Continue;

    if S[1] = Commentary then
      Continue;


    //Подготовить переменные
    sParam := '';
    sValue := '';
    IsName := True;

    //Цикл по символам
    k := Length(S);
    for j := 1 to k do
    begin
      //Выделить символ
      Symbol := S[j];

      //Проверить на разделитель
      if (Symbol = Separator) and IsName then
      begin
        IsName := False;
        Continue;
      end;

      //Добавить символ
      case IsName of
        True:
          sParam := sParam + Symbol;

        False:
          sValue := sValue + Symbol;
      end;

    end;

    //Обработать параметр
    sParam := sgeTrim(sParam);
    if sParam = '' then
      Continue;

    sValue := sgeTrim(sValue);
    SetValue(sParam, sValue);
  end;

  List.Free;
end;


function TsgeSimpleParameters.ToString: String;
var
  i, c: Integer;
begin
  Result := '';
  c := GetCount - 1;
  for i := 0 to c do
  begin
    Result := Result + FParamList[i].Name + ' ' + Separator + ' ' + FParamList[i].Value;
    if i <> c then
      Result := Result + LineSeparator;
  end;
end;


procedure TsgeSimpleParameters.FromMemoryStream(Stream: TsgeMemoryStream);
begin
  FromString(Stream.ToString);
end;


procedure TsgeSimpleParameters.ToMemoryStream(Stream: TsgeMemoryStream);
begin
  Stream.FromString(ToString);
end;


procedure TsgeSimpleParameters.CopyFrom(Parameters: TsgeSimpleParameters);
begin
  Clear;
  Add(Parameters);
end;


procedure TsgeSimpleParameters.CopyTo(Parameters: TsgeSimpleParameters);
var
  i, c: Integer;
begin
  //Почистить выходной список
  Parameters.Clear;

  //Скопировать строчки
  c := Count - 1;
  for i := 0 to c do
    Parameters.Add(FParamList[i].Name, FParamList[i].Value);
end;


procedure TsgeSimpleParameters.SaveToFile(FileName: String);
var
  F: TsgeFile;
  S: String;
  Size: Integer;
begin
  if FileName = '' then
    FileName := FFileName;

  //Подготовить строку для записи
  S := ToString;
  Size := Length(S);

  //Записать в файл
  try
    try
      F := TsgeFile.Create(FileName, fmWrite, True);
      F.Size := 0;
      F.Write(S[1], Size);
    except
      raise EsgeException.Create(_UNITNAME, Err_CantWriteFile, FileName);
    end;

  finally
    F.Free
  end;
end;


procedure TsgeSimpleParameters.LoadFromFile(FileName: String);
var
  F: TsgeFile;
  S: String;
  Size: Integer;
begin
  if FileName = '' then
    FileName := FFileName;

  if not sgeFileExists(FileName) then
    raise EsgeException.Create(_UNITNAME, Err_FileNotFound, FileName);

  //Прочитать файл в строку
  try
    try
      F := TsgeFile.Create(FileName, fmRead, False);
      Size := F.Size;
      SetLength(S, Size);
      F.Read(S[1], Size);
    except
      raise EsgeException.Create(_UNITNAME, Err_CantWriteFile, FileName);
    end;

  finally
    F.Free;
  end;

  //Преобразовать строку в параметры
  FromString(S);
end;


procedure TsgeSimpleParameters.UpdateInFile(FileName: String);
var
  Ms: TsgeMemoryStream;
  Str: String;
begin
  if FileName = '' then
    FileName := FFileName;

  if not sgeFileExists(FileName) then
    raise EsgeException.Create(_UNITNAME, Err_FileNotFound, FileName);

  try
    Ms := TsgeMemoryStream.Create;

    try
      //Загрузить файл
      Ms.LoadFromFile(FileName);

      //Изменить параметры в строке
      Str := Ms.ToString;

      //Обновить секции в строке
      UpdateInString(Str);

      //Обновить данные в памяти
      Ms.FromString(Str);

      //Сохранить в файл
      Ms.SaveToFile(FileName);
    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantUpdateInFile, FileName, E.Message);
    end;

  finally
    Ms.Free;
  end;
end;


procedure TsgeSimpleParameters.UpdateFromFile(FileName: String);
var
  Ms: TsgeMemoryStream;
begin
  if FileName = '' then
    FileName := FFileName;

  try
    Ms := TsgeMemoryStream.Create;

    try
      //Чтение из файла
      Ms.LoadFromFile(FileName);

      //Обновить из строки
      UpdateFromString(Ms.ToString);

    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantUpdateFromFile, FileName, E.Message);
    end;

  finally
    Ms.Free;
  end;
end;



end.

