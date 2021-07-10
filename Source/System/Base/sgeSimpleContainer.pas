{
Пакет             Simple Game Engine 2
Файл              sgeSimpleContainer.pas
Версия            1.0
Создан            07.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс простых контейнеров
}
{$Include Defines.inc}

unit sgeSimpleContainer;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeStringList, sgeSimpleParameters, sgeMemoryStream;


type
  //Тип переноса скобок
  TsgeSimpleContainerWrap = set of (scwAfterName, scwAfterOpenStaple, scwBeforeCloseStaple);


  //Один параметр
  TsgeSimpleContainerSection= record
    Name: String;
    Value: String;
  end;


  TsgeSimpleContainer = class
  private
    FSectionList: array of TsgeSimpleContainerSection;        //Массив секций
    FSearchOptions: TsgeSearchOptions;                        //Модификаторы поиска
    FFileName: String;                                        //Имя файла по умолчанию
    FWrap: TsgeSimpleContainerWrap;                           //Тип переноса скобок

    function  GetSecuredValue(AValue: String): String;
    function  GetParameterName(Str: String): String;
    function  GetSectionAsString(Section: TsgeSimpleContainerSection): String;

    function  GetCount: Integer;
    procedure Add(Section: TsgeSimpleContainerSection);
    procedure SetSection(Index: Integer; ASection: TsgeSimpleContainerSection);
    function  GetSection(Index: Integer): TsgeSimpleContainerSection;
    procedure SetNamedSection(Name: String; ASection: TsgeSimpleContainerSection);
    function  GetNamedSection(Name: String): TsgeSimpleContainerSection;

    procedure PreCreate;
  public
    constructor Create(FileName: String = '');
    constructor Create(Stream: TsgeMemoryStream);
    destructor  Destroy; override;

    procedure Clear;
    function  IndexOf(Name: String): Integer;
    procedure Add(Name: String; Value: String);
    procedure Add(Sections: TsgeSimpleContainer);
    procedure Delete(Index: Integer);
    procedure Delete(Name: String);

    procedure SetSection(Name: String; Value: String);
    function  GetSection(Name: String): String;
    procedure GetSectionList(Name: String; List: TsgeStringList);
    procedure SetSectionList(Name: String; List: TsgeStringList);
    procedure GetSectionParameters(Name: String; Parameters: TsgeSimpleParameters);
    procedure SetSectionParameters(Name: String; Parameters: TsgeSimpleParameters);

    procedure SetSectionParameter(Name: String; Parameter: String; Value: String);
    procedure SetSectionParameter(Name: String; Parameter: String; Value: Integer);
    procedure SetSectionParameter(Name: String; Parameter: String; Value: Real);
    procedure SetSectionParameter(Name: String; Parameter: String; Value: Boolean);
    function  GetSectionParameter(Name: String; Parameter: String; DefValue: String = ''): String;
    function  GetSectionParameter(Name: String; Parameter: String; DefValue: Integer = 0): Integer;
    function  GetSectionParameter(Name: String; Parameter: String; DefValue: Real = 0): Real;
    function  GetSectionParameter(Name: String; Parameter: String; DefValue: Boolean = False): Boolean;
    function  SectionParameterExist(Name: String; Parameter: String): Boolean;

    procedure UpdateInString(var Str: String);
    procedure UpdateFromString(Str: String);
    procedure FromString(Str: String);
    function  ToString: String; override;
    procedure FromMemoryStream(Stream: TsgeMemoryStream);
    procedure ToMemoryStream(Stream: TsgeMemoryStream);
    procedure CopyFrom(Sections: TsgeSimpleContainer);
    procedure CopyTo(Sections: TsgeSimpleContainer);
    procedure SaveToFile(FileName: String = '');
    procedure LoadFromFile(FileName: String = '');
    procedure UpdateInFile(FileName: String = '');
    procedure UpdateFromFile(FileName: String = '');

    property SearchOptions: TsgeSearchOptions read FSearchOptions write FSearchOptions;
    property Wrap: TsgeSimpleContainerWrap read FWrap write FWrap;
    property FileName: String read FFileName write FFileName;
    property Count: Integer read GetCount;
    property Section[Index: Integer]: TsgeSimpleContainerSection read GetSection write SetSection;
    property NamedSection[Name: String]: TsgeSimpleContainerSection read GetNamedSection write SetNamedSection;
  end;


implementation

uses
  sgeErrors, sgeOSPlatform, sgeFile, sgeSystemUtils;


const
  _UNITNAME = 'SimpleContainers';

  LineSeparator   = #13#10;   //Разделитель строк
  NamePrefix      = '#';      //Префикс имени
  StapleOpen      = '{';      //Открывающая скобка
  StapleClose     = '}';      //Закрывающая скобка
  Shield          = '`';      //Символ экранирования
  ParamSeparator  = '=';      //Разделитель имени и мпараметра секции

  Err_IndexOutOfBounds    = 'IndexOutOfBounds';
  Err_NameNotFound        = 'NameNotFound';
  Err_FileNotFound        = 'FileNotFound';
  Err_CantWriteFile       = 'CantWriteFile';
  Err_CantReadFile        = 'CantReadFile';
  Err_CantUpdateInFile    = 'CantUpdateInFile';
  Err_CantUpdateFromFile  = 'CantUpdateFromFile';



function TsgeSimpleContainer.GetSecuredValue(AValue: String): String;
var
  i: Integer;
begin
  Result := '';

  for i := 1 to Length(AValue) do
    begin
    if (AValue[i] = Shield) or (AValue[i] = StapleClose) then
      begin
      Result := Result + Shield + AValue[i];
      Continue;
      end;

    if AValue[i] = StapleClose then Break;

    Result := Result + AValue[i];
    end;
end;


function TsgeSimpleContainer.GetParameterName(Str: String): String;
var
  i, c: Integer;
  Symbol: Char;
begin
  Result := '';

  c := Length(Str);
  for i := 1 to c do
    begin
    Symbol := Str[i];                         //Выделить символ
    if (Symbol = ParamSeparator) then Break;  //Проверить на разделитель
    Result := Result + Symbol;                //Добавить символ
    end;
end;


function TsgeSimpleContainer.GetSectionAsString(Section: TsgeSimpleContainerSection): String;
begin
  Result := NamePrefix + Section.Name;                                      //Записать имя
  if (scwAfterName in FWrap) then Result := Result + LineSeparator;         //Перенос значения
  Result := Result + StapleOpen;                                            //Окрывающая кавычка
  if (scwAfterOpenStaple in FWrap) then Result := Result + LineSeparator;   //Перенос открывающей кавычки
  Result := Result + GetSecuredValue(Section.Value);                        //Значение
  if (scwBeforeCloseStaple in FWrap) then Result := Result + LineSeparator; //Перенос закрывающей кавычки
  Result := Result + StapleClose;                                           //Закрывающая кавычка
end;


function TsgeSimpleContainer.GetCount: Integer;
begin
  Result := Length(FSectionList);
end;


procedure TsgeSimpleContainer.Add(Section: TsgeSimpleContainerSection);
var
  Idx, c: Integer;
begin
  //Проверить на пустое имя
  if Section.Name = '' then Exit;

  //Проверить модификаторы поиска
  if (soUnique in FSearchOptions) then
    begin
    Idx := IndexOf(Section.Name);
    if Idx <> -1 then
      begin
      FSectionList[Idx].Value := Section.Value;
      Exit;
      end;
    end;

  //Добавить секцию
  c := GetCount;
  SetLength(FSectionList, c + 1);
  FSectionList[c] := Section;
end;


procedure TsgeSimpleContainer.SetSection(Index: Integer; ASection: TsgeSimpleContainerSection);
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  FSectionList[Index] := ASection;
end;


function TsgeSimpleContainer.GetSection(Index: Integer): TsgeSimpleContainerSection;
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FSectionList[Index];
end;


procedure TsgeSimpleContainer.SetNamedSection(Name: String; ASection: TsgeSimpleContainerSection);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_NameNotFound, Name);

  FSectionList[Idx] := ASection;
end;


function TsgeSimpleContainer.GetNamedSection(Name: String): TsgeSimpleContainerSection;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_NameNotFound, Name);

  Result := FSectionList[Idx];
end;


procedure TsgeSimpleContainer.PreCreate;
begin
  FFileName := '';
  FSearchOptions := [];
  FWrap := [scwAfterName, scwAfterOpenStaple, scwBeforeCloseStaple];
end;


constructor TsgeSimpleContainer.Create(FileName: String);
begin
  PreCreate;
  FFileName := FileName;
  if FFileName <> '' then LoadFromFile(FFileName)
end;


constructor TsgeSimpleContainer.Create(Stream: TsgeMemoryStream);
begin
  PreCreate;
  FromMemoryStream(Stream);
end;


destructor TsgeSimpleContainer.Destroy;
begin
  Clear;
end;


procedure TsgeSimpleContainer.Clear;
begin
  SetLength(FSectionList, 0);
end;


function TsgeSimpleContainer.IndexOf(Name: String): Integer;
var
  i, c: Integer;
  s: String;
begin
  Result := -1;

  //Проверить модификатор поиска
  if not (soCaseSensivity in FSearchOptions) then Name := LowerCase(Name);


  c := Length(FSectionList) - 1;
  for i := 0 to c do
    begin
    //Проверить модификатор поиска
    if not (soCaseSensivity in FSearchOptions) then s := LowerCase(FSectionList[i].Name) else s := FSectionList[i].Name;

    //Сравнить
    if s = Name then
      begin
      Result := i;
      Break;
      end;
    end;
end;


procedure TsgeSimpleContainer.Add(Name: String; Value: String);
var
  Sect: TsgeSimpleContainerSection;
begin
  Sect.Name := Name;
  Sect.Value := Value;

  Add(Sect);
end;


procedure TsgeSimpleContainer.Add(Sections: TsgeSimpleContainer);
var
  i, c: Integer;
begin
  c := Sections.Count - 1;
  for i := 0 to c do
    Add(Sections.Section[i]);
end;


procedure TsgeSimpleContainer.Delete(Index: Integer);
var
  c, i: Integer;
begin
  c := GetCount - 1;
  if (Index < 0) or (Index > GetCount) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  for i := Index to c - 1 do
    FSectionList[i] := FSectionList[i + 1];

  SetLength(FSectionList, c);
end;


procedure TsgeSimpleContainer.Delete(Name: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_NameNotFound, Name);

  Delete(Idx);
end;


procedure TsgeSimpleContainer.SetSection(Name: String; Value: String);
begin
  Add(Name, Value);
end;


function TsgeSimpleContainer.GetSection(Name: String): String;
var
  Idx: Integer;
begin
  Result := '';

  Idx := IndexOf(Name);
  if Idx = -1 then Exit;

  Result := FSectionList[Idx].Value;
end;


procedure TsgeSimpleContainer.GetSectionList(Name: String; List: TsgeStringList);
var
  Idx: Integer;
  so: TsgeSearchOptions;
begin
  List.Clear;
  Idx := IndexOf(Name);
  if Idx = -1 then Exit;

  so := List.SearchOptions;
  List.SearchOptions := [];
  List.FromString(sgeTrim(FSectionList[Idx].Value));
  List.SearchOptions := so;
end;


procedure TsgeSimpleContainer.SetSectionList(Name: String; List: TsgeStringList);
begin
  Add(Name, List.ToString);
end;


procedure TsgeSimpleContainer.GetSectionParameters(Name: String; Parameters: TsgeSimpleParameters);
var
  Idx: Integer;
begin
  Parameters.Clear;
  Idx := IndexOf(Name);
  if Idx = -1 then Exit;

  Parameters.FromString(FSectionList[Idx].Value);
end;


procedure TsgeSimpleContainer.SetSectionParameters(Name: String; Parameters: TsgeSimpleParameters);
begin
  Add(Name, Parameters.ToString);
end;


procedure TsgeSimpleContainer.SetSectionParameter(Name: String; Parameter: String; Value: String);
var
  List: TsgeStringList;
  i, c, Idx, LenName, LenDel: Integer;
  pName, ParamName: String;
  S: String;
  Found: Boolean;
begin
  //Нет секции, добавить новую
  Idx := IndexOf(Name);
  if Idx = -1 then
    begin
    Add(Name, Parameter + ' ' + ParamSeparator + ' ' + Value);
    Exit;
    end;


  //Подготовить список строк секции
  List := TsgeStringList.Create;
  List.FromString(sgeTrim(FSectionList[Idx].Value));

  //Перебор строк и поиск параметров
  ParamName := LowerCase(Parameter);
  Found := False;
  c := List.Count - 1;
  for i := 0 to c do
    begin
    //Поиск имени параметра и положения разделителя
    S := List.Part[i];
    pName := GetParameterName(S);

    //Проверить совпадает ли имя параметра с указанным
    if ParamName = LowerCase(sgeTrim(pName)) then
      begin
      //Удалить значение параметра из строки
      LenName := Length(pName);
      LenDel := Length(S) - LenName + 1;
      System.Delete(S, LenName, LenDel);

      //Изменить занчение параметра
      S := S + ' = ' + Value;
      List.Part[i] := S;
      Found := True;
      Break;
      end;
    end;

  //Проверить на изменение
  if not Found then List.Add(Parameter + ' ' + ParamSeparator + ' ' + Value);

  //Изменить секцию
  FSectionList[Idx].Value := List.ToString;

  //Удалить список
  List.Free;
end;


procedure TsgeSimpleContainer.SetSectionParameter(Name: String; Parameter: String; Value: Integer);
begin
  SetSectionParameter(Name, Parameter, sgeIntToStr(Value));
end;


procedure TsgeSimpleContainer.SetSectionParameter(Name: String; Parameter: String; Value: Real);
begin
  SetSectionParameter(Name, Parameter, sgeFloatToStr(Value));
end;


procedure TsgeSimpleContainer.SetSectionParameter(Name: String; Parameter: String; Value: Boolean);
var
  s: String;
begin
  if Value then s := '1' else s := '0';
  SetSectionParameter(Name, Parameter, s);
end;


function TsgeSimpleContainer.GetSectionParameter(Name: String; Parameter: String; DefValue: String): String;
var
  Idx: Integer;
  Params: TsgeSimpleParameters;
begin
  Result := DefValue;

  //Поиск секции
  Idx := IndexOf(Name);
  if Idx = -1 then Exit;

  //Поиск параметра
  Params := TsgeSimpleParameters.Create;
  Params.FromString(FSectionList[Idx].Value);
  Result := Params.GetValue(Parameter, DefValue);
  Params.Free;
end;


function TsgeSimpleContainer.GetSectionParameter(Name: String; Parameter: String; DefValue: Integer): Integer;
var
  Idx: Integer;
  Params: TsgeSimpleParameters;
begin
  Result := DefValue;

  //Поиск секции
  Idx := IndexOf(Name);
  if Idx = -1 then Exit;

  //Поиск параметра
  Params := TsgeSimpleParameters.Create;
  Params.FromString(FSectionList[Idx].Value);
  Result := Params.GetValue(Parameter, DefValue);
  Params.Free;
end;


function TsgeSimpleContainer.GetSectionParameter(Name: String; Parameter: String; DefValue: Real): Real;
var
  Idx: Integer;
  Params: TsgeSimpleParameters;
begin
  Result := DefValue;

  //Поиск секции
  Idx := IndexOf(Name);
  if Idx = -1 then Exit;

  //Поиск параметра
  Params := TsgeSimpleParameters.Create;
  Params.FromString(FSectionList[Idx].Value);
  Result := Params.GetValue(Parameter, DefValue);
  Params.Free;
end;


function TsgeSimpleContainer.GetSectionParameter(Name: String; Parameter: String; DefValue: Boolean): Boolean;
var
  Idx: Integer;
  Params: TsgeSimpleParameters;
begin
  Result := DefValue;

  //Поиск секции
  Idx := IndexOf(Name);
  if Idx = -1 then Exit;

  //Поиск параметра
  Params := TsgeSimpleParameters.Create;
  Params.FromString(FSectionList[Idx].Value);
  Result := Params.GetValue(Parameter, DefValue);
  Params.Free;
end;


function TsgeSimpleContainer.SectionParameterExist(Name: String; Parameter: String): Boolean;
var
  Idx: Integer;
  Params: TsgeSimpleParameters;
begin
  Result := False;

  //Поиск секции
  Idx := IndexOf(Name);
  if Idx = -1 then Exit;

  //Поиск параметра
  Params := TsgeSimpleParameters.Create;
  Params.FromString(FSectionList[Idx].Value);
  Result := Params.Exist[Parameter];
  Params.Free;
end;


procedure TsgeSimpleContainer.UpdateInString(var Str: String);
const
  sNormal = 0;
  sName = 1;
  sValue = 2;
  sShield = 3;

var
  S: Char;
  i, c, j, k, IdxOpen, IdxClose: Integer;
  State: Byte;
  cName, cValue, SectionName: String;
  IsSet: Boolean;
begin
  //Цикл по списку параметров
  c := GetCount - 1;
  for i := 0 to c do
    begin

    //Подготовить параметр
    State := sNormal;
    cName := '';
    cValue := '';
    IsSet := False;
    SectionName := LowerCase(FSectionList[i].Name);

    //Цикл по строке
    k := Length(Str);
    for j := 1 to k do
      begin
      S := Str[j];

      case State of
        sNormal:
          begin
          if S = NamePrefix then
            begin
            State := sName;
            Continue;
            end;
          end;


        sName:
          begin
          if S = StapleOpen then
            begin
            State := sValue;
            IdxOpen := j;
            Continue;
            end;

          cName := cName + S;
          end;


        sValue:
          begin
          if S = Shield then
            begin
            State := sShield;
            Continue;
            end;

          if S = StapleClose then
            begin
            IdxClose := j;

            //Найдено совпадение по имени секции
            if LowerCase(sgeTrim(cName)) = SectionName then
              begin
              System.Delete(Str, IdxOpen + 1, IdxClose - IdxOpen - 1);                  //Удалить значние
              cValue := FSectionList[i].Value;                                          //Взять значение секции
              if (scwAfterOpenStaple in FWrap) then cValue := LineSeparator + cValue;   //Перенос после открывающей скобки
              if (scwBeforeCloseStaple in FWrap) then cValue := cValue + LineSeparator; //Перенос перед закрывающей скобкой
              System.Insert(cValue, Str, IdxOpen + 1);                                  //Вставить значение
              IsSet := True;
              Break;
              end;

            //Обнулить параметры
            cName := '';
            cValue := '';
            State := sNormal;
            Continue;
            end;

          cValue := cValue + S;
          end;


        sShield:
          begin
          State := sValue;
          cValue := cValue + S;
          end;
      end;

      end;//Цикл по строке


    //Если секция не изменена, то добавить новую
    if not IsSet then
      begin
      if Str <> '' then Str := Str + LineSeparator + LineSeparator;
      Str := Str + GetSectionAsString(FSectionList[i]);
      end;

    end;//For
end;


procedure TsgeSimpleContainer.UpdateFromString(Str: String);
var
  Sc: TsgeSimpleContainer;
  i, c: Integer;
begin
  try
    Sc := TsgeSimpleContainer.Create;

    //Чтение из строки
    Sc.FromString(Str);

    //Изменение секций
    c := Sc.Count - 1;
    for i := 0 to c do
      SetSection(Sc.FSectionList[i].Name, Sc.FSectionList[i].Value);

  finally
    Sc.Free;
  end;
end;


procedure TsgeSimpleContainer.FromString(Str: String);
const
  sNormal = 0;
  sName = 1;
  sValue = 2;
  sShield = 3;

var
  S: Char;
  i: Integer;
  State: Byte;
  cName, cValue: String;
begin
  //Подготовка
  State := sNormal;
  cName := '';
  cValue := '';
  Clear;

  //Просмотреть символы по порядку
  for i := 1 to Length(Str) do
    begin
    S := Str[i];

    case State of
      sNormal:
        begin
        if S = NamePrefix then
          begin
          State := sName;
          Continue;
          end;
        end;


      sName:
        begin
        if S = StapleOpen then
          begin
          State := sValue;
          Continue;
          end;

        cName := cName + S;
        end;


      sValue:
        begin
        if S = Shield then
          begin
          State := sShield;
          Continue;
          end;

        if S = StapleClose then
          begin
          Add(sgeTrim(cName), sgeTrim(cValue));
          cName := '';
          cValue := '';
          State := sNormal;
          Continue;
          end;

        cValue := cValue + S;
        end;


      sShield:
        begin
        State := sValue;
        cValue := cValue + S;
        end;
    end;


    end;//For
end;


function TsgeSimpleContainer.ToString: String;
var
  i, c: Integer;
  S: String;
begin
  Result := '';

  c := GetCount - 1;
  for i := 0 to c do
    begin
    //Записать секцию в строку
    S := GetSectionAsString(FSectionList[i]);
    Result := Result + S;

    //Разделитель строк
    if i <> c then Result := Result + LineSeparator + LineSeparator;
    end;
end;


procedure TsgeSimpleContainer.FromMemoryStream(Stream: TsgeMemoryStream);
begin
  FromString(Stream.ToString);
end;


procedure TsgeSimpleContainer.ToMemoryStream(Stream: TsgeMemoryStream);
begin
  Stream.FromString(ToString);
end;


procedure TsgeSimpleContainer.CopyFrom(Sections: TsgeSimpleContainer);
begin
  Clear;
  Add(Sections);
end;


procedure TsgeSimpleContainer.CopyTo(Sections: TsgeSimpleContainer);
var
  i, c: Integer;
begin
  //Почистить выходной список
  Sections.Clear;

  //Скопировать строчки
  c := Count - 1;
  for i := 0 to c do
    Sections.Add(FSectionList[i]);
end;


procedure TsgeSimpleContainer.SaveToFile(FileName: String);
var
  F: TsgeFile;
  S: String;
  Size: Integer;
begin
  if FileName = '' then FileName := FFileName;

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


procedure TsgeSimpleContainer.LoadFromFile(FileName: String);
var
  F: TsgeFile;
  S: String;
  Size: Integer;
begin
  if FileName = '' then FileName := FFileName;

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
      raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName);
    end;

  finally
    F.Free;
  end;

  //Преобразовать строку в параметры
  FromString(S);
end;


procedure TsgeSimpleContainer.UpdateInFile(FileName: String);
var
  Ms: TsgeMemoryStream;
  Str: String;
begin
  if FileName = '' then FileName := FFileName;

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


procedure TsgeSimpleContainer.UpdateFromFile(FileName: String);
var
  Ms: TsgeMemoryStream;
begin
  if FileName = '' then FileName := FFileName;

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

