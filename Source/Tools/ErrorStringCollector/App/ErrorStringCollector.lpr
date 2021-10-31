program ErrorStringCollector;

{$mode objfpc}{$H+}
{$codepage UTF8}

uses
  sgeStringList, sgeSimpleParameters, sgeFileUtils, sgeOSPlatform,
  RegExpr;

const
  EXT_Pas = 'Pas';
  LngFileName = 'Error.Language';

type
  TLineType = (ltEmpty, ltUnit, ltError);

var
  BaseDir, FN, Value, Param: String;
  FileList, FileLines: TsgeStringList;
  LngFile: TsgeSimpleParameters;
  i, j: Integer;
  lt: TLineType;


procedure GetStringParameters(Str: String; out LineType: TLineType; out ConstValue: String);
var
  Reg: TRegExpr;
  Idx1, Idx2: Integer;
begin
  LineType := ltEmpty;
  ConstValue := '';

  Reg := TRegExpr.Create;

  //Проверка на имя модуля
  Reg.Expression := '_UNITNAME\s*=\s*'#39'\S*'#39';';
  if Reg.Exec(Str) then
    begin
    LineType := ltUnit;
    Idx1 := Pos(#39, Str);
    Idx2 := Pos(#39';', Str);
    ConstValue := Copy(Str, Idx1 + 1, Idx2 - Idx1 - 1);
    end;


  //Проверка на имя ошибки
  Reg.Expression := 'Err_\S*\s*=\s*'#39'\S*'#39';';
  if Reg.Exec(Str) then
    begin
    LineType := ltError;
    Idx1 := Pos(#39, Str);
    Idx2 := Pos(#39';', Str);
    ConstValue := Copy(Str, Idx1 + 1, Idx2 - Idx1 - 1);
    end;

  Reg.Free;
end;



{$R *.res}

begin
  //Базовый каталог
  BaseDir := sgeExtractFilePath(ParamStr(0));
  WriteLn('Каталог = ', BaseDir);

  //Создать выходной файл
  FN := BaseDir + LngFileName;
  LngFile := TsgeSimpleParameters.Create;
  if sgeFileExists(FN) then
    begin
    LngFile.LoadFromFile(FN);
    WriteLn('Найден файл языка ', LngFileName, ' загрузка');
    end;

  //Получить список файлов *.Pas
  FileList := TsgeStringList.Create;
  sgeFindFilesInFolderByExt(BaseDir, FileList, EXT_Pas);
  WriteLn('Найдено файлов = ', FileList.Count);

  //Просмотреть файлы
  for i := 0 to FileList.Count - 1 do
    begin
    FileLines := TsgeStringList.Create;
    FileLines.LoadFromFile(BaseDir + FileList.Part[i]);

    WriteLn('Обработка файла > ', FileList.Part[i]);

    //Обработать строки
    for j := 0 to FileLines.Count - 1 do
      begin
      //Пропуск пустых строк
      if FileLines.Part[j] = '' then Continue;

      //Определить тип строки и значение
      GetStringParameters(FileLines.Part[j], lt, Value);

      case lt of
        ltError : Param := 'Error:' + Value;
        ltUnit  : Param := 'Unit:' + Value;
      end;

      //Добавить параметр если его нет
      if lt <> ltEmpty then
        if not LngFile.Exist[Param] then
          LngFile.SetValue(Param, Value);
      end;

    FileLines.Free;
    end;


  //Почистить память
  FileList.Free;

  //Обновить файл если найдена хоть одна константа
  if LngFile.Count > 0 then
    begin
    if sgeFileExists(FN) then LngFile.UpdateInFile(FN) else LngFile.SaveToFile(FN);
    end;
  LngFile.Free;


  WriteLn('Создание файла ошибок выполнено.');
  ReadLn;
end.

