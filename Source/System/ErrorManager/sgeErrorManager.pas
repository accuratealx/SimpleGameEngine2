{
Пакет             Simple Game Engine 2
Файл              sgeErrorManager.pas
Версия            1.2
Создан            25.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс: Обработчик ошибок
}
{$Include Defines.inc}

unit sgeErrorManager;

{$mode objfpc}{$H+}

interface

uses
  sgeErrors, sgeCriticalSection, sgeJournal, sgeSimpleParameters;


const
  Object_ErrorManager = 'ErrorManager';


type
  TsgeErrorManager = class
  private
    //Классы
    FCS: TsgeCriticalSection;
    FJournal: TsgeJournal;                                          //Класс протоколирования в файл
    FLanguage: TsgeSimpleParameters;                                //Таблица с языком

    //Параметры
    FShowMessage: Boolean;
    FWriteToJournal: Boolean;
    FWriteToShell: Boolean;

    FShellHandler: TsgeErrorHandler;

    function GetLocalizedString(ErrorMessage: String): String;      //Перевести ошибку

    procedure SetWriteToJournal(AEnable: Boolean);
  public
    constructor Create(JournalFile: String);
    destructor  Destroy; override;

    procedure LogJournal(Message: String);                          //Записать в журнал
    procedure LogMessage(Message: String);                          //Показать диалоговое окно
    procedure LogShell(Message: String);                            //Записать в оболочку

    procedure ProcessError(Error: String);                          //Обработать ошибку

    property Language: TsgeSimpleParameters read FLanguage;
    property ShowMessage: Boolean read FShowMessage write FShowMessage;
    property WriteToJournal: Boolean read FWriteToJournal write SetWriteToJournal;
    property WriteToShell: Boolean read FWriteToShell write FWriteToShell;

    property ShellHandler: TsgeErrorHandler read FShellHandler write FShellHandler;
  end;



implementation

uses
  sgeOSPlatform, sgeStringList, sgeSimpleCommand;


function TsgeErrorManager.GetLocalizedString(ErrorMessage: String): String;
var
  Lines: TsgeStringList;
  Line: TsgeSimpleCommand;
  i: Integer;
  aUnitName, ErrorText, Info, S: String;
begin
  Lines := TsgeStringList.Create;
  Line := TsgeSimpleCommand.Create('', False, #9);

  //Разобрать на строки
  Lines.FromString(ErrorMessage);

  //Просмотреть строки
  for i := 0 to Lines.Count - 1 do
  begin
    //Разобрать на части ошибку
    Line.Command := Lines.Part[i];

    //Получить части
    if Line.Count >= 1 then
      aUnitName := Line.Part[0]
    else
      aUnitName := '';

    if Line.Count >= 2 then
      ErrorText := Line.Part[1]
    else
      ErrorText := '';

    if Line.Count >= 3 then
      Info := Line.Part[2]
    else
      Info := '';

    //Перевод модуля
    if aUnitName <> '' then
      aUnitName := FLanguage.GetValue('Unit:' + aUnitName, aUnitName);

    //Перевод ошибки
    if ErrorText <> '' then
      ErrorText := FLanguage.GetValue('Error:' + ErrorText, ErrorText);

    //Изменить строку по формату Имя модуля   Имя модуля: Строка ошибки (Пояснение)
    S := aUnitName;
    if ErrorText <> '' then
      S := S + ': ' + ErrorText;
    if Info <> '' then
      S := S + ' (' + Info + ')';
    Lines.Part[i] := S;
  end;

  //Вернуть результат
  Result := Lines.ToString;

  //Почистить память
  Line.Free;
  Lines.Free;
end;


procedure TsgeErrorManager.SetWriteToJournal(AEnable: Boolean);
begin
  FWriteToJournal := AEnable;
  FJournal.Enable := FWriteToJournal;
end;


constructor TsgeErrorManager.Create(JournalFile: String);
begin
  //Создать классы
  FCS := sgeCriticalSection.TsgeCriticalSection.Create;
  FJournal := TsgeJournal.Create(JournalFile);
  FLanguage := TsgeSimpleParameters.Create;

  //Задать параметры
  FShowMessage := False;
  FWriteToJournal := False;
  FWriteToShell := True;
end;


destructor TsgeErrorManager.Destroy;
begin
  FLanguage.Free;
  FJournal.Free;
  FCS.Free;
end;


procedure TsgeErrorManager.LogJournal(Message: String);
begin
  FCS.Enter;
  try
    FJournal.LogDetail(Message);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeErrorManager.LogMessage(Message: String);
begin
  FCS.Enter;
  try
    sgeShowMessage(Message, 'Error', mtError);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeErrorManager.LogShell(Message: String);
begin
  FCS.Enter;
  try
    if Assigned(FShellHandler) then
      FShellHandler(Message);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeErrorManager.ProcessError(Error: String);
begin
  //Переводим ошибку
  Error := GetLocalizedString(Error);

  //Обработать ошибку
  if FWriteToJournal then
    LogJournal(Error);

  if FWriteToShell then
    LogShell(Error);

  if FShowMessage then
    LogMessage(Error);
end;



end.

