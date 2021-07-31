{
Пакет             Simple Game Engine 2
Файл              sgeErrorManager.pas
Версия            1.1
Создан            25.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс: Обработчик ошибок
}
{$Include Defines.inc}

unit sgeErrorManager;

{$mode objfpc}{$H+}

interface

uses
  sgeErrors, sgeCriticalSection, sgeJournal;


const
  Object_ErrorManager = 'ErrorManager';


type
  TsgeErrorManager = class
  private
    //Классы
    FCS: TsgeCriticalSection;
    FJournal: TsgeJournal;

    //Параметры
    FShowMessage: Boolean;
    FWriteToJournal: Boolean;
    FWriteToShell: Boolean;

    FShellHandler: TsgeErrorHandler;

    procedure SetWriteToJournal(AEnable: Boolean);
  public
    constructor Create(JournalFile: String);
    destructor  Destroy; override;

    procedure LogJournal(Message: String);                          //Записать в журнал
    procedure LogMessage(Message: String);                          //Показать диалоговое окно
    procedure LogShell(Message: String);                            //Записать в оболочку

    procedure ProcessError(Error: String);                          //Обработать ошибку

    property ShowMessage: Boolean read FShowMessage write FShowMessage;
    property WriteToJournal: Boolean read FWriteToJournal write SetWriteToJournal;
    property WriteToShell: Boolean read FWriteToShell write FWriteToShell;

    property ShellHandler: TsgeErrorHandler read FShellHandler write FShellHandler;
  end;



implementation

uses
  sgeOSPlatform;


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

  //Задать параметры
  FShowMessage := False;
  FWriteToJournal := False;
  FWriteToShell := True;
end;


destructor TsgeErrorManager.Destroy;
begin
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

    sgeShowMessage(Message, 'Error', mtInfo);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeErrorManager.LogShell(Message: String);
begin
  FCS.Enter;
  try

    if Assigned(FShellHandler) then FShellHandler(Message);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeErrorManager.ProcessError(Error: String);
begin
  //Переделать ошибку в человеческий вид
  //Применить перевод

  //Обработать ошибку
  if FWriteToJournal then LogJournal(Error);
  if FWriteToShell then LogShell(Error);
  if FShowMessage then LogMessage(Error);
end;



end.

