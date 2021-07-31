{
Пакет             Simple Game Engine 2
Файл              sgeCommandHistory.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс хранения истории введённых команд
}
{$Include Defines.inc}

unit sgeCommandHistory;

{$mode objfpc}{$H+}

interface

uses
  sgeStringList, sgeMemoryStream;


type
  TsgeCommandHistory = class
  private
    FCommands: TsgeStringList;                                      //Массив строк истории
    FMaxLines: Word;                                                //Максимальное количество строк
    FCurrentIndex: Integer;                                         //Текущий индекс строки вставки

    procedure SetMaxLines(ALines: Word);
  public
    constructor Create(MaxLines: Word = 1024);
    destructor  Destroy; override;

    procedure Clear;
    procedure AddCommand(Cmd: String);

    procedure FromString(Str: String);
    function  ToString: String; override;
    procedure ToMemoryStream(Stream: TsgeMemoryStream);
    procedure FromMemoryStream(Stream: TsgeMemoryStream);
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);

    function  GetPreviousCommand: String;
    function  GetNextCommand: String;

    property MaxLines: Word read FMaxLines write SetMaxLines;
  end;



implementation

uses
  sgeErrors;


const
  _UNITNAME = 'sgeCommandHistory';

  Err_FileWriteError  = 'FileWriteError';
  Err_FileReadError   = 'FileReadError';


procedure TsgeCommandHistory.SetMaxLines(ALines: Word);
begin
  FMaxLines := ALines;

  while FCommands.Count > FMaxLines do
    FCommands.Delete(0);
end;


constructor TsgeCommandHistory.Create(MaxLines: Word);
begin
  FCommands := TsgeStringList.Create;
  FMaxLines := MaxLines;
end;


destructor TsgeCommandHistory.Destroy;
begin
  FCommands.Free;
end;


procedure TsgeCommandHistory.Clear;
begin
  FCommands.Clear;
end;


procedure TsgeCommandHistory.AddCommand(Cmd: String);
var
  Count: Integer;
begin
  if Cmd = '' then Exit;

  //Добавить команду
  Count := FCommands.Count - 1;
  if Count < 0 then FCommands.Add(Cmd) else
    if LowerCase(FCommands.Part[Count]) <> LowerCase(Cmd) then FCommands.Add(Cmd);

  //Проверить на выход за максимальное количество
  if FCommands.Count > FMaxLines then FCommands.Delete(0);

  //Поправить индекс
  FCurrentIndex := FCommands.Count;
end;


procedure TsgeCommandHistory.FromString(Str: String);
begin
  FCommands.FromString(Str);
  FCurrentIndex := FCommands.Count;
end;


function TsgeCommandHistory.ToString: String;
begin
  Result := FCommands.ToString;
end;


procedure TsgeCommandHistory.ToMemoryStream(Stream: TsgeMemoryStream);
begin
  FCommands.ToMemoryStream(Stream);
end;


procedure TsgeCommandHistory.FromMemoryStream(Stream: TsgeMemoryStream);
begin
  FCommands.FromMemoryStream(Stream);
  FCurrentIndex := FCommands.Count;
end;


procedure TsgeCommandHistory.SaveToFile(FileName: String);
begin
  try
    FCommands.SaveToFile(FileName);
  except
    raise EsgeException.Create(_UNITNAME, Err_FileWriteError, FileName);
  end;
end;


procedure TsgeCommandHistory.LoadFromFile(FileName: String);
begin
  try
    FCommands.LoadFromFile(FileName);
  except
    raise EsgeException.Create(_UNITNAME, Err_FileReadError, FileName);
  end;

  FCurrentIndex := FCommands.Count;
end;


function TsgeCommandHistory.GetPreviousCommand: String;
begin
  Result := '';
  if FCommands.Count = 0 then Exit;

  Dec(FCurrentIndex);
  if FCurrentIndex < 0 then FCurrentIndex := 0;
  Result := FCommands.Part[FCurrentIndex];
end;


function TsgeCommandHistory.GetNextCommand: String;
var
  c: Integer;
begin
  Result := '';
  c := FCommands.Count;
  if c = 0 then Exit;

  Inc(FCurrentIndex);
  if FCurrentIndex >= c then FCurrentIndex := c - 1;
  Result := FCommands.Part[FCurrentIndex];
end;


end.

