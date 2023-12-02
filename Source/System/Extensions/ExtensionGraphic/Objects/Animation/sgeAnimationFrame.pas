{
Пакет             Simple Game Engine 2
Файл              sgeAnimationFrame.pas
Версия            1.0
Создан            09.06.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Кадр анимации
}
{$Include Defines.inc}

unit sgeAnimationFrame;

{$mode ObjFPC}{$H+}

interface

type
  TsgeAnimationFrame = class
  private
    FColumn: Word;        //Номер столбца
    FRow: Word;           //Номер строки
    FDuration: Cardinal;  //Длительность кадра на экране мс
  public
    constructor Create(Column, Row: Word; Duration: Cardinal);
    constructor Create(Str: String);

    procedure FromString(Str: String);

    property Column: Word read FColumn write FColumn;
    property Row: Word read FRow write FRow;
    property Duration: Cardinal read FDuration write FDuration;
  end;


implementation

uses
  sgeErrors, sgeSimpleCommand, sgeSystemUtils;

const
  _UNITNAME = 'AnimationFrame';

  Err_NotEnoughParameters = 'NotEnoughParameters';
  Err_UnableToDetermineColumn = 'UnableToDetermineColumn';
  Err_UnableToDetermineRow = 'UnableToDetermineRow';
  Err_UnableToDetermineDuration = 'UnableToDetermineDuration';


constructor TsgeAnimationFrame.Create(Column, Row: Word; Duration: Cardinal);
begin
  FColumn := Column;
  FRow := Row;
  FDuration := Duration;
end;


constructor TsgeAnimationFrame.Create(Str: String);
begin
  FromString(Str);
end;


procedure TsgeAnimationFrame.FromString(Str: String);
var
  Line: TsgeSimpleCommand;
  acol, aRow, aDuration: Integer;
begin
  Line := TsgeSimpleCommand.Create(Str);
  try

    //Проверить на наличие 3 частей (Col Row Duration)
    if Line.Count < 3 then
      raise EsgeException.Create(_UNITNAME, Err_NotEnoughParameters, Str);

    //Определить номер столбца
    if not sgeTryStrToInt(Line.Part[0], aCol) then
      raise EsgeException.Create(_UNITNAME, Err_UnableToDetermineColumn, Line.Part[0]);

    //Определить номер строки
    if not sgeTryStrToInt(Line.Part[1], aRow) then
      raise EsgeException.Create(_UNITNAME, Err_UnableToDetermineRow, Line.Part[1]);

    //Определить время видимости в строке милисекунды
    if not sgeTryStrToInt(Line.Part[2], aDuration) then
      raise EsgeException.Create(_UNITNAME, Err_UnableToDetermineDuration, Line.Part[2]);

    //Записать параметры
    FColumn := acol;
    FRow := aRow;
    FDuration := aDuration;

  finally
    Line.Free;
  end;
end;



end.

