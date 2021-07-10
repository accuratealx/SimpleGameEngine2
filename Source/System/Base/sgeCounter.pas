{
Пакет             Simple Game Engine 2
Файл              sgeCounter.pas
Версия            1.1
Создан            05.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Счётчик событий в секунду
}
{$Include Defines.inc}

unit sgeCounter;

{$mode objfpc}{$H+}

interface


type
  TsgeCounter = class
  private
    FCurrentTime: QWord;                //Текущее время вызова
    FLastTime: QWord;                   //Последнее время вызова
    FCounter: Cardinal;                 //Счётчик до наступления интервала
    FCount: Cardinal;                   //Сколько срабатываний в интервал
    FInterval: Cardinal;                //Время замера в милисекундах

    function GetStrCount: String;
  public
    constructor Create(Interval: Cardinal = 1000);

    procedure Clear;
    procedure Inc;

    property Count: Cardinal read FCount;
    property StrCount: String read GetStrCount;
    property Interval: Cardinal read FInterval write FInterval;
  end;


implementation

uses
  sgeOSPlatform;


function TsgeCounter.GetStrCount: String;
begin
  Str(FCount, Result);
end;


constructor TsgeCounter.Create(Interval: Cardinal);
begin
  FInterval := Interval;
  Clear;
end;


procedure TsgeCounter.Clear;
begin
  FLastTime := sgeGetTickCount;
  FCounter := 0;
  FCount := 0;
end;


procedure TsgeCounter.Inc;
begin
  FCurrentTime := sgeGetTickCount;
  if FCurrentTime - FLastTime > FInterval then
    begin
    FCount := FCounter;
    FCounter := 0;
    FLastTime := FCurrentTime;
    end else System.Inc(FCounter);
end;


end.

