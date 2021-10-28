{
Пакет             Simple Game Engine 2
Файл              sgeMusicPLayerTaskBase.pas
Версия            1.0
Создан            21.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Music player: Задача: Базовая
}
{$Include Defines.inc}

unit sgeMusicPLayerTaskBase;

{$mode objfpc}{$H+}

interface

type
  TsgeMusicPlayerTaskBase = class
  type
    TTaskState = (tsBegin, tsWork, tsEnd);

  private
    function GetTimes: Integer;

  protected
    FState: TTaskState;                                             //Внутреннее состояние
    FDone: Boolean;                                                 //Флаг завершения
    FTimes: Integer;                                                //Количество срабатываний задачи
    FDeltaTime: Single;                                             //Шаг изменения громкости

    function CalculateDeltaTime(Current, New: Single; Times: Cardinal): Single;

    //Методы задачи
    function  GetDeltaTime: Single; virtual;
    procedure Start; virtual;
    procedure Work; virtual;
    procedure Stop; virtual;
  public
    constructor Create;

    procedure Process;

    property Done: Boolean read FDone;
  end;


implementation

uses
  sgeVars,
  sgeExtensionMusicPlayer;

type
  TsgeExtMusicPlayerExt = class(TsgeExtensionMusicPlayer);


function TsgeMusicPlayerTaskBase.GetTimes: Integer;
var
  d: Single;
  ExtMusic: TsgeExtMusicPlayerExt;
begin
  ExtMusic := TsgeExtMusicPlayerExt(SGE.ExtMusicPlayer);

  d := ExtMusic.FFadeTime / ExtMusic.FThreadDelay;
  if d < 1 then d := 1;

  Result := Round(d);
end;


function TsgeMusicPlayerTaskBase.CalculateDeltaTime(Current, New: Single; Times: Cardinal): Single;
begin
  Result := (New - Current) / Times;
end;


function TsgeMusicPlayerTaskBase.GetDeltaTime: Single;
begin
  Result := 1;
end;


procedure TsgeMusicPlayerTaskBase.Start;
begin
  //Начало
end;


procedure TsgeMusicPlayerTaskBase.Work;
var
  ExtMusic: TsgeExtMusicPlayerExt;
begin
  //Ссылка на расширение
  ExtMusic := TsgeExtMusicPlayerExt(SGE.ExtMusicPlayer);

  //Изменить громкость
  ExtMusic.FSource.Gain := ExtMusic.FSource.Gain + FDeltaTime;
end;


procedure TsgeMusicPlayerTaskBase.Stop;
begin
  //Конец
end;


constructor TsgeMusicPlayerTaskBase.Create;
begin
  FState := tsBegin;
  FDone := False;
end;


procedure TsgeMusicPlayerTaskBase.Process;
begin
  if FDone then Exit;

  //Начало задачи
  if FState = tsBegin then
    begin
    //Установить количество срабатываний
    FTimes := GetTimes;

    //Определить шаг изменения громкости
    FDeltaTime := GetDeltaTime;

    //Начало задачи
    Start;

    //Следующий шаг
    FState := tsWork;
    end;

  //Процесс
  if FState = tsWork then
    begin
    Work;
    Dec(FTimes);
    if FTimes = 0 then FState := tsEnd;
    end;

  //Конец задачи
  if FState = tsEnd then
    begin
    Stop;
    FDone := True;
    end;
end;



end.
