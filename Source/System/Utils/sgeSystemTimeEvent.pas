{
Пакет             Simple Game Engine 2
Файл              sgeSystemTimeEvent.pas
Версия            1.0
Создан            27.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Событие срабатывающее через определённый интервал времени
}
{$Include Defines.inc}

unit sgeSystemTimeEvent;

{$mode objfpc}{$H+}

interface



type
  TsgeEventProc = procedure of object;


  TsgeSystemTimeEvent = class
  private
    FDelay: Cardinal;                               //Задержка между тиками
    FEnable: Boolean;                               //Флаг работы
    FProc: TsgeEventProc;                           //Процедура для вызова
    FTimes: Integer;                                //Сколько раз срабатывать (меньше 0 - бесконечный вызов)
    FTimerID: Cardinal;                             //Идентификатор таймера
    FCount: Cardinal;                               //Всего вызовов
    FTimesCount: Cardinal;                          //Вызовов с каждого момента запуска

    FMinPeriod: Cardinal;                           //Миниимальный интервал
    FMaxPeriod: Cardinal;                           //Максимальный интервал

    procedure SetEnable(AEnable: Boolean);          //Переключить флаг работы
    procedure SetDelay(ADelay: Cardinal);           //Изменить задержку между вызовами
    procedure SetTimes(ATimes: Integer);            //Установить количество срабатываний после запуска
    procedure StartEvent;                           //Запуск таймера
    procedure StopEvent;                            //Останов таймера
    procedure CorrectPeriod(var Period: Cardinal);  //Поправить задержку
  public
    constructor Create(Delay: Cardinal; Enable: Boolean; Proc: TsgeEventProc; Times: Integer = -1);
    destructor  Destroy; override;

    property MinPeriod: Cardinal read FMinPeriod;
    property MaxPeriod: Cardinal read FMaxPeriod;
    property Delay: Cardinal read FDelay write SetDelay;
    property Enable: Boolean read FEnable write SetEnable;
    property Times: Integer read FTimes write SetTimes;
    property Proc: TsgeEventProc read FProc write FProc;
    property Count: Cardinal read FCount write FCount;
  end;


implementation

uses
  sgeErrors,
  MMSystem;


const
  _UNITNAME = 'SystemTimeEvent';

  Err_CantStartEvent = 'CantStartEvent';


function sgeTimeSetEvent(uDelay: Cardinal; uResolution: Cardinal; lpTimeProc: Pointer; dwUser: TObject; fuEvent: Cardinal): Cardinal; stdcall; external 'winmm.dll' name 'timeSetEvent';
function sgeTimeKillEvent(uTimerID: Cardinal): Cardinal; stdcall; external 'winmm.dll' name 'timeKillEvent';



//Функция обратного вызова
procedure sgeTimeEventProc(uTimerID, uMsg: Cardinal; dwUser, dw1, dw2: PtrUInt); stdcall;
var
  Eo: TsgeSystemTimeEvent absolute dwUser;
begin
  //Подготовить ссылку
  //Eo := TsgeEvent(dwUser);
  if Eo = nil then Exit;

  //Выполнить процедуру
  if Eo.FProc <> nil then
    begin
    Eo.Proc;                //Вызвать метод
    Inc(Eo.FCount);         //Прибавить вызов
    end;

  //Предусмотреть бесконечую работу, при отрицательных значениях
  if Eo.FTimes < 0 then Exit;

  //Обработать счётчик ограниченной работы
  Inc(Eo.FTimesCount);
  if Eo.FTimesCount >= Eo.FTimes then Eo.StopEvent;
end;


procedure TsgeSystemTimeEvent.SetEnable(AEnable: Boolean);
begin
  if FEnable = AEnable then Exit;
  if AEnable then StartEvent else StopEvent;
end;


procedure TsgeSystemTimeEvent.SetDelay(ADelay: Cardinal);
begin
  CorrectPeriod(ADelay);

  if FDelay = ADelay then Exit;
  FDelay := ADelay;

  if FEnable then
    begin
    StopEvent;
    StartEvent;
    end;
end;


procedure TsgeSystemTimeEvent.SetTimes(ATimes: Integer);
begin
  if FTimes = ATimes then Exit;
  FTimes := ATimes;

  if FEnable then
    begin
    StopEvent;
    StartEvent;
    end;
end;


procedure TsgeSystemTimeEvent.StartEvent;
begin
  FTimerID := sgeTimeSetEvent(FDelay, 0, @sgeTimeEventProc, Self, 1);
  if FTimerID = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantStartEvent);

  FEnable := True;
end;


procedure TsgeSystemTimeEvent.StopEvent;
begin
  sgeTimeKillEvent(FTimerID);             //Прибить таймер
  FEnable := False;                       //Выклоючить флаг работы
  FTimerID := 0;                          //Обнулить ID таймера
  FTimesCount := 0;                       //Обнулить счётчик ограниченной работы
end;


procedure TsgeSystemTimeEvent.CorrectPeriod(var Period: Cardinal);
begin
  if Period < FMinPeriod then Period := FMinPeriod;
  if Period > FMaxPeriod then Period := FMaxPeriod;
end;


constructor TsgeSystemTimeEvent.Create(Delay: Cardinal; Enable: Boolean; Proc: TsgeEventProc; Times: Integer);
var
  tc: TTIMECAPS;
begin
  //Определить диапазон
  timeGetDevCaps(@tc, SizeOf(TTIMECAPS));
  FMinPeriod := tc.wPeriodMin;
  FMaxPeriod := tc.wPeriodMax;

  //Поправить диапазон
  CorrectPeriod(Delay);

  FDelay := Delay;                        //Задержка
  FProc := Proc;                          //Метод класса без параметров
  FTimes := Times;                        //Раз срабатываний
  if Enable then StartEvent;              //Запуск если нужно
end;


destructor TsgeSystemTimeEvent.Destroy;
begin
  StopEvent;
end;



end.

