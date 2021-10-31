{
Пакет             Simple Game Engine
Файл              sgeTimeEventItem.pas
Версия            1.3
Создан            31.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс таймерного события
}
{$Include Defines.inc}

unit sgeTimeEventItem;

{$mode objfpc}{$H+}

interface

uses
  sgeEventTimeEvent;


type
  //Элемент события
  TsgeTimeEventItem = class
  private
    //Основные параметры
    FDelay: Cardinal;                           //Задержка между вызовами в ms
    FEnable: Boolean;                           //Активность
    FTimes: Integer;                            //Количество срабатываний, -1 - бесконечно
    FProc: TsgeTimeEventProc;                   //Указатель на метод
    FStartDelay: Cardinal;                      //Задержка перед первым выполнением
    FAutoDelete: Boolean;                       //Автоудаление при выполнении

    //Дополнительные параметры
    FLastExecuteTime: Int64;                    //Время последнего вызова
    FTimesCount: Int64;                         //Количество срабатываний

    procedure SetTimes(ATimes: Integer);
    procedure SetEnable(AEnable: Boolean);
  public
    constructor Create(Proc: TsgeTimeEventProc; Delay: Cardinal = 0; Times: Integer = -1; AutoDelete: Boolean = True; StartDelay: Cardinal = 0; Enable: Boolean = True);
    destructor  Destroy; override;

    function  IsTimePassed: Boolean;
    procedure IncTimes;

    procedure Restart;
    procedure Start;
    procedure Stop;

    property Delay: Cardinal read FDelay write FDelay;
    property Enable: Boolean read FEnable write SetEnable;
    property Times: Integer read FTimes write SetTimes;
    property Proc: TsgeTimeEventProc read FProc write FProc;
    property StartDelay: Cardinal read FStartDelay write FStartDelay;
    property AutoDelete: Boolean read FAutoDelete write FAutoDelete;
    property TimesCount: Int64 read FTimesCount;
  end;





implementation

uses
  sgeErrors, sgeOSPlatform, sgeVars;


const
  _UNITNAME = 'TimeEventItem';

  Err_EmptyMethodPointer = 'EmptyMethodPointer';



procedure TsgeTimeEventItem.SetTimes(ATimes: Integer);
begin
  if ATimes < 0 then ATimes := -1;
  if FTimes = ATimes then Exit;

  FTimes := ATimes;
end;


procedure TsgeTimeEventItem.SetEnable(AEnable: Boolean);
begin
  if AEnable = FEnable then Exit;

  FEnable := AEnable;
  if FEnable then FLastExecuteTime := sgeGetTickCount + FStartDelay;
end;


constructor TsgeTimeEventItem.Create(Proc: TsgeTimeEventProc; Delay: Cardinal; Times: Integer; AutoDelete: Boolean; StartDelay: Cardinal; Enable: Boolean);
begin
  //Проверить метод
  if Proc = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyMethodPointer);

  //Основные параметры
  FProc := Proc;
  FDelay := Delay;
  FTimes := Times;
  FEnable := False;
  FStartDelay := StartDelay;
  FAutoDelete := AutoDelete;

  //Дополнительные параметры
  FTimesCount := 0;

  //Добавить себя в список
  SGE.ExtTimeEvent.TimeEventList.Add(Self);

  //Изменить активность
  SetEnable(Enable);
end;


destructor TsgeTimeEventItem.Destroy;
begin
  //Удалить себя из списка
  SGE.ExtTimeEvent.TimeEventList.Delete(Self);
end;


function TsgeTimeEventItem.IsTimePassed: Boolean;
var
  Now: Int64;
begin
  Result := False;

  Now := sgeGetTickCount;
  if (Now - FLastExecuteTime) > FDelay then
    begin
    FLastExecuteTime := Now;
    Result := True;
    end;
end;


procedure TsgeTimeEventItem.IncTimes;
begin
  //Увеличить счётчик срабатываний
  Inc(FTimesCount);
end;


procedure TsgeTimeEventItem.Restart;
begin
  FLastExecuteTime := sgeGetTickCount + FStartDelay;
  FTimesCount := 0;
  FEnable := True;
end;


procedure TsgeTimeEventItem.Start;
begin
  SetEnable(True);
end;


procedure TsgeTimeEventItem.Stop;
begin
  SetEnable(False);
end;



end.

