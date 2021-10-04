{
Пакет             Simple Game Engine 2
Файл              sgeGUITimer.pas
Версия            1.0
Создан            03.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Таймер
}
{$Include Defines.inc}

unit sgeGUITimer;

{$mode objfpc}{$H+}

interface

uses
  sgeTimeEventItem;


type
  TsgeGUITimerProc = procedure of object;


  TsgeGUITimer = class
  private
    //Классы
    FTimeEvent: TsgeTimeEventItem;

    //Свойства
    FOnTimer: TsgeGUITimerProc;

    procedure Timer;

    procedure SetEnable(AEnable: Boolean);
    function  GetEnable: Boolean;
    procedure SetDelay(ADelay: Cardinal);
    function  GetDelay: Cardinal;
    procedure SetStartDelay(ADelay: Cardinal);
    function  GetStartDelay: Cardinal;
  public
    constructor Create(Delay: Cardinal = 1000; Enable: Boolean = False; StartDelay: Cardinal = 0);
    destructor  Destroy; override;

    property Enable: Boolean read GetEnable write SetEnable;
    property Delay: Cardinal read GetDelay write SetDelay;
    property StartDelay: Cardinal read GetStartDelay write SetStartDelay;

    property OnTimer: TsgeGUITimerProc read FOnTimer write FOnTimer;
  end;


implementation


procedure TsgeGUITimer.Timer;
begin
  if Assigned(FOnTimer) then
    FOnTimer();
end;


procedure TsgeGUITimer.SetEnable(AEnable: Boolean);
begin
  FTimeEvent.Enable := AEnable;
end;


function TsgeGUITimer.GetEnable: Boolean;
begin
  Result := FTimeEvent.Enable;
end;


procedure TsgeGUITimer.SetDelay(ADelay: Cardinal);
begin
  FTimeEvent.Delay := ADelay;
end;


function TsgeGUITimer.GetDelay: Cardinal;
begin
  Result := FTimeEvent.Delay;
end;


procedure TsgeGUITimer.SetStartDelay(ADelay: Cardinal);
begin
  FTimeEvent.StartDelay := ADelay;
end;


function TsgeGUITimer.GetStartDelay: Cardinal;
begin
  Result := FTimeEvent.StartDelay;
end;


constructor TsgeGUITimer.Create(Delay: Cardinal; Enable: Boolean; StartDelay: Cardinal);
begin
  FTimeEvent := TsgeTimeEventItem.Create(@Timer, Delay, -1, False, StartDelay, Enable);
end;


destructor TsgeGUITimer.Destroy;
begin
  FTimeEvent.Free;
end;


end.

