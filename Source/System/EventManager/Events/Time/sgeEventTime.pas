{
Пакет             Simple Game Engine 2
Файл              sgeEventTime.pas
Версия            1.4
Создан            31.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Таймер
}
{$Include Defines.inc}

unit sgeEventTime;

{$mode objfpc}{$H+}

interface

uses
  sgeEventBase;


const
  Event_Time  = 'Time';


type
  //Указатель на метод события
  TsgeTimeEventProc = procedure of object;


  TsgeEventTime = class(TsgeEventBase)
  private
    FProc: TsgeTimeEventProc;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(Proc: TsgeTimeEventProc);

    function Copy: TsgeEventBase; override;

    property Proc: TsgeTimeEventProc read FProc;
  end;


implementation


function TsgeEventTime.GetName: ShortString;
begin
  Result := Event_Time;
end;


constructor TsgeEventTime.Create(Proc: TsgeTimeEventProc);
begin
  FProc := Proc;
end;


function TsgeEventTime.Copy: TsgeEventBase;
begin
  Result := TsgeEventTime.Create(FProc);
end;



end.

