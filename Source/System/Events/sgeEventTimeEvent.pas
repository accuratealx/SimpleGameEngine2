{
Пакет             Simple Game Engine 2
Файл              sgeEventTimeEvent.pas
Версия            1.0
Создан            31.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Таймер
}
{$Include Defines.inc}

unit sgeEventTimeEvent;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeEventBase;


const
  Event_TimeEvent  = 'TimeEvent';


type
  //Указатель на метод события
  TsgeTimeEventProc = procedure of object;


  TsgeEventTimeEvent = class(TsgeEventBase)
  private
    FProc: TsgeTimeEventProc;
  public
    constructor Create(Proc: TsgeTimeEventProc);

    property Proc: TsgeTimeEventProc read FProc;
  end;




implementation


constructor TsgeEventTimeEvent.Create(Proc: TsgeTimeEventProc);
begin
  FProc := Proc;
end;


end.

