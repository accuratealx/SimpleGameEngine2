{
Пакет             Simple Game Engine 2
Файл              sgeEventTimeEvent.pas
Версия            1.2
Создан            31.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Таймер
}
{$Include Defines.inc}

unit sgeEventTimeEvent;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeEventBase;


const
  Event_TimeEvent  = 'TimeEvent';


type
  //Указатель на метод события
  TsgeTimeEventProc = procedure of object;


  TsgeEventTimeEvent = class(TsgeEventBase)
  private
    FProc: TsgeTimeEventProc;
  public
    constructor Create(Name: ShortString; Proc: TsgeTimeEventProc);

    property Proc: TsgeTimeEventProc read FProc;
  end;



implementation


constructor TsgeEventTimeEvent.Create(Name: ShortString; Proc: TsgeTimeEventProc);
begin
  inherited Create(Name);

  FProc := Proc;
end;


end.

