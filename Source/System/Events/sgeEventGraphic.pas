{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphic.pas
Версия            1.2
Создан            06.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика
}
{$Include Defines.inc}

unit sgeEventGraphic;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeEventBase;


const
  Event_GraphicFade  = 'Graphic.Fade';


type
  TsgeEventGraphicFade = class(TsgeEventBase)
  private
    FPassedTime: TsgePassedTime;
    FID: Integer;
  public
    constructor Create(Name: ShortString; PassedTime: TsgePassedTime; ID: Integer);

    property PassedTime: TsgePassedTime read FPassedTime;
    property ID: Integer read FID;
  end;



implementation


constructor TsgeEventGraphicFade.Create(Name: ShortString; PassedTime: TsgePassedTime; ID: Integer);
begin
  inherited Create(Name);

  FPassedTime := PassedTime;
  FID := ID;
end;



end.

