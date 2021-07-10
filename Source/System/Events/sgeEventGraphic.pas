{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphic.pas
Версия            1.0
Создан            06.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика
}
{$Include Defines.inc}

unit sgeEventGraphic;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeEventBase;


const
  Event_GraphicFade  = 'Graphic.Fade';


type
  TsgeEventGraphicFade = class(TsgeEventBase)
  private
    FPassedTime: TsgePassedTime;
  public
    constructor Create(PassedTime: TsgePassedTime);

    property PassedTime: TsgePassedTime read FPassedTime;
  end;




implementation


constructor TsgeEventGraphicFade.Create(PassedTime: TsgePassedTime);
begin
  FPassedTime := PassedTime;
end;


end.

