{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicFade.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Затемнение
}
{$Include Defines.inc}

unit sgeEventGraphicFade;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeEventBase;


const
  Event_Graphic_Fade = 'Graphic.Fade';


type
    TsgeEventGraphicFade = class(TsgeEventBase)
  private
    FPassedTime: TsgePassedTime;
    FID: Integer;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(PassedTime: TsgePassedTime; ID: Integer);

    function Copy: TsgeEventBase; override;

    property PassedTime: TsgePassedTime read FPassedTime;
    property ID: Integer read FID;
  end;


implementation


function TsgeEventGraphicFade.GetName: ShortString;
begin
  Result := Event_Graphic_Fade;
end;


constructor TsgeEventGraphicFade.Create(PassedTime: TsgePassedTime; ID: Integer);
begin
  FPassedTime := PassedTime;
  FID := ID;
end;


function TsgeEventGraphicFade.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicFade.Create(FPassedTime, FID);
end;



end.

