{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicFadeAdd.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Затемнение: Добавить
}
{$Include Defines.inc}

unit sgeEventGraphicFadeAdd;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeEventBase, sgeColor,
  sgeGraphicOpenGLDrawObjectFadeItem;


const
  Event_Graphic_FadeAdd = 'Graphic.Fade.Add';


type
  TsgeEventGraphicFadeAdd = class(TsgeEventBase)
  private
    FMode: TsgeFadeMode;
    FColor: TsgeColor;
    FTime: Cardinal;
    FID: Integer;
    FTimeProc: TsgeFadeProc;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer; TimeProc: TsgeFadeProc);

    function Copy: TsgeEventBase; override;

    property Mode: TsgeFadeMode read FMode;
    property Color: TsgeColor read FColor;
    property Time: Cardinal read FTime;
    property ID: Integer read FID;
    property TimeProc: TsgeFadeProc read FTimeProc;
  end;


implementation


function TsgeEventGraphicFadeAdd.GetName: ShortString;
begin
  Result := Event_Graphic_FadeAdd;
end;


constructor TsgeEventGraphicFadeAdd.Create(Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer; TimeProc: TsgeFadeProc);
begin
  FMode := Mode;
  FColor := Color;
  FTime := Time;
  FID := ID;
  FTimeProc := TimeProc;
end;


function TsgeEventGraphicFadeAdd.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicFadeAdd.Create(FMode, FColor, FTime, FID, FTimeProc);
end;



end.

