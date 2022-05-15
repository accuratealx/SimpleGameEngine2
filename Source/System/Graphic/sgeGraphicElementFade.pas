{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementFade.pas
Версия            1.0
Создан            15.05.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Затемнение
}
{$Include Defines.inc}

unit sgeGraphicElementFade;

{$mode ObjFPC}{$H+}

interface

uses
  sgeGraphic, sgeGraphicColor,
  sgeGraphicElementBase, sgeScreenFade;

type
  //Настройки элемента
  TsgeGraphicElementFadeData = record
    Color: TsgeColor;                                               //Цвет перехода
    Mode: TsgeScreenFadeMode;                                       //Режим перехода
    Time: Cardinal;                                                 //Время срабатывания
    Eventproc: TsgeScreenFadeProc;                                  //Функция
  end;


  TsgeGraphicElementFade = class(TsgeGraphicElementBase)
  private
    FData: TsgeGraphicElementFadeData;
    FNewData: TsgeGraphicElementFadeData;

    FFade: TsgeScreenFade;
  protected
    procedure UpdateData; override;
  public
    constructor Create(Color: TsgeColor; Mode: TsgeScreenFadeMode; Time: Cardinal = 1000; Eventproc: TsgeScreenFadeProc = nil);
    destructor Destroy; override;

    procedure Draw(Graphic: TsgeGraphic); override;

    property Color: TsgeColor read FNewData.Color write FNewData.Color;
    property Mode: TsgeScreenFadeMode read FNewData.Mode write FNewData.Mode;
    property Time: Cardinal read FNewData.Time write FNewData.Time;
    property EventProc: TsgeScreenFadeProc read FNewData.Eventproc write FNewData.Eventproc;
  end;


implementation


procedure TsgeGraphicElementFade.UpdateData;
begin
  FData := FNewData;

  //Запустить затемнение
  FFade.Start(FData.Mode, FData.Color, FData.Time, FData.Eventproc);
end;


constructor TsgeGraphicElementFade.Create(Color: TsgeColor; Mode: TsgeScreenFadeMode; Time: Cardinal; Eventproc: TsgeScreenFadeProc);
begin
  inherited Create;

  FData.Color := Color;
  FData.Mode := Mode;
  FData.Time := Time;
  FData.Eventproc := Eventproc;
  FNewData := FData;

  FFade := TsgeScreenFade.Create;
end;


destructor TsgeGraphicElementFade.Destroy;
begin
  FFade.Free;
end;


procedure TsgeGraphicElementFade.Draw(Graphic: TsgeGraphic);
begin
  if FFade.Enable then
  begin
    Graphic.doCoordinateType := gctClassic;
    Graphic.DrawRect(0, 0, Graphic.Width, Graphic.Height, FFade.GetColor);
  end;
end;


end.

