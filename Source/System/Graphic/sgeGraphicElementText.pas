{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementText.pas
Версия            1.0
Создан            26.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Текст
}
{$Include Defines.inc}

unit sgeGraphicElementText;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicElementBase,
  sgeGraphic, sgeGraphicColor, sgeGraphicFont;

type
  //Настройки элемента
  TsgeGraphicElementTextData = record
    X: Single;
    Y: Single;
    Font: TsgeGraphicFont;
    Color: TsgeColor;
    Text: ShortString;
  end;


  TsgeGraphicElementText = class(TsgeGraphicElementBase)
  private
    FData: TsgeGraphicElementTextData;
    FNewData: TsgeGraphicElementTextData;

    procedure SetAlpha(AAlpha: Single);
    function  GetAlpha: Single;
  public
    constructor Create(X, Y: Single; Font: TsgeGraphicFont; Text: String; Color: TsgeColor);

    procedure UpdateData; override;
    procedure Draw(Graphic: TsgeGraphic); override;

    property Alpha: Single read GetAlpha write SetAlpha;
    property X: Single read FNewData.X write FNewData.X;
    property Y: Single read FNewData.Y write FNewData.Y;
    property Font: TsgeGraphicFont read FNewData.Font write FNewData.Font;
    property Color: TsgeColor read FNewData.Color write FNewData.Color;
    property Text: ShortString read FNewData.Text write FNewData.Text;
  end;



implementation


procedure TsgeGraphicElementText.SetAlpha(AAlpha: Single);
begin
  FNewData.Color := sgeChangeColorAlpha(FNewData.Color, AAlpha);
end;


function TsgeGraphicElementText.GetAlpha: Single;
begin
  Result := FNewData.Color.Alpha;
end;


constructor TsgeGraphicElementText.Create(X, Y: Single; Font: TsgeGraphicFont; Text: String; Color: TsgeColor);
begin
  FData.X := X;
  FData.Y := Y;
  FData.Font := Font;
  FData.Color := Color;
  FData.Text := Text;

  FNewData := FData;
end;


procedure TsgeGraphicElementText.UpdateData;
begin
  FData := FNewData;
end;


procedure TsgeGraphicElementText.Draw(Graphic: TsgeGraphic);
begin
  Graphic.Color := FData.Color;
  Graphic.DrawText(FData.X, FData.Y, FData.Font, FData.Text);
end;




end.

