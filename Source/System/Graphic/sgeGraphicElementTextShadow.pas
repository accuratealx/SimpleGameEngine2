{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementTextShadow.pas
Версия            1.0
Создан            26.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Текст с тенью
}
{$Include Defines.inc}

unit sgeGraphicElementTextShadow;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicElementBase,
  sgeGraphic, sgeGraphicColor, sgeGraphicFont;

type
  //Настройки элемента
  TsgeGraphicElementTextShadowData = record
    X: Single;
    Y: Single;
    XShadow: Single;
    YShadow: Single;
    Font: TsgeGraphicFont;
    Color: TsgeColor;
    ShadowColor: TsgeColor;
    Text: ShortString;
  end;


  TsgeGraphicElementTextShadow = class(TsgeGraphicElementBase)
  private
    FData: TsgeGraphicElementTextShadowData;
    FNewData: TsgeGraphicElementTextShadowData;

    procedure CorrectAlpha;

    procedure SetAlpha(AAlpha: Single);
    function  GetAlpha: Single;
    procedure SetColor(AColor: TsgeColor);
    procedure SetShadowColor(AColor: TsgeColor);
  public
    constructor Create(X, Y: Single; Font: TsgeGraphicFont; Text: String; Color: TsgeColor; ShadowColor: TsgeColor; XShadow: Single = 1; YShadow: Single = 1);

    procedure UpdateData; override;
    procedure Draw(Graphic: TsgeGraphic); override;

    property Alpha: Single read GetAlpha write SetAlpha;
    property X: Single read FNewData.X write FNewData.X;
    property Y: Single read FNewData.Y write FNewData.Y;
    property XShadow: Single read FNewData.XShadow write FNewData.XShadow;
    property YShadow: Single read FNewData.YShadow write FNewData.YShadow;
    property Font: TsgeGraphicFont read FNewData.Font write FNewData.Font;
    property Color: TsgeColor read FNewData.Color write SetColor;
    property ShadowColor: TsgeColor read FNewData.ShadowColor write SetShadowColor;
    property Text: ShortString read FNewData.Text write FNewData.Text;
  end;



implementation


procedure TsgeGraphicElementTextShadow.CorrectAlpha;
begin
  FNewData.ShadowColor := sgeChangeColorAlpha(FNewData.ShadowColor, FNewData.Color.Alpha);
end;


procedure TsgeGraphicElementTextShadow.SetAlpha(AAlpha: Single);
begin
  FNewData.Color := sgeChangeColorAlpha(FNewData.Color, AAlpha);
  CorrectAlpha;
end;


function TsgeGraphicElementTextShadow.GetAlpha: Single;
begin
  Result := FNewData.Color.Alpha;
end;


procedure TsgeGraphicElementTextShadow.SetColor(AColor: TsgeColor);
begin
  FNewData.Color := AColor;
  CorrectAlpha;
end;


procedure TsgeGraphicElementTextShadow.SetShadowColor(AColor: TsgeColor);
begin
  FNewData.ShadowColor := AColor;
  CorrectAlpha;
end;


constructor TsgeGraphicElementTextShadow.Create(X, Y: Single; Font: TsgeGraphicFont; Text: String; Color: TsgeColor; ShadowColor: TsgeColor; XShadow: Single; YShadow: Single);
begin
  FData.X := X;
  FData.Y := Y;
  FData.XShadow := XShadow;
  FData.YShadow := YShadow;
  FData.Font := Font;
  FData.Color := Color;
  FData.ShadowColor := ShadowColor;
  FData.Text := Text;

  FNewData := FData;
end;


procedure TsgeGraphicElementTextShadow.UpdateData;
begin
  FData := FNewData;
end;


procedure TsgeGraphicElementTextShadow.Draw(Graphic: TsgeGraphic);
begin
  //Тень
  Graphic.Color := FData.ShadowColor;
  Graphic.DrawText(FData.X + FData.XShadow, FData.Y + FData.YShadow, FData.Font, FData.Text);

  //Текст
  Graphic.Color := FData.Color;
  Graphic.DrawText(FData.X, FData.Y, FData.Font, FData.Text);
end;



end.

