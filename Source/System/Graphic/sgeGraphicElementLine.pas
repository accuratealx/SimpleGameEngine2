{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementLine.pas
Версия            1.0
Создан            28.12.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Линия
}
{$Include Defines.inc}

unit sgeGraphicElementLine;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicElementBase,
  sgeGraphic, sgeGraphicColor;


type
  //Настройки элемента
  TsgeGraphicElementLineData = record
    X1: Single;
    Y1: Single;
    X2: Single;
    Y2: Single;
    Color: TsgeColor;
    Width: Word;
    StippleMode: TsgeGraphicLineStipple;
    StippleScale: Integer;
  end;


  TsgeGraphicElementLine = class(TsgeGraphicElementBase)
  private
    FData: TsgeGraphicElementLineData;
    FNewData: TsgeGraphicElementLineData;

    procedure SetAlpha(AAlpha: Single);
    function  GetAlpha: Single;
  protected
    procedure UpdateData; override;
  public
    constructor Create;
    constructor Create(X1, Y1, X2, Y2: Single; Color: TsgeColor; Width: Word = 1; StippleMode: TsgeGraphicLineStipple = glsSolid; StippleScale: Integer = 1);

    procedure Draw(Graphic: TsgeGraphic); override;

    property Alpha: Single read GetAlpha write SetAlpha;
    property X1: Single read FNewData.X1 write FNewData.X1;
    property Y1: Single read FNewData.Y1 write FNewData.Y1;
    property X2: Single read FNewData.X2 write FNewData.X2;
    property Y2: Single read FNewData.Y2 write FNewData.Y2;
    property StippleMode: TsgeGraphicLineStipple read FNewData.StippleMode write FNewData.StippleMode;
    property StippleScale: Integer read FNewData.StippleScale write FNewData.StippleScale;
    property Color: TsgeColor read FNewData.Color write FNewData.Color;
  end;

implementation


procedure TsgeGraphicElementLine.SetAlpha(AAlpha: Single);
begin
  FNewData.Color := sgeChangeColorAlpha(FNewData.Color, AAlpha);
end;


function TsgeGraphicElementLine.GetAlpha: Single;
begin
  Result := FNewData.Color.Alpha;
end;


procedure TsgeGraphicElementLine.UpdateData;
begin
  FData := FNewData;
end;


constructor TsgeGraphicElementLine.Create;
begin
  inherited Create;

  FData.X1 := 0;
  FData.Y1 := 0;
  FData.X2 := 0;
  FData.Y2 := 0;
  FData.Color := cBlack;
  FData.Width := 1;
  FData.StippleScale := 1;
  FData.StippleMode := glsSolid;

  FNewData := FData;
end;


constructor TsgeGraphicElementLine.Create(X1, Y1, X2, Y2: Single; Color: TsgeColor; Width: Word; StippleMode: TsgeGraphicLineStipple; StippleScale: Integer);
begin
  inherited Create;

  FData.X1 := X1;
  FData.Y1 := Y1;
  FData.X2 := X2;
  FData.Y2 := Y2;
  FData.Color := Color;
  FData.Width := Width;
  FData.StippleScale := StippleScale;
  FData.StippleMode := StippleMode;

  FNewData := FData;
end;


procedure TsgeGraphicElementLine.Draw(Graphic: TsgeGraphic);
begin
  Graphic.PushAttrib;
  Graphic.LineStipple := True;
  Graphic.SetLineStipple(FData.StippleScale, FData.StippleMode);
  Graphic.DrawLine(FData.X1, FData.Y1, FData.X2, FData.Y2, FData.Color);
  Graphic.PopAttrib;
end;



end.

