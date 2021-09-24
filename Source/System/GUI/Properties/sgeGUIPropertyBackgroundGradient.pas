{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyBackgroundGradient.pas
Версия            1.0
Создан            22.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Background: Градиентная заливка
}
{$Include Defines.inc}

unit sgeGUIPropertyBackgroundGradient;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicColor,
  sgeGUIProperty;


type
  TsgeGUIPropertyBackgroundGradient = class(TsgeGUIProperty)
  private
    FTopLeft: TsgeColor;
    FTopRight: TsgeColor;
    FBottomLeft: TsgeColor;
    FBottomRight: TsgeColor;

    procedure SetTopLeft(AColor: TsgeColor);
    procedure SetTopRight(AColor: TsgeColor);
    procedure SetBottomLeft(AColor: TsgeColor);
    procedure SetBottomRight(AColor: TsgeColor);
  public
    constructor Create(AOwner: TObject); override;

    procedure Draw;

    property TopLeft: TsgeColor read FTopLeft write SetTopLeft;
    property TopRight: TsgeColor read FTopRight write SetTopRight;
    property BottomLeft: TsgeColor read FBottomLeft write SetBottomLeft;
    property BottomRight: TsgeColor read FBottomRight write SetBottomRight;
  end;



implementation

uses
  sgeGraphic,
  sgeGUIElement, sgeVars;

type
  TsgeGUIElementHack = class(TsgeGUIElement);


procedure TsgeGUIPropertyBackgroundGradient.SetTopLeft(AColor: TsgeColor);
begin
  FTopLeft := AColor;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundGradient.SetTopRight(AColor: TsgeColor);
begin
  FTopRight := AColor;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundGradient.SetBottomLeft(AColor: TsgeColor);
begin
  FBottomLeft := AColor;
  RepaintParent;
end;


procedure TsgeGUIPropertyBackgroundGradient.SetBottomRight(AColor: TsgeColor);
begin
  FBottomRight := AColor;
  RepaintParent;
end;


constructor TsgeGUIPropertyBackgroundGradient.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FTopLeft := cBlack;
  FTopRight := cBlack;
  FBottomLeft := cBlack;
  FBottomRight := cBlack;
end;


procedure TsgeGUIPropertyBackgroundGradient.Draw;
var
  Col: TsgeQuadColor;
  W, H: Single;
begin
  Col := sgeGetQuadColor(FTopLeft, FBottomLeft, FBottomRight, FTopRight);
  W := TsgeGUIElementHack(FOwner).FWidth;
  H := TsgeGUIElementHack(FOwner).FHeight;

  with SGE.ExtGraphic.Graphic do
    begin
    doCoordinateType := gctNormal;
    DrawRectGradient(0, 0, W, H, Col);
    end;
end;



end.
