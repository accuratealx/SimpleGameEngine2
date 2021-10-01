{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyGradient.pas
Версия            1.1
Создан            22.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Градиентная заливка
}
{$Include Defines.inc}

unit sgeGUIPropertyGradient;

{$mode objfpc}{$H+}

interface

uses
  sgeGUIProperty, sgeGUIPropertyColor;


type
  TsgeGUIPropertyGradient = class(TsgeGUIProperty)
  private
    FTopLeft: TsgeGUIPropertyColor;
    FTopRight: TsgeGUIPropertyColor;
    FBottomLeft: TsgeGUIPropertyColor;
    FBottomRight: TsgeGUIPropertyColor;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property TopLeft: TsgeGUIPropertyColor read FTopLeft;
    property TopRight: TsgeGUIPropertyColor read FTopRight;
    property BottomLeft: TsgeGUIPropertyColor read FBottomLeft;
    property BottomRight: TsgeGUIPropertyColor read FBottomRight;
  end;


  TsgeGUIPropertyGradientExt = class(TsgeGUIPropertyGradient)
  public
    procedure Draw;
  end;


implementation

uses
  sgeGraphic, sgeGraphicColor,
  sgeGUIElement, sgeVars;

type
  TsgeGUIElementExtended = class(TsgeGUIElement);



constructor TsgeGUIPropertyGradient.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FTopLeft := TsgeGUIPropertyColor.Create(AOwner);
  FTopRight := TsgeGUIPropertyColor.Create(AOwner);
  FBottomLeft := TsgeGUIPropertyColor.Create(AOwner);
  FBottomRight := TsgeGUIPropertyColor.Create(AOwner);
end;


destructor TsgeGUIPropertyGradient.Destroy;
begin
  FTopLeft.Free;
  FTopRight.Free;
  FBottomLeft.Free;
  FBottomRight.Free;

  inherited Destroy;
end;



procedure TsgeGUIPropertyGradientExt.Draw;
var
  Col: TsgeQuadColor;
  W, H: Single;
begin
  Col := sgeGetQuadColor(FTopLeft.Color, FBottomLeft.Color, FBottomRight.Color, FTopRight.Color);
  W := TsgeGUIElementExtended(FOwner).FWidth;
  H := TsgeGUIElementExtended(FOwner).FHeight;

  with SGE.ExtGraphic.Graphic do
    begin
    doCoordinateType := gctNormal;
    DrawRectGradient(0, 0, W, H, Col);
    end;
end;



end.

