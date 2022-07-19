{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyGradient.pas
Версия            1.2
Создан            22.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Градиентная заливка
}
{$Include Defines.inc}

unit sgeGUIPropertyGradient;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeSimpleParameters,
  sgeGUIProperty, sgeGUIPropertyColor;


type
  TsgeGUIPropertyGradient = class(TsgeGUIProperty)
  private
    FTopLeft: TsgeGUIPropertyColorExt;
    FTopRight: TsgeGUIPropertyColorExt;
    FBottomLeft: TsgeGUIPropertyColorExt;
    FBottomRight: TsgeGUIPropertyColorExt;

    function GetTopLeft: TsgeGUIPropertyColor;
    function GetTopRight: TsgeGUIPropertyColor;
    function GetBottomLeft: TsgeGUIPropertyColor;
    function GetBottomRight: TsgeGUIPropertyColor;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property TopLeft: TsgeGUIPropertyColor read GetTopLeft;
    property TopRight: TsgeGUIPropertyColor read GetTopRight;
    property BottomLeft: TsgeGUIPropertyColor read GetBottomLeft;
    property BottomRight: TsgeGUIPropertyColor read GetBottomRight;
  end;


  TsgeGUIPropertyGradientExt = class(TsgeGUIPropertyGradient)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
    procedure Draw(Rect: TsgeFloatRect);
  end;


implementation

uses
  sgeCorePointerUtils,
  sgeGraphic, sgeGraphicColor;


function TsgeGUIPropertyGradient.GetTopLeft: TsgeGUIPropertyColor;
begin
  Result := FTopLeft;
end;


function TsgeGUIPropertyGradient.GetTopRight: TsgeGUIPropertyColor;
begin
  Result := FTopRight;
end;


function TsgeGUIPropertyGradient.GetBottomLeft: TsgeGUIPropertyColor;
begin
  Result := FBottomLeft;
end;


function TsgeGUIPropertyGradient.GetBottomRight: TsgeGUIPropertyColor;
begin
  Result := FBottomRight;
end;


constructor TsgeGUIPropertyGradient.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FTopLeft := TsgeGUIPropertyColorExt.Create(AOwner);
  FTopRight := TsgeGUIPropertyColorExt.Create(AOwner);
  FBottomLeft := TsgeGUIPropertyColorExt.Create(AOwner);
  FBottomRight := TsgeGUIPropertyColorExt.Create(AOwner);
end;


destructor TsgeGUIPropertyGradient.Destroy;
begin
  FTopLeft.Free;
  FTopRight.Free;
  FBottomLeft.Free;
  FBottomRight.Free;

  inherited Destroy;
end;


procedure TsgeGUIPropertyGradientExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
begin
  //TopLeft
  FTopLeft.LoadParameters(Parameters, Prefix + 'TopLeft.');

  //TopRight
  FTopRight.LoadParameters(Parameters, Prefix + 'TopRight.');

  //BottomLeft
  FBottomLeft.LoadParameters(Parameters, Prefix + 'BottomLeft.');

  //BottomRight
  FBottomRight.LoadParameters(Parameters, Prefix + 'BottomRight.');
end;


procedure TsgeGUIPropertyGradientExt.Draw(Rect: TsgeFloatRect);
var
  Col: TsgeQuadColor;
begin
  Col := sgeGetQuadColor(FTopLeft.Color, FBottomLeft.Color, FBottomRight.Color, FTopRight.Color);

  with sgeCorePointer_GetSGE.ExtGraphic.Graphic do
  begin
    doCoordinateType := gctClassic;
    DrawRectGradient(Rect, Col);
  end
end;



end.

