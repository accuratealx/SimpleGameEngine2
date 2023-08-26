{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertySprite.pas
Версия            1.0
Создан            26.08.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Спрайт
}
{$Include Defines.inc}

unit sgeGUIPropertySprite;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeSprite,
  sgeGraphicColor,
  sgeGUIProperty, sgeGUIPropertyScale,
  sgeDisplayElementSprite;

type
  TsgeGUIPropertySprite = class(TsgeGUIProperty)
  private
    FScale: TsgeGUIPropertyScaleExt;
    FVerticalAlign: TsgeVerticalAlign;
    FHorizontalAlign: TsgeHorizontalAlign;

    FDisplaySprite: TsgeDisplayElementSprite;

    function GetScale: TsgeGUIPropertyScale;

    procedure SetVerticalAlign(AAlign: TsgeVerticalAlign);
    procedure SetHorizontalAlign(AAlign: TsgeHorizontalAlign);
    procedure SetReflect(AReflect: TsgeReflectSet);
    function  GetReflect: TsgeReflectSet;
    procedure SetColor(AColor: TsgeColor);
    function  GetColor: TsgeColor;
    procedure SetSprite(ASprite: TsgeSprite);
    function  GetSprite: TsgeSprite;
  public
    constructor Create(AOwner: TObject; DisplaySprite: TsgeDisplayElementSprite); reintroduce;

    property Scale: TsgeGUIPropertyScale read GetScale;
    property Sprite: TsgeSprite read GetSprite write SetSprite;
    property VerticalAlign: TsgeVerticalAlign read FVerticalAlign write SetVerticalAlign;
    property HorizontalAlign: TsgeHorizontalAlign read FHorizontalAlign write SetHorizontalAlign;
    property Reflect: TsgeReflectSet read GetReflect write SetReflect;
    property Color: TsgeColor read GetColor write SetColor;
  end;


  TsgeGUIPropertySpriteEx = class(TsgeGUIPropertySprite)
  public
    function GetRealPosAndSize: TsgeIntRect;
  end;



implementation

uses
  sgeGUIElement;

type
  TsgeGUIElementExt = class(TsgeGUIElement);


function TsgeGUIPropertySprite.GetScale: TsgeGUIPropertyScale;
begin
  Result := FScale;
end;


procedure TsgeGUIPropertySprite.SetVerticalAlign(AAlign: TsgeVerticalAlign);
begin
  if FVerticalAlign = AAlign then
    Exit;

  FVerticalAlign := AAlign;
  UpdateParent;
end;


procedure TsgeGUIPropertySprite.SetHorizontalAlign(AAlign: TsgeHorizontalAlign);
begin
  if FHorizontalAlign = AAlign then
    Exit;

  FHorizontalAlign := AAlign;
  UpdateParent;
end;


procedure TsgeGUIPropertySprite.SetReflect(AReflect: TsgeReflectSet);
begin
  if FDisplaySprite.Reflect = AReflect then
    Exit;

  FDisplaySprite.Reflect := AReflect;
  FDisplaySprite.Update;
end;


function TsgeGUIPropertySprite.GetReflect: TsgeReflectSet;
begin
  Result := FDisplaySprite.Reflect;
end;


procedure TsgeGUIPropertySprite.SetColor(AColor: TsgeColor);
begin
  FDisplaySprite.Color := AColor;
  FDisplaySprite.Update;
end;


function TsgeGUIPropertySprite.GetColor: TsgeColor;
begin
  Result := FDisplaySprite.Color;
end;


procedure TsgeGUIPropertySprite.SetSprite(ASprite: TsgeSprite);
begin
  FDisplaySprite.Sprite := ASprite;
  FDisplaySprite.Update;
end;


function TsgeGUIPropertySprite.GetSprite: TsgeSprite;
begin
  Result := FDisplaySprite.Sprite;
end;


constructor TsgeGUIPropertySprite.Create(AOwner: TObject; DisplaySprite: TsgeDisplayElementSprite);
begin
  inherited Create(AOwner);

  FScale := TsgeGUIPropertyScaleExt.Create(AOwner);
  FVerticalAlign := vaBottom;
  FHorizontalAlign := haCenter;

  FDisplaySprite := DisplaySprite;
end;


function TsgeGUIPropertySpriteEx.GetRealPosAndSize: TsgeIntRect;
var
  SpriteSize, ElementSize, ElementPos: TsgeIntPoint;
  Element: TsgeGUIElement;
begin
  //Ссылка на родителя
  Element := TsgeGUIElement(FOwner);

  //Реальный размер элемента
  ElementSize := Element.ScaleSize;

  //Реальное положение элемента
  ElementPos := TsgeGUIElementExt(Element).GetGlobalPos;

  //Получить размеры спрайта
  SpriteSize := FScale.GetSize(ElementSize.X, ElementSize.Y, FDisplaySprite.Sprite.Width, FDisplaySprite.Sprite.Height);

  //Размеры и положение
  Result.X2 := SpriteSize.X;
  Result.Y2 := SpriteSize.Y;
  Result.X1 := ElementPos.X + sgeGetHorizontalAlignOffset(FHorizontalAlign, ElementSize.X, SpriteSize.X);
  Result.Y1 := ElementPos.Y + sgeGetVerticaAlignOffset(FVerticalAlign, ElementSize.Y, SpriteSize.Y);
end;


end.

