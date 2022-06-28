{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertySprite.pas
Версия            1.1
Создан            23.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Спрайт
}
{$Include Defines.inc}

unit sgeGUIPropertySprite;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGraphicSprite,
  sgeGUIProperty, sgeGUIPropertyScaleXY, sgeGUIPropertyHorizontalAlign, sgeGUIPropertyVerticalAlign,
  sgeGUIPropertySpriteRect, sgeGUIPropertyDrawMethod;


type
  TsgeGUIPropertySprite = class(TsgeGUIProperty)
  private
    FSprite: TsgeGraphicSprite;                                     //Ссылка на спрайт

    FScale: TsgeGUIPropertyScaleXYExt;                              //Масштаб
    FHorizontalAlign: TsgeGUIPropertyHorizontalAlignExt;            //Выравнивание по X
    FVerticalAlign: TsgeGUIPropertyVerticalAlignExt;                //Выравнивание по Y
    FDrawMode: TsgeGUIPropertySpriteRectExt;                        //Часть спрайта
    FDrawMethod: TsgeGUIPropertyDrawMethodExt;                      //Метод вывода спрайта

    procedure SetSprite(ASprite: TsgeGraphicSprite);

    function  GetScale: TsgeGUIPropertyScaleXY;
    function  GetHorizontalAlign: TsgeGUIPropertyHorizontalAlign;
    function  GetVerticalAlign: TsgeGUIPropertyVerticalAlign;
    function  GetDrawMode: TsgeGUIPropertySpriteRect;
    function  GetDrawMethod: TsgeGUIPropertyDrawMethod;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property Scale: TsgeGUIPropertyScaleXY read GetScale;
    property HorizontalAlign: TsgeGUIPropertyHorizontalAlign read GetHorizontalAlign;
    property VerticalAlign: TsgeGUIPropertyVerticalAlign read GetVerticalAlign;
    property DrawMode: TsgeGUIPropertySpriteRect read GetDrawMode;
    property DrawMethod: TsgeGUIPropertyDrawMethod read GetDrawMethod;

    property Sprite: TsgeGraphicSprite read FSprite write SetSprite;
  end;


  TsgeGUIPropertySpriteExt = class(TsgeGUIPropertySprite)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
    procedure Draw;
  end;


implementation

uses
  sgeCorePointerUtils,
  sgeTypes, sgeGraphic,
  sgeGUIElement;

type
  TsgeGUIElementHack = class(TsgeGUIElement);


procedure TsgeGUIPropertySprite.SetSprite(ASprite: TsgeGraphicSprite);
begin
  if FSprite = ASprite then
    Exit;

  FSprite := ASprite;
  UpdateParent;
end;


function TsgeGUIPropertySprite.GetScale: TsgeGUIPropertyScaleXY;
begin
  Result := FScale;
end;


function TsgeGUIPropertySprite.GetHorizontalAlign: TsgeGUIPropertyHorizontalAlign;
begin
  Result := FHorizontalAlign;
end;


function TsgeGUIPropertySprite.GetVerticalAlign: TsgeGUIPropertyVerticalAlign;
begin
  Result := FVerticalAlign;
end;


function TsgeGUIPropertySprite.GetDrawMode: TsgeGUIPropertySpriteRect;
begin
  Result := FDrawMode;
end;


function TsgeGUIPropertySprite.GetDrawMethod: TsgeGUIPropertyDrawMethod;
begin
  Result := FDrawMethod;
end;


constructor TsgeGUIPropertySprite.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  //Создать свойства
  FScale := TsgeGUIPropertyScaleXYExt.Create(AOwner);
  FHorizontalAlign := TsgeGUIPropertyHorizontalAlignExt.Create(AOwner);
  FVerticalAlign := TsgeGUIPropertyVerticalAlignExt.Create(AOwner);
  FDrawMode := TsgeGUIPropertySpriteRectExt.Create(AOwner);
  FDrawMethod := TsgeGUIPropertyDrawMethodExt.Create(AOwner);

  //Задать параметры
  FSprite := nil;
end;


destructor TsgeGUIPropertySprite.Destroy;
begin
  FDrawMethod.Free;
  FDrawMode.Free;
  FVerticalAlign.Free;
  FHorizontalAlign.Free;
  FScale.Free;

  inherited Destroy;
end;


procedure TsgeGUIPropertySpriteExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName: String;
begin
  //Sprite
  ParamName := Prefix + 'Name';
  if Parameters.Exist[ParamName] then
    FSprite := sgeCorePointer_GetSGE.ExtResourceList.GetSprite(Parameters.GetValue(ParamName, 'Sprite'));

  //Scale
  FScale.LoadParameters(Parameters, Prefix + 'Scale.');

  //HorizontalAlign
  FHorizontalAlign.LoadParameters(Parameters, Prefix + 'HorizontalAlign.');

  //VerticalAlign
  FVerticalAlign.LoadParameters(Parameters, Prefix + 'VerticalAlign.');

  //DrawMode
  FDrawMode.LoadParameters(Parameters, Prefix + 'DrawMode.');

  //DrawMethod
  FDrawMethod.LoadParameters(Parameters, Prefix + 'DrawMethod.');
end;


procedure TsgeGUIPropertySpriteExt.Draw;
var
  DrawOpt: TsgeGraphicDrawOptions;
  BaseWidth, BaseHeight: Integer;
  Size: TsgeIntPoint;
begin
  if FSprite = nil then
    Exit;

  //Определить размеры элемента
  BaseWidth := TsgeGUIElementHack(FOwner).FWidth;
  BaseHeight := TsgeGUIElementHack(FOwner).FHeight;

  //Подготовить стандартныйе настройки вывода
  DrawOpt := DefaultDrawOptions;

  //Установить спрайт
  DrawOpt.Sprite := FSprite;
  DrawOpt.CoordinateType := gctClassic;

  //Определить размеры вывода
  Size := FScale.GetSize(BaseWidth, BaseHeight, FSprite.Width, FSprite.Height);

  //Поправить область вывода
  DrawOpt.Rect.X1 := FHorizontalAlign.GetOffset(BaseWidth, Size.X);
  DrawOpt.Rect.Y1 := FVerticalAlign.GetOffset(BaseHeight, Size.Y);
  DrawOpt.Rect.X2 := DrawOpt.Rect.X1 + Size.X;
  DrawOpt.Rect.Y2 := DrawOpt.Rect.Y1 + Size.Y;

  //Поправить область вывода спрайта
  DrawOpt.SpriteRect := FDrawMode.GetRect(FSprite);

  //Вывод в зависимости от метода
  case FDrawMethod.Mode of
    dmmNormal:
      sgeCorePointer_GetSGE.ExtGraphic.Graphic.DrawSprite(DrawOpt);

    dmmSegment:
      sgeCorePointer_GetSGE.ExtGraphic.Graphic.DrawSpriteSegment(DrawOpt, FDrawMethod.Offset.Rect);
  end;
end;



end.

