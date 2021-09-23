{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyBackground.pas
Версия            1.0
Создан            22.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Фон элемента
}
{$Include Defines.inc}

unit sgeGUIPropertyBackground;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicColor,
  sgeGUIProperty, sgeGUIPropertyBackgroundGradient, sgeGUIPropertyBackgroundSprite;


type
  //Тип фона
  TsgeGUIPropertyBackgroundType = (pbtColor, pbtGradient, pbtSprite);


  TsgeGUIBackground = class(TsgeGUIProperty)
  private
    //Объекты
    FGradient: TsgeGUIPropertyBackgroundGradient;                   //Градиент
    FSprite: TsgeGUIPropertyBackgroundSprite;                       //Спрайт

    //Свойства
    FType: TsgeGUIPropertyBackgroundType;                           //Тип заполнения
    FColor: TsgeColor;                                              //Цвет фона

    procedure DrawColor;

    procedure SetType(AType: TsgeGUIPropertyBackgroundType);
    procedure SetColor(AColor: TsgeColor);
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    procedure Draw;

    property DrawType: TsgeGUIPropertyBackgroundType read FType write SetType;
    property Color: TsgeColor read FColor write SetColor;
    property Gradient: TsgeGUIPropertyBackgroundGradient read FGradient;
    property Sprite: TsgeGUIPropertyBackgroundSprite read FSprite;
  end;



implementation

uses
  sgeVars;


procedure TsgeGUIBackground.DrawColor;
begin
  with SGE.ExtGraphic.Graphic do
    begin
    BGColor := FColor;
    EraseBG;
    end;
end;


procedure TsgeGUIBackground.SetType(AType: TsgeGUIPropertyBackgroundType);
begin
  if FType = AType then Exit;

  FType := AType;
  RepaintParent;
end;


procedure TsgeGUIBackground.SetColor(AColor: TsgeColor);
begin
  FColor := AColor;
  RepaintParent;
end;


constructor TsgeGUIBackground.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FGradient := TsgeGUIPropertyBackgroundGradient.Create(AOwner);
  FSprite := TsgeGUIPropertyBackgroundSprite.Create(AOwner);

  FType := pbtColor;
  FColor := cGray;
end;


destructor TsgeGUIBackground.Destroy;
begin
  FSprite.Free;
  FGradient.Free;

  inherited Destroy;
end;


procedure TsgeGUIBackground.Draw;
begin
  if FOwner = nil then Exit;

  //Нарисовать фон
  case FType of
    pbtColor    : DrawColor;
    pbtGradient : FGradient.Draw;
    pbtSprite   : FSprite.Draw;
  end;
end;


end.

