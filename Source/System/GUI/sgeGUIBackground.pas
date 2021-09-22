{
Пакет             Simple Game Engine 2
Файл              sgeGUIBackground.pas
Версия            1.0
Создан            22.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Класс фона элемента
}
{$Include Defines.inc}

unit sgeGUIBackground;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicColor,
  sgeGUIProperty, sgeGUIBackgroundGradient;


type
  //Тип фона
  TsgeGUIBackgroundType = (btColor, btGradient, btSprite);


  TsgeGUIBackground = class(TsgeGUIProperty)
  private
    //Объекты
    FGradient: TsgeGUIBackgroundGradient;                           //Градиент

    //Свойства
    FType: TsgeGUIBackgroundType;                                   //Тип заполнения
    FColor: TsgeColor;                                              //Цвет фона

    procedure DrawColor;

    procedure SetType(AType: TsgeGUIBackgroundType);
    procedure SetColor(AColor: TsgeColor);
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    procedure Draw;

    property DrawType: TsgeGUIBackgroundType read FType write SetType;
    property Color: TsgeColor read FColor write SetColor;
    property Gradient: TsgeGUIBackgroundGradient read FGradient;
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


procedure TsgeGUIBackground.SetType(AType: TsgeGUIBackgroundType);
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

  FGradient := TsgeGUIBackgroundGradient.Create(AOwner);

  FType := btColor;
  FColor := cGray;
end;


destructor TsgeGUIBackground.Destroy;
begin
  FGradient.Free;

  inherited Destroy;
end;


procedure TsgeGUIBackground.Draw;
begin
  if FOwner = nil then Exit;

  //Нарисовать фон
  case FType of
    btColor   : DrawColor;
    btGradient: FGradient.Draw;
    btSprite  : ;
  end;
end;


end.

