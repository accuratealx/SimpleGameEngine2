{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyBackground.pas
Версия            1.1
Создан            22.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Фон элемента
}
{$Include Defines.inc}

unit sgeGUIPropertyBackground;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGUIProperty, sgeGUIPropertyColor, sgeGUIPropertyGradient, sgeGUIPropertySprite;


type
  //Тип фона
  TsgeGUIPropertyBackgroundType = (pbtColor, pbtGradient, pbtSprite);


  TsgeGUIPropertyBackground = class(TsgeGUIProperty)
  private
    //Объекты
    FColor: TsgeGUIPropertyColorExt;                                //Цвет
    FGradient: TsgeGUIPropertyGradientExt;                          //Градиент
    FSprite: TsgeGUIPropertySpriteExt;                              //Спрайт

    //Свойства
    FType: TsgeGUIPropertyBackgroundType;                           //Тип заполнения

    procedure SetType(AType: TsgeGUIPropertyBackgroundType);

    function  GetColor: TsgeGUIPropertyColor;
    function  GetGradient: TsgeGUIPropertyGradient;
    function  GetSprite: TsgeGUIPropertySprite;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property DrawType: TsgeGUIPropertyBackgroundType read FType write SetType;
    property Color: TsgeGUIPropertyColor read GetColor;
    property Gradient: TsgeGUIPropertyGradient read GetGradient;
    property Sprite: TsgeGUIPropertySprite read GetSprite;
  end;


  TsgeGUIBackgroundExt = class(TsgeGUIPropertyBackground)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
    procedure Draw;
  end;


implementation




procedure TsgeGUIPropertyBackground.SetType(AType: TsgeGUIPropertyBackgroundType);
begin
  if FType = AType then
    Exit;

  FType := AType;
  UpdateParent;
end;


function TsgeGUIPropertyBackground.GetColor: TsgeGUIPropertyColor;
begin
  Result := FColor;
end;


function TsgeGUIPropertyBackground.GetGradient: TsgeGUIPropertyGradient;
begin
  Result := FGradient;
end;


function TsgeGUIPropertyBackground.GetSprite: TsgeGUIPropertySprite;
begin
  Result := FSprite;
end;


constructor TsgeGUIPropertyBackground.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FColor := TsgeGUIPropertyColorExt.Create(AOwner);
  FGradient := TsgeGUIPropertyGradientExt.Create(AOwner);
  FSprite := TsgeGUIPropertySpriteExt.Create(AOwner);

  FType := pbtColor;
end;


destructor TsgeGUIPropertyBackground.Destroy;
begin
  FSprite.Free;
  FGradient.Free;
  FColor.Free;

  inherited Destroy;
end;


procedure TsgeGUIBackgroundExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName, s: String;
begin
  //Type
  ParamName := Prefix + 'Type';
  if Parameters.Exist[ParamName] then
  begin
    s := LowerCase(Parameters.GetValue(ParamName, ''));
    case s of
      'color':
        FType := pbtColor;

      'gradient':
        FType := pbtGradient;
      'sprite':
        FType := pbtSprite;
    end;
  end;

  //Color
  FColor.LoadParameters(Parameters, Prefix);

  //Gradient
  FGradient.LoadParameters(Parameters, Prefix + 'Gradient.');

  //Sprite
  FSprite.LoadParameters(Parameters, Prefix + 'Sprite.');
end;


procedure TsgeGUIBackgroundExt.Draw;
begin
  if FOwner = nil then Exit;

  case FType of
    pbtColor:
      FColor.Draw;

    pbtGradient:
      FGradient.Draw;

    pbtSprite:
      FSprite.Draw;
  end;
end;



end.

