{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyColor.pas
Версия            1.0
Создан            30.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Цвет
}
{$Include Defines.inc}

unit sgeGUIPropertyColor;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGraphicColor,
  sgeGUIProperty;

type
  TsgeGUIPropertyColor = class(TsgeGUIProperty)
  private
    FColor: TsgeColor;

    procedure CheckValue(var Value: Single);

    procedure SetColor(AColor: TsgeColor);
    procedure SetRed(ARed: Single);
    procedure SetGreen(AGreen: Single);
    procedure SetBlue(ABlue: Single);
    procedure SetAlpha(AAlpha: Single);
  public
    constructor Create(AOwner: TObject); override;

    property Color: TsgeColor read FColor write SetColor;
    property Red: Single read FColor.Red write SetRed;
    property Green: Single read FColor.Green write SetGreen;
    property Blue: Single read FColor.Blue write SetBlue;
    property Alpha: Single read FColor.Alpha write SetAlpha;
  end;


  TsgeGUIPropertyColorExt = class(TsgeGUIPropertyColor)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
    procedure Draw;
  end;


implementation

uses
  sgeVars,
  sgeGraphicColorUtils;


procedure TsgeGUIPropertyColor.CheckValue(var Value: Single);
begin
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;
end;


procedure TsgeGUIPropertyColor.SetColor(AColor: TsgeColor);
begin
  FColor := AColor;

  //Поправить границы цвета
  CheckValue(FColor.Red);
  CheckValue(FColor.Green);
  CheckValue(FColor.Blue);
  CheckValue(FColor.Alpha);

  //Оновить владельца
  UpdateParent;
end;


procedure TsgeGUIPropertyColor.SetRed(ARed: Single);
begin
  FColor.Red := ARed;
  CheckValue(FColor.Red);
  UpdateParent;
end;


procedure TsgeGUIPropertyColor.SetGreen(AGreen: Single);
begin
  FColor.Green := AGreen;
  CheckValue(FColor.Green);
  UpdateParent;
end;


procedure TsgeGUIPropertyColor.SetBlue(ABlue: Single);
begin
  FColor.Blue := ABlue;
  CheckValue(FColor.Blue);
  UpdateParent;
end;


procedure TsgeGUIPropertyColor.SetAlpha(AAlpha: Single);
begin
  FColor.Alpha := AAlpha;
  CheckValue(FColor.Alpha);
  UpdateParent;
end;


constructor TsgeGUIPropertyColor.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FColor := CBlack;
end;


procedure TsgeGUIPropertyColorExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName, s: String;
begin
  ParamName := Prefix + 'Color';
  if Parameters.Exist[ParamName] then
    begin
    s := Parameters.GetValue(ParamName, '0.0.0.255');
    FColor := sgeRGBAToColor(sgeStringToRGBA(s));
    end;
end;


procedure TsgeGUIPropertyColorExt.Draw;
begin
  with SGE.ExtGraphic.Graphic do
    begin
    BGColor := FColor;
    EraseBG;
    end;
end;



end.

