{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyLabel.pas
Версия            1.0
Создан            22.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Надпись
}
{$Include Defines.inc}

unit sgeGUIPropertyLabel;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGUIProperty, sgeGUIPropertyFont, sgeGUIPropertyColor, sgeGUIPropertyHorizontalAlign, sgeGUIPropertyVerticalAlign;


type
  TsgeGUIPropertyLabel = class(TsgeGUIProperty)
  private
    FText: String;
    FFont: TsgeGUIPropertyFontExt;
    FColor: TsgeGUIPropertyColorExt;
    FHorizontalAlign: TsgeGUIPropertyHorizontalAlignExt;
    FVerticalAlign: TsgeGUIPropertyVerticalAlignExt;

    procedure SetText(AText: String);
    function  GetFont: TsgeGUIPropertyFont;
    function  GetColor: TsgeGUIPropertyColor;
    function  GetHorizontalAlign: TsgeGUIPropertyHorizontalAlign;
    function  GetVerticalAlign: TsgeGUIPropertyVerticalAlign;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property Text: String read FText write SetText;
    property Font: TsgeGUIPropertyFont read GetFont;
    property Color: TsgeGUIPropertyColor read GetColor;
    property HorizontalAlign: TsgeGUIPropertyHorizontalAlign read GetHorizontalAlign;
    property VerticalAlign: TsgeGUIPropertyVerticalAlign read GetVerticalAlign;
  end;


  TsgeGUIPropertyLabelExt = class(TsgeGUIPropertyLabel)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
    procedure Draw;
  end;


implementation

uses
  sgeVars,
  sgeGUIElement;

type
  TsgeGUIElementExt = class(TsgeGUIElement);


procedure TsgeGUIPropertyLabel.SetText(AText: String);
begin
  if FText = AText then
    Exit;

  FText := AText;
  UpdateParent;
end;


function TsgeGUIPropertyLabel.GetFont: TsgeGUIPropertyFont;
begin
  Result := FFont;
end;


function TsgeGUIPropertyLabel.GetColor: TsgeGUIPropertyColor;
begin
  Result := FColor;
end;


function TsgeGUIPropertyLabel.GetHorizontalAlign: TsgeGUIPropertyHorizontalAlign;
begin
  Result := FHorizontalAlign;
end;


function TsgeGUIPropertyLabel.GetVerticalAlign: TsgeGUIPropertyVerticalAlign;
begin
  Result := FVerticalAlign;
end;


constructor TsgeGUIPropertyLabel.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FFont := TsgeGUIPropertyFontExt.Create(AOwner);
  FColor := TsgeGUIPropertyColorExt.Create(AOwner);
  FHorizontalAlign := TsgeGUIPropertyHorizontalAlignExt.Create(AOwner);
  FVerticalAlign := TsgeGUIPropertyVerticalAlignExt.Create(AOwner);
end;


destructor TsgeGUIPropertyLabel.Destroy;
begin
  FFont.Free;
  FColor.Free;
  FHorizontalAlign.Free;
  FVerticalAlign.Free;

  inherited Destroy;
end;



procedure TsgeGUIPropertyLabelExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName: String;
begin
  //Text
  ParamName := Prefix + 'Text';
  if Parameters.Exist[ParamName] then
    FText := Parameters.GetValue(ParamName, '');

  //Font
  FFont.LoadParameters(Parameters, Prefix + 'Font.');

  //Color
  FColor.LoadParameters(Parameters, Prefix + 'Color.');

  //HorizontalAlign
  FHorizontalAlign.LoadParameters(Parameters, Prefix + 'HorizontalAlign.');

  //VerticalAlign
  FVerticalAlign.LoadParameters(Parameters, Prefix + 'VerticalAlign.');
end;


procedure TsgeGUIPropertyLabelExt.Draw;
var
  BaseWidth, BaseHeight, TextWidth, TextHeight: Integer;
  X1, Y1: Single;
begin
  //Определить размеры элемента
  BaseWidth := TsgeGUIElementExt(FOwner).FWidth;
  BaseHeight := TsgeGUIElementExt(FOwner).FHeight;

  //Размеры текста
  TextWidth := FFont.Font.GetStringWidth(FText);
  TextHeight := FFont.Font.GetStringHeight(FText);

  //Координаты вывода
  X1 := FHorizontalAlign.GetOffset(BaseWidth, TextWidth);
  Y1 := FVerticalAlign.GetOffset(BaseHeight, TextHeight);

  //Вывод
  with SGE.ExtGraphic.Graphic do
  begin
    //Изменить цвет текста
    Color := FColor.Color;

    //Вывод текста
    DrawText(X1, Y1, FFont.Font, FText);
  end;
end;



end.

