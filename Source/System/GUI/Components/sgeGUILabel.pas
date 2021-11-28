{
Пакет             Simple Game Engine 2
Файл              sgeGUILabel.pas
Версия            1.0
Создан            22.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Метки
}
{$Include Defines.inc}

unit sgeGUILabel;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeSimpleParameters,
  sgeGUIElement,
  sgeGUIPropertyLabel, sgeGUIPropertyColor;


type
  TsgeGUILabel = class(TsgeGUIElement)
  private
    FLabel: TsgeGUIPropertyLabelExt;
    FBGColor: TsgeGUIPropertyColorExt;

    function GetLabel: TsgeGUIPropertyLabel;
    function GetBGColor: TsgeGUIPropertyColor;
  protected
    class function GetParameterSectionName: String; override;
    procedure LoadData(Data: TsgeSimpleParameters); override;
    procedure DrawBefore; override;
    procedure CalculateAutosize(var NewWidth, NewHeight: Integer); override;
    procedure CheckMinimalSize(var NewWidth, NewHeight: Integer); override;
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    property &Label: TsgeGUIPropertyLabel read GetLabel;
    property BGColor: TsgeGUIPropertyColor read GetBGColor;
  end;


implementation

uses
  sgeVars, sgeGraphicColor;


function TsgeGUILabel.GetLabel: TsgeGUIPropertyLabel;
begin
  Result := FLabel;
end;


function TsgeGUILabel.GetBGColor: TsgeGUIPropertyColor;
begin
  Result := FBGColor;
end;


class function TsgeGUILabel.GetParameterSectionName: String;
begin
  Result := 'Label';
end;


procedure TsgeGUILabel.LoadData(Data: TsgeSimpleParameters);
begin
  inherited LoadData(Data);

  //Label
  FLabel.LoadParameters(Data, 'Label.');

  //BGColor
  FBGColor.LoadParameters(Data, 'BGColor.');
end;


procedure TsgeGUILabel.DrawBefore;
begin
  //Вывод фона
  with SGE.ExtGraphic.Graphic do
    begin
    BGColor := FBGColor.Color;
    EraseBG;
    end;

  //Вывод метки
  FLabel.Draw;
end;


procedure TsgeGUILabel.CalculateAutosize(var NewWidth, NewHeight: Integer);
begin
  NewWidth := FLabel.Font.GetTextWidth(FLabel.Text);
  NewHeight := FLabel.Font.GetTextHeight(FLabel.Text);
end;


procedure TsgeGUILabel.CheckMinimalSize(var NewWidth, NewHeight: Integer);
var
  W, H: Integer;
begin
  W := FLabel.Font.GetTextWidth(FLabel.Text);
  H := FLabel.Font.GetTextHeight(FLabel.Text);

  if NewWidth < W then NewWidth := W;
  if NewHeight < H then NewHeight := H;
end;


constructor TsgeGUILabel.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  LockUpdate;

  FLabel := TsgeGUIPropertyLabelExt.Create(Self);
  FBGColor := TsgeGUIPropertyColorExt.Create(Self);
  FBGColor.Color := cTransparentBlack;

  UnLockUpdate;

  Repaint;
end;


destructor TsgeGUILabel.Destroy;
begin
  FLabel.Free;
  FBGColor.Free;

  inherited Destroy;
end;


end.

