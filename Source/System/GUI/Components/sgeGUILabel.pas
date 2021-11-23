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
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    property &Label: TsgeGUIPropertyLabel read GetLabel;
    property BGColor: TsgeGUIPropertyColor read GetBGColor;
  end;


implementation

uses
  sgeVars;


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


constructor TsgeGUILabel.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  FLabel := TsgeGUIPropertyLabelExt.Create(Self);
  FBGColor := TsgeGUIPropertyColorExt.Create(Self);

  Repaint;
end;


destructor TsgeGUILabel.Destroy;
begin
  FLabel.Free;
  FBGColor.Free;

  inherited Destroy;
end;


end.

