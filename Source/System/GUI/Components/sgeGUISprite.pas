{
Пакет             Simple Game Engine 2
Файл              sgeGUISprite.pas
Версия            1.0
Создан            10.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Спрайт
}
{$Include Defines.inc}

unit sgeGUISprite;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeSimpleParameters,
  sgeGUIElement, sgeGUIPropertySprite;


type
  TsgeGUISprite = class(TsgeGUIElement)
  private
    FSprite: TsgeGUIPropertySpriteExt;

    function GetSprite: TsgeGUIPropertySprite;
  protected
    class function GetParameterSectionName: String; override;
    procedure LoadData(Data: TsgeSimpleParameters); override;
    procedure DrawBefore; override;
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    property Sprite: TsgeGUIPropertySprite read GetSprite;
  end;



implementation

uses
  sgeCorePointerUtils,
  sgeGUIPropertyScaleXY;


function TsgeGUISprite.GetSprite: TsgeGUIPropertySprite;
begin
  Result := FSprite;
end;


class function TsgeGUISprite.GetParameterSectionName: String;
begin
  Result := 'Sprite';
end;


procedure TsgeGUISprite.LoadData(Data: TsgeSimpleParameters);
begin
  inherited LoadData(Data);

  //Sprite
  FSprite.LoadParameters(Data, 'Sprite.');
end;


procedure TsgeGUISprite.DrawBefore;
begin
  FSprite.Draw;
end;


constructor TsgeGUISprite.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  FSprite := TsgeGUIPropertySpriteExt.Create(Self);
  FSprite.LockUpdate;
  FSprite.Scale.Mode := smStretch;
  FSprite.UnlockUpdate;
  FSprite.Sprite := sgeCorePointer_GetSGE.ExtResourceList.Default.Sprite;

  Repaint;
end;


destructor TsgeGUISprite.Destroy;
begin
  FSprite.Free;

  inherited Destroy;
end;


end.

