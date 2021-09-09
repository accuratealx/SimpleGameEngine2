{
Пакет             Simple Game Engine 2
Файл              sgeGUIForm.pas
Версия            1.0
Создан            04.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Форма
}
{$Include Defines.inc}
{$ModeSwitch duplicatelocals+}

unit sgeGUIForm;

{$mode objfpc}{$H+}

interface

uses
  sgeGUIElement, sgeGraphicElementSpriteCashed;


type
  TsgeGUIForm = class(TsgeGUIElement)
  private
    FGraphicElement: TsgeGraphicElementSpriteCashed;

  protected
    procedure DrawBefore; override;

  public
    constructor Create(AName: String; ALeft, ATop, AWidth, AHeight: Integer; AParent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    procedure Draw; override;
  end;


implementation

uses
  sgeVars, sgeGraphicColor;



procedure TsgeGUIForm.DrawBefore;
begin
  with SGE.ExtGraphic.Graphic do
    begin
    BGColor := cRed;
    EraseBG;
    end;
end;


constructor TsgeGUIForm.Create(AName: String; ALeft, ATop, AWidth, AHeight: Integer; AParent: TsgeGUIElement);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight, AParent);

  //Создать графический элемент
  FGraphicElement := TsgeGraphicElementSpriteCashed.Create(Left, Top, Width, Height, FCanvas);
  SGE.ExtGraphic.DrawList.AddElement(FGraphicElement, 'GUI');

  //Перерисовать форму
  Repaint;
end;


destructor TsgeGUIForm.Destroy;
begin
  //Удалить графический элемент
  FGraphicElement.Delete;

  inherited Destroy;
end;


procedure TsgeGUIForm.Draw;
begin
  inherited Draw;

  if FGraphicElement <> nil then FGraphicElement.Update;
end;

end.

