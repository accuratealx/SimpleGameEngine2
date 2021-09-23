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
  sgeGUIElement, sgeGraphicElementSpriteCashed,
  sgeGUIPropertyBackground;


type
  TsgeGUIForm = class(TsgeGUIElement)
  private
    FGraphicElement: TsgeGraphicElementSpriteCashed;                //Элемент отрисовки

    FBackground: TsgeGUIBackground;                                 //Фон

  protected
    procedure DrawBefore; override;
    procedure SetVisible(AVisible: Boolean); override;
    procedure SetAlpha(AAlpha: Single); override;
  public
    constructor Create(AName: String; ALeft, ATop, AWidth, AHeight: Integer; AParent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    procedure Draw; override;

    property Background: TsgeGUIBackground read FBackground;
  end;


implementation

uses
  sgeVars, sgeGraphicColor, sgeOSPlatform;



procedure TsgeGUIForm.DrawBefore;
begin
  FBackground.Draw;
end;


procedure TsgeGUIForm.SetVisible(AVisible: Boolean);
begin
  inherited SetVisible(AVisible);

  FGraphicElement.Visible := AVisible;
end;


procedure TsgeGUIForm.SetAlpha(AAlpha: Single);
begin
  inherited SetAlpha(AAlpha);

  FGraphicElement.Alpha := AAlpha;
  FGraphicElement.Update;
end;


constructor TsgeGUIForm.Create(AName: String; ALeft, ATop, AWidth, AHeight: Integer; AParent: TsgeGUIElement);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight, AParent);

  //Создать графический элемент
  FGraphicElement := TsgeGraphicElementSpriteCashed.Create(Left, Top, Width, Height, FCanvas);

  //Добавить элемент в список отрисовки
  SGE.ExtGraphic.DrawList.AddElement(FGraphicElement, 'GUI');

  //Создать свойство фона
  FBackground := TsgeGUIBackground.Create(Self);

  //Перерисовать форму
  Repaint;

  //Добавить себя в список форм
  SGE.ExtGUI.FormList.Add(Self);
end;


destructor TsgeGUIForm.Destroy;
begin
  //Спрятать форму
  SetVisible(False);

  //Удалить свойство фона
  FBackground.Free;

  //Удалить себя из списока форм
  SGE.ExtGUI.FormList.Delete(Self);

  inherited Destroy;
end;


procedure TsgeGUIForm.Draw;
begin
  inherited Draw;

  if FGraphicElement <> nil then FGraphicElement.Update;
end;

end.

