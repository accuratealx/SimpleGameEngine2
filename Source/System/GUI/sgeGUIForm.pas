{
Пакет             Simple Game Engine 2
Файл              sgeGUIForm.pas
Версия            1.0
Создан            04.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Форма
}
{$Include Defines.inc}

unit sgeGUIForm;

{$mode objfpc}{$H+}

interface

uses
  sgeGUIElement, sgeSimpleParameters,
  sgeGraphicElementSpriteCashed,
  sgeGUIPropertyBackground;


type
  TsgeGUIForm = class(TsgeGUIElement)
  private
    FAlpha: Single;                                                 //Прозрачность элемента

    FGraphicElement: TsgeGraphicElementSpriteCashed;                //Элемент отрисовки

    FBackground: TsgeGUIBackgroundExt;                              //Фон

    function GetBackground: TsgeGUIPropertyBackground;
  protected
    class function GetParameterSectionName: String; override;       //Имя секции
    procedure LoadData(Data: TsgeSimpleParameters); override;       //Загрузить параметры
    procedure DrawBefore; override;                                 //Отрисовать перед выводом детей

    procedure SetVisible(AVisible: Boolean); override;
    procedure SetAlpha(AAlpha: Single);
    procedure SetFocused(AFocused: Boolean); override;
  public
    constructor Create(AName: String; ALeft, ATop, AWidth, AHeight: Integer; AParent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    procedure Draw; override;

    property Alpha: Single read FAlpha write SetAlpha;

    property Background: TsgeGUIPropertyBackground read GetBackground;
  end;


implementation

uses
  sgeVars;

const
  GUILayer = 'GUI';


function TsgeGUIForm.GetBackground: TsgeGUIPropertyBackground;
begin
  Result := FBackground;
end;


class function TsgeGUIForm.GetParameterSectionName: String;
begin
  Result := 'Form'
end;


procedure TsgeGUIForm.LoadData(Data: TsgeSimpleParameters);
var
  ParamName: String;
begin
  inherited LoadData(Data);

  //Alpha
  ParamName := 'Alpha';
  if Data.Exist[ParamName] then SetAlpha(Data.GetValue(ParamName, 1.0));

  //Background
  FBackground.LoadParameters(Data, 'Background.');
end;


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
  if AAlpha < 0 then AAlpha := 0;
  if AAlpha > 1 then AAlpha := 1;
  FAlpha := AAlpha;

  Repaint;

  //Обновить графический элемент
  FGraphicElement.Alpha := AAlpha;
  FGraphicElement.Update;
end;


procedure TsgeGUIForm.SetFocused(AFocused: Boolean);
begin
  inherited SetFocused(AFocused);

  //Изменить Z-Index в списке форм
  SGE.ExtGUI.FormList.ToTopIndex(Self);

  //Поместить графический элемент в конец списка
  SGE.ExtGraphic.LayerList.MoveElementToListEnd(FGraphicElement, GUILayer);
end;


constructor TsgeGUIForm.Create(AName: String; ALeft, ATop, AWidth, AHeight: Integer; AParent: TsgeGUIElement);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight, AParent);

  //Задать параметры
  FAlpha := 1;

  //Создать графический элемент
  FGraphicElement := TsgeGraphicElementSpriteCashed.Create(Left, Top, Width, Height, FCanvas);

  //Добавить элемент в список отрисовки
  SGE.ExtGraphic.LayerList.AddElement(FGraphicElement, GUILayer);

  //Создать свойство фона
  FBackground := TsgeGUIBackgroundExt.Create(Self);

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

  if FGraphicElement <> nil then
    begin
    FGraphicElement.X := FLeft;
    FGraphicElement.Y := FTop;
    FGraphicElement.W := FWidth;
    FGraphicElement.H := FHeight;
    FGraphicElement.Update;
    end;
end;



end.

