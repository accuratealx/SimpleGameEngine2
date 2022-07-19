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
    FGraphicElement: TsgeGraphicElementSpriteCashed;                //Элемент отрисовки

    FAlpha: Single;                                                 //Прозрачность элемента
    FBackground: TsgeGUIPropertyBackgroundExt;                      //Фон

    function  GetBackground: TsgeGUIPropertyBackground;
  protected
    class function GetParameterSectionName: String; override;       //Имя секции
    procedure LoadData(Data: TsgeSimpleParameters); override;       //Загрузить параметры
    procedure DrawBefore; override;                                 //Отрисовать перед выводом детей

    procedure SetVisible(AVisible: Boolean); override;
    procedure SetAlpha(AAlpha: Single);
    procedure SetFocused(AFocused: Boolean); override;
    procedure SetScale(AScale: Single); override;
    procedure SetTop(ATop: Integer); override;
    procedure SetLeft(ALeft: Integer); override;

  public
    constructor Create(AName: String; ALeft, ATop, AWidth, AHeight: Integer; AParent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    procedure Draw; override;
    procedure BringToFront;

    property Background: TsgeGUIPropertyBackground read GetBackground;
    property Alpha: Single read FAlpha write SetAlpha;
    property Scale: Single read GetScale write SetScale;
  end;


implementation

uses
  sgeTypes, sgeCorePointerUtils;


procedure TsgeGUIForm.SetScale(AScale: Single);
begin
  inherited SetScale(AScale);

  //Поправить элемент отрисовки
  FGraphicElement.Scale := sgeGetFloatPoint(FScale, FScale);
  FGraphicElement.Update;
end;


procedure TsgeGUIForm.SetTop(ATop: Integer);
begin
  inherited SetTop(ATop);

  //Поправить элемент отрисовки
  FGraphicElement.Y := ATop;
  FGraphicElement.Update;
end;


procedure TsgeGUIForm.SetLeft(ALeft: Integer);
begin
  inherited SetLeft(ALeft);

  //Поправить элемент отрисовки
  FGraphicElement.X := ALeft;
  FGraphicElement.Update;
end;


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
  if Data.Exist[ParamName] then
    SetAlpha(Data.GetValue(ParamName, FAlpha));

  //Scale
  ParamName := 'Scale';
  if Data.Exist[ParamName] then
    SetScale(Data.GetValue(ParamName, FScale));

  //Background
  FBackground.LoadParameters(Data, 'Background.');
end;


procedure TsgeGUIForm.DrawBefore;
begin
  FBackground.Draw(sgeGetFloatRect(0, 0, FWidth, FHeight));
end;


procedure TsgeGUIForm.SetVisible(AVisible: Boolean);
begin
  inherited SetVisible(AVisible);

  FGraphicElement.Visible := AVisible;
end;


procedure TsgeGUIForm.SetAlpha(AAlpha: Single);
begin
  if AAlpha < 0 then
    AAlpha := 0;
  if AAlpha > 1 then
    AAlpha := 1;
  FAlpha := AAlpha;

  //Обновить графический элемент
  FGraphicElement.Alpha := AAlpha;
  FGraphicElement.Update;
end;


procedure TsgeGUIForm.SetFocused(AFocused: Boolean);
begin
  inherited SetFocused(AFocused);

  BringToFront;
end;


constructor TsgeGUIForm.Create(AName: String; ALeft, ATop, AWidth, AHeight: Integer; AParent: TsgeGUIElement);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight, AParent);

  //Задать параметры
  FAlpha := 1;
  FScale := 1;

  //Создать графический элемент
  FGraphicElement := TsgeGraphicElementSpriteCashed.Create(Left, Top, Width, Height, FCanvas);

  //Добавить элемент в список отрисовки
  sgeCorePointer_GetSGE.ExtGraphic.LayerList.AddElement(FGraphicElement, Graphic_Layer_System_GUI);

  //Создать свойство фона
  FBackground := TsgeGUIPropertyBackgroundExt.Create(Self);

  //Перерисовать форму
  Repaint;

  //Добавить себя в список форм
  sgeCorePointer_GetSGE.ExtGUI.FormList.Add(Self);
end;


destructor TsgeGUIForm.Destroy;
begin
  Include(FState, esLockUpdate);

  //Удалить себя из списка форм
  sgeCorePointer_GetSGE.ExtGUI.FormList.Delete(Self);

  //Удалить примитив
  FGraphicElement.Visible := False;
  FGraphicElement.Delete;

  //Удалить свойство фона
  FBackground.Free;

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


procedure TsgeGUIForm.BringToFront;
begin
  //Изменить Z-Index в списке форм
  sgeCorePointer_GetSGE.ExtGUI.FormList.ToTopIndex(Self);

  //Поместить графический элемент в конец списка
  sgeCorePointer_GetSGE.ExtGraphic.LayerList.MoveElementToListEnd(FGraphicElement, Graphic_Layer_System_GUI);
end;



end.

