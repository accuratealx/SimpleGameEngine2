{
Пакет             Simple Game Engine 2
Файл              sgeGUIForm.pas
Версия            1.0
Создан            27.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Форма
}
{$Include Defines.inc}

unit sgeGUIForm;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeGUIElement,
  sgeDisplayElementRect;

type
  TsgeGUIForm = class(TsgeGUIElement)
  private
    FDisplayElement: TsgeDisplayElementRect;

  protected
    procedure DisplayElement_CorrectPosition(RealLeft, RealTop: Integer); override;
    procedure DisplayElement_CorrectSize(Width, Height: Integer); override;
    procedure DisplayElement_CorrectVisible(Visible: Boolean); override;
    function  DisplayElement_GetVisible: Boolean; override;

  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Visible: Boolean = True);
    destructor  Destroy; override;
  end;



implementation

uses
  sgeCorePointerUtils,
  sgeGraphicColor;


procedure TsgeGUIForm.DisplayElement_CorrectPosition(RealLeft, RealTop: Integer);
begin
  FDisplayElement.PositionX := RealLeft;
  FDisplayElement.PositionY := RealTop;
  FDisplayElement.Update;
end;


procedure TsgeGUIForm.DisplayElement_CorrectSize(Width, Height: Integer);
begin
  FDisplayElement.Width := Width;
  FDisplayElement.Height := Height;
  FDisplayElement.Update;
end;


procedure TsgeGUIForm.DisplayElement_CorrectVisible(Visible: Boolean);
begin
  FDisplayElement.Visible := Visible;
end;


function TsgeGUIForm.DisplayElement_GetVisible: Boolean;
begin
  Result := FDisplayElement.Visible;
end;


constructor TsgeGUIForm.Create(Name: String; Left, Top, Width, Height: Integer; Visible: Boolean);
begin
  FDisplayElement := TsgeDisplayElementRect.Create(Left, Top, Left + Width, Top + Height, cBlack);
  FDisplayElement.Add(Layer_GUI_Name);

  inherited Create(Name, Left, Top, Width, Height, Visible);

  //Добавить себя в список форм
  sgeCorePointer_GetSGE.ExtGUI.FormList.Add(Self);
end;


destructor TsgeGUIForm.Destroy;
begin
  //Удалить себя из списка форм
  sgeCorePointer_GetSGE.ExtGUI.FormList.Delete(Self);

  //Удалить элемент отображения
  FDisplayElement.Delete;
  FDisplayElement.Free;

  inherited Destroy;
end;



end.

