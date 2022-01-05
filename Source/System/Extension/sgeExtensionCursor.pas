{
Пакет             Simple Game Engine 2
Файл              sgeExtensionCursor.pas
Версия            1.0
Создан            03.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Курсоры
}
{$Include Defines.inc}

unit sgeExtensionCursor;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeExtensionBase, sgeExtensionWindow, sgeExtensionGraphic, sgeExtensionResourceList,
  sgeGraphicElementAnimation,
  sgeEventBase, sgeEventMouse,
  sgeCursor;


const
  Extension_Cursor = 'Cursor';


type
  TsgeExtensionCursor = class(TsgeExtensionBase)
  private
    FExtWindow: TsgeExtensionWindow;                                //Ссылка на расширение окна
    FExtGraphic: TsgeExtensionGraphic;                              //Ссылка на расширение графики
    FExtResList: TsgeExtensionResourceList;                         //Ссылка на рисширение ресурсов

    FGUIElement: TsgeGraphicElementAnimation;                       //Отрисовка анимации
    FShowCursor: Boolean;                                           //Показывать курсор
    FCursor: TsgeCursor;                                            //Текущий курсор
    FCursorPos: TsgeIntPoint;                                       //Последние координаты курсора

    procedure SetShowCursor(AShow: Boolean);
    procedure SetCursor(ACursor: TsgeCursor);

    procedure CorrectVisible(Visible: Boolean);
    procedure CorrectCoordinate;

    function Handler_MouseMove(EventObj: TsgeEventMouse): Boolean;
    function Handler_MouseEnter(EventObj: TsgeEventMouse): Boolean;
    function Handler_MouseLeave(EventObj: TsgeEventMouse): Boolean;
  protected
    class function GetName: String; override;

    procedure RegisterEventHandlers; override;

  public
    constructor Create(ObjectList: TObject); override;

    property ShowCursor: Boolean read FShowCursor write SetShowCursor;
    property Cursor: TsgeCursor read FCursor write SetCursor;
  end;


implementation

uses
  sgeErrors;


const
  _UNITNAME = 'ExtensionCursors';


procedure TsgeExtensionCursor.SetShowCursor(AShow: Boolean);
begin
  if FShowCursor = AShow then Exit;

  FShowCursor := AShow;
  CorrectVisible(FShowCursor);
end;


procedure TsgeExtensionCursor.SetCursor(ACursor: TsgeCursor);
begin
  if FCursor = ACursor then Exit;

  FCursor := ACursor;

  //Поправить параметры курсора
  if FCursor <> nil then
    begin
    FGUIElement.Frames := FCursor.Frames;
    FGUIElement.W := FCursor.Width;
    FGUIElement.H := FCursor.Height;
    CorrectCoordinate;
    FGUIElement.Update;
    end;

  //Поправить видимость
  CorrectVisible(FShowCursor);
end;


procedure TsgeExtensionCursor.CorrectVisible(Visible: Boolean);
begin
  case Visible of

    True:
      if FCursor = nil then
        begin
        FExtWindow.ShowCursor := True;
        FGUIElement.Visible := False;
        end
        else begin
        FExtWindow.ShowCursor := False;
        FGUIElement.Visible := True;
        end;

    False:
      begin
      FExtWindow.ShowCursor := False;
      FGUIElement.Visible := False;
      end;

  end;
end;


procedure TsgeExtensionCursor.CorrectCoordinate;
begin
  if FCursor <> nil then
    begin
    FGUIElement.X := FCursorPos.X - FCursor.HotPoint.X;
    FGUIElement.Y := FCursorPos.Y - FCursor.HotPoint.Y;
    end;
end;


function TsgeExtensionCursor.Handler_MouseMove(EventObj: TsgeEventMouse): Boolean;
begin
  Result := False;

  //Запомнить последние координаты курсора
  FCursorPos := EventObj.Pos;

  //Поправить графический примитив
  CorrectCoordinate;
  FGUIElement.Update;
end;


function TsgeExtensionCursor.Handler_MouseEnter(EventObj: TsgeEventMouse): Boolean;
begin
  Result := False;

  //Восстановить курсор
  CorrectVisible(FShowCursor);
end;


function TsgeExtensionCursor.Handler_MouseLeave(EventObj: TsgeEventMouse): Boolean;
begin
  Result := False;

  //Спрятать курсоры
  CorrectVisible(False);

  //Показать сиситемный если он скрыт
  if not FExtWindow.ShowCursor then
    FExtWindow.ShowCursor := true;
end;


class function TsgeExtensionCursor.GetName: String;
begin
  Result := Extension_Cursor;
end;


procedure TsgeExtensionCursor.RegisterEventHandlers;
begin
  EventManager.SubscriberGroupList.Subscribe(Event_MouseMove, TsgeEventHandler(@Handler_MouseMove), Event_Priority_Max);
  EventManager.SubscriberGroupList.Subscribe(Event_MouseEnter, TsgeEventHandler(@Handler_MouseEnter), Event_Priority_Max);
  EventManager.SubscriberGroupList.Subscribe(Event_MouseLeave, TsgeEventHandler(@Handler_MouseLeave), Event_Priority_Max);
end;


constructor TsgeExtensionCursor.Create(ObjectList: TObject);
const
  LayerName = 'Cursor';
begin
  try
    inherited Create(ObjectList);

    //Поиск указателей
    FExtWindow := TsgeExtensionWindow(GetExtension(Extension_Window));
    FExtGraphic := TsgeExtensionGraphic(GetExtension(Extension_Graphic));
    FExtResList := TsgeExtensionResourceList(GetExtension(Extension_ResourceList));

    //Задать параметры
    FShowCursor := True;

    //Создать слой для вывода курсора
    FExtGraphic.LayerList.Add(LayerName, Graphic_LayerIndex_Cursor, True);

    //Создать примитив отрисовки
    FGUIElement := TsgeGraphicElementAnimation.Create(0, 0, 16, 16, FExtResList.Default.Frames);
    FGUIElement.Visible := False;

    //Добавить примитив в список отрисовки
    FExtGraphic.LayerList.AddElement(FGUIElement, LayerName);

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;



end.

