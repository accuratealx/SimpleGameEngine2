{
Пакет             Simple Game Engine 2
Файл              sgeExtensionGUI.pas
Версия            1.3
Создан            03.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Графический интерфейс пользователя
}
{$Include Defines.inc}

unit sgeExtensionGUI;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeExtensionBase, sgeEventBase, sgeEventKeyboard, sgeEventMouse, sgeEventSubscriber,
  sgeGraphicElementLayer, sgeExtensionGraphic, sgeGUIFormList, sgeGUIElement;

const
  Extension_GUI = 'GUI';


type
  TsgeExtensionGUI = class(TsgeExtensionBase)
  const
    MAX_SUB_COUNT = 8;
  private
    //Ссылки
    FExtGraphic: TsgeExtensionGraphic;
    FGUILayer: TsgeGraphicElementLayer;                             //Ссылка на слой GUI

    //Объекты
    FFormList: TsgeGUIFormList;

    //Вспомогательные параметры
    FCapturedElement: TsgeGUIElement;                               //Ссылка на элемент, захвативший события мыши
    FFocusedElement: TsgeGUIElement;                                //Элемент имеющий фокус ввода
    FLastElementAtCursor: TsgeGUIElement;                           //Последний элемент под курсором

    //Параметры
    FEnable: Boolean;
    FVisible: Boolean;

    //Ссылки на объекты подписки
    FEventSubscriber: array [0..MAX_SUB_COUNT] of TsgeEventSubscriber;

    //Свойства
    procedure SetEnable(AEnable: Boolean);
    procedure SetVisible(AVisible: Boolean);

    //Обработчики событий
    function  Handler_KeyDown(EventObj: TsgeEventKeyboard): Boolean;
    function  Handler_KeyUp(EventObj: TsgeEventKeyboard): Boolean;
    function  Handler_KeyChar(EventObj: TsgeEventKeyboardChar): Boolean;

    function  Handler_MouseLeave(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseMove(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseDown(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseUp(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseWheel(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseDblClick(EventObj: TsgeEventMouse): Boolean;

    function  MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse): Boolean;
    function  ButtonHandler(EventType: TsgeGUIElementButtonEventType; Keyboard: TsgeEventBase): Boolean;

    //Вспомогательные функции
    procedure ClearForms;
    function  ElementAtCursor(X, Y: Integer): TsgeGUIElement;
  protected
    class function GetName: String; override;

    procedure RegisterEventHandlers; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    procedure RepaintForms;                                                   //Перерисовать формы

    //Мышь
    procedure MouseCapture(Element: TsgeGUIElement);                          //Установить захват мыши
    procedure ReleaseMouse(Element: TsgeGUIElement);                          //Отменить захват мыши

    //Активный элемент
    procedure SetFocus(Element: TsgeGUIElement);                              //Установить фокус ввода на элемент
    procedure LostFocus(Element: TsgeGUIElement);                             //Убрать фокус с элемента

    //Свойства
    property Enable: Boolean read FEnable write SetEnable;
    property Visible: Boolean read FVisible write SetVisible;

    property FormList: TsgeGUIFormList read FFormList;
    property GUILayer: TsgeGraphicElementLayer read FGUILayer;
  end;



implementation

uses
  sgeErrors, sgeOSPlatform, sgeGUIForm;

const
  _UNITNAME = 'ExtensionGSUI';


procedure TsgeExtensionGUI.SetEnable(AEnable: Boolean);
var
  i: Integer;
begin
  if FEnable = AEnable then Exit;

  FEnable := AEnable;

  for i := 0 to MAX_SUB_COUNT do
    FEventSubscriber[i].Enable := FEnable;
end;


procedure TsgeExtensionGUI.SetVisible(AVisible: Boolean);
begin
  if FVisible = AVisible then Exit;

  FVisible := AVisible;

  //Поправить слой отрисовки
  FGUILayer.Visible := FVisible;
end;


function TsgeExtensionGUI.Handler_KeyDown(EventObj: TsgeEventKeyboard): Boolean;
begin
  Result := ButtonHandler(ebetDown, EventObj);
end;


function TsgeExtensionGUI.Handler_KeyUp(EventObj: TsgeEventKeyboard): Boolean;
begin
  Result := ButtonHandler(ebetUp, EventObj);
end;


function TsgeExtensionGUI.Handler_KeyChar(EventObj: TsgeEventKeyboardChar): Boolean;
begin
  Result := ButtonHandler(ebetChar, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseLeave(EventObj: TsgeEventMouse): Boolean;
begin
  Result := MouseHandler(emetMove, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseMove(EventObj: TsgeEventMouse): Boolean;
begin
  Result := MouseHandler(emetMove, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseDown(EventObj: TsgeEventMouse): Boolean;
begin
  Result := MouseHandler(emetDown, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseUp(EventObj: TsgeEventMouse): Boolean;
begin
  Result := MouseHandler(emetUp, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseWheel(EventObj: TsgeEventMouse): Boolean;
begin
  Result := MouseHandler(emetScroll, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseDblClick(EventObj: TsgeEventMouse): Boolean;
begin
  Result := MouseHandler(emetDblClick, EventObj);
end;


function TsgeExtensionGUI.MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse): Boolean;
var
  Form: TsgeGUIForm;
  Element: TsgeGUIElement;
  MousePoint, Pt: TsgeIntPoint;
begin
  //Передавать событие
  Result := False;

  //Сохранить координаты мыши
  MousePoint := Mouse.Pos;

  //Узнать элемент под курсором
  Element := ElementAtCursor(Mouse.X, Mouse.Y);

  //Проверить событие MouseEnter, MouseLeave
  if EventType = emetMove then
    begin
    if FLastElementAtCursor <> Element then
      begin
      //Уход мыши
      if FLastElementAtCursor <> nil then
        begin
        Pt := FLastElementAtCursor.GetGlobalPos;
        Mouse.ChangeXY(MousePoint.X - Pt.X, MousePoint.Y - Pt.Y);
        FLastElementAtCursor.MouseHandler(emetLeave, Mouse);
        end;

      //Заход мыши
      if Element <> nil then
        begin
        Pt := Element.GetGlobalPos;
        Mouse.ChangeXY(MousePoint.X - Pt.X, MousePoint.Y - Pt.Y);
        Element.MouseHandler(emetEnter, Mouse);
        end;
      end;

    //Запомнить последний элемент под курсором
    FLastElementAtCursor := Element;
    end;

  //Если монопольный захват событи мыши, то поправить
  if FCapturedElement <> nil then
    Element := FCapturedElement;

  //Проверить что элемент найден
  if Element = nil then
    begin
    //Если нажали вне GUI формы, то сбросить фокус ввода
    if EventType = emetDown then SetFocus(nil);
    Exit;
    end;

  //Если элемент найден, то подавить событие
  Result := True;

  //Поправить координаты относительно текущего элемента
  Pt := Element.GetGlobalPos;
  Mouse.ChangeXY(MousePoint.X - Pt.X, MousePoint.Y - Pt.Y);

  //Передать событие элементу
  case EventType of
    emetUp, emetMove, emetScroll, emetDblClick:
      Element.MouseHandler(EventType, Mouse);

    emetDown:
      begin
      //Найти форму
      Form := TsgeGUIForm(Element.GetTopParent);

      //Сделать форму активной
      SetFocus(Form);

      //Выполнить обработчик элемента
      Element.MouseHandler(EventType, Mouse);
      end;
  end;

end;


function TsgeExtensionGUI.ButtonHandler(EventType: TsgeGUIElementButtonEventType; Keyboard: TsgeEventBase): Boolean;
begin
  Result := False;

  if FFocusedElement <> nil then
    Result := FFocusedElement.ButtonHandler(EventType, Keyboard);
end;


procedure TsgeExtensionGUI.ClearForms;
begin
  if FFormList.Count = 0 then Exit;

  //Удалить объекты
  while FFormList.Count > 0 do
    FFormList.Item[0].Free;
end;


function TsgeExtensionGUI.ElementAtCursor(X, Y: Integer): TsgeGUIElement;

  function CursorInElement(Element: TsgeGUIElement; X, Y: Integer): Boolean;
  begin
    Result := (X >= Element.Left) and
              (X <= Element.Left + Element.Width) and
              (Y >= Element.Top) and
              (Y <= Element.Top + Element.Height);
  end;

  function ProcessElement(Element: TsgeGUIElement; X, Y: Integer): TsgeGUIElement;
  var
    Xl, Yl, i: Integer;
    E: TsgeGUIElement;
  begin
    Result := nil;

    //Если элемент неактивен, то выход
    if not Element.Visible or not Element.Enable then Exit;

    //Проверить нахождение координат в текщум объекте
    if CursorInElement(Element, X, Y) then
      begin

      //Результат по умолчанию
      Result := Element;

      //Поправить координаты относительно текущего элемента
      Xl := X - Element.Left;
      Yl := Y - Element.Top;

      //Проверить детей
      for i := 0 to Element.ChildList.Count - 1 do
        begin
        E := ProcessElement(Element.ChildList.Item[i], Xl, Yl);
        if E <> nil then Exit(E);
        end;
      end;
  end;

var
  i: Integer;
  E: TsgeGUIElement;
begin
  Result := nil;

  //Перебрать формы
  for i := FFormList.Count - 1 downto 0 do
    begin
    E := ProcessElement(FFormList.Item[i], X, Y);
    if E <> nil then Exit(E);
    end;
end;


class function TsgeExtensionGUI.GetName: String;
begin
  Result := Extension_GUI;
end;


procedure TsgeExtensionGUI.RegisterEventHandlers;
begin
  //Клавиатура
  FEventSubscriber[0] := EventManager.SubscriberGroupList.Subscribe(Event_KeyboardDown, TsgeEventHandler(@Handler_KeyDown), Event_Priority_GUI, True);
  FEventSubscriber[1] := EventManager.SubscriberGroupList.Subscribe(Event_KeyboardUp, TsgeEventHandler(@Handler_KeyUp), Event_Priority_GUI, True);
  FEventSubscriber[2] := EventManager.SubscriberGroupList.Subscribe(Event_KeyboardChar, TsgeEventHandler(@Handler_KeyChar), Event_Priority_GUI, True);

  //Мышь
  FEventSubscriber[3] := EventManager.SubscriberGroupList.Subscribe(Event_MouseLeave, TsgeEventHandler(@Handler_MouseLeave), Event_Priority_GUI, True);
  FEventSubscriber[4] := EventManager.SubscriberGroupList.Subscribe(Event_MouseMove, TsgeEventHandler(@Handler_MouseMove), Event_Priority_GUI, True);
  FEventSubscriber[5] := EventManager.SubscriberGroupList.Subscribe(Event_MouseDown, TsgeEventHandler(@Handler_MouseDown), Event_Priority_GUI, True);
  FEventSubscriber[6] := EventManager.SubscriberGroupList.Subscribe(Event_MouseUp, TsgeEventHandler(@Handler_MouseUp), Event_Priority_GUI, True);
  FEventSubscriber[7] := EventManager.SubscriberGroupList.Subscribe(Event_MouseScroll, TsgeEventHandler(@Handler_MouseWheel), Event_Priority_GUI, True);
  FEventSubscriber[8] := EventManager.SubscriberGroupList.Subscribe(Event_MouseDoubleClick, TsgeEventHandler(@Handler_MouseDblClick), Event_Priority_GUI, True);
end;


constructor TsgeExtensionGUI.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Поиск указателей
    FExtGraphic := TsgeExtensionGraphic(GetExtension(Extension_Graphic));

    //Создать классы
    FFormList := TsgeGUIFormList.Create(False);

    //Подписаться на события
    RegisterEventHandlers;

    //Создать слой отрисовки GUI
    FGUILayer := FExtGraphic.LayerList.Add(Extension_GUI, Graphic_LayerIndex_GUI, True);

    //Установить параметры
    FVisible := True;
    FVisible := True;
    FLastElementAtCursor := nil;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;



destructor TsgeExtensionGUI.Destroy;
begin
  //Удалить формы
  ClearForms;

  //Удалить объекты
  FFormList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionGUI.RepaintForms;
var
  i: Integer;
begin
  for i := 0 to FFormList.Count - 1 do
    FFormList.Item[i].Repaint;
end;


procedure TsgeExtensionGUI.MouseCapture(Element: TsgeGUIElement);
begin
  if FCapturedElement = Element then Exit;

  FCapturedElement := Element;
end;


procedure TsgeExtensionGUI.ReleaseMouse(Element: TsgeGUIElement);
begin
  if FCapturedElement = Element then FCapturedElement := nil;
end;


procedure TsgeExtensionGUI.SetFocus(Element: TsgeGUIElement);
begin
  if FFocusedElement = Element then Exit;

  //Убираем фокус со старого элемента
  if FFocusedElement <> nil then FFocusedElement.Focused := False;

  //Запомнить новый элемент
  FFocusedElement := Element;

  //Установить фокус новому элементу
  if FFocusedElement <> nil then FFocusedElement.Focused := True;
end;


procedure TsgeExtensionGUI.LostFocus(Element: TsgeGUIElement);
begin
  if FFocusedElement = Element then FFocusedElement := nil;
end;



end.

