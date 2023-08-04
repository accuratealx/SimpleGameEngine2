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
  sgeExtensionBase, sgeEventBase, sgeEventKeyboard, sgeEventMouse, sgeEventSubscriber,
  sgeDisplayLayer, sgeGUIElement, sgeGUIFormList;

const
  Extension_GUI = 'GUI';


type
  TsgeExtensionGUI = class(TsgeExtensionBase)
  const
    MAX_SUB_COUNT = 8;
  private
    //Ссылки
    FDrawLayer: TsgeDisplayLayer;         //Объект управления слоем графики

    //Объекты
    FFormList: TsgeGUIFormList;           //Список форм

    //Вспомогательные параметры
    FCapturedElement: TsgeGUIElement;     //Ссылка на элемент, захвативший события мыши
    FFocusedElement: TsgeGUIElement;      //Элемент имеющий фокус ввода
    FLastElementAtCursor: TsgeGUIElement; //Последний элемент под курсором

    //Параметры
    FEnable: Boolean;
    FVisible: Boolean;

    //Ссылки на объекты подписки
    FEventSubscriber: array [0..MAX_SUB_COUNT] of TsgeEventSubscriber;

    //Свойства
    procedure SetEnable(AEnable: Boolean);
    procedure SetVisible(AVisible: Boolean);

    //Подписчики на события
    function  Handler_KeyDown(EventObj: TsgeEventKeyboard): TsgeEventHandlerResult;
    function  Handler_KeyUp(EventObj: TsgeEventKeyboard): TsgeEventHandlerResult;
    function  Handler_KeyChar(EventObj: TsgeEventKeyboardChar): TsgeEventHandlerResult;

    function  Handler_MouseLeave(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
    function  Handler_MouseMove(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
    function  Handler_MouseDown(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
    function  Handler_MouseUp(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
    function  Handler_MouseWheel(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
    function  Handler_MouseDblClick(EventObj: TsgeEventMouse): TsgeEventHandlerResult;

    //Обработчики событий
    function  MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse): TsgeEventHandlerResult;
    function  ButtonHandler(EventType: TsgeGUIElementButtonEventType; Keyboard: TsgeEventBase): TsgeEventHandlerResult;

    //Вспомогательные функции
    procedure DestroyForms;
    function  ElementAtCursor(X, Y: Integer): TsgeGUIElement;
  protected
    function GetName: String; override;

    procedure RegisterEventHandlers; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    //Мышь
    procedure MouseCapture(Element: TsgeGUIElement);                //Установить захват мыши
    procedure ReleaseMouse(Element: TsgeGUIElement);                //Отменить захват мыши

    //Активный элемент
    procedure SetFocus(Element: TsgeGUIElement);                    //Установить фокус ввода на элемент
    procedure LostFocus(Element: TsgeGUIElement);                   //Убрать фокус с элемента
    procedure ClearFocus;                                           //Очистить фокус

    //Свойства
    property Enable: Boolean read FEnable write SetEnable;
    property Visible: Boolean read FVisible write SetVisible;

    property FormList: TsgeGUIFormList read FFormList;
  end;



implementation

uses
  sgeErrors, sgeOSPlatform, sgeGUIForm;

type
  TsgeGUIElementExt = class(TsgeGUIElement);

const
  Layer_Name = 'System.GUI';
  Layer_Index = $FFFE;

  _UNITNAME = 'ExtensionGSUI';


procedure TsgeExtensionGUI.SetEnable(AEnable: Boolean);
var
  i: Integer;
begin
  if FEnable = AEnable then
    Exit;

  FEnable := AEnable;

  for i := 0 to MAX_SUB_COUNT do
    FEventSubscriber[i].Enable := FEnable;
end;


procedure TsgeExtensionGUI.SetVisible(AVisible: Boolean);
begin
  if FVisible = AVisible then
    Exit;

  FVisible := AVisible;

  //Поправить слой отрисовки
  FDrawLayer.Visible := FVisible;
  FDrawLayer.Update;
end;


function TsgeExtensionGUI.Handler_KeyDown(EventObj: TsgeEventKeyboard): TsgeEventHandlerResult;
begin
  Result := ButtonHandler(ebetDown, EventObj);
end;


function TsgeExtensionGUI.Handler_KeyUp(EventObj: TsgeEventKeyboard): TsgeEventHandlerResult;
begin
  Result := ButtonHandler(ebetUp, EventObj);
end;


function TsgeExtensionGUI.Handler_KeyChar(EventObj: TsgeEventKeyboardChar): TsgeEventHandlerResult;
begin
  Result := ButtonHandler(ebetChar, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseLeave(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
begin
  Result := MouseHandler(emetMove, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseMove(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
begin
  Result := MouseHandler(emetMove, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseDown(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
begin
  Result := MouseHandler(emetDown, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseUp(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
begin
  Result := MouseHandler(emetUp, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseWheel(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
begin
  Result := MouseHandler(emetScroll, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseDblClick(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
begin
  Result := MouseHandler(emetDblClick, EventObj);
end;


function TsgeExtensionGUI.MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse): TsgeEventHandlerResult;
var
  Form: TsgeGUIForm;
  Element: TsgeGUIElement;
begin
  //Передавать событие
  Result := ehrNormal;

  //Узнать элемент под курсором
  Element := ElementAtCursor(Mouse.X, Mouse.Y);

  //Проверить событие MouseEnter, MouseLeave
  if EventType = emetMove then
  begin
    if FLastElementAtCursor <> Element then
    begin
      //Уход мыши
      if FLastElementAtCursor <> nil then
        FLastElementAtCursor.MouseHandler(emetLeave, Mouse);

      //Заход мыши
      if Element <> nil then
        Element.MouseHandler(emetEnter, Mouse);
    end;

    //Запомнить последний элемент под курсором
    FLastElementAtCursor := Element;
  end;

  //Если монопольный захват событий мыши, то поправить
  if FCapturedElement <> nil then
    Element := FCapturedElement;

  //Проверить что элемент найден
  if Element = nil then
  begin
    //Если нажали вне GUI формы, то сбросить фокус ввода
    if EventType = emetDown then
      SetFocus(nil);
    Exit;
  end;

  //Если элемент найден, то подавить событие
  Result := ehrBreak;

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


function TsgeExtensionGUI.ButtonHandler(EventType: TsgeGUIElementButtonEventType; Keyboard: TsgeEventBase): TsgeEventHandlerResult;
begin
  Result := ehrNormal;

  //Проверить на видимость слоя
  if not FVisible then
    Exit;

  if (FFocusedElement <> nil) and (FFocusedElement.Visible) and (TsgeGUIElementExt(FFocusedElement).DisplayElement_GetVisible) then
    if FFocusedElement.ButtonHandler(EventType, Keyboard) then
      Result := ehrBreak;
end;


procedure TsgeExtensionGUI.DestroyForms;
begin
  if FFormList.Count = 0 then
    Exit;

  //Удалить формы
  while FFormList.Count > 0 do
    FFormList.Item[0].Free;
end;


function TsgeExtensionGUI.ElementAtCursor(X, Y: Integer): TsgeGUIElement;

  function ProcessElement(Element: TsgeGUIElement): TsgeGUIElement;
  var
    i: Integer;
    E: TsgeGUIElement;
  begin
    Result := nil;

    //Если элемент неактивен, то выход
    if (not Element.Visible) or (not Element.Enable) or (not TsgeGUIElementExt(Element).DisplayElement_GetVisible) then
      Exit;

    //Проверить нахождение координат в текущем элементе
    if Element.PointInElement(X, Y) then
    begin
      //Результат по умолчанию
      Result := Element;

      //Проверить детей
      for i := 0 to TsgeGUIElementExt(Element).FChildList.Count - 1 do
      begin
        E := ProcessElement(TsgeGUIElementExt(Element).FChildList.Item[i]);
        if E <> nil then
          Exit(E);
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
    E := ProcessElement(FFormList.Item[i]);
    if E <> nil then
      Exit(E);
  end;
end;


function TsgeExtensionGUI.GetName: String;
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


constructor TsgeExtensionGUI.Create;
begin
  try
    inherited Create;

    //Создать классы
    FFormList := TsgeGUIFormList.Create(False);

    //Подписаться на события
    RegisterEventHandlers;

    //Создать слой отрисовки GUI
    FDrawLayer := TsgeDisplayLayer.Create(Layer_Name, Layer_Index, True);
    FDrawLayer.Add;

    //Установить параметры
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
  DestroyForms;

  //Удалить объекты
  FDrawLayer.Free;
  FFormList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionGUI.MouseCapture(Element: TsgeGUIElement);
begin
  if FCapturedElement = Element then
    Exit;

  FCapturedElement := Element;
end;


procedure TsgeExtensionGUI.ReleaseMouse(Element: TsgeGUIElement);
begin
  if FCapturedElement = Element then
    FCapturedElement := nil;
end;


procedure TsgeExtensionGUI.SetFocus(Element: TsgeGUIElement);
begin
  if FFocusedElement = Element then
    Exit;

  //Убираем фокус со старого элемента
  if FFocusedElement <> nil then
    FFocusedElement.Focused := False;

  //Запомнить новый элемент
  FFocusedElement := Element;

  //Установить фокус новому элементу
  if FFocusedElement <> nil then
    FFocusedElement.Focused := True;
end;


procedure TsgeExtensionGUI.LostFocus(Element: TsgeGUIElement);
begin
  if FFocusedElement = Element then
    FFocusedElement := nil;
end;


procedure TsgeExtensionGUI.ClearFocus;
begin
  FFocusedElement := nil;
end;



end.

