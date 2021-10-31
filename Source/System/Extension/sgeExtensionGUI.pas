{
Пакет             Simple Game Engine 2
Файл              sgeExtensionGUI.pas
Версия            1.1
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
  sgeGraphicElementLayer, sgeExtensionGraphic, sgeGUIFormList, sgeGUIElement;

const
  Extension_GUI = 'GUI';


type
  TsgeExtensionGUI = class(TsgeExtensionBase)
  const
    MAX_SUB_COUNT = 9;
  private
    //Ссылки
    FExtGraphic: TsgeExtensionGraphic;
    FGUILayer: TsgeGraphicElementLayer;                             //Ссылка на слой GUI

    //Объекты
    FFormList: TsgeGUIFormList;

    //Вспомогательные параметры
    FCapturedElement: TsgeGUIElement;                               //Ссылка на элемент, захвативший события мыши
    FFocusedElement: TsgeGUIElement;                                //Элемент имеющий фокус ввода

    //Параметры
    FEnable: Boolean;
    FVisible: Boolean;

    //Ссылки на объекты подписки
    FEventSubscriber: array [0..MAX_SUB_COUNT] of TsgeEventSubscriber;

    //Свойства
    procedure SetEnable(AEnable: Boolean);
    procedure SetVisible(AVisible: Boolean);

    //Обработчики событий
    procedure RegisterEventHandlers;
    procedure UnregisterEventHandlers;

    function  Handler_KeyDown(EventObj: TsgeEventKeyboard): Boolean;
    function  Handler_KeyUp(EventObj: TsgeEventKeyboard): Boolean;
    function  Handler_KeyChar(EventObj: TsgeEventKeyboardChar): Boolean;

    function  Handler_MouseEnter(EventObj: TsgeEventMouse): Boolean;
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
  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    procedure RepaintForms;                                                   //Перерисовать формы

    //Мышь
    procedure MouseCapture(Element: TsgeGUIElement);                          //Установить захват мыши
    procedure ReleaseMouse(Element: TsgeGUIElement);                          //Отменить захват мыши
    procedure ReleaseMouse;                                                   //Отменить захват мыши

    //Активный элемент
    procedure SetFocus(Element: TsgeGUIElement);                              //Установить фокус ввода на элемент
    procedure LostFocus(Element: TsgeGUIElement);                             //Убрать фокус с элемента

    //Свойства
    property Enable: Boolean read FEnable write SetEnable;
    property Visible: Boolean read FVisible write SetVisible;

    property FormList: TsgeGUIFormList read FFormList;
  end;



implementation

uses
  sgeTypes, sgeErrors, sgeOSPlatform, sgeGUIForm;

const
  _UNITNAME = 'ExtensionGUI';

type
  TsgeGUIFormHack = class(TsgeGUIForm);


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


procedure TsgeExtensionGUI.RegisterEventHandlers;
begin
  //Клавиатура
  FEventSubscriber[0] := EventManager.SubscriberGroupList.Subscribe(Event_KeyboardDown, TsgeEventHandler(@Handler_KeyDown), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[1] := EventManager.SubscriberGroupList.Subscribe(Event_KeyboardUp, TsgeEventHandler(@Handler_KeyUp), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[2] := EventManager.SubscriberGroupList.Subscribe(Event_KeyboardChar, TsgeEventHandler(@Handler_KeyChar), EventPriorityMaxMinusTwo, True);

  //Мышь
  FEventSubscriber[3] := EventManager.SubscriberGroupList.Subscribe(Event_MouseEnter, TsgeEventHandler(@Handler_MouseEnter), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[4] := EventManager.SubscriberGroupList.Subscribe(Event_MouseLeave, TsgeEventHandler(@Handler_MouseLeave), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[5] := EventManager.SubscriberGroupList.Subscribe(Event_MouseMove, TsgeEventHandler(@Handler_MouseMove), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[6] := EventManager.SubscriberGroupList.Subscribe(Event_MouseDown, TsgeEventHandler(@Handler_MouseDown), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[7] := EventManager.SubscriberGroupList.Subscribe(Event_MouseUp, TsgeEventHandler(@Handler_MouseUp), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[8] := EventManager.SubscriberGroupList.Subscribe(Event_MouseScroll, TsgeEventHandler(@Handler_MouseWheel), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[9] := EventManager.SubscriberGroupList.Subscribe(Event_MouseDoubleClick, TsgeEventHandler(@Handler_MouseDblClick), EventPriorityMaxMinusTwo, True);
end;


procedure TsgeExtensionGUI.UnregisterEventHandlers;
begin
  EventManager.SubscriberGroupList.UnSubscribe(Self);
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


function TsgeExtensionGUI.Handler_MouseEnter(EventObj: TsgeEventMouse): Boolean;
begin
  Result := MouseHandler(emetEnter, EventObj);
end;


function TsgeExtensionGUI.Handler_MouseLeave(EventObj: TsgeEventMouse): Boolean;
begin
  Result := MouseHandler(emetLeave, EventObj);
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
  i: Integer;
  Pt: TsgeIntPoint;
  Form: TsgeGUIForm;
begin
  //Проверить монопольный доступ к событиям мыши
  if FCapturedElement <> nil then
    begin
    Result := True;
    Pt := FCapturedElement.GetGlobalPos;
    Mouse.ChangeXY(Mouse.X - Pt.X, Mouse.Y - Pt.Y);
    FCapturedElement.MouseHandler(EventType, Mouse);
    Exit;
    end;



  Result := False;

  //Просмотреть формы
  for i := 0 to FFormList.Count - 1 do
    begin
    //Ссылка
    Form := FFormList.Item[i];

    //Обработать тип события
    case EventType of
      emetDown, emetUp, emetScroll, emetDblClick:
        begin
        if Form.CursorInElement(Mouse.X, Mouse.Y) then
          begin
          //Координаты относительно элемента
          Mouse.ChangeXY(Mouse.X - Form.Left, Mouse.Y - Form.Top);

          //Вызвать обработчик
          Result := Form.MouseHandler(EventType, Mouse);
          if Result then Break;
          end;
        end;


      emetMove:
        begin
        //Движение над формой
        if Form.CursorInElement(Mouse.X, Mouse.Y) then
          begin

          //Координаты относительно элемента
          Mouse.ChangeXY(Mouse.X - Form.Left, Mouse.Y - Form.Top);

          //Если флаг захода мыши не установлен, то выполнить
          if not TsgeGUIFormHack(Form).FEventMouseEntered then
            begin
            TsgeGUIFormHack(Form).FEventMouseEntered := True;
            Form.MouseHandler(emetEnter, Mouse);
            end;

          //Движение мыши
          Result := Form.MouseHandler(emetMove, Mouse);
          end
          else begin
            if TsgeGUIFormHack(Form).FEventMouseEntered then
              begin
              TsgeGUIFormHack(Form).FEventMouseEntered := False;
              Form.MouseHandler(emetLeave, Mouse);
              end;
          end;
        end;


      emetLeave:
        begin
        //Просмотреть формы на предмет выхода за границы
        if not Form.CursorInElement(Mouse.X, Mouse.Y) then
          begin
            if TsgeGUIFormHack(Form).FEventMouseEntered then
              begin
              TsgeGUIFormHack(Form).FEventMouseEntered := False;
              Form.MouseHandler(emetLeave, Mouse);
              end;
          end;
        end;

    end;



    end;  //for
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


class function TsgeExtensionGUI.GetName: String;
begin
  Result := Extension_GUI;
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
    FGUILayer := FExtGraphic.LayerList.Add(Extension_GUI, $FE, True);

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;



destructor TsgeExtensionGUI.Destroy;
begin
  //Отписаться от событий
  UnregisterEventHandlers;

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
  if FCapturedElement = Element then
    FCapturedElement := nil;
end;


procedure TsgeExtensionGUI.ReleaseMouse;
begin
  FCapturedElement := nil;
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

