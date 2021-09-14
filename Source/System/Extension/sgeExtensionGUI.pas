{
Пакет             Simple Game Engine 2
Файл              sgeExtensionGUI.pas
Версия            1.0
Создан            03.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Графический интерфейс пользователя
}
{$Include Defines.inc}

unit sgeExtensionGUI;

{$mode objfpc}{$H+}

interface

uses
  sgeExtensionBase, sgeEventKeyboard, sgeEventMouse, sgeEventSubscriber, sgeGraphicElementLayer,
  sgeExtensionGraphic, sgeGUIFormList, sgeGUIElement;

const
  Extension_GUI = 'GUI';


type
  TsgeExtensionGUI = class(TsgeExtensionBase)
  const
    MAX_SUB_COUNT = 7;
  private
    //Ссылки
    FExtGraphic: TsgeExtensionGraphic;
    FGUILayer: TsgeGraphicElementLayer;                             //Ссылка на слой GUI

    //Объекты
    FFormList: TsgeGUIFormList;

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

    function  Handler_MouseMove(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseDown(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseUp(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseWheel(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseDblClick(EventObj: TsgeEventMouse): Boolean;

    function  MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse): Boolean;
  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;



    property Enable: Boolean read FEnable write SetEnable;
    property Visible: Boolean read FVisible write SetVisible;

    property FormList: TsgeGUIFormList read FFormList;
  end;



implementation

uses
  sgeErrors, sgeStringList, sgeFileUtils, sgeOSPlatform, sgeEventBase, sgeGUIForm;

const
  _UNITNAME = 'ExtensionGUI';


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
  FEventSubscriber[0] := EventManager.Subscribe(Event_KeyboardDown, TsgeEventHandler(@Handler_KeyDown), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[1] := EventManager.Subscribe(Event_KeyboardUp, TsgeEventHandler(@Handler_KeyUp), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[2] := EventManager.Subscribe(Event_KeyboardChar, TsgeEventHandler(@Handler_KeyChar), EventPriorityMaxMinusTwo, True);

  //Мышь
  FEventSubscriber[3] := EventManager.Subscribe(Event_MouseMove, TsgeEventHandler(@Handler_MouseMove), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[4] := EventManager.Subscribe(Event_MouseDown, TsgeEventHandler(@Handler_MouseDown), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[5] := EventManager.Subscribe(Event_MouseUp, TsgeEventHandler(@Handler_MouseUp), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[6] := EventManager.Subscribe(Event_MouseScroll, TsgeEventHandler(@Handler_MouseWheel), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[7] := EventManager.Subscribe(Event_MouseDoubleClick, TsgeEventHandler(@Handler_MouseDblClick), EventPriorityMaxMinusTwo, True);
end;


procedure TsgeExtensionGUI.UnregisterEventHandlers;
begin
  EventManager.UnSubscribe(Self);
end;


function TsgeExtensionGUI.Handler_KeyDown(EventObj: TsgeEventKeyboard): Boolean;
begin
  Result := False;
end;


function TsgeExtensionGUI.Handler_KeyUp(EventObj: TsgeEventKeyboard): Boolean;
begin
  Result := False;
end;


function TsgeExtensionGUI.Handler_KeyChar(EventObj: TsgeEventKeyboardChar): Boolean;
begin
  Result := False;
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
  Form: TsgeGUIForm;
begin
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
          Form.MouseHandler(EventType, Mouse);
          Result := True;
          Break;
          end;
        end;


      emetMove:
        begin

        end;
    end;



    end;  //for
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
    FFormList := TsgeGUIFormList.Create(True);

    //Подписаться на события
    RegisterEventHandlers;

    //Создать слой отрисовки GUI
    FGUILayer := FExtGraphic.DrawList.AddLayer(Extension_GUI, $FE, True);

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;



destructor TsgeExtensionGUI.Destroy;
begin
  //Отписаться от событий
  UnregisterEventHandlers;

  //Удалить объекты
  FFormList.Free;

  inherited Destroy;
end;



end.

