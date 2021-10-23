{
Пакет             Simple Game Engine 2
Файл              sgeExtensionKeyCommand.pas
Версия            1.6
Создан            01.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Команда на кнопках
}
{$Include Defines.inc}

unit sgeExtensionKeyCommand;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeExtensionBase, sgeExtensionShell, sgeEventBase,
  sgeEventKeyboard, sgeEventMouse, sgeEventControllers,
  sgeKeyCommandKeyboard, sgeKeyCommandMouse, sgeKeyCommandJoystick;


const
  Extension_KeyCommand = 'KeyCommand';


type
  TsgeExtensionKeyCommand = class(TsgeExtensionBase)
  private
    //Ссылки
    FExtShell: TsgeExtensionShell;

    //Объекты
    FKeyboard: TsgeKeyCommandKeyboard;                    //Кнопки клавиатуры
    FMouse: TsgeKeyCommandMouse;                          //Кнопки мыши
    FJoystick: TsgeKeyCommandJoystick;                    //Кнопки, крестовина, оси джойстиков

    //Вспомогательные переменные
    FBlockCharEvent: Boolean;

    function GetMouseButtonIndex(Buttons: TsgeMouseButtons): Byte;  //Вернуть номер кнопки мышки из множества
    function CommandHandler(const Command: String): Boolean;        //Проверить команду и выполнить

    //Обработчики событий
    procedure RegisterEventHandlers;
    procedure UnRegisterEventHandlers;

    function  Handler_KeyDown(EventObj: TsgeEventKeyboard): Boolean;
    function  Handler_KeyUp(EventObj: TsgeEventKeyboard): Boolean;
    function  Handler_KeyChar(EventObj: TsgeEventKeyboardChar): Boolean;

    function  Handler_MouseDown(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseUp(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseWheel(EventObj: TsgeEventMouse): Boolean;
    function  Handler_MouseDblClick(EventObj: TsgeEventMouse): Boolean;

    function  Handler_JoystickButtonDown(EventObj: TsgeEventControllerButton): Boolean;
    function  Handler_JoystickButtonUp(EventObj: TsgeEventControllerButton): Boolean;
    function  Handler_JoystickPadDown(EventObj: TsgeEventControllerPOV): Boolean;
    function  Handler_JoystickPadUp(EventObj: TsgeEventControllerPOV): Boolean;
    function  Handler_JoystickAxisDown(EventObj: TsgeEventControllerAxis): Boolean;
    function  Handler_JoystickAxisUp(EventObj: TsgeEventControllerAxis): Boolean;

  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    property Keyboard: TsgeKeyCommandKeyboard read FKeyboard;
    property Mouse: TsgeKeyCommandMouse read FMouse;
    property Joystick: TsgeKeyCommandJoystick read FJoystick;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'ExtensionKeyCommand';



function TsgeExtensionKeyCommand.GetMouseButtonIndex(Buttons: TsgeMouseButtons): Byte;
begin
  Result := 0;
  if (mbLeft in Buttons) then Result := 0;
  if (mbMiddle in Buttons) then Result := 1;
  if (mbRight in Buttons) then Result := 2;
  if (mbExtra1 in Buttons) then Result := 3;
  if (mbExtra2 in Buttons) then Result := 4;
end;


function TsgeExtensionKeyCommand.CommandHandler(const Command: String): Boolean;
begin
  //Результат по умолчанию
  Result := False;

  //Проверить команду
  if Command <> '' then
    begin
    //Дальше не передавать этот объект
    Result := True;

    //Выполнить команду
    FExtShell.DoCommand(Command);
    end;
end;


procedure TsgeExtensionKeyCommand.RegisterEventHandlers;
begin
  //Клавиатура
  EventManager.SubscriberGroupList.Subscribe(Event_KeyboardDown, TsgeEventHandler(@Handler_KeyDown), EventPriorityMaxMinusOne, True);
  EventManager.SubscriberGroupList.Subscribe(Event_KeyboardUp, TsgeEventHandler(@Handler_KeyUp), EventPriorityMaxMinusOne, True);
  EventManager.SubscriberGroupList.Subscribe(Event_KeyboardChar, TsgeEventHandler(@Handler_KeyChar), EventPriorityMax, True);

  //Мышь
  EventManager.SubscriberGroupList.Subscribe(Event_MouseDown, TsgeEventHandler(@Handler_MouseDown), EventPriorityMaxMinusOne, True);
  EventManager.SubscriberGroupList.Subscribe(Event_MouseUp, TsgeEventHandler(@Handler_MouseUp), EventPriorityMaxMinusOne, True);
  EventManager.SubscriberGroupList.Subscribe(Event_MouseScroll, TsgeEventHandler(@Handler_MouseWheel), EventPriorityMaxMinusOne, True);
  EventManager.SubscriberGroupList.Subscribe(Event_MouseDoubleClick, TsgeEventHandler(@Handler_MouseDblClick), EventPriorityMaxMinusOne, True);

  //Контроллеры
  EventManager.SubscriberGroupList.Subscribe(Event_ControllerButtonDown, TsgeEventHandler(@Handler_JoystickButtonDown), EventPriorityMaxMinusOne, True);
  EventManager.SubscriberGroupList.Subscribe(Event_ControllerButtonUp, TsgeEventHandler(@Handler_JoystickButtonUp), EventPriorityMaxMinusOne, True);
  EventManager.SubscriberGroupList.Subscribe(Event_ControllerPovDown, TsgeEventHandler(@Handler_JoystickPadDown), EventPriorityMaxMinusOne, True);
  EventManager.SubscriberGroupList.Subscribe(Event_ControllerPovUp, TsgeEventHandler(@Handler_JoystickPadUp), EventPriorityMaxMinusOne, True);
  EventManager.SubscriberGroupList.Subscribe(Event_ControllerAxisDown, TsgeEventHandler(@Handler_JoystickAxisDown), EventPriorityMaxMinusOne, True);
  EventManager.SubscriberGroupList.Subscribe(Event_ControllerAxisUp, TsgeEventHandler(@Handler_JoystickAxisUp), EventPriorityMaxMinusOne, True);
end;


procedure TsgeExtensionKeyCommand.UnRegisterEventHandlers;
begin
  //Отписаться от всех событий
  EventManager.SubscriberGroupList.UnSubscribe(Self);
end;


function TsgeExtensionKeyCommand.Handler_KeyDown(EventObj: TsgeEventKeyboard): Boolean;
var
  Command: String;
begin
  //По умолчанию
  Result := False;

  //Проверить команду на кнопке
  Command := FKeyboard.Key[EventObj.Key].Down;
  if Command <> '' then
    begin
    Result := True;                                           //Дальше не передавать этот объект
    FBlockCharEvent := True;                                  //Заблокировать следующее событие WM_CHAR
    if EventObj.FirstDown then FExtShell.DoCommand(Command);  //Выполнить команду
    end else FBlockCharEvent := False;
end;


function TsgeExtensionKeyCommand.Handler_KeyUp(EventObj: TsgeEventKeyboard): Boolean;
begin
  Result := CommandHandler(FKeyboard.Key[EventObj.Key].Up);
end;


function TsgeExtensionKeyCommand.Handler_KeyChar(EventObj: TsgeEventKeyboardChar): Boolean;
begin
  Result := False;

  //Проверить подавление события WM_CHAR
  if FBlockCharEvent then
    begin
    FBlockCharEvent := False;
    Result := True;
    end;
end;


function TsgeExtensionKeyCommand.Handler_MouseDown(EventObj: TsgeEventMouse): Boolean;
begin
  Result := CommandHandler(FMouse.Button[TsgeMouseButton(GetMouseButtonIndex(EventObj.MouseButtons))].Down);
end;


function TsgeExtensionKeyCommand.Handler_MouseUp(EventObj: TsgeEventMouse): Boolean;
begin
  Result := CommandHandler(FMouse.Button[TsgeMouseButton(GetMouseButtonIndex(EventObj.MouseButtons))].Up);
end;


function TsgeExtensionKeyCommand.Handler_MouseWheel(EventObj: TsgeEventMouse): Boolean;
const
  ckmUp = 0;
  ckmDown = 1;
var
  Command: String;
  KeyMethod: Byte;
begin
  //Определить направление
  if EventObj.Delta > 0 then KeyMethod := ckmUp else KeyMethod := ckmDown;

  //Проверить команду
  case KeyMethod of
    ckmUp   : Command := FMouse.Wheel.Up;
    ckmDown : Command := FMouse.Wheel.Down;
  end;

  //Обработать команду
  Result := CommandHandler(Command);
end;


function TsgeExtensionKeyCommand.Handler_MouseDblClick(EventObj: TsgeEventMouse): Boolean;
begin
  Result := CommandHandler(FMouse.Button[TsgeMouseButton(GetMouseButtonIndex(EventObj.MouseButtons))].DblClick);
end;


function TsgeExtensionKeyCommand.Handler_JoystickButtonDown(EventObj: TsgeEventControllerButton): Boolean;
begin
  Result := CommandHandler(FJoystick.Item[EventObj.ID].Buttons.Item[EventObj.ButtonID].Down);
end;


function TsgeExtensionKeyCommand.Handler_JoystickButtonUp(EventObj: TsgeEventControllerButton): Boolean;
begin
  Result := CommandHandler(FJoystick.Item[EventObj.ID].Buttons.Item[EventObj.ButtonID].Up);
end;


function TsgeExtensionKeyCommand.Handler_JoystickPadDown(EventObj: TsgeEventControllerPOV): Boolean;
begin
  Result := CommandHandler(FJoystick.Item[EventObj.ID].Pad.Item[EventObj.Direction].Down);
end;


function TsgeExtensionKeyCommand.Handler_JoystickPadUp(EventObj: TsgeEventControllerPOV): Boolean;
begin
  Result := CommandHandler(FJoystick.Item[EventObj.ID].Pad.Item[EventObj.Direction].Up);
end;


function TsgeExtensionKeyCommand.Handler_JoystickAxisDown(EventObj: TsgeEventControllerAxis): Boolean;
begin
  Result := CommandHandler(FJoystick.Item[EventObj.ID].Axis.Item[EventObj.Axis].Tilt[EventObj.Tilt].Down);
end;


function TsgeExtensionKeyCommand.Handler_JoystickAxisUp(EventObj: TsgeEventControllerAxis): Boolean;
begin
  Result := CommandHandler(FJoystick.Item[EventObj.ID].Axis.Item[EventObj.Axis].Tilt[EventObj.Tilt].Up);
end;


class function TsgeExtensionKeyCommand.GetName: String;
begin
  Result := Extension_KeyCommand;
end;


constructor TsgeExtensionKeyCommand.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Поиск указателей
    FExtShell := TsgeExtensionShell(GetExtension(Extension_Shell));

    //Создать объекты
    FKeyboard := TsgeKeyCommandKeyboard.Create;
    FMouse := TsgeKeyCommandMouse.Create;
    FJoystick := TsgeKeyCommandJoystick.Create;

    //Подписать обработчики
    RegisterEventHandlers;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionKeyCommand.Destroy;
begin
  //Отписать подписчиков
  UnRegisterEventHandlers;

  //Удалить объекты
  FJoystick.Free;
  FMouse.Free;
  FKeyboard.Free;

  inherited Destroy;
end;


end.

