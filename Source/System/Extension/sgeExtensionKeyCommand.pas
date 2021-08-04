{
Пакет             Simple Game Engine 2
Файл              sgeExtensionKeyCommand.pas
Версия            1.1
Создан            01.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Команда на кнопках
}
{$Include Defines.inc}

unit sgeExtensionKeyCommand;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeExtensionBase, sgeExtensionShell, sgeEventBase, sgeEventSubscriber,
  sgeEventWindow, sgeKeyCommandKeyboard, sgeKeyCommandMouse;


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

    //Вспомогательные переменные
    FBlockCharEvent: Boolean;

    function GetMouseButtonIndex(Buttons: TsgeMouseButtons): Byte;  //Вернуть номер кнопки мышки из множества

    //Обработчики событий
    procedure RegisterEventHandlers;
    procedure UnRegisterEventHandlers;

    function  Handler_KeyDown(EventObj: TsgeEventWindowKeyboard): Boolean;
    function  Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
    function  Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;

    function  Handler_MouseDown(EventObj: TsgeEventWindowMouse): Boolean;
    function  Handler_MouseUp(EventObj: TsgeEventWindowMouse): Boolean;
    function  Handler_MouseWheel(EventObj: TsgeEventWindowMouse): Boolean;

  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    property Keyboard: TsgeKeyCommandKeyboard read FKeyboard;
    property Mouse: TsgeKeyCommandMouse read FMouse;
  end;


implementation

uses
  sgeErrors, sgeKeys;

const
  _UNITNAME = 'ExtensionKeyCommand';

  //Приоритеты
  HandlerPriority = $FFFE;



function TsgeExtensionKeyCommand.GetMouseButtonIndex(Buttons: TsgeMouseButtons): Byte;
begin
  Result := 0;
  if (mbLeft in Buttons) then Result := 0;
  if (mbMiddle in Buttons) then Result := 1;
  if (mbRight in Buttons) then Result := 2;
  if (mbExtra1 in Buttons) then Result := 3;
  if (mbExtra2 in Buttons) then Result := 4;
end;


procedure TsgeExtensionKeyCommand.RegisterEventHandlers;
begin
  //Клавиатура
  EventManager.Subscribe(Event_WindowKeyDown, TsgeEventHandler(@Handler_KeyDown), HandlerPriority, True);
  EventManager.Subscribe(Event_WindowKeyUp, TsgeEventHandler(@Handler_KeyUp), HandlerPriority, True);
  EventManager.Subscribe(Event_WindowChar, TsgeEventHandler(@Handler_KeyChar), HandlerPriority, True);

  //Мышь
  EventManager.Subscribe(Event_WindowMouseDown, TsgeEventHandler(@Handler_MouseDown), HandlerPriority, True);
  EventManager.Subscribe(Event_WindowMouseUp, TsgeEventHandler(@Handler_MouseUp), HandlerPriority, True);
  EventManager.Subscribe(Event_WindowMouseScroll, TsgeEventHandler(@Handler_MouseWheel), HandlerPriority, True);

  //Контроллеры
end;


procedure TsgeExtensionKeyCommand.UnRegisterEventHandlers;
begin
  //Отписаться от всех событий
  EventManager.UnSubscribe(Self);
end;


function TsgeExtensionKeyCommand.Handler_KeyDown(EventObj: TsgeEventWindowKeyboard): Boolean;
var
  Command: String;
begin
  //Значение по умолчанию
  Result := False;

  //Проверить команду на кнопке
  Command := FKeyboard.Key[EventObj.Key].Down;
  if Command <> '' then
    begin
    //Дальше не передавать этот объект
    Result := True;

    //Заблокировать следующее событие WM_CHAR
    FBlockCharEvent := True;

    //Выполнить команду
    if EventObj.FirstDown then FExtShell.DoCommand(Command);
    end
    else FBlockCharEvent := False;
end;


function TsgeExtensionKeyCommand.Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
var
  Command: String;
begin
  //Значение по умолчанию
  Result := False;

  //Проверить команду на кнопке
  Command := FKeyboard.Key[EventObj.Key].Up;
  if Command <> '' then
    begin
    //Дальше не передавать этот объект
    Result := True;

    //Выполнить команду
    FExtShell.DoCommand(Command);
    end;
end;


function TsgeExtensionKeyCommand.Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;
begin
  //Заглушка. Что бы подавить событие WM_CHAR
  Result := FBlockCharEvent;
end;


function TsgeExtensionKeyCommand.Handler_MouseDown(EventObj: TsgeEventWindowMouse): Boolean;
var
  Command: String;
begin
  Result := False;

  Command := FMouse.Key[GetMouseButtonIndex(EventObj.MouseButtons)].Down;
  if Command <> '' then
    begin
    //Дальше не передавать этот объект
    Result := True;

    //Выполнить команду
    FExtShell.DoCommand(Command);
    end;
end;


function TsgeExtensionKeyCommand.Handler_MouseUp(EventObj: TsgeEventWindowMouse): Boolean;
var
  Command: String;
begin
  Result := False;

  Command := FMouse.Key[GetMouseButtonIndex(EventObj.MouseButtons)].Up;
  if Command <> '' then
    begin
    //Дальше не передавать этот объект
    Result := True;

    //Выполнить команду
    FExtShell.DoCommand(Command);
    end;
end;


function TsgeExtensionKeyCommand.Handler_MouseWheel(EventObj: TsgeEventWindowMouse): Boolean;
const
  ckmUp = 0;
  ckmDown = 1;
var
  Command: String;
  KeyMethod: Byte;
begin
  Result := False;

  //Определить направление
  if EventObj.Delta > 0 then KeyMethod := ckmUp else KeyMethod := ckmDown;

  //Проверить команду
  case KeyMethod of
    ckmUp   : Command := FMouse.Key[mouseWheel].Up;
    ckmDown : Command := FMouse.Key[mouseWheel].Down;
  end;

  //Обработать команду
  if Command <> '' then
    begin
    //Дальше не передавать этот объект
    Result := True;

    //Выполнить команду
    FExtShell.DoCommand(Command);
    end;
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
  FMouse.Free;
  FKeyboard.Free;

  inherited Destroy;
end;


end.

