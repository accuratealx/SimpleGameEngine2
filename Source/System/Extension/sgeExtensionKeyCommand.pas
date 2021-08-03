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
  sgeExtensionBase, sgeExtensionShell, sgeEventBase, sgeEventSubscriber, sgeEventWindow,
  sgeKeyCommandKeyboard;


const
  Extension_KeyCommand = 'KeyCommand';


type
  TsgeExtensionKeyCommand = class(TsgeExtensionBase)
  private
    //Ссылки
    FExtShell: TsgeExtensionShell;

    //Объекты
    FKeyboard: TsgeKeyCommandKeyboard;                    //Кнопки клавиатуры

    //Ссылки на объекты подписки
    FSubKeyDown: TsgeEventSubscriber;
    FSubKeyUp: TsgeEventSubscriber;
    FSubKeyChar: TsgeEventSubscriber;

    //Вспомогательные переменные
    FBlockCharEvent: Boolean;

    //Обработчики событий
    procedure RegisterEventHandlers;
    procedure UnRegisterEventHandlers;
    function  Handler_KeyDown(EventObj: TsgeEventWindowKeyboard): Boolean;
    function  Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
    function  Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;

  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    property Keyboard: TsgeKeyCommandKeyboard read FKeyboard;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'ExtensionKeyCommand';

  //Приоритеты
  HandlerPriority = $FFFE;


procedure TsgeExtensionKeyCommand.RegisterEventHandlers;
begin
  FSubKeyDown := EventManager.Subscribe(Event_WindowKeyDown, TsgeEventHandler(@Handler_KeyDown), HandlerPriority, True);
  FSubKeyUp := EventManager.Subscribe(Event_WindowKeyUp, TsgeEventHandler(@Handler_KeyUp), HandlerPriority, True);
  FSubKeyChar := EventManager.Subscribe(Event_WindowChar, TsgeEventHandler(@Handler_KeyChar), HandlerPriority, True);
end;


procedure TsgeExtensionKeyCommand.UnRegisterEventHandlers;
begin
  EventManager.UnSubscribe(Event_WindowKeyDown, TsgeEventHandler(@Handler_KeyDown));
  EventManager.UnSubscribe(Event_WindowKeyUp, TsgeEventHandler(@Handler_KeyUp));
  EventManager.UnSubscribe(Event_WindowChar, TsgeEventHandler(@Handler_KeyChar));
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
  Result := FBlockCharEvent;
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
  FKeyboard.Free;

  inherited Destroy;
end;


end.

