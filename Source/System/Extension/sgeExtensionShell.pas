{
Пакет             Simple Game Engine 2
Файл              sgeExtensionShell.pas
Версия            1.0
Создан            14.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Оболочка
}
{$Include Defines.inc}

unit sgeExtensionShell;

{$mode objfpc}{$H+}

interface

uses
  sgeExtensionBase, sgeEventWindow, sgeEventSubscriber;


const
  Extension_Controllers = 'Shell';


type
  TsgeExtensionShell = class(TsgeExtensionBase)
  private
    //Ссылки на объекты подписки
    FSubKeyDown: TsgeEventSubscriber;
    FSubKeyUp: TsgeEventSubscriber;
    FSubKeyChar: TsgeEventSubscriber;

    //Поля
    FEnable: Boolean;

    //Обработчики событий
    procedure RegisterEventHandlers;
    procedure UnRegisterEventHandlers;
    function  Handler_KeyDown(EventObj: TsgeEventWindowKeyboard): Boolean;
    function  Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
    function  Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;

    //Свойства
    procedure SetEnable(AEnable: Boolean);

  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    property Enable: Boolean read FEnable write SetEnable;
  end;



implementation

uses
  sgeErrors, sgeEventBase;

const
  _UNITNAME = 'ExtensionShell';

  HandlerPriority = $FFFF;



procedure TsgeExtensionShell.RegisterEventHandlers;
begin
  FSubKeyDown := EventManager.Subscribe(Event_WindowKeyDown, TsgeEventHandler(@Handler_KeyDown), HandlerPriority, False);
  FSubKeyUp := EventManager.Subscribe(Event_WindowKeyUp, TsgeEventHandler(@Handler_KeyUp), HandlerPriority, False);
  FSubKeyChar := EventManager.Subscribe(Event_WindowChar, TsgeEventHandler(@Handler_KeyChar), HandlerPriority, False);
end;


procedure TsgeExtensionShell.UnRegisterEventHandlers;
begin
  EventManager.UnSubscribe(Event_WindowKeyDown, TsgeEventHandler(@Handler_KeyDown));
  EventManager.UnSubscribe(Event_WindowKeyUp, TsgeEventHandler(@Handler_KeyUp));
  EventManager.UnSubscribe(Event_WindowChar, TsgeEventHandler(@Handler_KeyChar));
end;


function TsgeExtensionShell.Handler_KeyDown(EventObj: TsgeEventWindowKeyboard): Boolean;
begin
  Result := True;
end;


function TsgeExtensionShell.Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
begin
  Result := True;
end;


function TsgeExtensionShell.Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;
begin
  Result := True;
end;


procedure TsgeExtensionShell.SetEnable(AEnable: Boolean);
begin
  //Запомнить состояние
  FEnable := AEnable;

  //Поправить подписчиков событий
  FSubKeyDown.Enable := FEnable;
  FSubKeyUp.Enable := FEnable;
  FSubKeyUp.Enable := FEnable;
end;


class function TsgeExtensionShell.GetName: String;
begin
  Result := Extension_Controllers;
end;


constructor TsgeExtensionShell.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Создать объекты

    //Задать параметры
    FEnable := False;

    //Подписать обработчики
    RegisterEventHandlers;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionShell.Destroy;
begin
  //Отписать подписчиков
  UnRegisterEventHandlers;


  inherited Destroy;
end;


end.


