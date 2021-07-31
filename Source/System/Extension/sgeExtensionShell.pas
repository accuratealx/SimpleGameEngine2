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
  sgeThread,
  sgeExtensionBase, sgeEventWindow, sgeEventSubscriber,
  sgeShellCommandList, sgeLineEditor,
  sgeExtensionGraphic, sgeExtensionResourceList;


const
  Extension_Controllers = 'Shell';


type
  TsgeExtensionShell = class(TsgeExtensionBase)
  private
    FExtGraphic: TsgeExtensionGraphic;
    FExtResList: TsgeExtensionResourceList;

  private
    //Классы
    FThread: TsgeThread;
    FCommandList: TsgeShellCommandList;
    FEditor: TsgeLineEditor;

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

    procedure Draw;

    //Свойства
    procedure SetEnable(AEnable: Boolean);
  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;


    procedure LogError(Txt: String);                      //Вывести в журнал ошибку


    procedure DoCommand(Cmd: String);

    property Enable: Boolean read FEnable write SetEnable;
    property CommandList: TsgeShellCommandList read FCommandList;
  end;



implementation

uses
  sgeErrors, sgeEventBase, sgeKeys, sgeGraphicColor;

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

  //Обработать системные клавиши
  case EventObj.Key of
    keyEscape: Enable := False;

    else
      FEditor.ProcessKey(EventObj.Key, EventObj.KeyboardButtons);
  end;
end;


function TsgeExtensionShell.Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
begin
  Result := True;
end;


function TsgeExtensionShell.Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;
begin
  Result := True;

  //Отослать в редактор
  FEditor.ProcessChar(EventObj.Char, EventObj.KeyboardButtons);
end;


procedure TsgeExtensionShell.Draw;
begin
  if not FEnable then Exit;

  //Тут мы рисуем оболочку на экране
  with FExtGraphic.Graphic do
    begin
    PushAttrib;
    Color := cGray;
    DrawRect(0, 0, Width, FExtResList.Default.Font.Height + 10);
    Color := cWhite;
    DrawText(0, 0, FExtResList.Default.Font, FEditor.Line);
    PopAttrib;
    end;
end;


procedure TsgeExtensionShell.SetEnable(AEnable: Boolean);
begin
  //Запомнить состояние
  FEnable := AEnable;

  //Поправить подписчиков событий
  FSubKeyDown.Enable := FEnable;
  FSubKeyUp.Enable := FEnable;
  FSubKeyChar.Enable := FEnable;
end;


class function TsgeExtensionShell.GetName: String;
begin
  Result := Extension_Controllers;
end;


constructor TsgeExtensionShell.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Поиск указателей
    FExtGraphic := TsgeExtensionGraphic(GetExtension(Extension_Graphic));
    FExtResList := TsgeExtensionResourceList(GetExtension(Extension_ResourceList));

    //Создать объекты
    FThread := TsgeThread.Create;
    FCommandList := TsgeShellCommandList.Create;
    FEditor := TsgeLineEditor.Create;

    //Задать параметры
    FEnable := False;

    //Установить обработчик ошибок
    ErrorManager.ShellHandler := @LogError;

    //Подписать обработчики
    RegisterEventHandlers;

    //Установить метод отрисовки оболочки
    FExtGraphic.DrawShellproc := @Draw;
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionShell.Destroy;
begin
  //Отписать подписчиков
  UnRegisterEventHandlers;

  //Удалить объекты
  FThread.Free;
  FEditor.Free;
  FCommandList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionShell.LogError(Txt: String);
begin

end;


procedure TsgeExtensionShell.DoCommand(Cmd: String);
begin

end;


end.


