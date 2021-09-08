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
  sgeExtensionBase, sgeEventWindow, sgeEventSubscriber, sgeGraphicElementLayer, sgeExtensionGraphic,
  sgeGUIFormList;

const
  Extension_GUI = 'GUI';


type
  TsgeExtensionGUI = class(TsgeExtensionBase)
  const
    MAX_SUB_COUNT = 5;
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



    property Enable: Boolean read FEnable write SetEnable;
    property Visible: Boolean read FVisible write SetVisible;

    property FormList: TsgeGUIFormList read FFormList;
  end;



implementation

uses
  sgeErrors, sgeStringList, sgeFileUtils, sgeOSPlatform, sgeEventBase;

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
  FEventSubscriber[0] := EventManager.Subscribe(Event_WindowKeyDown, TsgeEventHandler(@Handler_KeyDown), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[1] := EventManager.Subscribe(Event_WindowKeyUp, TsgeEventHandler(@Handler_KeyUp), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[2] := EventManager.Subscribe(Event_WindowChar, TsgeEventHandler(@Handler_KeyChar), EventPriorityMaxMinusTwo, True);
  //Мышь
  FEventSubscriber[3] := EventManager.Subscribe(Event_WindowMouseDown, TsgeEventHandler(@Handler_MouseDown), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[4] := EventManager.Subscribe(Event_WindowMouseUp, TsgeEventHandler(@Handler_MouseUp), EventPriorityMaxMinusTwo, True);
  FEventSubscriber[5] := EventManager.Subscribe(Event_WindowMouseScroll, TsgeEventHandler(@Handler_MouseWheel), EventPriorityMaxMinusTwo, True);
end;


procedure TsgeExtensionGUI.UnregisterEventHandlers;
begin
  EventManager.UnSubscribe(Self);
end;


function TsgeExtensionGUI.Handler_KeyDown(EventObj: TsgeEventWindowKeyboard): Boolean;
begin
  Result := False;
end;


function TsgeExtensionGUI.Handler_KeyUp(EventObj: TsgeEventWindowKeyboard): Boolean;
begin
  Result := False;
end;


function TsgeExtensionGUI.Handler_KeyChar(EventObj: TsgeEventWindowChar): Boolean;
begin
  Result := False;
end;


function TsgeExtensionGUI.Handler_MouseDown(EventObj: TsgeEventWindowMouse): Boolean;
begin
  Result := False;
end;


function TsgeExtensionGUI.Handler_MouseUp(EventObj: TsgeEventWindowMouse): Boolean;
begin
  Result := False;
end;


function TsgeExtensionGUI.Handler_MouseWheel(EventObj: TsgeEventWindowMouse): Boolean;
begin
  Result := False;
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

