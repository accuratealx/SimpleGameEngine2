{
Пакет             Simple Game Engine 2
Файл              sgeExtensionCursor.pas
Версия            1.0
Создан            03.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Курсоры
}
{$Include Defines.inc}

unit sgeExtensionCursor;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeExtensionBase, sgeExtensionWindow, sgeExtensionGraphic, sgeExtensionResourceList,
  {sgeGraphicElementAnimation,}
  sgeEventBase, sgeEventMouse, sgeEventGraphic,
  sgeDisplayLayer, sgeCursor;


const
  Extension_Cursor = 'Cursor';


type
  TsgeExtensionCursor = class(TsgeExtensionBase)
  private
    FExtWindow: TsgeExtensionWindow;                                //Ссылка на расширение окна
    FExtGraphic: TsgeExtensionGraphic;                              //Ссылка на расширение графики

    FDrawLayer: TsgeDisplayLayer;                                   //Объект управления слоем графики

    FShowCursor: Boolean;                                           //Показывать курсор
    FCursor: TsgeCursor;                                            //Текущий курсор
    FCursorPos: TsgeIntPoint;                                       //Последние координаты курсора
    FScale: Single;                                                 //Масштаб курсора
    FLeftHand: Boolean;                                             //Курсор для левшей

    procedure UpdateCursor;                                         //Поправить параметры курсора
    procedure CorrectCoordinate;                                    //Поправить координаты
    procedure CorrectVisible(Visible: Boolean);                     //Поправить видимость курсора

    function Handler_MouseMove(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
    function Handler_MouseEnter(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
    function Handler_MouseLeave(EventObj: TsgeEventMouse): TsgeEventHandlerResult;

    procedure SetCursor(ACursor: TsgeCursor);
    procedure SetShowCursor(AShow: Boolean);
    procedure SetScale(AScale: Single);
    procedure SetLeftHanded(ALeft: Boolean);
  protected
    function  GetName: String; override;
    procedure RegisterEventHandlers; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    property ShowCursor: Boolean read FShowCursor write SetShowCursor;
    property Cursor: TsgeCursor read FCursor write SetCursor;
    property Scale: Single read FScale write SetScale;
    property LeftHanded: Boolean read FLeftHand write SetLeftHanded;
  end;


implementation

uses
  sgeErrors;


const
  Layer_Name = 'System.Cursor';
  Layer_Index = $FFFF;

  _UNITNAME = 'ExtensionCursors';



procedure TsgeExtensionCursor.UpdateCursor;
var
  X: Single;
begin
  if FCursor = nil then
    Exit;

  //FGUIElement.Frames := FCursor.Frames;
  //FGUIElement.W := FCursor.Width;
  //FGUIElement.H := FCursor.Height;
  //FGUIElement.CoordType := gctNormal;

  X := FScale;
  if FLeftHand then
    X := -FScale;
  //FGUIElement.Scale := sgeGetFloatPoint(X, FScale);

  CorrectCoordinate;
end;


procedure TsgeExtensionCursor.CorrectCoordinate;
begin
  if FCursor = nil then
    Exit;

  //FGUIElement.X := FCursorPos.X - FCursor.HotPoint.X;
  //FGUIElement.Y := FCursorPos.Y - FCursor.HotPoint.Y;
  //FGUIElement.Update;
end;


procedure TsgeExtensionCursor.CorrectVisible(Visible: Boolean);
begin
  {case Visible of
    True:
      if FCursor = nil then
      begin
        FExtWindow.ShowCursor := True;
        FGUIElement.Visible := False;
      end
      else
      begin
        FExtWindow.ShowCursor := False;
        FGUIElement.Visible := True;
      end;

    False:
    begin
      FExtWindow.ShowCursor := False;
      FGUIElement.Visible := False;
    end;
  end;}
end;


function TsgeExtensionCursor.Handler_MouseMove(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
begin
  Result := ehrNormal;

  //Запомнить последние координаты курсора
  FCursorPos := EventObj.Pos;

  //Поправить графический примитив
  CorrectCoordinate;
end;


function TsgeExtensionCursor.Handler_MouseEnter(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
begin
  Result := ehrNormal;

  //Восстановить курсор
  CorrectVisible(FShowCursor);
end;


function TsgeExtensionCursor.Handler_MouseLeave(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
begin
  Result := ehrNormal;

  //Спрятать курсоры
  CorrectVisible(False);

  //Показать сиситемный если он скрыт
  if not FExtWindow.ShowCursor then
    FExtWindow.ShowCursor := true;
end;


procedure TsgeExtensionCursor.SetCursor(ACursor: TsgeCursor);
begin
  if FCursor = ACursor then
    Exit;

  FCursor := ACursor;

  //Поправить параметры курсора
  UpdateCursor;

  //Поправить видимость
  CorrectVisible(FShowCursor);
end;


procedure TsgeExtensionCursor.SetShowCursor(AShow: Boolean);
begin
  if FShowCursor = AShow then
    Exit;

  FShowCursor := AShow;

  //Поправить видимость
  CorrectVisible(FShowCursor);
end;


procedure TsgeExtensionCursor.SetScale(AScale: Single);
begin
  FScale := AScale;

  UpdateCursor;
end;


procedure TsgeExtensionCursor.SetLeftHanded(ALeft: Boolean);
begin
  if FLeftHand = ALeft then
    Exit;

  FLeftHand := ALeft;
  UpdateCursor;
end;


function TsgeExtensionCursor.GetName: String;
begin
  Result := Extension_Cursor;
end;


procedure TsgeExtensionCursor.RegisterEventHandlers;
begin
  EventManager.SubscriberGroupList.Subscribe(Event_MouseMove, TsgeEventHandler(@Handler_MouseMove), Event_Priority_Max);
  EventManager.SubscriberGroupList.Subscribe(Event_MouseEnter, TsgeEventHandler(@Handler_MouseEnter), Event_Priority_Max);
  EventManager.SubscriberGroupList.Subscribe(Event_MouseLeave, TsgeEventHandler(@Handler_MouseLeave), Event_Priority_Max);
end;


constructor TsgeExtensionCursor.Create;
begin
  try
    inherited Create;

    //Поиск указателей
    FExtWindow := TsgeExtensionWindow(GetExtension(Extension_Window));
    FExtGraphic := TsgeExtensionGraphic(GetExtension(Extension_Graphic));

    //Задать параметры
    FShowCursor := True;
    FScale := 1;
    FLeftHand := False;

    //Создать слой для вывода курсора
    FDrawLayer := TsgeDisplayLayer.Create(Layer_Name, Layer_Index, True);
    FDrawLayer.Add;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionCursor.Destroy;
begin
  FDrawLayer.Delete;
  FDrawLayer.Free;
end;



end.

