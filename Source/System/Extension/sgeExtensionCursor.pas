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
  sgeEventBase, sgeEventMouse, sgeEventGraphic,
  sgeDisplayLayer, sgeDisplayElementAnimation, sgeCursor;


const
  Extension_Cursor = 'Cursor';


type
  TsgeExtensionCursor = class(TsgeExtensionBase)
  private
    FExtWindow: TsgeExtensionWindow;              //Ссылка на расширение окна
    FExtGraphic: TsgeExtensionGraphic;            //Ссылка на расширение графики

    FDrawLayer: TsgeDisplayLayer;                 //Объект управления слоем графики
    FDIsplayElement: TsgeDisplayElementAnimation; //Объект управления анимацией

    FShowCursor: Boolean;                         //Показывать курсор
    FCursor: TsgeCursor;                          //Текущий курсор
    FCursorPos: TsgeIntPoint;                     //Последние координаты курсора
    FScale: Single;                               //Масштаб курсора
    FLeftHand: Boolean;                           //Курсор для левшей

    //procedure UpdateCursor;                       //Поправить параметры курсора
    procedure CorrectCoordinate;                  //Поправить координаты
    procedure CorrectVisible(AVisible: Boolean);  //Поправить видимость курсора
    procedure SetDisplayElementVisible(AVisible: Boolean);

    function Handler_MouseMove(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
    function Handler_MouseEnter(EventObj: TsgeEventMouse): TsgeEventHandlerResult;
    function Handler_MouseLeave(EventObj: TsgeEventMouse): TsgeEventHandlerResult;

    procedure SetCursor(ACursor: TsgeCursor);
    procedure SetShowCursor(AShow: Boolean);
    procedure SetScale(AScale: Single);
    //procedure SetLeftHanded(ALeft: Boolean);
  protected
    function  GetName: String; override;
    procedure RegisterEventHandlers; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    property ShowCursor: Boolean read FShowCursor write SetShowCursor;
    property Cursor: TsgeCursor read FCursor write SetCursor;
    property Scale: Single read FScale write SetScale;
    //property LeftHanded: Boolean read FLeftHand write SetLeftHanded;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;


const
  Layer_Name = 'System.Cursor';
  Layer_Index = $FFFF;

  _UNITNAME = 'ExtensionCursors';



{procedure TsgeExtensionCursor.UpdateCursor;
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
end;}


procedure TsgeExtensionCursor.CorrectCoordinate;
begin
  if FCursor = nil then
    Exit;

  //Изменить положение
  FDIsplayElement.Position := sgeGetFloatPoint(FCursorPos.X, FCursorPos.Y);
  //FDIsplayElement.UpdateAnimation;

  //Обновить
  FDIsplayElement.Update;
end;


procedure TsgeExtensionCursor.CorrectVisible(AVisible: Boolean);
begin
  case AVisible of
    True:
      if FCursor = nil then
      begin
        FExtWindow.ShowCursor := True;
        SetDisplayElementVisible(False);
      end
      else
      begin
        FExtWindow.ShowCursor := False;
        SetDisplayElementVisible(True);
      end;

    False:
    begin
      FExtWindow.ShowCursor := False;
      SetDisplayElementVisible(False);
    end;
  end;
end;


procedure TsgeExtensionCursor.SetDisplayElementVisible(AVisible: Boolean);
begin
  if FDIsplayElement = nil then
    Exit;

  FDIsplayElement.Visible := AVisible;
  FDIsplayElement.Update;
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

  //Создать новый объект отображения
  if (FCursor = nil) and (ACursor <> nil) then
  begin
    FDIsplayElement := TsgeDisplayElementAnimation.Create(
      FCursorPos.X,
      FCursorPos.Y,
      ACursor.Width,
      ACursor.Height,
      ACursor.Sprite,
      ACursor.Frames
    );

    //Поправить масштаб
    FDIsplayElement.Scale := FScale;

    //Добавить в графику
    FDIsplayElement.Add(Layer_Name);
  end;


  //Изменить объект отображения
  if (FCursor <> nil) and (ACursor <> nil) then
  begin
    //Изменить поля
    FDIsplayElement.Sprite := ACursor.Sprite;
    FDIsplayElement.Frames := ACursor.Frames;

    //Обновить
    FDIsplayElement.Update;
  end;


  //Удалить объект отображения
  if (FCursor <> nil) and (ACursor = nil) then
  begin
    FDIsplayElement.Delete;
    sgeFreeAndNil(FDIsplayElement);
  end;


  //Сохранить
  FCursor := ACursor;

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
  //Сохранить объект
  FScale := AScale;

  //Проверить на пустой объект
  if FDIsplayElement = nil then
    Exit;

  //Изменить параметры
  FDIsplayElement.Scale := FScale;

  //Обновить
  FDIsplayElement.Update;
end;


{procedure TsgeExtensionCursor.SetLeftHanded(ALeft: Boolean);
var
  X: Single;
begin
  if FLeftHand = ALeft then
    Exit;

  FLeftHand := ALeft;

  X := FScale;
  if FLeftHand then
    X := -FScale;

  FDIsplayElement.ScaleX := X;
  FDIsplayElement.Update;
end;}


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
  if FDIsplayElement <> nil then
    FDIsplayElement.Free;

  FDrawLayer.Delete;
  FDrawLayer.Free;
end;



end.

