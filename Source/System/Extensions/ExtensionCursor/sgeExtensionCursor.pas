{
Пакет             Simple Game Engine 2
Файл              sgeExtensionCursor.pas
Версия            1.1
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
  sgeExtensionBase, sgeExtensionWindow, sgeExtensionGraphic,
  sgeEventBase, sgeEventMouseMove, sgeEventMouseEnter, sgeEventMouseLeave,
  sgeDisplayLayer, sgeDisplayElementAnimationUnmanaged, sgeCursor;


const
  Extension_Cursor = 'Cursor';


type
  TsgeExtensionCursor = class(TsgeExtensionBase)
  private
    FExtWindow: TsgeExtensionWindow;    //Ссылка на расширение окна
    FExtGraphic: TsgeExtensionGraphic;  //Ссылка на расширение графики

    FDrawLayer: TsgeDisplayLayer;       //Объект управления слоем графики
    FDisplayElement: TsgeDisplayElementAnimationUnmanaged;  //Объект управления анимацией

    FShowCursor: Boolean;               //Показывать курсор
    FCursor: TsgeCursor;                //Текущий курсор
    FCursorPos: TsgeIntPoint;           //Последние координаты курсора
    FScale: Single;                     //Масштаб курсора
    FLeftHand: Boolean;                 //Курсор для левшей

    procedure CorrectCoordinate;
    procedure CorrectVisible(AVisible: Boolean);
    procedure SetDisplayElementVisible(AVisible: Boolean);
    procedure CorrectLeftHand(ALeftHand: Boolean; X: Integer);

    function Handler_MouseMove(EventObj: TsgeEventMouseMove): TsgeEventHandlerResult;
    function Handler_MouseEnter(EventObj: TsgeEventMouseEnter): TsgeEventHandlerResult;
    function Handler_MouseLeave(EventObj: TsgeEventMouseLeave): TsgeEventHandlerResult;

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
  sgeErrors, sgeSystemUtils;


const
  Layer_Name = 'System.Cursor';
  Layer_Index = $FFFF;

  _UNITNAME = 'ExtensionCursors';


procedure TsgeExtensionCursor.CorrectCoordinate;
begin
  if FCursor = nil then
    Exit;

  //Изменить положение
  FDisplayElement.Position := sgeGetFloatPoint(FCursorPos.X, FCursorPos.Y);

  //Обновить
  FDisplayElement.Update;
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
  if FDisplayElement = nil then
    Exit;

  FDisplayElement.Visible := AVisible;
  FDisplayElement.Update;
end;


procedure TsgeExtensionCursor.CorrectLeftHand(ALeftHand: Boolean; X: Integer);
begin
  //Поправить параметры
  if ALeftHand then
  begin
    FDisplayElement.Reflect := [rHorizontal];
    FDisplayElement.OriginX := FDisplayElement.Width - X;
  end
  else
  begin
    FDisplayElement.Reflect := [];
    FDisplayElement.OriginX := X;
  end;
end;


function TsgeExtensionCursor.Handler_MouseMove(EventObj: TsgeEventMouseMove): TsgeEventHandlerResult;
begin
  Result := ehrNormal;

  //Запомнить последние координаты курсора
  FCursorPos := EventObj.Pos;

  //Поправить графический примитив
  CorrectCoordinate;
end;


function TsgeExtensionCursor.Handler_MouseEnter(EventObj: TsgeEventMouseEnter): TsgeEventHandlerResult;
begin
  Result := ehrNormal;

  //Восстановить курсор
  CorrectVisible(FShowCursor);
end;


function TsgeExtensionCursor.Handler_MouseLeave(EventObj: TsgeEventMouseLeave): TsgeEventHandlerResult;
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
    FDisplayElement := TsgeDisplayElementAnimationUnmanaged.Create(
      FCursorPos.X,
      FCursorPos.Y,
      ACursor.Width,
      ACursor.Height,
      ACursor.Sprite,
      ACursor.Frames
    );

    //Поправить смещение для горячей точки
    FDisplayElement.Origin := sgeGetFloatPoint(ACursor.HotPoint.X, ACursor.HotPoint.Y);

    //Поправить отражение
    CorrectLeftHand(FLeftHand, ACursor.HotPoint.X);

    //Поправить масштаб
    FDisplayElement.Scale := FScale;

    //Добавить в графику
    FDisplayElement.Add(Layer_Name);
  end;


  //Изменить объект отображения
  if (FCursor <> nil) and (ACursor <> nil) then
  begin
    //Изменить поля
    FDisplayElement.Width := ACursor.Width;
    FDisplayElement.Height := ACursor.Height;
    FDisplayElement.Sprite := ACursor.Sprite;
    FDisplayElement.Frames := ACursor.Frames;

    //Поправить смещение для горячей точки
    FDisplayElement.Origin := sgeGetFloatPoint(ACursor.HotPoint.X, ACursor.HotPoint.Y);

    //Поправить отражение
    CorrectLeftHand(FLeftHand, ACursor.HotPoint.X);

    //Поправить масштаб
    FDisplayElement.Scale := FScale;

    //Обновить
    FDisplayElement.Update;
  end;


  //Удалить объект отображения
  if (FCursor <> nil) and (ACursor = nil) then
  begin
    FDisplayElement.Delete;
    sgeFreeAndNil(FDisplayElement);
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
  if FDisplayElement = nil then
    Exit;

  //Изменить параметры
  FDisplayElement.Scale := FScale;

  //Обновить
  FDisplayElement.Update;
end;


procedure TsgeExtensionCursor.SetLeftHanded(ALeft: Boolean);
begin
  if FLeftHand = ALeft then
    Exit;

  FLeftHand := ALeft;

  //Поправить отражение, если установлен курсор
  if Assigned(FCursor) then
  begin
    CorrectLeftHand(FLeftHand, FCursor.HotPoint.X);

    //Обновить
    FDisplayElement.Update;
  end;
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
  if FDisplayElement <> nil then
    FDisplayElement.Free;

  FDrawLayer.Free;
end;



end.

