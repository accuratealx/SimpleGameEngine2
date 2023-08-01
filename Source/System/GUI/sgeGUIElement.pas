{
Пакет             Simple Game Engine 2
Файл              sgeGUIElement.pas
Версия            1.0
Создан            25.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Базовый элемент
}
{$Include Defines.inc}

unit sgeGUIElement;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeTemplateCollection,
  sgeEventBase, sgeEventMouse, sgeEventKeyboard;

const
  Layer_GUI_Name = 'System.GUI';


type
  TsgeGUIElement = class;
  TsgeGUIElementList = class;


  //Обработчики событий
  TsgeGUIProcEvent = procedure(Obj: TsgeGUIElement) of Object;
  TsgeGUIProcMouseEvent = procedure(Obj: TsgeGUIElement; Mouse: TsgeEventMouse) of object;
  TsgeGUIProcButtonEvent = procedure(Obj: TsgeGUIElement; Keyboard: TsgeEventKeyboard) of object;
  TsgeGUIProcButtonCharEvent = procedure(Obj: TsgeGUIElement; Keyboard: TsgeEventKeyboardChar) of object;


  //Тип обработчика мыши
  TsgeGUIElementMouseEventType = (emetDown, emetUp, emetMove, emetScroll, emetEnter, emetLeave, emetDblClick);


  //Тип обработчика клавиатуры
  TsgeGUIElementButtonEventType = (ebetDown, ebetUp, ebetChar);


  //Базовый элемент
  TsgeGUIElement = class
  protected
    //Параметры
    FParent: TsgeGUIElement;
    FChildList: TsgeGUIElementList;

    FName: String;
    FEnable: Boolean;               //Флаг обработки событий пользователя
    FFocused: Boolean;              //Фокус ввода
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FVisible: Boolean;
    FAutoSize: Boolean;             //Авторазмер элемента, для компонентов с текстом
    FScale: Single;
    FClickButton: TsgeMouseButton;  //Кнопка мыши для Click
    FEnableDoubleClick: Boolean;    //Доступность двойного клика

    //Вспомогательные параметры
    FPressed: Boolean;

    //Обработчики событий
    FOnShow: TsgeGUIProcEvent;
    FOnHide: TsgeGUIProcEvent;
    FOnSetFocus: TsgeGUIProcEvent;
    FOnLostFocus: TsgeGUIProcEvent;
    FOnMouseClick: TsgeGUIProcMouseEvent;
    FOnMouseDoubleClick: TsgeGUIProcMouseEvent;
    FOnMouseMove: TsgeGUIProcMouseEvent;
    FOnMouseDown: TsgeGUIProcMouseEvent;
    FOnMouseUp: TsgeGUIProcMouseEvent;
    FOnMouseLeave: TsgeGUIProcMouseEvent;
    FOnMouseEnter: TsgeGUIProcMouseEvent;
    FOnMouseScroll: TsgeGUIProcMouseEvent;
    FOnButtonDown: TsgeGUIProcButtonEvent;
    FOnButtonUp: TsgeGUIProcButtonEvent;
    FOnButtonChar: TsgeGUIProcButtonCharEvent;

    procedure Handler_Show; virtual;
    procedure Handler_Hide; virtual;
    procedure Handler_SetFocus; virtual;
    procedure Handler_LostFocus; virtual;
    procedure Handler_MouseClick(Mouse: TsgeEventMouse); virtual;
    procedure Handler_MouseDoubleClick(Mouse: TsgeEventMouse); virtual;
    procedure Handler_MouseMove(Mouse: TsgeEventMouse); virtual;
    procedure Handler_MouseDown(Mouse: TsgeEventMouse); virtual;
    procedure Handler_MouseUp(Mouse: TsgeEventMouse); virtual;
    procedure Handler_MouseLeave(Mouse: TsgeEventMouse); virtual;
    procedure Handler_MouseEnter(Mouse: TsgeEventMouse); virtual;
    procedure Handler_MouseScroll(Mouse: TsgeEventMouse); virtual;
    procedure Handler_ButtonDown(Keyboard: TsgeEventKeyboard); virtual;
    procedure Handler_ButtonUp(Keyboard: TsgeEventKeyboard); virtual;
    procedure Handler_ButtonChar(Keyboard: TsgeEventKeyboardChar); virtual;

    //Вспомогательные методы
    function  GetGlobalPos: TsgeIntPoint;                                   //Получить глобальные координаты
    procedure ChangeSize(NewWidth, NewHeight: Integer);                     //Изменить размеры элемента
    function  GetTopParent: TsgeGUIElement;                                 //Получить ссылку на форму
    procedure CalculateAutosize(var NewWidth, NewHeight: Integer); virtual; //Расчёт авторазмера
    procedure CheckMinimalSize(var NewWidth, NewHeight: Integer); virtual;  //Проверка наименьших размеров

    //Управление отображением
    procedure CorrectDisplayElementSizeAndPosition;
    procedure CorrectDisplayElementPosition; virtual;
    procedure CorrectDisplayElementSize; virtual;
    procedure DisplayElement_CorrectPosition(RealLeft, RealTop: Integer); virtual;
    procedure DisplayElement_CorrectSize(Width, Height: Integer); virtual;
    procedure DisplayElement_CorrectVisible(Visible: Boolean); virtual;

    //Дети
    procedure AddChild(Element: TsgeGUIElement);
    procedure DeleteChild(Element: TsgeGUIElement);
    procedure DestroyChild;
    procedure CorrectChildSizeAndPos;
    procedure CorrectChildPos;
    procedure CorrectChildSize;
    procedure CorrectChildVisible(AVisible: Boolean); //Переделать по человечески

    //Свойства
    procedure SetParent(AParent: TsgeGUIElement); virtual;
    procedure SetEnable(AEnabled: Boolean); virtual;
    procedure SetFocused(AFocused: Boolean); virtual;
    procedure SetLeft(ALeft: Integer); virtual;
    procedure SetTop(ATop: Integer); virtual;
    procedure SetPos(APos: TsgeIntPoint); virtual;
    function  GetPos: TsgeIntPoint; virtual;
    procedure SetWidth(AWidth: Integer); virtual;
    procedure SetHeight(AHeight: Integer); virtual;
    procedure SetVisible(AVisible: Boolean); virtual;
    procedure SetAutoSize(AAutoSize: Boolean); virtual;
    procedure SetScale(AScale: Single); virtual;
    function  GetScale: Single; virtual;
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Visible: Boolean = True; Parent: TsgeGUIElement = nil);
    destructor  Destroy; override;

    procedure MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse); virtual;
    function  ButtonHandler(EventType: TsgeGUIElementButtonEventType; Keyboard: TsgeEventBase): Boolean; virtual;
    function  PointInElement(X, Y: Integer): Boolean;

    //Свойства
    property Parent: TsgeGUIElement read FParent write SetParent;
    property Enable: Boolean read FEnable write SetEnable;
    property Focused: Boolean read FFocused write SetFocused;
    property Name: String read FName write FName;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Pos: TsgeIntPoint read GetPos write SetPos;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Visible: Boolean read FVisible write SetVisible;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Scale: Single read GetScale write SetScale;
    property ClickButton: TsgeMouseButton read FClickButton write FClickButton;

    //Обработчики
    property OnShow: TsgeGUIProcEvent read FOnShow write FOnShow;
    property OnHide: TsgeGUIProcEvent read FOnHide write FOnHide;
    property OnSetFocus: TsgeGUIProcEvent read FOnSetFocus write FOnSetFocus;
    property OnLostFocus: TsgeGUIProcEvent read FOnLostFocus write FOnLostFocus;
    property OnMouseClick: TsgeGUIProcMouseEvent read FOnMouseClick write FOnMouseClick;
    property OnMouseDoubleClick: TsgeGUIProcMouseEvent read FOnMouseDoubleClick write FOnMouseDoubleClick;
    property OnMouseMove: TsgeGUIProcMouseEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TsgeGUIProcMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TsgeGUIProcMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseLeave: TsgeGUIProcMouseEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TsgeGUIProcMouseEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseScroll: TsgeGUIProcMouseEvent read FOnMouseScroll write FOnMouseScroll;
    property OnButtonDown: TsgeGUIProcButtonEvent read FOnButtonDown write FOnButtonDown;
    property OnButtonUp: TsgeGUIProcButtonEvent read FOnButtonUp write FOnButtonUp;
    property OnButtonChar: TsgeGUIProcButtonCharEvent read FOnButtonChar write FOnButtonChar;
  end;


  //Список элементов
  TsgeGUIElementList = class(specialize TsgeTemplateCollection<TsgeGUIElement>)
  public
    function  IndexOf(Element: TsgeGUIElement): Integer;
    procedure Delete(Index: Integer);
    procedure Delete(Element: TsgeGUIElement);
  end;


implementation

uses
  sgeCorePointerUtils;


procedure TsgeGUIElement.Handler_Show;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;


procedure TsgeGUIElement.Handler_Hide;
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;


procedure TsgeGUIElement.Handler_SetFocus;
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Self);
end;


procedure TsgeGUIElement.Handler_LostFocus;
begin
  if Assigned(FOnLostFocus) then
    FOnLostFocus(Self);
end;


procedure TsgeGUIElement.Handler_MouseClick(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseClick) then
    FOnMouseClick(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseDoubleClick(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseDoubleClick) then
    FOnMouseDoubleClick(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseMove(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseDown(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseUp(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseLeave(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseEnter(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseScroll(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseScroll) then
    FOnMouseScroll(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_ButtonDown(Keyboard: TsgeEventKeyboard);
begin
  if Assigned(FOnButtonDown) then
    FOnButtonDown(Self, Keyboard);
end;


procedure TsgeGUIElement.Handler_ButtonUp(Keyboard: TsgeEventKeyboard);
begin
  if Assigned(FOnButtonUp) then
    FOnButtonUp(Self, Keyboard);
end;


procedure TsgeGUIElement.Handler_ButtonChar(Keyboard: TsgeEventKeyboardChar);
begin
  if Assigned(FOnButtonChar) then
    FOnButtonChar(Self, Keyboard);
end;


function TsgeGUIElement.GetGlobalPos: TsgeIntPoint;
var
  Pt: TsgeIntPoint;
  FormScale: Single;
  E, P: TsgeGUIElement;
begin
  if FParent = nil then
    Result := sgeGetIntPoint(FLeft, FTop)
  else
  begin
    //Координаты текущего элемента
    Pt := sgeGetIntPoint(FLeft, FTop);

    //Ссылка на родителя
    E := FParent;
    while E <> nil do
    begin
      //Прибавить смещение родителя
      Pt.X := Pt.X + E.Left;
      Pt.Y := Pt.Y + E.Top;

      //Следующий родитель
      E := E.FParent;
    end;

    //Отнять смещение формы
    P := GetTopParent;
    Pt.X := Pt.X - P.Left;
    Pt.Y := Pt.Y - P.Top;

    //Масштаб
    FormScale := GetScale;

    //Результат
    Result.X := P.Left + Round(Pt.X * FormScale);
    Result.Y := P.Top + Round(Pt.Y * FormScale);
  end;
end;


procedure TsgeGUIElement.ChangeSize(NewWidth, NewHeight: Integer);
begin
  //Проверить авторазмер
  if FAutoSize then
    CalculateAutosize(NewWidth, NewHeight);

  //Проверить ограничение размера
  //FConstrains.Check(NewWidth, NewHeight);

  //Проверку на наименьший размер
  CheckMinimalSize(NewWidth, NewHeight);

  //Сохранить новые размеры
  FWidth := NewWidth;
  FHeight := NewHeight;

  //Поправить размеры
  CorrectDisplayElementSize;
end;


function TsgeGUIElement.GetTopParent: TsgeGUIElement;
begin
  Result := Self;
  if FParent <> nil then
    Result := FParent.GetTopParent;
end;


procedure TsgeGUIElement.CalculateAutosize(var NewWidth, NewHeight: Integer);
begin
  //Заглушка, переопределяется в потомке
end;


procedure TsgeGUIElement.CheckMinimalSize(var NewWidth, NewHeight: Integer);
begin
  //Заглушка, переопределяется в потомке
end;


procedure TsgeGUIElement.CorrectDisplayElementSizeAndPosition;
begin
  CorrectDisplayElementPosition;
  CorrectDisplayElementSize;
end;


procedure TsgeGUIElement.CorrectDisplayElementPosition;
var
  RealPos: TsgeIntPoint;
begin
  RealPos := GetGlobalPos;

  DisplayElement_CorrectPosition(RealPos.X, RealPos.Y);
end;


procedure TsgeGUIElement.CorrectDisplayElementSize;
var
  RealWidth, RealHeight: Integer;
  FormScale: Single;
begin
  //Масштаб формы
  FormScale := GetScale;

  //Реальные размеры
  RealWidth := Round(FWidth * FormScale);
  RealHeight := Round(FHeight * FormScale);

  //Поправить размеры DisplayElement
  DisplayElement_CorrectSize(RealWidth, RealHeight);
end;


procedure TsgeGUIElement.DisplayElement_CorrectPosition(RealLeft, RealTop: Integer);
begin
  //Заглушка, переопределяется в потомке
end;


procedure TsgeGUIElement.DisplayElement_CorrectSize(Width, Height: Integer);
begin
  //Заглушка, переопределяется в потомке
end;


procedure TsgeGUIElement.DisplayElement_CorrectVisible(Visible: Boolean);
begin
  //Заглушка, переопределяется в потомке
end;


procedure TsgeGUIElement.AddChild(Element: TsgeGUIElement);
begin
  FChildList.Add(Element);
end;


procedure TsgeGUIElement.DeleteChild(Element: TsgeGUIElement);
begin
  FChildList.Delete(Element);
end;


procedure TsgeGUIElement.DestroyChild;
begin
  if FChildList.Count = 0 then
    Exit;

  //Удалить объекты
  while FChildList.Count > 0 do
  begin
    FChildList.Item[0].Free;
  end;
end;


procedure TsgeGUIElement.CorrectChildSizeAndPos;
begin
  CorrectChildPos;
  CorrectChildSize;
end;


procedure TsgeGUIElement.CorrectChildPos;
var
  i: Integer;
  Item: TsgeGUIElement;
begin
  for i := 0 to FChildList.Count - 1 do
  begin
    //Изменить положение ребенка
    Item := FChildList.Item[i];
    Item.CorrectDisplayElementPosition;

    //Изменить положение детей ребенка
    Item.CorrectChildPos;
  end;
end;


procedure TsgeGUIElement.CorrectChildSize;
var
  i: Integer;
  Item: TsgeGUIElement;
begin
  for i := 0 to FChildList.Count - 1 do
  begin
    //Изменить положение ребенка
    Item := FChildList.Item[i];
    Item.CorrectDisplayElementSize;

    //Изменить положение детей ребенка
    Item.CorrectChildSize;
  end;
end;


procedure TsgeGUIElement.CorrectChildVisible(AVisible: Boolean);
var
  i: Integer;
  Item: TsgeGUIElement;
begin
  for i := 0 to FChildList.Count - 1 do
  begin
    //Изменить положение ребенка
    Item := FChildList.Item[i];
    Item.DisplayElement_CorrectVisible(AVisible);

    //Изменить положение детей ребенка
    Item.CorrectChildVisible(AVisible);
  end;
end;


procedure TsgeGUIElement.SetParent(AParent: TsgeGUIElement);
begin
  if FParent = AParent then
    Exit;

  if FParent <> nil then
    FParent.DeleteChild(Self);

  FParent := AParent;

  if FParent <> nil then
    FParent.AddChild(Self);

  //Поправить себя
  CorrectDisplayElementSizeAndPosition;

  //Поправить детей
  CorrectChildSizeAndPos;
end;


procedure TsgeGUIElement.SetEnable(AEnabled: Boolean);
begin
  if FEnable = AEnabled then
    Exit;

  FEnable := AEnabled;

  //Если элемент неактивен, то убрать монопольный захват мыши
  if not FEnable then
    sgeCorePointer_GetSGE.ExtGUI.ReleaseMouse(Self);
end;


procedure TsgeGUIElement.SetFocused(AFocused: Boolean);
begin
  if FFocused = AFocused then
    Exit;

  FFocused := AFocused;

  if FFocused then
    Handler_SetFocus
  else
    Handler_LostFocus;
end;


procedure TsgeGUIElement.SetLeft(ALeft: Integer);
begin
  if FLeft = ALeft then
    Exit;

  FLeft := ALeft;

  CorrectDisplayElementPosition;

  CorrectChildPos;
end;


procedure TsgeGUIElement.SetTop(ATop: Integer);
begin
  if FTop = ATop then
    Exit;

  FTop := ATop;

  CorrectDisplayElementPosition;

  CorrectChildPos;
end;


procedure TsgeGUIElement.SetPos(APos: TsgeIntPoint);
begin
  if (FLeft = APos.X) and (FTop = APos.Y) then
    Exit;

  FLeft := APos.X;
  FTop := APos.Y;

  CorrectDisplayElementPosition;

  CorrectChildPos;
end;


function TsgeGUIElement.GetPos: TsgeIntPoint;
begin
  Result := sgeGetIntPoint(FLeft, FTop);
end;


procedure TsgeGUIElement.SetWidth(AWidth: Integer);
begin
  if AWidth < 0 then
    AWidth := 0;
  if FWidth = AWidth then
    Exit;

  ChangeSize(AWidth, FHeight);
end;


procedure TsgeGUIElement.SetHeight(AHeight: Integer);
begin
  if AHeight < 0 then
    AHeight := 0;
  if FHeight = AHeight then
    Exit;

  FHeight := AHeight;
  ChangeSize(FWidth, AHeight);
end;


procedure TsgeGUIElement.SetVisible(AVisible: Boolean);
begin
  if FVisible = AVisible then
    Exit;

  FVisible := AVisible;

  //Выполнить обработчики
  if FVisible then
    Handler_Show
  else
    Handler_Hide;

  //Поправить себя
  DisplayElement_CorrectVisible(FVisible);

  //Поправить детей
  CorrectChildVisible(FVisible);
end;


procedure TsgeGUIElement.SetAutoSize(AAutoSize: Boolean);
begin
  if FAutoSize = AAutoSize then
    Exit;

  FAutoSize := AAutoSize;

  ChangeSize(FWidth, FHeight);
end;


procedure TsgeGUIElement.SetScale(AScale: Single);
begin
  FScale := AScale;

  //Поправить размеры отображаемого элемента
  CorrectDisplayElementSize;

  //Поправить детей
  CorrectChildSizeAndPos;
end;


function TsgeGUIElement.GetScale: Single;
begin
  Result := FScale;

  if FParent <> nil then
    Result := FParent.GetScale;
end;


constructor TsgeGUIElement.Create(Name: String; Left, Top, Width, Height: Integer; Visible: Boolean; Parent: TsgeGUIElement);
begin
  //Создать объекты
  FChildList := TsgeGUIElementList.Create(False);

  //Запомнить параметры
  FName := Name;
  FLeft := Left;
  FTop := Top;
  FVisible := False;
  FAutoSize := False;
  FScale := 1;
  FEnable := True;
  FFocused := False;
  FClickButton := mbLeft;
  FEnableDoubleClick := True;

  //Установить родителя
  SetParent(Parent);

  //Поправить размеры
  ChangeSize(Width, Height);

  //Поправить положение
  //CorrectPosition;

  //Поправить видимость
  SetVisible(Visible);
end;


destructor TsgeGUIElement.Destroy;
begin
  //Отключить захват мыши
  sgeCorePointer_GetSGE.ExtGUI.ReleaseMouse(Self);

  //Убрать фокус с элемента
  sgeCorePointer_GetSGE.ExtGUI.LostFocus(Self);

  //Почистить детей
  DestroyChild;
  FChildList.Free;

  //Удалить себя у родителя
  if FParent <> nil then
    FParent.DeleteChild(Self);
end;


procedure TsgeGUIElement.MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse);
var
  LocalMouse: TsgeEventMouse;
  Pt: TsgeIntPoint;
  X, Y: Integer;
  FormScale: Single;
begin
  //Если неактивен, то выход
  if not FVisible or not FEnable then
    Exit;

  //Реальное положение на экране
  Pt := GetGlobalPos;

  //Изменить координаты мыши под текущим элементом
  if FParent = nil then
  begin
    X := Mouse.X - Pt.X;
    Y := Mouse.Y - Pt.Y;
  end
  else
  begin
    FormScale := GetScale;
    X := Round((Mouse.X - Pt.X) / FormScale);
    Y := Round((Mouse.Y - Pt.Y) / FormScale);
  end;

  //Локальный объект события мыши
  LocalMouse := TsgeEventMouse.Create(Mouse.Name, X, Y, Mouse.MouseButtons, Mouse.KeyboardButtons, Mouse.Delta);

  try
    //Обработать событие
    case EventType of
      emetDown:
      begin
        if PointInElement(Mouse.X, Mouse.Y) then
        begin
          //Запомнить флаг нажатия, если нужная кнопка
          if FClickButton in Mouse.MouseButtons then
          begin
            FPressed := True;
            sgeCorePointer_GetSGE.ExtGUI.MouseCapture(Self);
          end;

          //Обработчик нажатия мыши
          Handler_MouseDown(LocalMouse);

          //Установить фокус ввода
          sgeCorePointer_GetSGE.ExtGUI.SetFocus(Self);
        end;
      end;

      emetUp:
      begin
        if PointInElement(Mouse.X, Mouse.Y) then
        begin
          //Событие OnClick
          if FPressed then
            Handler_MouseClick(LocalMouse);
        end;

        //Обработчик отпускания мыши
        Handler_MouseUp(LocalMouse);

        //Сбросить флаг нажатия
        FPressed := False;
        sgeCorePointer_GetSGE.ExtGUI.ReleaseMouse(Self);
      end;

      emetMove:
         Handler_MouseMove(LocalMouse);

      emetScroll:
        Handler_MouseScroll(LocalMouse);

      emetDblClick:
        if PointInElement(Mouse.X, Mouse.Y) then
        begin
          if FEnableDoubleClick then
            Handler_MouseDoubleClick(LocalMouse)
          else
            MouseHandler(emetDown, Mouse);
        end;

      emetLeave:
        Handler_MouseLeave(LocalMouse);

      emetEnter:
        Handler_MouseEnter(LocalMouse);
    end;

  finally
    LocalMouse.Free;
  end;
end;


function TsgeGUIElement.ButtonHandler(EventType: TsgeGUIElementButtonEventType; Keyboard: TsgeEventBase): Boolean;
begin
  Result := False;
  if (not FVisible) or (not FEnable) then
    Exit;

  Result := True;

  case EventType of
    ebetDown:
      Handler_ButtonDown(TsgeEventKeyboard(Keyboard));

    ebetUp:
      Handler_ButtonUp(TsgeEventKeyboard(Keyboard));

    ebetChar:
      Handler_ButtonChar(TsgeEventKeyboardChar(Keyboard));
  end;
end;


function TsgeGUIElement.PointInElement(X, Y: Integer): Boolean;
var
  Pt: TsgeIntPoint;
  FormScale: Single;
  W, H: Integer;
begin
  //Глобальные координаты c учётом масштаба
  Pt := GetGlobalPos;

  //Масштаб
  FormScale := GetScale;

  //Размеры элементы
  W := Round(FWidth * FormScale);
  H := Round(FHeight * FormScale);

  //Проверить попадание
  Result := (X >= Pt.X) and (X <= Pt.X + W) and (Y >= Pt.Y) and (Y <= Pt.Y + H);
end;


{$Region TsgeGUIElementList}
function TsgeGUIElementList.IndexOf(Element: TsgeGUIElement): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FCount - 1 do
    if FList[i] = Element then
      Exit(i);
end;


procedure TsgeGUIElementList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;


procedure TsgeGUIElementList.Delete(Element: TsgeGUIElement);
var
  Idx: Integer;
begin
  Idx := IndexOf(Element);
  if Idx <> -1 then
    Delete(Idx);
end;
{$EndRegion TsgeGUIElementList}


end.

