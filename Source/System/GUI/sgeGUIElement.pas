{
Пакет             Simple Game Engine 2
Файл              sgeGUIElement.pas
Версия            1.1
Создан            04.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Базовый элемент
}
{$Include Defines.inc}

unit sgeGUIElement;

{$mode objfpc}{$H+}

interface

uses
  sgeSystemUtils,
  sgeTypes, sgeSimpleParameters, sgeSimpleContainer, sgeTemplateCollection,
  sgeGraphicSprite, sgeGraphicColor,
  sgeEventBase, sgeEventKeyboard, sgeEventMouse,
  sgeGUIPropertyConstrains;


type
  TsgeGUIElementList = class;
  TsgeGUIElement = class;

  //Обработчики
  TsgeGUIProcEvent = procedure(Obj: TsgeGUIElement) of Object;
  TsgeGUIProcMouseEvent = procedure(Obj: TsgeGUIElement; Mouse: TsgeEventMouse) of object;
  TsgeGUIProcButtonEvent = procedure(Obj: TsgeGUIElement; Keyboard: TsgeEventKeyboard) of object;
  TsgeGUIProcButtonCharEvent = procedure(Obj: TsgeGUIElement; Keyboard: TsgeEventKeyboardChar) of object;


  //Состояние элемента
  TsgeGUIElementState = set of (esLockUpdate, esCorrectSize, esRepaint, esRepaintParent);


  //Тип обработчика мыши
  TsgeGUIElementMouseEventType = (emetDown, emetUp, emetMove, emetScroll, emetEnter, emetLeave, emetDblClick);


  //Тип обработчика клавиатуры
  TsgeGUIElementButtonEventType = (ebetDown, ebetUp, ebetChar);


  //Элемент
  TsgeGUIElement = class
  protected
    //Классы
    FCanvas: TsgeGraphicSprite;                                     //Холст для рисования
    FChildList: TsgeGUIElementList;                                 //Список детей

    //Ссылки
    FParent: TsgeGUIElement;                                        //Ссылка на родителя

    //Параметры
    FState: TsgeGUIElementState;                                    //Состояние элемента
    FName: ShortString;                                             //Имя элемента
    FVisible: Boolean;                                              //Видимость элемента
    FEnable: Boolean;                                               //Реагирование на действия пользователя
    FFocused: Boolean;                                              //Флаг активности
    FLeft: Integer;                                                 //X относительно родителя
    FTop: Integer;                                                  //Y относительно родителя
    FWidth: Integer;                                                //Ширина
    FHeight: Integer;                                               //Высота
    FAutoSize: Boolean;                                             //Авторазмер
    FScale: Single;                                                 //Масштаб элемента
    FClickButton: TsgeMouseButton;                                  //Кнопка мыши для Click
    FConstrains: TsgeGUIPropertyConstrainsExt;                      //Ограничение размеров
    FStyle: ShortString;                                            //Имя стиля

    //Вспомогательные параметры
    FPressed: Boolean;                                              //Флаг нажатия для Click

    //Обработчики событий
    FOnDrawBefore       : TsgeGUIProcEvent;
    FOnDrawAfter        : TsgeGUIProcEvent;
    FOnShow             : TsgeGUIProcEvent;
    FOnHide             : TsgeGUIProcEvent;
    FOnSetFocus         : TsgeGUIProcEvent;
    FOnLostFocus        : TsgeGUIProcEvent;
    FOnMouseClick       : TsgeGUIProcMouseEvent;
    FOnMouseDoubleClick : TsgeGUIProcMouseEvent;
    FOnMouseMove        : TsgeGUIProcMouseEvent;
    FOnMouseDown        : TsgeGUIProcMouseEvent;
    FOnMouseUp          : TsgeGUIProcMouseEvent;
    FOnMouseLeave       : TsgeGUIProcMouseEvent;
    FOnMouseEnter       : TsgeGUIProcMouseEvent;
    FOnMouseScroll      : TsgeGUIProcMouseEvent;
    FOnButtonDown       : TsgeGUIProcButtonEvent;
    FOnButtonUp         : TsgeGUIProcButtonEvent;
    FOnButtonChar       : TsgeGUIProcButtonCharEvent;

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
    procedure GetPrefferedSize(var NewWidth, NewHeight: Integer); virtual;  //Взять предпочтительный размер элемента
    procedure CalculateAutosize(var NewWidth, NewHeight: Integer); virtual; //Расчёт авторазмер
    procedure CheckMinimalSize(var NewWidth, NewHeight: Integer); virtual;  //Проверка наименьших размеров
    procedure Notify(Action: TsgeGUIElementState); virtual;         //Вызвать уведомление
    procedure DrawChild; virtual;                                   //Нарисовать детей
    class function GetParameterSectionName: String; virtual;        //Вернуть имя секции (У каждого класса своё имя)
    procedure LoadData(Data: TsgeSimpleParameters); virtual;        //Загрузить параметры из массива
    procedure LockUpdate;                                           //Запретить перерисовку
    procedure UnLockUpdate;                                         //Разрешить перерисовку

    //Отрисовка
    procedure GraphicPrepare; virtual;                              //Подготовить графику
    procedure GraphicRestore; virtual;                              //Восстановить графику
    procedure DrawBefore; virtual;
    procedure DrawAfter; virtual;

    //Свойства
    procedure SetLeft(ALeft: Integer); virtual;
    procedure SetTop(ATop: Integer); virtual;
    procedure SetWidth(AWidth: Integer); virtual;
    procedure SetHeight(AHeight: Integer); virtual;
    procedure SetDrawAfter(ADrawAfter: TsgeGUIProcEvent);
    procedure SetDrawBefore(ADrawBefore: TsgeGUIProcEvent);
    procedure SetBounds(ABounds: TsgeFloatRect); virtual;
    function  GetBounds: TsgeFloatRect; virtual;
    procedure SetParent(AParent: TsgeGUIElement);
    procedure SetEnable(AEnabled: Boolean); virtual;
    procedure SetVisible(AVisible: Boolean); virtual;
    procedure SetFocused(AFocused: Boolean); virtual;
    procedure SetAutoSize(AAutoSize: Boolean); virtual;
    procedure SetStyle(AStyle: ShortString); virtual;
    function  GetScaleWidth: Integer; virtual;
    function  GetScaleHeight: Integer; virtual;
    procedure SetScale(AScale: Single); virtual;
    function  GetScale: Single; virtual;

    function  GetConstrains: TsgeGUIPropertyConstrains;

    //Методы Parent
    procedure AddChild(Element: TsgeGUIElement);
    procedure DeleteChild(Element: TsgeGUIElement);
    procedure DestroyChild;
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); virtual;
    destructor  Destroy; override;

    //Обработчики событий
    procedure MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse); virtual;
    function  ButtonHandler(EventType: TsgeGUIElementButtonEventType; Keyboard: TsgeEventBase): Boolean; virtual;

    procedure Repaint;
    procedure Resize;
    procedure Draw; virtual;
    procedure LoadParameters(Params: TsgeSimpleContainer);          //Загрузить параметры

    //Вспомогательные методы
    function  PointInElement(X, Y: Integer): Boolean;               //Проверить нахождение точки внутри элемента
    function  GetTopParent: TsgeGUIElement;                         //Вернуть родителя верхнего уровня
    function  GetGlobalPos: TsgeIntPoint;                           //Узнать глобальные координаты элемента

    //Параметры
    property Canvas: TsgeGraphicSprite read FCanvas;
    property Name: ShortString read FName;
    property Enable: Boolean read FEnable write SetEnable;
    property Visible: Boolean read FVisible write SetVisible;
    property Focused: Boolean read FFocused write SetFocused;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Parent: TsgeGUIElement read FParent write SetParent;
    property Bounds: TsgeFloatRect read GetBounds write SetBounds;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Constrains: TsgeGUIPropertyConstrains read GetConstrains;
    property ClickButton: TsgeMouseButton read FClickButton write FClickButton;
    property ChildList: TsgeGUIElementList read FChildList;
    property Style: ShortString read FStyle write SetStyle;
    property ScaleWidth: Integer read GetScaleWidth;
    property ScaleHeight: Integer read GetScaleHeight;

    //Обработчики
    property OnDrawBefore: TsgeGUIProcEvent read FOnDrawBefore write SetDrawBefore;
    property OnDrawAfter: TsgeGUIProcEvent read FOnDrawAfter write SetDrawAfter;
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
  sgeCorePointerUtils,
  sgeGraphic, sgeGUIUtils;


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





//////////////////////////////////TsgeGUIElement//////////////////////////////////
procedure TsgeGUIElement.SetParent(AParent: TsgeGUIElement);
const
  mNewParent = 1;
  mChangeParent = 2;
  mClearParent = 3;
var
  Mode: Byte;
begin
  if FParent = AParent then
    Exit;

  Mode := 0;
  if (FParent = nil) and (AParent <> nil) then
    Mode := mNewParent;
  if (FParent <> nil) and (AParent <> nil) then
    Mode := mChangeParent;
  if (FParent <> nil) and (AParent = nil) then
    Mode := mClearParent;

  case Mode of
    mNewParent:
    begin
      FParent := AParent;
      FParent.AddChild(Self);
    end;

    mChangeParent:
    begin
      FParent.DeleteChild(Self);
      FParent := AParent;
      AParent.AddChild(Self);
    end;

    mClearParent:
    begin
      FParent.DeleteChild(Self);
      FParent := nil;
    end;
  end;
end;


procedure TsgeGUIElement.SetEnable(AEnabled: Boolean);
begin
  if FEnable = AEnabled then
    Exit;
  FEnable := AEnabled;

  //Если элемент неактивен, то убрать монопольный захват мыши
  if not FEnable then
    sgeCorePointer_GetSGE.ExtGUI.ReleaseMouse(Self);

  Repaint;
end;


procedure TsgeGUIElement.SetVisible(AVisible: Boolean);
begin
  if FVisible = AVisible then
    Exit;

  FVisible := AVisible;

  //Выполнить обработчик
  if FVisible then
    Handler_Show
  else
    Handler_Hide;

  Notify([esRepaintParent]);
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


procedure TsgeGUIElement.SetAutoSize(AAutoSize: Boolean);
begin
  if FAutoSize = AAutoSize then
    Exit;

  FAutoSize := AAutoSize;

  Notify([esCorrectSize]);
end;


procedure TsgeGUIElement.SetStyle(AStyle: ShortString);
begin
  FStyle := AStyle;
end;


function TsgeGUIElement.GetScaleWidth: Integer;
begin
  Result := Round(FWidth * GetScale);
end;


function TsgeGUIElement.GetScaleHeight: Integer;
begin
  Result := Round(FHeight * GetScale);
end;


procedure TsgeGUIElement.SetScale(AScale: Single);
begin
  FScale := AScale;
end;


function TsgeGUIElement.GetScale: Single;
begin
  //Масштаб по умолчанию
  Result := FScale;

  //Масштаб можно менять только у формы
  if FParent <> nil then
    Result := FParent.GetScale;
end;


function TsgeGUIElement.GetConstrains: TsgeGUIPropertyConstrains;
begin
  Result := FConstrains;
end;


procedure TsgeGUIElement.AddChild(Element: TsgeGUIElement);
begin
  FChildList.Add(Element);
  Repaint;
end;


procedure TsgeGUIElement.DeleteChild(Element: TsgeGUIElement);
begin
  FChildList.Delete(Element);
  Repaint;
end;


procedure TsgeGUIElement.DestroyChild;
begin
  if FChildList.Count = 0 then
    Exit;

  //Удалить объекты
  while FChildList.Count > 0 do
    FChildList.Item[0].Free;

  //Перерисовать
  Repaint;
end;


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


procedure TsgeGUIElement.GetPrefferedSize(var NewWidth, NewHeight: Integer);
begin
  //Проверить авторазмер
  if FAutoSize then
    CalculateAutosize(NewWidth, NewHeight);

  //Проверить ограничение размера
  FConstrains.Check(NewWidth, NewHeight);

  //Проверку на наименьший размер
  CheckMinimalSize(NewWidth, NewHeight);
end;


procedure TsgeGUIElement.CalculateAutosize(var NewWidth, NewHeight: Integer);
begin
  //Заглушка. Тут код определения авторазмеров элемента
  //для компонентов, использующих шрифт
end;


procedure TsgeGUIElement.CheckMinimalSize(var NewWidth, NewHeight: Integer);
begin
  //Заглушка. Тут код проверки на наименьший возможный размер
end;


procedure TsgeGUIElement.Notify(Action: TsgeGUIElementState);
begin
  //Если запрет обновления, то выход
  if esLockUpdate in FState then
    Exit;

  //Добавить новое состояние
  FState := FState + Action;

  //Поправить размеры
  if esCorrectSize in FState then
  begin
    Exclude(FState, esCorrectSize);                                 //Удалить флаг изменения размеров
    GetPrefferedSize(FWidth, FHeight);                              //Взять предпочтительный размер

    //Изменить размеры канваса если они отличаются
    if (FWidth <> FCanvas.Width) or (FHeight <> FCanvas.Height) then
      FCanvas.SetSize(FWidth, FHeight);

    Include(FState, esRepaint);                                     //Дбавить флаг перерисовки
  end;

  //Проверить перерисовку
  if esRepaint in FState then
  begin
    Exclude(FState, esRepaint);                                     //Удалить флаг собственной отрисовки
    Draw;                                                           //Перерисовать себя и элементы
    Include(FState, esRepaintParent);                               //Добавить флаг для родителя о перерисовке
  end;

  //Проверить перерисовку родителя
  if esRepaintParent in FState then
  begin
    Exclude(FState, esRepaintParent);                               //Удалить флаг перерисовки родителя
    if FParent <> nil then                                          //Перерисовать родителя
      FParent.Repaint;
  end;
end;


procedure TsgeGUIElement.DrawChild;
var
  i: Integer;
  El: TsgeGUIElement;
begin
  for i := 0 to FChildList.Count - 1 do
  begin
    //Указатель на элемент
    El := FChildList.Item[i];

    //Пропуск невидимых
    if not El.Visible then
      Continue;

    //Вывод спрайтов детей
    with sgeCorePointer_GetSGE.ExtGraphic.Graphic do
    begin
      ResetDrawOptions;
      DrawSprite(El.Left, El.Top, El.Width, El.Height, El.Canvas);
    end;
  end;
end;


class function TsgeGUIElement.GetParameterSectionName: String;
begin
  Result := '';
end;


procedure TsgeGUIElement.LoadData(Data: TsgeSimpleParameters);
var
  ParamName, s: String;
begin
  //ClickButton
  ParamName := 'ClickButton';
  if Data.Exist[ParamName] then
  begin
    s := LowerCase(Data.GetValue(ParamName, ''));
    case s of
      'left':
        FClickButton := mbLeft;

      'middle':
        FClickButton := mbMiddle;

      'right':
        FClickButton := mbRight;

      'extra1':
        FClickButton := mbExtra1;

      'extra2':
        FClickButton := mbExtra2;
    end;
  end;

  //Constrains
  FConstrains.LoadParameters(Data, 'Constrains.');

  //Left
  sgeGUISetValue(Data, 'Left', FLeft);

  //Top
  sgeGUISetValue(Data, 'Top', FTop);

  //Width
  sgeGUISetValue(Data, 'Width', FWidth, 100);

  //Height
  sgeGUISetValue(Data, 'Height', FHeight, 100);

  //AutoSize
  sgeGUISetValue(Data, 'AutoSize', FAutoSize, False);

  //Enable
  sgeGUISetValue(Data, 'Enable', FEnable, True);

  //Visible
  ParamName := 'Visible';
  if Data.Exist[ParamName] then
    SetVisible(Data.GetValue(ParamName, True));
end;


procedure TsgeGUIElement.LockUpdate;
begin
  Include(FState, esLockUpdate);
end;


procedure TsgeGUIElement.UnLockUpdate;
begin
  Exclude(FState, esLockUpdate);
  Notify([esCorrectSize]);
end;


procedure TsgeGUIElement.GraphicPrepare;
begin
  with sgeCorePointer_GetSGE.ExtGraphic.Graphic do
  begin
    PushAttrib;
    Reset;
    ResetDrawOptions;
    RenderSprite := FCanvas;
    RenderPlace := grpSprite;
    ColorBlend := True;

    //Стереть фон холста
    BGColor := cTransparentWhite;
    EraseBG;
  end;
end;


procedure TsgeGUIElement.GraphicRestore;
begin
  with sgeCorePointer_GetSGE.ExtGraphic.Graphic do
  begin
    RenderSprite := nil;
    RenderPlace := grpScreen;
    PopAttrib;
    Finish;
  end;
end;


procedure TsgeGUIElement.DrawBefore;
begin
  //Отрисовка перед выводом детей
end;


procedure TsgeGUIElement.DrawAfter;
begin
  //Отрисовка после вывода детей
end;


procedure TsgeGUIElement.SetLeft(ALeft: Integer);
begin
  if FLeft = ALeft then
    Exit;

  FLeft := ALeft;

  Notify([esRepaintParent]);
end;


procedure TsgeGUIElement.SetTop(ATop: Integer);
begin
  if FTop = ATop then
    Exit;

  FTop := ATop;

  Notify([esRepaintParent]);
end;


procedure TsgeGUIElement.SetWidth(AWidth: Integer);
begin
  if AWidth <= 0 then
    AWidth := 0;
  if FWidth = AWidth then
    Exit;

  FWidth := AWidth;

  Notify([esCorrectSize]);
end;


procedure TsgeGUIElement.SetHeight(AHeight: Integer);
begin
  if AHeight <= 0 then
    AHeight := 0;
  if FHeight = AHeight then
    Exit;

  FHeight := AHeight;

  Notify([esCorrectSize]);
end;


procedure TsgeGUIElement.SetDrawAfter(ADrawAfter: TsgeGUIProcEvent);
begin
  if FOnDrawBefore = ADrawAfter then
    Exit;

  FOnDrawAfter := ADrawAfter;
  Repaint;
end;


procedure TsgeGUIElement.SetDrawBefore(ADrawBefore: TsgeGUIProcEvent);
begin
  if FOnDrawBefore = ADrawBefore then
    Exit;

  FOnDrawBefore := ADrawBefore;
  Repaint;
end;


procedure TsgeGUIElement.SetBounds(ABounds: TsgeFloatRect);
var
  W, H: Single;
begin
  //Определить размеры
  W := ABounds.X2 - ABounds.X1;
  H := ABounds.Y2 - ABounds.Y1;

  //Поправить размеры
  if W <= 0 then
    W := 0;
  if H <= 0 then
    H := 0;

  //Запомнить положение и размеры
  FLeft := Round(ABounds.X1);
  FTop := Round(ABounds.Y1);
  FWidth := Round(W);
  FHeight := Round(H);

  //Внести изменения
  Notify([esCorrectSize]);
end;


function TsgeGUIElement.GetBounds: TsgeFloatRect;
begin
  with Result do
  begin
    X1 := FLeft;
    Y1 := FTop;
    X2 := FLeft + FWidth;
    Y2 := FTop + FHeight;
  end;
end;


constructor TsgeGUIElement.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  //Установить флаг создания
  FState := [esLockUpdate];

  //Установить параметры
  FName := Name;
  FVisible := True;
  FEnable := True;
  FFocused := False;
  FAutoSize := False;
  FScale := 1;
  FClickButton := mbLeft;
  FStyle := '';

  //Создать объекты
  FCanvas := TsgeGraphicSprite.Create(Width, Height, cTransparentWhite);
  FChildList := TsgeGUIElementList.Create(False);
  FConstrains := TsgeGUIPropertyConstrainsExt.Create(Self);

  //Изменить размеры
  SetBounds(sgeGetFloatRect(Left, Top, Left + Width, Top + Height));

  //Установить Родителя
  SetParent(Parent);

  //Убрать флаг создания
  Exclude(FState, esLockUpdate);
end;


destructor TsgeGUIElement.Destroy;
begin
  //Установить флаг уничтожения объекта
  Include(FState, esLockUpdate);

  //Отключить захват мыши
  sgeCorePointer_GetSGE.ExtGUI.ReleaseMouse(Self);

  //Убрать фокус с элемента
  sgeCorePointer_GetSGE.ExtGUI.LostFocus(Self);

  //Удалить детей
  DestroyChild;

  //Удалить объекты
  FConstrains.Free;
  FChildList.Free;
  FCanvas.Free;

  //Удалить себя у родителя
  if FParent <> nil then
    FParent.DeleteChild(Self);

  //Убрать флаг уничтожения объекта
  Exclude(FState, esLockUpdate);
end;


procedure TsgeGUIElement.MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse);
var
  LocalMouse: TsgeEventMouse;
  Pt: TsgeIntPoint;
  X, Y: Integer;
  Scale: Single;
begin
  //Если неактивен, то выход
  if not FVisible or not FEnable then
    Exit;

  //Реальное положение на экране
  Pt := GetGlobalPos;

  //Изменить координаты мыши под текущий элемент
  if FParent = nil then
  begin
    X := Mouse.X - Pt.X;
    Y := Mouse.Y - Pt.Y;
  end
  else
  begin
    Scale := GetScale;
    X := Round((Mouse.X - Pt.X) / Scale);
    Y := Round((Mouse.Y - Pt.Y) / Scale);
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
          Handler_MouseDoubleClick(LocalMouse);

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
  if not FVisible or not FEnable then
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


procedure TsgeGUIElement.Repaint;
begin
  Notify([esRepaint]);
end;


procedure TsgeGUIElement.Resize;
begin
  Notify([esCorrectSize]);
end;


procedure TsgeGUIElement.Draw;
begin
  //Проверка на запрет обновления
  if (esLockUpdate in FState) then
    Exit;

  //Подготовить графику
  GraphicPrepare;

  //Вывод перед отрисовкой детей
  if Assigned(FOnDrawBefore) then
    FOnDrawBefore(Self)
  else
    DrawBefore;

  //Отрисовка детей
  DrawChild;

  //Вывод после отрисовки детей
  if Assigned(FOnDrawAfter) then
    FOnDrawAfter(Self)
  else
    DrawAfter;

  //Вывод рамки элемента
  {$IfDef SGE_GUI_Bounds}
  with sgeCorePointer_GetSGE.ExtGraphic.Graphic do
  begin
    ColorBlend := False;
    PoligonMode := gpmLine;
    Color := cRed;
    DrawRect(1, 0, FWidth - 1, FHeight - 1);
  end;
  {$EndIf}

  //Восстановить графику
  GraphicRestore;
end;


procedure TsgeGUIElement.LoadParameters(Params: TsgeSimpleContainer);
const
  Separartor = ':';
var
  Data: TsgeSimpleParameters;
  SectionName: String;
begin
  Data := TsgeSimpleParameters.Create;
  try
    //Определить имя секции
    SectionName := GetParameterSectionName;
    if FStyle <> '' then
      SectionName := SectionName + Separartor + FStyle;

    //Разобрать секцию на параметры
    Data.FromString(Params.GetSection(SectionName));

    //Заблокировать отрисовку
    LockUpdate;

    //Загрузить параметры
    LoadData(Data);

    //Разблокировать отрисовку
    UnLockUpdate;
  finally
    Data.Free;
  end;
end;


function TsgeGUIElement.PointInElement(X, Y: Integer): Boolean;
var
  Pt: TsgeIntPoint;
  Scale: Single;
  W, H: Integer;
begin
  //Глобальные координаты c учётом масштаба
  Pt := GetGlobalPos;

  //Масштаб
  Scale := GetScale;

  //Размеры элементы
  W := Round(FWidth * Scale);
  H := Round(FHeight * Scale);

  //Проверить попадание
  Result := (X >= Pt.X) and (X <= Pt.X + W) and (Y >= Pt.Y) and (Y <= Pt.Y + H);
end;


function TsgeGUIElement.GetTopParent: TsgeGUIElement;
begin
  Result := Self;
  if FParent <> nil then
    Result := FParent.GetTopParent;
end;


function TsgeGUIElement.GetGlobalPos: TsgeIntPoint;
var
  Pt: TsgeIntPoint;
  Scale: Single;
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
    Scale := GetScale;

    //Результат
    Result.X := P.Left + Round(Pt.X * Scale);
    Result.Y := P.Top + Round(Pt.Y * Scale);
  end;
end;



end.


