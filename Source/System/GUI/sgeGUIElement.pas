{
Пакет             Simple Game Engine 2
Файл              sgeGUIElement.pas
Версия            1.0
Создан            04.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Базовый элемент
}
{$Include Defines.inc}

unit sgeGUIElement;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeTemplateObjectCollection,
  sgeGraphicSprite, sgeGraphicColor,
  sgeEventBase, sgeEventKeyboard, sgeEventMouse,
  sgeGUIElementPropertyConstrains;


type
  TsgeGUIElementList = class;
  TsgeGUIElement = class;

  //Обработчики
  TsgeGUIProcEvent = procedure(Obj: TsgeGUIElement) of Object;
  TsgeGUIProcMouseEvent = procedure(Obj: TsgeGUIElement; Mouse: TsgeEventMouse) of object;
  TsgeGUIProcButtonEvent = procedure(Obj: TsgeGUIElement; Keyboard: TsgeEventKeyboard) of object;
  TsgeGUIProcButtonCharEvent = procedure(Obj: TsgeGUIElement; Keyboard: TsgeEventKeyboardChar) of object;


  //Состояние элемента
  TsgeGUIElementState = set of (esCreating, esDestroing, esCorrectSize, esRepaint, esRepaintParent);


  //Тип обработчика мыши
  TsgeGUIElementMouseEventType = (emetDown, emetUp, emetMove, emetEnter, emetLeave, emetScroll, emetDblClick);


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
    FAlpha: Single;                                                 //Прозрачность элемента
    FLeft: Integer;                                                 //X относительно родителя
    FTop: Integer;                                                  //Y относительно родителя
    FWidth: Integer;                                                //Ширина
    FHeight: Integer;                                               //Высота
    FAutoSize: Boolean;                                             //Авторазмер
    FClickButton: TsgeMouseButton;                                  //Кнопка мыши для Click
    FConstrains: TsgeGUIElementPropertyConstrains;                  //Ограничение размеров

    //Вспомогательные параметры
    FEventMouseEntered: Boolean;                                    //Флаг захода мышки на элемент
    FPressed: Boolean;                                              //Флаг нажатия для Click

    //Обработчики событий
    FOnDrawBefore: TsgeGUIProcEvent;
    FOnDrawAfter: TsgeGUIProcEvent;

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
    procedure Notify(Action: TsgeGUIElementState); virtual;         //Вызвать уведомление
    procedure DrawChild;                                            //Нарисовать детей

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
    procedure SetAlpha(AAlpha: Single); virtual;
    procedure SetFocused(AFocused: Boolean); virtual;
    procedure SetAutoSize(AAutoSize: Boolean); virtual;

    //Методы Parent
    procedure Repaint;
    procedure AddChild(Element: TsgeGUIElement);
    procedure DeleteChild(Element: TsgeGUIElement);
    procedure DeleteChild;
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); virtual;
    destructor  Destroy; override;

    //Обработчики событий
    function  MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse): Boolean; virtual;
    function  ButtonHandler(EventType: TsgeGUIElementButtonEventType; Keyboard: TsgeEventBase): Boolean; virtual;

    procedure Draw; virtual;

    //Вспомогательные методы
    function  CursorInElement(X, Y: Integer): Boolean;              //Проверить нахождение курсора в элементе
    function  GetGlobalPos: TsgeIntPoint;                           //Узнать координаты элемента относительно формы

    //Параметры
    property Canvas: TsgeGraphicSprite read FCanvas;
    property Name: ShortString read FName;
    property Enable: Boolean read FEnable write SetEnable;
    property Visible: Boolean read FVisible write SetVisible;
    property Focused: Boolean read FFocused write SetFocused;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Alpha: Single read FAlpha write SetAlpha;
    property Parent: TsgeGUIElement read FParent write SetParent;
    property Bounds: TsgeFloatRect read GetBounds write SetBounds;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Constrains: TsgeGUIElementPropertyConstrains read FConstrains;
    property ClickButton: TsgeMouseButton read FClickButton write FClickButton;
    property ChildList: TsgeGUIElementList read FChildList;

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

    property OnButtonDown   : TsgeGUIProcButtonEvent read FOnButtonDown write FOnButtonDown;
    property OnButtonUp     : TsgeGUIProcButtonEvent read FOnButtonUp write FOnButtonUp;
    property OnButtonChar   : TsgeGUIProcButtonCharEvent read FOnButtonChar write FOnButtonChar;
  end;



  //Список элементов
  TsgeGUIElementListTemplate = specialize TsgeTemplateObjectCollection<TsgeGUIElement>;
  TsgeGUIElementList = class(TsgeGUIElementListTemplate)
  public
    function  IndexOf(Element: TsgeGUIElement): Integer;
    procedure Delete(Index: Integer);
    procedure Delete(Element: TsgeGUIElement);
  end;



implementation

uses
  sgeVars, sgeGraphic, sgeMathUtils;

type
  TsgeGUIElementHack = class(TsgeGUIElement);


function TsgeGUIElementList.IndexOf(Element: TsgeGUIElement): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FCount - 1 do
    if FList[i] = Element then Exit(i);
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
  if Idx <> -1 then inherited Delete(Idx);
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
  if FParent = AParent then Exit;

  Mode := 0;
  if (FParent = nil) and (AParent <> nil) then Mode := mNewParent;
  if (FParent <> nil) and (AParent <> nil) then Mode := mChangeParent;
  if (FParent <> nil) and (AParent = nil) then Mode := mClearParent;

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
  if FEnable = AEnabled then Exit;

  FEnable := AEnabled;
  Repaint;
end;


procedure TsgeGUIElement.SetVisible(AVisible: Boolean);
begin
  if FVisible = AVisible then Exit;

  FVisible := AVisible;

  //Выполнить обработчик
  if FVisible then Handler_Show else Handler_Hide;

  Notify([esRepaintParent]);
end;


procedure TsgeGUIElement.SetAlpha(AAlpha: Single);
begin
  if AAlpha < 0 then AAlpha := 0;
  if AAlpha > 1 then AAlpha := 1;

  FAlpha := AAlpha;
  Repaint;
end;


procedure TsgeGUIElement.SetFocused(AFocused: Boolean);
begin
  if FFocused = AFocused then Exit;

  FFocused := AFocused;

  if FFocused then Handler_SetFocus else Handler_LostFocus;
end;


procedure TsgeGUIElement.SetAutoSize(AAutoSize: Boolean);
begin
  if FAutoSize = AAutoSize then Exit;

  FAutoSize := AAutoSize;

  Notify([esCorrectSize]);
end;


procedure TsgeGUIElement.Handler_Show;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;


procedure TsgeGUIElement.Handler_Hide;
begin
  if Assigned(FOnHide) then FOnHide(Self);
end;


procedure TsgeGUIElement.Handler_SetFocus;
begin
  if Assigned(FOnSetFocus) then FOnSetFocus(Self);
end;


procedure TsgeGUIElement.Handler_LostFocus;
begin
  if Assigned(FOnLostFocus) then FOnLostFocus(Self);
end;


procedure TsgeGUIElement.Handler_MouseClick(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseClick) then FOnMouseClick(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseDoubleClick(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseDoubleClick) then FOnMouseDoubleClick(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseMove(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseDown(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseUp(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseLeave(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseEnter(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_MouseScroll(Mouse: TsgeEventMouse);
begin
  if Assigned(FOnMouseScroll) then FOnMouseScroll(Self, Mouse);
end;


procedure TsgeGUIElement.Handler_ButtonDown(Keyboard: TsgeEventKeyboard);
begin
  if Assigned(FOnButtonDown) then FOnButtonDown(Self, Keyboard);
end;


procedure TsgeGUIElement.Handler_ButtonUp(Keyboard: TsgeEventKeyboard);
begin
  if Assigned(FOnButtonUp) then FOnButtonUp(Self, Keyboard);
end;


procedure TsgeGUIElement.Handler_ButtonChar(Keyboard: TsgeEventKeyboardChar);
begin
  if Assigned(FOnButtonChar) then FOnButtonChar(Self, Keyboard);
end;


procedure TsgeGUIElement.GetPrefferedSize(var NewWidth, NewHeight: Integer);
begin
  //Проверить авторазмер
  if FAutoSize then CalculateAutosize(NewWidth, NewHeight);

  //Проверить ограничение размера
  FConstrains.Check(NewWidth, NewHeight);
end;


procedure TsgeGUIElement.CalculateAutosize(var NewWidth, NewHeight: Integer);
begin
  //Заглушка. Тут код определения авторазмеров элемента
  //для компонентов, использующих шрифт
end;


procedure TsgeGUIElement.Notify(Action: TsgeGUIElementState);
begin
  FState := FState + Action;

  //Поправить размеры
  if esCorrectSize in FState then
    begin
    Exclude(FState, esCorrectSize);                                 //Удалить флаг изменения размеров
    GetPrefferedSize(FWidth, FHeight);                              //Взять предпочтительный размер
    FCanvas.SetSize(FWidth, FHeight);                               //Изменить размеры канваса
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
    if FParent <> nil then FParent.Repaint;                         //Перерисовать родителя
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
    if not El.Visible then Continue;

    //Вывод спрайтов детей
    with SGE.ExtGraphic.Graphic do
      begin
      ResetDrawOptions;
      //BlendFunction := gbfAlphaCopy;
      doTransparentColor := sgeGetColor(1, 1, 1, El.Alpha);
      DrawSprite(El.Left, El.Top, El.Width, El.Height, El.Canvas);
      //BlendFunction := gbfTransparent;
      end;
    end;
end;


procedure TsgeGUIElement.GraphicPrepare;
begin
  with SGE.ExtGraphic.Graphic do
    begin
    PushAttrib;
    Reset;
    ResetDrawOptions;
    RenderSprite := FCanvas;
    RenderPlace := grpSprite;
    PoligonMode := gpmFill;

    //Стереть фон холста
    ColorBlend := False;
    Color := cTransparent;
    DrawRect(0, 0, FWidth, FHeight);

    ColorBlend := True;
    end;
end;


procedure TsgeGUIElement.GraphicRestore;
begin
  with SGE.ExtGraphic.Graphic do
    begin
    RenderSprite := nil;
    RenderPlace := grpScreen;
    ResetDrawOptions;
    PopAttrib;
    Finish;
    end;
end;


procedure TsgeGUIElement.DrawBefore;
begin
  with SGE.ExtGraphic.Graphic do
    begin
    BGColor := cRed;
    EraseBG;
    end;
end;


procedure TsgeGUIElement.DrawAfter;
begin

end;


procedure TsgeGUIElement.SetLeft(ALeft: Integer);
begin
  if FLeft = ALeft then Exit;

  FLeft := ALeft;

  Notify([esRepaintParent]);
end;


procedure TsgeGUIElement.SetTop(ATop: Integer);
begin
  if FTop = ATop then Exit;

  FTop := ATop;

  Notify([esRepaintParent]);
end;


procedure TsgeGUIElement.SetWidth(AWidth: Integer);
begin
  if AWidth <= 0 then AWidth := 0;
  if FWidth = AWidth then Exit;

  FWidth := AWidth;

  Notify([esCorrectSize]);
end;


procedure TsgeGUIElement.SetHeight(AHeight: Integer);
begin
  if AHeight <= 0 then AHeight := 0;
  if FHeight = AHeight then Exit;

  FHeight := AHeight;

  Notify([esCorrectSize]);
end;


procedure TsgeGUIElement.SetDrawAfter(ADrawAfter: TsgeGUIProcEvent);
begin
  if FOnDrawBefore = ADrawAfter then Exit;

  FOnDrawAfter := ADrawAfter;
  Repaint;
end;


procedure TsgeGUIElement.SetDrawBefore(ADrawBefore: TsgeGUIProcEvent);
begin
  if FOnDrawBefore = ADrawBefore then Exit;

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
  if W <= 0 then W := 0;
  if H <= 0 then H := 0;

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
  FState := [esCreating];

  //Установить параметры
  FName := Name;
  FVisible := True;
  FEnable := True;
  FAlpha := 1;
  FFocused := False;
  FAutoSize := False;
  FClickButton := mbLeft;

  //Создать объекты
  FCanvas := TsgeGraphicSprite.Create(Width, Height, cTransparent);
  FChildList := TsgeGUIElementList.Create(False);
  FConstrains := TsgeGUIElementPropertyConstrains.Create(Self);

  //Изменить размеры
  SetBounds(sgeGetFloatRect(Left, Top, Left + Width, Top + Height));

  //Установить Родителя
  SetParent(Parent);

  //Убрать флаг создания
  Exclude(FState, esCreating);
end;


destructor TsgeGUIElement.Destroy;
begin
  //Установить флаг уничтожения объекта
  Include(FState, esDestroing);

  //Отключить захват мыши
  SGE.ExtGUI.ReleaseMouse(Self);

  //Убрать фокус с элемента
  SGE.ExtGUI.LostFocus(Self);

  //Удалить детей
  DeleteChild;

  //Удалить объекты
  FConstrains.Free;
  FChildList.Free;
  FCanvas.Free;

  //Удалить себя у родителя
  if FParent <> nil then FParent.DeleteChild(Self);

  //Убрать флаг уничтожения объекта
  Exclude(FState, esDestroing);
end;


procedure TsgeGUIElement.Repaint;
begin
  Notify([esRepaint]);
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


procedure TsgeGUIElement.DeleteChild;
begin
  if FChildList.Count = 0 then Exit;

  //Удалить объекты
  while FChildList.Count > 0 do
    FChildList.Item[0].Free;

  //Перерисовать
  Repaint;
end;


function TsgeGUIElement.MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventMouse): Boolean;

  function IsMouseInElement: Boolean;
  begin
    Result := (Mouse.X >= 0) and (Mouse.X <= FWidth) and (Mouse.Y >= 0) and (Mouse.Y <= FHeight);
  end;

var
  i: Integer;
  El: TsgeGUIElement;
  ChildHandled: Boolean;
  MouseChild: TsgeEventMouse;
begin
  Result := False;

  if not FVisible then Exit;
  if not FEnable then Exit;

  Result := True;

  //Обработать тип события
  case EventType of
    emetDown, emetUp, emetScroll, emetDblClick:
      begin
      ChildHandled := False;

      MouseChild := TsgeEventMouse.Create(Mouse.X, Mouse.Y, Mouse.MouseButtons, Mouse.KeyboardButtons, Mouse.Delta);

      //Проверить детей
      for i := 0 to FChildList.Count - 1 do
        begin
        El := FChildList.Item[i];

        MouseChild.ChangeXY(Mouse.X - El.Left, Mouse.Y - El.Top);

        if El.CursorInElement(Mouse.X, Mouse.Y) then
          begin
          El.MouseHandler(EventType, Mouse);
          ChildHandled := True;
          Break;
          end;
        end;

      MouseChild.Free;


      //Собственный обработчик если не выполнился у дочернего элемента
      if not ChildHandled then
        case EventType of
          emetDown:
            begin
            //Установить фокус ввода
            SGE.ExtGUI.SetFocus(Self);

            if FClickButton in Mouse.MouseButtons then
              begin
              FPressed := True;
              SGE.ExtGUI.MouseCapture(Self);
              end;

            Handler_MouseDown(Mouse);
            end;

          emetUp:
            begin
            //Проверить событие MouseClick
            if FPressed and IsMouseInElement then
              begin
              FPressed := False;
              Handler_MouseClick(Mouse);
              end;

            SGE.ExtGUI.ReleaseMouse;

            Handler_MouseUp(Mouse);
            end;

          emetScroll:
            Handler_MouseScroll(Mouse);

          emetDblClick:
            begin
            SGE.ExtGUI.SetFocus(Self);                              //Установить фокус ввода
            Handler_MouseDoubleClick(Mouse);
            end;
        end;
      end;


    emetMove:
      begin
      MouseChild := TsgeEventMouse.Create(Mouse.X, Mouse.Y, Mouse.MouseButtons, Mouse.KeyboardButtons, Mouse.Delta);

      //Проверить заход и уход мыши с элементов
      for i := 0 to FChildList.Count - 1 do
        begin
        El := FChildList.Item[i];

        MouseChild.ChangeXY(Mouse.X - El.Left, Mouse.Y - El.Top);

        if El.CursorInElement(Mouse.X, Mouse.Y) then
          begin
            //Заход мыши
            if not TsgeGUIElement(El).FEventMouseEntered then
              begin
              TsgeGUIElement(El).FEventMouseEntered := True;
              El.MouseHandler(emetEnter, MouseChild);
              end;
          end
          else begin
            //Выход мыши
            if TsgeGUIElement(El).FEventMouseEntered then
              begin
              TsgeGUIElement(El).FEventMouseEntered := False;
              El.MouseHandler(emetLeave, MouseChild);
              end;
            end;
          end;  //For

      MouseChild.Free;


      //Проверить дочерние элементы
      ChildHandled := False;
      for i := 0 to FChildList.Count - 1 do
        begin
        El := FChildList.Item[i];

        if El.CursorInElement(Mouse.X, Mouse.Y) then
          begin
          Mouse.ChangeXY(Mouse.X - El.Left, Mouse.Y - El.Top);
          El.MouseHandler(emetMove, Mouse);
          ChildHandled := True;
          //Break;
          end;
        end;

      //Собственный обработчик если не выполнился у дочернего элемента
      if not ChildHandled then Handler_MouseMove(Mouse);
      end;


    emetEnter:
      begin
      Handler_MouseEnter(Mouse);
      MouseHandler(emetMove, Mouse);
      end;


    emetLeave:
      begin
      Handler_MouseLeave(Mouse);
      MouseHandler(emetMove, Mouse);
      end;

  end;  //case
end;


function TsgeGUIElement.ButtonHandler(EventType: TsgeGUIElementButtonEventType; Keyboard: TsgeEventBase): Boolean;
begin
  Result := False;

  if not FVisible then Exit;
  if not FEnable then Exit;

  Result := True;

  case EventType of
    ebetDown: Handler_ButtonDown(TsgeEventKeyboard(Keyboard));
    ebetUp  : Handler_ButtonUp(TsgeEventKeyboard(Keyboard));
    ebetChar: Handler_ButtonChar(TsgeEventKeyboardChar(Keyboard));
  end;
end;


procedure TsgeGUIElement.Draw;
begin
  //Проверка на создание
  if (esCreating in FState) or (esDestroing in FState) then Exit;

  //Подготовить графику
  GraphicPrepare;

  //Вывод перед отрисовкой детей
  if Assigned(FOnDrawBefore) then FOnDrawBefore(Self) else DrawBefore;

  //Отрисовка детей
  DrawChild;

  //Вывод после отрисовки детей
  if Assigned(FOnDrawAfter) then FOnDrawAfter(Self) else DrawAfter;

  //Восстановить графику
  GraphicRestore;
end;


function TsgeGUIElement.CursorInElement(X, Y: Integer): Boolean;
begin
  if FParent = nil then
    Result := (X >= FLeft) and (X <= FLeft + FWidth) and (Y >= FTop) and (Y <= FTop + FHeight)
      else Result := (X >= sgeMax(0, FLeft)) and (X <= sgeMin(FParent.Width,  FLeft + FWidth)) and
                     (Y >= sgeMax(0, FTop)) and (Y <= sgeMin(FParent.Height, FTop + FHeight));
end;


function TsgeGUIElement.GetGlobalPos: TsgeIntPoint;
var
  Pt: TsgeIntPoint;
begin
  Result := sgeGetIntPoint(FLeft, FTop);

  if FParent <> nil then
    begin
    Pt := FParent.GetGlobalPos;
    Result.X := Result.X + Pt.X;
    Result.Y := Result.Y + Pt.Y;
    end;
end;




end.

