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
  sgeEventWindow;


type
  TsgeGUIElementList = class;
  TsgeGUIElement = class;

  //Обработчики
  TsgeGUIProcEvent = procedure(Obj: TsgeGUIElement) of Object;
  TsgeGUIProcMouseEvent = procedure(Obj: TsgeGUIElement; Mouse: TsgeEventWindowMouse) of object;
  TsgeGUIProcButtonEvent = procedure(Obj: TsgeGUIElement; Key: Byte; KeyboardButtons: TsgeKeyboardButtons) of object;
  TsgeGUIProcButtonCharEvent = procedure(Obj: TsgeGUIElement; Key: Char; KeyboardButtons: TsgeKeyboardButtons) of object;


  //Состояние элемента
  TsgeGUIElementState = set of (esCreating, esDestroing, esCorrectSize, esRepaint, esRepaintParent);


  //Тип обработчика мыши
  TsgeGUIElementMouseEventType = (emetDown, emetUp, emetMove, emetEnter, emetLeave, emetScroll);


  //Элемент
  TsgeGUIElement = class
  public
    FColor: TsgeColor;
    procedure SetColor(AColor: TsgeColor);
    property Color: TsgeColor read FColor write SetColor;
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
    FAlpha: Single;                                                 //Прозрачность элемента
    FLeft: Integer;                                                 //X относительно родителя
    FTop: Integer;                                                  //Y относительно родителя
    FWidth: Integer;                                                //Ширина
    FHeight: Integer;                                               //Высота

    //Обработчики событий
    FOnDrawBefore: TsgeGUIProcEvent;
    FOnDrawAfter: TsgeGUIProcEvent;
    FOnShow: TsgeGUIProcEvent;
    FOnHide: TsgeGUIProcEvent;

    FOnMouseClick   : TsgeGUIProcMouseEvent;
    FOnMouseDblClick: TsgeGUIProcMouseEvent;
    FOnMouseMove    : TsgeGUIProcMouseEvent;
    FOnMouseDown    : TsgeGUIProcMouseEvent;
    FOnMouseUp      : TsgeGUIProcMouseEvent;
    FOnMouseLeave   : TsgeGUIProcMouseEvent;
    FOnMouseEnter   : TsgeGUIProcMouseEvent;
    FOnMouseScroll  : TsgeGUIProcMouseEvent;

    FOnButtonDown   : TsgeGUIProcButtonEvent;
    FOnButtonUp     : TsgeGUIProcButtonEvent;
    FOnButtonChar   : TsgeGUIProcButtonCharEvent;

    procedure Handler_Show; virtual;
    procedure Handler_Hide; virtual;

    //Вспомогательные методы
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
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); virtual;
    destructor  Destroy; override;

    //Методы Parent
    procedure Repaint;
    procedure AddChild(Element: TsgeGUIElement);
    procedure DeleteChild(Element: TsgeGUIElement);
    procedure DeleteChild;

    //Обработчики событий
    function  MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventWindowMouse): Boolean;

    procedure Draw; virtual;
    function  CursorInElement(X, Y: Integer): Boolean;

    //Параметры
    property State: TsgeGUIElementState read FState write FState;
    property Canvas: TsgeGraphicSprite read FCanvas;
    property Name: ShortString read FName;
    property Enable: Boolean read FEnable write SetEnable;
    property Visible: Boolean read FVisible write SetVisible;
    property Alpha: Single read FAlpha write SetAlpha;
    property Parent: TsgeGUIElement read FParent write SetParent;
    property Bounds: TsgeFloatRect read GetBounds write SetBounds;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property ChildList: TsgeGUIElementList read FChildList;

    //Обработчики
    property OnDrawBefore: TsgeGUIProcEvent read FOnDrawBefore write SetDrawBefore;
    property OnDrawAfter: TsgeGUIProcEvent read FOnDrawAfter write SetDrawAfter;
    property OnShow: TsgeGUIProcEvent read FOnShow write FOnShow;
    property OnHide: TsgeGUIProcEvent read FOnHide write FOnHide;

    property OnMouseClick: TsgeGUIProcMouseEvent read FOnMouseClick write FOnMouseClick;
    property OnMouseDblClick: TsgeGUIProcMouseEvent read FOnMouseDblClick write FOnMouseDblClick;
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
  private
  public
    function  IndexOf(Element: TsgeGUIElement): Integer;
    procedure Delete(Index: Integer);
    procedure Delete(Element: TsgeGUIElement);
  end;



implementation

uses
  sgeVars, sgeGraphic;


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


procedure TsgeGUIElement.SetColor(AColor: TsgeColor);
begin
  FColor := AColor;
  Repaint;
end;

procedure TsgeGUIElement.Handler_Show;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;


procedure TsgeGUIElement.Handler_Hide;
begin
  if Assigned(FOnHide) then FOnHide(Self);
end;


procedure TsgeGUIElement.Notify(Action: TsgeGUIElementState);
begin
  FState := FState + Action;

  //Поправить размеры
  if esCorrectSize in FState then
    begin
    Exclude(FState, esCorrectSize);                                 //Удалить флаг изменения размеров
    //GetPrefferedSize(FWidth, FHeight);                            //Взять предпочтительный размер
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
    PopAttrib;
    Finish;
    end;
end;


procedure TsgeGUIElement.DrawBefore;
begin
  with SGE.ExtGraphic.Graphic do
    begin
    BGColor := FColor; //sgeGetColor(0, 0, 0, 0.5);
    EraseBG;
    //Color := cBlack;
    //DrawRect(0, 0, FWidth, FHeight);
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
  FColor := cLime;

  //Установить параметры
  FName := Name;
  FVisible := True;
  FEnable := True;
  FAlpha := 1;

  //Создать холст
  FCanvas := TsgeGraphicSprite.Create(Width, Height, cTransparent);
  FChildList := TsgeGUIElementList.Create(False);

  //Изменить размеры
  SetBounds(sgeGetFloatRect(Left, Top, Left + Width, Top + Height));

  //Установить Родителя
  SetParent(Parent);
end;


destructor TsgeGUIElement.Destroy;
begin
  //Удалить детей
  DeleteChild;

  //Удалить объекты
  FChildList.Free;
  FCanvas.Free;

  //Удалить себя у родителя
  if FParent <> nil then FParent.DeleteChild(Self);
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
  //Определить флаг перерисовки
  if FChildList.Count = 0 then Exit;

  //Удалить объекты
  while FChildList.Count > 0 do
    FChildList.Item[0].Free;

  //Перерисовать
  Repaint;
end;


function TsgeGUIElement.MouseHandler(EventType: TsgeGUIElementMouseEventType; Mouse: TsgeEventWindowMouse): Boolean;
var
  i: Integer;
  El: TsgeGUIElement;
  ChildHandled: Boolean;
begin
  Result := True;

  //Обработать тип события
  case EventType of
    emetDown, emetUp, emetScroll:
      begin
      ChildHandled := False;

      //Проверить детей
      for i := 0 to FChildList.Count - 1 do
        begin
        El := FChildList.Item[i];

        if El.CursorInElement(Mouse.X, Mouse.Y) then
          begin
          //Координаты относительно элемента
          Mouse.ChangeXY(Mouse.X - El.Left, Mouse.Y - El.Top);

          //Вызвать обработчик
          El.MouseHandler(EventType, Mouse);

          ChildHandled := True;
          Break;
          end;
        end;


      //Собственный обработчик если не выполнился у дитя
      if not ChildHandled then
        case EventType of
          emetDown  : if Assigned(FOnMouseDown) then FOnMouseDown(Self, Mouse);
          emetUp    : if Assigned(FOnMouseUp) then FOnMouseUp(Self, Mouse);
          emetScroll: if Assigned(FOnMouseScroll) then FOnMouseScroll(Self, Mouse);
        end;
      end;


    emetMove:
      begin
      end;

  end;  //case



end;


procedure TsgeGUIElement.Draw;
begin
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
  Result := (X >= FLeft) and (X <= FLeft + FWidth) and (Y >= FTop) and (Y <= FTop + FHeight);
end;




end.

