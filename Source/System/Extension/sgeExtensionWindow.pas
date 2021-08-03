{
Пакет             Simple Game Engine 2
Файл              sgeExtensionWindow.pas
Версия            1.6
Создан            31.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Окно
}
{$Include Defines.inc}

unit sgeExtensionWindow;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeExtensionBase,
  sgeThread, sgeWindow,
  Windows;

const
  Extension_Window = 'Window';


type
  TsgeExtensionWindow = class(TsgeExtensionBase)
  private
    //Классы
    FWindow: TsgeWindow;                                                                //Класс окна
    FThread: TsgeThread;                                                                //Поток выборки собщений системы

    //Вспомогательные поля
    FMouseOut: Boolean;                                                                 //Мышь за границей окна

    //Методы потока
    procedure CreateWindow;                                                             //Создать окно
    procedure DestroyWindow;                                                            //Удалить окно
    procedure MessageProc;                                                              //Опрос событий

    //Вспомогательные функции
    procedure SetMouseTrackEvent;                                                       //Запустить слежение за нестандартными сообщениями мыши
    function  GetKeyboardButtons: TsgeKeyboardButtons;                                  //Определить функциональные клавиши
    function  GetMouseButtons(wParam: WPARAM): TsgeMouseButtons;                        //Узнать нажатые клавиши
    function  GetMouseScrollDelta(wParam: WPARAM): Integer;                             //Определить значение прокрутки

    function  WndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    property Window: TsgeWindow read FWindow;
  end;



implementation

uses
  sgeErrors, sgeEventWindow;


var
  _Self: TsgeExtensionWindow;


const
  _UNITNAME = 'ExtensionWindow';


//Оконная функция
function sgeWndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := _Self.WndProc(hWnd, Msg, wParam, lParam);
end;


procedure TsgeExtensionWindow.CreateWindow;
begin
  try
    //Создать окно
    FWindow := TsgeWindow.Create('SGEMainWindowClass', 'Simple Game Engine 2', 100, 100, 800, 600);

    //Изменить оконную функцию
    FWindow.SetWindowProc(@sgeWndProc);

    //Запустить механизм отлова уходы мыши с формы
    SetMouseTrackEvent;

  except
  end;
end;


procedure TsgeExtensionWindow.DestroyWindow;
begin
  FWindow.Hide;
  FWindow.Free;
end;


class function TsgeExtensionWindow.GetName: String;
begin
  Result := Extension_Window;
end;


procedure TsgeExtensionWindow.MessageProc;
var
  Message: TMSG;
  Pos: TPoint;
  wp: WPARAM;
begin
  if GetMessage(Message, 0, 0, 0) then
    begin
    //Отослать событие WM_Char
    TranslateMessage(Message);

    //Поправить сообщение
    case Message.message of
      WM_LBUTTONUP:
        begin
        Message.wParam := MK_LBUTTON;
        DispatchMessage(Message);
        end;

      WM_MBUTTONUP:
        begin
        Message.wParam := MK_MBUTTON;
        DispatchMessage(Message);
        end;

      WM_RBUTTONUP:
        begin
        Message.wParam := MK_RBUTTON;
        DispatchMessage(Message);
        end;

      WM_XBUTTONUP:
        begin
        if SmallInt(HIWORD(Message.wParam)) = 1 then Message.wParam := MK_XBUTTON1 else Message.wParam := MK_XBUTTON2;
        DispatchMessage(Message);
        end;

      WM_MOUSEWHEEL:
        begin
        //Положение курсора относительно экрана, а не окна
        Pos.X := GET_X_LPARAM(Message.lParam);
        Pos.Y := GET_Y_LPARAM(Message.lParam);
        Windows.ScreenToClient(Message.hwnd, Pos);
        Message.lParam := MAKELPARAM(Pos.X, Pos.Y);

        DispatchMessage(Message);
        end;

      WM_MOUSELEAVE:
        begin
        //Положение курсора относительно экрана, а не окна
        Windows.GetCursorPos(Pos);
        Windows.ScreenToClient(Message.hwnd, Pos);
        Message.lParam := MAKELPARAM(Pos.X, Pos.Y);

        //Кнопки мыши
        wp := 0;
        if (GetKeyState(VK_LBUTTON) and $8000) <> 0 then wp := wp or MK_LBUTTON;
        if (GetKeyState(VK_RBUTTON) and $8000) <> 0 then wp := wp or MK_RBUTTON;
        if (GetKeyState(VK_MBUTTON) and $8000) <> 0 then wp := wp or MK_MBUTTON;
        if (GetKeyState(VK_XBUTTON1) and $8000) <> 0 then wp := wp or MK_XBUTTON1;
        if (GetKeyState(VK_XBUTTON2) and $8000) <> 0 then wp := wp or MK_XBUTTON2;
        Message.wParam := wp;

        DispatchMessage(Message);
        end;

      else DispatchMessage(Message);
    end;
    end;
end;


procedure TsgeExtensionWindow.SetMouseTrackEvent;
var
  tme: TTrackMouseEvent;
begin
  tme.cbSize := SizeOf(TTrackMouseEvent); //Размер структуры
  tme.hwndTrack := FWindow.Handle;        //Хэндл окна
  tme.dwFlags := TME_LEAVE;               //Вызывать сообщение только ухода мыши
  tme.dwHoverTime := 0;                   //Таймаут нависания (в моём случае не используется)
  TrackMouseEvent(tme);                   //Запустить КОСТЫЛЬ!
end;


function TsgeExtensionWindow.GetKeyboardButtons: TsgeKeyboardButtons;
begin
  Result := [];
  if (GetKeyState(VK_LMENU) and $8000) <> 0 then Include(Result, kbLeftAlt);
  if (GetKeyState(VK_RMENU) and $8000) <> 0 then Include(Result, kbRightAlt);
  if (GetKeyState(VK_LCONTROL) and $8000) <> 0 then Include(Result, kbLeftCtrl);
  if (GetKeyState(VK_RCONTROL) and $8000) <> 0 then Include(Result, kbRightCtrl);
  if (GetKeyState(VK_LSHIFT) and $8000) <> 0 then Include(Result, kbLeftShift);
  if (GetKeyState(VK_RSHIFT) and $8000) <> 0 then Include(Result, kbRightShift);
  if (GetKeyState(VK_CAPITAL) and 1) = 1 then Include(Result, kbCapsLock);
  if (GetKeyState(VK_NUMLOCK) and 1) = 1 then Include(Result, kbNumLock);
  if (GetKeyState(VK_SCROLL) and 1) = 1 then Include(Result, kbScrollLock);
  if (GetKeyState(VK_INSERT) and 1) = 1 then Include(Result, kbInsert);
  if (kbLeftAlt in Result) or (kbRightAlt in Result) then Include(Result, kbAlt);
  if (kbLeftCtrl in Result) or (kbRightCtrl in Result) then Include(Result, kbCtrl);
  if (kbLeftShift in Result) or (kbRightShift in Result) then Include(Result, kbShift);
end;


function TsgeExtensionWindow.GetMouseButtons(wParam: WPARAM): TsgeMouseButtons;
begin
  Result := [];
  wParam := LOWORD(wParam);
  if (wParam and MK_LBUTTON) = MK_LBUTTON then Include(Result, mbLeft);
  if (wParam and MK_MBUTTON) = MK_MBUTTON then Include(Result, mbMiddle);
  if (wParam and MK_RBUTTON) = MK_RBUTTON then Include(Result, mbRight);
  if (wParam and MK_XBUTTON1) = MK_XBUTTON1 then Include(Result, mbExtra1);
  if (wParam and MK_XBUTTON2) = MK_XBUTTON2 then Include(Result, mbExtra2);
end;


function TsgeExtensionWindow.GetMouseScrollDelta(wParam: WPARAM): Integer;
begin
  Result := SmallInt(HIWORD(wParam)) div 120;
end;


function TsgeExtensionWindow.WndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  CursorPos: TsgeIntPoint;
begin
  Result := 0;

  case Msg of
    WM_CLOSE:
      EventManager.Publish(Event_WindowClose);


    WM_SETFOCUS:
      EventManager.Publish(Event_WindowSetFocus);


    WM_KILLFOCUS:
      EventManager.Publish(Event_WindowLostFocus);


    WM_SHOWWINDOW:
      if wParam = 1 then EventManager.Publish(Event_WindowShow) else EventManager.Publish(Event_WindowHide);


    WM_ACTIVATE:
      if wParam = WA_INACTIVE then EventManager.Publish(Event_WindowDeActivate) else EventManager.Publish(Event_WindowActivate);


    WM_CHAR:
      EventManager.Publish(Event_WindowChar, TsgeEventWindowChar.Create(chr(wParam), GetKeyboardButtons));


    WM_KEYDOWN, WM_SYSKEYDOWN:
      EventManager.Publish(Event_WindowKeyDown, TsgeEventWindowKeyboard.Create(wParam, GetKeyboardButtons, (lParam shr 30) = 0));


    WM_KEYUP, WM_SYSKEYUP:
      EventManager.Publish(Event_WindowKeyUp, TsgeEventWindowKeyboard.Create(wParam, GetKeyboardButtons, False));


    WM_LBUTTONDOWN, WM_MBUTTONDOWN, WM_RBUTTONDOWN, WM_XBUTTONDOWN:
      EventManager.Publish(Event_WindowMouseDown, TsgeEventWindowMouse.Create(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam), GetMouseButtons(wParam), GetKeyboardButtons));


    WM_LBUTTONUP, WM_MBUTTONUP, WM_RBUTTONUP, WM_XBUTTONUP:
      EventManager.Publish(Event_WindowMouseUp, TsgeEventWindowMouse.Create(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam), GetMouseButtons(wParam), GetKeyboardButtons));


    WM_LBUTTONDBLCLK, WM_MBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_XBUTTONDBLCLK:
      EventManager.Publish(Event_WindowMouseDoubleClick, TsgeEventWindowMouse.Create(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam), GetMouseButtons(wParam), GetKeyboardButtons));


    WM_MOUSEWHEEL:
      EventManager.Publish(Event_WindowMouseScroll, TsgeEventWindowMouse.Create(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam), GetMouseButtons(wParam), GetKeyboardButtons, GetMouseScrollDelta(wParam)));


    WM_MOUSEMOVE:
      begin
      //Координаты курсора
      CursorPos.X := GET_X_LPARAM(lParam);
      CursorPos.Y := GET_Y_LPARAM(lParam);

      //Обработать возврат мыши на форму
      if FMouseOut then
        begin
        FMouseOut := False;
        SetMouseTrackEvent;
        EventManager.Publish(Event_WindowMouseEnter, TsgeEventWindowMouse.Create(CursorPos.X, CursorPos.Y, GetMouseButtons(wParam), GetKeyboardButtons));
        end;

      EventManager.Publish(Event_WindowMouseMove, TsgeEventWindowMouse.Create(CursorPos.X, CursorPos.Y, GetMouseButtons(wParam), GetKeyboardButtons));
      end;


    WM_MOUSELEAVE:
      begin
      FMouseOut := True;
      EventManager.Publish(Event_WindowMouseLeave, TsgeEventWindowMouse.Create(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam), GetMouseButtons(wParam), GetKeyboardButtons));
      end;


    WM_SIZE:
      begin
      EventManager.Publish(Event_WindowSize, TsgeEventWindowSize.Create(LOWORD(lParam), HIWORD(lParam)));

      case wParam of
        SIZE_RESTORED : EventManager.Publish(Event_WindowRestore);
        SIZE_MINIMIZED: EventManager.Publish(Event_WindowMinimize);
        SIZE_MAXIMIZED: EventManager.Publish(Event_WindowMaximize);
      end;
      end;


    {WM_SIZING:
      begin
      //TODO : Неправильные размеры клиентской части
      Rct := Pointer(lParam);
      FEventManager.Publish(Event_WindowSize, TsgeEventWindowSize.Create(Rct^.Right - Rct^.Left, Rct^.Bottom - Rct^.Top));
      end;}


    WM_ERASEBKGND: ;


    WM_SYSCOMMAND:
      if wParam <> SC_KEYMENU then Result := DefWindowProc(hWnd, Msg, wParam, lParam);  //Убирает пиканье при нажатии Alt


    else Result := DefWindowProc(hWnd, Msg, wParam, lParam);
  end;
end;


constructor TsgeExtensionWindow.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

      //Задать начальные параметры
    _Self := Self;
    FMouseOut := False;

    //Создать поток
    FThread := TsgeThread.Create(nil, True, False);

    //Создать окно
    FThread.RunProcAndWait(@CreateWindow);

    //Проверить создание окна на ошибки
    if FThread.Exception <> nil then
      raise EsgeException.Create(FThread.Exception.Message);

    //Установить бесконечный метод выборки сообщений
    FThread.LoopProc := @MessageProc;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionWindow.Destroy;
begin
  //Удалить функцию выборки сообщений
  FThread.LoopProc := nil;

  if FWindow <> nil then
    begin
    //Послать окну сообщение что бы поток вышел из GetMessage
    PostMessage(FWindow.Handle, WM_NULL, 0, 0);

    //Уничтожить окно
    FThread.RunProcAndWait(@DestroyWindow);
    end;

  //Уничтожить поток
  FThread.Free;

  inherited Destroy;
end;




end.


