{
Пакет             Simple Game Engine 2
Файл              sgeWindow.pas
Версия            1.2
Создан            22.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание					Окно на WinAPI
}
{$Include Defines.inc}

unit sgeWindow;

{$mode objfpc}{$H+}

interface

uses
  Windows;


type
  //Стили
  TsgeWindowStyle = set of (wsCaption, wsSizeable, wsSystemMenu, wsTopMost, wsDoubleClick);

  //Кнопки
  TsgeWindowButtons = set of (wbClose, wbMinimize, wbMaximize);

  //Состояние
  TsgeWindowViewMode = (wvmNormal, wvmMinimize, wvmMaximize);

  //Вид выравнивание окна
  TsgeWindowCenterPos = (wcpScreen, wcpClientArea, wcpVirtualScreen);


  //Класс окна
  TsgeWindow = class
  private
    //Параметры
    FHandle: HWND;
    FWindowClass: TWNDClassEx;          //Класс окна
    FSystemMenu: HMENU;                 //Ссылка на главное меню
    FButtons: TsgeWindowButtons;        //Кнопки
    FClipCursor: Boolean;               //Держать курсор
    FStyle: TsgeWindowStyle;            //Стиль

    function  GetHWNDPos: HWND;

    function  GetDC: HDC;
    procedure SetCaption(ACaption: String);
    function  GetCaption: String;
    procedure SetStyle(AStyle: TsgeWindowStyle);
    procedure SetViewMode(AMode: TsgeWindowViewMode);
    function  GetViewMode: TsgeWindowViewMode;
    procedure SetButtons(AButtons: TsgeWindowButtons);
    procedure SetEnable(AEnable: Boolean);
    function  GetEnable: Boolean;
    procedure SetVisible(AVisible: Boolean);
    function  GetVisible: Boolean;
    procedure SetIcon(AIcon: HICON);
    function  GetIcon: HICON;
    procedure SetStatusBarVisible(AEnable: Boolean);
    function  GetStatusBarVisible: Boolean;
    procedure SetStatusBarIcon(AIcon: HICON);
    function  GetStatusBarIcon: HICON;
    procedure SetCursor(ACursor: HCURSOR);
    function  GetCursor: HCURSOR;
    procedure SetShowCursor(AShow: Boolean);
    function  GetShowCursor: Boolean;
    procedure SetClipCursor(AClip: Boolean);
    procedure SetLeft(ALeft: Integer);
    function  GetLeft: Integer;
    procedure SetTop(ATop: Integer);
    function  GetTop: Integer;
    procedure SetWindowWidth(AWidth: Integer);
    function  GetWindowWidth: Integer;
    procedure SetWindowHeight(AHeight: Integer);
    function  GetWindowHeight: Integer;
    procedure SetWidth(AWidth: Integer);
    function  GetWidth: Integer;
    procedure SetHeight(AHeight: Integer);
    function  GetHeight: Integer;

    procedure ChangeWindowStyle;
  public
    constructor Create(WndClassName, Caption: String; Left, Top: Integer; Width, Height: Integer);
    destructor  Destroy; override;

    function  IsFocused: Boolean;
    procedure Show;
    procedure Hide;
    procedure Activate;
    procedure Minimize;
    procedure Maximize;
    procedure Restore;
    procedure Update;
    procedure SetWindowProc(Proc: Pointer);
    procedure Center(APos: TsgeWindowCenterPos = wcpClientArea);

    property DC: HDC read GetDC;
    property Handle: HWND read FHandle;
    property Caption: String read GetCaption write SetCaption;
    property Style: TsgeWindowStyle read FStyle write SetStyle;
    property ViewMode: TsgeWindowViewMode read GetViewMode write SetViewMode;
    property Buttons: TsgeWindowButtons read FButtons write SetButtons;
    property Enable: Boolean read GetEnable write SetEnable;
    property Visible: Boolean read GetVisible write SetVisible;
    property Icon: HICON read GetIcon write SetIcon;
    property StatusBarIcon: HICON read GetStatusBarIcon write SetStatusBarIcon;
    property StatusBarVisible: Boolean read GetStatusBarVisible write SetStatusBarVisible;
    property Cursor: HCURSOR read GetCursor write SetCursor;
    property ShowCursor: Boolean read GetShowCursor write SetShowCursor;
    property ClipCursor: Boolean read FClipCursor write SetClipCursor;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property WindowWidth: Integer read GetWindowWidth write SetWindowWidth;
    property WindowHeight: Integer read GetWindowHeight write SetWindowHeight;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;



implementation

uses
  sgeErrors, sgeTypes;

const
  _UNITNAME = 'Window';

  Err_CantRegisterClass = 'CantRegisterClass';
  Err_CantCreateWindow  = 'CantCreateWindow';



function TsgeWindow.GetHWNDPos: HWND;
begin
  if wsTopMost in FStyle then Result := HWND_TOPMOST else Result := HWND_NOTOPMOST;
end;


function TsgeWindow.GetDC: HDC;
begin
  Result := Windows.GetDC(FHandle);
end;


procedure TsgeWindow.SetCaption(ACaption: String);
begin
  Windows.SetWindowText(FHandle, PChar(ACaption));
end;


function TsgeWindow.GetCaption: String;
var
  B: array of Char;
  Count: Integer;
begin
  Count := Windows.GetWindowTextLength(FHandle) + 1;
  SetLength(B, Count);
  GetWindowText(FHandle, @B[0], Count);
  SetString(Result, @B[0], Count);
  SetLength(B, 0);
end;


procedure TsgeWindow.SetStyle(AStyle: TsgeWindowStyle);
begin
  if FStyle = AStyle then Exit;
  FStyle := AStyle;
  ChangeWindowStyle;
end;


procedure TsgeWindow.SetViewMode(AMode: TsgeWindowViewMode);
begin
  case AMode of
    wvmNormal: Restore;
    wvmMinimize: Minimize;
    wvmMaximize: Maximize;
  end;
end;


function TsgeWindow.GetViewMode: TsgeWindowViewMode;
begin
  Result := wvmNormal;
  if IsIconic(FHandle) = LongBool(1) then Result := wvmMinimize;
  if IsZoomed(FHandle) = LongBool(1) then Result := wvmMaximize;
end;


procedure TsgeWindow.SetButtons(AButtons: TsgeWindowButtons);
begin
  if FButtons = AButtons then Exit;
  FButtons := AButtons;
  ChangeWindowStyle;
end;


procedure TsgeWindow.SetEnable(AEnable: Boolean);
begin
  Windows.EnableWindow(FHandle, AEnable);
end;


function TsgeWindow.GetEnable: Boolean;
begin
  Result := Windows.IsWindowEnabled(FHandle) <> LongBool(0);
end;


procedure TsgeWindow.SetVisible(AVisible: Boolean);
begin
  if AVisible then Show else Hide;
end;


function TsgeWindow.GetVisible: Boolean;
begin
  Result := (Windows.IsWindowVisible(FHandle) = LongBool(1));
end;


procedure TsgeWindow.SetIcon(AIcon: HICON);
begin
  Windows.SetClassLongPtr(FHandle, GCLP_HICONSM, AIcon);
end;


function TsgeWindow.GetIcon: HICON;
begin
  Result := Windows.GetClassLongPtr(FHandle, GCLP_HICONSM);
end;


procedure TsgeWindow.SetStatusBarVisible(AEnable: Boolean);
var
  i: HWND;
begin
  if AEnable then i := 0 else i := GetDesktopWindow;
  Windows.SetWindowLongPtr(FHandle, GWLP_HWNDPARENT, i);
end;


function TsgeWindow.GetStatusBarVisible: Boolean;
begin
  Result := Windows.GetWindowLongPtr(FHandle, GWL_HWNDPARENT) = 0;
end;


procedure TsgeWindow.SetStatusBarIcon(AIcon: HICON);
begin
  Windows.SetClassLongPtr(FHandle, GCLP_HICON, AIcon);
end;


function TsgeWindow.GetStatusBarIcon: HICON;
begin
  Result := Windows.GetClassLongPtr(FHandle, GCLP_HICON);
end;


procedure TsgeWindow.SetCursor(ACursor: HCURSOR);
begin
  Windows.SetClassLongPtr(FHandle, GCLP_HCURSOR, ACursor);
end;


function TsgeWindow.GetCursor: HCURSOR;
begin
  Result := Windows.GetClassLongPtr(FHandle, GCLP_HCURSOR);
end;


procedure TsgeWindow.SetShowCursor(AShow: Boolean);
var
  Cnt: Integer;
begin
  if AShow then
    begin
      repeat
      Cnt := Windows.ShowCursor(True);
      until Cnt >= 0;
    end
    else begin
      repeat
      Cnt := Windows.ShowCursor(False);
      until Cnt < 0;
    end;
end;


function TsgeWindow.GetShowCursor: Boolean;
var
  ci: TCURSORINFO;
begin
  ci.cbSize := SizeOf(TCURSORINFO);
  Windows.GetCursorInfo(ci);
  Result := (ci.flags = 1);
end;


procedure TsgeWindow.SetClipCursor(AClip: Boolean);
var
  Pt: TsgeIntPoint;
  Rct: TsgeIntRect;
begin
  if AClip = FClipCursor then Exit;
  FClipCursor := AClip;
  if FClipCursor then
    begin
    Pt.x := 0;
    Pt.y := 0;
    ClientToScreen(FHandle, @Pt);             //Преобразовать координаты нулевой точки окна в координаты на экране
    Windows.GetClientRect(FHandle, @Rct);     //Узнать размеры окна
    Rct.X1 := Pt.X;
    Rct.Y1 := Pt.Y;
    Inc(Rct.X2, Rct.X1);
    Inc(Rct.Y2, Rct.Y1);
    Windows.ClipCursor(@Rct);                 //Заблокировать курсор в прямоугольнике
    end else Windows.ClipCursor(nil);
end;


procedure TsgeWindow.SetLeft(ALeft: Integer);
var
  Rct: TsgeIntRect;
begin
  Windows.GetWindowRect(FHandle, @Rct);
  SetWindowPos(FHandle, GetHWNDPos, ALeft, Rct.Y1, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE);
end;


function TsgeWindow.GetLeft: Integer;
var
  Rct: TsgeIntRect;
begin
  Windows.GetWindowRect(FHandle, @Rct);
  Result := Rct.X1;
end;


procedure TsgeWindow.SetTop(ATop: Integer);
var
  Rct: TsgeIntRect;
begin
  Windows.GetWindowRect(FHandle, @Rct);
  SetWindowPos(FHandle, GetHWNDPos, Rct.X1, ATop, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE);
end;


function TsgeWindow.GetTop: Integer;
var
  Rct: TsgeIntRect;
begin
  Windows.GetWindowRect(FHandle, @Rct);
  Result := Rct.Y1;
end;


procedure TsgeWindow.SetWindowWidth(AWidth: Integer);
var
  Rct: TsgeIntRect;
  H: Integer;
begin
  Windows.GetWindowRect(FHandle, @Rct);
  H := Rct.Y2 - Rct.Y1;
  Windows.SetWindowPos(FHandle, GetHWNDPos, 0, 0, AWidth, H, SWP_NOMOVE or SWP_NOACTIVATE);
end;


function TsgeWindow.GetWindowWidth: Integer;
var
  Rct: TsgeIntRect;
begin
  Windows.GetWindowRect(FHandle, @Rct);
  Result := Rct.X2 - Rct.X1;
end;


procedure TsgeWindow.SetWindowHeight(AHeight: Integer);
var
  Rct: TsgeIntRect;
  W: Integer;
begin
  Windows.GetWindowRect(FHandle, @Rct);
  W := Rct.X2 - Rct.X1;
  Windows.SetWindowPos(FHandle, GetHWNDPos, 0, 0, W, AHeight, SWP_NOMOVE or SWP_NOACTIVATE);
end;


function TsgeWindow.GetWindowHeight: Integer;
var
  Rct: TsgeIntRect;
begin
  Windows.GetWindowRect(FHandle, @Rct);
  Result := Rct.Y2 - Rct.Y1;
end;


procedure TsgeWindow.SetWidth(AWidth: Integer);
var
  NewWidth, NewHeight: Integer;
  WRct, Rct: TsgeIntRect;
begin
  Windows.GetWindowRect(FHandle, @WRct);
  Windows.GetClientRect(FHandle, @Rct);
  NewWidth := AWidth + (WRct.X2 - WRct.X1 - Rct.X2);
  NewHeight := WRct.Y2 - WRct.Y1;
  Windows.SetWindowPos(FHandle, GetHWNDPos, 0, 0, NewWidth, NewHeight, SWP_NOMOVE or SWP_NOACTIVATE);
end;


function TsgeWindow.GetWidth: Integer;
var
  Rct: TsgeIntRect;
begin
  Windows.GetClientRect(FHandle, @Rct);
  Result := Rct.X2;
end;


procedure TsgeWindow.SetHeight(AHeight: Integer);
var
  NewWidth, NewHeight: Integer;
  WRct, Rct: TsgeIntRect;
begin
  Windows.GetWindowRect(FHandle, @WRct);
  Windows.GetClientRect(FHandle, @Rct);
  NewHeight := AHeight + (WRct.Y2 - WRct.Y1 - Rct.Y2);
  NewWidth := WRct.X2 - WRct.X1;
  Windows.SetWindowPos(FHandle, GetHWNDPos, 0, 0, NewWidth, NewHeight, SWP_NOMOVE or SWP_NOACTIVATE);
end;


function TsgeWindow.GetHeight: Integer;
var
  Rct: TsgeIntRect;
begin
  Windows.GetClientRect(FHandle, @Rct);
  Result := Rct.Y2;
end;


procedure TsgeWindow.ChangeWindowStyle;
var
  NewStyle: DWORD;
begin
  //Изменить стандартный стиль окна
  NewStyle := 0;                                                      //Обнулить, при в ходе имеет уже значение
  if GetVisible then NewStyle := WS_VISIBLE;                          //Если окно видимо, то учесть флаг
  NewStyle := NewStyle or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;         //Для OpenGL
  if wsCaption in FStyle then NewStyle := NewStyle or WS_CAPTION;     //Заголовок окна
  if wsSystemMenu in FStyle then NewStyle := NewStyle or WS_SYSMENU;  //Системное меню с кнопками
  if wsSizeable in FStyle then NewStyle := NewStyle + WS_SIZEBOX;     //Изменение размеров

  //Кнопки
  if wbMaximize in FButtons then NewStyle := NewStyle or WS_MAXIMIZEBOX;
  if wbMinimize in FButtons then NewStyle := NewStyle or WS_MINIMIZEBOX;

  //Затенить/показать "закрыть" через MenuItem
  if wbClose in FButtons then EnableMenuItem(FSystemMenu, SC_CLOSE, MF_ENABLED)
      else EnableMenuItem(FSystemMenu, SC_CLOSE, MF_GRAYED or MF_DISABLED);

  //Применить изменения
  SetWindowLongPtr(FHandle, GWL_STYLE, NewStyle);

  //Двойной клик на окне
  NewStyle := CS_HREDRAW or CS_VREDRAW;
  if wsDoubleClick in FStyle then NewStyle := NewStyle or CS_DBLCLKS;

  //Установить стиль
  SetClassLongPtr(FHandle, GCL_STYLE, NewStyle);

  //Пошевелить окно
  SetWindowPos(FHandle, GetHWNDPos, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOACTIVATE);
end;


constructor TsgeWindow.Create(WndClassName, Caption: String; Left, Top: Integer; Width, Height: Integer);
var
  NewWidth, NewHeight: Integer;
  Rct: TsgeIntRect;
begin
  //Подготовительные работы
  FStyle := [wsCaption, wsSizeable, wsSystemMenu];    //Заполняем начальный стиль окна
  //FOldStyle := FStyle;                                //Запомнить старый стиль
  FButtons := [wbClose, wbMinimize, wbMaximize];      //Заполняем системные кнопки

  //Заполняем класс окна
  with FWindowClass do
    begin
    cbSize := SizeOf(TWNDClassEx);                    //Размер структуры
    style := CS_HREDRAW or CS_VREDRAW;                //Стиль окна
    lpfnWndProc := @DefWindowProc;                    //Обработчик событий окна
    cbClsExtra := 0;                                  //Дополнительная память
    cbWndExtra := 0;                                  //Дополнительная память для всех потомков
    hInstance := System.HINSTANCE;                    //Адрес начала данных приложения Win32
    hIcon := LoadIcon(0, IDI_APPLICATION);            //Загрузка стандартного значка приложения
    hCursor := LoadCursor(0, IDC_ARROW);              //Загрузка стандартного курсора Win32
    hbrBackground := GetStockObject(BLACK_BRUSH);     //Заливка стандартной чёрной кистью
    lpszMenuName := nil;                              //Имя строки-ресурса системного меню
    lpszClassName := PChar(WndClassName);             //Уникальное имя класса
    hIconSm := 0;                                     //Ссылка на маленькую иконку
    end;

  //Регистрация окна
  if RegisterClassEx(FWindowClass) = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantRegisterClass, WndClassName);

  //Создание окна
  FHandle := CreateWindow(FWindowClass.lpszClassName, PChar(Caption), WS_OVERLAPPEDWINDOW, Left, Top, Width, Height, 0, 0, FWindowClass.hInstance, nil);
  if FHandle = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateWindow, Caption);

  //Ссылка на системное меню формы
  FSystemMenu := GetSystemMenu(FHandle, False);

  //Поправить стиль
  ChangeWindowStyle;

  //Изменить размеры на нормальные
  Windows.GetClientRect(FHandle, @Rct);
  NewWidth := Width + (Width - Rct.X2);
  NewHeight := Height + (Height - Rct.Y2);
  Windows.SetWindowPos(FHandle, GetHWNDPos, 0, 0, NewWidth, NewHeight, SWP_NOMOVE or SWP_NOACTIVATE);
end;



destructor TsgeWindow.Destroy;
begin
  DestroyWindow(FHandle);                                                       //Удаляем окно
  Windows.UnregisterClass(FWindowClass.lpszClassName, FWindowClass.hInstance);  //Удаляем класс окна
end;


function TsgeWindow.IsFocused: Boolean;
begin
  Result := (Windows.GetForegroundWindow = FHandle);
end;


procedure TsgeWindow.Show;
begin
  Windows.ShowWindow(FHandle, SW_SHOWNORMAL);
end;


procedure TsgeWindow.Hide;
begin
  Windows.ShowWindow(FHandle, SW_HIDE);
end;


procedure TsgeWindow.Activate;
begin
  if Windows.IsIconic(FHandle) = LongBool(1) then Restore
    else Windows.SetForegroundWindow(FHandle);
end;


procedure TsgeWindow.Minimize;
begin
  Windows.ShowWindow(FHandle, SW_MINIMIZE);
end;


procedure TsgeWindow.Maximize;
begin
  Windows.ShowWindow(FHandle, SW_MAXIMIZE);
end;


procedure TsgeWindow.Restore;
begin
  Windows.ShowWindow(FHandle, SW_RESTORE);
end;


procedure TsgeWindow.Update;
begin
  Windows.InvalidateRect(FHandle, nil, True);
end;


procedure TsgeWindow.SetWindowProc(Proc: Pointer);
begin
  if Proc = nil then Exit;
  Windows.SetWindowLongPtr(FHandle, GWLP_WNDPROC,  LONG_PTR(Proc));
end;


procedure TsgeWindow.Center(APos: TsgeWindowCenterPos);
var
  W, H: Integer;
  Rct: TsgeIntRect;
begin
  //Определить размеры центрирования
  case APos of
    //Размер первого экрана
    wcpScreen:
      begin
      W := Windows.GetSystemMetrics(SM_CXSCREEN);
      H := Windows.GetSystemMetrics(SM_CYSCREEN);
      end;

    //Клиенстская область рабочего стола
    wcpClientArea:
      begin
      W := Windows.GetSystemMetrics(SM_CXFULLSCREEN);
      H := Windows.GetSystemMetrics(SM_CYFULLSCREEN);
      end;

    //Размер всех мониторов
    wcpVirtualScreen:
      begin
      W := Windows.GetSystemMetrics(SM_CXVIRTUALSCREEN);
      H := Windows.GetSystemMetrics(SM_CYVIRTUALSCREEN);
      end;
  end;

  //Получить размеры окна
  Windows.GetClientRect(FHandle, @Rct);

  //Изменить размеры
  SetLeft(W div 2 - Rct.X2 div 2);
  SetTop(H div 2 - Rct.Y2 div 2);
end;



end.

