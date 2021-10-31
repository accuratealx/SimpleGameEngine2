{
Пакет             Simple Game Engine 2
Файл              sgeTrayIcon.pas
Версия            1.1
Создан            23.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Иконка для TrayMenu
}
{$Include Defines.inc}

unit sgeSystemTrayIcon;

{$mode objfpc}{$H+}

interface

uses
  Windows;


type
  //Тип всплывающего сообщения
  TsgeSystemTrayIconMessageType = (mtNone, mtInfo, mtWarning, mtError);


  //Кнопки мыши
  TsgeSystemTrayIconMouseButtons = (mbLeft, mbMiddle, mbRight);


  //Обработчики событий
  TsgeSystemTrayIconMouseHandler = procedure of object;
  TsgeSystemTrayIconMouseButtonHandler = procedure(Button: TsgeSystemTrayIconMouseButtons) of object;


  //Класс
  TsgeSystemTrayIcon = class
  private
    FWindowClass: TWNDClassEx;        //Класс невидимого окна
    FHandle: HANDLE;                  //Хэндл невидимого окна

  private
    FIcon: HICON;                     //Ссылка на Иконку
    FHint: String;                    //Подсказка
    FVisible: Boolean;                //Флаг видимости

    //Обработчики событий
    FOnMouseDown: TsgeSystemTrayIconMouseButtonHandler;
    FOnMouseUp: TsgeSystemTrayIconMouseButtonHandler;
    FOnMouseDblClick: TsgeSystemTrayIconMouseButtonHandler;
    FOnMouseMove: TsgeSystemTrayIconMouseHandler;

    //Обработчики событий
    procedure Handler_OnMouseDown(Button: TsgeSystemTrayIconMouseButtons);
    procedure Handler_OnMouseUp(Button: TsgeSystemTrayIconMouseButtons);
    procedure Handler_OnMouseDblClick(Button: TsgeSystemTrayIconMouseButtons);
    procedure Handler_OnMouseMove;

    //Окно
    procedure CreateWindow;
    procedure DestroyWindow;

    //Иконка
    procedure Add;
    procedure Delete;

    //Свойства
    procedure SetVisible(AVisible: Boolean);
    procedure SetIcon(AIcon: HICON);
    procedure SetHint(AHint: String);

    function WndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
  public
    constructor Create(HIcon: HICON; Hint: String = ''; Visible: Boolean = True);
    destructor  Destroy; override;

    procedure SetFocus;
    procedure ShowMessage(Caption: String; Message: String; MessageType: TsgeSystemTrayIconMessageType = mtInfo; uTimeout: Cardinal = 3000);
    procedure ShowMessage(Caption: String; Message: String; UserIcon: HICON; uTimeout: Cardinal = 3000);

    property Handle: HANDLE read FHandle;
    property Visible: Boolean read FVisible write SetVisible;
    property Icon: HICON read FIcon write SetIcon;
    property Hint: String read FHint write SetHint;

    property OnMouseDown: TsgeSystemTrayIconMouseButtonHandler read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TsgeSystemTrayIconMouseButtonHandler read FOnMouseUp write FOnMouseUp;
    property OnMouseDblClick: TsgeSystemTrayIconMouseButtonHandler read FOnMouseDblClick write FOnMouseDblClick;
    property OnMouseMove: TsgeSystemTrayIconMouseHandler read FOnMouseMove write FOnMouseMove;
  end;


implementation

uses
  sgeErrors,
  SysUtils;

const
  _UNITNAME = 'TrayIcon';

  //Ошибки
  Err_CantRegisterWindowClass = 'CantRegisterWindowClass';
  Err_CantCreateWindow        = 'CantCreateWindow';
  Err_CantAddIcon             = 'CantAddIcon';
  Err_CantChangeIcon          = 'CantChangeIcon';
  Err_CantChangeHint          = 'CantChangeHint';
  Err_CantShowMessage         = 'CantShowMessage';


  //TryIcon CallBack message
  WM_TRAYICONMESSAGE = WM_USER + 1000;


  //Notify Icon Flags
  NIF_MESSAGE = $00000001;
  NIF_ICON = $00000002;
  NIF_TIP = $00000004;
  //NIF_STATE = $00000008;
  NIF_INFO = $00000010;
  //NIF_GUID = $00000020;
  //NIF_REALTIME = $00000040;
  //NIF_SHOWTIP = $00000080;


  //Notify Icon Message
  NIM_ADD = $00000000;
  NIM_MODIFY = $00000001;
  NIM_DELETE = $00000002;
  NIM_SETFOCUS = $00000003;
  //NIM_SETVERSION = $00000004;


type
  //Структура для взаимодействия с иконкой в лотке
  TsgeNotifyIconDataW = record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of WideChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of WideChar;
    u: record
         case longint of
           0 : ( uTimeout : UINT );
           1 : ( uVersion : UINT );
          end;
    szInfoTitle: array[0..63] of WideChar;
    dwInfoFlags: DWORD;
    guidItem: GUID;
    hBalloonIcon: HICON;
  end;


var
  _Self: TsgeSystemTrayIcon;


//Оконная функция
function sgeSystemTrayIconWndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := _Self.WndProc(hWnd, Msg, wParam, lParam);
end;


function GetIconData(Wnd: HANDLE; Icon: HICON; Hint: String): TsgeNotifyIconDataW;
begin
  //Подготовить структуру
  ZeroMemory(@Result, SizeOf(TsgeNotifyIconDataW));
  //если не отнимать последний элемент от записи, то не
  //показывается пользовательская иконка в BaloonHint
  Result.cbSize := SizeOf(TsgeNotifyIconDataW) - SizeOf(TsgeNotifyIconDataW.hBalloonIcon);

  //Обязательные поля
  Result.uID := 10;
  Result.hWnd := Wnd;
  Result.hIcon := Icon;
  Result.uCallbackMessage := WM_TRAYICONMESSAGE;
  Result.szTip := PWideChar(WideString(Utf8ToAnsi(Hint)));
end;


procedure TsgeSystemTrayIcon.Handler_OnMouseDown(Button: TsgeSystemTrayIconMouseButtons);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Button);
end;


procedure TsgeSystemTrayIcon.Handler_OnMouseUp(Button: TsgeSystemTrayIconMouseButtons);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Button);
end;


procedure TsgeSystemTrayIcon.Handler_OnMouseDblClick(Button: TsgeSystemTrayIconMouseButtons);
begin
  if Assigned(FOnMouseDblClick) then FOnMouseDblClick(Button);
end;


procedure TsgeSystemTrayIcon.Handler_OnMouseMove;
begin
  if Assigned(FOnMouseMove) then FOnMouseMove;
end;


procedure TsgeSystemTrayIcon.CreateWindow;
var
  GUID: TGUID;
  s: String;
begin
  //Уникальный класс окна
  CreateGUID(GUID);
  s := GUIDToString(GUID);

  //Заполнить класс окна
  with FWindowClass do
    begin
    cbSize := SizeOf(TWNDClassEx);                    //Размер структуры
    style := CS_HREDRAW or CS_VREDRAW;                //Стиль окна
    lpfnWndProc := @sgeSystemTrayIconWndProc;         //Обработчик событий окна
    cbClsExtra := 0;                                  //Дополнительная память
    cbWndExtra := 0;                                  //Дополнительная память для всех потомков
    hInstance := System.HINSTANCE;                    //Адрес начала данных приложения Win32
    hIcon := LoadIcon(0, IDI_APPLICATION);            //Загрузка стандартного значка приложения
    hCursor := LoadCursor(0, IDC_ARROW);              //Загрузка стандартного курсора Win32
    hbrBackground := GetStockObject(BLACK_BRUSH);     //Заливка стандартной чёрной кистью
    lpszMenuName := nil;                              //Имя строки-ресурса системного меню
    lpszClassName := PChar(s);                        //Уникальное имя класса
    hIconSm := 0;                                     //Ссылка на маленькую иконку
    end;


  //Регистрация окна
  if RegisterClassEx(FWindowClass) = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantRegisterWindowClass, s);

  //Создание окна
  FHandle := Windows.CreateWindow(FWindowClass.lpszClassName, PChar(''), WS_OVERLAPPEDWINDOW, 0, 0, 100, 100, 0, 0, FWindowClass.hInstance, nil);
  if FHandle = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateWindow);
end;


procedure TsgeSystemTrayIcon.DestroyWindow;
begin
  //Удаляем окно
  Windows.DestroyWindow(FHandle);

  //Удаляем класс окна
  Windows.UnregisterClass(FWindowClass.lpszClassName, FWindowClass.hInstance);
end;


procedure TsgeSystemTrayIcon.Add;
var
  ID: TsgeNotifyIconDataW;
begin
  //Заполнить начальную структуру
  ID := GetIconData(FHandle, FIcon, FHint);

  //Заполнить поля
  ID.uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;

  //Добавить иконку
  if Shell_NotifyIconW(NIM_ADD, @ID) = False then
    raise EsgeException.Create(_UNITNAME, Err_CantAddIcon);
end;


procedure TsgeSystemTrayIcon.Delete;
var
  ID: TsgeNotifyIconDataW;
begin
  //Заполнить начальную структуру
  ID := GetIconData(FHandle, FIcon, FHint);

  //Удалить иконку
  Shell_NotifyIconW(NIM_DELETE, @ID);
end;


procedure TsgeSystemTrayIcon.SetVisible(AVisible: Boolean);
begin
  if FVisible = AVisible then Exit;

  FVisible := AVisible;
  case FVisible of
    True : Add;
    False: Delete;
  end;
end;


procedure TsgeSystemTrayIcon.SetIcon(AIcon: HICON);
var
  ID: TsgeNotifyIconDataW;
begin
  if FIcon = AIcon then Exit;

  //Запомнить параметр
  FIcon := AIcon;

  //Заполнить начальную структуру
  ID := GetIconData(FHandle, FIcon, FHint);

  //Задать параметры
  ID.uFlags := NIF_ICON;

  //Изменить параметры
  if FVisible then
    if Shell_NotifyIconW(NIM_MODIFY, @ID) = False then
      raise EsgeException.Create(_UNITNAME, Err_CantChangeIcon);
end;


procedure TsgeSystemTrayIcon.SetHint(AHint: String);
var
  ID: TsgeNotifyIconDataW;
begin
  if FHint = AHint then Exit;

  //Запомнить параметр
  FHint := AHint;

  //Заполнить начальную структуру
  ID := GetIconData(FHandle, FIcon, FHint);

  //Задать параметры
  ID.uFlags := NIF_TIP;

  //Изменить параметры
  if FVisible then
    if Shell_NotifyIconW(NIM_MODIFY, @ID) = False then
      raise EsgeException.Create(_UNITNAME, Err_CantChangeHint);
end;


function TsgeSystemTrayIcon.WndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
begin
  Result := 0;

  case Msg of
    WM_TRAYICONMESSAGE:

      case lParam of
        WM_LBUTTONDOWN    : Handler_OnMouseDown(mbLeft);
        WM_LBUTTONUP      : Handler_OnMouseUp(mbLeft);
        WM_LBUTTONDBLCLK  : Handler_OnMouseDblClick(mbLeft);
        WM_MBUTTONDOWN    : Handler_OnMouseDown(mbMiddle);
        WM_MBUTTONUP      : Handler_OnMouseUp(mbMiddle);
        WM_MBUTTONDBLCLK  : Handler_OnMouseDblClick(mbMiddle);
        WM_RBUTTONDOWN    : Handler_OnMouseDown(mbRight);
        WM_RBUTTONUP      : Handler_OnMouseUp(mbRight);
        WM_RBUTTONDBLCLK  : Handler_OnMouseDblClick(mbRight);
        WM_MOUSEMOVE      : Handler_OnMouseMove;
      end;

    else
      Result := DefWindowProc(hWnd, Msg, wParam, lParam);
  end;
end;


constructor TsgeSystemTrayIcon.Create(HIcon: HICON; Hint: String; Visible: Boolean);
begin
  //Создать окно
  CreateWindow;

  //Сохранить параметры
  FIcon := HIcon;
  FHint := Hint;
  FVisible := False;

  //Установить видимость
  SetVisible(Visible);

  _Self := Self;
end;


destructor TsgeSystemTrayIcon.Destroy;
begin
  //Удалить иконку
  Delete;

  //Удалить окно
  DestroyWindow;
end;


procedure TsgeSystemTrayIcon.SetFocus;
var
  ID: TsgeNotifyIconDataW;
begin
  if not FVisible then Exit;

  //Заполнить начальную структуру
  ID := GetIconData(FHandle, FIcon, FHint);

  //Установить фокус
  Shell_NotifyIconW(NIM_SETFOCUS, @ID);
end;


procedure TsgeSystemTrayIcon.ShowMessage(Caption: String; Message: String; MessageType: TsgeSystemTrayIconMessageType; uTimeout: Cardinal);
var
  aCpt, aTxt: WideString;
  ID: TsgeNotifyIconDataW;
begin
  if not FVisible then Exit;

  //Подготовить строки
  aCpt := Utf8ToAnsi(Caption);
  aTxt := Utf8ToAnsi(Message);

  //Заполнить начальную структуру
  ID := GetIconData(FHandle, FIcon, FHint);

  //Подготовить данные для всплывающей подсказки
  ID.uFlags := NIF_INFO;
  ID.u.uTimeout := uTimeout;
  ID.szInfo := PWideChar(aTxt);       //255 символов максимум
  ID.szInfoTitle := PWideChar(aCpt);  //63 символа максимум
  ID.dwInfoFlags := Ord(MessageType);

  //Вызвать подсказку
  if Shell_NotifyIconW(NIM_MODIFY, @ID) = False then
    raise EsgeException.Create(_UNITNAME, Err_CantShowMessage);
end;


procedure TsgeSystemTrayIcon.ShowMessage(Caption: String; Message: String; UserIcon: HICON; uTimeout: Cardinal);
var
  aCpt, aTxt: WideString;
  ID: TsgeNotifyIconDataW;
begin
  if not FVisible then Exit;

  //Подготовить строки
  aCpt := Utf8ToAnsi(Caption);
  aTxt := Utf8ToAnsi(Message);

  //Заполнить начальную структуру
  ID := GetIconData(FHandle, FIcon, FHint);

  //Подготовить данные для всплывающей подсказки
  ID.uFlags := NIF_INFO;
  ID.u.uTimeout := uTimeout;
  ID.szInfo := PWideChar(aTxt);       //255 символов максимум
  ID.szInfoTitle := PWideChar(aCpt);  //63 символа максимум
  ID.hBalloonIcon := UserIcon;
  ID.dwInfoFlags := 4; //Пользовательская иконка (NIIF_USER = 4);

  //Вызвать подсказку
  if Shell_NotifyIconW(NIM_MODIFY, @ID) = False then
    raise EsgeException.Create(_UNITNAME, Err_CantShowMessage);
end;




end.

