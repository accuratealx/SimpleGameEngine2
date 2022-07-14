{
Пакет             Simple Game Engine 2
Файл              sgeSystemConsole.pas
Версия            1.1
Создан            25.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс: Взаимодействия с системной консолью
}
{$Include Defines.inc}

unit sgeSystemConsole;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes;


const
  //Кодовые страницы
  CP_866          = 866;  //Кирилица Dos
  CP_Windows1251  = 1251; //Кирилица Windows


type
  //Цвета консоли
  TsgeSystemConsoleColors = (sccBlack, sccNavy, sccGreen, sccTeal, sccMaroon, sccPurple, sccOlive, sccGray,
                             sccSilver, sccBlue, sccLime, sccAqua, sccRed, sccFuchsia, sccYellow, sccWhite);


  //Класс консоли
  TsgeSystemConsole = class
  private
    FInputHandle: THandle;
    FOutputHandle: THandle;
    FErrorHandle: THandle;

    //Вспомогательные методы
    function GetColorAttrib(TxtCol, BGCol: TsgeSystemConsoleColors): Byte;  //Возвращает Атрибут цвета и фона текста
    function ColorToWordColor(Color: TsgeSystemConsoleColors): Byte;
    function WordColorToColor(WordColor: Byte): TsgeSystemConsoleColors;

    //Методы свойств
    procedure SetCaption(ACaption: String);
    function  GetCaption: String;
    procedure SetInputCodePage(ACodePage: Cardinal);
    function  GetInputCodePage: Cardinal;
    procedure SetOutputCodePage(ACodePage: Cardinal);
    function  GetOutputCodePage: Cardinal;
    procedure SetTextColor(AColor: TsgeSystemConsoleColors);
    function  GetTextColor: TsgeSystemConsoleColors;
    procedure SetBGColor(AColor: TsgeSystemConsoleColors);
    function  GetBGColor: TsgeSystemConsoleColors;
    procedure SetCursorPos(APos: TsgeSmallPoint);
    function  GetCursorPos: TsgeSmallPoint;
    function  GetWidth: Word;
    function  GetHeight: Word;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure FillColors;
    procedure Clear(Chr: Char = #32);

    procedure Write(Msg: String);
    procedure WriteLn(Msg: String);

    property Caption: String read GetCaption write SetCaption;
    property InputCodePage: Cardinal read GetInputCodePage write SetInputCodePage;
    property OutputCodePage: Cardinal read GetOutputCodePage write SetOutputCodePage;
    property TextColor: TsgeSystemConsoleColors read GetTextColor write SetTextColor;
    property BGColor: TsgeSystemConsoleColors read GetBGColor write SetBGColor;
    property CursorPos: TsgeSmallPoint read GetCursorPos write SetCursorPos;
    property Width: Word read GetWidth;
    property Height: Word read GetHeight;
  end;


var
  SystemConsole: TsgeSystemConsole;


implementation

uses
  sgeErrors, sgeOSPlatform,
  Windows;

const
  _UNITNAME = 'SystemConsole';

  Err_CantCreateConsole       = 'CantCreateConsole';
  Err_CantGetSTDInputHandle   = 'CantGetSTDInputHandle';
  Err_CantGetSTDOutputHandle  = 'CantGetSTDOutputHandle';
  Err_CantGetSTDErrorHandle   = 'CantGetSTDErrorHandle';
  Err_CantSetCaption          = 'CantSetCaption';
  Err_CantGetCaption          = 'CantGetCaption';
  Err_CantSetInputCodePage    = 'CantSetInputCodePage';
  Err_CantGetInputCodePage    = 'CantGetInputCodePage';
  Err_CantSetOutputCodePage   = 'CantSetOutputCodePage';
  Err_CantGetOutputCodePage   = 'CantGetOutputCodePage';
  Err_CantSetTextColor        = 'CantSetTextColor';
  Err_CantGetTextColor        = 'CantGetTextColor';
  Err_CantSetBGColor          = 'CantSetBGColor';
  Err_CantGetBGColor          = 'CantGetBGColor';
  Err_CantSetCursorPos        = 'CantSetCursorPos';
  Err_CantGetCursorPos        = 'CantGetCursorPos';
  Err_CantGetWidth            = 'CantGetWidth';
  Err_CantGetHeight           = 'CantGetHeight';
  Err_CantFillColor           = 'CantFillColor';
  Err_CantWrite               = 'CantWrite';




function TsgeSystemConsole.GetColorAttrib(TxtCol, BGCol: TsgeSystemConsoleColors): Byte;
var
  Txt, BG: Byte;
begin
  Txt := ColorToWordColor(TxtCol);
  BG := ColorToWordColor(BGCol);

  Result := Txt or BG shl 4;
end;


function TsgeSystemConsole.ColorToWordColor(Color: TsgeSystemConsoleColors): Byte;
begin
  Result := Ord(Color);
end;


function TsgeSystemConsole.WordColorToColor(WordColor: Byte): TsgeSystemConsoleColors;
begin
  Result := TsgeSystemConsoleColors(WordColor);
end;


procedure TsgeSystemConsole.SetCaption(ACaption: String);
begin
  if not SetConsoleTitle(PChar(ACaption)) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetCaption, GetLastOSErrorMessage);
end;


function TsgeSystemConsole.GetCaption: String;
var
  Buf: array[0..MAX_PATH] of Char;
begin
  if GetConsoleTitle(@Buf, MAX_PATH) = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantGetCaption, GetLastOSErrorMessage);

  Result := Buf;
end;


procedure TsgeSystemConsole.SetInputCodePage(ACodePage: Cardinal);
begin
  if not SetConsoleCP(ACodePage) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetInputCodePage, GetLastOSErrorMessage);
end;


function TsgeSystemConsole.GetInputCodePage: Cardinal;
begin
  Result := GetConsoleCP;
  if Result = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantGetInputCodePage, GetLastOSErrorMessage);
end;


procedure TsgeSystemConsole.SetOutputCodePage(ACodePage: Cardinal);
begin
  if not SetConsoleOutputCP(ACodePage) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetOutputCodePage, GetLastOSErrorMessage);
end;


function TsgeSystemConsole.GetOutputCodePage: Cardinal;
begin
  Result := GetConsoleOutputCP;
  if Result = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantGetOutputCodePage, GetLastOSErrorMessage);
end;


procedure TsgeSystemConsole.SetTextColor(AColor: TsgeSystemConsoleColors);
var
  Att: Byte;
begin
  Att := GetColorAttrib(AColor, GetBGColor);

  if not SetConsoleTextAttribute(FOutputHandle, Att) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetTextColor, GetLastOSErrorMessage);
end;


function TsgeSystemConsole.GetTextColor: TsgeSystemConsoleColors;
var
  Info: CONSOLE_SCREEN_BUFFER_INFO;
begin
  if not GetConsoleScreenBufferInfo(FOutputHandle, Info) then
    raise EsgeException.Create(_UNITNAME, Err_CantGetTextColor, GetLastOSErrorMessage);

  Result := WordColorToColor(Lo(Byte(Info.wAttributes)));
end;


procedure TsgeSystemConsole.SetBGColor(AColor: TsgeSystemConsoleColors);
var
  Att: Byte;
begin
  Att := GetColorAttrib(GetTextColor, AColor);

  if not SetConsoleTextAttribute(FOutputHandle, Att) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetBGColor, GetLastOSErrorMessage);
end;


function TsgeSystemConsole.GetBGColor: TsgeSystemConsoleColors;
var
  Info: CONSOLE_SCREEN_BUFFER_INFO;
begin
  if not GetConsoleScreenBufferInfo(FOutputHandle, Info) then
    raise EsgeException.Create(_UNITNAME, Err_CantGetBGColor, GetLastOSErrorMessage);

  Result := WordColorToColor(Hi(Byte(Info.wAttributes)));
end;


procedure TsgeSystemConsole.SetCursorPos(APos: TsgeSmallPoint);
begin
  if not SetConsoleCursorPosition(FOutputHandle, COORD(APos)) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetCursorPos, GetLastOSErrorMessage);
end;


function TsgeSystemConsole.GetCursorPos: TsgeSmallPoint;
var
  Info: CONSOLE_SCREEN_BUFFER_INFO;
begin
  if not GetConsoleScreenBufferInfo(FOutputHandle, Info) then
    raise EsgeException.Create(_UNITNAME, Err_CantGetCursorPos, GetLastOSErrorMessage);

  Result := TsgeSmallPoint(Info.dwCursorPosition);
end;


function TsgeSystemConsole.GetWidth: Word;
var
  Info: CONSOLE_SCREEN_BUFFER_INFO;
begin
  if not GetConsoleScreenBufferInfo(FOutputHandle, Info) then
    raise EsgeException.Create(_UNITNAME, Err_CantGetWidth, GetLastOSErrorMessage);

  Result := Info.dwSize.X;
end;


function TsgeSystemConsole.GetHeight: Word;
var
  Info: CONSOLE_SCREEN_BUFFER_INFO;
begin
  if not GetConsoleScreenBufferInfo(FOutputHandle, Info) then
    raise EsgeException.Create(_UNITNAME, Err_CantGetHeight, GetLastOSErrorMessage);

  Result := Info.dwSize.Y;
end;


constructor TsgeSystemConsole.Create;
begin
  if not AllocConsole then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateConsole, GetLastOSErrorMessage);

  FInputHandle := GetStdHandle(STD_INPUT_HANDLE);
  if FInputHandle = INVALID_HANDLE_VALUE then
    raise EsgeException.Create(_UNITNAME, Err_CantGetSTDInputHandle, GetLastOSErrorMessage);

  FOutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  if FOutputHandle = INVALID_HANDLE_VALUE then
    raise EsgeException.Create(_UNITNAME, Err_CantGetSTDOutputHandle, GetLastOSErrorMessage);

  FErrorHandle := GetStdHandle(STD_ERROR_HANDLE);
  if FErrorHandle = INVALID_HANDLE_VALUE then
    raise EsgeException.Create(_UNITNAME, Err_CantGetSTDErrorHandle, GetLastOSErrorMessage);
end;


destructor TsgeSystemConsole.Destroy;
begin
  FreeConsole;
end;


procedure TsgeSystemConsole.FillColors;
var
  Info: CONSOLE_SCREEN_BUFFER_INFO;
  Len, Dummy: DWORD;
  Att: Byte;
  Crd: TCOORD;
begin
  //Запросить параметры консоли
  if not GetConsoleScreenBufferInfo(FOutputHandle, Info) then
    raise EsgeException.Create(_UNITNAME, Err_CantFillColor, GetLastOSErrorMessage);

  //Атрибуты текста
  Att := GetColorAttrib(GetTextColor, GetBGColor);

  //Всего символов в буфере
  Len := Info.dwSize.X * Info.dwSize.Y;

  //Стартовая позиция курсора
  Crd.X := 0;
  Crd.Y := 0;

  //Применить цвета от текущей позиции курсора до Len
  if not FillConsoleOutputAttribute(FOutputHandle, Att, Len, Crd, Dummy) then
    raise EsgeException.Create(_UNITNAME, Err_CantFillColor, GetLastOSErrorMessage);
end;


procedure TsgeSystemConsole.Clear(Chr: Char);
var
  Info: CONSOLE_SCREEN_BUFFER_INFO;
  Len, Dummy: DWORD;
  Crd: TsgeSmallPoint;
begin
  //Запросить параметры консоли
  if not GetConsoleScreenBufferInfo(FOutputHandle, Info) then
    raise EsgeException.Create(_UNITNAME, Err_CantFillColor, GetLastOSErrorMessage);

  //Всего символов в буфере
  Len := Info.dwSize.X * Info.dwSize.Y;

  //Стартовая позиция курсора
  Crd.X := 0;
  Crd.Y := 0;

  //Применить цвета от текущей позиции курсора до Len
  if not FillConsoleOutputCharacter(FOutputHandle, Chr, Len, COORD(Crd), Dummy) then
    raise EsgeException.Create(_UNITNAME, Err_CantFillColor, GetLastOSErrorMessage);

  //Вернуть координаты в начало
  SetCursorPos(Crd);
end;


procedure TsgeSystemConsole.Write(Msg: String);
var
  S: String;
  Dummy: DWORD;
begin
  //Перевод в однобайтовую последовательность
  s := Utf8ToAnsi(Msg);

  //Вывод в консоль
  if not WriteConsole(FOutputHandle, PChar(s), Length(s), Dummy, nil) then
    raise EsgeException.Create(_UNITNAME, Err_CantWrite, GetLastOSErrorMessage);
end;


procedure TsgeSystemConsole.WriteLn(Msg: String);
begin
  Self.Write(Msg + LineEnding);
end;



initialization
begin
  SystemConsole := TsgeSystemConsole.Create;
end;


finalization
begin
  SystemConsole.Free;
end;


end.

