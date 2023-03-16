{
Пакет             Simple Game Engine 2
Файл              sgeOSPlatform.pas
Версия            1.10
Создан            27.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Функции взаимодействия с операционной системой
}
{$Include Defines.inc}

unit sgeOSPlatform;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  sgeTypes;


const
  //Неправильный хэндл файла
  FileInvalidHandle: THandle = THandle(-1);

  //Режимы открытия файла
  FileModeRead  = 0;
  FileModeWrite = 1;

  //Начало отступа в файле
  FileOffsetBegin = 0;
  FileOffsetCurrent = 1;
  FileOffsetEnd = 2;

  //Время ожидания WaitForSingleObject
  INFINITE = $FFFFFFFF;
  WAIT_OBJECT_0 = 0;
  WAIT_ABANDONED = $80;
  WAIT_TIMEOUT = $102;
  WAIT_FAILED = $FFFFFFFF;

  //Приоритеты потоков
  THREAD_PRIORITY_IDLE          = -(15);
  THREAD_PRIORITY_LOWEST        = -(2);
  THREAD_PRIORITY_BELOW_NORMAL  = -(1);
  THREAD_PRIORITY_NORMAL        = 0;
  THREAD_PRIORITY_ABOVE_NORMAL  = 1;
  THREAD_PRIORITY_HIGHEST       = 2;
  THREAD_PRIORITY_TIME_CRITICAL = 15;

  //Атрибуты файла
  FileAttribDirectory  = $00000010;
  FileAttribAnyFile    = $000001FF;

type
  //Тип диалогового окна
  TsgeMessageType = (mtError, mtInfo);

  //Критическая секция
  TsgeCriticalSection = TRTLCriticalSection;

  //Запись для поиска файла
  TsgeSearchRec = TSearchRec;

  //Атом
  TAtom = Word;



//Операции с файлами
function  sgeGetFileSize(const Filename: String): int64;
function  sgeFileExists(const FileName: String): Boolean;
function  sgeOpenFile(const FileName: String; Mode: Byte = FileModeRead; Exclusive: Boolean = False): THandle;
function  sgeCreateFile(const FileName: String; Mode: Byte = FileModeRead; Exclusive: Boolean = False): THandle;
procedure sgeCloseFile(Handle: THandle);
function  sgeTruncateFile(Handle: THandle; Size: Int64): boolean;
function  sgeFlushFile(Handle: THandle): Boolean;
function  sgeSeekFile(Handle: THandle; Offset: LongInt; Origin: Byte = FileOffsetBegin): LongInt;
function  sgeWriteFile(Handle: THandle; const Buffer; Count: Longint): Boolean;
function  sgeReadFile(Handle: THandle; out Buffer; Count: longint): Boolean;
function  sgeFindFirst(const Path: String; Attr: Longint; out SearchRec: TsgeSearchRec): Longint;
function  sgeFindNext(var SearchRec: TsgeSearchRec): Longint;
procedure sgeFindClose(var SearchRec: TsgeSearchRec);
function  sgeForceDirectories(const Dir: String): Boolean;
function  sgeDirectoryExists(const Directory: String): Boolean;
function  sgeDeleteFile(const FileName: String): Boolean;
function  sgeRenameFile(const OldName, NewName: String): Boolean;


//Процессор
function  GetProcessorCount: Word;


//Ошибки Операционной системы
function  GetLastOSErrorMessage: String;


//Ожидание события от объекта ядра
function sgeWaitForSingleObject(Handle: THandle; TimeoutMSec: DWord = 0): DWord;


//События
function  sgeCreateEvent(ManualReset: Boolean; InitialState: Boolean): THandle;
procedure sgeCloseEvent(Handle: THandle);
function  sgeSetEvent(Handle: THandle): Boolean;
function  sgeResetEvent(Handle: THandle): Boolean;


//Критические секции
procedure sgeInitCriticalSection(var CriticalSection: TsgeCriticalSection);
procedure sgeEnterCriticalSection(var CriticalSection: TsgeCriticalSection);
procedure sgeLeaveCriticalSection(var CriticalSection: TsgeCriticalSection);
procedure sgeDeleteCriticalSection(var CriticalSection: TsgeCriticalSection);
function  sgeTryEnterCriticalSection(var CriticalSection: TsgeCriticalSection): Boolean;


//Потоки
function  sgeGetThreadIdealProcessor(Thread: THandle; out ProcessorNumber: DWord): Boolean;
function  sgeSetThreadIdealProcessor(Thread: THandle; ProcessorNumber: DWORD): Boolean;
function  sgeGetThreadPriority(Thread: THandle; out Priority: LongInt): Boolean;
function  sgeSetThreadPriority(Thread: THandle; Priority: LongInt): Boolean;
function  sgeCreateThread(StackSize: PtrUInt; StartAddress: Pointer; Parameter: Pointer; var ThreadId: DWord): THandle;
procedure sgeDeleteThread(Thread: THandle);
function  sgeSuspendThread(Thread: THandle): Boolean;
function  sgeResumeThread(Thread: THandle): Boolean;


//Параметры запуска
function  sgeGetCommandLine: String;


//Счётчики
function  sgeGetTickCount: Int64;
function  SgeGetApplicationTickCount: Int64;                        //Милисекунд с момента старта приложения
function  sgeGetCPUFrequency: Int64;
function  sgeGetCPUCounter: Int64;


//Приложение
function  sgeGetApplicationDirectory: String;


//Задержки
procedure sgeSleep(Milliseconds: Cardinal);


//Диалоговые окна
procedure sgeShowMessage(Message: String; Caption: String; DlgType: TsgeMessageType = mtError);


//Контроллеры
procedure sgeControllerConfigChanged;
function  sgeGetMaxControllerCount: Byte;
function  sgeControllerExist(ID: Byte): Boolean;


//Буфер обмена
function sgeCopyToClipboard(Str: String): Integer;
function sgeCopyFromClipboard(var Str: String): Integer;


//Атомы
function  sgeGlobalAddAtom(Name: ShortString): TAtom;
procedure sgeGlobalDeleteAtom(Atom: TAtom);
function  sgeGlobalFindAtom(Name: ShortString): TAtom;
function  sgeGlobalGetAtomName(Atom: TAtom): ShortString;


//Раскладки
function sgeDisableIME(IdThread: Cardinal): Boolean;


//Кодировки
function sgeUtf8ToAnsiBytes(Str: String): TsgeByteArray;


var
  OneSecFrequency: Int64;



implementation

uses
  Windows, MMSystem, sgeFileUtils;


type
  //GetThreadIdealProcessorEx
  TProcessorNumber = record
    Group: WORD;
    Number: BYTE;
    Reserved: BYTE;
  end;
  PProcessorNumber = ^TProcessorNumber;


//Экспортные функции
function GetThreadIdealProcessorEx(Thread: HANDLE; ProcessorNumber: PProcessorNumber): BOOL; stdcall; external kernel32 name 'GetThreadIdealProcessorEx';
function SetThreadIdealProcessor(Thread: HANDLE; ProcessorNumber: DWORD): DWORD; stdcall; external kernel32 name 'SetThreadIdealProcessor';
function Wow64SuspendThread(ThreadHandle: HANDLE): DWORD; stdcall; external kernel32 name 'Wow64SuspendThread';
function GetCommandLine: PWideChar; stdcall; external 'kernel32.dll' name 'GetCommandLineW';

function ImmDisableIME(IdThread: DWORD): BOOL; stdcall; external 'Imm32.dll';

function joyConfigChanged(dwFlags: DWORD): MMRESULT; external 'winmm.dll';


var
  AppStartTime: Int64;



function sgeGetFileSize(const Filename: String): int64;
var
  R: TSearchRec;
begin
  Result := -1;

  if SysUtils.FindFirst(FileName, faAnyFile, R) = 0 then
  begin
    Result := R.Size;
    SysUtils.FindClose(R);
  end;
end;


function sgeFileExists(const FileName: String): Boolean;
begin
  Result := SysUtils.FileExists(FileName);
end;


function sgeOpenFile(const FileName: String; Mode: Byte; Exclusive: Boolean): THandle;
var
  Md: Integer;
begin
  //Определить режим доступа к файлу
  case Mode of
    FileModeRead:
      case Exclusive of
        True:
          Md := fmOpenRead or fmShareExclusive;

        False:
          Md := fmOpenRead or fmShareDenyNone;
      end;

    FileModeWrite:
      case Exclusive of
        True:
          Md := fmOpenReadWrite or fmShareExclusive;

        False:
          Md := fmOpenReadWrite or fmShareDenyWrite;
      end;

    else
      Md := fmOpenRead;
  end;

  //Открыть файл
  Result := SysUtils.FileOpen(FileName, Md);
end;


function sgeCreateFile(const FileName: String; Mode: Byte; Exclusive: Boolean): THandle;
var
  Md: Integer;
begin
  //Определить режим доступа к файлу
  case Mode of
    FileModeRead:
      case Exclusive of
        True:
          Md := fmOpenRead or fmShareExclusive;

        False:
          Md := fmOpenRead or fmShareDenyNone;
      end;

    FileModeWrite:
      case Exclusive of
        True:
          Md := fmOpenReadWrite or fmShareExclusive;

        False:
          Md := fmOpenReadWrite or fmShareDenyWrite;
      end;

    else
      Md := fmOpenRead;
  end;

  //Создать файл
  Result := SysUtils.FileCreate(FileName, Md);
end;


procedure sgeCloseFile(Handle: THandle);
begin
  SysUtils.FileClose(Handle);
end;


function sgeTruncateFile(Handle: THandle; Size: Int64): boolean;
begin
  Result := SysUtils.FileTruncate(Handle, Size);
end;


function sgeFlushFile(Handle: THandle): Boolean;
begin
  Result := SysUtils.FileFlush(Handle);
end;


function sgeSeekFile(Handle: THandle; Offset: LongInt; Origin: Byte): LongInt;
begin
  Result := SysUtils.FileSeek(Handle, Offset, Origin);
end;


function sgeWriteFile(Handle: THandle; const Buffer; Count: Longint): Boolean;
begin
  Result := SysUtils.FileWrite(Handle, Buffer, Count) <> feInvalidHandle;
end;


function sgeReadFile(Handle: THandle; out Buffer; Count: longint): Boolean;
begin
  Result := SysUtils.FileRead(Handle, Buffer, Count) <> feInvalidHandle;
end;


function sgeFindFirst(const Path: String; Attr: Longint; out SearchRec: TsgeSearchRec): Longint;
begin
  Result := SysUtils.FindFirst(Path, Attr, SearchRec);
end;


function sgeFindNext(var SearchRec: TsgeSearchRec): Longint;
begin
  Result := SysUtils.FindNext(SearchRec);
end;


procedure sgeFindClose(var SearchRec: TsgeSearchRec);
begin
  SysUtils.FindClose(SearchRec);
end;


function sgeForceDirectories(const Dir: String): Boolean;
begin
  try
    Result := SysUtils.ForceDirectories(Dir);
  except
    Result := False;
  end;
end;


function sgeDirectoryExists(const Directory: String): Boolean;
begin
  Result := SysUtils.DirectoryExists(Directory, True);
end;


function sgeDeleteFile(const FileName: String): Boolean;
begin
  Result := SysUtils.DeleteFile(FileName);
end;


function sgeRenameFile(const OldName, NewName: String): Boolean;
begin
  Result := SysUtils.RenameFile(OldName, NewName);
end;


function GetProcessorCount: Word;
var
  si: TSYSTEMINFO;
begin
  Windows.GetSystemInfo(si);
  Result := si.dwNumberOfProcessors;
end;


function GetLastOSErrorMessage: String;
begin
  Result := SysUtils.SysErrorMessage(SysUtils.GetLastOSError);
end;


function sgeWaitForSingleObject(Handle: THandle; TimeoutMSec: DWord): DWord;
begin
  Result := Windows.WaitForSingleObject(Handle, TimeoutMSec);
end;


function sgeCreateEvent(ManualReset: Boolean; InitialState: Boolean): THandle;
begin
  Result := Windows.CreateEvent(nil, ManualReset, InitialState, '');
end;


procedure sgeCloseEvent(Handle: THandle);
begin
  CloseHandle(Handle);
end;


function sgeSetEvent(Handle: THandle): Boolean;
begin
  Result := Windows.SetEvent(Handle);
end;


function sgeResetEvent(Handle: THandle): Boolean;
begin
  Result := Windows.ResetEvent(Handle);
end;


procedure sgeInitCriticalSection(var CriticalSection: TsgeCriticalSection);
begin
  Windows.InitializeCriticalSection(CriticalSection);
end;


procedure sgeEnterCriticalSection(var CriticalSection: TsgeCriticalSection);
begin
  Windows.EnterCriticalSection(CriticalSection);
end;


procedure sgeLeaveCriticalSection(var CriticalSection: TsgeCriticalSection);
begin
  Windows.LeaveCriticalSection(CriticalSection);
end;


procedure sgeDeleteCriticalSection(var CriticalSection: TsgeCriticalSection);
begin
  Windows.DeleteCriticalSection(CriticalSection);
end;


function sgeTryEnterCriticalSection(var CriticalSection: TsgeCriticalSection): Boolean;
begin
  Result := Windows.TryEnterCriticalSection(CriticalSection);
end;


function sgeGetThreadIdealProcessor(Thread: THandle; out ProcessorNumber: DWord): Boolean;
var
  Info: TProcessorNumber;
begin
  Result := False;
  ProcessorNumber := 0;


  if GetThreadIdealProcessorEx(Thread, @Info) then
  begin
    ProcessorNumber := Info.Number;
    Result := True;
  end;
end;


function sgeSetThreadIdealProcessor(Thread: THandle; ProcessorNumber: DWORD): Boolean;
var
  PID: DWORD;
begin
  PID := SetThreadIdealProcessor(Thread, ProcessorNumber);
  Result := PID <> DWORD(-1);
end;


function sgeGetThreadPriority(Thread: THandle; out Priority: LongInt): Boolean;
begin
  Priority := Windows.GetThreadPriority(Thread);
  Result := Priority <> THREAD_PRIORITY_ERROR_RETURN;
end;


function sgeSetThreadPriority(Thread: THandle; Priority: LongInt): Boolean;
begin
  Result := Windows.SetThreadPriority(Thread, Priority);
end;


function sgeCreateThread(StackSize: PtrUInt; StartAddress: Pointer; Parameter: Pointer; var ThreadId: DWord): THandle;
begin
  Result := Windows.CreateThread(nil, StackSize, StartAddress, Parameter, CREATE_SUSPENDED or STACK_SIZE_PARAM_IS_A_RESERVATION, ThreadId);
end;


procedure sgeDeleteThread(Thread: THandle);
begin
  CloseHandle(Thread);
end;


function sgeSuspendThread(Thread: THandle): Boolean;
var
  D: DWORD;
begin
  {$IfDef WIN64}
    D := Wow64SuspendThread(Thread);
  {$Else}
    D := Windows.SuspendThread(Thread);
  {$EndIf}

  Result := D <> DWORD(-1);
end;


function sgeResumeThread(Thread: THandle): Boolean;
var
  D: DWORD;
begin
  D := Windows.ResumeThread(Thread);

  Result := D <> DWORD(-1);
end;


function sgeGetCommandLine: String;
begin
  Result := WideString(GetCommandLine);
end;


//Patch MrShoor 19.05.2021
function sgeGetTickCount: Int64;
begin
  Result := sgeGetCPUCounter * 1000 div OneSecFrequency;
end;


//MrShoor 19.05.2021
function SgeGetApplicationTickCount: Int64;
begin
  Result := sgeGetTickCount - AppStartTime;
end;

function sgeGetCPUFrequency: Int64;
begin
  Windows.QueryPerformanceFrequency(Result);
end;


function sgeGetCPUCounter: Int64;
begin
  Windows.QueryPerformanceCounter(Result);
end;


function sgeGetApplicationDirectory: String;
begin
  Result := AnsiToUtf8(sgeExtractFilePath(ParamStr(0)));
  Result := sgeCheckPathDelimiter(Result);
end;


procedure sgeSleep(Milliseconds: Cardinal);
begin
  Windows.Sleep(Milliseconds);
end;


procedure sgeShowMessage(Message: String; Caption: String; DlgType: TsgeMessageType);
var
  d: UINT;
begin
  d := MB_OK;

  case DlgType of
    mtError:
       d := d or MB_ICONERROR;

    mtInfo:
      d := d or MB_ICONINFORMATION;
  end;

  MessageBox(0, PChar(Message), PChar(Caption), d);
end;


procedure sgeControllerConfigChanged;
begin
  joyConfigChanged(0);
end;


function sgeGetMaxControllerCount: Byte;
begin
  Result := joyGetNumDevs;
end;


function sgeControllerExist(ID: Byte): Boolean;
var
  J: TJOYINFOEX;
begin
  ZeroMemory(@J, SizeOf(J));
  J.dwSize := SizeOf(J);
  J.dwFlags := JOY_RETURNRAWDATA;
  Result := (joyGetPosEx(ID, @J) = JOYERR_NOERROR);
end;


{
Описание
  Вставить строку в буфер обмена
Параметры
  Str - Строка для вставки
Результат
  0 - Успешно
  1 - Пустая строка
  2 - Невозможно выделить память
  3 - Невозможно заблокировать память
  4 - Невозможно открыть буфер обмена
  5 - Невозможно очистить буфер обмена
  6 - Невозможно записать строку в буфер обмена
  7 - Невозможно закрыть буфер обмена
}
function sgeCopyToClipboard(Str: String): Integer;
var
  ptr: Pointer;
  Handle: HGLOBAL;
  Size: Integer;
  WS: WideString;
begin
  Result := 0;

  if Str = '' then                                                  //Нечего передавать
    Exit(1);

  WS := Str;                                                        //В микрософте юникод 2 байтный, преобразовать

  Size := (Length(WS) + 1) * 2;                                     //Определить длину на конце символ #0

  Handle := GlobalAlloc(GMEM_MOVEABLE, Size);                       //Выделить память из глобальной кучи
  if Handle = 0 then
    Exit(2);

  ptr := GlobalLock(Handle);                                        //Заблокировать память от перемещения
  if ptr = nil then
  begin
    GlobalFree(Handle);
    Exit(3);
  end;

  Move(PWideChar(WS)^, ptr^, Size);                                 //Скопировать строку в глобальную память
  GlobalUnlock(Handle);                                             //Отменить блокировку памяти

  if not OpenClipboard(0) then                                      //Открыть буфер обмена
  begin
    GlobalFree(Handle);
    Exit(4);
  end;

  if not EmptyClipboard then                                        //Стереть данные в буфере обмена от других программ
  begin
    GlobalFree(Handle);
    Exit(5);
  end;

  if SetClipboardData(CF_UNICODETEXT, Handle) = 0 then              //Отдать в буфер обмена строку юникода
  begin
    GlobalFree(Handle);
    Exit(6);
  end;

  if not CloseClipboard then                                        //Закрыть буфер обмена
    Result := 7;
end;


{
Описание
  Скопировать строку из буфера обмена
Параметры
  Str - Строка для вставки
Результат
  0 - Успешно
  1 - Невозможно открыть буфер обмена
  2 - Невозможно прочитать строку из буфера обмена
  3 - Невозможно закрыть буфер обмена
  4 - Невозможно заблокировать память
  5 - Невозможно узнать размер данных
}
function sgeCopyFromClipboard(var Str: String): Integer;
var
  Handle: HGLOBAL;
  ptr: Pointer;
  Size: Integer;
  buf: array of Byte;
begin
  Result := 0;

  if not OpenClipboard(0) then                                      //Открыть буфер обмена
    Exit(1);

  Handle := GetClipboardData(CF_UNICODETEXT);                       //Взять указатель на глобальный блок памяти
  if Handle = 0 then
    Exit(2);

  if not CloseClipboard then                                        //Закрыть буфер обмена
    Exit(3);

  ptr := GlobalLock(Handle);                                        //Заблокировать память от перемещения
  if ptr = nil then
    Exit(4);

  Size := GlobalSize(Handle);                                       //Узнать размер данных
  if Size = 0 then
    Exit(5);

  SetLength(buf, Size);                                             //Подготовить буфер для копирования
  CopyMemory(@buf[0], ptr, Size);                                   //Скопировать юникодную строку в буфер
  GlobalUnlock(Handle);                                             //Отменить блоктровку памяти

  Str := PWideChar(buf);                                            //Преобразовать в строку Ansi

  SetLength(buf, 0);
end;


function sgeGlobalAddAtom(Name: ShortString): TAtom;
begin
  Result := Windows.GlobalAddAtom(PChar(String(Name)));
end;


procedure sgeGlobalDeleteAtom(Atom: TAtom);
begin
  Windows.GlobalDeleteAtom(Atom);
end;


function sgeGlobalFindAtom(Name: ShortString): TAtom;
var
  s: String;
begin
  s := Name;
  Result := Windows.GlobalFindAtom(PChar(s));
end;


function sgeGlobalGetAtomName(Atom: TAtom): ShortString;
var
  Buf: array[0..255] of Char;
  Len: Integer;
begin
  Len := Windows.GlobalGetAtomName(Atom, @Buf, 255);
  Result := Copy(Buf, 0, Len);
end;


function sgeDisableIME(IdThread: Cardinal): Boolean;
begin
  Result := ImmDisableIME(IdThread);
end;


function sgeUtf8ToAnsiBytes(Str: String): TsgeByteArray;
begin
  Result := TEncoding.ANSI.GetAnsiBytes(Str);
end;



initialization
begin
  OneSecFrequency := sgeGetCPUFrequency;

  AppStartTime := 0;
  AppStartTime := SgeGetApplicationTickCount;
end;



end.

