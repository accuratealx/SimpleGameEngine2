{
Пакет             Simple Game Engine 2
Файл              sgeWindowsVersion.pas
Версия            1.0
Создан            17.04.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Функция определения версии Windows
}
{$Include Defines.inc}

unit sgeWindowsVersion;

{$mode ObjFPC}{$H+}

interface

type
  TsgeWindowsVersion = (wvUnknown, wvVista, wvServer2008, wvServer2008R2, wv7, WvServer2012,
                        wv8, wvServer2012R2, wv81, wvServer2016, wv10);


function sgeGetWindowsVersion: TsgeWindowsVersion;


implementation

type
  //https://docs.microsoft.com/ru-ru/windows/win32/api/winnt/ns-winnt-osversioninfoexa
  OSVERSIONINFOEXA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of CHAR;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;

const
  VER_NT_WORKSTATION = 1;


function GetVersionEx(var VersionInformation: OSVERSIONINFOEXA): Boolean; external 'kernel32' name 'GetVersionExA';


function sgeGetWindowsVersion: TsgeWindowsVersion;
var
  Ver: OSVERSIONINFOEXA;
begin
  Result := wvUnknown;

  FillChar(Ver, SizeOf(Ver), 0);
  Ver.dwOSVersionInfoSize := SizeOf(OSVERSIONINFOEXA);
  if not GetVersionEx(Ver) then
    Exit;

  if (Ver.dwMajorVersion = 6)  and (Ver.dwMinorVersion = 0) and (Ver.wProductType = VER_NT_WORKSTATION)  then
    Result := wvVista;

  if (Ver.dwMajorVersion = 6)  and (Ver.dwMinorVersion = 0) and (Ver.wProductType <> VER_NT_WORKSTATION) then
    Result := wvServer2008;

  if (Ver.dwMajorVersion = 6)  and (Ver.dwMinorVersion = 1) and (Ver.wProductType <> VER_NT_WORKSTATION) then
    Result := wvServer2008R2;

  if (Ver.dwMajorVersion = 6)  and (Ver.dwMinorVersion = 1) and (Ver.wProductType = VER_NT_WORKSTATION) then
    Result := wv7;

  if (Ver.dwMajorVersion = 6)  and (Ver.dwMinorVersion = 2) and (Ver.wProductType <> VER_NT_WORKSTATION) then
    Result := WvServer2012;

  if (Ver.dwMajorVersion = 6)  and (Ver.dwMinorVersion = 2) and (Ver.wProductType = VER_NT_WORKSTATION) then
    Result := wv8;

  if (Ver.dwMajorVersion = 6)  and (Ver.dwMinorVersion = 3) and (Ver.wProductType <> VER_NT_WORKSTATION) then
    Result := wvServer2012R2;

  if (Ver.dwMajorVersion = 6)  and (Ver.dwMinorVersion = 3) and (Ver.wProductType = VER_NT_WORKSTATION) then
    Result := wv81;

  if (Ver.dwMajorVersion = 10) and (Ver.dwMinorVersion = 0) and (Ver.wProductType <> VER_NT_WORKSTATION) then
    Result := wvServer2016;

  if (Ver.dwMajorVersion = 10) and (Ver.dwMinorVersion = 0) and (Ver.wProductType = VER_NT_WORKSTATION) then
    Result := wv10;
end;



end.

