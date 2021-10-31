{
Пакет             Simple Game Engine 2
Файл              sgeSystemCursor.pas
Версия            1.1
Создан            26.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс системного сурсора *.cur, *.ani для формы
}
{$Include Defines.inc}

unit sgeSystemCursor;

{$mode objfpc}{$H+}

interface

uses
  Windows;


type
  //Типы курсоров по умолчанию
  TsgeSystemCursorType = (sctArrow, sctAppStart, sctCross, sctHand, sctHelp, sctIbeam, sctNo, sctSize, sctSizeUpDown,
                          sctSizeLeftRight, sctSizeTopLeftBottomRight, sctSizeTopRightBottomLeft, sctUpArrow, sctWait);

  TsgeSystemCursor = class
  private
    FHandle: HCURSOR;
    FFileName: String;

    procedure DeleteCursor;
  public
    constructor Create(AType: TsgeSystemCursorType = sctArrow);
    constructor CreateFromFile(FileName: String);
    constructor CreateFromHinstance(Name: String);
    destructor  Destroy; override;

    procedure LoadFromFile(FileName: String);
    procedure LoadFromHinstance(Name: String);
    procedure LoadDefault(AType: TsgeSystemCursorType);
    procedure Reload;

    property Handle: HCURSOR read FHandle;
    property FileName: String read FFileName write FFileName;
  end;



implementation

uses
  sgeErrors;


const
  _UNITNAME = 'SystemCursor';

  Err_CantReadFile          = 'CantReadFile';
  Err_CantLoadFromHinstance = 'CantLoadFromHinstance';



procedure TsgeSystemCursor.DeleteCursor;
begin
  DestroyCursor(FHandle);
end;


constructor TsgeSystemCursor.Create(AType: TsgeSystemCursorType);
begin
  LoadDefault(AType);
end;


constructor TsgeSystemCursor.CreateFromFile(FileName: String);
begin
  FFileName := FileName;
  LoadFromFile(FFileName);
end;


constructor TsgeSystemCursor.CreateFromHinstance(Name: String);
begin
  LoadFromHinstance(Name);
end;


destructor TsgeSystemCursor.Destroy;
begin
  DeleteCursor;
end;


procedure TsgeSystemCursor.LoadFromFile(FileName: String);
var
  H: HCURSOR;
begin
  H := LoadCursorFromFile(PChar(FileName));
  if H = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName);

  DeleteCursor;
  FHandle := H;
end;

procedure TsgeSystemCursor.LoadFromHinstance(Name: String);
var
  H: HCURSOR;
begin
  H := LoadCursor(HINSTANCE, PChar(Name));
  if H = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantLoadFromHinstance, Name);

  DeleteCursor;
  FHandle := H;
end;


procedure TsgeSystemCursor.LoadDefault(AType: TsgeSystemCursorType);
var
  H: HCURSOR;
begin
  case AType of
    sctArrow                  : H := LoadCursor(Hinstance, IDC_ARROW);
    sctAppStart               : H := LoadCursor(Hinstance, IDC_APPSTARTING);
    sctCross                  : H := LoadCursor(Hinstance, IDC_CROSS);
    sctHand                   : H := LoadCursor(Hinstance, IDC_HAND);
    sctHelp                   : H := LoadCursor(Hinstance, IDC_HELP);
    sctIbeam                  : H := LoadCursor(Hinstance, IDC_IBEAM);
    sctNo                     : H := LoadCursor(Hinstance, IDC_NO);
    sctSize                   : H := LoadCursor(Hinstance, IDC_SIZEALL);
    sctSizeUpDown             : H := LoadCursor(Hinstance, IDC_SIZENS);
    sctSizeLeftRight          : H := LoadCursor(Hinstance, IDC_SIZEWE);
    sctSizeTopLeftBottomRight : H := LoadCursor(Hinstance, IDC_SIZENWSE);
    sctSizeTopRightBottomLeft : H := LoadCursor(Hinstance, IDC_SIZENESW);
    sctUpArrow                : H := LoadCursor(Hinstance, IDC_UPARROW);
    sctWait                   : H := LoadCursor(Hinstance, IDC_WAIT);
  end;

  DeleteCursor;
  FHandle := H;
end;


procedure TsgeSystemCursor.Reload;
begin
  LoadFromFile(FFileName);
end;



end.

