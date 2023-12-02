{
Пакет             Simple Game Engine 2
Файл              sgeSystemIcon.pas
Версия            1.1
Создан            26.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс системного значка *.ico для формы
}
{$Include Defines.inc}

unit sgeSystemIcon;

{$mode objfpc}{$H+}

interface

uses
  Windows;


type
  //Типы иконок по умолчанию
  TsgeSystemIconType = (sitApplication, sitInformation, sitError, sitWarning, sitQuestion, sitShield);


  TsgeSystemIcon = class
  private
    FHandle: HICON;
    FFileName: String;

    procedure DeleteIcon;
  public
    constructor Create(AType: TsgeSystemIconType = sitApplication);
    constructor CreateFromFile(FileName: String);
    constructor CreateFromHinstance(Name: String);
    destructor  Destroy; override;

    procedure LoadFromFile(FileName: String);
    procedure LoadFromHinstance(Name: String);
    procedure LoadDefault(AType: TsgeSystemIconType);
    procedure Reload;

    property Handle: HICON read FHandle;
    property FileName: String read FFileName write FFileName;
  end;



implementation

uses
  sgeErrors;


const
  _UNITNAME = 'SystemIcon';

  Err_CantReadFile          = 'CantReadFile';
  Err_CantLoadFromHinstance = 'CantLoadFromHinstance';



procedure TsgeSystemIcon.DeleteIcon;
begin
  DestroyIcon(FHandle);
end;


constructor TsgeSystemIcon.Create(AType: TsgeSystemIconType);
begin
  LoadDefault(AType);
end;


constructor TsgeSystemIcon.CreateFromFile(FileName: String);
begin
  FFileName := FileName;
  LoadFromFile(FFileName);
end;


constructor TsgeSystemIcon.CreateFromHinstance(Name: String);
begin
  LoadFromHinstance(Name);
end;


destructor TsgeSystemIcon.Destroy;
begin
  DeleteIcon;
end;


procedure TsgeSystemIcon.LoadFromFile(FileName: String);
var
  H: HICON;
begin
  H := LoadImage(0, PChar(FileName), IMAGE_ICON, 0, 0, LR_LOADFROMFILE or LR_DEFAULTSIZE);
  if H = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName);

  DeleteIcon;
  FHandle := H;
end;

procedure TsgeSystemIcon.LoadFromHinstance(Name: String);
var
  H: HICON;
begin
  H := LoadIcon(HINSTANCE, PChar(Name));
  if H = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantLoadFromHinstance, Name);

  DeleteIcon;
  FHandle := H;
end;


procedure TsgeSystemIcon.LoadDefault(AType: TsgeSystemIconType);
var
  H: HICON;
begin
  case AType of
    sitApplication:
      H := Loadicon(0, IDI_APPLICATION);

    sitInformation:
      H := Loadicon(0, IDI_ASTERISK);

    sitError:
      H := Loadicon(0, IDI_HAND);

    sitWarning:
      H := Loadicon(0, IDI_EXCLAMATION);

    sitQuestion:
      H := Loadicon(0, IDI_QUESTION);

    sitShield:
      H := LoadIcon(0, MakeIntResource(32518));
  end;

  DeleteIcon;
  FHandle := H;
end;


procedure TsgeSystemIcon.Reload;
begin
  LoadFromFile(FFileName);
end;



end.

