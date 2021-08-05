{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandMouse.pas
Версия            1.0
Создан            04.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Мышь
}
{$Include Defines.inc}

unit sgeKeyCommandMouse;

{$mode objfpc}{$H+}

interface

uses
  sgeKeyCommandTypes;


type
  TsgeKeyCommandMouse = class
  const
    MAX_BUTTONS = 5;

  private
    FList: array[0..MAX_BUTTONS] of TsgeKeyCommandAction;

    function GetCount: Byte;
    function GetKey(Index: Byte): TsgeKeyCommandAction;
    function GetNamedKey(Name: ShortString): TsgeKeyCommandAction;
  public
    constructor Create;
    destructor  Destroy; override;

    function  IndexOf(Name: ShortString): Integer;
    function  GetName(Index: Byte): ShortString;

    procedure Clear;
    procedure Delete(Index: Byte);
    procedure Delete(Name: ShortString);

    property Count: Byte read GetCount;
    property Key[Index: Byte]: TsgeKeyCommandAction read GetKey;
    property NamedKey[Name: ShortString]: TsgeKeyCommandAction read GetNamedKey;
  end;



implementation

uses
  sgeErrors, sgeSystemUtils;

const
  //Имена кнопок мыши
  KeyNames: array[0..5] of ShortString = (
    'Left',       //0
    'Middle',     //1
    'Right',      //2
    'X1',         //3
    'X2',         //4
    'Wheel'       //5
    );

  _UNITNAME = 'KeyCommandKeyboard';



function TsgeKeyCommandMouse.GetCount: Byte;
begin
  Result := MAX_BUTTONS;
end;


function TsgeKeyCommandMouse.GetKey(Index: Byte): TsgeKeyCommandAction;
begin
  if (Index > MAX_BUTTONS) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FList[Index];
end;


function TsgeKeyCommandMouse.GetNamedKey(Name: ShortString): TsgeKeyCommandAction;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_KeyNotFound, Name);

  Result := FList[Idx];
end;


constructor TsgeKeyCommandMouse.Create;
var
  i: Integer;
begin
  for i := 0 to MAX_BUTTONS do
    FList[i] := TsgeKeyCommandAction.Create;
end;


destructor TsgeKeyCommandMouse.Destroy;
var
  i: Integer;
begin
  for i := 0 to MAX_BUTTONS do
    FList[i].Free;
end;


function TsgeKeyCommandMouse.IndexOf(Name: ShortString): Integer;
var
  i: Byte;
begin
  Result := -1;

  Name := LowerCase(Name);
  for i := 0 to MAX_BUTTONS do
    if Name = LowerCase(KeyNames[i]) then
      begin
      Result := i;
      Break;
      end;
end;


function TsgeKeyCommandMouse.GetName(Index: Byte): ShortString;
begin
  Result := KeyNames[Index];
end;


procedure TsgeKeyCommandMouse.Clear;
var
  i: Integer;
begin
  for i := 0 to MAX_BUTTONS do
    Delete(i);
end;


procedure TsgeKeyCommandMouse.Delete(Index: Byte);
begin
  if (Index > MAX_BUTTONS) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  FList[Index].Up := '';
  FList[Index].Down := '';
end;


procedure TsgeKeyCommandMouse.Delete(Name: ShortString);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_KeyNotFound, Name);

  Delete(Idx);
end;





end.

