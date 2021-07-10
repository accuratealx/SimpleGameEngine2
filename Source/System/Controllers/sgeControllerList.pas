{
Пакет             Simple Game Engine 2
Файл              sgeControllerList.pas
Версия            1.2
Создан            22.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс хранилища подключённых устройств
}
{$Include Defines.inc}

unit sgeControllerList;

{$mode objfpc}{$H+}

interface

uses
  sgeController;


type
  TsgeControllerList = class
  private
    FList: array of TsgeController;

    function  GetCount: Byte;

    function  GetItem(Index: Byte): TsgeController;
  public
    destructor  Destroy; override;

    procedure Add(Controller: TsgeController);
    procedure Delete(Index: Byte);

    procedure Clear;
    procedure Reset;
    procedure Change(Idx1, Idx2: Byte);

    function Exist(DriverID: Byte): Boolean;

    property Count: Byte read GetCount;
    property Item[Index: Byte]: TsgeController read GetItem;
  end;




implementation

uses
  sgeErrors, sgeSystemUtils;


const
  _UNITNAME = 'ControllerList';

  Err_IndexOutOfBounds = 'IndexOutOfBounds';



function TsgeControllerList.GetCount: Byte;
begin
  Result := Length(FList);
end;


function TsgeControllerList.GetItem(Index: Byte): TsgeController;
begin
  if Index > GetCount - 1 then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FList[Index];
end;


destructor TsgeControllerList.Destroy;
begin
  Clear;
end;


procedure TsgeControllerList.Add(Controller: TsgeController);
var
  c: Integer;
begin
  c := GetCount;
  SetLength(FList, c + 1);
  FList[c] := Controller;
end;


procedure TsgeControllerList.Delete(Index: Byte);
var
  i, c: Integer;
begin
  c := GetCount - 1;
  if Index > c then Exit;

  FList[Index].Free;

  for i := Index to c - 1 do
    FList[i] := FList[i + 1];

  SetLength(FList, c);
end;


procedure TsgeControllerList.Clear;
var
  i, c: Integer;
begin
  c := GetCount - 1;
  for i := 0 to c do
    FList[i].Free;

  SetLength(FList, 0);
end;


procedure TsgeControllerList.Reset;
var
  i, c: Integer;
begin
  c := GetCount - 1;
  for i := 0 to c do
    FList[i].Reset;
end;


procedure TsgeControllerList.Change(Idx1, Idx2: Byte);
var
  J: TsgeController;
begin
  if Idx1 < GetCount then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Idx1));

  if Idx2 < GetCount then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Idx2));

  //Поменять местами
  J := FList[Idx1];
  FList[Idx1] := FList[Idx2];
  FList[Idx2] := J;
end;


function TsgeControllerList.Exist(DriverID: Byte): Boolean;
var
  i, c: Integer;
begin
  Result := False;

  c := GetCount - 1;
  for i := 0 to c do
    if DriverID = FList[i].DriverID then
      begin
      Result := True;
      Break;
      end;
end;



end.

