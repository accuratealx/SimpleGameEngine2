{
Пакет             Simple Game Engine 2
Файл              sgeControllerList.pas
Версия            1.3
Создан            22.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс хранилища подключённых устройств
}
{$Include Defines.inc}

unit sgeControllerList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection,
  sgeController;


type
  TsgeControllerList = class(specialize TsgeTemplateCollection<TsgeController>)
  public
    procedure Reset;
    procedure Change(Idx1, Idx2: Byte);
    function  Exist(DriverID: Byte): Boolean;
  end;



implementation

uses
  sgeErrors, sgeSystemUtils;


const
  _UNITNAME = 'ControllerList';

  Err_IndexOutOfBounds = 'IndexOutOfBounds';



procedure TsgeControllerList.Reset;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FList[i].Reset;
end;


procedure TsgeControllerList.Change(Idx1, Idx2: Byte);
var
  J: TsgeController;
begin
  if Idx1 < FCount then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Idx1));

  if Idx2 < FCount then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Idx2));

  //Поменять местами
  J := FList[Idx1];
  FList[Idx1] := FList[Idx2];
  FList[Idx2] := J;
end;


function TsgeControllerList.Exist(DriverID: Byte): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to FCount - 1 do
    if DriverID = FList[i].DriverID then
      Exit(True);
end;



end.

