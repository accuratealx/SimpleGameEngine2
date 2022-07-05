{
Пакет             Simple Game Engine 2
Файл              sgeEventReceiver.pas
Версия            1.0
Создан            01.07.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Асинхронный класс получения и хранения объектов событий
}
{$Include Defines.inc}

unit sgeEventReceiver;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventList;


type
  TsgeEventReceiver = class
  private
    FEventList: TsgeEventList;
  public
    constructor Create;
    destructor  Destroy; override;

    function  GetFirstEvent: TsgeEventBase;
    procedure DeleteFirstEvent;
  end;


implementation


constructor TsgeEventReceiver.Create;
begin
  FEventList := TsgeEventList.Create(True);
end;


destructor TsgeEventReceiver.Destroy;
begin
  FEventList.Free;
end;


function TsgeEventReceiver.GetFirstEvent: TsgeEventBase;
begin
  Result := nil;
  if FEventList.Count > 0 then
    Result := FEventList.Item[0];
end;


procedure TsgeEventReceiver.DeleteFirstEvent;
begin
  if FEventList.Count > 0 then
    FEventList.Delete(0);
end;



end.

