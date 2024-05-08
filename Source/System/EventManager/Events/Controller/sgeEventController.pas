{
Пакет             Simple Game Engine 2
Файл              sgeEventController.pas
Версия            1.0
Создан            07.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллер
}
{$Include Defines.inc}

unit sgeEventController;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


type
  //Базовый класс
  TsgeEventController = class(TsgeEventBase)
  protected
    FID: Byte;
    function GetName: ShortString; override;
  public
    constructor Create(ID: Byte);

    property ID: Byte read FID;
  end;


implementation


function TsgeEventController.GetName: ShortString;
begin
  Result := 'Controller';
end;


constructor TsgeEventController.Create(ID: Byte);
begin
  FID := ID;
end;



end.

