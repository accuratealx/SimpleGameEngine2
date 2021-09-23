{
Пакет             Simple Game Engine 2
Файл              sgeGUIProperty.pas
Версия            1.0
Создан            22.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Базовое свойство
}
{$Include Defines.inc}

unit sgeGUIProperty;

{$mode objfpc}{$H+}

interface

type
  TsgeGUIProperty = class
  protected
    FOwner: TObject;

    procedure ResizeParent;
    procedure RepaintParent;
  public
    constructor Create(AOwner: TObject); virtual;
  end;


implementation

uses
  sgeGUIElement;

type
  TsgeGUIElementHack = class(TsgeGUIElement);


procedure TsgeGUIProperty.ResizeParent;
begin
  if FOwner <> nil then
    TsgeGUIElementHack(FOwner).Notify([esCorrectSize]);
end;


procedure TsgeGUIProperty.RepaintParent;
begin
  if FOwner <> nil then
    TsgeGUIElementHack(FOwner).Notify([esRepaint]);
end;


constructor TsgeGUIProperty.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;



end.

