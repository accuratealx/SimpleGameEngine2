{
Пакет             Simple Game Engine 2
Файл              sgeGUIProperty.pas
Версия            1.2
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

    FLockUpdate: Boolean;

    procedure UpdateParent;
  public
    constructor Create(AOwner: TObject); virtual;

    procedure LockUpdate;
    procedure UnlockUpdate;
  end;


implementation

uses
  sgeGUIElement;

type
  TsgeGUIElementExtended = class(TsgeGUIElement);


procedure TsgeGUIProperty.UpdateParent;
begin
  //Проверить блокировку обновления
  if FLockUpdate then Exit;

  //Если есть родитель, то обновить
  if FOwner <> nil then
    TsgeGUIElementExtended(FOwner).Notify([esCorrectSize]);
end;


procedure TsgeGUIProperty.LockUpdate;
begin
  FLockUpdate := True;
end;


procedure TsgeGUIProperty.UnlockUpdate;
begin
  FLockUpdate := False;;
end;


constructor TsgeGUIProperty.Create(AOwner: TObject);
begin
  FOwner := AOwner;

  FLockUpdate := False;
end;



end.

