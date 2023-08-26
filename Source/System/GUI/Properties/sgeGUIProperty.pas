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

    procedure UpdateParent;
  public
    constructor Create(AOwner: TObject); virtual;
  end;


implementation

uses
  sgeGUIElement;

type
  TsgeGUIElementExt = class(TsgeGUIElement);


procedure TsgeGUIProperty.UpdateParent;
begin
  TsgeGUIElementExt(FOwner).PropertyChanged;
end;


constructor TsgeGUIProperty.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;



end.

