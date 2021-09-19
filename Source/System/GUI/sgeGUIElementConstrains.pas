{
Пакет             Simple Game Engine 2
Файл              sgeGUIElementConstrains.pas
Версия            1.0
Создан            19.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Ограничение размеров базового элемента
}
{$Include Defines.inc}

unit sgeGUIElementConstrains;

{$mode objfpc}{$H+}

interface


type
  TsgeGUIElementConstrains = class
  private
    FOwner: TObject;                                                //Ссылка на владельца

    FMinWidth: Integer;
    FMaxWidth: Integer;
    FMinHeight: Integer;
    FMaxHeight: Integer;

    procedure UpdateParent;

    procedure SetMinHeight(AMinHeight: Integer);
    procedure SetMinWidth(AMinWidth: Integer);
    procedure SetMaxHeight(AMaxHeight: Integer);
    procedure SetMaxWidth(AMaxWidth: Integer);
  public
    constructor Create(AOwner: TObject);

    procedure Check(var NewWidth, NewHeight: Integer);              //Проверка размеров

    property MinWidth: Integer read FMinWidth write SetMinWidth;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth;
    property MinHeight: Integer read FMinHeight write SetMinHeight;
    property MaxHeight: Integer read FMaxHeight write SetMaxHeight;
  end;


implementation

uses
  sgeGUIElement;

type
  TsgeGUIElementHack = class(TsgeGUIElement);


procedure TsgeGUIElementConstrains.UpdateParent;
begin
  TsgeGUIElementHack(FOwner).Notify([esCorrectSize]);
end;


procedure TsgeGUIElementConstrains.SetMinHeight(AMinHeight: Integer);
begin
  if AMinHeight < 0 then AMinHeight := 0;
  if FMinHeight = AMinHeight then Exit;

  //Изменить значение
  FMinHeight := AMinHeight;
  if FMinHeight > FMaxHeight then FMaxHeight := FMinHeight;

  //Внести изменения
  UpdateParent;
end;


procedure TsgeGUIElementConstrains.SetMinWidth(AMinWidth: Integer);
begin
  if AMinWidth < 0 then AMinWidth := 0;
  if FMinWidth = AMinWidth then Exit;

  //Изменить значение
  FMinWidth := AMinWidth;
  if FMinWidth > FMaxWidth then FMaxWidth := FMinWidth;

  //Внести изменения
  UpdateParent;
end;


procedure TsgeGUIElementConstrains.SetMaxHeight(AMaxHeight: Integer);
begin
  if AMaxHeight < 0 then AMaxHeight := 0;
  if FMaxHeight = AMaxHeight then Exit;

  //Изменить значение
  FMaxHeight := AMaxHeight;
  if FMaxHeight < FMinHeight then FMinHeight := FMaxHeight;

  //Внести изменения
  UpdateParent;
end;


procedure TsgeGUIElementConstrains.SetMaxWidth(AMaxWidth: Integer);
begin
  if AMaxWidth < 0 then AMaxWidth := 0;
  if FMaxWidth = AMaxWidth then Exit;

  //Изменить значение
  FMaxWidth := AMaxWidth;
  if FMaxWidth < FMinWidth then FMinWidth := FMaxWidth;

  //Внести изменения
  UpdateParent;
end;


constructor TsgeGUIElementConstrains.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;


procedure TsgeGUIElementConstrains.Check(var NewWidth, NewHeight: Integer);
begin
  if (FMinWidth <> 0) and (NewWidth < FMinWidth) then NewWidth := FMinWidth;
  if (FMaxWidth <> 0) and (NewWidth > FMaxWidth) then NewWidth := FMaxWidth;
  if (FMinHeight <> 0) and (NewHeight < FMinHeight) then NewHeight := FMinHeight;
  if (FMaxHeight <> 0) and (NewHeight > FMaxHeight) then NewHeight := FMaxHeight;
end;



end.

