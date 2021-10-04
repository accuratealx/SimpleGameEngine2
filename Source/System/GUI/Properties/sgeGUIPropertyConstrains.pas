{
Пакет             Simple Game Engine 2
Файл              sgeGUIElementConstrains.pas
Версия            1.1
Создан            19.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Ограничение размеров базового элемента
}
{$Include Defines.inc}

unit sgeGUIPropertyConstrains;

{$mode objfpc}{$H+}

interface

uses
  sgeGUIProperty;


type
  TsgeGUIPropertyConstrains = class(TsgeGUIProperty)
  private
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FMinHeight: Integer;
    FMaxHeight: Integer;

    procedure SetMinHeight(AMinHeight: Integer);
    procedure SetMinWidth(AMinWidth: Integer);
    procedure SetMaxHeight(AMaxHeight: Integer);
    procedure SetMaxWidth(AMaxWidth: Integer);
  public
    property MinWidth: Integer read FMinWidth write SetMinWidth;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth;
    property MinHeight: Integer read FMinHeight write SetMinHeight;
    property MaxHeight: Integer read FMaxHeight write SetMaxHeight;
  end;


  TsgeGUIPropertyConstrainsExt = class(TsgeGUIPropertyConstrains)
  public
    procedure Check(var NewWidth, NewHeight: Integer);
  end;


implementation



procedure TsgeGUIPropertyConstrains.SetMinHeight(AMinHeight: Integer);
begin
  if AMinHeight < 0 then AMinHeight := 0;
  if FMinHeight = AMinHeight then Exit;

  //Изменить значение
  FMinHeight := AMinHeight;
  if FMinHeight > FMaxHeight then FMaxHeight := FMinHeight;

  //Внести изменения
  UpdateParent;
end;


procedure TsgeGUIPropertyConstrains.SetMinWidth(AMinWidth: Integer);
begin
  if AMinWidth < 0 then AMinWidth := 0;
  if FMinWidth = AMinWidth then Exit;

  //Изменить значение
  FMinWidth := AMinWidth;
  if FMinWidth > FMaxWidth then FMaxWidth := FMinWidth;

  //Внести изменения
  UpdateParent;
end;


procedure TsgeGUIPropertyConstrains.SetMaxHeight(AMaxHeight: Integer);
begin
  if AMaxHeight < 0 then AMaxHeight := 0;
  if FMaxHeight = AMaxHeight then Exit;

  //Изменить значение
  FMaxHeight := AMaxHeight;
  if FMaxHeight < FMinHeight then FMinHeight := FMaxHeight;

  //Внести изменения
  UpdateParent;
end;


procedure TsgeGUIPropertyConstrains.SetMaxWidth(AMaxWidth: Integer);
begin
  if AMaxWidth < 0 then AMaxWidth := 0;
  if FMaxWidth = AMaxWidth then Exit;

  //Изменить значение
  FMaxWidth := AMaxWidth;
  if FMaxWidth < FMinWidth then FMinWidth := FMaxWidth;

  //Внести изменения
  UpdateParent;
end;




procedure TsgeGUIPropertyConstrainsExt.Check(var NewWidth, NewHeight: Integer);
begin
  if (FMinWidth <> 0) and (NewWidth < FMinWidth) then NewWidth := FMinWidth;
  if (FMaxWidth <> 0) and (NewWidth > FMaxWidth) then NewWidth := FMaxWidth;
  if (FMinHeight <> 0) and (NewHeight < FMinHeight) then NewHeight := FMinHeight;
  if (FMaxHeight <> 0) and (NewHeight > FMaxHeight) then NewHeight := FMaxHeight;
end;


end.

