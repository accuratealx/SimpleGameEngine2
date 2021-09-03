{
Пакет             Simple Game Engine 2
Файл              sgeGraphicDrawList.pas
Версия            1.5
Создан            04.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Графика: Список слоёв отрисовки графических элементов
}
{$Include Defines.inc}

unit sgeGraphicDrawList;

{$mode objfpc}{$H+}

interface

uses
  sgeCriticalSection, sgeGraphicElementLayer, sgeGraphicElementLayerList, sgeGraphicElementBase;


type
  TsgeGraphicDrawList = class
  private
    FCS: TsgeCriticalSection;
    FList: TsgeGraphicElementLayerList;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure Lock;
    procedure UnLock;

    procedure ClearLayers;
    function  AddLayer(Name: String; Index: Word; Visible: Boolean = True): TsgeGraphicElementLayer;
    procedure DeleteLayer(Name: String);

    procedure AddElement(Element: TsgeGraphicElementBase; LayerName: String = '');

    property LayerList: TsgeGraphicElementLayerList read FList;
  end;


implementation


constructor TsgeGraphicDrawList.Create;
begin
  FCS := TsgeCriticalSection.Create;
  FList := TsgeGraphicElementLayerList.Create;
end;


destructor TsgeGraphicDrawList.Destroy;
begin
  FList.Free;
  FCS.Free;
end;


procedure TsgeGraphicDrawList.Lock;
begin
  FCS.Enter;
end;


procedure TsgeGraphicDrawList.UnLock;
begin
  FCS.Leave;
end;


procedure TsgeGraphicDrawList.ClearLayers;
begin
  FCS.Enter;

  FList.Clear;

  FCS.Leave;
end;

function TsgeGraphicDrawList.AddLayer(Name: String; Index: Word; Visible: Boolean): TsgeGraphicElementLayer;
begin
  FCS.Enter;
  try

    //Проверить слой на существование
    if FList.IndexOf(Name) <> - 1 then Exit;

    //Создать слой
    Result := TsgeGraphicElementLayer.Create(Name, Index, Visible);

    //Добавить в список
    FList.AddLayer(Result);

  finally
    FCS.Leave;
  end;
end;

procedure TsgeGraphicDrawList.DeleteLayer(Name: String);
var
  Idx: Integer;
begin
  FCS.Enter;
  try

    //Удалить слой если есть
    Idx := FList.IndexOf(Name);
    if Idx <> -1 then FList.Delete(Idx);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeGraphicDrawList.AddElement(Element: TsgeGraphicElementBase; LayerName: String);
begin
  FCS.Enter;
  try

    FList.AddElement(Element, LayerName);

  finally
    FCS.Leave;
  end;
end;



end.


