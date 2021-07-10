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
  sgeCriticalSection, sgeGraphicElementLayerList, sgeGraphicElementBase;


type
  TsgeGraphicDrawList = class
  private
    FCS: TsgeCriticalSection;
    FList: TsgeGraphicElementLayerList;

    function  GetLayerCount: Integer;
    function  GetLayer(Index: Integer): TsgeGraphicElementLayerItem;
    function  GetLayerVisible(Index: Integer): Boolean;
    procedure SetLayerVisible(Index: Integer; AVisible: Boolean);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Lock;
    procedure UnLock;

    procedure ClearLayers;
    procedure AddLayer(Name: String; Index: Word);
    procedure DeleteLayer(Name: String);

    function  AddElement(Element: TsgeGraphicElementBase; LayerName: String = ''): TsgeGraphicElementBase;

    property LayerCount: Integer read GetLayerCount;
    property Layer[Index: Integer]: TsgeGraphicElementLayerItem read GetLayer;
    property LayerVisible[Index: Integer]: Boolean read GetLayerVisible write SetLayerVisible;
  end;




implementation

uses
  sgeErrors, sgeSystemUtils;


const
  _UNITNAME = 'GraphicDrawList';

  Err_IndexOutOfBounds = 'IndexOutOfBounds';



function TsgeGraphicDrawList.GetLayerCount: Integer;
begin
  FCS.Enter;
  Result := FList.Count;
  FCS.Leave;
end;


function TsgeGraphicDrawList.GetLayer(Index: Integer): TsgeGraphicElementLayerItem;
begin
  FCS.Enter;
  try

    if (Index < 0) or (Index > FList.Count - 1) then
      raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

    Result := FList.Item[Index];

  finally
    FCS.Leave;
  end;
end;


function TsgeGraphicDrawList.GetLayerVisible(Index: Integer): Boolean;
begin
  FCS.Enter;
  try

    Result := FList.Visible[Index];

  finally
    FCS.Leave;
  end;
end;


procedure TsgeGraphicDrawList.SetLayerVisible(Index: Integer; AVisible: Boolean);
begin
  FCS.Enter;
  try

    FList.Visible[Index] := AVisible;

  finally
    FCS.Leave;
  end;
end;


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

  FList.ClearItem;

  FCS.Leave;
end;

procedure TsgeGraphicDrawList.AddLayer(Name: String; Index: Word);
begin
  FCS.Enter;
  try

    FList.AddLayer(Name, Index);

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


function TsgeGraphicDrawList.AddElement(Element: TsgeGraphicElementBase; LayerName: String): TsgeGraphicElementBase;
begin
  FCS.Enter;
  try

    Result := Element;
    FList.AddElement(Element, LayerName);

  finally
    FCS.Leave;
  end;
end;



end.


