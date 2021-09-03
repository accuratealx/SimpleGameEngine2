{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementLayerList.pas
Версия            1.2
Создан            14.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список слоёв элементов отисовки
}
{$Include Defines.inc}

unit sgeGraphicElementLayerList;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeGraphicElementLayer, sgeTemplateObjectCollection, sgeGraphicElementBase;


type
  TsgeGraphicElementLayerTemplate = specialize TsgeTemplateObjectCollection<TsgeGraphicElementLayer>;


  //Список слоёв
  TsgeGraphicElementLayerList = class(TsgeGraphicElementLayerTemplate)
  private
    procedure Sort;
    function  LayerIndexOf(Index: Word): Integer;                                         //Найти индекс слоя по номеру

    function  GetLayerVisible(Index: Integer): Boolean;
    procedure SetLayerVisible(Index: Integer; AVisible: Boolean);
  public
    function  IndexOf(Name: String): Integer;                                             //Найти индекс слоя по имени

    procedure AddLayer(Item: TsgeGraphicElementLayer);                                    //Добавить слой
    procedure Delete(Index: Integer);
    procedure Delete(Name: String);                                                       //Удалить слой

    procedure AddElement(DrawElement: TsgeGraphicElementBase; LayerName: string = '');    //Добавить элемент

    property Visible[Index: Integer]: Boolean read GetLayerVisible write SetLayerVisible;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'GraphicElementLayerList';

  Err_LayerNotFound = 'LayerNotFound';


procedure TsgeGraphicElementLayerList.Sort;
var
  i, j, ci, cj: Integer;
  El: TsgeGraphicElementLayer;
begin
  ci := Fcount - 1;
  cj := ci - 1;
  for i := 0 to ci do
    for j := 0 to cj - i do
      if FList[j].Index > FList[j + 1].Index then
        begin
        El := FList[j];
        FList[j] := FList[j + 1];
        FList[j + 1] := El;
        end;
end;


function TsgeGraphicElementLayerList.LayerIndexOf(Index: Word): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FCount - 1 do
    if FList[i].Index = Index then
      begin
      Result := i;
      Break;
      end;
end;


function TsgeGraphicElementLayerList.GetLayerVisible(Index: Integer): Boolean;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FList[Index].Visible;
end;


procedure TsgeGraphicElementLayerList.SetLayerVisible(Index: Integer; AVisible: Boolean);
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  FList[Index].Visible := AVisible;
end;


function TsgeGraphicElementLayerList.IndexOf(Name: String): Integer;
var
  i: Integer;
begin
  Result := -1;

  Name := LowerCase(Name);
  for i := 0 to FCount - 1 do
    if LowerCase(FList[i].Name) = Name then
      begin
      Result := i;
      Break;
      end;
end;


procedure TsgeGraphicElementLayerList.AddLayer(Item: TsgeGraphicElementLayer);
begin
  //Добавить слой
  inherited Add(Item);

  //Упорядочить
  Sort;
end;

procedure TsgeGraphicElementLayerList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;


procedure TsgeGraphicElementLayerList.Delete(Name: String);
var
  Idx: Integer;
begin
  //Найти индекс слоя
  Idx := IndexOf(Name);

  //Слой не найден
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_LayerNotFound, Name);

  //Удалить
  Delete(Idx);
end;


procedure TsgeGraphicElementLayerList.AddElement(DrawElement: TsgeGraphicElementBase; LayerName: string);
var
  Idx: Integer;
begin
  //Если нет слоя, то добавить в слой по умолчанию
  Idx := IndexOf(LayerName);
  if Idx = -1 then
    begin
    Idx := IndexOf('');
    if Idx = -1 then
      begin
      AddLayer(TsgeGraphicElementLayer.Create('', 0));
      Idx := IndexOf('');
      end;
    end;

  //Добавить элемент в слой
  FList[Idx].Elements.Add(DrawElement);
end;



end.

