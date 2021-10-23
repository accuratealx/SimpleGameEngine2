{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementLayerList.pas
Версия            1.4
Создан            14.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список слоёв элементов отисовки
}
{$Include Defines.inc}

unit sgeGraphicElementLayerList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateThreadSafeCollection,
  sgeGraphicElementLayer, sgeGraphicElementBase;


type
  TsgeGraphicElementLayerList = class(specialize TsgeTemplateThreadSafeCollection<TsgeGraphicElementLayer>)
  private
    procedure Sort;

    function  GetLayerVisible(Index: Integer): Boolean;
    procedure SetLayerVisible(Index: Integer; AVisible: Boolean);
  public
    procedure Lock;
    procedure UnLock;

    function  IndexOf(Name: String): Integer;                                             //Найти индекс слоя по имени

    function  Add(Name: String; Index: Word; Visible: Boolean = True): TsgeGraphicElementLayer;
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


function TsgeGraphicElementLayerList.GetLayerVisible(Index: Integer): Boolean;
begin
  FCS.Enter;
  try

    if (Index < 0) or (Index > FCount - 1) then
      raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

    Result := FList[Index].Visible;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeGraphicElementLayerList.SetLayerVisible(Index: Integer; AVisible: Boolean);
begin
  FCS.Enter;
  try

    if (Index < 0) or (Index > FCount - 1) then
      raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

    FList[Index].Visible := AVisible;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeGraphicElementLayerList.Lock;
begin
  FCS.Enter;
end;


procedure TsgeGraphicElementLayerList.UnLock;
begin
  FCS.Leave;
end;


function TsgeGraphicElementLayerList.IndexOf(Name: String): Integer;
var
  i: Integer;
begin
  FCS.Enter;
  try

    Result := -1;

    Name := LowerCase(Name);
    for i := 0 to FCount - 1 do
      if LowerCase(FList[i].Name) = Name then
        begin
        Result := i;
        Break;
        end;

  finally
    FCS.Leave;
  end;
end;


function TsgeGraphicElementLayerList.Add(Name: String; Index: Word; Visible: Boolean): TsgeGraphicElementLayer;
begin
  FCS.Enter;
  try

    //Проверить слой на существование
    if IndexOf(Name) <> - 1 then Exit;

    //Создать слой
    Result := TsgeGraphicElementLayer.Create(Name, Index, Visible);

    //Добавить в список
    inherited Add(Result);

    //Упорядочить
    Sort;

  finally
    FCS.Leave;
  end;
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
  FCS.Enter;
  try

    //Если нет слоя, то добавить в слой по умолчанию
    Idx := IndexOf(LayerName);
    if Idx = -1 then
      begin
      Idx := IndexOf('');
      if Idx = -1 then
        begin
        Add('', 0, True);
        Idx := IndexOf('');
        end;
      end;

    //Добавить элемент в слой
    FList[Idx].Elements.Add(DrawElement);

  finally
    FCS.Leave;
  end;
end;



end.

