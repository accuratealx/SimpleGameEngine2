{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLLayerList.pas
Версия            1.5
Создан            14.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список слоёв элементов отисовки
}
{$Include Defines.inc}

unit sgeGraphicOpenGLLayerList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection,
  sgeGraphicOpenGLLayer;


type
  TsgeGraphicOpenGLLayerList = class(specialize TsgeTemplateCollection<TsgeGraphicOpenGLLayer>)
  private
    procedure Sort;

    function GetNamedItem(AName: String): TsgeGraphicOpenGLLayer;
  public
    function  IndexOf(Name: String): Integer;

    procedure Add(Layer: TsgeGraphicOpenGLLayer);
    procedure Delete(Name: String);

    property NamedItem[Name: String]: TsgeGraphicOpenGLLayer read GetNamedItem;

    //procedure AddElement(DrawElement: TsgeGraphicElementBase; LayerName: string = '');  //Добавить элемент
    //procedure MoveElementToListEnd(DrawElement: TsgeGraphicElementBase; LayerName: string = '');  //Переместить элемент в конец списка
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'GraphicElementLayerList';

  Err_LayerNotFound = 'LayerNotFound';
  Err_LayerExist = 'LayerExist';


procedure TsgeGraphicOpenGLLayerList.Sort;
var
  i, j, ci, cj: Integer;
  El: TsgeGraphicOpenGLLayer;
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


function TsgeGraphicOpenGLLayerList.GetNamedItem(AName: String): TsgeGraphicOpenGLLayer;
var
  Idx: Integer;
begin
  Idx := IndexOf(AName);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_LayerNotFound);

  Result := FList[Idx];
end;


function TsgeGraphicOpenGLLayerList.IndexOf(Name: String): Integer;
var
  i: Integer;
begin
  Result := -1;

  Name := LowerCase(Name);
  for i := 0 to FCount - 1 do
    if LowerCase(FList[i].Name) = Name then
      Exit(i);
end;


procedure TsgeGraphicOpenGLLayerList.Add(Layer: TsgeGraphicOpenGLLayer);
begin
  //Проверить слой на существование
  if IndexOf(Layer.Name) <> - 1 then
    raise EsgeException.Create(_UNITNAME, Err_LayerExist, Layer.Name);

  //Добавить в список
  inherited Add(Layer);

  //Упорядочить
  Sort;
end;


procedure TsgeGraphicOpenGLLayerList.Delete(Name: String);
var
  Idx: Integer;
begin
  //Найти индекс слоя
  Idx := IndexOf(Name);

  //Слой не найден
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_LayerNotFound, Name);

  //Удалить
  inherited Delete(Idx);
end;


{procedure TsgeGraphicOpenGLLayerList.AddElement(DrawElement: TsgeGraphicElementBase; LayerName: string);
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
    //FList[Idx].Elements.Add(DrawElement);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeGraphicOpenGLLayerList.MoveElementToListEnd(DrawElement: TsgeGraphicElementBase; LayerName: string);
var
  Idx: Integer;
begin
  FCS.Enter;
  try
    //Найти слой
    Idx := IndexOf(LayerName);

    //Подвинуть элемент в конец списка
    //if Idx <> -1 then
    //  FList[Idx].Elements.MoveToEnd(DrawElement);
  finally
    FCS.Leave;
  end;
end;}



end.

