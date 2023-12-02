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
  sgeGraphicOpenGLLayer, sgeGraphicOpenGLDrawObject;


type
  TsgeGraphicOpenGLLayerList = class(specialize TsgeTemplateCollection<TsgeGraphicOpenGLLayer>)
  private
    procedure Sort;

  public
    function  IndexOf(Name: String): Integer;

    procedure Add(Layer: TsgeGraphicOpenGLLayer);
    procedure Delete(Name: String);

    //Найти отображаемый объект на нужном слое и удалить, освободив память
    procedure DeleteDrawObject(DrawObject: TsgeGraphicOpenGLDrawObject);
  end;


implementation

uses
  sgeErrors;

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


procedure TsgeGraphicOpenGLLayerList.DeleteDrawObject(DrawObject: TsgeGraphicOpenGLDrawObject);
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FList[i].Items.Delete(DrawObject, True);
end;



end.

