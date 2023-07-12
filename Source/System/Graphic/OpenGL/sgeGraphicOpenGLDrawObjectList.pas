{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectList.pas
Версия            1.4
Создан            14.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Список элементов отисовки
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateList,
  sgeGraphicOpenGLDrawObject;


type
  TsgeGraphicOpenGLDrawObjectList = class(specialize TsgeTemplateList<TsgeGraphicOpenGLDrawObject>)
  private
    FIterator: PListItem;

  public
    function  IndexOf(aItem: TsgeGraphicOpenGLDrawObject): PListItem; //Найти элемент по Data
    //procedure MoveToEnd(Element: TsgeGraphicElementBase);             //Переместить элемент в конец списка

    procedure Delete(DrawObject: TsgeGraphicOpenGLDrawObject; FreeObject: Boolean); //Удалить объект из списка если есть

    function  GetFirst: TsgeGraphicOpenGLDrawObject;                  //Взять первый элемент
    function  GetNext: TsgeGraphicOpenGLDrawObject;                   //Следующий элемент
    //procedure DeleteCurrentElement;                                   //Удалить текущий элемент
  end;


implementation


function TsgeGraphicOpenGLDrawObjectList.IndexOf(aItem: TsgeGraphicOpenGLDrawObject): PListItem;
var
  P: PListItem;
begin
  Result := nil;

  //Поиск по полезной нагрузке
  P := FFirst;
  while P <> nil do
  begin
    if P^.Item = aItem then
      Exit(P);
    P := P^.Next;
  end;
end;


procedure TsgeGraphicOpenGLDrawObjectList.Delete(DrawObject: TsgeGraphicOpenGLDrawObject; FreeObject: Boolean);
var
  AItem: PListItem;
begin
  AItem := IndexOf(DrawObject);
  if AItem <> nil then
    DeleteItemByPointer(AItem, FreeObject);
end;


{procedure TsgeGraphicOpenGLDrawObjectList.MoveToEnd(Element: TsgeGraphicElementBase);
var
  PItem: PListItem;
  Data: TsgeGraphicElementBase;
begin
  //Найти указатель на запись
  PItem := IndexOf(Element);
  if pItem <> nil then
  begin
    //Ссылка на данные
    Data := PItem^.Item;

    //Поправить ссылки списка
    if PItem^.Next <> nil then
      PItem^.Next^.Prev := PItem^.Prev
    else
      FLast := PItem^.Prev;

    if PItem^.Prev <> nil then
      PItem^.Prev^.Next := PItem^.Next
    else
      FFirst := PItem^.Next;

    //Удалить память текущей записи
    Dispose(PItem);

    //Уменьшить счётчик
    Dec(FCount);

    //Добавить элемент в хвост
    Add(Data);
  end;
end;}


function TsgeGraphicOpenGLDrawObjectList.GetFirst: TsgeGraphicOpenGLDrawObject;
begin
  //Результат по умолчанию
  Result := nil;

  //Запомнить первый элемент
  FIterator := FFirst;

  //Проверить на пустое значение
  if FIterator <> nil then
    Result := FIterator^.Item;
end;


function TsgeGraphicOpenGLDrawObjectList.GetNext: TsgeGraphicOpenGLDrawObject;
begin
  //Результат по умолчанию
  Result := nil;

  //Проверить итератор на пустой указатель
  if FIterator = nil then
    Exit;

  //Сместить на следующий элемент
  FIterator := FIterator^.Next;

  //Проверить на пустое значение
  if FIterator <> nil then
    Result := FIterator^.Item;
end;


{procedure TsgeGraphicOpenGLDrawObjectList.DeleteCurrentElement;
var
  P: PListItem;
begin
  //Запомнить адрес текущего элемента
  P := FIterator;

  //Проверка на пустое значение
  if FIterator = nil then
    Exit;

  //Сместить указатель на следующий элемент
  FIterator := FIterator^.Next;

  //Удалить текущий элемент
  DeleteItemByPointer(P, FFreeObjects);
end;}


end.

