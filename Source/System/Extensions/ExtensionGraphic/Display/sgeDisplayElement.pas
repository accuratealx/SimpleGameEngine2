{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElement.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент отображения: Базовый
}
{$Include Defines.inc}

unit sgeDisplayElement;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeEventManager;

type
  //Базовый елемент вывода
  TsgeDisplayElement = class
  protected
    FEventManager: TsgeEventManager;  //Ссылка на менеджер событий

    FID: Integer;                     //Уникальный номер элемента
    FVisible: Boolean;                //Видимость элемента
    FClipped: Boolean;                //Флаг ограничения вывода
    FClipRect: TsgeClipRect;          //Ораничивающий прямоугольник

    //Свойства
    procedure SetVisible(AVisible: Boolean);
    procedure SetClipped(AClipped: Boolean);
    procedure SetClipRect(AClipRect: TsgeClipRect);

    //Вспомогательные методы
    procedure SendClipRectEvent;                        //Послать событие изменения ограничения вывода
    procedure ResetChangeSet; virtual; abstract;        //Сброс флагов изменения
    function  IsNeedUpdate: Boolean; virtual; abstract; //Проверить необходимость обновления

  public
    constructor Create;

    function  GetCopy: TsgeDisplayElement; virtual; abstract; //Копирование объекта

    //События
    procedure Add(LayerName: String); //Добавление нового объекта на слой
    procedure Update;                 //Изменение объекта
    procedure Delete;                 //Удаление объекта

    //Свойства
    property ID: Integer read FID;
    property Visible: Boolean read FVisible write SetVisible;
    property Clipped: Boolean read FClipped write SetClipped;
    property ClipRect: TsgeClipRect read FClipRect write SetClipRect;
  end;


implementation

uses
  sgeUniqueID, sgeCorePointerUtils,
  sgeEventGraphic;


procedure TsgeDisplayElement.SetVisible(AVisible: Boolean);
var
  Event: TsgeEventGraphicElementVisible;
begin
  if FVisible = AVisible then
    Exit;

  FVisible := AVisible;

  //Послать событие изменения видимости
  Event := TsgeEventGraphicElementVisible.Create(FID, FVisible);
  FEventManager.Publish(Event);
end;


procedure TsgeDisplayElement.SetClipped(AClipped: Boolean);
begin
  if FClipped = AClipped then
    Exit;

  FClipped := AClipped;

  //Послать событие изменения границы вывода
  SendClipRectEvent;
end;


procedure TsgeDisplayElement.SetClipRect(AClipRect: TsgeClipRect);
begin
  if FClipRect = AClipRect then
    Exit;

  FClipRect := AClipRect;

  //Послать событие изменения границы вывода если включено обрезние
  if FClipped then
    SendClipRectEvent;
end;


procedure TsgeDisplayElement.SendClipRectEvent;
var
  Event: TsgeEventGraphicElementClipRect;
begin
  Event := TsgeEventGraphicElementClipRect.Create(FID, FClipped, FClipRect);
  FEventManager.Publish(Event);
end;


constructor TsgeDisplayElement.Create;
begin
  //Сгенерировать уникальный ID
  FID := UniqueID.GetID;

  //Получить ссылку на менеджер событий
  FEventManager := sgeCorePointer_GetEventManager;

  //Задать параметры
  FVisible := False;
  FClipped := False;
end;


procedure TsgeDisplayElement.Add(LayerName: String);
var
  Event: TsgeEventGraphicElementAdd;
begin
  Event := TsgeEventGraphicElementAdd.Create(FID, Self.GetCopy, LayerName);
  FEventManager.Publish(Event);

  ResetChangeSet;
end;


procedure TsgeDisplayElement.Update;
var
  Event: TsgeEventGraphicElementUpdate;
begin
  //Не посылать событие, если нечего менять
  if not IsNeedUpdate then
    Exit;

  Event := TsgeEventGraphicElementUpdate.Create(FID, Self.GetCopy);
  FEventManager.Publish(Event);

  ResetChangeSet;
end;


procedure TsgeDisplayElement.Delete;
var
  Event: TsgeEventGraphicElementDelete;
begin
  Event := TsgeEventGraphicElementDelete.Create(FID);
  FEventManager.Publish(Event);

  ResetChangeSet;
end;



end.
