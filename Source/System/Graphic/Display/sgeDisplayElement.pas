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
  sgeEventManager;

type
  TsgeDisplayElement = class
  protected
    FEventManager: TsgeEventManager;  //Ссылка на менеджер событий

    FID: Integer;                     //Уникальный номер элемента
    FVisible: Boolean;                //Видимость элемента

    procedure SetVisible(AVisible: Boolean);

    procedure ResetChangeSet; virtual; abstract;        //Сброс флагов изменения
    function  IsNeedUpdate: Boolean; virtual; abstract; //Проверить необходимость обновления

  public
    constructor Create;

    function  GetCopy: TsgeDisplayElement; virtual; abstract; //Копирование объекта

    //События
    procedure Add(LayerName: String); //Добавление нового объекта на слой
    procedure ChangeVisibility;       //Изменение видимости
    procedure Update;                 //Изменение объекта
    procedure Delete;                 //Удаление объекта

    property ID: Integer read FID;
    property Visible: Boolean read FVisible write SetVisible;
  end;


implementation

uses
  sgeUniqueID, sgeCorePointerUtils,
  sgeEventGraphic;


procedure TsgeDisplayElement.SetVisible(AVisible: Boolean);
begin
  if FVisible = AVisible then
    Exit;

  FVisible := AVisible;

  //Послать событие изменения видимости
  ChangeVisibility;
end;


constructor TsgeDisplayElement.Create;
begin
  //Сгенерировать уникальный ID
  FID := UniqueID.GetID;

  //Получить ссылку на менеджер событий
  FEventManager := sgeCorePointer_GetEventManager;

  //Задать параметры
  FVisible := False;
end;


procedure TsgeDisplayElement.Add(LayerName: String);
var
  Event: TsgeEventGraphicElementAdd;
begin
  Event := TsgeEventGraphicElementAdd.Create(FID, Self.GetCopy, LayerName);
  FEventManager.Publish(Event);

  ResetChangeSet;
end;


procedure TsgeDisplayElement.ChangeVisibility;
var
  Event: TsgeEventGraphicElementVisible;
begin
  Event := TsgeEventGraphicElementVisible.Create(FID, FVisible);
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
