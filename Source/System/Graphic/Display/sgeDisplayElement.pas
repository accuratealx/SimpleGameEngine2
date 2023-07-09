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


type
  TsgeDisplayElement = class
  protected
    FID: Integer;       //Уникальный номер элемента
    FVisible: Boolean;  //Видимость элемента

    procedure SetVisible(AVisible: Boolean);

    procedure ResetChangeSet; virtual; abstract;  //Сброс флагов изменения
    procedure SetUniqueID;                        //Присвоить уникальный номер

  public
    destructor Destroy; override;

    function  GetCopy: TsgeDisplayElement; virtual; abstract; //Копирование объекта

    //События
    procedure Add;              //Добавление нового объекта
    procedure ChangeVisibility; //Изменение видимости
    procedure Update;           //Изменение объекта
    procedure Delete;           //Удаление объекта

    property ID: Integer read FID;
    property Visible: Boolean read FVisible write SetVisible;
  end;


implementation

uses
  sgeUniqueID;


procedure TsgeDisplayElement.SetVisible(AVisible: Boolean);
begin
  if FVisible = AVisible then
    Exit;

  FVisible := AVisible;

  //Послать событие изменения видимости
  ChangeVisibility;
end;


procedure TsgeDisplayElement.SetUniqueID;
begin
  FID := UniqueID.GetID;
end;


destructor TsgeDisplayElement.Destroy;
begin
  //При разрушении почистить объект
  Delete;
end;


procedure TsgeDisplayElement.Add;
begin
  //Новый объект

  ResetChangeSet;
end;


procedure TsgeDisplayElement.ChangeVisibility;
begin
  //Зименение видимости
end;


procedure TsgeDisplayElement.Update;
begin
  //Изменился объект

  ResetChangeSet;
end;


procedure TsgeDisplayElement.Delete;
begin
  //Удалить объект

  ResetChangeSet;
end;



end.

