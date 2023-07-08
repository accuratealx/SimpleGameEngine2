{
Пакет             Simple Game Engine 2
Файл              sgeDisplayLayer.pas
Версия            1.0
Создан            07.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Слой для элементов отображения
}
{$Include Defines.inc}

unit sgeDisplayLayer;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeEventManager;

type
  //Набор измененных параметров
  TsgeDisplayLayerChangeSet = set of (
    dlcsName,     //Имя слоя
    dlcsIndex,    //Приоритет вывода
    dlcsVisible,  //Видимость
    dlcsOffset,   //Смещение от левого-верхнего угла экрана
    dlcsScale     //Масштаб
  );


  //Настройки отображения
  TsgeDisplayLayerData = record
    Name: String;           //Имя слоя
    Index: Word;            //Приоритет вывода (Чем меньше число, там раньше вывод)
    Visible: Boolean;       //Видимость
    Offset: TsgeFloatPoint; //Смещение относительно левого-верхнего угла экрана
    Scale: TsgeFloatPoint;  //Масштаб
  end;


  TsgeDisplayLayer = class
  private
    FEventManager: TsgeEventManager;        //Ссылка на менеджер событий

    FID: Integer;                           //Уникальный номер элемента
    FData: TsgeDisplayLayerData;
    FChangeSet: TsgeDisplayLayerChangeSet;

    procedure SetName(AName: String);
    procedure SetIndex(AIndex: Word);
    procedure SetVisible(AVisible: Boolean);
    procedure SetOffset(AOffset: TsgeFloatPoint);
    procedure SetOffsetX(AOffsetX: Single);
    procedure SetOffsetY(AOffsetY: Single);
    procedure SetScale(AScale: TsgeFloatPoint);
    procedure SetScaleX(AScaleX: Single);
    procedure SetScaleY(AScaleY: Single);
  protected
    procedure ResetChangeSet;

  public
    constructor Create;
    constructor Create(Name: String; Index: Word; Visible: Boolean = True);

    function GetCopy: TsgeDisplayLayer;

    procedure Add;
    procedure Modify;
    procedure Delete;

    property Data: TsgeDisplayLayerData read FData;
    property ChangeSet: TsgeDisplayLayerChangeSet read FChangeSet;

    property ID: Integer read FID;
    property Name: String read FData.Name write SetName;
    property Index: Word read FData.Index write SetIndex;
    property Visible: Boolean read FData.Visible write SetVisible;
    property Offset: TsgeFloatPoint read FData.Offset write SetOffset;
    property OffsetX: Single read FData.Offset.X write SetOffsetX;
    property OffsetY: Single read FData.Offset.Y write SetOffsetY;
    property Scale: TsgeFloatPoint read FData.Scale write SetScale;
    property ScaleX: Single read FData.Scale.X write SetScaleX;
    property ScaleY: Single read FData.Scale.Y write SetScaleY;
  end;


implementation

uses
  sgeUniqueID, sgeCorePointerUtils,
  sgeEventGraphic;


procedure TsgeDisplayLayer.SetName(AName: String);
begin
  if FData.Name = AName then
    Exit;

  FData.Name := AName;
  Include(FChangeSet, dlcsName);
end;


procedure TsgeDisplayLayer.SetIndex(AIndex: Word);
begin
  if FData.Index = AIndex then
    Exit;

  FData.Index := AIndex;
  Include(FChangeSet, dlcsIndex);
end;


procedure TsgeDisplayLayer.SetVisible(AVisible: Boolean);
begin
  if FData.Visible = AVisible then
    Exit;

  FData.Visible := AVisible;
  Include(FChangeSet, dlcsVisible);
end;


procedure TsgeDisplayLayer.SetOffset(AOffset: TsgeFloatPoint);
begin
  FData.Offset := AOffset;
  Include(FChangeSet, dlcsOffset);
end;


procedure TsgeDisplayLayer.SetOffsetX(AOffsetX: Single);
begin
  FData.Offset.X := AOffsetX;
  Include(FChangeSet, dlcsOffset);
end;


procedure TsgeDisplayLayer.SetOffsetY(AOffsetY: Single);
begin
  FData.Offset.Y := AOffsetY;
  Include(FChangeSet, dlcsOffset);
end;


procedure TsgeDisplayLayer.SetScale(AScale: TsgeFloatPoint);
begin
  FData.Scale := AScale;
  Include(FChangeSet, dlcsScale);
end;


procedure TsgeDisplayLayer.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, dlcsScale);
end;


procedure TsgeDisplayLayer.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, dlcsScale);
end;


procedure TsgeDisplayLayer.ResetChangeSet;
begin
  FChangeSet := [];
end;


constructor TsgeDisplayLayer.Create;
begin
  //Заглушка
end;


constructor TsgeDisplayLayer.Create(Name: String; Index: Word; Visible: Boolean);
begin
  FChangeSet := [dlcsName, dlcsIndex, dlcsVisible, dlcsOffset, dlcsScale];

  //Задать параметры
  FData.Name := Name;
  FData.Index := Index;
  FData.Visible := Visible;
  FData.Offset := sgeGetFloatPoint(0, 0);
  FData.Scale := sgeGetFloatPoint(1, 1);

  //Получить уникальный номер
  FID := UniqueID.GetID;

  //Получить ссылку на менеджер событий
  FEventManager := sgeCorePointer_GetEventManager;
end;


function TsgeDisplayLayer.GetCopy: TsgeDisplayLayer;
begin
  Result := TsgeDisplayLayer.Create;

  //Заполнить данные
  Result.FData := Self.FData;
  Result.FChangeSet := Self.FChangeSet;
end;


procedure TsgeDisplayLayer.Add;
var
  Event: TsgeEventGraphicLayer;
begin
  //Опубликовать событие
  Event := TsgeEventGraphicLayer.Create(Event_Graphic_LayerAdd, FID, Self.GetCopy);
  FEventManager.Publish(Event);

  //Сбросить
  ResetChangeSet;
end;


procedure TsgeDisplayLayer.Modify;
var
  Event: TsgeEventGraphicLayer;
begin
  //Опубликовать событие
  Event := TsgeEventGraphicLayer.Create(Event_Graphic_LayerModify, FID, Self.GetCopy);
  FEventManager.Publish(Event);

  //Сбросить
  ResetChangeSet;
end;


procedure TsgeDisplayLayer.Delete;
var
  Event: TsgeEventGraphicLayer;
begin
  //Опубликовать событие
  Event := TsgeEventGraphicLayer.Create(Event_Graphic_LayerDelete, FID);
  FEventManager.Publish(Event);

  //Сбросить
  ResetChangeSet;
end;



end.

