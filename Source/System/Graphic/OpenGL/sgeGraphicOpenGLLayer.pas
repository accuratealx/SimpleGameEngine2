{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLLayer.pas
Версия            1.2
Создан            14.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Слой
}
{$Include Defines.inc}

unit sgeGraphicOpenGLLayer;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayLayer;


type
  TsgeGraphicOpenGLLayer = class
  private
    FData: TsgeDisplayLayerData;

  public
    constructor Create(Layer: TsgeDisplayLayer);
    destructor  Destroy; override;

    procedure Update(Layer: TsgeDisplayLayer);

    property Name: String read FData.Name;
    property Index: Word read FData.Index;
    property Visible: Boolean read FData.Visible;
    property Offset: TsgeFloatPoint read FData.Offset;
    property Scale: TsgeFloatPoint read FData.Scale;
  end;


implementation


constructor TsgeGraphicOpenGLLayer.Create(Layer: TsgeDisplayLayer);
begin
  Update(Layer);
end;


destructor TsgeGraphicOpenGLLayer.Destroy;
begin

end;


procedure TsgeGraphicOpenGLLayer.Update(Layer: TsgeDisplayLayer);
begin
  //Видимость
  if dlcsName in Layer.ChangeSet then
    FData.Name := Layer.Data.Name;

  //Видимость
  if dlcsVisible in Layer.ChangeSet then
    FData.Visible := Layer.Data.Visible;

  //Приоритет
  if dlcsIndex in Layer.ChangeSet then
  begin
    FData.Index := Layer.Data.Index;
    //Отсортировать слои как нибудь
  end;

  //Смещение
  if dlcsOffset in Layer.ChangeSet then
    FData.Offset := Layer.Data.Offset;

  //Масштаб
  if dlcsScale in Layer.ChangeSet then
    FData.Scale := Layer.Data.Scale;
end;



end.

