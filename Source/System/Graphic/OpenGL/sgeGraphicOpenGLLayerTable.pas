{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLSpriteTable.pas
Версия            1.0
Создан            08.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Хэш-таблица слоев
}
{$Include Defines.inc}

unit sgeGraphicOpenGLLayerTable;

{$mode ObjFPC}{$H+}

interface

uses
  Contnrs,
  sgeGraphicOpenGLLayer;

type
  TsgeGraphicOpenGLLayerTable = class
  private
    FTable: TFPHashObjectList;

  public
    constructor Create;
    destructor  Destroy; override;

    //Удаление элементов с уничтожением
    procedure Clear;

    procedure Add(UniqueID: Integer; Layer: TsgeGraphicOpenGLLayer);
    procedure Delete(UniqueID: Integer);

    function  Get(UniqueID: Integer): TsgeGraphicOpenGLLayer;
  end;


var
  OpenGLLayerTable: TsgeGraphicOpenGLLayerTable;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'GraphicOpenGLLayerTable';

  Err_EmptyLayer = 'EmptyLayer';
  Err_DuplicateLayer = 'DuplicateLayer';
  Err_LayerNotFound = 'LayerNotFound';


constructor TsgeGraphicOpenGLLayerTable.Create;
begin
  FTable := TFPHashObjectList.Create(False);
end;


destructor TsgeGraphicOpenGLLayerTable.Destroy;
begin
  FTable.Free;
end;


procedure TsgeGraphicOpenGLLayerTable.Clear;
begin
  FTable.Clear;
end;


procedure TsgeGraphicOpenGLLayerTable.Add(UniqueID: Integer; Layer: TsgeGraphicOpenGLLayer);
var
  ID: ShortString;
begin
  //Проверить объект
  if Layer = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyLayer);

  ID := sgeIntToStr(UniqueID);

  //Проверить наличие объекта
  if FTable.Find(ID) <> nil then
    raise EsgeException.Create(_UNITNAME, Err_DuplicateLayer, Layer.Name);

  //Добавить
  FTable.Add(ID, Layer);
end;


procedure TsgeGraphicOpenGLLayerTable.Delete(UniqueID: Integer);
var
  ID: ShortString;
  Idx: Integer;
begin
  ID := sgeIntToStr(UniqueID);

  //Удалить из словаря
  Idx := FTable.FindIndexOf(ID);
  if Idx <> -1 then
    FTable.Delete(Idx);
end;


function TsgeGraphicOpenGLLayerTable.Get(UniqueID: Integer): TsgeGraphicOpenGLLayer;
var
  ID: ShortString;
begin
  ID := sgeIntToStr(UniqueID);
  Result := TsgeGraphicOpenGLLayer(FTable.Find(ID));

  if Result = nil then
    raise EsgeException.Create(_UNITNAME, Err_LayerNotFound, ID);
end;



initialization
begin
  OpenGLLayerTable := TsgeGraphicOpenGLLayerTable.Create;
end;


finalization
begin
  OpenGLLayerTable.Free;
end;



end.

