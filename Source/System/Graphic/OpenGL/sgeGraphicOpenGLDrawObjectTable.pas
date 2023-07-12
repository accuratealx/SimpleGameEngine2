{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectTable.pas
Версия            1.0
Создан            11.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Хэш-таблица объектов рисования
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectTable;

{$mode ObjFPC}{$H+}

interface

uses
  Contnrs,
  sgeGraphicOpenGLDrawObject;

type
  TsgeGraphicOpenGLDrawObjectTable = class
  private
    FTable: TFPHashObjectList;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(UniqueID: Integer; DrawObject: TsgeGraphicOpenGLDrawObject);
    procedure Delete(UniqueID: Integer);

    function  Get(UniqueID: Integer): TsgeGraphicOpenGLDrawObject;
  end;


var
  OpenGLDrawObjectTable: TsgeGraphicOpenGLDrawObjectTable;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'GraphicOpenGLLayerTable';

  Err_EmptyLayer = 'EmptyLayer';
  Err_DuplicateObject = 'DuplicateObject';
  Err_ObjectNotFound = 'ObjectNotFound';


constructor TsgeGraphicOpenGLDrawObjectTable.Create;
begin
  FTable := TFPHashObjectList.Create(False);
end;


destructor TsgeGraphicOpenGLDrawObjectTable.Destroy;
begin
  FTable.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectTable.Add(UniqueID: Integer; DrawObject: TsgeGraphicOpenGLDrawObject);
var
  ID: ShortString;
begin
  //Проверить объект
  if DrawObject = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyLayer);

  ID := sgeIntToStr(UniqueID);

  //Проверить наличие объекта
  if FTable.Find(ID) <> nil then
    raise EsgeException.Create(_UNITNAME, Err_DuplicateObject, ID);

  //Добавить
  FTable.Add(ID, DrawObject);
end;


procedure TsgeGraphicOpenGLDrawObjectTable.Delete(UniqueID: Integer);
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


function TsgeGraphicOpenGLDrawObjectTable.Get(UniqueID: Integer): TsgeGraphicOpenGLDrawObject;
var
  ID: ShortString;
begin
  ID := sgeIntToStr(UniqueID);
  Result := TsgeGraphicOpenGLDrawObject(FTable.Find(ID));

  if Result = nil then
    raise EsgeException.Create(_UNITNAME, Err_ObjectNotFound, ID);
end;



initialization
begin
  OpenGLDrawObjectTable := TsgeGraphicOpenGLDrawObjectTable.Create;
end;


finalization
begin
  OpenGLDrawObjectTable.Free;
end;



end.

