{
Пакет             Simple Game Engine 2
Файл              sgeExtensionBase.pas
Версия            1.5
Создан            31.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Базовый
}
{$Include Defines.inc}

unit sgeExtensionBase;

{$mode objfpc}{$H+}

interface

uses
  sgeEventManager, sgeErrorManager;

const
  Err_CantCreateExtension   = 'CantCreateExtension';


type
  TsgeExtensionBase = class
  private
    //Ссылки
    FObjectList: TObject;                                           //Указатель на список объектов
    FExtensionList: TObject;                                        //Указатель на список расширений
    FEventManager: TsgeEventManager;                                //Менеджер событий
    FErrorManager: TsgeErrorManager;                                //Менеджер ошибок

  protected
     class function GetName: String; virtual; abstract;

    //Список объектов
    function  GetObject(Name: String): TObject;
    procedure AddObject(Name: String; Obj: TObject);
    procedure DeleteObject(Name: String);

    //Список расширений
    function  GetExtension(Name: String): TsgeExtensionBase;
    procedure AddExtension(Ext: TsgeExtensionBase);
    procedure DeleteExtension(Name: String);
  public
    constructor Create(ObjectList: TObject); virtual;
    destructor  Destroy; override;

    property Name: String read GetName;
    property EventManager: TsgeEventManager read FEventManager;
    property ErrorManager: TsgeErrorManager read FErrorManager;
  end;



implementation

uses
  sgeErrors, sgeNamedObjectList, sgeExtensionList;

const
  _UNITNAME = 'ExtensionBase';

  Err_ObjectNotFound    = 'ObjectNotFound';
  Err_ExtensionNotFound = 'ExtensionNotFound';



function TsgeExtensionBase.GetObject(Name: String): TObject;
begin
  Result := TsgeNamedObjectList(FObjectList).Get(Name);

  if Result = nil then
    raise EsgeException.Create(_UNITNAME, Err_ObjectNotFound, Name);
end;


procedure TsgeExtensionBase.AddObject(Name: String; Obj: TObject);
begin
  TsgeNamedObjectList(FObjectList).Add(Name, Obj);
end;


procedure TsgeExtensionBase.DeleteObject(Name: String);
begin
  if TsgeNamedObjectList(FObjectList).Exist(Name) then
    TsgeNamedObjectList(FObjectList).Delete(Name);
end;


function TsgeExtensionBase.GetExtension(Name: String): TsgeExtensionBase;
begin
  Result := TsgeExtensionList(FExtensionList).Get(Name);

  if Result = nil then
    raise EsgeException.Create(_UNITNAME, Err_ExtensionNotFound, Name);
end;


procedure TsgeExtensionBase.AddExtension(Ext: TsgeExtensionBase);
begin
  TsgeExtensionList(FExtensionList).Add(Ext);
end;


procedure TsgeExtensionBase.DeleteExtension(Name: String);
begin
  if TsgeExtensionList(FExtensionList).Exist(Name) then
    TsgeExtensionList(FExtensionList).Delete(Name);
end;


constructor TsgeExtensionBase.Create(ObjectList: TObject);
begin
  //Запомнить список объектов
  FObjectList := ObjectList;

  //Найти ссылки
  FExtensionList := GetObject(Object_ExtensionList);
  FEventManager := TsgeEventManager(GetObject(Object_EventManager));
  FErrorManager := TsgeErrorManager(GetObject(Object_ErrorManager));

  //Записать себя в список расширений
  AddExtension(Self);
end;


destructor TsgeExtensionBase.Destroy;
begin
  //Удалить себя из списка расширений
  DeleteExtension(GetName);
end;



end.

