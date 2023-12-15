{
Пакет             Simple Game Engine 2
Файл              sgeExtensionBase.pas
Версия            1.7
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
    FExtensionList: TObject;                      //Указатель на список расширений
    FEventManager: TsgeEventManager;              //Менеджер событий
    FErrorManager: TsgeErrorManager;              //Менеджер ошибок

  protected
    function GetName: String; virtual; abstract;

    //Список объектов
    function  Core_GetObject(Name: String; ItemType: ShortString = ''): TObject;
    procedure Core_AddObject(Name: String; Obj: TObject; ItemType: ShortString = '');
    procedure Core_DeleteObject(Name: String; ItemType: ShortString = '');

    //Список расширений
    function  ExtensionExist(Name: String): Boolean;
    function  GetExtension(Name: String): TsgeExtensionBase;
    procedure AddExtension(Ext: TsgeExtensionBase);
    procedure DeleteExtension(Ext: TsgeExtensionBase);

    //События
    procedure RegisterEventHandlers; virtual;
    procedure UnregiterEventHandlers; virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    property Name: String read GetName;
    property EventManager: TsgeEventManager read FEventManager;
    property ErrorManager: TsgeErrorManager read FErrorManager;
  end;



implementation

uses
  sgeErrors, sgeCorePointerList, sgeExtensionList;

const
  _UNITNAME = 'ExtensionBase';

  Err_ObjectNotFound    = 'ObjectNotFound';
  Err_ExtensionNotFound = 'ExtensionNotFound';



function TsgeExtensionBase.Core_GetObject(Name: String; ItemType: ShortString): TObject;
begin
  //Поиск объекта по имени
  if ItemType = '' then
    Result := CorePointerList.GetObject(Name)
  else
    Result := CorePointerList.GetObject(Name, ItemType);
  if Result = nil then
    raise EsgeException.Create(_UNITNAME, Err_ObjectNotFound, Name);
end;


procedure TsgeExtensionBase.Core_AddObject(Name: String; Obj: TObject; ItemType: ShortString);
begin
  CorePointerList.AddObject(Name, Obj, ItemType);
end;


procedure TsgeExtensionBase.Core_DeleteObject(Name: String; ItemType: ShortString);
begin
  if ItemType = '' then
    if CorePointerList.Exist(Name) then
      CorePointerList.Delete(Name)
  else
    if CorePointerList.Exist(Name, ItemType) then
      CorePointerList.Delete(Name, ItemType);
end;


function TsgeExtensionBase.ExtensionExist(Name: String): Boolean;
begin
  Result := TsgeExtensionList(FExtensionList).IndexOf(Name) <> -1;
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


procedure TsgeExtensionBase.DeleteExtension(Ext: TsgeExtensionBase);
begin
  TsgeExtensionList(FExtensionList).Delete(Ext);
end;


procedure TsgeExtensionBase.RegisterEventHandlers;
begin
  //Подписка на события
end;


procedure TsgeExtensionBase.UnregiterEventHandlers;
begin
  //Отписка от событий
  FEventManager.SubscriberGroupList.UnSubscribe(Self);
end;


constructor TsgeExtensionBase.Create;
begin
  //Найти ссылки
  FExtensionList := Core_GetObject(Object_ExtensionList);
  FEventManager := TsgeEventManager(Core_GetObject(Object_EventManager));
  FErrorManager := TsgeErrorManager(Core_GetObject(Object_ErrorManager));

  //Подписаться на события
  RegisterEventHandlers;

  //Записать себя в список расширений
  AddExtension(Self);

  //Добавить себя в CoreList
  Core_AddObject(ItemType_SGEExtension + '.' + GetName, Self, ItemType_SGEExtension);
end;


destructor TsgeExtensionBase.Destroy;
begin
  //Отписаться от событий
  UnregiterEventHandlers;

  //Удалить себя из списка расширений
  DeleteExtension(Self);
end;



end.

