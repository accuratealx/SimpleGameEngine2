{
Пакет             Simple Game Engine 2
Файл              sgeExtensionList.pas
Версия            1.3
Создан            24.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс: Список расширений
}
{$Include Defines.inc}

unit sgeExtensionList;

{$mode objfpc}{$H+}


interface

uses
  sgeExtensionBase, sgeTemplateCollection;


const
  //Имя объекта
  Object_ExtensionList = 'ExtensionList';


type
  //Список расширений
  TsgeExtensionList = class(specialize TsgeTemplateCollection<TsgeExtensionBase>)
  private

  public
    constructor Create;

    function  IndexOf(Name: String): Integer;
    function  IndexOf(Extension: TsgeExtensionBase): Integer;

    procedure Add(Extension: TsgeExtensionBase);
    procedure Delete(Extension: TsgeExtensionBase);
    function  Get(Name: String): TsgeExtensionBase;

    property Count: Integer read FCount;
    property Item[Index: Integer]: TsgeExtensionBase read GetItem;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'ExtensionList';

  Err_ObjectNotFound    = 'ObjectNotFound';


constructor TsgeExtensionList.Create;
begin
  inherited Create(True);
end;


function TsgeExtensionList.IndexOf(Name: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  Name := LowerCase(Name);
  for i := 0 to FCount - 1 do
    if LowerCase(FList[i].Name) = Name then
      Exit(i);
end;


function TsgeExtensionList.IndexOf(Extension: TsgeExtensionBase): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if FList[i] = Extension then
      Exit(i);
end;

procedure TsgeExtensionList.Add(Extension: TsgeExtensionBase);
begin
  inherited Add(Extension);
end;


procedure TsgeExtensionList.Delete(Extension: TsgeExtensionBase);
var
  Idx: Integer;
begin
  Idx := IndexOf(Extension);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ObjectNotFound, Extension.Name);
end;


function TsgeExtensionList.Get(Name: String): TsgeExtensionBase;
var
  Idx: Integer;
begin
  Result := nil;

  Idx := IndexOf(Name);
  if Idx <> -1 then
    Result := FList[Idx];
end;



end.

