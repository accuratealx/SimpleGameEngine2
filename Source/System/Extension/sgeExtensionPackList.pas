{
Пакет             Simple Game Engine 2
Файл              sgeExtensionPackList.pas
Версия            1.3
Создан            07.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Файловые архивы
}
{$Include Defines.inc}

unit sgeExtensionPackList;

{$mode objfpc}{$H+}

interface

uses
  sgeExtensionBase,
  sgePackFileList;

const
  Extension_PackList = 'PackList';

  ExtPack = 'SGEPack';


type
  TsgeExtensionPackList = class(TsgeExtensionBase)
  private
    FPackList: TsgePackFileList;

  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    procedure LoadPackFromDirectory(Directory: String = ''; Ext: String = ExtPack);

    property PackList: TsgePackFileList read FPackList;
  end;



implementation

uses
  sgeErrors, sgeStringList, sgeFileUtils, sgeOSPlatform;

const
  _UNITNAME = 'ExtensionPackList';

  Err_CantLoadPackFile = 'CantLoadPackFile';



class function TsgeExtensionPackList.GetName: String;
begin
  Result := Extension_PackList;
end;


constructor TsgeExtensionPackList.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Создать список архивов
    FPackList := TsgePackFileList.Create;
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionPackList.Destroy;
begin
  FPackList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionPackList.LoadPackFromDirectory(Directory: String; Ext: String);
var
  List: TsgeStringList;
  i, c: Integer;
begin
  List := TsgeStringList.Create;
  try
    //Подготовить каталог
    if Directory = '' then
      Directory := sgeGetApplicationDirectory;
    Directory := sgeCheckPathDelimiter(Directory);

    //Получить список файлов
    sgeFindFilesInFolderByExt(Directory, List, Ext);

    //Добавить архивы в массив
    try
      c := List.Count - 1;
      for i := 0 to c do
        FPackList.Add(Directory + List.Part[i]);
    except
       on E: EsgeException do
         raise EsgeException.Create(_UNITNAME, Err_CantLoadPackFile, '', E.Message);
    end;
  finally
    List.Free;
  end;
end;



end.

