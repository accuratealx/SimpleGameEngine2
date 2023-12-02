{
Пакет             Simple Game Engine 2
Файл              sgeResource.pas
Версия            1.0
Создан            05.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс хранения информации о загруженном ресурсе
}
{$Include Defines.inc}

unit sgeResourceItem;

{$mode ObjFPC}{$H+}

interface

uses
  sgeMetaInfoList;

type
  //Типы ресурсов
  TsgeResourceType = (
    rtUnknown,          //Неизвестный
    rtStream,           //Сырой блок байтов
    rtShader,           //Шейдеры
    rtAnsiFont,         //Спрайтовый шрифт
    rtSprite,           //Спрайт
    rtAnimationFrames,  //Кадры анимации
    rtSoundBuffer,      //Звуковой буфер
    rtStringList,       //Список строк
    rtParameters,       //Параметры
    rtContainer,        //Контейнер
    rtCursor            //Курсор
  );


const
  //Имена ресурсов
  sgeResourceNames: array[TsgeResourceType] of String = (
    'Unknown',
    'Stream',
    'Shader',
    'AnsiFont',
    'Sprite',
    'AnimationFrames',
    'SoundBuffer',
    'StringList',
    'Parameters',
    'Container',
    'Cursor'
  );


type
  TsgeResourceItem = class
  private
    FName: String;              //Имя ресурса в таблице
    FResType: TsgeResourceType; //Тип ресурса
    FGroup: String;             //Группа
    FObj: TObject;              //Ссылка на объект
    FMeta: TsgeMetaInfoList;    //Список метаинформации

  public
    constructor Create(Name: String; ResType: TsgeResourceType; Obj: TObject; MetaStr: String = ''; Group: String = '');
    destructor  Destroy; override;

    property Name: String read FName;
    property ResType: TsgeResourceType read FResType;
    property Group: String read FGroup;
    property Obj: TObject read FObj;
    property Meta: TsgeMetaInfoList read FMeta;
  end;



implementation


constructor TsgeResourceItem.Create(Name: String; ResType: TsgeResourceType; Obj: TObject; MetaStr: String; Group: String);
begin
  //Создать объекты
  FMeta := TsgeMetaInfoList.Create(MetaStr);

  //Записать параметры
  FName := Name;
  FResType := ResType;
  FGroup := Group;
  FObj := Obj;

  //Найти группу в метаинформации
  if Group = '' then
    FGroup := FMeta.GetValue('Group', '', True);
end;


destructor TsgeResourceItem.Destroy;
begin
  //Удалить объекты
  FMeta.Free;
  FObj.Free;
end;



end.

