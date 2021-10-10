{
Пакет             Simple Game Engine 2
Файл              sgeExtensionResourceList.pas
Версия            1.6
Создан            14.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Список ресурсов
}
{$Include Defines.inc}

unit sgeExtensionResourceList;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeMemoryStream, sgeStringList, sgeSimpleParameters, sgeSimpleCommand,
  sgeExtensionBase,
  sgeResourceList, sgeMetaInfoList, sgeExtensionFileSystem, sgeExtensionSound,
  sgeSystemFont, sgeGraphicFont, sgeGraphicSprite, sgeGraphicAnimationFrames,
  sgeGraphicAnimation, sgeSimpleContainer, sgeSoundBuffer;


const
  Extension_ResourceList = 'ResourceList';


type
  //Типы ресурсов
  TsgeResourceType = sgeResourceList.TsgeResourceType;


  //Ресурсы по умолчанию
  TsgeExtensionResourceDefault = record
    Font: TsgeGraphicFont;
    Sprite: TsgeGraphicSprite;
    Frames: TsgeGraphicAnimationFrames;
    Animation: TsgeGraphicAnimation;
    SoundBufer: TsgeSoundBuffer;
    StringList: TsgeStringList;
    Parameters: TsgeSimpleParameters;
    Container: TsgeSimpleContainer;
  end;



  TsgeExtensionResourceList = class(TsgeExtensionBase)
  private
    //Ссылки
    FExtFileSystem: TsgeExtensionFileSystem;

    //Классы
    FResourceList: TsgeResourceList;

    //Параметры
    FDefault: TsgeExtensionResourceDefault;

    //Создание объектов
    function  LoadResource_SystemFont(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_Font(Cmd: TsgeSimpleCommand; Meta: TsgeMetaInfoList): TObject;
    function  LoadResource_Sprite(Stream: TsgeMemoryStream; Meta: TsgeMetaInfoList): TObject;
    function  LoadResource_AnimationFrames(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_Animation(Stream: TsgeMemoryStream; Meta: TsgeMetaInfoList): TObject;
    function  LoadResource_SoundBuffer(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_StringList(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_Parameters(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_Container(Stream: TsgeMemoryStream): TObject;

    //Команды таблицы
    procedure Command_SetParam(Prm: TsgeSimpleParameters; Cmd: TsgeSimpleCommand);
    procedure Command_DeleteParam(Prm: TsgeSimpleParameters; Cmd: TsgeSimpleCommand);
    procedure Command_ClearParams(Prm: TsgeSimpleParameters);
    procedure Command_LoadResource(Cmd: TsgeSimpleCommand; BaseDirectory: String = '');
    procedure Command_LoadTable(Cmd: TsgeSimpleCommand; BaseDirectory: String = '');

  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    function  GetFont(Name: String): TsgeGraphicFont;
    function  GetSprite(Name: String): TsgeGraphicSprite;
    function  GetFrames(Name: String): TsgeGraphicAnimationFrames;
    function  GetAnimation(Name: String): TsgeGraphicAnimation;
    function  GetSoundBuffer(Name: String): TsgeSoundBuffer;
    function  GetStringList(Name: String): TsgeStringList;
    function  GetParameters(Name: String): TsgeSimpleParameters;
    function  GetContainer(Name: String): TsgeSimpleContainer;

    procedure FromString(Str: String; BaseDirectory: String = '');
    procedure FromMemoryStream(Stream: TsgeMemoryStream; BaseDirectory: String = '');
    procedure LoadFromFile(FileName: String);

    property ResourceList: TsgeResourceList read FResourceList;
    property Default: TsgeExtensionResourceDefault read FDefault;
  end;




implementation

uses
  sgeErrors,
  sgeSystemUtils, sgeFileUtils, sgeStringUtils, sgeOSPlatform;


const
  //Имя модуля
  _UNITNAME = 'ExtensionResourceList';

  Err_UnknownCommand      = 'UnknownCommand';
  Err_FileNotFound        = 'FileNotFound';
  Err_CantReadFile        = 'CantReadFile';
  Err_NotEnoughParameters = 'NotEnoughParameters';
  Err_LoadResourceError   = 'LoadResourceError';
  Err_DuplicateResource   = 'DuplicateResource';
  Err_UnknownResource     = 'UnknownResource';
  Err_ResourceNotFound    = 'ResourceNotFound';


  //Имена команд
  rcSetParameter          = 'setparam';
  rcDeleteParameter       = 'delparam';
  rcClearParameters       = 'clearparams';
  rcLoadResource          = 'loadres';
  rcLoadTable             = 'loadtable';



function TsgeExtensionResourceList.LoadResource_SystemFont(Stream: TsgeMemoryStream): TObject;
begin
  Result := TsgeSystemFont.Create(Stream);
end;


function TsgeExtensionResourceList.LoadResource_Font(Cmd: TsgeSimpleCommand; Meta: TsgeMetaInfoList): TObject;
var
  Size: Integer;
  fAttr: TsgeGraphicFontAttrib;
  s: String;
  FntName: String;
begin
  //Name
  if Cmd.Count >= 4 then FntName := Cmd.Part[3] else FntName := '';

  //Size
  Size := Meta.GetValue('Size', 12, True);

  //Attrib
  fAttr := [];
  s := LowerCase(Meta.GetValue('Attrib', '', True));
  if Pos('b', s) <> 0 then Include(fAttr, gfaBold);
  if Pos('i', s) <> 0 then Include(fAttr, gfaItalic);
  if Pos('u', s) <> 0 then Include(fAttr, gfaUnderline);
  if Pos('s', s) <> 0 then Include(fAttr, gfaStrikeOut);

  //Вернуть объект
  Result := TsgeGraphicFont.Create(FntName, Size, fAttr);
end;


function TsgeExtensionResourceList.LoadResource_Sprite(Stream: TsgeMemoryStream; Meta: TsgeMetaInfoList): TObject;

  function GetValue(Str: String): TsgeGraphicSpriteFilter;
  begin
    case LowerCase(Str) of
      'linear' : Result := gsfLinear;
      'nearest': Result := gsfNearest;
      else Result := gsfNearest;
    end;
  end;

var
  Cols, Rows: Integer;
  MagFilter, MinFilter: TsgeGraphicSpriteFilter;
begin
  //Определить параметры
  Cols := Meta.GetValue('Cols', 1, True);
  Rows := Meta.GetValue('Rows', 1, True);
  MinFilter := GetValue(Meta.GetValue('MinFilter', '', True));
  MagFilter := GetValue(Meta.GetValue('MagFilter', '', True));

  //Вернуть объект
  Result := TsgeGraphicSprite.Create(Stream, Cols, Rows, MagFilter, MinFilter);
end;


function TsgeExtensionResourceList.LoadResource_AnimationFrames(Stream: TsgeMemoryStream): TObject;
begin
  Result := TsgeGraphicAnimationFrames.Create(Stream, FResourceList);
end;


function TsgeExtensionResourceList.LoadResource_Animation(Stream: TsgeMemoryStream; Meta: TsgeMetaInfoList): TObject;
var
  Width, Height: Integer;
begin
  //Найти параметры
  Width := Meta.GetValue('Width', 8, True);
  Height := Meta.GetValue('Height', 8, True);

  //Вернуть объект
  Result := TsgeGraphicAnimation.Create(Stream, Width, Height, FResourceList);
end;


function TsgeExtensionResourceList.LoadResource_SoundBuffer(Stream: TsgeMemoryStream): TObject;
begin
  Result := TsgeSoundBuffer.Create(Stream);
end;


function TsgeExtensionResourceList.LoadResource_StringList(Stream: TsgeMemoryStream): TObject;
begin
  Result := TsgeStringList.Create;
  TsgeStringList(Result).FromMemoryStream(Stream);
end;


function TsgeExtensionResourceList.LoadResource_Parameters(Stream: TsgeMemoryStream): TObject;
begin
  Result := TsgeSimpleParameters.Create(Stream);
end;


function TsgeExtensionResourceList.LoadResource_Container(Stream: TsgeMemoryStream): TObject;
begin
  Result := TsgeSimpleContainer.Create;
  TsgeSimpleContainer(Result).FromMemoryStream(Stream);
end;


procedure TsgeExtensionResourceList.Command_SetParam(Prm: TsgeSimpleParameters; Cmd: TsgeSimpleCommand);
begin
  if Cmd.Count < 3 then
    raise EsgeException.Create(_UNITNAME, Err_NotEnoughParameters);

  Prm.SetValue(Cmd.Part[1], Cmd.Part[2]);
end;


procedure TsgeExtensionResourceList.Command_DeleteParam(Prm: TsgeSimpleParameters; Cmd: TsgeSimpleCommand);
begin
  if Cmd.Count < 2 then
    raise EsgeException.Create(_UNITNAME, Err_NotEnoughParameters);

  Prm.Delete(Cmd.Part[1]);
end;


procedure TsgeExtensionResourceList.Command_ClearParams(Prm: TsgeSimpleParameters);
begin
  Prm.Clear;
end;


procedure TsgeExtensionResourceList.Command_LoadResource(Cmd: TsgeSimpleCommand; BaseDirectory: String);
var
  Idx: Integer;
  nm, fn, Group, MetaStr, s: String;
  ResObj: TObject;
  ResType, rt: TsgeResourceType;
  Stream: TsgeMemoryStream;
  MetaObj: TsgeMetaInfoList;
begin
  //Проверить количество частей
  if Cmd.Count < 4 then
    raise EsgeException.Create(_UNITNAME, Err_NotEnoughParameters);

  //Проверить на одинаковое имя
  nm := Cmd.Part[2];
  Idx := FResourceList.IndexOf(nm);
  if Idx <> -1 then
    raise EsgeException.Create(_UNITNAME, Err_DuplicateResource, nm);

  //Поправить базовый каталог
  if BaseDirectory <> '' then BaseDirectory := sgeCheckPathDelimiter(BaseDirectory);

  //Подготовить переменные
  ResObj := nil;
  fn := BaseDirectory + Cmd.Part[3];

  //Подготовить метастроку
  MetaStr := '';
  s := sgeTrim(Cmd.GetTail(4));

  if (s <> '') and (s[1] = '[') and (s[Length(s)] = ']') then
    begin
    System.Delete(s, Length(s), 1);
    System.Delete(s, 1, 1);
    MetaStr := s;
    end;

  //Создать объект метаинформации из строки
  MetaObj := TsgeMetaInfoList.Create(MetaStr);

  //Подготовить группу из метаинформации
  Group := MetaObj.GetValue('Group', '', True);

  //Загрузить файл в MemoryStream
  try
    Stream := TsgeMemoryStream.Create;

    try
      //Тип ресурса
      s := LowerCase(Cmd.Part[1]);
      ResType := sgeStrToResType(s);

      //Прочитаем файл, если это не Font
      if ResType <> rtFont then FExtFileSystem.ReadFile(fn, Stream);

      //Создать ресурс
      case ResType of
        rtSystemFont:
          begin
          ResObj := LoadResource_SystemFont(Stream);
          TsgeSystemFont(ResObj).FileName := fn;
          rt := rtSystemFont;
          end;

        rtFont:
          begin
          ResObj := LoadResource_Font(Cmd, MetaObj);
          rt := rtFont;
          end;

        rtSprite:
          begin
          ResObj := LoadResource_Sprite(Stream, MetaObj);
          TsgeGraphicSprite(ResObj).FileName := fn;
          rt := rtSprite;
          end;

        rtAnimationFrames:
          begin
          ResObj := LoadResource_AnimationFrames(Stream);
          TsgeGraphicAnimationFrames(ResObj).FileName := fn;
          rt := rtAnimationFrames;
          end;

        rtAnimation:
          begin
          ResObj := LoadResource_Animation(Stream, MetaObj);
          TsgeGraphicAnimation(ResObj).FileName := fn;
          rt := rtAnimation;
          end;

        rtSoundBuffer:
          begin
          ResObj := LoadResource_SoundBuffer(Stream);
          TsgeSoundBuffer(ResObj).FileName := fn;
          rt := rtSoundBuffer;
          end;

        rtStringList:
          begin
          ResObj := LoadResource_StringList(Stream);
          rt := rtStringList;
          end;

        rtParameters:
          begin
          ResObj := LoadResource_Parameters(Stream);
          TsgeSimpleParameters(ResObj).FileName := fn;
          rt := rtParameters;
          end;

        rtContainer:
          begin
          ResObj := LoadResource_Container(Stream);
          TsgeSimpleContainer(ResObj).FileName := fn;
          rt := rtContainer;
          end;

        rtUnknown:
          raise EsgeException.Create(_UNITNAME, Err_UnknownResource, Cmd.Part[1]);


        else
          raise EsgeException.Create(_UNITNAME, Err_UnknownResource, Cmd.Part[1]);
      end;

      //Добавить в хранилище
      if ResObj <> nil then FResourceList.AddItem(nm, rt, ResObj, MetaObj, Group);


    except
      on E: EsgeException do
        begin
        MetaObj.Free;
        raise EsgeException.Create(E.Message);
        end;
    end;


  finally
    Stream.Free;
  end;
end;


procedure TsgeExtensionResourceList.Command_LoadTable(Cmd: TsgeSimpleCommand; BaseDirectory: String);
begin
  //Проверить на нужное количество частей
  if Cmd.Count < 2 then
    raise EsgeException.Create(_UNITNAME, Err_NotEnoughParameters);

  //Поправить базовый каталог
  if BaseDirectory <> '' then BaseDirectory := sgeCheckPathDelimiter(BaseDirectory);

  //Загрузить таблицу
  LoadFromFile(BaseDirectory + Cmd.Part[1]);
end;


class function TsgeExtensionResourceList.GetName: String;
begin
  Result := Extension_ResourceList;
end;


constructor TsgeExtensionResourceList.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Получить указатели на классы
    FExtFileSystem := TsgeExtensionFileSystem(GetExtension(Extension_FileSystem));

    //Классы
    FResourceList := TsgeResourceList.Create;

    //Создать ресурсы по умолчанию
    FDefault.Font := TsgeGraphicFont.Create('System', 14, [gfaBold]);
    FDefault.Sprite := TsgeGraphicSprite.Create;
    FDefault.Frames := TsgeGraphicAnimationFrames.Create;
    FDefault.Frames.Add(FDefault.Sprite);
    FDefault.Animation := TsgeGraphicAnimation.Create(FDefault.Frames, 32, 32);
    FDefault.StringList := TsgeStringList.Create;
    FDefault.Parameters := TsgeSimpleParameters.Create;
    FDefault.Container := TsgeSimpleContainer.Create;
    if ExtensionExist(Extension_Sound) then
      FDefault.SoundBufer := TsgeSoundBuffer.CreateBlank;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionResourceList.Destroy;
begin
  //Ресурсы по умолчанию
  FDefault.Font.Free;
  FDefault.Sprite.Free;
  FDefault.Frames.Free;
  FDefault.Animation.Free;
  FDefault.SoundBufer.Free;
  FDefault.StringList.Free;
  FDefault.Parameters.Free;
  FDefault.Container.Free;

  //Классы
  FResourceList.Free;

  inherited Destroy;
end;


function TsgeExtensionResourceList.GetFont(Name: String): TsgeGraphicFont;
begin
  Result := TsgeGraphicFont(FResourceList.TypedObj[Name, rtFont]);
  if Result = nil then
    begin
    Result := FDefault.Font;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
    end;
end;


function TsgeExtensionResourceList.GetSprite(Name: String): TsgeGraphicSprite;
begin
  Result := TsgeGraphicSprite(FResourceList.TypedObj[Name, rtSprite]);
  if Result = nil then
    begin
    Result := FDefault.Sprite;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
    end;
end;


function TsgeExtensionResourceList.GetFrames(Name: String): TsgeGraphicAnimationFrames;
begin
  Result := TsgeGraphicAnimationFrames(FResourceList.TypedObj[Name, rtAnimationFrames]);
  if Result = nil then
    begin
    Result := FDefault.Frames;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
    end;
end;


function TsgeExtensionResourceList.GetAnimation(Name: String): TsgeGraphicAnimation;
begin
  Result := TsgeGraphicAnimation(FResourceList.TypedObj[Name, rtAnimation]);
  if Result = nil then
    begin
    Result := FDefault.Animation;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
    end;
end;


function TsgeExtensionResourceList.GetSoundBuffer(Name: String): TsgeSoundBuffer;
begin
  Result := TsgeSoundBuffer(FResourceList.TypedObj[Name, rtSoundBuffer]);
  if Result = nil then
    begin
    Result := FDefault.SoundBufer;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
    end;
end;


function TsgeExtensionResourceList.GetStringList(Name: String): TsgeStringList;
begin
  Result := TsgeStringList(FResourceList.TypedObj[Name, rtStringList]);
  if Result = nil then
    begin
    Result := FDefault.StringList;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
    end;
end;


function TsgeExtensionResourceList.GetParameters(Name: String): TsgeSimpleParameters;
begin
  Result := TsgeSimpleParameters(FResourceList.TypedObj[Name, rtParameters]);
  if Result = nil then
    begin
    Result := FDefault.Parameters;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
    end;
end;


function TsgeExtensionResourceList.GetContainer(Name: String): TsgeSimpleContainer;
begin
  Result := TsgeSimpleContainer(FResourceList.TypedObj[Name, rtContainer]);
  if Result = nil then
    begin
    Result := FDefault.Container;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
    end;
end;


procedure TsgeExtensionResourceList.FromString(Str: String; BaseDirectory: String);
var
  Params: TsgeSimpleParameters;
  Lines: TsgeStringList;
  Cmd: TsgeSimpleCommand;
  i, c: Integer;
begin
  try
    //Подготовить классы
    Lines := TsgeStringList.Create;
    Params := TsgeSimpleParameters.Create;
    Cmd := TsgeSimpleCommand.Create;

    //Загрузить строки из строки
    Lines.FromString(Str);

    //Пробежать по строкам
    c := Lines.Count - 1;
    for i := 0 to c do
      begin
      Lines.Part[i] := sgeTrim(Lines.Part[i]);                                        //Отрезать лишнее
      if Lines.Part[i] = '' then Continue;                                            //Пусто
      if Lines.Part[i][1] = '#' then Continue;                                        //Заметка
      Lines.Part[i] := sgeSubstituteParamsToString(Lines.Part[i], Params, '%', '%');  //Подставить в строку переменные
      Cmd.Command := Lines.Part[i];                                                   //Разобрать на части

      //Загрузить ресурс
      try
        case LowerCase(Cmd.Part[0]) of
          rcClearParameters : Command_ClearParams(Params);
          rcSetParameter    : Command_SetParam(Params, Cmd);
          rcDeleteParameter : Command_DeleteParam(Params, Cmd);
          rcLoadTable       : Command_LoadTable(Cmd, BaseDirectory);
          rcLoadResource    : Command_LoadResource(Cmd, BaseDirectory);
          else
            raise EsgeException.Create(_UNITNAME, Err_UnknownCommand, Cmd.Part[0]);
        end;
      except
        on E: EsgeException do
          raise EsgeException.Create(_UNITNAME, Err_LoadResourceError, Cmd.Part[2] + ' [' + sgeIntToStr(i + 1) + ']', E.Message);
      end;

      end;

  finally
    Lines.Free;
    Cmd.Free;
    Params.Free;
  end;
end;


procedure TsgeExtensionResourceList.FromMemoryStream(Stream: TsgeMemoryStream; BaseDirectory: String);
begin
  FromString(Stream.ToString, BaseDirectory);
end;


procedure TsgeExtensionResourceList.LoadFromFile(FileName: String);
var
  //F: TsgeFile;
  MS: TsgeMemoryStream;
  S, BaseDirectory: String;
begin
  //Проверить на существование файла
  if not FExtFileSystem.FileExists(FileName) then
    raise EsgeException.Create(_UNITNAME, Err_FileNotFound, FileName);

  //Прочитать файл в строку
  try

    MS := TsgeMemoryStream.Create;
    try

      FExtFileSystem.ReadFile(FileName, MS);
      S := MS.ToString;

    except
      raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName);
    end;

  finally
    MS.Free;
  end;

  //Подготовить базовый каталог
  BaseDirectory := sgeExtractFilePath(FileName);

  //Преобразовать строку в массив
  FromString(S, BaseDirectory);
end;


end.

