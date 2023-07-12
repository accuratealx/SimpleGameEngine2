{
Пакет             Simple Game Engine 2
Файл              sgeExtensionResourceList.pas
Версия            1.8
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
  sgeExtensionBase, sgeResourceList, sgeResourceItem, sgeMetaInfoList,
  sgeExtensionFileSystem, sgeExtensionSound,
  sgeAnsiFont, sgeSprite, sgeAnimationFrameList, sgeSimpleContainer, sgeSoundBuffer, sgeCursor;


const
  Extension_ResourceList = 'ResourceList';


type
  //Типы ресурсов
  TsgeResourceType = sgeResourceItem.TsgeResourceType;


  //Ресурсы по умолчанию
  TsgeExtensionResourceDefault = record
    AnsiFont: TsgeAnsiFont;
    Sprite: TsgeSprite;
    Frames: TsgeAnimationFrameList;
    SoundBufer: TsgeSoundBuffer;
    StringList: TsgeStringList;
    Parameters: TsgeSimpleParameters;
    Container: TsgeSimpleContainer;
    Cursor: TsgeCursor;
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
    function  LoadResource_AnsiFont(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_Sprite(Stream: TsgeMemoryStream; Meta: TsgeMetaInfoList): TObject;
    function  LoadResource_AnimFrames(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_SoundBuffer(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_StringList(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_Parameters(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_Container(Stream: TsgeMemoryStream): TObject;
    function  LoadResource_Cursor(Stream: TsgeMemoryStream): TObject;

    //Команды таблицы
    procedure Command_SetParam(Prm: TsgeSimpleParameters; Cmd: TsgeSimpleCommand);
    procedure Command_DeleteParam(Prm: TsgeSimpleParameters; Cmd: TsgeSimpleCommand);
    procedure Command_ClearParams(Prm: TsgeSimpleParameters);
    procedure Command_LoadResource(Cmd: TsgeSimpleCommand; BaseDirectory: String = '');
    procedure Command_LoadTable(Cmd: TsgeSimpleCommand; BaseDirectory: String = '');

    procedure GetMetaStringAndFileName(FileName: String; var Name, Meta: String);
  protected
    function GetName: String; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure SetDefaultAnsiFont(Font: TsgeAnsiFont);

    function GetAnsiFont(Name: String): TsgeAnsiFont;
    function GetSprite(Name: String): TsgeSprite;
    function GetAnimationFrames(Name: String): TsgeAnimationFrameList;
    function GetSoundBuffer(Name: String): TsgeSoundBuffer;
    function GetStringList(Name: String): TsgeStringList;
    function GetParameters(Name: String): TsgeSimpleParameters;
    function GetContainer(Name: String): TsgeSimpleContainer;
    function GetCursor(Name: String): TsgeCursor;

    //Загрузка ресурса из файла с разбором метаинформации
    procedure LoadResourceFromFile(ResType: TsgeResourceType; FileName: String);

    procedure FromString(Str: String; BaseDirectory: String = '');
    procedure FromMemoryStream(Stream: TsgeMemoryStream; BaseDirectory: String = '');
    procedure LoadFromFile(FileName: String);

    property ResourceList: TsgeResourceList read FResourceList;
    property Default: TsgeExtensionResourceDefault read FDefault;
  end;



implementation

uses
  sgeErrors,
  sgeSystemUtils, sgeFileUtils, sgeStringUtils, sgeOSPlatform,
  sgeAnimationFrame;


const
  //Имя модуля
  _UNITNAME = 'ExtensionResourceList';

  //Ошибки
  Err_UnknownCommand      = 'UnknownCommand';
  Err_FileNotFound        = 'FileNotFound';
  Err_CantReadFile        = 'CantReadFile';
  Err_NotEnoughParameters = 'NotEnoughParameters';
  Err_LoadResourceError   = 'LoadResourceError';
  Err_UnknownResource     = 'UnknownResource';
  Err_ResourceNotFound    = 'ResourceNotFound';

  //Имена команд
  rcSetParameter          = 'setparam';
  rcDeleteParameter       = 'delparam';
  rcClearParameters       = 'clearparams';
  rcLoadResource          = 'loadres';
  rcLoadTable             = 'loadtable';




function TsgeExtensionResourceList.LoadResource_AnsiFont(Stream: TsgeMemoryStream): TObject;
begin
  Result := TsgeAnsiFont.Create(Stream);
end;


function TsgeExtensionResourceList.LoadResource_Sprite(Stream: TsgeMemoryStream; Meta: TsgeMetaInfoList): TObject;

  //Фильтр для увеличения
  function GetMagFilter(Str: String): TsgeSpriteMagFilter;
  begin
    case LowerCase(Str) of
      'linear':
        Result := smagfLinear;

      'nearest':
        Result := smagfNearest;

      else
        Result := smagfNearest;
    end;
  end;

  //Фильтр для уменьшения
  function GetMinFilter(Str: String): TsgeSpriteMinFilter;
  begin
    case LowerCase(Str) of
      'linear':
        Result := sminfLinear;

      'nearest':
        Result := sminfNearest;

      else
        Result := sminfNearest;
    end;
  end;

var
  Cols, Rows: Integer;
  MagFilter: TsgeSpriteMagFilter;
  MinFilter: TsgeSpriteMinFilter;
begin
  //Определить параметры
  Cols := Meta.GetValue('Cols', 1, True);
  Rows := Meta.GetValue('Rows', 1, True);
  MinFilter := GetMinFilter(Meta.GetValue('MinFilter', '', True));
  MagFilter := GetMagFilter(Meta.GetValue('MagFilter', '', True));

  //Вернуть объект
  Result := TsgeSprite.Create(Stream, Cols, Rows);
  TsgeSprite(Result).MagFilter := MagFilter;
  TsgeSprite(Result).MinFilter := MinFilter;
end;


function TsgeExtensionResourceList.LoadResource_AnimFrames(Stream: TsgeMemoryStream): TObject;
begin
  Result := TsgeAnimationFrameList.Create(Stream);
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


function TsgeExtensionResourceList.LoadResource_Cursor(Stream: TsgeMemoryStream): TObject;
var
  Width, Height, HotX, HotY: Integer;
  FrameName, SpriteName: String;
  Sprite: TsgeSprite;
  Frames: TsgeAnimationFrameList;
  Params: TsgeSimpleParameters;
begin
  Params := TsgeSimpleParameters.Create(Stream);
  try
    //Прочитать параметры из файла
    Width := Params.GetValue('Width', 16);
    Height := Params.GetValue('Height', 16);
    HotX := Params.GetValue('HotX', 0);
    HotY := Params.GetValue('HotY', 0);
    SpriteName := Params.GetValue('Sprite', '');
    FrameName := Params.GetValue('AnimationFrames', '');

    //Найти спрайт
    Sprite := TsgeSprite(FResourceList.TypedItem[SpriteName, rtSprite].Obj);

    //Найти кадры анимации
    Frames :=  TsgeAnimationFrameList(FResourceList.TypedItem[FrameName, rtAnimationFrames].Obj);

    //Вернуть объект
    Result := TsgeCursor.Create(Sprite, Frames, Width, Height, HotX, HotY);

  finally
    Params.Free;
  end;
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
  nm, fn, MetaStr, s: String;
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

  //Поправить базовый каталог
  if BaseDirectory <> '' then
    BaseDirectory := sgeCheckPathDelimiter(BaseDirectory);

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

  //Загрузить файл в MemoryStream
  try
    Stream := TsgeMemoryStream.Create;

    try
      //Тип ресурса
      s := LowerCase(Cmd.Part[1]);
      ResType := sgeStrToResType(s);

      //Прочитаем файл
      FExtFileSystem.ReadFile(fn, Stream);

      //Создать ресурс
      case ResType of
        rtStream:
        begin
          ResObj := TsgeMemoryStream.Create(Stream);
          rt := rtStream;
        end;

        rtShader:
        begin
          ResObj := TsgeMemoryStream.Create(Stream);
          rt := rtShader;
        end;

        rtAnsiFont:
        begin
          ResObj := LoadResource_AnsiFont(Stream);
          rt := rtAnsiFont;
        end;

        rtSprite:
        begin
          ResObj := LoadResource_Sprite(Stream, MetaObj);
          rt := rtSprite;
        end;

        rtAnimationFrames:
        begin
          ResObj := LoadResource_AnimFrames(Stream);
          rt := rtAnimationFrames;
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

        rtCursor:
        begin
          ResObj := LoadResource_Cursor(Stream);
          rt := rtCursor;
        end;

        rtUnknown:
          raise EsgeException.Create(_UNITNAME, Err_UnknownResource, Cmd.Part[1]);

        else
          raise EsgeException.Create(_UNITNAME, Err_UnknownResource, Cmd.Part[1]);
      end;

      //Добавить в хранилище
      if ResObj <> nil then
        FResourceList.AddItem(nm, rt, ResObj, MetaStr);

    except
      on E: EsgeException do
        raise EsgeException.Create(E.Message);
    end;

  finally
    MetaObj.Free;
    Stream.Free;
  end;
end;


procedure TsgeExtensionResourceList.Command_LoadTable(Cmd: TsgeSimpleCommand; BaseDirectory: String);
begin
  //Проверить на нужное количество частей
  if Cmd.Count < 2 then
    raise EsgeException.Create(_UNITNAME, Err_NotEnoughParameters);

  //Поправить базовый каталог
  if BaseDirectory <> '' then
    BaseDirectory := sgeCheckPathDelimiter(BaseDirectory);

  //Загрузить таблицу
  LoadFromFile(BaseDirectory + Cmd.Part[1]);
end;


procedure TsgeExtensionResourceList.GetMetaStringAndFileName(FileName: String; var Name, Meta: String);
var
  sName: String;
  Idx1, Idx2: Integer;
begin
  //Имя файла без расширения
  sName := sgeChangeFileExt(sgeExtractFileName(FileName), '');

  //Значения по умолчанию
  Name := sName;
  Meta := '';

  //Поиск метастроки
  Idx1 := Pos('[', sName);
  Idx2 := Pos(']', sName);

  if (Idx1 <> 0) and (Idx2 <> 0) and (Idx1 < Idx2) then
  begin
    Meta := Copy(sName, Idx1 + 1, Idx2 - Idx1 - 1);                 //Выделить метастроку

    Delete(Name, Idx1, Length(Name) - Idx1 + 1);                    //Удалить метастроку из имени
    Name := sgeTrim(Name);                                          //Обрезать лишнее
  end;
end;


function TsgeExtensionResourceList.GetName: String;
begin
  Result := Extension_ResourceList;
end;


constructor TsgeExtensionResourceList.Create;
begin
  try
    inherited Create;

    //Получить указатели на классы
    FExtFileSystem := TsgeExtensionFileSystem(GetExtension(Extension_FileSystem));

    //Классы
    FResourceList := TsgeResourceList.Create;

    //Создать ресурсы по умолчанию
    FDefault.Sprite := TsgeSprite.Create(16, 16);
    FDefault.Sprite.FillChessBoard(4);

    FDefault.Frames := TsgeAnimationFrameList.Create;
    FDefault.Frames.Add(TsgeAnimationFrame.Create(0, 0, 0));

    FDefault.StringList := TsgeStringList.Create;
    FDefault.Parameters := TsgeSimpleParameters.Create;
    FDefault.Container := TsgeSimpleContainer.Create;
    FDefault.Cursor := TsgeCursor.Create(FDefault.Sprite, FDefault.Frames, 16, 16, 0, 0);

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
  FDefault.Sprite.Free;
  FDefault.Frames.Free;
  FDefault.SoundBufer.Free;
  FDefault.StringList.Free;
  FDefault.Parameters.Free;
  FDefault.Container.Free;
  FDefault.Cursor.Free;

  //Классы
  FResourceList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionResourceList.SetDefaultAnsiFont(Font: TsgeAnsiFont);
begin
  FDefault.AnsiFont := Font;
end;


function TsgeExtensionResourceList.GetAnsiFont(Name: String): TsgeAnsiFont;
begin
  Result := TsgeAnsiFont(FResourceList.TypedObj[Name, rtAnsiFont]);
  if Result = nil then
  begin
    Result := FDefault.AnsiFont;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
  end;
end;


function TsgeExtensionResourceList.GetSprite(Name: String): TsgeSprite;
begin
  Result := TsgeSprite(FResourceList.TypedObj[Name, rtSprite]);
  if Result = nil then
  begin
    Result := FDefault.Sprite;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
  end;
end;


function TsgeExtensionResourceList.GetAnimationFrames(Name: String): TsgeAnimationFrameList;
begin
  Result := TsgeAnimationFrameList(FResourceList.TypedObj[Name, rtAnimationFrames]);
  if Result = nil then
  begin
    Result := FDefault.Frames;
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


function TsgeExtensionResourceList.GetCursor(Name: String): TsgeCursor;
begin
  Result := TsgeCursor(FResourceList.TypedObj[Name, rtCursor]);
  if Result = nil then
  begin
    Result := FDefault.Cursor;
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_ResourceNotFound, Name));
  end;
end;


procedure TsgeExtensionResourceList.LoadResourceFromFile(ResType: TsgeResourceType; FileName: String);
var
  Cmd: TsgeSimpleCommand;
  s, aName, aBasePath, aMetaStr, aResType: String;
begin
  //Тип ресурса
  aResType := sgeResourceNames[ResType];

  //Путь
  aBasePath := sgeExtractFilePath(FileName);

  //Разделить имя и метастроку
  aName := '';
  aMetaStr := '';
  GetMetaStringAndFileName(FileName, aName, aMetaStr);

  //Подготовить команду
  s := rcLoadResource + #32 + aResType + #32#39 + aName + #39#32#39 + sgeExtractFileName(FileName) + #39#32'[' + aMetaStr + ']';

  //Загрузить ресурс
  Cmd := TsgeSimpleCommand.Create(s);
  try
    Command_LoadResource(Cmd, aBasePath);
  finally
    Cmd.Free;
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
      Lines.Part[i] := sgeTrim(Lines.Part[i]);

      if Lines.Part[i] = '' then
        Continue;

      if Lines.Part[i][1] = '#' then
        Continue;

      Lines.Part[i] := sgeSubstituteParamsToString(Lines.Part[i], Params, '%', '%');
      Cmd.Command := Lines.Part[i];

      //Загрузить ресурс
      try
        case LowerCase(Cmd.Part[0]) of
          rcClearParameters:
            Command_ClearParams(Params);

          rcSetParameter:
            Command_SetParam(Params, Cmd);

          rcDeleteParameter:
            Command_DeleteParam(Params, Cmd);

          rcLoadTable:
            Command_LoadTable(Cmd, BaseDirectory);

          rcLoadResource:
            Command_LoadResource(Cmd, BaseDirectory);

          else
            raise EsgeException.Create(_UNITNAME, Err_UnknownCommand, Cmd.Part[0]);
        end;
      except
        on E: EsgeException do
          raise EsgeException.Create(_UNITNAME, Err_LoadResourceError, 'Line ' + sgeIntToStr(i + 1), E.Message);
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
  MS: TsgeMemoryStream;
  S, BaseDirectory: String;
begin
  //Проверить на существование файла
  if not FExtFileSystem.FileExists(FileName) then
    raise EsgeException.Create(_UNITNAME, Err_FileNotFound, FileName);

  //Прочитать файл в строку
  MS := TsgeMemoryStream.Create;
  try
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
