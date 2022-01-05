{
Пакет             Simple Game Engine 2
Файл              sgeGraphicAnimation.pas
Версия            1.2
Создан            14.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс анимации
}
{$Include Defines.inc}

unit sgeGraphicAnimation;

{$mode objfpc}{$H+}

interface

uses
  sgeMemoryStream, sgeGraphicAnimationFrames, sgeResourceList;


type
  TsgeGraphicAnimation = class
  private
    FResources: TsgeResourceList;         //Указатель на таблицу ресурсов

    FFileName: String;                    //имя файла
    FFrames: TsgeGraphicAnimationFrames;  //Массив кадров
    FNeedFreeFrames: Boolean;             //Флаг освобождения памяти, если загрузка из файла
    FCurrentFrameIndex: Cardinal;         //Текущий индекс кадра
    FLastChangeTime: Int64;               //Время последней смены кадра
    FTimeOffset: Integer;                 //Смещение времени для всех кадров

    FWidth: Integer;
    FHeight: Integer;

    procedure PreCreate;
    function  GetCurrentFrame: TsgeGraphicAnimationFrame;
    procedure SetFrames(AFrames: TsgeGraphicAnimationFrames);
    procedure SetCurrentFrameIndex(AIndex: Cardinal);
    function  GetFrameCount: Integer;

    procedure Process;
  public
    constructor Create(AFrames: TsgeGraphicAnimationFrames; Width, Height: Integer);
    constructor Create(FileName: String; Width, Height: Integer; Resources: TsgeResourceList);
    constructor Create(Stream: TsgeMemoryStream; Width, Height: Integer; Resources: TsgeResourceList);
    destructor  Destroy; override;

    procedure Reload;
    procedure Reset;

    procedure FromMemoryStream(Stream: TsgeMemoryStream);
    procedure LoadFromFile(FileName: String);

    property FileName: String read FFileName write FFileName;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property TimeOffset: Integer read FTimeOffset write FTimeOffset;
    property CurrentFrame: TsgeGraphicAnimationFrame read GetCurrentFrame;
    property CurrentFrameIndex: Cardinal read FCurrentFrameIndex write SetCurrentFrameIndex;
    property FrameCount: Integer read GetFrameCount;
    property Frames: TsgeGraphicAnimationFrames read FFrames write SetFrames;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils, sgeOSPlatform;


const
  _UNITNAME = 'GraphicAnimation';

  Err_IndexOutOfBounds    = 'IndexOutOfBounds';
  Err_EmptyFrames         = 'EmptyFrames';
  Err_CantLoadFromStream  = 'CantLoadFromStream';
  Err_CantReadFile        = 'CantReadFile';



procedure TsgeGraphicAnimation.PreCreate;
begin
  FNeedFreeFrames := False;
  FTimeOffset := 0;
  Reset;
end;


function TsgeGraphicAnimation.GetCurrentFrame: TsgeGraphicAnimationFrame;
begin
  //Переключить кадр
  Process;

  //Вернуть запись о кадре
  if FFrames.Count = 0 then Result := DefaultAnimationFrame else Result := FFrames.Frame[FCurrentFrameIndex];
end;


procedure TsgeGraphicAnimation.SetFrames(AFrames: TsgeGraphicAnimationFrames);
begin
  if FNeedFreeFrames then
    begin
    FNeedFreeFrames := False;
    FFrames.Free;
    end;
  FFrames := AFrames;
  Reset;
end;


procedure TsgeGraphicAnimation.SetCurrentFrameIndex(AIndex: Cardinal);
begin
  if AIndex > FFrames.Count - 1 then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(AIndex));

  FCurrentFrameIndex := AIndex;
end;


function TsgeGraphicAnimation.GetFrameCount: Integer;
begin
  Result := FFrames.Count;
end;


procedure TsgeGraphicAnimation.Process;
var
  CurrentTime: Int64;
begin
  if FFrames.Count < 1 then Exit;

  CurrentTime := sgeGetTickCount;
  if CurrentTime - FLastChangeTime - FTimeOffset >= FFrames.Frame[FCurrentFrameIndex].Time then
    begin
    FLastChangeTime := CurrentTime;
    Inc(FCurrentFrameIndex);
    if FCurrentFrameIndex >= FFrames.Count then FCurrentFrameIndex := 0;
    end;
end;


constructor TsgeGraphicAnimation.Create(AFrames: TsgeGraphicAnimationFrames; Width, Height: Integer);
begin
  if AFrames = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyFrames);

  PreCreate;
  FWidth := Width;
  FHeight := Height;
  FFrames := AFrames;
end;


constructor TsgeGraphicAnimation.Create(FileName: String; Width, Height: Integer; Resources: TsgeResourceList);
begin
  PreCreate;
  FResources := Resources;
  FWidth := Width;
  FHeight := Height;
  FFileName := FileName;

  LoadFromFile(FileName);
end;


constructor TsgeGraphicAnimation.Create(Stream: TsgeMemoryStream; Width, Height: Integer; Resources: TsgeResourceList);
begin
  PreCreate;
  FResources := Resources;
  FWidth := Width;
  FHeight := Height;

  FromMemoryStream(Stream);
end;


destructor TsgeGraphicAnimation.Destroy;
begin
  if FNeedFreeFrames then FFrames.Free;
end;


procedure TsgeGraphicAnimation.Reload;
begin
  LoadFromFile(FFileName);
end;


procedure TsgeGraphicAnimation.Reset;
begin
  FCurrentFrameIndex := 0;
end;


procedure TsgeGraphicAnimation.FromMemoryStream(Stream: TsgeMemoryStream);
begin
  try

    case FNeedFreeFrames of
      True:
        begin
        FFrames.FromMemoryStream(Stream);
        Reset;
        end;

      False:
        begin
        FNeedFreeFrames := True;
        FFrames := TsgeGraphicAnimationFrames.Create(Stream, FResources);
        Reset;
        end;
    end;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantLoadFromStream, '', E.Message);
  end;
end;


procedure TsgeGraphicAnimation.LoadFromFile(FileName: String);
begin
  try

    case FNeedFreeFrames of
      True:
        begin
        FFrames.LoadFromFile(FileName);
        Reset;
        end;

      False:
        begin
        FNeedFreeFrames := True;
        FFrames := TsgeGraphicAnimationFrames.Create(FileName, FResources);
        Reset;
        end;
    end;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName, E.Message);
  end;
end;



end.

