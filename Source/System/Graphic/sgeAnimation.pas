{
Пакет             Simple Game Engine 2
Файл              sgeAnimation.pas
Версия            1.0
Создан            09.06.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Анимация
}
{$Include Defines.inc}

unit sgeAnimation;

{$mode ObjFPC}{$H+}

interface

uses
  sgeAnimationFrameList;

type
  TsgeAnimation = class
  private
    FFrameList: TsgeAnimationFrameList;
    FCurrentFrameIndex: Cardinal;       //Текущий индекс кадра
    FTimeOffset: Integer;               //Смещение времени для всех кадров
    FLastChangeTime: Int64;             //Время последней смены кадра

  public
    constructor Create(FrameList: TsgeAnimationFrameList);

    procedure Reset;
    function IsFrameChanged: Boolean;   //Функция определения смены кадра

    property FrameList: TsgeAnimationFrameList read FFrameList;
    property TimeOffset: Integer read FTimeOffset write FTimeOffset;
    property CurrentFrameIndex: Cardinal read FCurrentFrameIndex;
  end;


implementation

uses
  sgeErrors, sgeOSPlatform;

const
  _UNITNAME = 'Animation';

  Err_EmptyFrameList = 'EmptyFrameList';
  Err_ZeroFrames = 'ZeroFrames';


constructor TsgeAnimation.Create(FrameList: TsgeAnimationFrameList);
begin
  //Проверить кадры анимации
  if FrameList = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyFrameList);

  //Проверить количество кадров
  if FrameList.Count < 1 then
    raise EsgeException.Create(_UNITNAME, Err_ZeroFrames);

  FFrameList := FrameList;

  Reset;
end;


procedure TsgeAnimation.Reset;
begin
  FCurrentFrameIndex := 0;
end;


function TsgeAnimation.IsFrameChanged: Boolean;
var
  CurrentTime: Int64;
begin
  Result := False;

  //Проверить что бы был хотя бы один кадр
  if FFrameList.Count < 1 then
    raise EsgeException.Create(_UNITNAME, Err_ZeroFrames);

  //Определить смену кадра
  CurrentTime := sgeGetTickCount;
  if CurrentTime - FLastChangeTime - FTimeOffset >= FFrameList.Item[FCurrentFrameIndex].Duration then
  begin
    //Обновить параметры
    FLastChangeTime := CurrentTime;
    Inc(FCurrentFrameIndex);
    if FCurrentFrameIndex >= FFrameList.Count then
      FCurrentFrameIndex := 0;

    //Признак смены кадра
    Result := True;
  end;
end;



end.
