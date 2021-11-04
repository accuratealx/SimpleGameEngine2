{
Пакет             Simple Game Engine 2
Файл              sgeExtensionMusic.pas
Версия            1.1
Создан            17.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Музыкальный проигрыватель
}
{$Include Defines.inc}

unit sgeExtensionMusicPlayer;

{$mode objfpc}{$H+}

interface

uses
  sgeThread, sgeSoundSource, sgeSoundBuffer,
  sgeExtensionBase,
  sgeMusicPlayerTrack, sgeMusicPLayerTrackList, sgeMusicPLayerTaskList;


const
  Extension_Music = 'MusicPlayer';


type
  //Режим смены дорожек (Случайно, Вперёд, Назад)
  TsgeExtensionMusicPlayerChangeMode = (cmRandom, cmForward, cmBackward);


  //Режим повторения дорожек (Дорожка, Список)
  TsgeExtensionMusicPlayerRepeatMode = (rmNone, rmTrack, rmList);


  //Состояние проигрывателя (Проигрывается, Остановлен)
  TsgeExtensionMusicPlayerSate = (psPlay, psStop);


  TsgeExtensionMusicPlayer = class(TsgeExtensionBase)
  private
    //Классы
    FThread: TsgeThread;
    FTrackList: TsgeMusicPlayerTrackList;
    FTaskList: TsgeMusicPLayerTaskList;

    //Параметры
    FChangeMode: TsgeExtensionMusicPlayerChangeMode;                //Режим смены дорожек
    FRepeatmode: TsgeExtensionMusicPlayerRepeatMode;                //Режим повторения
    FGroup: String;                                                 //Текущая группа

    //Вспомогательные параметры
    FFadeSeconds: Single;                                           //Время затухания и нарастания в секундах
    FFading: Boolean;                                               //Флаг затухания для потока

    procedure ThreadProc;                                           //Метод потока

    function GetNextTrackByMode: TsgeMusicPlayerTrack;              //Взять следующую дорожку

    //Свойства
    procedure SetVolume(AVolume: Single);
    procedure SetFadeTime(ATime: Word);
    function  GetState: TsgeExtensionMusicPlayerSate;

  protected
    FSource: TsgeSoundSource;
    FBuffer: TsgeSoundBuffer;

    FVolume: Single;                                                //Громкость
    FFadeTime: Word;                                                //Время затухания и нарастания в мс
    FThreadDelay: Cardinal;                                         //Задержка вызовов между срабатываниями

    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    procedure Play(Track: TsgeMusicPlayerTrack);
    procedure Play(TrackName: String);
    procedure Play;

    procedure Next;
    procedure Prev;
    procedure Random;

    procedure Stop;

    property Volume: Single read FVolume write SetVolume;
    property ChangeMode: TsgeExtensionMusicPlayerChangeMode read FChangeMode write FChangeMode;
    property Repeatmode: TsgeExtensionMusicPlayerRepeatMode read FRepeatmode write FRepeatmode;
    property Group: String read FGroup write FGroup;
    property FadeTime: Word read FFadeTime write SetFadeTime;
    property State: TsgeExtensionMusicPlayerSate read GetState;
    property TrackList: TsgeMusicPlayerTrackList read FTrackList;
  end;


implementation

uses
  sgeErrors, sgeOSPlatform,
  sgeMusicPlayerTaskBase, sgeMusicPlayerTaskPlay, sgeMusicPlayerTaskStop,
  sgeMusicPlayerTaskVolume;

const
  _UNITNAME = 'ExtensionMusicPlayer';

  Err_TrackNotFound = 'TrackNotFound';
  Err_TrackIsEmpty  = 'TrackIsEmpty';


procedure TsgeExtensionMusicPlayer.ThreadProc;
var
  Task: TsgeMusicPlayerTaskBase;
  Track: TsgeMusicPlayerTrack;
  TimeToEnd: Single;
begin
  //Обработать задачу
  if FTaskList.Count > 0 then
    begin
    //Ссылка на задачу
    Task := FTaskList.Item[0];

    //Проверить задачу на завершение
    if Task.Done then FTaskList.Delete(0) else Task.Process;
    end;



  //Проверить затухание композиции
  case GetState of
    psPlay:
      begin
      //Секунд до конца дорожки
      TimeToEnd := FSource.Buffer.Time - FSource.OffsetSecond;

      //Наступило время
      if (TimeToEnd <= FFadeSeconds) and not FFading then
        begin
        FFading := True;

        //Остановить проигрывание
        Stop;

        //Проверить режим проигрывания
        if FRepeatmode <> rmNone then
          begin
          //Текуший трек
          if FRepeatmode = rmTrack then
            Track := FTrackList.CurrentTrack;

          //Список
          if FRepeatmode = rmList then
            Track := GetNextTrackByMode;

          //Проверить дорожку
          if Track <> nil then FTaskList.Add(TsgeMusicPlayerTaskPlay.Create(Track))
            else ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_TrackNotFound, FGroup));
          end;
        end;

      end;//psPlay

    psStop:
      begin
      if FFading then FFading := False;
      end;

  end;


  //Отдать ресурсы системе
  sgeSleep(FThreadDelay);
end;


function TsgeExtensionMusicPlayer.GetNextTrackByMode: TsgeMusicPlayerTrack;
begin
  //Значение по умолчанию
  Result := nil;

  //Взять следующую дорожку
  case FChangeMode of
    cmRandom  : Result := FTrackList.GetRandomTrack(FGroup);
    cmForward : Result := FTrackList.GetNextTrack(FGroup);
    cmBackward: Result := FTrackList.GetPrevTrack(FGroup);
  end;
end;


procedure TsgeExtensionMusicPlayer.SetVolume(AVolume: Single);
begin
  //Проверить диапазон
  if AVolume < 0 then AVolume := 0;
  if AVolume > 1 then AVolume := 1;

  //Запомнить гормкость
  FVolume := AVolume;

  //Создать задачу изменения громкости
  FTaskList.Add(TsgeMusicPlayerTaskVolume.Create(FVolume));
end;


procedure TsgeExtensionMusicPlayer.SetFadeTime(ATime: Word);
begin
  if FFadeTime = ATime then Exit;

  FFadeTime := ATime;

  //Посчитать время затухания в секундах
  FFadeSeconds := FFadeTime / 1000;
end;


function TsgeExtensionMusicPlayer.GetState: TsgeExtensionMusicPlayerSate;
begin
  if FSource.State = sssPlay then Result := psPlay else Result := psStop;
end;


class function TsgeExtensionMusicPlayer.GetName: String;
begin
  Result := Extension_Music;
end;


constructor TsgeExtensionMusicPlayer.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Создать классы
    FThread := TsgeThread.Create;
    FTrackList := TsgeMusicPlayerTrackList.Create(True);
    FTaskList := TsgeMusicPLayerTaskList.Create(True);

    FBuffer := TsgeSoundBuffer.CreateBlank;

    FSource := TsgeSoundSource.Create;
    FSource.RelativePos := True;
    FSource.Gain := 0;

    //Установить параметры
    FVolume := 0.5;
    FChangeMode := cmRandom;
    FRepeatmode := rmList;
    FThreadDelay := 10;
    FGroup := '';

    //Задать время затухания
    SetFadeTime(5000);

    //Установить метод потока
    FThread.LoopProc := @ThreadProc;

    //Запустить поток
    FThread.Resume;
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionMusicPlayer.Destroy;
begin
  //Убрать метод потока
  FThread.LoopProc := nil;

  //Остановить проигрывание
  FSource.Stop;
  FSource.Buffer := nil;

  //Удалить классы
  FThread.Free;
  FBuffer.Free;
  FSource.Free;
  FTaskList.Free;
  FTrackList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionMusicPlayer.Play(Track: TsgeMusicPlayerTrack);
begin
  //Проверить дорожку
  if Track = nil then
    begin
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_TrackIsEmpty, FGroup));
    Exit;
    end;

  //Если проигрыватель играет, то остановить
  if GetState = psPlay then Stop;

  //Добавить задачу
  FTaskList.Add(TsgeMusicPlayerTaskPlay.Create(Track));
end;


procedure TsgeExtensionMusicPlayer.Play(TrackName: String);
var
  Idx: Integer;
  Track: TsgeMusicPlayerTrack;
begin
  //Найти индекс дорожки
  Idx := FTrackList.IndexOfName(TrackName);

  //Проверить индекс
  if Idx = -1 then
    begin
    ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_TrackNotFound, TrackName));
    Exit;
    end;

  //Ссылка на дорожку
  Track := FTrackList.Item[Idx];

  //Запустить проигрывание
  Play(Track);
end;


procedure TsgeExtensionMusicPlayer.Play;
begin
  Play(GetNextTrackByMode);
end;


procedure TsgeExtensionMusicPlayer.Next;
begin
  Play(FTrackList.GetNextTrack(FGroup));
end;


procedure TsgeExtensionMusicPlayer.Prev;
begin
  Play(FTrackList.GetPrevTrack(FGroup));
end;


procedure TsgeExtensionMusicPlayer.Random;
begin
  Play(FTrackList.GetRandomTrack(FGroup));
end;


procedure TsgeExtensionMusicPlayer.Stop;
begin
  //Удалить текущие задачи
  FTaskList.Clear;

  //Проверка на проигрышь
  if GetState = psStop then Exit;

  //Добавить задачу остановки
  FTaskList.Add(TsgeMusicPlayerTaskStop.Create);
end;



end.

