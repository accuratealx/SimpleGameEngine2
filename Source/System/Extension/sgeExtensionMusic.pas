{
Пакет             Simple Game Engine 2
Файл              sgeExtensionMusic.pas
Версия            1.0
Создан            17.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Музыкальный проигрыватель
}
{$Include Defines.inc}

unit sgeExtensionMusic;

{$mode objfpc}{$H+}

interface

uses
  sgeThread, sgeSoundSource, sgeSoundBuffer,
  sgeExtensionBase,
  sgeMusicPLayerTrackList;


const
  Extension_Music = 'Music';


type
  //Режим проигрывания дорожек (Случайно, Вперёд, Назад)
  TsgeExtensionMusicPlayMode = (empmRandom, empmForward, empmBackward);


  //Режим повторения дорожек (Дорожка, Список)
  TsgeExtensionMusicRepeatMode = (emrpTrack, emrpList);


  //Состояние проигрывателя (Проигрывается, Остановлен)
  TsgeExtensionMusicSate = (emsPlay, emsStop);


  TsgeExtensionMusic = class(TsgeExtensionBase)
  private
    //Классы
    FThread: TsgeThread;
    FTrackList: TsgeMusicPlayerTrackList;
    FSource: TsgeSoundSource;
    FBuffer: TsgeSoundBuffer;

    //Параметры
    FPlayMode: TsgeExtensionMusicPlayMode;                          //Режим проигрывания
    FRepeatmode: TsgeExtensionMusicRepeatMode;                      //Режим повторения
    FGroup: String;                                                 //Текущая группа
    FFadeTime: Word;                                                //Время затухания и нарастания в мс

    //Вспомогательные параметры
    FFadeSeconds: Single;                                           //Время затухания и нарастания в секундах

    procedure ThreadProc;

    //Свойства
    procedure SetFadeTime(ATime: Word);
    function  GetState: TsgeExtensionMusicSate;
  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    procedure Play;
    procedure Stop;

    property PlayMode: TsgeExtensionMusicPlayMode read FPlayMode write FPlayMode;
    property Repeatmode: TsgeExtensionMusicRepeatMode read FRepeatmode;
    property Group: String read FGroup write FGroup;
    property FadeTime: Word read FFadeTime write SetFadeTime;
    property State: TsgeExtensionMusicSate read GetState;
    property TrackList: TsgeMusicPlayerTrackList read FTrackList;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'ExtensionMusic';


procedure TsgeExtensionMusic.ThreadProc;
begin

end;


procedure TsgeExtensionMusic.SetFadeTime(ATime: Word);
begin
  if FFadeTime = ATime then Exit;

  FFadeTime := ATime;

  //Посчитать время затухания в секундах
  FFadeSeconds := FFadeTime / 1000;
end;


function TsgeExtensionMusic.GetState: TsgeExtensionMusicSate;
begin
  if FSource.State = sssPlay then Result := emsPlay
    else Result := emsStop;
end;


class function TsgeExtensionMusic.GetName: String;
begin
  Result := Extension_Music;
end;


constructor TsgeExtensionMusic.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Создать классы
    FTrackList := TsgeMusicPlayerTrackList.Create(True);
    FThread := TsgeThread.Create;

    FSource := TsgeSoundSource.Create;
    FSource.RelativePos := True;
    FSource.Gain := 1;

    FBuffer := TsgeSoundBuffer.CreateBlank;

    //Установить параметры
    FPlayMode := empmRandom;
    FRepeatmode := emrpList;
    FGroup := '';

    //Задать время затухания
    SetFadeTime(2000);

    //Установить метод потока
    FThread.LoopProc := @ThreadProc;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionMusic.Destroy;
begin
  //Остановить проигрывание
  FSource.Stop;
  FSource.Buffer := nil;

  //Удалить классы
  FThread.Free;
  FBuffer.Free;
  FSource.Free;
  FTrackList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionMusic.Play;
begin
  FTrackList.GetRandomTrack;
  FBuffer.LoadFromFile(FTrackList.CurrentTrack.FileName);
  FSource.Buffer := FBuffer;
  FSource.Play;
end;


procedure TsgeExtensionMusic.Stop;
begin
  if GetState = emsStop then Exit;

  FSource.Stop;
  FSource.Buffer := nil;
end;



end.

