program MusicPlayer;

{$AppType GUI}

uses
  sgeSystemUtils, SimpleGameEngine,
  sgeKeys, sgeEventBase, sgeEventKeyboard, sgeEventMusicPlayer,
  sgeExtensionMusicPlayer,
  sgeGraphicColor,
  sgeGraphicElementText;


type
  TGame = class(TSimpleGameEngine)
  private
    FMainVolume: TsgeGraphicElementText;
    FVolume: TsgeGraphicElementText;
    FChangeMode: TsgeGraphicElementText;
    FRepeatMode: TsgeGraphicElementText;
    FState: TsgeGraphicElementText;
    FTrackName: TsgeGraphicElementText;

    procedure UpdateStatus;
    procedure AddInfoText;

    function EventWindowKeyDown(Obj: TsgeEventKeyboard): Boolean;
    function EventMusicPlayerStart(Obj: TsgeEventMusicPlayerStart): Boolean;
    function EventMusicPlayerStop(Obj: TsgeEventBase): Boolean;
  public
    constructor Create(Options: TsgeInitOptions = []); override;
    destructor  Destroy; override;
  end;

var
  Game: TGame;


procedure TGame.UpdateStatus;
var
  s: String;
begin
  FVolume.Text := 'Volume - ' + sgeFloatToStr(ExtMusicPlayer.Volume);
  FVolume.Update;
  FMainVolume.Text := 'Main volume - ' + sgeFloatToStr(ExtSound.Sound.Gain);
  FMainVolume.Update;

  case ExtMusicPlayer.ChangeMode of
    cmRandom  : s := 'Random';
    cmForward : s := 'Forward';
    cmBackward: s := 'Backward';
  end;
  FChangeMode.Text := 'ChangeMode - ' + s;
  FChangeMode.Update;

  case ExtMusicPlayer.Repeatmode of
    rmNone : s := 'None';
    rmTrack: s := 'Track';
    rmList : s := 'List';
  end;
  FRepeatMode.Text := 'RepeatMode - ' + s;
  FRepeatMode.Update;
end;


procedure TGame.AddInfoText;

  procedure AddText(X, Y: Integer; Txt: String);
  begin
    ExtGraphic.LayerList.AddElement(TsgeGraphicElementText.Create(X, Y, ExtResourceList.Default.Font, Txt, cWhite));
  end;

  function AddStatusText(X, Y: Integer; Txt: String): TsgeGraphicElementText;
  begin
    Result := TsgeGraphicElementText.Create(X, Y, ExtResourceList.Default.Font, Txt, cWhite);
    ExtGraphic.LayerList.AddElement(Result);
  end;

begin
  AddText(10, 10, 'F3 - Play');
  AddText(10, 30, 'F4 - Stop');
  AddText(10, 50, 'F5 - Previvous track');
  AddText(10, 70, 'F6 - Random track');
  AddText(10, 90, 'F7 - Next track');
  AddText(10, 110, '1 - Change mode: Random');
  AddText(10, 130, '2 - Change mode: Forward');
  AddText(10, 150, '3 - Change mode: Backward');
  AddText(10, 170, '4 - Repeat mode: Node');
  AddText(10, 190, '5 - Repeat mode: Track');
  AddText(10, 210, '6 - Repeat mode: List');
  AddText(10, 230, '9 - Volume down');
  AddText(10, 250, '0 - Volume up');

  AddText(10, 290, 'Minus - Master volume down');
  AddText(10, 310, 'Plus - Master volume up');


  FMainVolume := AddStatusText(250, 10, '');
  FVolume := AddStatusText(250, 30, '');
  FChangeMode := AddStatusText(250, 50, '');
  FRepeatMode := AddStatusText(250, 70, '');
  FState := AddStatusText(250, 90, 'State - Stop');
  FTrackName := AddStatusText(250, 110, 'Track name -');
end;


function TGame.EventWindowKeyDown(Obj: TsgeEventKeyboard): Boolean;
begin
  Result := False;

  case Obj.Key of
    keyEscape : Stop;

    keyF3     : ExtMusicPlayer.Play;
    keyF4     : ExtMusicPlayer.Stop;

    keyF5     : ExtMusicPlayer.Prev;
    keyF6     : ExtMusicPlayer.Random;
    keyF7     : ExtMusicPlayer.Next;

    key1      : ExtMusicPlayer.ChangeMode := cmRandom;
    key2      : ExtMusicPlayer.ChangeMode := cmForward;
    key3      : ExtMusicPlayer.ChangeMode := cmBackward;

    key4      : ExtMusicPlayer.Repeatmode := rmNone;
    key5      : ExtMusicPlayer.Repeatmode := rmTrack;
    key6      : ExtMusicPlayer.Repeatmode := rmList;

    key9      : ExtMusicPlayer.Volume := ExtMusicPlayer.Volume - 0.1;
    key0      : ExtMusicPlayer.Volume := ExtMusicPlayer.Volume + 0.1;

    keyMinus  : ExtSound.Sound.Gain := ExtSound.Sound.Gain - 0.1;
    keyPlus   : ExtSound.Sound.Gain := ExtSound.Sound.Gain + 0.1;
  end;

  UpdateStatus;
end;


function TGame.EventMusicPlayerStart(Obj: TsgeEventMusicPlayerStart): Boolean;
begin
  Result := False;
  FState.Text := 'State - Play';
  FState.Update;
  FTrackName.Text := 'Track name - ' + Obj.Track.Name;
  FTrackName.Update;
end;


function TGame.EventMusicPlayerStop(Obj: TsgeEventBase): Boolean;
begin
  Result := True;
  FState.Text := 'State - Stop';
  FState.Update;
  FTrackName.Text := 'Track name -';
  FTrackName.Update;
end;


constructor TGame.Create(Options: TsgeInitOptions);
begin
  Randomize;
  inherited Create(Options);

  //Привязать основные команды на кнопки
  AttachDefaultCommand;

  //Подписка на события
  EventManager.SubscriberGroupList.Subscribe(Event_KeyboardDown, TsgeEventHandler(@EventWindowKeyDown));
  EventManager.SubscriberGroupList.Subscribe(Event_MusicPLayerStart, TsgeEventHandler(@EventMusicPlayerStart));
  EventManager.SubscriberGroupList.Subscribe(Event_MusicPLayerStop, TsgeEventHandler(@EventMusicPlayerStop));

  //Проверка проигрывателя
  ExtMusicPlayer.TrackList.LoadFromFile('Music\PlayList.txt');

  //Добавить информацию
  AddInfoText;
  UpdateStatus;

  //Настройка
  ExtGraphic.FPS.Enable := True;
  ExtGraphic.AutoEraseBG := True;
  ExtWindow.Window.Center;
  ExtWindow.Window.Show;
end;


destructor TGame.Destroy;
begin
  inherited Destroy;
end;


begin
  Game := TGame.Create([ioSound]);
  Game.Run;
  Game.Free;
end.


