program LB;

{$AppType GUI}

uses
  SimpleGameEngine,
  sgeTypes, sgeThread, sgeKeys, sgeSystemUtils, sgeOSPlatform, sgeMathUtils,
  sgeEventBase, sgeEventKeyboard,
  sgeGraphicColor, sgeGraphicElementCircle;



type
  TGame = class(TSimpleGameEngine)
  private
    function EventWindowKeyDown(Obj: TsgeEventKeyboard): Boolean;
    procedure ThreadProc;

  public
    constructor Create(InitSound: Boolean = True); override;
    destructor  Destroy; override;
  end;


const
  MaxItem = 256;


type
  TTestItem = record
    Enable: Boolean;
    El: TsgeGraphicElementCircle;
    R: Integer;
    Angle: Single;
    Speed: Single;
  end;


var
  Game: TGame;
  FElArray: array[0..MaxItem] of TTestItem;
  THRD: TsgeThread;


function TGame.EventWindowKeyDown(Obj: TsgeEventKeyboard): Boolean;
begin
  Result := False;

  case Obj.Key of
    keyEscape : Stop;
  end;
end;


var
  A: Single = 0;


procedure TGame.ThreadProc;
var
  i: Integer;
  X0, Y0, A0: Single;
begin

  try
    A := A + 0.001;
    if A > sgeRadToDeg(360) then A := 0;

    X0 := ExtWindow.Window.Width div 2;
    Y0 := ExtWindow.Window.Height div 2;
    for i := 0 to MaxItem do
      begin
      if not FElArray[i].Enable then Continue;
      //Лемниската Бернули
      A0 := A + FElArray[i].Angle;
      FElArray[i].El.X := X0 + (FElArray[i].R * Sqrt(2) * Cos(A0)) / (1 + Sqr(Sin(A0)));
      FElArray[i].El.Y := Y0 + (FElArray[i].R * Sqrt(2) * Sin(A0) * Cos(A0)) / (1 + Sqr(Sin(A0)));

      //Обновить графический примитив
      FElArray[i].El.Update;
      end;

    sgeSleep(10);

  except
    ErrorManager.LogMessage('Ошибка при изменении парметров примитивов');
  end;
end;



constructor TGame.Create(InitSound: Boolean);
var
  i: Integer;
begin
  Randomize;
  inherited Create(InitSound);

  //Привязать основные команды на кнопки
  AttachDefaultCommand;

  //Подписка на нажатие клавиш
  EventManager.Subscribe(Event_KeyboardDown, TsgeEventHandler(@EventWindowKeyDown));

  //Лемниската Бернули
  ExtGraphic.DrawList.AddLayer('Circle', 0);
  for i := 0 to MaxItem do
    begin
    FElArray[i].El := TsgeGraphicElementCircle.Create(400 + Random(100), Random(ExtGraphic.Graphic.Height), Random(10) + 2, sgeGetRandomColor(1), 16{3 + Random(9)});
    FElArray[i].R := 220 + Random(32);
    FElArray[i].Angle := sgeDegToRad(Random(360 * 3));
    FElArray[i].Enable := True;
    FElArray[i].Speed := Random + 0.01;
    ExtGraphic.DrawList.AddElement(FElArray[i].El, 'Circle');
    end;

  //Поток изменения координат
  THRD := TsgeThread.Create(@ThreadProc, False, False);

  //Настройка
  ExtGraphic.FPS.Enable := True;
  ExtGraphic.MaxFPS := 10000;
  ExtWindow.Window.Caption := Utf8ToAnsi('Лемниската Бернули');
  ExtWindow.Window.Center;
  ExtWindow.Window.Show;
end;


destructor TGame.Destroy;
begin
  THRD.Free;
  inherited Destroy;
end;


begin
  Game := TGame.Create(False);
  Game.Run;
  Game.Free;
end.


