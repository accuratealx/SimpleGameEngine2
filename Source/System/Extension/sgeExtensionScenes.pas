{
Пакет             Simple Game Engine 2
Файл              sgeExtensionScenes.pas
Версия            1.0
Создан            17.12.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Сцены
}
{$Include Defines.inc}

unit sgeExtensionScenes;

{$mode ObjFPC}{$H+}

interface

uses
  sgeGraphicColor,
  sgeExtensionBase, sgeExtensionGraphic,
  sgeEventBase, sgeEventGraphic,
  sgeSceneList, sgeSceneBase;


const
  Extension_Scenes = 'Scenes';


type
  TsgeExtensionScenes = class(TsgeExtensionBase)
  private
    FFadeScene: TsgeSceneBase;                                      //Временный указатель на сцену, для перехода

  private
    FExtGraphic: TsgeExtensionGraphic;

    FSceneList: TsgeSceneList;

    function EventFadeHandler(EventObj: TsgeEventGraphicFade): TsgeEventHandlerResult;
  protected
    function GetName: String; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Start(Scene: TsgeSceneBase; Color: TsgeColor; Time: Cardinal);

    procedure Push(Scene: TsgeSceneBase);
    procedure Push(Scene: TsgeSceneBase; Mode: TsgeExtensionFadeMode; Color: TsgeColor; Time: Cardinal);
    procedure Pop;
    procedure Pop(Mode: TsgeExtensionFadeMode; Color: TsgeColor; Time: Cardinal);

    property SceneList: TsgeSceneList read FSceneList;
  end;


implementation

uses
  sgeErrors, sgeTypes, sgeCorePointerUtils;

const
  _UNITNAME = 'ExtensionScenes';
  FADEID_START = MaxInt - 1;
  FADEID_PUSH = MaxInt - 2;
  FADEID_POP = MaxInt - 3;


function TsgeExtensionScenes.EventFadeHandler(EventObj: TsgeEventGraphicFade): TsgeEventHandlerResult;
begin
  Result := ehrNormal;

  //Переход для запуска приложения
  case EventObj.PassedTime of
    ptBegin:
    begin
      if EventObj.ID = FADEID_START then
      begin
        Push(FFadeScene);
        FFadeScene := nil;
      end;
    end;

    ptMiddle:
    begin
      case EventObj.ID of
        FADEID_PUSH:
        begin
          Push(FFadeScene);
          FFadeScene := nil;
        end;

        FADEID_POP:
          Pop;
      end;
    end;
  end;

  //Переходы для смены сцен
  if EventObj.PassedTime = ptMiddle then
  begin
  end;
end;


function TsgeExtensionScenes.GetName: String;
begin
  Result := Extension_Scenes;
end;


constructor TsgeExtensionScenes.Create;
begin
  try
    inherited Create;

    //Указатели на классы
    FExtGraphic := TsgeExtensionGraphic(GetExtension(Extension_Graphic));

    //Классы
    FSceneList := TsgeSceneList.Create(False);

    //Подписаться на затемнение
    EventManager.SubscriberGroupList.Subscribe(Event_GraphicFade, TsgeEventHandler(@EventFadeHandler));

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionScenes.Destroy;
begin
  //Отписаться от затемнения
  EventManager.SubscriberGroupList.UnSubscribe(TsgeEventHandler(@EventFadeHandler));

  //Классы
  FSceneList.Free;

  inherited Destroy;
end;


procedure TsgeExtensionScenes.Start(Scene: TsgeSceneBase; Color: TsgeColor; Time: Cardinal);
begin
  //Запомнить указатель на сцену
  FFadeScene := Scene;

  //Запустить затемнение
  FExtGraphic.Fade(efmColorToNormal, Color, Time, FADEID_START);
end;


procedure TsgeExtensionScenes.Push(Scene: TsgeSceneBase);
begin
  //Добавить сцену в очередь
  if FSceneList.Count = 0 then
    FSceneList.Add(Scene)
  else
    FSceneList.Insert(0, Scene);

  //Деактивировать текущую
  if FSceneList.Count > 1 then
    FSceneList.Item[1].Deactivate;

  //Активировать новую
  Scene.Activate;
end;


procedure TsgeExtensionScenes.Push(Scene: TsgeSceneBase; Mode: TsgeExtensionFadeMode; Color: TsgeColor; Time: Cardinal);
begin
  //Запомнить указатель на сцену
  FFadeScene := Scene;

  //Запустить затемнение
  FExtGraphic.Fade(Mode, Color, Time, FADEID_PUSH);
end;


procedure TsgeExtensionScenes.Pop;
begin
  //Деактивировать текущую сцену
  if FSceneList.Count > 0 then
    FSceneList.Item[0].Deactivate;

  //Удалить текущую сцену
  FSceneList.Delete(0);

  //Активировать предыдущую сцену
  if FSceneList.Count > 0 then
    FSceneList.Item[0].Activate
  else
    //Тут нет больше сцен, по идее закрыть программу
    sgeCorePointer_GetSGE.Stop;
end;


procedure TsgeExtensionScenes.Pop(Mode: TsgeExtensionFadeMode; Color: TsgeColor; Time: Cardinal);
begin
  //Запустить затемнение
  FExtGraphic.Fade(Mode, Color, Time, FADEID_POP);
end;



end.

