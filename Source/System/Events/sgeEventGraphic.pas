{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphic.pas
Версия            1.3
Создан            06.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика
}
{$Include Defines.inc}

unit sgeEventGraphic;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeMemoryStream, sgeEventBase,
  sgeGraphicColor,
  sgeDisplayLayer,
  sgeGraphicOpenGLDrawObjectFadeItem;


const
  //Шейдеры
  Event_GraphicShaderAdd    = 'Graphic.ShaderAdd';

  //Затемнение
  Event_GraphicFade         = 'Graphic.Fade';
  Event_GraphicFadeNew      = 'Graphic.FadeNew';

  //Слои
  Event_Graphic_LayerAdd    = 'Graphic.LayerAdd';
  Event_Graphic_LayerModify = 'Graphic.LayerModify';
  Event_Graphic_LayerDelete = 'Graphic.LayerDelete';

  //Элементы

type
  //Добавление шейдера
  TsgeEventGraphicShaderAdd = class(TsgeEventBase)
  private
    FShaderName: String;
    FShaderStream: TsgeMemoryStream;
  public
    constructor Create(Name: ShortString; ShaderName: String; Stream: TsgeMemoryStream);

    function Copy: TsgeEventBase; override;

    property ShaderName: String read FShaderName;
    property ShaderStream: TsgeMemoryStream read FShaderStream;
  end;


  //Процесс затемнения
  TsgeEventGraphicFade = class(TsgeEventBase)
  private
    FPassedTime: TsgePassedTime;
    FID: Integer;
  public
    constructor Create(Name: ShortString; PassedTime: TsgePassedTime; ID: Integer);

    function Copy: TsgeEventBase; override;

    property PassedTime: TsgePassedTime read FPassedTime;
    property ID: Integer read FID;
  end;



  //Добавление нового затемнения
  TsgeEventGraphicFadeNew = class(TsgeEventBase)
  private
    FMode: TsgeFadeMode;
    FColor: TsgeColor;
    FTime: Cardinal;
    FID: Integer;
    FTimeProc: TsgeFadeProc;
  public
    constructor Create(Name: ShortString; Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer; TimeProc: TsgeFadeProc);

    function Copy: TsgeEventBase; override;

    property Mode: TsgeFadeMode read FMode;
    property Color: TsgeColor read FColor;
    property Time: Cardinal read FTime;
    property ID: Integer read FID;
    property TimeProc: TsgeFadeProc read FTimeProc;
  end;


  //Событие слоев
  TsgeEventGraphicLayer = class(TsgeEventBase)
  private
    FUniqueID: Integer;
    FLayer: TsgeDisplayLayer;
  public
    constructor Create(Name: ShortString; UniqueID: Integer; Layer: TsgeDisplayLayer = nil);

    function Copy: TsgeEventBase; override;

    property UniqueID: Integer read FUniqueID;
    property Layer: TsgeDisplayLayer read FLayer;
  end;


implementation


constructor TsgeEventGraphicShaderAdd.Create(Name: ShortString; ShaderName: String; Stream: TsgeMemoryStream);
begin
  inherited Create(Name);

  FShaderName := ShaderName;
  FShaderStream := Stream;
end;


function TsgeEventGraphicShaderAdd.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicShaderAdd.Create(FName, FShaderName, FShaderStream);
end;



constructor TsgeEventGraphicFade.Create(Name: ShortString; PassedTime: TsgePassedTime; ID: Integer);
begin
  inherited Create(Name);

  FPassedTime := PassedTime;
  FID := ID;
end;


function TsgeEventGraphicFade.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicFade.Create(FName, FPassedTime, FID);
end;



constructor TsgeEventGraphicFadeNew.Create(Name: ShortString; Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer; TimeProc: TsgeFadeProc);
begin
  inherited Create(Name);

  FMode := Mode;
  FColor := Color;
  FTime := Time;
  FID := ID;
  FTimeProc := TimeProc;
end;


function TsgeEventGraphicFadeNew.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicFadeNew.Create(FName, FMode, FColor, FTime, FID, FTimeProc);
end;



constructor TsgeEventGraphicLayer.Create(Name: ShortString; UniqueID: Integer; Layer: TsgeDisplayLayer);
begin
  inherited Create(Name);
  FUniqueID := UniqueID;
  FLayer := Layer;
end;


function TsgeEventGraphicLayer.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicLayer.Create(FName, FUniqueID, FLayer);
end;



end.

