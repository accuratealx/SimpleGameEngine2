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
  sgeGraphicColor, sgeGraphicOpenGLDrawObjectFadeItem;


const
  Event_GraphicFade         = 'Graphic.Fade';
  EVENT_GRAPHIC_ADD_SHADER  = 'Graphic.AddShader';
  Event_GraphicNewFade      = 'Graphic.NewFade';



type
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


  //Добавление шейдера
  TsgeEventGraphicAddShader = class(TsgeEventBase)
  private
    FShaderName: String;
    FShaderStream: TsgeMemoryStream;
  public
    constructor Create(Name: ShortString; ShaderName: String; Stream: TsgeMemoryStream);

    function Copy: TsgeEventBase; override;

    property ShaderName: String read FShaderName;
    property ShaderStream: TsgeMemoryStream read FShaderStream;
  end;



  //Добавление нового затемнения
  TsgeEventGraphicNewFade = class(TsgeEventBase)
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



implementation


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



constructor TsgeEventGraphicAddShader.Create(Name: ShortString; ShaderName: String; Stream: TsgeMemoryStream);
begin
  inherited Create(Name);

  FShaderName := ShaderName;
  FShaderStream := Stream;
end;


function TsgeEventGraphicAddShader.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicAddShader.Create(FName, FShaderName, FShaderStream);
end;




constructor TsgeEventGraphicNewFade.Create(Name: ShortString; Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer; TimeProc: TsgeFadeProc);
begin
  inherited Create(Name);

  FMode := Mode;
  FColor := Color;
  FTime := Time;
  FID := ID;
  FTimeProc := TimeProc;
end;


function TsgeEventGraphicNewFade.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicNewFade.Create(FName, FMode, FColor, FTime, FID, FTimeProc);
end;


end.

